package org.devocative.metis.service;

import ar.com.fdvs.dj.core.DynamicJasperHelper;
import ar.com.fdvs.dj.core.layout.ClassicLayoutManager;
import ar.com.fdvs.dj.domain.builders.ColumnBuilder;
import ar.com.fdvs.dj.domain.builders.FastReportBuilder;
import ar.com.fdvs.dj.domain.constants.Page;
import ar.com.fdvs.dj.domain.entities.columns.AbstractColumn;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import net.sf.jasperreports.engine.export.HtmlExporter;
import net.sf.jasperreports.engine.export.JRPdfExporter;
import net.sf.jasperreports.engine.export.JRXlsExporter;
import net.sf.jasperreports.engine.type.RunDirectionEnum;
import net.sf.jasperreports.export.SimpleExporterInput;
import net.sf.jasperreports.export.SimpleHtmlExporterOutput;
import net.sf.jasperreports.export.SimpleOutputStreamExporterOutput;
import net.sf.jasperreports.export.SimpleXlsReportConfiguration;
import org.devocative.adroit.ConfigUtil;
import org.devocative.adroit.ExcelExporter;
import org.devocative.adroit.date.UniDate;
import org.devocative.demeter.entity.EFileStorage;
import org.devocative.demeter.entity.ELayoutDirection;
import org.devocative.demeter.entity.EMimeType;
import org.devocative.demeter.entity.FileStore;
import org.devocative.demeter.iservice.FileStoreHandler;
import org.devocative.demeter.iservice.IFileStoreService;
import org.devocative.demeter.vo.UserVO;
import org.devocative.metis.MetisConfigKey;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/*
URL: https://community.jaspersoft.com/wiki/adding-fonts-embedding-pdf
 */
public class ReportBuilder {
	private final IFileStoreService fileStoreService;
	private final String name;
	private final UserVO currentUser;
	private final Format format;

	private String title;
	private List<Column> columns = new ArrayList<>();
	private List<Map<String, Object>> rows;

	// ------------------------------

	public ReportBuilder(IFileStoreService fileStoreService, String name, UserVO currentUser, Format format) {
		this.fileStoreService = fileStoreService;
		this.name = name;
		this.currentUser = currentUser;
		this.format = format;
	}

	// ------------------------------

	public ReportBuilder setTitle(String title) {
		this.title = title;
		return this;
	}

	public ReportBuilder setRows(List<Map<String, Object>> rows) {
		this.rows = rows;
		return this;
	}

	public ReportBuilder addColumn(Column column) {
		columns.add(column);
		return this;
	}

	// ---------------

	public FileStore build() {
		UniDate now = UniDate.now();
		UniDate expire = now.updateDay(ConfigUtil.getInteger(MetisConfigKey.ExportReportExpireDays));
		String fileName = String.format("%s-%s.%s",
			name,
			currentUser.formatDate(now.toDate(), "yyyyMMdd-HHmmss"),
			format.fileExtension);

		final FileStoreHandler fileStoreHandler = fileStoreService.create(
			fileName,
			EFileStorage.DISK,
			format.mimeType,
			expire.toDate(),
			name);


		try {
			if (format == Format.Data) {
				simpleData(fileStoreHandler);
			} else {
				dynamicJasper(fileStoreHandler);
			}

			fileStoreHandler.close();

			return fileStoreHandler.getFileStore();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	private void dynamicJasper(FileStoreHandler fileStoreHandler) throws JRException, IOException {
		FastReportBuilder drb = new FastReportBuilder();

		drb
			.setTitle(title)
			.setUseFullPageWidth(true)
			.setPageSizeAndOrientation(Page.Page_A4_Landscape())
			.setPrintBackgroundOnOddRows(true)
			.setIgnorePagination(true)
		;

		for (Column column : columns) {
			AbstractColumn col = ColumnBuilder.getNew()
				.setTitle(column.title)
				.setColumnProperty(column.property, column.type)
				.build();

			drb.addColumn(col);
		}

		JRDataSource ds = new JRBeanCollectionDataSource(rows);
		JasperPrint jp = DynamicJasperHelper.generateJasperPrint(drb.build(), new ClassicLayoutManager(), ds);

		switch (format) {

			case Excel:
				SimpleXlsReportConfiguration xlsConfig = new SimpleXlsReportConfiguration();
				if (currentUser.getLayoutDirection() == ELayoutDirection.RTL) {
					xlsConfig.setSheetDirection(RunDirectionEnum.RTL);
				}
				xlsConfig.setSheetNames(new String[]{title});

				JRXlsExporter xlsExporter = new JRXlsExporter();
				xlsExporter.setConfiguration(xlsConfig);
				xlsExporter.setExporterInput(new SimpleExporterInput(jp));
				xlsExporter.setExporterOutput(new SimpleOutputStreamExporterOutput(fileStoreHandler));
				xlsExporter.exportReport();
				break;

			case PDF:
				JRPdfExporter pdfExporter = new JRPdfExporter();
				pdfExporter.setExporterInput(new SimpleExporterInput(jp));
				pdfExporter.setExporterOutput(new SimpleOutputStreamExporterOutput(fileStoreHandler));
				pdfExporter.exportReport();
				break;

			case Print:
				StringBuilder builder = new StringBuilder();
				HtmlExporter exporter = new HtmlExporter();
				//exporter.setConfiguration(new SimpleHtmlReportConfiguration().set);
				exporter.setExporterInput(new SimpleExporterInput(jp));
				exporter.setExporterOutput(new SimpleHtmlExporterOutput(builder));
				exporter.exportReport();

				builder
					.append("<script>")
					.append("window.print();");
				builder.append("window.close();");
				builder.append("</script>");

				String report = builder.toString();
				if (currentUser.getLayoutDirection() == ELayoutDirection.RTL) {
					report = report.replaceAll("<html>", "<html dir='rtl'>");
				}
				fileStoreHandler.write(report.getBytes("UTF-8"));
				break;
		}
	}

	private void simpleData(FileStoreHandler fileStoreHandler) throws IOException {
		ExcelExporter exporter = new ExcelExporter(title)
			.setRtl(currentUser.getLayoutDirection() == ELayoutDirection.RTL)
			.setColumnsHeader(columns.stream()
				.map(column -> column.title)
				.collect(Collectors.toList()));

		for (Map<String, Object> map : rows) {
			List<Object> row = new ArrayList<>();
			for (Column column : columns) {
				final Object cell = map.get(column.property);
				row.add(cell != null ? cell.toString() : "");
			}
			exporter.addRowData(row);
		}

		exporter.generate(fileStoreHandler);
	}

	// ------------------------------

	public static class Column {
		private final String property;
		private final String title;
		private final Class type;

		public Column(String property, String title, Class type) {
			this.property = property;
			this.title = title;
			this.type = type;
		}
	}

	// ------------------------------

	public enum Format {
		Data("xlsx", EMimeType.EXCEL),
		Excel("xls", EMimeType.EXCEL),
		PDF("pdf", EMimeType.PDF),
		Print("html", EMimeType.HTML);

		private final String fileExtension;
		private final EMimeType mimeType;

		Format(String fileExtension, EMimeType mimeType) {
			this.fileExtension = fileExtension;
			this.mimeType = mimeType;
		}
	}
}
