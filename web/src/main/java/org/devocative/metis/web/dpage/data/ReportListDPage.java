//overwrite
package org.devocative.metis.web.dpage.data;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.MetisPrivilegeKey;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.iservice.data.IReportService;
import org.devocative.metis.vo.filter.data.ReportFVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.range.WDateRangeInput;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.formatter.ONumberFormatter;
import org.devocative.wickomp.grid.IGridDataSource;
import org.devocative.wickomp.grid.OGrid;
import org.devocative.wickomp.grid.WDataGrid;
import org.devocative.wickomp.grid.WSortField;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.grid.column.link.OAjaxLinkColumn;
import org.devocative.wickomp.grid.toolbar.OExportExcelButton;
import org.devocative.wickomp.html.WAjaxLink;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.html.window.WModalWindow;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.util.Collections;
import java.util.List;

public class ReportListDPage extends DPage implements IGridDataSource<Report> {
	private static final long serialVersionUID = 1803110562L;

	@Inject
	private IReportService reportService;

	private ReportFVO filter;
	private boolean formVisible = true;
	private String[] invisibleFormItems;

	private WDataGrid<Report> grid;
	private String[] removeColumns;

	private Boolean gridFit;
	private boolean gridEnabled = false;
	private OSize gridHeight = OSize.fixed(500);
	private OSize gridWidth = OSize.percent(100);

	// ------------------------------

	// Panel Call - New Filter
	public ReportListDPage(String id) {
		this(id, Collections.<String>emptyList(), new ReportFVO());
	}

	// Panel Call - Open Filter
	public ReportListDPage(String id, ReportFVO filter) {
		this(id, Collections.<String>emptyList(), filter);
	}

	// REST Call - New Filter
	public ReportListDPage(String id, List<String> params) {
		this(id, params, new ReportFVO());
	}

	// Main Constructor
	private ReportListDPage(String id, List<String> params, ReportFVO filter) {
		super(id, params);

		this.filter = filter;
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		final WModalWindow window = new WModalWindow("window");
		window.getOptions().setHeight(OSize.percent(80)).setWidth(OSize.percent(80));
		add(window);

		add(new WAjaxLink("add", MetisIcon.ADD) {
			private static final long serialVersionUID = -277701275L;

			@Override
			public void onClick(AjaxRequestTarget target) {
				window.setContent(new ReportFormDPage(window.getContentId()));
				window.show(target);
			}
		}.setVisible(hasPermission(MetisPrivilegeKey.ReportAdd)));

		WFloatTable floatTable = new WFloatTable("floatTable");
		//floatTable.setEqualWidth(true);
		floatTable.add(new WTextInput("title")
			.setLabel(new ResourceModel("Report.title")));
		floatTable.add(new WSelectionInput("dataView", reportService.getDataViewList(), true)
			.setLabel(new ResourceModel("Report.dataView")));
		floatTable.add(new WSelectionInput("groups", reportService.getGroupsList(), true)
			.setLabel(new ResourceModel("Report.groups")));
		floatTable.add(new WDateRangeInput("creationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.creationDate")));
		floatTable.add(new WSelectionInput("creatorUser", reportService.getCreatorUserList(), true)
			.setLabel(new ResourceModel("entity.creatorUser")));
		floatTable.add(new WDateRangeInput("modificationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.modificationDate")));
		floatTable.add(new WSelectionInput("modifierUser", reportService.getModifierUserList(), true)
			.setLabel(new ResourceModel("entity.modifierUser")));

		Form<ReportFVO> form = new Form<>("form", new CompoundPropertyModel<>(filter));
		form.add(floatTable);
		form.add(new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
			private static final long serialVersionUID = -1075633782L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				grid.setEnabled(true);
				grid.loadData(target);
			}
		});
		add(form);

		OColumnList<Report> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<>(new ResourceModel("Report.title"), "title"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("Report.dataView"), "dataView"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("Report.groups"), "groups"));
		columnList.add(new OPropertyColumn<Report>(new ResourceModel("entity.creationDate"), "creationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.creatorUser"), "creatorUser"));
		columnList.add(new OPropertyColumn<Report>(new ResourceModel("entity.modificationDate"), "modificationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.modifierUser"), "modifierUser"));
		columnList.add(new OPropertyColumn<Report>(new ResourceModel("entity.version"), "version")
			.setFormatter(ONumberFormatter.integer())
			.setStyle("direction:ltr"));

		if (hasPermission(MetisPrivilegeKey.ReportEdit)) {
			columnList.add(new OAjaxLinkColumn<Report>(new Model<>(), MetisIcon.EDIT) {
				private static final long serialVersionUID = -857298157L;

				@Override
				public void onClick(AjaxRequestTarget target, IModel<Report> rowData) {
					window.setContent(new ReportFormDPage(window.getContentId(), rowData.getObject()));
					window.show(target);
				}
			}.setField("EDIT"));
		}

		OGrid<Report> oGrid = new OGrid<>();
		oGrid
			.setColumns(columnList)
			.setMultiSort(false)
			.addToolbarButton(new OExportExcelButton<>(MetisIcon.EXPORT_EXCEL, this)
				.setMaxRowsCount(Integer.MAX_VALUE)
				.setFileName("ReportsList"))
			.setHeight(gridHeight)
			.setWidth(gridWidth)
			.setFit(gridFit);

		grid = new WDataGrid<>("grid", oGrid, this);
		add(grid);

		// ---------------

		form.setVisible(formVisible);
		grid.setEnabled(gridEnabled || !formVisible);

		if (invisibleFormItems != null) {
			for (String formItem : invisibleFormItems) {
				floatTable.get(formItem).setVisible(false);
			}
		}

		if (removeColumns != null) {
			for (String column : removeColumns) {
				columnList.removeColumn(column);
			}
		}
	}

	// ------------------------------

	public ReportListDPage setFormVisible(boolean formVisible) {
		this.formVisible = formVisible;
		return this;
	}

	public ReportListDPage setInvisibleFormItems(String... invisibleFormItems) {
		this.invisibleFormItems = invisibleFormItems;
		return this;
	}

	public ReportListDPage setGridHeight(OSize gridHeight) {
		this.gridHeight = gridHeight;
		return this;
	}

	public ReportListDPage setGridWidth(OSize gridWidth) {
		this.gridWidth = gridWidth;
		return this;
	}

	public ReportListDPage setGridFit(Boolean gridFit) {
		this.gridFit = gridFit;
		return this;
	}

	public ReportListDPage setGridEnabled(boolean gridEnabled) {
		this.gridEnabled = gridEnabled;
		return this;
	}

	public ReportListDPage setRemoveColumns(String... removeColumns) {
		this.removeColumns = removeColumns;
		return this;
	}

	// ------------------------------ IGridDataSource

	@Override
	public List<Report> list(long pageIndex, long pageSize, List<WSortField> sortFields) {
		return reportService.search(filter, pageIndex, pageSize);
	}

	@Override
	public long count() {
		return reportService.count(filter);
	}

	@Override
	public IModel<Report> model(Report object) {
		return new WModel<>(object);
	}
}