package org.devocative.metis.web.dpage.connection;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.component.DButton;
import org.devocative.demeter.web.component.grid.ORESTLinkColumn;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.iservice.connection.IDBConnectionGroupService;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.grid.IGridDataSource;
import org.devocative.wickomp.grid.OGrid;
import org.devocative.wickomp.grid.WDataGrid;
import org.devocative.wickomp.grid.WSortField;
import org.devocative.wickomp.grid.column.OColumn;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.html.WEasyLayout;
import org.devocative.wickomp.opt.OHorizontalAlign;

import javax.inject.Inject;
import java.util.List;

public class DBConnectionGroupForm extends DPage {
	private static final long serialVersionUID = -1203339338350100077L;

	@Inject
	private IDBConnectionGroupService connectionGroupService;

	private FileUploadField configFile;

	public DBConnectionGroupForm(String id, List<String> params) {
		super(id, params);

		WebMarkupContainer west = new WebMarkupContainer("west");
		WEasyLayout layout = new WEasyLayout("layout");
		layout.setWest(west);
		add(layout);

		DBConnectionGroup dbConnectionGroup = params.size() == 0 ?
			new DBConnectionGroup() :
			connectionGroupService.getByName(params.get(0));

		final Form<DBConnectionGroup> form = new Form<>("form", new CompoundPropertyModel<>(dbConnectionGroup));
		form.setMultiPart(true);

		form.add(new WTextInput("name")
			.setLabelVisible(false)
			.setLabel(new ResourceModel("DBConnection.name"))
			.setRequired(true));
		form.add(new WTextInput("driver")
			.setLabelVisible(false)
			.setLabel(new ResourceModel("DBConnection.driver")));
		form.add(new WTextInput("url")
			.setLabelVisible(false)
			.setLabel(new ResourceModel("DBConnection.url")));
		form.add(new WTextInput("testQuery")
			.setLabelVisible(false)
			.setLabel(new ResourceModel("DBConnection.testQuery")));
		form.add(configFile = new FileUploadField("configFile", new WModel<List<FileUpload>>()));
		form.add(new DButton("save", new ResourceModel("label.save"), MetisIcon.SAVE) {
			private static final long serialVersionUID = 570113399579714626L;

			@Override
			protected void onFormSubmit() {
				String mappingXML = null;
				FileUpload fileUpload = configFile.getFileUpload();
				if (fileUpload != null) {
					mappingXML = new String(fileUpload.getBytes());
				}
				connectionGroupService.saveOrUpdate(form.getModelObject(), mappingXML);
			}
		});
		west.add(form);

		OColumnList<DBConnectionGroup> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<DBConnectionGroup>(new ResourceModel("DBConnection.name"), "name"));
		columnList.add(new OPropertyColumn<DBConnectionGroup>(new ResourceModel("DBConnection.driver"), "driver"));
		columnList.add(new OPropertyColumn<DBConnectionGroup>(new ResourceModel("DBConnection.url"), "url"));
		columnList.add(new OColumn<DBConnectionGroup>(new ResourceModel("DBConnection.testQuery")) {
			private static final long serialVersionUID = -6779049575847536279L;

			@Override
			public String cellValue(DBConnectionGroup bean, String id, int colNo, String url) {
				return bean.getTestQuery() != null ? MetisIcon.TRUE.toString() : MetisIcon.FALSE.toString();
			}

			@Override
			public String footerCellValue(Object bean, int colNo, String url) {
				return null;
			}
		}.setAlign(OHorizontalAlign.Center));
		columnList.add(new OColumn<DBConnectionGroup>(new ResourceModel("DBConnection.config")) {
			private static final long serialVersionUID = 7514001254778483999L;

			@Override
			public String cellValue(DBConnectionGroup bean, String id, int colNo, String url) {
				return bean.getConfigId() != null ? MetisIcon.TRUE.toString() : MetisIcon.FALSE.toString();
			}

			@Override
			public String footerCellValue(Object bean, int colNo, String url) {
				return null;
			}
		}.setAlign(OHorizontalAlign.Center));
		columnList.add(new OPropertyColumn<DBConnectionGroup>(new ResourceModel("entity.creationDate", "Creation Date"), "creationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference()));
		columnList.add(new OPropertyColumn<DBConnectionGroup>(new ResourceModel("entity.creatorUser", "Creator User"), "creatorUser"));
		columnList.add(new OPropertyColumn<DBConnectionGroup>(new ResourceModel("entity.modificationDate", "Modification Date"), "modificationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference()));
		columnList.add(new OPropertyColumn<DBConnectionGroup>(new ResourceModel("entity.modifierUser", "Modifier User"), "modifierUser"));

		columnList.add(new ORESTLinkColumn<DBConnectionGroup>(new Model<String>(), DBConnectionGroupForm.class, "name", MetisIcon.EDIT));

		OGrid<DBConnectionGroup> oGrid = new OGrid<>();
		oGrid
			.setColumns(columnList)
			.setMultiSort(false)
			.setFit(true);

		layout.add(new WDataGrid<>("grid", oGrid, new IGridDataSource<DBConnectionGroup>() {
			private static final long serialVersionUID = 5895190921902445124L;

			@Override
			public List<DBConnectionGroup> list(long pageIndex, long pageSize, List<WSortField> sortFields) {
				return connectionGroupService.search(pageIndex, pageSize);
			}

			@Override
			public long count() {
				return connectionGroupService.count();
			}

			@Override
			public IModel<DBConnectionGroup> model(DBConnectionGroup object) {
				return new WModel<>(object);
			}
		}));
	}
}
