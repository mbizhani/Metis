package org.devocative.metis.web.dPage;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.PasswordTextField;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.form.validation.EqualPasswordInputValidator;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.component.DButton;
import org.devocative.demeter.web.component.grid.DDataGrid;
import org.devocative.demeter.web.component.grid.ORESTLinkColumn;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.iservice.IDBConnectionGroupService;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.data.WGridDataSource;
import org.devocative.wickomp.data.WSortField;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.grid.OGrid;
import org.devocative.wickomp.grid.column.OColumn;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.grid.column.link.OAjaxLinkColumn;
import org.devocative.wickomp.html.WEasyLayout;
import org.devocative.wickomp.opt.OHorizontalAlign;

import javax.inject.Inject;
import java.util.HashMap;
import java.util.List;

public class DBConnectionForm extends DPage {

	@Inject
	private IDBConnectionService connectionService;

	@Inject
	private IDBConnectionGroupService connectionGroupService;

	private FileUploadField configFile;

	public DBConnectionForm(String id, List<String> params) {
		super(id, params);

		WebMarkupContainer west = new WebMarkupContainer("west");
		WEasyLayout layout = new WEasyLayout("layout");
		layout.setWest(west);
		add(layout);

		DBConnection dbConnection = params.size() == 0 ?
			new DBConnection() :
			connectionService.getByName(params.get(0));

		FormComponent password, password2;

		final Form<DBConnection> form = new Form<>("form", new CompoundPropertyModel<>(dbConnection));
		form.setMultiPart(true);

		form.add(new WTextInput("name")
			.setLabelVisible(false)
			.setLabel(new ResourceModel("DBConnection.name"))
			.setRequired(true));
		form.add(new WSelectionInput("group", connectionGroupService.list(), false)
			.setLabelVisible(false)
			.setLabel(new ResourceModel("DBConnection.group"))
			.setRequired(true));
		form.add(new WTextInput("driver")
			.setLabelVisible(false)
			.setLabel(new ResourceModel("DBConnection.driver")));
		form.add(new WTextInput("url")
			.setLabelVisible(false)
			.setLabel(new ResourceModel("DBConnection.url")));
		form.add(new WTextInput("username")
			.setLabelVisible(false)
			.setLabel(new ResourceModel("DBConnection.username"))
			.setRequired(true));
		form.add(password = new PasswordTextField("password")
			.setLabel(new ResourceModel("DBConnection.password"))
			.setRequired(true));
		form.add(password2 = new PasswordTextField("password2", new Model<String>())
			.setLabel(new ResourceModel("DBConnection.password2"))
			.setRequired(true));
		form.add(new WTextInput("schema")
			.setLabelVisible(false));
		form.add(new WTextInput("testQuery")
			.setLabelVisible(false)
			.setLabel(new ResourceModel("DBConnection.testQuery")));
		form.add(configFile = new FileUploadField("configFile", new WModel<List<FileUpload>>()));
		form.add(new DButton("save", new ResourceModel("label.save"), MetisIcon.SAVE) {
			@Override
			protected void onFormSubmit() {
				String mappingXML = null;
				FileUpload fileUpload = configFile.getFileUpload();
				if (fileUpload != null) {
					mappingXML = new String(fileUpload.getBytes());
				}
				DBConnection modelObject = form.getModelObject();
				boolean valid = true;
				if (modelObject.getSafeDriver() == null) {
					valid = false;
					error(getString("Required", getMapModel("DBConnection.driver")));
				}
				if (modelObject.getSafeUrl() == null) {
					valid = false;
					error(getString("Required", getMapModel("DBConnection.url")));
				}
				if (modelObject.getSafeTestQuery() == null) {
					valid = false;
					error(getString("Required", getMapModel("DBConnection.testQuery")));
				}
				if (valid) {
					connectionService.saveOrUpdate(modelObject, mappingXML);
				}
			}
		});

		west.add(form);

		form.add(new EqualPasswordInputValidator(password, password2));

		OColumnList<DBConnection> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("DBConnection.name"), "name"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("DBConnection.group"), "group"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("DBConnection.driver"), "safeDriver"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("DBConnection.url"), "safeUrl"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("DBConnection.username"), "username"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("DBConnection.schema"), "schema"));
		columnList.add(new OColumn<DBConnection>(new ResourceModel("DBConnection.testQuery")) {
			@Override
			public String cellValue(DBConnection bean, String id, int colNo, String url) {
				return bean.getSafeTestQuery() != null ? MetisIcon.TRUE.toString() : MetisIcon.FALSE.toString();
			}
		}.setAlign(OHorizontalAlign.Center));
		columnList.add(new OColumn<DBConnection>(new ResourceModel("DBConnection.config")) {
			@Override
			public String cellValue(DBConnection bean, String id, int colNo, String url) {
				return bean.getSafeConfigId() != null ? MetisIcon.TRUE.toString() : MetisIcon.FALSE.toString();
			}
		}.setAlign(OHorizontalAlign.Center));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("entity.creationDate", "Creation Date"), "creationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference()));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("entity.creatorUser", "Creator User"), "creatorUser"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("entity.modificationDate", "Modification Date"), "modificationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference()));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("entity.modifierUser", "Modifier User"), "modifierUser"));

		columnList.add(new ORESTLinkColumn<DBConnection>(new Model<String>(), DBConnectionForm.class, "name", MetisIcon.EDIT));
		columnList.add(new OAjaxLinkColumn<DBConnection>(new Model<String>(), MetisIcon.CHECK_CONNECTION) {
			@Override
			public void onClick(AjaxRequestTarget target, IModel<DBConnection> rowData) {
				boolean b = connectionService.checkConnection(rowData.getObject().getId());
				String msg = b ? getString("label.true") : getString("label.false");
				target.appendJavaScript(String.format("alert('%s');", msg));
			}
		});

		OGrid<DBConnection> oGrid = new OGrid<>();
		oGrid
			.setColumns(columnList)
			.setMultiSort(false)
			.setFit(true)
		;

		layout.add(new DDataGrid<>("grid", oGrid, new WGridDataSource<DBConnection>() {
			@Override
			public List<DBConnection> list(long pageIndex, long pageSize, List<WSortField> sortFields) {
				return connectionService.search(pageIndex, pageSize);
			}

			@Override
			public long count() {
				return connectionService.count();
			}

			@Override
			public IModel<DBConnection> model(DBConnection object) {
				return new WModel<>(object);
			}
		}));
	}

	private Model getMapModel(String resource) {
		HashMap map = new HashMap<>();
		map.put("label", getString(resource));
		return new Model(map);
	}
}
