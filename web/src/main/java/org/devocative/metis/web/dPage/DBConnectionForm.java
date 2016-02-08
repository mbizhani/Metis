package org.devocative.metis.web.dPage;

import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.PasswordTextField;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.form.validation.EqualPasswordInputValidator;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.component.grid.ORESTLinkColumn;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.data.WGridDataSource;
import org.devocative.wickomp.data.WSortField;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.grid.OGrid;
import org.devocative.wickomp.grid.WDataGrid;
import org.devocative.wickomp.grid.column.OColumn;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.html.icon.FontAwesome;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.util.List;

public class DBConnectionForm extends DPage {

	@Inject
	private IDBConnectionService connectionService;

	private FileUploadField configFile;

	public DBConnectionForm(String id, List<String> params) {
		super(id, params);

		DBConnection dbConnection = params.size() == 0 ?
			new DBConnection() :
			connectionService.getByName(params.get(0));

		add(new FeedbackPanel("feedback"));

		FormComponent password, password2;

		final Form<DBConnection> form = new Form<>("form", new CompoundPropertyModel<>(dbConnection));
		form.setMultiPart(true);

		form.add(new WTextInput("name")
			.setLabel(new ResourceModel("DBConnection.name"))
			.setRequired(true));
		form.add(new WTextInput("driver")
			.setLabel(new ResourceModel("DBConnection.driver"))
			.setRequired(true));
		form.add(new WTextInput("url")
			.setLabel(new ResourceModel("DBConnection.url"))
			.setRequired(true));
		form.add(new WTextInput("username")
			.setLabel(new ResourceModel("DBConnection.username"))
			.setRequired(true));
		form.add(password = new PasswordTextField("password")
			.setLabel(new ResourceModel("DBConnection.password"))
			.setRequired(true));
		form.add(password2 = new PasswordTextField("password2", new Model<String>())
			.setLabel(new ResourceModel("DBConnection.password2"))
			.setRequired(true));
		form.add(new WTextInput("schema"));
		form.add(new WTextInput("testQuery")
			.setLabel(new ResourceModel("DBConnection.testQuery"))
			.setRequired(true));
		form.add(configFile = new FileUploadField("configFile", new WModel<List<FileUpload>>()));
		form.add(new Button("save") {
			@Override
			public void onSubmit() {
				String mappingXML = null;
				FileUpload fileUpload = configFile.getFileUpload();
				if (fileUpload != null) {
					mappingXML = new String(fileUpload.getBytes());
				}
				connectionService.saveOrUpdate(form.getModelObject(), mappingXML);
			}
		});
		add(form);

		form.add(new EqualPasswordInputValidator(password, password2));

		OColumnList<DBConnection> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("DBConnection.name"), "name"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("DBConnection.driver"), "driver"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("DBConnection.url"), "url"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("DBConnection.username"), "username"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("DBConnection.schema"), "schema"));
		columnList.add(new OColumn<DBConnection>(new ResourceModel("DBConnection.testQuery")) {
			@Override
			public String cellValue(DBConnection bean, String id, int colNo, String url) {
				return bean.getTestQuery() != null ?
					new FontAwesome("check", "green").toString() :
					new FontAwesome("times", "red").toString();
			}
		});
		columnList.add(new OColumn<DBConnection>(new ResourceModel("DBConnection.config")) {
			@Override
			public String cellValue(DBConnection bean, String id, int colNo, String url) {
				return bean.getConfigId() != null ?
					new FontAwesome("check", "green").toString() :
					new FontAwesome("times", "red").toString();
			}
		});
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("entity.creationDate", "Creation Date"), "creationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference()));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("entity.creatorUser", "Creator User"), "creatorUser"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("entity.modificationDate", "Modification Date"), "modificationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference()));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("entity.modifierUser", "Modifier User"), "modifierUser"));

		columnList.add(new ORESTLinkColumn<DBConnection>(new Model<String>(), DBConnectionForm.class, "name",
			new FontAwesome("pencil", "green", new ResourceModel("label.edit", "Edit"))));

		OGrid<DBConnection> oGrid = new OGrid<>();
		oGrid
			.setColumns(columnList)
			.setMultiSort(false)
			.setHeight(OSize.fixed(350))
			.setWidth(OSize.percent(100));

		add(new WDataGrid<>("grid", oGrid, new WGridDataSource<DBConnection>() {
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
}
