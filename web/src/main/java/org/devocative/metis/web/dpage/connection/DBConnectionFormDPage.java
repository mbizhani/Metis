package org.devocative.metis.web.dpage.connection;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.validation.AbstractFormValidator;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.form.WFileInput;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.validator.WEqualInputValidator;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.html.window.WModalWindow;

import javax.inject.Inject;
import java.nio.charset.Charset;
import java.util.Collections;
import java.util.List;

public class DBConnectionFormDPage extends DPage {
	private static final long serialVersionUID = -1873188792L;

	@Inject
	private IDBConnectionService dBConnectionService;

	private DBConnection entity;

	private WTextInput password;
	private WFileInput config;
	private FormComponent driver, url, testQuery, group;

	// ------------------------------

	public DBConnectionFormDPage(String id) {
		this(id, new DBConnection());
	}

	// Main Constructor - For Ajax Call
	public DBConnectionFormDPage(String id, DBConnection entity) {
		super(id, Collections.emptyList());

		this.entity = entity;
	}

	// ---------------

	// Main Constructor - For REST Call
	public DBConnectionFormDPage(String id, List<String> params) {
		super(id, params);

		this.entity = params != null && !params.isEmpty() ?
			dBConnectionService.load(Long.valueOf(params.get(0))) :
			new DBConnection();
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		WFloatTable floatTable = new WFloatTable("floatTable");
		floatTable.add(new WTextInput("name")
			.setRequired(true)
			.setLabel(new ResourceModel("DBConnection.name")));
		floatTable.add(driver = new WTextInput("driver")
			.setLabel(new ResourceModel("DBConnection.driver")));
		floatTable.add(url = new WTextInput("url")
			.setLabel(new ResourceModel("DBConnection.url")));
		floatTable.add(new WTextInput("username")
			.setRequired(true)
			.setLabel(new ResourceModel("DBConnection.username")));

		password = new WTextInput("password", new Model<>(), true);
		password
			.setLabel(new ResourceModel("DBConnection.password"))
			.setRequired(entity.getId() == null);
		floatTable.add(password);

		WTextInput password2 = new WTextInput("password2", new Model<>(), true);
		password2
			.setLabel(new ResourceModel("DBConnection.password2"))
			.setRequired(entity.getId() == null);
		floatTable.add(password2);

		floatTable.add(new WTextInput("schema")
			.setLabel(new ResourceModel("DBConnection.schema")));
		floatTable.add(testQuery = new WTextInput("testQuery")
			.setLabel(new ResourceModel("DBConnection.testQuery")));
		floatTable.add(group = new WSelectionInput("group", dBConnectionService.getGroupList(), false)
			.setRequired(true)
			.setLabel(new ResourceModel("DBConnection.group")));
		floatTable.add(new WTextInput("customParam1")
			.setLabel(new ResourceModel("DBConnection.customParam1")));

		config = new WFileInput("config");
		config.setLabel(new ResourceModel("DBConnection.config"));
		floatTable.add(config);

		Form<DBConnection> form = new Form<>("form", new CompoundPropertyModel<>(entity));
		form.add(new WEqualInputValidator(password, password2));
		form.add(new AbstractFormValidator() {
			private static final long serialVersionUID = -7704498471312365987L;

			@Override
			public FormComponent<?>[] getDependentFormComponents() {
				return new FormComponent<?>[]{driver, url, testQuery};
			}

			@Override
			public void validate(Form<?> form) {
				DBConnectionGroup connectionGroup = (DBConnectionGroup) group.getConvertedInput();

				if ((connectionGroup == null || connectionGroup.getDriver() == null) && driver.getConvertedInput() == null) {
					error(driver, "Required");
				}
				if ((connectionGroup == null || connectionGroup.getDriver() == null) && url.getConvertedInput() == null) {
					error(url, "Required");
				}
				if ((connectionGroup == null || connectionGroup.getDriver() == null) && testQuery.getConvertedInput() == null) {
					error(testQuery, "Required");
				}
			}
		});
		form.add(floatTable);

		form.add(new DAjaxButton("save", new ResourceModel("label.save"), MetisIcon.SAVE) {
			private static final long serialVersionUID = 461568048L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				String mappingXML = null;
				List<FileUpload> fileUploads = config.getFileUpload();
				if (!fileUploads.isEmpty()) {
					mappingXML = new String(fileUploads.get(0).getBytes(), Charset.forName("UTF-8"));
				}
				dBConnectionService.saveOrUpdate(entity, mappingXML, password.getModelObject());

				if (!WModalWindow.closeParentWindow(DBConnectionFormDPage.this, target)) {
					UrlUtil.redirectTo(DBConnectionListDPage.class);
				}
			}
		});
		add(form);
	}

}