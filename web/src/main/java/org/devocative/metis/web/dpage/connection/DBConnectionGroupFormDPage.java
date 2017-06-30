package org.devocative.metis.web.dpage.connection;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.iservice.connection.IDBConnectionGroupService;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.form.WFileInput;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.html.window.WModalWindow;

import javax.inject.Inject;
import java.nio.charset.Charset;
import java.util.Collections;
import java.util.List;

public class DBConnectionGroupFormDPage extends DPage {
	private static final long serialVersionUID = 659487991L;

	@Inject
	private IDBConnectionGroupService dBConnectionGroupService;

	private DBConnectionGroup entity;

	private WFileInput config;

	// ------------------------------

	public DBConnectionGroupFormDPage(String id) {
		this(id, new DBConnectionGroup());
	}

	// Main Constructor - For Ajax Call
	public DBConnectionGroupFormDPage(String id, DBConnectionGroup entity) {
		super(id, Collections.<String>emptyList());

		this.entity = entity;
	}

	// ---------------

	// Main Constructor - For REST Call
	public DBConnectionGroupFormDPage(String id, List<String> params) {
		super(id, params);

		this.entity = params != null && !params.isEmpty() ?
			dBConnectionGroupService.load(Long.valueOf(params.get(0))) :
			new DBConnectionGroup();
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		WFloatTable floatTable = new WFloatTable("floatTable");
		floatTable.add(new WTextInput("name")
			.setRequired(true)
			.setLabel(new ResourceModel("DBConnectionGroup.name")));
		floatTable.add(new WTextInput("driver")
			.setLabel(new ResourceModel("DBConnectionGroup.driver")));
		floatTable.add(new WTextInput("url")
			.setLabel(new ResourceModel("DBConnectionGroup.url")));
		floatTable.add(new WTextInput("testQuery")
			.setLabel(new ResourceModel("DBConnectionGroup.testQuery")));

		config = new WFileInput("config");
		config.setLabel(new ResourceModel("DBConnection.config"));
		floatTable.add(config);

		Form<DBConnectionGroup> form = new Form<>("form", new CompoundPropertyModel<>(entity));
		form.add(floatTable);

		form.add(new DAjaxButton("save", new ResourceModel("label.save"), MetisIcon.SAVE) {
			private static final long serialVersionUID = -1236533665L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				String mappingXML = null;
				FileUpload fileUpload = config.getFileUpload();
				if (fileUpload != null) {
					mappingXML = new String(fileUpload.getBytes(), Charset.forName("UTF-8"));
				}
				dBConnectionGroupService.saveOrUpdate(entity, mappingXML);

				if (!WModalWindow.closeParentWindow(DBConnectionGroupFormDPage.this, target)) {
					UrlUtil.redirectTo(DBConnectionGroupListDPage.class);
				}
			}
		});
		add(form);
	}
}