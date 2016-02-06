package org.devocative.metis.web.dPage;

import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.form.WTextInput;

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

		final Form<DBConnection> form = new Form<>("form", new CompoundPropertyModel<>(dbConnection));
		form.setMultiPart(true);

		form.add(new WTextInput("name").setLabel(new ResourceModel("DBConnection.name")).setRequired(true));
		form.add(new WTextInput("driver").setLabel(new ResourceModel("DBConnection.driver")).setRequired(true));
		form.add(new WTextInput("url").setLabel(new ResourceModel("DBConnection.url")).setRequired(true));
		form.add(new WTextInput("username").setLabel(new ResourceModel("DBConnection.username")).setRequired(true));
		form.add(new WTextInput("password"));
		form.add(new WTextInput("schema"));
		form.add(new WTextInput("testQuery").setLabel(new ResourceModel("DBConnection.testQuery")).setRequired(true));
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
	}
}
