package org.devocative.metis.web.dPage;

import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.CompoundPropertyModel;
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

		//TODO edit

		final Form<DBConnection> form = new Form<>("form", new CompoundPropertyModel<>(new DBConnection()));
		form.setMultiPart(true);

		form.add(new WTextInput("name"));
		form.add(new WTextInput("driver"));
		form.add(new WTextInput("url"));
		form.add(new WTextInput("username"));
		form.add(new WTextInput("password"));
		form.add(new WTextInput("schema"));
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
