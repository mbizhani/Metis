package org.devocative.metis.web.panel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.model.PropertyModel;
import org.devocative.demeter.web.DPanel;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.iservice.connection.IDBConnectionGroupService;
import org.devocative.metis.iservice.data.IDataGroupService;
import org.devocative.metis.iservice.data.IDataViewService;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.form.WFileInput;
import org.devocative.wickomp.form.WSelectionInput;

import javax.inject.Inject;
import java.io.IOException;

public class ExportImportPanel extends DPanel {
	private static final long serialVersionUID = -896140188954016556L;

	private WFileInput file;

	@Inject
	private IDataViewService dataViewService;

	@Inject
	private IDataGroupService dataGroupService;

	@Inject
	private IDBConnectionGroupService dbConnectionGroupService;

	private DataGroup dataGroup;

	private DBConnectionGroup dbConnectionGroup;

	public ExportImportPanel(String id) {
		super(id);

		Form<Void> exportForm = new Form<>("exportForm");
		exportForm.add(new WSelectionInput("dataGroup", new PropertyModel(this, "dataGroup"), dataGroupService.list(), false));
		exportForm.add(new WSelectionInput("dbConnectionGroup", new PropertyModel(this, "dbConnectionGroup"), dbConnectionGroupService.list(), false));
		exportForm.add(new DAjaxButton("export") {
			private static final long serialVersionUID = -8486125182073683336L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				String fileId = dataViewService.exportAll(dataGroup, dbConnectionGroup);
				target.appendJavaScript(String.format("location.href='%s';", UrlUtil.getFileUri(fileId)));
			}
		});
		add(exportForm);

		// --------------- IMPORT

		Form<Void> importForm = new Form<>("importForm");
		file = new WFileInput("file");
		file.setRequired(true);
		importForm.add(file);
		importForm.add(new DAjaxButton("import", MetisIcon.UPLOAD) {
			private static final long serialVersionUID = -6659127554044856922L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				FileUpload fileUpload = file.getFileUpload();
				if (fileUpload != null) {
					try {
						dataViewService.importAll(fileUpload.getInputStream());
					} catch (IOException e) {
						throw new RuntimeException(e);
					}
				}
			}
		});
		add(importForm);
	}
}
