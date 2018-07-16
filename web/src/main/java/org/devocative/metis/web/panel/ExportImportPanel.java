package org.devocative.metis.web.panel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.model.PropertyModel;
import org.devocative.demeter.web.DPanel;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.MetisPrivilegeKey;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.iservice.connection.IDBConnectionGroupService;
import org.devocative.metis.iservice.data.IDataGroupService;
import org.devocative.metis.iservice.data.IDataViewService;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.WebUtil;
import org.devocative.wickomp.form.WFileInput;
import org.devocative.wickomp.form.WSelectionInput;

import javax.inject.Inject;
import java.util.List;

public class ExportImportPanel extends DPanel {
	private static final long serialVersionUID = -896140188954016556L;

	private WebMarkupContainer panel;
	private WFileInput file;

	@Inject
	private IDataViewService dataViewService;

	@Inject
	private IDataGroupService dataGroupService;

	@Inject
	private IDBConnectionGroupService dbConnectionGroupService;

	private List<DataGroup> dataGroups;

	private DBConnectionGroup dbConnectionGroup;

	private String dataViewNames;

	// ------------------------------

	public ExportImportPanel(String id) {
		super(id);

		panel = new WebMarkupContainer("panel");
		panel.setOutputMarkupId(true);
		add(panel);

		WebMarkupContainer exportPanel = new WebMarkupContainer("exportPanel");
		exportPanel.setVisible(hasPermission(MetisPrivilegeKey.DataViewExport));
		panel.add(exportPanel);

		Form<Void> exportForm = new Form<>("exportForm");
		exportForm.add(new WSelectionInput("dataGroups", new PropertyModel(this, "dataGroups"), dataGroupService.list(), true));
		exportForm.add(new WSelectionInput("dbConnectionGroup", new PropertyModel(this, "dbConnectionGroup"), dbConnectionGroupService.list(), false));
		exportForm.add(new TextArea<>("dataViewNames", new PropertyModel<>(this, "dataViewNames")));
		exportForm.add(new DAjaxButton("export") {
			private static final long serialVersionUID = -8486125182073683336L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				String fileId = dataViewService.exportAll(dataGroups, dbConnectionGroup, dataViewNames);
				target.appendJavaScript(String.format("location.href='%s';", UrlUtil.getFileUri(fileId)));
			}
		});
		exportPanel.add(exportForm);

		// --------------- IMPORT

		WebMarkupContainer importPanel = new WebMarkupContainer("importPanel");
		importPanel.setVisible(hasPermission(MetisPrivilegeKey.DataViewImport));
		panel.add(importPanel);

		FeedbackPanel feedback = new FeedbackPanel("feedback");
		feedback.setEscapeModelStrings(false).setOutputMarkupId(true);
		importPanel.add(feedback);

		Form<Void> importForm = new Form<>("importForm");
		file = new WFileInput("file");
		file
			.setMultiple(true)
			.setRequired(true);
		importForm.add(file);
		importForm.add(new DAjaxButton("import", MetisIcon.UPLOAD) {
			private static final long serialVersionUID = -6659127554044856922L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				List<FileUpload> fileUploads = file.getFileUpload();
				for (FileUpload fileUpload : fileUploads) {
					try {
						dataViewService.importAll(fileUpload.getInputStream());

						info("Ok <br/>File: " + fileUpload.getClientFileName());
					} catch (Exception e) {
						error("Error <br/>File: " + fileUpload.getClientFileName() + "<br/>Message: " + e.getMessage());
					}

					target.add(feedback);
				}
			}
		});
		importPanel.add(importForm);
	}

	@Override
	protected void onAfterRender() {
		super.onAfterRender();

		WebUtil.writeJQueryCall(String.format("$('#%s').tabs()", panel.getMarkupId()), true);
	}
}
