package org.devocative.metis.web.panel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.resource.ContentDisposition;
import org.devocative.demeter.web.DPanel;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.MetisPrivilegeKey;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.iservice.connection.IDBConnectionGroupService;
import org.devocative.metis.iservice.data.IDataGroupService;
import org.devocative.metis.iservice.data.IDataViewService;
import org.devocative.metis.iservice.data.IReportService;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.WebUtil;
import org.devocative.wickomp.form.WFileInput;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.html.WMessager;

import javax.inject.Inject;
import java.util.List;

public class ExportImportPanel extends DPanel {
	private static final long serialVersionUID = -896140188954016556L;

	private WebMarkupContainer panel;

	@Inject
	private IDataViewService dataViewService;

	@Inject
	private IDataGroupService dataGroupService;

	@Inject
	private IDBConnectionGroupService dbConnectionGroupService;

	@Inject
	private IReportService reportService;

	private List<DataGroup> dataGroups;

	private DBConnectionGroup dbConnectionGroup;

	private String dataViewNames;

	private List<Report> reports;

	private String sentDBConnection;

	// ------------------------------

	public ExportImportPanel(String id) {
		super(id);
	}

	// ------------------------------

	public ExportImportPanel setSentDBConnection(String sentDBConnection) {
		this.sentDBConnection = sentDBConnection;
		return this;
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		panel = new WebMarkupContainer("panel");
		panel.setOutputMarkupId(true);
		add(panel);

		exportAll();

		importAll();

		exportReport();

		importReport();
	}

	@Override
	protected void onAfterRender() {
		super.onAfterRender();

		WebUtil.writeJQueryCall(String.format("$('#%s').tabs()", panel.getMarkupId()), true);
	}

	// ------------------------------

	private void exportAll() {
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
				if (dataGroups != null || dataViewNames != null) {
					String fileId = dataViewService.exportAll(dataGroups, dbConnectionGroup, dataViewNames);
					target.appendJavaScript(String.format("location.href='%s';", UrlUtil.getFileUri(fileId, ContentDisposition.ATTACHMENT)));
				} else {
					WMessager.show("Error", "'Data Group' or 'DataView Names' are required!", target);
				}
			}
		});
		exportPanel.add(exportForm);
	}

	private void importAll() {
		WebMarkupContainer importPanel = new WebMarkupContainer("importPanel");
		importPanel.setVisible(hasPermission(MetisPrivilegeKey.DataViewImport));
		panel.add(importPanel);

		FeedbackPanel feedback = new FeedbackPanel("feedback");
		feedback.setEscapeModelStrings(false).setOutputMarkupId(true);
		importPanel.add(feedback);

		Form<Void> importForm = new Form<>("importForm");
		WFileInput file = new WFileInput("file");
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

	private void exportReport() {
		WebMarkupContainer exportReportPanel = new WebMarkupContainer("exportReportPanel");
		exportReportPanel.setVisible(hasPermission(MetisPrivilegeKey.ReportExport));
		panel.add(exportReportPanel);

		Form<Void> form = new Form<>("form");
		form.add(new WSelectionInput("dataGroups", new PropertyModel(this, "dataGroups"), dataGroupService.list(), true));
		form.add(new WSelectionInput("reports", new PropertyModel(this, "reports"), reportService.list(), true));
		form.add(new DAjaxButton("export") {
			private static final long serialVersionUID = 5291830965686594210L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				if (dataGroups != null || reports != null) {
					String fileId = dataViewService.exportReport(dataGroups, reports);
					target.appendJavaScript(String.format("location.href='%s';", UrlUtil.getFileUri(fileId, ContentDisposition.ATTACHMENT)));
				} else {
					WMessager.show("Error", "'Data Group' or 'Report' are required!", target);
				}
			}
		});
		exportReportPanel.add(form);
	}

	private void importReport() {
		WebMarkupContainer importReportPanel = new WebMarkupContainer("importReportPanel");
		importReportPanel.setVisible(hasPermission(MetisPrivilegeKey.ReportImport));
		panel.add(importReportPanel);

		FeedbackPanel feedback = new FeedbackPanel("feedback");
		feedback.setEscapeModelStrings(false).setOutputMarkupId(true);
		importReportPanel.add(feedback);

		Form<Void> form = new Form<>("form");
		WFileInput file = new WFileInput("file");
		file
			.setMultiple(true)
			.setRequired(true);
		form.add(file);
		form.add(new DAjaxButton("import", MetisIcon.UPLOAD) {
			private static final long serialVersionUID = 5829859770226623834L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				List<FileUpload> fileUploads = file.getFileUpload();
				for (FileUpload fileUpload : fileUploads) {
					try {
						dataViewService.importReport(fileUpload.getInputStream(), sentDBConnection);

						info("Ok <br/>File: " + fileUpload.getClientFileName());
					} catch (Exception e) {
						error("Error <br/>File: " + fileUpload.getClientFileName() + "<br/>Message: " + e.getMessage());
					}

					target.add(feedback);
				}
			}
		});
		importReportPanel.add(form);
	}
}
