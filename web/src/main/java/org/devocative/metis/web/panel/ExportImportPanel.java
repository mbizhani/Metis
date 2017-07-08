package org.devocative.metis.web.panel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.request.handler.resource.ResourceRequestHandler;
import org.devocative.adroit.CalendarUtil;
import org.devocative.demeter.web.DPanel;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.iservice.data.IDataViewService;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.form.WFileInput;
import org.devocative.wickomp.resource.OutputStreamResource;

import javax.inject.Inject;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Date;

public class ExportImportPanel extends DPanel {
	private static final long serialVersionUID = -896140188954016556L;

	private WFileInput file;

	@Inject
	private IDataViewService dataViewService;

	public ExportImportPanel(String id) {
		super(id);

		add(new Link<Void>("export") {
			private static final long serialVersionUID = -8486125182073683336L;

			@Override
			public void onClick() {
				String fileName = String.format("exportDataView-%s.xml", CalendarUtil.toPersian(new Date(), "yyyyMMdd"));
				OutputStreamResource out = new OutputStreamResource("text/xml", fileName) {
					private static final long serialVersionUID = -8667216585656718440L;

					@Override
					protected void handleStream(OutputStream stream) throws IOException {
						dataViewService.exportAll(stream);
					}
				};

				getRequestCycle().scheduleRequestHandlerAfterCurrent(new ResourceRequestHandler(out, null));
			}
		});

		Form<Void> form = new Form<>("form");
		file = new WFileInput("file");
		file.setRequired(true);
		form.add(file);
		form.add(new DAjaxButton("import", MetisIcon.UPLOAD) {
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
		add(form);
	}
}
