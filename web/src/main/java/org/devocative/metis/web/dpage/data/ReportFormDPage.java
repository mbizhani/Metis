package org.devocative.metis.web.dpage.data;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.iservice.data.IReportService;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.html.window.WModalWindow;

import javax.inject.Inject;
import java.util.Collections;
import java.util.List;

public class ReportFormDPage extends DPage {
	private static final long serialVersionUID = 541955132L;

	@Inject
	private IReportService reportService;

	private Report entity;

	// ------------------------------

	public ReportFormDPage(String id) {
		this(id, new Report());
	}

	// Main Constructor - For Ajax Call
	public ReportFormDPage(String id, Report entity) {
		super(id, Collections.<String>emptyList());

		this.entity = entity;
	}

	// ---------------

	// Main Constructor - For REST Call
	public ReportFormDPage(String id, List<String> params) {
		super(id, params);

		this.entity = params != null && !params.isEmpty() ?
			reportService.load(Long.valueOf(params.get(0))) :
			new Report();
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		WFloatTable floatTable = new WFloatTable("floatTable");
		//floatTable.setEqualWidth(true);
		floatTable.add(new WTextInput("title")
			.setRequired(true)
			.setLabel(new ResourceModel("Report.title")));
		floatTable.add(new WSelectionInput("dataView", reportService.getDataViewList(), false)
			.setRequired(true)
			.setLabel(new ResourceModel("Report.dataView")));
		floatTable.add(new WSelectionInput("groups", reportService.getGroupsList(), true)
			.setRequired(true)
			.setLabel(new ResourceModel("Report.groups")));

		Form<Report> form = new Form<>("form", new CompoundPropertyModel<>(entity));
		form.add(floatTable);
		form.add(new TextArea("config")
			.setLabel(new ResourceModel("Report.config")));

		form.add(new DAjaxButton("save", new ResourceModel("label.save"), MetisIcon.SAVE) {
			private static final long serialVersionUID = -1267673564L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				reportService.saveOrUpdate(entity);

				if (!WModalWindow.closeParentWindow(ReportFormDPage.this, target)) {
					UrlUtil.redirectTo(ReportListDPage.class);
				}
			}
		});
		add(form);
	}
}