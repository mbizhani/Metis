package org.devocative.metis.web.dpage.data.menu;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.dpage.data.DataViewFilterPanel;
import org.devocative.metis.web.dpage.data.DataViewGridPanel;
import org.devocative.wickomp.WPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.util.HashMap;
import java.util.Map;

public class ReportExecutorPanel extends WPanel {
	private static final long serialVersionUID = -4821094866554886611L;

	private static final Logger logger = LoggerFactory.getLogger(ReportExecutorPanel.class);

	private DataViewGridPanel mainGrid;
	private Report report;

	@Inject
	private IDataService dataService;

	// ------------------------------

	public ReportExecutorPanel(String id, Report report) {
		super(id);

		this.report = report;
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		DataVO dataVO = dataService.loadDataVO(report.getDataViewId());

		final Map<String, Object> filter = new HashMap<>();

		Form<Map<String, Object>> form = new Form<>("form");
		add(form);


		form.add(
			new DataViewFilterPanel("filterPanel", dataVO.getDataSourceId(), filter, dataVO.getAllFields())
		);
		form.add(new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
			private static final long serialVersionUID = -8066384058553336246L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				logger.debug("filter = {}", filter);
				mainGrid.loadData(target);
				target.appendJavaScript(String.format("$('#%s').datagrid('loading');", mainGrid.getGridHtmlId()));
			}
		});
		mainGrid = new DataViewGridPanel("mainGrid", dataVO, filter);
		add(mainGrid);

	}
}
