package org.devocative.metis.web.dpage.data.menu;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.ResourceModel;
import org.devocative.adroit.ConfigUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.data.IReportService;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.FilterInputParamsVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.MetisWebParam;
import org.devocative.metis.web.dpage.data.DataViewFilterPanel2;
import org.devocative.metis.web.dpage.data.DataViewGridPanel;
import org.devocative.wickomp.WPanel;
import org.devocative.wickomp.WebUtil;
import org.devocative.wickomp.html.WEasyLayout;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class ReportExecutorPanel extends WPanel {
	private static final long serialVersionUID = -4821094866554886611L;

	private static final Logger logger = LoggerFactory.getLogger(ReportExecutorPanel.class);

	private DataViewGridPanel mainGrid;
	private String reportId;
	private String sentDBConnection;

	private boolean searchOnStart = false;
	private DAjaxButton search;

	@Inject
	private IReportService reportService;

	@Inject
	private IDataService dataService;

	// ------------------------------

	public ReportExecutorPanel(String id, String reportId, String sentDBConnection) {
		super(id);

		this.reportId = reportId;
		this.sentDBConnection = sentDBConnection;
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		final Report report = reportService.load(reportId);
		final DataVO dataVO = dataService.loadDataVO(report.getDataViewId());
		dataVO.setReportId(report.getId());

		final Map<String, Object> filter = new HashMap<>();

		Map<String, List<String>> targetParam = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
		dataService.processDynamicFilterAndParam(report.getConfig(), targetParam, null, null);

		FilterInputParamsVO inputParamsVO = new FilterInputParamsVO();
		inputParamsVO.putAll(targetParam);

		Form<Map<String, Object>> form = new Form<>("form");
		form.add(
			new DataViewFilterPanel2("filterPanel", dataVO, filter)
				.setInputParamsVO(inputParamsVO)
		);

		search = new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
			private static final long serialVersionUID = -8066384058553336246L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				reportService.assertReportAuthorization(report, sentDBConnection);

				logger.debug("filter = {}", filter);
				mainGrid.loadData(target);
				//target.appendJavaScript(String.format("$('#%s').datagrid('loading');", mainGrid.getGridHtmlId()));
			}
		};
		search.setOutputMarkupId(true);
		form.add(search);

		searchOnStart = inputParamsVO.containsKey(MetisWebParam.SEARCH_ON_START) &&
			"1".equals(inputParamsVO.getAsString(MetisWebParam.SEARCH_ON_START));

		mainGrid = new DataViewGridPanel("mainGrid", dataVO, filter);
		mainGrid
			.setInputParamsVO(inputParamsVO)
			.setMultiSelect(ConfigUtil.getBoolean(MetisConfigKey.ReportGridMultiSelect))
		;

		WebMarkupContainer filterPanel = new WebMarkupContainer("filterPanel");
		filterPanel.add(form);

		WEasyLayout layout = new WEasyLayout("layout");
		layout.add(filterPanel);
		layout.add(mainGrid);
		layout.setWestOfLTRDir(filterPanel);
		add(layout);
	}

	@Override
	protected void onAfterRender() {
		super.onAfterRender();

		if (searchOnStart) {
			String script = String.format("$('#%s').click();", search.getMarkupId());
			WebUtil.writeJQueryCall(script, true);
		}
	}
}
