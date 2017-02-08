package org.devocative.metis.web.dpage.data.menu;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.data.IReportService;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.MetisWebParam;
import org.devocative.metis.web.dpage.data.DataViewFilterPanel;
import org.devocative.metis.web.dpage.data.DataViewGridPanel;
import org.devocative.wickomp.WPanel;
import org.devocative.wickomp.WebUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.util.*;

public class ReportExecutorPanel extends WPanel {
	private static final long serialVersionUID = -4821094866554886611L;

	private static final Logger logger = LoggerFactory.getLogger(ReportExecutorPanel.class);

	private DataViewGridPanel mainGrid;
	private Long reportId;

	private boolean searchOnStart = false;
	private DAjaxButton search;

	@Inject
	private IReportService reportService;

	@Inject
	private IDataService dataService;

	// ------------------------------

	public ReportExecutorPanel(String id, Long reportId) {
		super(id);

		this.reportId = reportId;
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		Report report = reportService.load(reportId);
		DataVO dataVO = dataService.loadDataVO(report.getDataViewId());

		final Map<String, Object> filter = new HashMap<>();

		Map<String, Object> targetFilter = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
		Map<String, List<String>> targetParam = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);

		dataService.processDynamicFilterAndParam(report.getConfig(), targetFilter, targetParam, null);

		filter.putAll(dataService.convertFilterToFilter(
			dataVO.getDataSourceId(),
			dataVO.getAllFields(),
			targetFilter, null));

		Form<Map<String, Object>> form = new Form<>("form");
		add(form);

		Set<String> filterWithDefAndReqOrDis = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
		filterWithDefAndReqOrDis.addAll(filter.keySet());

		form.add(
			new DataViewFilterPanel("filterPanel", dataVO.getDataSourceId(), filter, dataVO.getAllFields())
				.setWebParams(targetParam)
				.setFilterWithDefAndReqOrDis(filterWithDefAndReqOrDis)
		);

		search = new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
			private static final long serialVersionUID = -8066384058553336246L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				logger.debug("filter = {}", filter);
				mainGrid.loadData(target);
				target.appendJavaScript(String.format("$('#%s').datagrid('loading');", mainGrid.getGridHtmlId()));
			}
		};
		search.setOutputMarkupId(true);
		form.add(search);

		if (targetParam.containsKey(MetisWebParam.SEARCH_ON_START)) {
			searchOnStart = "1".equals(targetParam.get(MetisWebParam.SEARCH_ON_START).get(0));
		}

		mainGrid = new DataViewGridPanel("mainGrid", dataVO, filter);
		mainGrid.setWebParams(targetParam);

		add(mainGrid);
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
