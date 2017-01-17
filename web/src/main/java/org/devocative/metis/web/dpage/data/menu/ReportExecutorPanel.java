package org.devocative.metis.web.dpage.data.menu;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.iservice.template.IStringTemplate;
import org.devocative.demeter.iservice.template.IStringTemplateService;
import org.devocative.demeter.iservice.template.TemplateEngineType;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.data.IReportService;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.dpage.data.DataViewFilterPanel;
import org.devocative.metis.web.dpage.data.DataViewGridPanel;
import org.devocative.wickomp.WPanel;
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
	private Long reportId;

	@Inject
	private IReportService reportService;

	@Inject
	private IDataService dataService;

	@Inject
	private IStringTemplateService stringTemplateService;

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

		TreeMap<String, Object> targetFilter = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
		TreeMap<String, List<String>> targetParam = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);

		Map<String, Object> params = new HashMap<>();
		params.put("params", targetParam);
		params.put("filter", targetFilter);

		StringBuilder script = new StringBuilder();
		script
			.append("def range(l,u){new org.devocative.adroit.vo.RangeVO(l,u)}\n")
			.append("def now(){new Date()}\n")
			.append("def list(Object... p){def list=[]; p.each{list.add(it)}; return list}\n")
			.append(report.getConfig());

		IStringTemplate stringTemplate = stringTemplateService
			.create(script.toString(), TemplateEngineType.GroovyShell);
		stringTemplate.process(params);

		filter.putAll(dataService.convertFilterToFilter(
			dataVO.getDataSourceId(),
			dataVO.getAllFields(),
			targetFilter, null));

		Form<Map<String, Object>> form = new Form<>("form");
		add(form);

		form.add(
			new DataViewFilterPanel("filterPanel", dataVO.getDataSourceId(), filter, dataVO.getAllFields())
				.setWebParams(targetParam)
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
		mainGrid.setWebParams(targetParam);

		add(mainGrid);
	}
}
