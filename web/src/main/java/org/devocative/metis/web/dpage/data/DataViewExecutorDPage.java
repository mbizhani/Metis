package org.devocative.metis.web.dpage.data;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.adroit.ConfigUtil;
import org.devocative.demeter.DLogCtx;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.data.config.XDVGridSelectionMode;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.FilterInputParamsVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.MetisWebParam;
import org.devocative.metis.web.dpage.data.form.DataViewFormDPage;
import org.devocative.wickomp.WebUtil;
import org.devocative.wickomp.html.WEasyLayout;
import org.devocative.wickomp.html.WMessager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.util.*;

public class DataViewExecutorDPage extends DPage {
	private static final long serialVersionUID = 733765900178805871L;

	private static final Logger logger = LoggerFactory.getLogger(DataViewExecutorDPage.class);

	private DataVO dataVO;
	private DataViewGridPanel mainGrid;
	private Map<String, Object> filter = new HashMap<>();
	private FilterInputParamsVO inputParamsVO = new FilterInputParamsVO();

	private IModel<String> title;
	private boolean multiSelect;
	private String selectionJSCallback;
	private boolean considerWebParams = true;

	private DAjaxButton search;

	@Inject
	private IDataService dataService;

	// ------------------------------

	public DataViewExecutorDPage(String id, String dataViewName) {
		this(id, Collections.singletonList(dataViewName));
	}

	// Main Constructor
	public DataViewExecutorDPage(String id, List<String> params) {
		super(id, params);

		if (!params.isEmpty() && !params.get(0).isEmpty()) {
			dataVO = dataService.loadDataVOByName(params.get(0));
			title = new Model<>(dataVO.getTitle());
			multiSelect = XDVGridSelectionMode.Multiple.equals(dataVO.getSelectionModeSafely());
		} else {
			throw new MetisException(MetisErrorCode.NoDataViewName);
		}
	}

	// ------------------------------

	public DataViewExecutorDPage setSelectionJSCallback(String selectionJSCallback) {
		this.selectionJSCallback = selectionJSCallback;
		return this;
	}

	public DataViewExecutorDPage setMultiSelect(boolean multiSelect) {
		this.multiSelect = multiSelect;
		return this;
	}

	public DataViewExecutorDPage setConsiderWebParams(boolean considerWebParams) {
		this.considerWebParams = considerWebParams;
		return this;
	}

	public DataViewExecutorDPage addParams(Map<String, ?> params) {
		inputParamsVO.putAll(params);
		return this;
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		if (considerWebParams) {
			if (ConfigUtil.hasKey(MetisConfigKey.IgnoreParameterValues)) {
				List<String> ignoredValues = Arrays.asList(ConfigUtil.getString(MetisConfigKey.IgnoreParameterValues).split("[,]"));
				inputParamsVO.putAll(WebUtil.toMap(true, true, ignoredValues));
			} else {
				inputParamsVO.putAll(WebUtil.toMap(true, true));
			}
		}

		DLogCtx
			.put("dataView", dataVO.getName())
			.put("inoutParams", inputParamsVO);

		WebMarkupContainer north = new WebMarkupContainer("north");
		north.add(new Label("dvTitle", title));
		north.add(new Label("dvName", dataVO.getName()));
		north.add(new ExternalLink("edit", String.format("%s/%s", UrlUtil.createUri(DataViewFormDPage.class, true), dataVO.getName())));
		north.setVisible(hasPermission(DataViewFormDPage.class));
		//north.setVisible(!ConfigUtil.getBoolean(DemeterConfigKey.DeploymentMode));

		Form<Map<String, Object>> form = new Form<>("form");
		form.add(
			new DataViewFilterPanel2("filterPanel", dataVO, filter)
				.setInputParamsVO(inputParamsVO)
		);
		search = new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
			private static final long serialVersionUID = -8066384058553336246L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				//target.appendJavaScript(String.format("$('#%s').datagrid('loading');", mainGrid.getGridHtmlId()));
				logger.debug("filter = {}", filter);
				mainGrid.loadData(target);
				//WebUtil.sendByWebSocket(this, new WebSocketJavascriptResult(String.format("$('#%s').datagrid('loading');", mainGrid.getGridHtmlId())));
			}
		};
		search.setOutputMarkupId(true);
		form.add(search);

		WebMarkupContainer centerPanel = new WebMarkupContainer("centerPanel");
		centerPanel.add(new AttributeModifier("title", getString("label.result") + " - " + dataVO.getTitle()));

		mainGrid = new DataViewGridPanel("mainGrid", dataVO, filter);
		mainGrid
			.setMultiSelect(multiSelect)
			.setSelectionJSCallback(selectionJSCallback)
			.setInputParamsVO(inputParamsVO);
		centerPanel.add(mainGrid);

		WebMarkupContainer filterPanel = new WebMarkupContainer("filterPanel");
		filterPanel.add(form);

		if (inputParamsVO.getAsStringOrDefault(MetisWebParam.HIDE_FILTER, "").equals("1")) {
			filterPanel.add(new AttributeAppender("data-options", "collapsed:true", ",")); //TODO the comma
		}

		WEasyLayout layout = new WEasyLayout("layout");
		layout.add(north);
		layout.add(filterPanel);
		layout.add(centerPanel);
		layout.setWestOfLTRDir(filterPanel);
		add(layout);
	}

	@Override
	protected void onAfterRender() {
		super.onAfterRender();

		WMessager.writeErrorsInAfterRender(this);

		if (inputParamsVO.containsKey(MetisWebParam.SEARCH_ON_START) &&
			"1".equals(inputParamsVO.getAsString(MetisWebParam.SEARCH_ON_START))) {
			String script = String.format("$('#%s').click();", search.getMarkupId());
			WebUtil.writeJQueryCall(script, true);
		}

		DLogCtx
			.remove("dataView")
			.remove("webParams")
		;
	}
}
