package org.devocative.metis.web.dpage.data;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebComponent;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.adroit.ConfigUtil;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.MetisWebParam;
import org.devocative.metis.web.dpage.data.form.DataViewFormDPage;
import org.devocative.wickomp.html.WMessager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DataViewExecutorDPage extends DPage {
	private static final long serialVersionUID = 733765900178805871L;

	private static final Logger logger = LoggerFactory.getLogger(DataViewExecutorDPage.class);

	private DataVO dataVO;
	private String sentDBConnection;
	private DataViewGridPanel mainGrid;
	private Map<String, Object> filter = new HashMap<>();

	private IModel<String> title;
	private boolean hasDataVO = true;
	private boolean multiSelect;
	private String selectionJSCallback;

	@Inject
	private IDataService dataService;

	// ------------------------------

	public DataViewExecutorDPage(String id, String dataViewName) {
		this(id, Collections.singletonList(dataViewName));
	}

	// Main Constructor
	public DataViewExecutorDPage(String id, List<String> params) {
		super(id, params);

		if (params.size() > 0) {
			dataVO = dataService.loadDataVO(params.get(0));
		}

		if (dataVO == null) {
			title = params.size() > 0 ?
				new ResourceModel("DataView.err.invalid.name", "Invalid DataView") :
				new ResourceModel("DataView.err.no.param", "No DataView Name");

			dataVO = new DataVO();
			dataVO.setName(params.size() > 0 ? params.get(0) : "-");

			hasDataVO = false;
		} else {
			title = new Model<>(dataVO.getTitle());
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

	public DataViewExecutorDPage setSentDBConnection(String sentDBConnection) {
		this.sentDBConnection = sentDBConnection;
		return this;
	}

	public DataViewExecutorDPage addToFilter(Map<String, Object> filter) {
		this.filter.putAll(dataService.convertFilterToFilter(dataVO.getDataSourceId(), dataVO.getAllFields(),
			filter, sentDBConnection));
		return this;
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		if (ConfigUtil.hasKey(MetisConfigKey.DBConnParamName)) {
			sentDBConnection = getWebRequest()
				.getRequestParameters()
				.getParameterValue(ConfigUtil.getString(MetisConfigKey.DBConnParamName))
				.toOptionalString();
		}

		String color = hasDataVO ? "color:inherit;" : "color:red;";

		add(new Label("dvTitle", title).add(new AttributeModifier("style", color)));
		add(new Label("dvName", dataVO.getName()).add(new AttributeModifier("style", color)));
		add(new ExternalLink("edit", String.format("%s/%s", UrlUtil.createUri(DataViewFormDPage.class, true), dataVO.getName()))
				.setVisible(getWebRequest().getRequestParameters().getParameterValue(MetisWebParam.WINDOW).isEmpty() && hasDataVO)
		);

		Form<Map<String, Object>> form = new Form<>("form");
		form.setVisible(hasDataVO);
		add(form);

		if (hasDataVO) {
			form.add(
				new DataViewFilterPanel("filterPanel", dataVO.getDataSourceId(), filter, dataVO.getAllFields())
					.setSentDBConnection(sentDBConnection)
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
			mainGrid
				.setMultiSelect(multiSelect)
				.setSelectionJSCallback(selectionJSCallback)
				.setSentDBConnection(sentDBConnection);
			add(mainGrid);
		} else {
			form.add(new WebComponent("filterPanel"));
			form.add(new WebComponent("search"));

			add(new WebComponent("mainGrid"));
		}
	}

	@Override
	protected void onAfterRender() {
		super.onAfterRender();

		WMessager.writeErrorsInAfterRender(this);
	}
}