package org.devocative.metis.web.dPage.data;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebComponent;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.dPage.data.form.DataViewFormDPage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DataViewExecutorDPage extends DPage {
	private static final Logger logger = LoggerFactory.getLogger(DataViewExecutorDPage.class);

	private Map<String, Object> filter = new HashMap<>();
	private DataViewGridPanel mainGrid;

	@Inject
	private IDataService dataService;

	public DataViewExecutorDPage(String id, List<String> params) {
		super(id, params);

		boolean hasDataVO = true;
		IModel<String> title;
		String color = "color:inherit;";

		DataVO dataVO = null;
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
			color = "color:red;";
		} else {
			title = new Model<>(dataVO.getTitle());
		}

		add(new Label("dvTitle", title).add(new AttributeModifier("style", color)));
		add(new Label("dvName", dataVO.getName()).add(new AttributeModifier("style", color)));
		add(new ExternalLink("edit", String.format("%s/%s", UrlUtil.createUri(DataViewFormDPage.class, true), dataVO.getName()))
				.setVisible(getWebRequest().getRequestParameters().getParameterValue("window").isEmpty() && hasDataVO)
		);

		Form<Map<String, Object>> form = new Form<>("form");
		form.setVisible(hasDataVO);
		add(form);

		if (hasDataVO) {
			form.add(new DataViewFilterPanel("filterPanel", dataVO.getDataSourceName(), filter, dataVO.getFields(), dataVO.getParams()));
			form.add(new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
				@Override
				protected void onSubmit(AjaxRequestTarget target) {
					logger.debug("filter = {}", filter);
					mainGrid.loadData(target);
				}
			});
			add(mainGrid = new DataViewGridPanel("mainGrid", dataVO, filter));
		} else {
			form.add(new WebComponent("filterPanel"));
			form.add(new WebComponent("search"));

			add(new WebComponent("mainGrid"));
		}
	}

	public DataViewExecutorDPage setSelectionJSCallback(String jsCallback) {
		if (mainGrid != null) {
			mainGrid.setSelectionJSCallback(jsCallback);
		}
		return this;
	}
}
