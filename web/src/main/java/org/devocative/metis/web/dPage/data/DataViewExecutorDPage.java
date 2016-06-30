package org.devocative.metis.web.dPage.data;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.ExternalLink;
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

		DataVO dataVO = dataService.loadDataVO(params.get(0));

		add(new Label("dvTitle", dataVO.getTitle()));
		add(new Label("dvName", dataVO.getName()));
		add(new ExternalLink("edit", String.format("%s/%s", UrlUtil.createUri(DataViewFormDPage.class, true), params.get(0)))
				.setVisible(getWebRequest().getRequestParameters().getParameterValue("window").isEmpty())
		);

		Form<Map<String, Object>> form = new Form<>("form");
		form.add(new DataViewFilterPanel("filterPanel", dataVO.getDataSourceName(), filter, dataVO.getAllFields()));
		form.add(new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				logger.debug("filter = {}", filter);
				mainGrid.loadData(target);
			}
		});
		add(form);

		add(mainGrid = new DataViewGridPanel("mainGrid", dataVO, filter));
	}

	public DataViewExecutorDPage setSelectionJSCallback(String jsCallback) {
		mainGrid.setSelectionJSCallback(jsCallback);
		return this;
	}
}
