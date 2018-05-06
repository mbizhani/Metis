package org.devocative.metis.web.dpage.data;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.request.Url;
import org.devocative.adroit.ConfigUtil;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.demeter.web.DPanel;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.vo.query.QueryExecInfoRVO;

import java.util.List;

class SearchDebugPanel extends DPanel {
	private static final long serialVersionUID = -5678280342452888775L;

	SearchDebugPanel(String id, List<QueryExecInfoRVO> queryExecInfoList, String dataViewName, String user, String time) {
		super(id);

		String searchServer = getWebRequest().getClientUrl().getHost();
		String searchApp = getWebRequest().getContextPath().substring(1);
		String dbConnName = "";
		if (!queryExecInfoList.isEmpty()) {
			dbConnName = queryExecInfoList.get(0).getDbConnName();
		}
		String ccbKey = String.format("S=%s:A=%s:C=%s:V=%s:U=%s:T=%s",
			searchServer, searchApp, dbConnName, dataViewName, user, time);

		add(new Label("issueKey", ccbKey));

		// ---------------

		StringBuilder builder = new StringBuilder();
		builder
			.append(searchServer)
			.append(":")
			.append(getWebRequest().getClientUrl().getPort());

		boolean firstParam = true;

		for (Url.QueryParameter parameter : getWebRequest().getClientUrl().getQueryParameters()) {
			if (!parameter.getValue().isEmpty()) {
				if (firstParam) {
					builder.append(" || ");
					firstParam = false;
				}

				String name = parameter.getName();
				if (name.startsWith("amp;")) {
					name = name.substring(4);
				}
				builder.append(name).append(" = ").append(parameter.getValue()).append(" | ");
			}
		}

		WebMarkupContainer debugInfo = new WebMarkupContainer("debugInfo");
		debugInfo.setVisible(ConfigUtil.getBoolean(MetisConfigKey.ShowSearchDebugger));
		add(debugInfo);

		debugInfo.add(new Label("sentURL", builder.toString()));

		debugInfo.add(new Label("app", searchApp));

		debugInfo.add(new Label("dbConnName", dbConnName));

		debugInfo.add(new ListView<QueryExecInfoRVO>("list", queryExecInfoList) {
			private static final long serialVersionUID = 8291860105472506977L;

			@Override
			protected void populateItem(ListItem<QueryExecInfoRVO> item) {
				QueryExecInfoRVO infoRVO = item.getModelObject();

				item.add(new Label("sql", infoRVO.getFinalSQL()).setEscapeModelStrings(true));

				item.add(new ListView<KeyValueVO<Integer, Object>>("param", KeyValueVO.fromMap(infoRVO.getFinalParams())) {
					private static final long serialVersionUID = 3470976981108949137L;

					@Override
					protected void populateItem(ListItem<KeyValueVO<Integer, Object>> item) {
						KeyValueVO<Integer, Object> entry = item.getModelObject();
						item.add(new Label("paramIndex", entry.getKey()));
						item.add(new Label("paramValue", String.valueOf(entry.getValue())));
					}
				});

				item.add(new Label("duration", infoRVO.getDuration()));
			}
		});
	}
}
