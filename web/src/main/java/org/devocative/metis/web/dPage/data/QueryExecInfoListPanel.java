package org.devocative.metis.web.dPage.data;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.demeter.web.DPanel;
import org.devocative.metis.vo.query.QueryExecInfoRVO;

import java.util.List;

public class QueryExecInfoListPanel extends DPanel {
	private static final long serialVersionUID = -5678280342452888775L;

	public QueryExecInfoListPanel(String id, List<QueryExecInfoRVO> queryExecInfoList) {
		super(id);

		add(new ListView<QueryExecInfoRVO>("list", queryExecInfoList) {
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
