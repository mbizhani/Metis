package org.devocative.metis.web.panel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.vo.query.DbQueryRVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.WPanel;
import org.devocative.wickomp.form.code.OCode;
import org.devocative.wickomp.form.code.OCodeMode;
import org.devocative.wickomp.form.code.WCodeInput;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class QueryEditorPanel extends WPanel {
	private static final long serialVersionUID = -978099734594170667L;

	private List<String> header = new ArrayList<>();
	private List<List<Object>> rows = new ArrayList<>();

	private WebMarkupContainer resultTable;
	private WCodeInput editor;

	@Inject
	private IDBConnectionService dbConnectionService;

	public QueryEditorPanel(String id, final Long dbConnId, String sql, final Map<String, Object> params) {
		super(id);

		Form form = new Form("form");
		form.add(editor = new WCodeInput("editor", new Model<>(sql), new OCode(OCodeMode.SQL)));
		form.add(new DAjaxButton("execute", new ResourceModel("label.execute"), MetisIcon.EXECUTE) {
			private static final long serialVersionUID = 5285685168080899566L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				header.clear();
				rows.clear();

				DbQueryRVO resultVO = dbConnectionService.executeQuery(dbConnId, editor.getModelObject(), "QryEdtr", params, 1L, 10L);
				header.addAll(resultVO.getHeader());
				rows.addAll(resultVO.getRows());
				target.add(resultTable);
			}
		});
		add(form);

		resultTable = new WebMarkupContainer("resultTable");
		resultTable.setOutputMarkupId(true);
		add(resultTable);

		resultTable.add(new ListView<String>("headerRow", header) {
			private static final long serialVersionUID = -3070418784239881495L;

			@Override
			protected void populateItem(ListItem<String> item) {
				String header = item.getModelObject();
				item.add(new Label("headerCell", header));
			}
		});

		resultTable.add(new ListView<List<Object>>("rows", rows) {
			private static final long serialVersionUID = 7559706450201621325L;

			@Override
			protected void populateItem(ListItem<List<Object>> item) {
				List<Object> row = item.getModelObject();

				item.add(new ListView<Object>("row", row) {
					private static final long serialVersionUID = -6340802887851465912L;

					@Override
					protected void populateItem(ListItem<Object> item) {
						Object cell = item.getModelObject();
						item.add(new Label("cell", cell != null ? cell.toString() : ""));
					}
				});
			}
		});
	}
}
