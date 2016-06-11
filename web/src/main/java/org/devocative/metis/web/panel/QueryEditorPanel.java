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
import org.devocative.metis.vo.query.QueryRVO;
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
	private List<String> header = new ArrayList<>();
	private List<List<Object>> rows = new ArrayList<>();

	private OCode options = new OCode(OCodeMode.SQL);
	private WebMarkupContainer resultTable;
	private WCodeInput editor;

	@Inject
	private IDBConnectionService dbConnectionService;

	public QueryEditorPanel(String id, final Long dbConnId, String sql, final Map<String, Object> params) {
		super(id);

		Form form = new Form("form");
		form.add(editor = new WCodeInput("editor", new Model<>(sql), options));
		form.add(new DAjaxButton("execute", new ResourceModel("label.execute"), MetisIcon.EXECUTE) {
			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				QueryRVO resultVO = dbConnectionService.executeQuery(dbConnId, editor.getModelObject(), "QryEdtr", params, 1L, 10L);
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
			@Override
			protected void populateItem(ListItem<String> item) {
				String header = item.getModelObject();
				item.add(new Label("headerCell", header));
			}
		});

		resultTable.add(new ListView<List<Object>>("rows", rows) {
			@Override
			protected void populateItem(ListItem<List<Object>> item) {
				List<Object> row = item.getModelObject();

				item.add(new ListView<Object>("row", row) {
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
