package org.devocative.metis.web.dPage.data.form;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.entity.connection.mapping.XSchema;
import org.devocative.metis.entity.data.config.XDSQueryMode;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.web.panel.QueryEditorPanel;
import org.devocative.wickomp.form.WAjaxButton;
import org.devocative.wickomp.form.code.OCode;
import org.devocative.wickomp.form.code.OCodeMode;
import org.devocative.wickomp.form.code.WCodeInput;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;
import org.devocative.wickomp.html.window.WModalWindow;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.util.HashMap;
import java.util.Map;

class QueryStep extends WWizardStepPanel {
	private DataVO dataVO;

	private WAjaxButton showSQL;
	private OCode oCode = new OCode(OCodeMode.SQL);
	private WModalWindow modalWindow;

	@Inject
	private IDataService dataService;

	@Inject
	private IDataSourceService dataSourceService;

	@Inject
	private IDBConnectionService connectionService;

	public QueryStep(DataVO dataVO) {
		this.dataVO = dataVO;
	}

	@Override
	public void onStepSubmit() {
		if (dataVO.isDataSourceEditable()) {
			dataService.updateParamsByQuery(dataVO.getQuery().getText(), dataVO.getParams());
		}
	}

	@Override
	protected void onInit() {
		add(modalWindow = new WModalWindow("modal"));
		modalWindow
			.getOptions()
			.setTitle("Query Editor")
			.setWidth(OSize.percent(70))
			.setHeight(OSize.fixed(800));

		add(new CheckBox("dynamic", new PropertyModel<Boolean>(dataVO.getQuery(), "dynamic"))
			.setEnabled(dataVO.isDataSourceEditable()));

		add(new WCodeInput("query", new PropertyModel<String>(dataVO.getQuery(), "text"), oCode)
			.setRequired(true)
			.setLabel(new ResourceModel("DataSource.query")));

		add(showSQL = new DAjaxButton("showSQL") {
			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				String sql = dataSourceService.processQuery(
					dataVO.getConnectionId(),
					dataVO.getQuery().getMode(),
					dataVO.getQuery().getText());

				Map<String, Object> params = new HashMap<>();
				for (DataParameterVO parameterVO : dataVO.getParams()) {
					params.put(parameterVO.getName(), parameterVO.getSampleData());
				}

				modalWindow.setContent(new QueryEditorPanel(
					modalWindow.getContentId(),
					dataVO.getConnectionId(),
					sql,
					params));
				modalWindow.show(target);
				//WMessager.show("SQL", String.format("<p class='al-ltr'>%s</p>", sql), target);
			}
		});
	}

	@Override
	protected void onBeforeRender() {
		super.onBeforeRender();

		if (dataVO.getQuery().getMode() != XDSQueryMode.Sql) {
			XSchema xSchema = connectionService.getSchemaOfMapping(dataVO.getConnectionId());

			Map<String, Map> tables = new HashMap<>();
			tables.put("tables", xSchema.getHierarchy());
			oCode.setHintOptions(tables);
		} else {
			oCode.setHintOptions(null);
		}

		//showSQL.setVisible(dataVO.getQuery().getMode() != XDSQueryMode.Sql);
	}
}
