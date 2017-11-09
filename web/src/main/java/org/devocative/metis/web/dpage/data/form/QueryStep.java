package org.devocative.metis.web.dpage.data.form;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.entity.connection.mapping.XSchema;
import org.devocative.metis.entity.data.config.XDSQueryMode;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.iservice.data.IDataSourceService;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.query.EQLMetaDataVO;
import org.devocative.metis.web.panel.QueryEditorPanel;
import org.devocative.wickomp.form.code.OCode;
import org.devocative.wickomp.form.code.OCodeMode;
import org.devocative.wickomp.form.code.WCodeInput;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;
import org.devocative.wickomp.html.WMessager;
import org.devocative.wickomp.html.window.WModalWindow;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class QueryStep extends WWizardStepPanel {
	private static final long serialVersionUID = 6539341649298665749L;

	private DataVO dataVO;

	private OCode oCode = new OCode(OCodeMode.SQL);
	private WModalWindow modalWindow;
	private DAjaxButton showEqlWarns;
	private boolean waitOnWarn = false;

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
	public boolean onStepSubmit(AjaxRequestTarget target) {
		if (dataVO.isDataSourceEditable()) {
			dataService.updateParamsByQuery(dataVO.getParams(), dataVO.getQuery().getText(), dataVO.getQuery().getBefore());

			if (dataVO.getQuery().getMode() == XDSQueryMode.Eql) {
				EQLMetaDataVO metaDataVO = dataSourceService.processEntityQuery(
					dataVO.getConnectionId(),
					dataVO.getQuery().getText());

				if (!waitOnWarn && metaDataVO.getErrors().size() > 0) {
					waitOnWarn = true;

					StringBuilder builder = new StringBuilder();
					for (Map.Entry<MetisErrorCode, List<String>> entry : metaDataVO.getErrors().entrySet()) {
						builder.append(entry.getKey().getCode());
						builder.append(WMessager.getHtml(entry.getValue()));
					}
					WMessager.show("Warnings", builder.toString(), target);

					return false;
				}
			}
		}
		return true;
	}

	@Override
	protected void onInit() {
		add(modalWindow = new WModalWindow("modal"));
		modalWindow
			.getOptions()
			.setTitle("Query Editor")
			.setWidth(OSize.percent(70))
			.setHeight(OSize.fixed(800));

		add(new CheckBox("dynamic", new PropertyModel<>(dataVO.getQuery(), "dynamic"))
				.setEnabled(dataVO.isDataSourceEditable())
		);

		add(new TextField<>("before", new PropertyModel<>(dataVO.getQuery(), "before"), String.class)
				.setEnabled(dataVO.isDataSourceEditable())
		);

		add(new WCodeInput("query", new PropertyModel<>(dataVO.getQuery(), "text"), oCode)
				.setRequired(true)
				.setLabel(new ResourceModel("DataSource.query"))
				.setEnabled(dataVO.isDataSourceEditable())
		);

		add(new TextArea<>("after", new PropertyModel<>(dataVO.getQuery(), "after"))
				.setEnabled(dataVO.isDataSourceEditable())
		);

		add(new DAjaxButton("showSQL") {
			private static final long serialVersionUID = -8648409656785402139L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				String sql = dataSourceService.processQuery(
					dataVO.getConnectionId(),
					dataVO.getQuery().getText(),
					dataVO.getQuery().getMode());

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

		add(showEqlWarns = new DAjaxButton("showEqlWarns") {
			private static final long serialVersionUID = -3258368858868144176L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				EQLMetaDataVO metaDataVO = dataSourceService.processEntityQuery(
					dataVO.getConnectionId(),
					dataVO.getQuery().getText());
				if (metaDataVO.getErrors().size() > 0) {
					StringBuilder builder = new StringBuilder();
					for (Map.Entry<MetisErrorCode, List<String>> entry : metaDataVO.getErrors().entrySet()) {
						builder.append(entry.getKey().getCode());
						builder.append(WMessager.getHtml(entry.getValue()));
					}
					WMessager.show("Warnings", builder.toString(), target);
				} else {
					WMessager.show("Info", "No Warnings!", target);
				}
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
		showEqlWarns.setEnabled(dataVO.isDataSourceEditable() && dataVO.getQuery().getMode() == XDSQueryMode.Eql);
	}
}
