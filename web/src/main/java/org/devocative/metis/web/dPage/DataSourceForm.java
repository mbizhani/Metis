package org.devocative.metis.web.dPage;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxCheckBox;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.mapping.XSchema;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.entity.dataSource.config.*;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.web.panel.QueryEditorPanel;
import org.devocative.wickomp.form.WAjaxButton;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WSelectionInputAjaxUpdatingBehavior;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.code.OCode;
import org.devocative.wickomp.form.code.OCodeMode;
import org.devocative.wickomp.form.code.WCodeInput;
import org.devocative.wickomp.form.wizard.OWizard;
import org.devocative.wickomp.form.wizard.WWizardPanel;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;
import org.devocative.wickomp.html.WMessager;
import org.devocative.wickomp.html.window.WModalWindow;
import org.devocative.wickomp.opt.OSize;
import org.devocative.wickomp.wrcs.EasyUIBehavior;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.io.Serializable;
import java.util.*;

public class DataSourceForm extends DPage {
	private static Logger logger = LoggerFactory.getLogger(DataSourceForm.class);

	private XDSQuery xdsQuery;
	private DataSource dataSource;
	private List<XDSField> xdsFields;
	private List<XDSParameter> xdsParams;

	@Inject
	private IDataSourceService dataSourceService;

	@Inject
	private IDBConnectionService connectionService;

	public DataSourceForm(String id, List<String> params) {
		super(id, params);

		add(new EasyUIBehavior());

		if (params.size() > 0) {
			dataSource = dataSourceService.getDataSource(params.get(0));
			XDataSource xDataSource = dataSourceService.getXDataSource(dataSource);
			xdsQuery = xDataSource.getQuery();
			xdsFields = xDataSource.getFields();
			xdsParams = xDataSource.getParams();
		} else {
			dataSource = new DataSource();
			xdsFields = new ArrayList<>();
			xdsParams = new ArrayList<>();
			xdsQuery = new XDSQuery();
		}

		Form form = new Form("form");
		add(form);

		OWizard oWizard = new OWizard()
			.addStep("init", new InitStep())
			.addStep("query", new QueryStep())
			.addStep("params", new ParamStep())
			.addStep("columns", new DefineColumnsStep())
			.addStep("lookup", new DefineLookupStep());
		//TODO a review step

		form.add(new WWizardPanel("wizard", oWizard, WWizardPanel.ButtonBarPlace.TOP) {
				@Override
				protected void onNext(AjaxRequestTarget target, String stepId) {
					if ("query".equals(stepId)) {
						setTitle(dataSource.getName());

						List<XDSParameter> list = dataSourceService.createParams(xdsQuery.getText(), xdsParams);
						xdsParams.clear();
						xdsParams.addAll(list);

					} else if ("params".equals(stepId)) {
						List<XDSField> list = dataSourceService.createFields(
							xdsFields,
							xdsQuery,
							dataSource.getConnection().getId(),
							xdsParams
						);
						xdsFields.clear();
						xdsFields.addAll(list);
					}
				}

				@Override
				protected void onFinish(AjaxRequestTarget target, String stepId) {
					dataSourceService.saveOrUpdate(dataSource, xdsQuery, xdsFields, xdsParams);

					UrlUtil.redirectTo(DataSourceExecutor.class, dataSource.getName());
				}

				@Override
				protected void onError(AjaxRequestTarget target, String stepId, List<Serializable> errors) {
					WMessager.show(getString("label.error"), errors, target);
				}

			}.setTitle(dataSource.getName())
		);
	}

	private class InitStep extends WWizardStepPanel {
		@Override
		protected void onInit() {
			final WSelectionInput connection, queryMode;

			add(new WTextInput("name", new PropertyModel<String>(DataSourceForm.this, "dataSource.name"))
					.setLabelVisible(false)
					.setRequired(true)
					.setLabel(new ResourceModel("DataSource.name"))
			);
			add(new WTextInput("title", new PropertyModel<String>(DataSourceForm.this, "dataSource.title"))
				.setLabelVisible(false)
				.setRequired(true)
				.setLabel(new ResourceModel("DataSource.title")));
			add(connection = new WSelectionInput("connection",
				new PropertyModel<String>(DataSourceForm.this, "dataSource.connection"),
				connectionService.list(),
				false
			));
			connection
				.setLabelVisible(false)
				.setRequired(true)
				.setLabel(new ResourceModel("DataSource.connection"));

			List<XDSQueryMode> modes;
			if (dataSource.getConnection() != null && dataSource.getConnection().getSafeConfigId() != null) {
				modes = Arrays.asList(XDSQueryMode.values());
			} else {
				modes = Arrays.asList(XDSQueryMode.Sql);
			}

			add(queryMode = new WSelectionInput("queryMode", new PropertyModel(xdsQuery, "mode"), modes, false));
			queryMode
				.setLabelVisible(false)
				.setRequired(true)
				.setLabel(new ResourceModel("DataSource.query.mode"));

			connection.addToChoices(new WSelectionInputAjaxUpdatingBehavior() {
				@Override
				protected void onUpdate(AjaxRequestTarget target) {
					DBConnection dbConnection = (DBConnection) getComponent().getDefaultModelObject();
					if (dbConnection.getSafeConfigId() != null) {
						queryMode.updateChoices(target, Arrays.asList(XDSQueryMode.values()));
					} else {
						queryMode.updateChoices(target, Arrays.asList(XDSQueryMode.Sql));
					}
				}
			});
		}
	}

	private class QueryStep extends WWizardStepPanel {
		private WAjaxButton showSQL;
		private OCode oCode = new OCode(OCodeMode.SQL);
		private WModalWindow modalWindow;

		@Override
		protected void onInit() {
			add(modalWindow = new WModalWindow("modal"));
			modalWindow
				.getOptions()
				.setTitle("Query Editor")
				.setWidth(OSize.percent(70))
				.setHeight(OSize.fixed(800));

			add(new CheckBox("dynamic", new PropertyModel<Boolean>(xdsQuery, "dynamic")));

			add(new WCodeInput("query", new PropertyModel<String>(xdsQuery, "text"), oCode)
				.setRequired(true)
				.setLabel(new ResourceModel("DataSource.query")));

			add(showSQL = new DAjaxButton("showSQL") {
				@Override
				protected void onSubmit(AjaxRequestTarget target) {
					String sql = dataSourceService.processQuery(dataSource.getConnection().getId(), xdsQuery.getMode(), xdsQuery.getText());

					Map<String, Object> params = new HashMap<>();
					for (XDSParameter xdsParam : xdsParams) {
						params.put(xdsParam.getName(), xdsParam.getSampleData());
					}

					modalWindow.setContent(new QueryEditorPanel(
						modalWindow.getContentId(),
						dataSource.getConnection().getId(),
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
			if (xdsQuery.getMode() != XDSQueryMode.Sql) {
				XSchema xSchema = connectionService.getSchemaOfMapping(dataSource.getConnection().getId());

				Map<String, Map> tables = new HashMap<>();
				tables.put("tables", xSchema.getHierarchy());
				oCode.setHintOptions(tables);
			} else {
				oCode.setHintOptions(null);
			}

			showSQL.setVisible(xdsQuery.getMode() != XDSQueryMode.Sql);
		}
	}

	private class ParamStep extends WWizardStepPanel {
		private Label messageWhenNoParam;
		private WebMarkupContainer table;

		@Override
		protected void onInit() {
			table = new WebMarkupContainer("table");
			add(table);

			table.add(new ListView<XDSParameter>("params", xdsParams) {
				@Override
				protected void populateItem(ListItem<XDSParameter> item) {
					XDSParameter xdsParameter = item.getModelObject();

					final WSelectionInput type;
					item.add(new Label("name", xdsParameter.getName()));
					item.add(new WTextInput("title", new PropertyModel<String>(xdsParameter, "title"))
						.setLabelVisible(false));
					item.add(type = new WSelectionInput("type", new PropertyModel<String>(xdsParameter, "type"),
						Arrays.asList(XDSFieldType.values()), false));
					item.add(new CheckBox("required", new PropertyModel<Boolean>(xdsParameter, "required")));
					item.add(new WTextInput("sampleData", new PropertyModel<String>(xdsParameter, "sampleData"))
						.setLabelVisible(false));

					type
						.setLabelVisible(false)
						.setRequired(true)
						.setLabel(new Model<>(getString("XDSField.type") + " " + xdsParameter.getName()));

				}
			});

			add(messageWhenNoParam = new Label("messageWhenNoParam", new ResourceModel("DataSource.alert.noParameter")));
		}

		@Override
		protected void onBeforeRender() {
			table.setVisible(xdsParams.size() > 0);
			messageWhenNoParam.setVisible(xdsParams.size() == 0);

			super.onBeforeRender();
		}
	}

	private class DefineColumnsStep extends WWizardStepPanel {
		@Override
		protected void onInit() {
			add(new ListView<XDSField>("fields", xdsFields) {
				@Override
				protected void populateItem(ListItem<XDSField> item) {
					XDSField xdsField = item.getModelObject();


					final WSelectionInput type, filterType;
					final CheckBox required;

					item.add(new Label("name", xdsField.getName()));
					item.add(new Label("dbType", xdsField.getDbType()));
					item.add(new Label("dbSize", xdsField.getDbSize()));
					item.add(new WTextInput("title", new PropertyModel<String>(xdsField, "title")).setLabelVisible(false));
					item.add(type = new WSelectionInput("type", new PropertyModel<String>(xdsField, "type"), Arrays.asList(XDSFieldType.values()), false));

					item.add(required = new CheckBox("required", new PropertyModel<Boolean>(xdsField, "required")));
					item.add(filterType = new WSelectionInput("filterType", new PropertyModel<String>(xdsField, "filterType"), Arrays.asList(xdsField.getType().getProperFilterTypes()), false));
					item.add(new AjaxCheckBox("inFilterPanel", new PropertyModel<Boolean>(xdsField, "inFilterPanel")) {
						@Override
						protected void onUpdate(AjaxRequestTarget target) {
							IModel<?> defaultModel = getDefaultModel();
							Boolean bool = (Boolean) defaultModel.getObject();
							if (bool == null) {
								bool = false;
							}
							target.add(required.setVisible(bool));
							target.add(filterType.setVisible(bool));
						}
					});

					item.add(new WSelectionInput("resultType", new PropertyModel<String>(xdsField, "resultType"), Arrays.asList(XDSFieldResultType.values()), false)
						.setLabelVisible(false)
						.setRequired(true)
						.setLabel(new Model<>(getString("XDSField.resultType") + " " + xdsField.getName())));
					item.add(new CheckBox("isKeyField", new PropertyModel<Boolean>(xdsField, "isKeyField"))
						.add(new AttributeModifier("group", "isKeyField")));
					item.add(new CheckBox("isTitleField", new PropertyModel<Boolean>(xdsField, "isTitleField"))
						.add(new AttributeModifier("group", "isTitleField")));
					item.add(new CheckBox("isSelfRelPointerField", new PropertyModel<Boolean>(xdsField, "isSelfRelPointerField"))
						.add(new AttributeModifier("group", "isSelfRelField")));

					type.addToChoices(new WSelectionInputAjaxUpdatingBehavior() {
						@Override
						protected void onUpdate(AjaxRequestTarget target) {
							XDSFieldType type = (XDSFieldType) getComponent().getDefaultModelObject();
							filterType.updateChoices(target, Arrays.asList(type.getProperFilterTypes()));
						}
					});

					boolean needFilter = xdsField.getInFilterPanel() != null && xdsField.getInFilterPanel();
					type
						.setLabelVisible(false)
						.setRequired(true)
						.setLabel(new Model<>(getString("XDSField.type") + " " + xdsField.getName()));
					filterType
						.setLabelVisible(false)
						.setLabel(new Model<>(getString("XDSField.filterType") + " " + xdsField.getName()))
						.setRequired(needFilter)
						.setVisible(needFilter)
						.setOutputMarkupId(true)
						.setOutputMarkupPlaceholderTag(true);
					required
						.setOutputMarkupId(true)
						.setOutputMarkupPlaceholderTag(true)
						.setVisible(needFilter);
				}
			});
		}
	}

	private class DefineLookupStep extends WWizardStepPanel {
		private List<XDSAbstractField> lookupFields = new ArrayList<>();
		private List<DataSource> dataSourceList = new ArrayList<>();

		private WebMarkupContainer table;
		private Label messageWhenNoLookUp;

		@Override
		protected void onInit() {
			table = new WebMarkupContainer("table");
			add(table);

			table.add(new ListView<XDSAbstractField>("fields", lookupFields) {
				@Override
				protected void populateItem(ListItem<XDSAbstractField> item) {
					XDSAbstractField field = item.getModelObject();
					if (field.getTargetId() != null) {
						field.setTarget(new DataSource(field.getTargetId()));
					}

					item.add(new Label("name", field.getName()));
					item.add(new WSelectionInput("dataSources", new PropertyModel(field, "target"),
						dataSourceList, false)
						.setLabelVisible(false)
						.setRequired(true)
						.setLabel(new Model<>(field.getName())));
				}
			});

			add(messageWhenNoLookUp = new Label("messageWhenNoLookUp", new ResourceModel("DataSource.alert.noLookUp")));
		}

		@Override
		protected void onBeforeRender() {
			//TODO: the following "for" must be added to service
			lookupFields.clear();

			for (XDSParameter xdsParam : xdsParams) {
				if (XDSFieldType.LookUp == xdsParam.getType()) {
					lookupFields.add(xdsParam);
				}
			}

			for (XDSField xdsField : xdsFields) {
				if (XDSFieldType.LookUp == xdsField.getType()) {
					lookupFields.add(xdsField);
				}
			}

			table.setVisible(lookupFields.size() > 0);
			messageWhenNoLookUp.setVisible(lookupFields.size() == 0);

			dataSourceList.clear();
			dataSourceList.addAll(dataSourceService.getListForLookup());

			super.onBeforeRender();
		}
	}

}
