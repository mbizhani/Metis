package org.devocative.metis.web.dPage;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.PropertyModel;
import org.devocative.demeter.web.DPage;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.entity.dataSource.config.*;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WSelectionInputAjaxUpdatingBehavior;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.code.OCode;
import org.devocative.wickomp.form.code.OCodeMode;
import org.devocative.wickomp.form.code.WCodeInput;
import org.devocative.wickomp.html.wizard.OWizard;
import org.devocative.wickomp.html.wizard.WWizardPanel;
import org.devocative.wickomp.html.wizard.WWizardStepPanel;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DataSourceForm extends DPage {
	private XDSQuery xdsQuery;
	private DataSource dataSource;
	private List<XDSField> xdsFields;

	@Inject
	private IDataSourceService dataSourceService;

	@Inject
	private IDBConnectionService connectionService;

	public DataSourceForm(String id, List<String> params) {
		super(id, params);

		if (params.size() > 0) {
			dataSource = dataSourceService.getDataSource(params.get(0));
			XDataSource xDataSource = dataSourceService.getXDataSource(dataSource);
			xdsQuery = xDataSource.getQuery();
			xdsFields = xDataSource.getFields();
		} else {
			dataSource = new DataSource();
			xdsFields = new ArrayList<>();
			xdsQuery = new XDSQuery();
		}

		Form form = new Form("form");
		add(form);

		OWizard oWizard = new OWizard()
			.addStep("init", new InitStep())
			.addStep("query", new QueryStep())
			.addStep("columns", new DefineColumnsStep())
			.addStep("lookup", new DefineLookupStep());

		form.add(new WWizardPanel("wizard", oWizard, WWizardPanel.ButtonBarPlace.TOP) {
			@Override
			protected void onNext(AjaxRequestTarget target, String stepId) {
				if ("columns".equals(stepId)) {
					List<XDSField> list = dataSourceService.createFields(
						xdsFields,
						xdsQuery,
						dataSource.getConnection().getId()
					);
					xdsFields.clear();
					xdsFields.addAll(list);
				}
			}

			@Override
			protected void onFinish(AjaxRequestTarget target) {
				dataSourceService.saveOrUpdate(dataSource, xdsQuery, xdsFields);
			}
		});
	}

	private class InitStep extends WWizardStepPanel {
		@Override
		protected void onInit() {
			add(new WTextInput("name", new PropertyModel<String>(DataSourceForm.this, "dataSource.name"))
				.addToTextField(new AttributeModifier("style", "direction:ltr;")));
			add(new WTextInput("title", new PropertyModel<String>(DataSourceForm.this, "dataSource.title")));
			add(new WSelectionInput("connection",
				new PropertyModel<String>(DataSourceForm.this, "dataSource.connection"),
				connectionService.list(),
				false
			));
			add(new WSelectionInput("queryMode", new PropertyModel(xdsQuery, "mode"),
				Arrays.asList(XDSQueryMode.values()), false));
		}
	}

	private class QueryStep extends WWizardStepPanel {
		@Override
		protected void onInit() {
			OCode oCode = new OCode(OCodeMode.SQL);
			if (xdsQuery.getMode() != XDSQueryMode.SQL) {
				//oCode.setHintOptions(); TODO load mappings
			}
			add(new WCodeInput("query", new PropertyModel<String>(xdsQuery, "text"), oCode));
		}
	}

	private class DefineColumnsStep extends WWizardStepPanel {
		@Override
		protected void onInit() {
			add(new ListView<XDSField>("fields", xdsFields) {
				@Override
				protected void populateItem(ListItem<XDSField> item) {
					XDSField field = item.getModelObject();

					final WSelectionInput type, filterType;

					item.add(new Label("name", field.getName()));
					item.add(new Label("dbType", field.getDbType()));
					item.add(new Label("dbSize", field.getDbSize()));
					item.add(new WTextInput("title", new PropertyModel<String>(field, "title")));
					item.add(type = new WSelectionInput("type", new PropertyModel<String>(field, "type"), Arrays.asList(XDSFieldType.values()), false));
					item.add(filterType = new WSelectionInput("filterType", new PropertyModel<String>(field, "filterType"), Arrays.asList(field.getType().getProperFilterTypes()), false));
					item.add(new WSelectionInput("resultType", new PropertyModel<String>(field, "resultType"), Arrays.asList(XDSFieldResultType.values()), false));
					item.add(new CheckBox("isKeyField", new PropertyModel<Boolean>(field, "isKeyField"))
						.add(new AttributeModifier("group", "isKeyField")));
					item.add(new CheckBox("isTitleField", new PropertyModel<Boolean>(field, "isTitleField"))
						.add(new AttributeModifier("group", "isTitleField")));
					item.add(new CheckBox("isSelfRelPointerField", new PropertyModel<Boolean>(field, "isSelfRelPointerField"))
						.add(new AttributeModifier("group", "isSelfRelField")));

					type.addToChoices(new WSelectionInputAjaxUpdatingBehavior() {
						@Override
						protected void onUpdate(AjaxRequestTarget target) {
							XDSFieldType type = (XDSFieldType) getComponent().getDefaultModelObject();
							filterType.updateChoices(target, Arrays.asList(type.getProperFilterTypes()));
						}
					});
				}
			});
		}
	}

	private class DefineLookupStep extends WWizardStepPanel {
		@Inject
		private IDataSourceService dataSourceService;

		@Override
		protected void onInit() {
			//TODO: the following "for" must be added to service
			List<XDSField> lookupFields = new ArrayList<>();
			for (XDSField xdsField : xdsFields) {
				if (XDSFieldType.LookUp == xdsField.getType()) {
					lookupFields.add(xdsField);
				}
			}

			final List<DataSource> dataSourceList = dataSourceService.getListForLookup();

			WebMarkupContainer table = new WebMarkupContainer("table");
			table.setVisible(lookupFields.size() > 0);
			add(table);

			table.add(new ListView<XDSField>("fields", lookupFields) {
				@Override
				protected void populateItem(ListItem<XDSField> item) {
					XDSField field = item.getModelObject();
					field.setTarget(new DataSource(field.getTargetId()));

					item.add(new Label("name", field.getName()));
					item.add(new WSelectionInput("dataSources", new PropertyModel(field, "target"),
						dataSourceList, false));
				}
			});

			String message = "";
			if (lookupFields.size() == 0) {
				message = "No Lookup field"; //TODO I18N
			}
			add(new Label("lbl", message));
		}
	}

}
