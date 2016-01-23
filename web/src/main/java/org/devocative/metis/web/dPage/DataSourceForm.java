package org.devocative.metis.web.dPage;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.PropertyModel;
import org.devocative.demeter.web.DPage;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.entity.dataSource.config.XDSField;
import org.devocative.metis.entity.dataSource.config.XDSFieldResultType;
import org.devocative.metis.entity.dataSource.config.XDSFieldType;
import org.devocative.metis.entity.dataSource.config.XDataSource;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WSelectionInputAjaxUpdatingBehavior;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.html.wizard.OWizard;
import org.devocative.wickomp.html.wizard.WWizardPanel;
import org.devocative.wickomp.html.wizard.WWizardStepPanel;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DataSourceForm extends DPage {
	private String sql;
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
			sql = xDataSource.getSql().trim();
			xdsFields = xDataSource.getFields();
		} else {
			dataSource = new DataSource();
			xdsFields = new ArrayList<>();
		}

		Form form = new Form("form");
		add(form);

		OWizard oWizard = new OWizard()
			.addStep("query", new DefineQueryStep())
			.addStep("columns", new DefineColumnsStep());

		form.add(new WWizardPanel("wizard", oWizard, WWizardPanel.ButtonBarPlace.TOP) {
			@Override
			protected void onNext(AjaxRequestTarget target, String stepId) {
				if ("columns".equals(stepId)) {
					List<XDSField> list = dataSourceService.createFields(
						xdsFields,
						sql,
						dataSource.getConnection().getId()
					);
					xdsFields.clear();
					xdsFields.addAll(list);
				}
			}

			@Override
			protected void onFinish(AjaxRequestTarget target) {
				dataSourceService.saveOrUpdate(dataSource, sql, xdsFields);
			}
		});
	}

	private class DefineQueryStep extends WWizardStepPanel {
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
			add(new TextArea<>("sql", new PropertyModel<String>(DataSourceForm.this, "sql")));
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
}
