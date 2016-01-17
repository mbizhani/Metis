package org.devocative.metis.web.dPage;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.PropertyModel;
import org.devocative.demeter.web.DPage;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.entity.dataSource.config.*;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.wickomp.form.WSelectionInput;
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
			sql = xDataSource.getSql();
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
				// The only next step is DefineColumnsStep
				List<XDSField> list = dataSourceService.createFields(
					xdsFields,
					sql,
					dataSource.getConnection().getId()
				);
				xdsFields.clear();
				xdsFields.addAll(list);
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
			add(new WTextInput("name", new PropertyModel<String>(DataSourceForm.this, "dataSource.name")));
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

					item.add(new Label("name", field.getName()));
					item.add(new WTextInput("title", new PropertyModel<String>(field, "title")));
					item.add(new WSelectionInput("type", new PropertyModel<String>(field, "type"), Arrays.asList(XDSFieldType.values()), false));
					item.add(new WSelectionInput("filterType", new PropertyModel<String>(field, "filterType"), Arrays.asList(XDSFieldFilterType.values()), false));
					item.add(new WSelectionInput("placeType", new PropertyModel<String>(field, "placeType"), Arrays.asList(XDSFieldPlaceType.values()), false));
				}
			});
		}
	}
}
