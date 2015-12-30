package org.devocative.metis.web;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.devocative.metis.entity.dataSource.DSField;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.service.DataSourceService;
import org.devocative.wickomp.data.WDataSource;
import org.devocative.wickomp.data.WSortField;
import org.devocative.wickomp.form.WNumberInput;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.grid.OGrid;
import org.devocative.wickomp.grid.WDataGrid;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.grid.toolbar.OExportExcelButton;
import org.devocative.wickomp.html.icon.FontAwesome;
import org.devocative.wickomp.opt.OSize;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Index extends WebPage {
	private static final Logger logger = LoggerFactory.getLogger(Index.class);

	private SearchDataSource gridDS;
	private Map<String, Object> filters;
	private WDataGrid<Map<String, Object>> grid;

	private String dataSourceName;

	public Index(PageParameters parameters) {
		super(parameters);

		filters = new HashMap<>();
		gridDS = new SearchDataSource();
		gridDS.setEnabled(false);

		dataSourceName = parameters.get("ds").toString("ItemDS");
		logger.info("DataSource param = {}", dataSourceName);

		final DataSource dataSource = DataSourceService.get().getDataSource(dataSourceName);

		Form<Map<String, Object>> dynamicForm = new Form<>("dynamicForm", new CompoundPropertyModel<>(filters));
		dynamicForm.add(new ListView<DSField>("fields", dataSource.getFields()) {
			@Override
			protected void populateItem(ListItem<DSField> item) {
				DSField field = item.getModelObject();
				item.add(new Label("label", field.getTitle()));
				//item.add(new TextField<String>("field"));

				RepeatingView view = new RepeatingView("field");
				switch (field.getType()) {

					case String:
						view.add(new WTextInput(field.getName()));
						break;

					case Integer:
						view.add(new WNumberInput(field.getName()).setGroupSeparator(","));
						break;

					case Real:
						view.add(new WNumberInput(field.getName()).setPrecision(2).setGroupSeparator(","));
						break;

					case Boolean:
						break;
				}

				item.add(view);
			}
		});
		dynamicForm.add(new AjaxButton("save") {
			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
				gridDS.setEnabled(true);
				grid.loadData(target);
			}

		});
		add(dynamicForm);

		OColumnList<Map<String, Object>> columns = new OColumnList<>();
		for (DSField dsField : dataSource.getFields()) {
			columns.add(new OPropertyColumn<Map<String, Object>>(new Model<>(dsField.getTitle()), dsField.getName()));
		}

		OGrid<Map<String, Object>> gridOptions = new OGrid<>();
		gridOptions
			.setColumns(columns)
			.setMultiSort(true)
			.addToolbarButton(new OExportExcelButton<Map<String, Object>>(
				new FontAwesome("file-excel-o", "green", new Model<>("Export to excel")),
				String.format("%s-Export.xlsx", dataSourceName),
				10000));
		gridOptions.setHeight(OSize.percent(100));

		add(grid = new WDataGrid<>("grid", gridOptions, gridDS));
	}

	private class SearchDataSource extends WDataSource<Map<String, Object>> {
		@Override
		public List<Map<String, Object>> list(long l, long l1, List<WSortField> list) {
			try {
				return DataSourceService.get().executeDataSource(dataSourceName, null, filters, null, null, null);
			} catch (SQLException e) {
				throw new RuntimeException(e);
			}
		}

		@Override
		public long count() {
			try {
				return DataSourceService.get().getCountForDataSource(dataSourceName, filters);
			} catch (SQLException e) {
				throw new RuntimeException(e);
			}
		}

		@Override
		public IModel<Map<String, Object>> model(final Map<String, Object> objectMap) {
			return new IModel<Map<String, Object>>() {
				@Override
				public Map<String, Object> getObject() {
					return objectMap;
				}

				@Override
				public void setObject(Map<String, Object> object) {
				}

				@Override
				public void detach() {
				}
			};
		}
	}
}
