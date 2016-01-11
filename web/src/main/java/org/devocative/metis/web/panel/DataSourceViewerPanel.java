package org.devocative.metis.web.panel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.apache.wicket.request.IRequestParameters;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.metis.entity.dataSource.DSField;
import org.devocative.metis.entity.dataSource.DSFieldFilterType;
import org.devocative.metis.entity.dataSource.DSFieldType;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.service.DataSourceService;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.data.WDataSource;
import org.devocative.wickomp.data.WSortField;
import org.devocative.wickomp.form.*;
import org.devocative.wickomp.formatter.OBooleanFormatter;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.formatter.ONumberFormatter;
import org.devocative.wickomp.grid.OGrid;
import org.devocative.wickomp.grid.WDataGrid;
import org.devocative.wickomp.grid.column.OColumn;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.grid.toolbar.OExportExcelButton;
import org.devocative.wickomp.grid.toolbar.OGroupFieldButton;
import org.devocative.wickomp.html.icon.FontAwesome;
import org.devocative.wickomp.opt.OSize;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DataSourceViewerPanel extends Panel {
	private static final Logger logger = LoggerFactory.getLogger(DataSourceViewerPanel.class);

	private SearchDataSource gridDS;
	private Map<String, Object> filters;
	private WDataGrid<Map<String, Object>> grid;

	private String dataSourceName;

	public DataSourceViewerPanel(String id) {
		super(id);

		filters = new HashMap<>();
		gridDS = new SearchDataSource();
		gridDS.setEnabled(false);

		IRequestParameters parameters = getRequest().getRequestParameters();
		dataSourceName = parameters.getParameterValue("ds").toString("ItemDS");
		logger.info("DataSource param = {}", dataSourceName);


		final DataSource dataSource = DataSourceService.get().getDataSource(dataSourceName);

		Form<Map<String, Object>> dynamicForm = new Form<>("dynamicForm", new CompoundPropertyModel<>(filters));
		dynamicForm.add(new ListView<DSField>("fields", dataSource.getFields()) {
			@Override
			protected void populateItem(ListItem<DSField> item) {
				DSField dsField = item.getModelObject();
				item.add(new Label("label", dsField.getTitle()));

				RepeatingView view = new RepeatingView("field");
				switch (dsField.getType()) {

					case String:
						view.add(new WTextInput(dsField.getName()));
						break;

					case Integer:
						view.add(new WNumberInput(dsField.getName(), Long.class)
							.setThousandSeparator(","));
						break;

					case Real:
						view.add(new WNumberInput(dsField.getName(), BigDecimal.class).setPrecision(2)
							.setThousandSeparator(",")
							.setPrecision(3));
						break;

					case Boolean:
						view.add(new WBooleanInput(dsField.getName()));
						break;

					case Date:
					case DateTime:
						if (DSFieldFilterType.Range.equals(dsField.getFilterType())) {
							view.add(new WDateRangeInput(dsField.getName()).setTimePartVisible(DSFieldType.DateTime == dsField.getType()));
						} else {
							view.add(new WDateInput(dsField.getName()).setTimePartVisible(DSFieldType.DateTime == dsField.getType()));
						}
						break;

					case LookUp:
						List<KeyValueVO<Serializable, String>> lookUpList = DataSourceService.get().getLookUpList(dataSource, dsField);
						view.add(new WSelectionInput(dsField.getName(), lookUpList, dsField.getFilterType() == DSFieldFilterType.List));
						break;
				}

				item.add(view);
			}
		});
		dynamicForm.add(new AjaxButton("search") {
			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
				logger.debug("Map: {}", filters);
				gridDS.setEnabled(true);
				grid.loadData(target);
			}

		});
		add(dynamicForm);

		OColumnList<Map<String, Object>> columns = new OColumnList<>();
		for (DSField dsField : dataSource.getFields()) {
			if (DSFieldType.LookUp != dsField.getType()) {
				OColumn<Map<String, Object>> column = new OPropertyColumn<Map<String, Object>>(new Model<>(dsField.getTitle()), dsField.getName())
					.setSortable(true);
				switch (dsField.getType()) {
					case Integer:
						column.setFormatter(ONumberFormatter.integer());
						break;
					case Real:
						column.setFormatter(ONumberFormatter.real());
						break;
					case Date:
						column.setFormatter(ODateFormatter.prDate());
						break;
					case DateTime:
						column.setFormatter(ODateFormatter.prDateTime());
						break;
					case Boolean:
						column.setFormatter(OBooleanFormatter.bool());
						break;
				}
				columns.add(column);
			}
		}

		OGrid<Map<String, Object>> gridOptions = new OGrid<>();
		gridOptions
			.setColumns(columns)
			.setMultiSort(true)
			.setGroupStyle("background-color:#dddddd")
			.addToolbarButton(new OExportExcelButton<Map<String, Object>>(
				new FontAwesome("file-excel-o", "green", new ResourceModel("label.export.excel")),
				String.format("%s-Export.xlsx", dataSourceName),
				10000))
			.addToolbarButton(new OGroupFieldButton<Map<String, Object>>());
		gridOptions.setHeight(OSize.fixed(800));

		add(grid = new WDataGrid<>("grid", gridOptions, gridDS));

	}

	private class SearchDataSource extends WDataSource<Map<String, Object>> {
		@Override
		public List<Map<String, Object>> list(long pageIndex, long pageSize, List<WSortField> sortFieldList) {
			Map<String, String> sortFieldsMap = null;
			if (sortFieldList != null && sortFieldList.size() > 0) {
				sortFieldsMap = new HashMap<>();
				for (WSortField sortField : sortFieldList) {
					sortFieldsMap.put(sortField.getField(), sortField.getOrder());
				}
			}

			Map<String, Object> filtersCloned = new HashMap<>();
			for (Map.Entry<String, Object> entry : filters.entrySet()) {
				if (entry.getValue() != null) {
					filtersCloned.put(entry.getKey(), entry.getValue());
				}
			}
			return DataSourceService.get().executeDataSource(dataSourceName, filtersCloned, sortFieldsMap,
				pageIndex, pageSize);
		}

		@Override
		public long count() {
			Map<String, Object> filtersCloned = new HashMap<>();
			for (Map.Entry<String, Object> entry : filters.entrySet()) {
				if (entry.getValue() != null) {
					filtersCloned.put(entry.getKey(), entry.getValue());
				}
			}
			return DataSourceService.get().getCountForDataSource(dataSourceName, filtersCloned);
		}

		@Override
		public IModel<Map<String, Object>> model(final Map<String, Object> objectMap) {
			return new WModel<>(objectMap);
		}
	}

}
