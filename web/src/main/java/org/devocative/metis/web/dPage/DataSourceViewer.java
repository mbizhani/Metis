package org.devocative.metis.web.dPage;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.demeter.web.DPage;
import org.devocative.metis.entity.dataSource.config.XDSField;
import org.devocative.metis.entity.dataSource.config.XDSFieldFilterType;
import org.devocative.metis.entity.dataSource.config.XDSFieldType;
import org.devocative.metis.entity.dataSource.config.XDataSource;
import org.devocative.metis.iservice.IDataSourceService;
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

import javax.inject.Inject;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DataSourceViewer extends DPage {
	private static final Logger logger = LoggerFactory.getLogger(DataSourceViewer.class);

	@Inject
	private IDataSourceService dataSourceService;

	private SearchDataSource gridDS;
	private Map<String, Object> filters;
	private WDataGrid<Map<String, Object>> grid;

	private String dataSourceName;
	private XDataSource dataSource;

	public DataSourceViewer(String id, List<String> params) {
		super(id, params);

		WebMarkupContainer mainTable = new WebMarkupContainer("mainTable");
		add(mainTable);

		if (params.size() > 0) {
			filters = new HashMap<>();
			gridDS = new SearchDataSource();
			gridDS.setEnabled(false);

			dataSourceName = params.get(0);
			logger.info("DataSource param = {}", dataSourceName);
			dataSource = dataSourceService.getXDataSource(dataSourceName);
		} else {
			mainTable.setVisible(false);
			dataSource = new XDataSource();
			dataSource.setFields(new ArrayList<XDSField>());
			dataSourceName = "No DataSource defined!";
		}

		add(new Label("dataSourceName", dataSourceName));

		Form<Map<String, Object>> dynamicForm = new Form<>("dynamicForm", new CompoundPropertyModel<>(filters));
		dynamicForm.add(new ListView<XDSField>("fields", dataSource.getFields()) {
			@Override
			protected void populateItem(ListItem<XDSField> item) {
				XDSField dsField = item.getModelObject();
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
						if (XDSFieldFilterType.Range.equals(dsField.getFilterType())) {
							view.add(new WDateRangeInput(dsField.getName()).setTimePartVisible(XDSFieldType.DateTime == dsField.getType()));
						} else {
							view.add(new WDateInput(dsField.getName()).setTimePartVisible(XDSFieldType.DateTime == dsField.getType()));
						}
						break;

					case LookUp:
						List<KeyValueVO<Serializable, String>> lookUpList = dataSourceService.getLookUpList(dataSource, dsField);
						view.add(new WSelectionInput(dsField.getName(), lookUpList, dsField.getFilterType() == XDSFieldFilterType.List));
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
		mainTable.add(dynamicForm);

		OColumnList<Map<String, Object>> columns = new OColumnList<>();
		for (XDSField dsField : dataSource.getFields()) {
			if (XDSFieldType.LookUp != dsField.getType()) {
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

		mainTable.add(grid = new WDataGrid<>("grid", gridOptions, gridDS));

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
			return dataSourceService.executeDataSource(dataSourceName, filtersCloned, sortFieldsMap,
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
			return dataSourceService.getCountForDataSource(dataSourceName, filtersCloned);
		}

		@Override
		public IModel<Map<String, Object>> model(final Map<String, Object> objectMap) {
			return new WModel<>(objectMap);
		}
	}

}
