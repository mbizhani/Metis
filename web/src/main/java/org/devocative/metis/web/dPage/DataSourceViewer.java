package org.devocative.metis.web.dPage;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.DemeterWebSession;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.entity.dataSource.config.*;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.data.WSortField;
import org.devocative.wickomp.data.WTreeGridDataSource;
import org.devocative.wickomp.form.*;
import org.devocative.wickomp.formatter.OBooleanFormatter;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.formatter.ONumberFormatter;
import org.devocative.wickomp.grid.*;
import org.devocative.wickomp.grid.column.OColumn;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.grid.toolbar.OExportExcelButton;
import org.devocative.wickomp.grid.toolbar.OGridGroupingButton;
import org.devocative.wickomp.grid.toolbar.OTreeGridClientButton;
import org.devocative.wickomp.opt.OLayoutDirection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.*;

public class DataSourceViewer extends DPage {
	private static final Logger logger = LoggerFactory.getLogger(DataSourceViewer.class);

	@Inject
	private IDataSourceService dataSourceService;

	private SearchDataSource gridDS;
	private Map<String, Object> filters;
	private WBaseGrid<Map<String, Object>> grid;

	private DataSource dataSource;
	private List<XDSField> xdsFieldList;
	private List<XDSParameter> xdsParameterList;

	public DataSourceViewer(String id, List<String> params) {
		super(id, params);

		WebMarkupContainer mainTable = new WebMarkupContainer("mainTable");
		add(mainTable);

		String title;
		if (params.size() > 0) {
			dataSource = dataSourceService.getDataSource(params.get(0));
			logger.info("DataSource param = {}", dataSource.getName());
			XDataSource xDataSource = dataSourceService.getXDataSource(dataSource);
			xdsFieldList = xDataSource.getFields();
			xdsParameterList = xDataSource.getParams();
			title = String.format("%s (%s)", dataSource.getTitle(), dataSource.getName());

			filters = new HashMap<>();
			gridDS = new SearchDataSource();
			gridDS.setEnabled(false);
		} else {
			mainTable.setVisible(false);
			title = "No DataSource defined!"; // TODO use ResourceModel
		}

		add(new Label("title", title));

		WebMarkupContainer searchPanel = new WebMarkupContainer("searchPanel");
		if (DemeterWebSession.get().getLayoutDirection() == OLayoutDirection.RTL) {
			searchPanel.add(new AttributeModifier("data-options", "region:'east',split:true"));
		} else {
			searchPanel.add(new AttributeModifier("data-options", "region:'west',split:true"));
		}
		mainTable.add(searchPanel);

		Form<Map<String, Object>> dynamicForm = new Form<>("dynamicForm", new CompoundPropertyModel<>(filters));
		dynamicForm.add(new ListView<XDSAbstractField>("fields", getFieldForFilter()) {
			@Override
			protected void populateItem(ListItem<XDSAbstractField> item) {
				XDSAbstractField dsField = item.getModelObject();
				item.add(new Label("label", dsField.getSafeTitle()));

				FormComponent fieldFormItem = null;
				switch (dsField.getType()) {

					case String:
						fieldFormItem = new WTextInput(dsField.getName());
						break;

					case Integer:
						if (XDSFieldFilterType.Range == dsField.getFilterType()) {
							fieldFormItem = new WNumberRangeInput(dsField.getName(), Long.class).setThousandSeparator(",");
						} else {
							fieldFormItem = new WNumberInput(dsField.getName(), Long.class)
								.setThousandSeparator(",");
						}
						break;

					case Real:
						if (XDSFieldFilterType.Range == dsField.getFilterType()) {
							fieldFormItem = new WNumberRangeInput(dsField.getName(), BigDecimal.class).setPrecision(2)
								.setThousandSeparator(",")
								.setPrecision(3);
						} else {
							fieldFormItem = new WNumberInput(dsField.getName(), BigDecimal.class).setPrecision(2)
								.setThousandSeparator(",")
								.setPrecision(3);
						}
						break;

					case Boolean:
						fieldFormItem = new WBooleanInput(dsField.getName());
						break;

					case Date:
					case DateTime:
						if (XDSFieldFilterType.Range == dsField.getFilterType()) {
							fieldFormItem = new WDateRangeInput(dsField.getName()).setTimePartVisible(XDSFieldType.DateTime == dsField.getType());
						} else {
							fieldFormItem = new WDateInput(dsField.getName()).setTimePartVisible(XDSFieldType.DateTime == dsField.getType());
						}
						break;

					case LookUp:
						if (dsField.getFilterType() == XDSFieldFilterType.List) {
							List<KeyValueVO<Serializable, String>> lookUpList = dataSourceService.getLookUpList(dsField);
							fieldFormItem = new WSelectionInput(dsField.getName(), lookUpList, true);
						} else {
							//TODO XDSFieldFilterType.Search
						}
						break;
				}

				RepeatingView view = new RepeatingView("field");
				if (fieldFormItem != null) {
					fieldFormItem
						.setLabel(new Model<>(dsField.getTitle()))
						.setRequired(dsField.getRequired());
					view.add(fieldFormItem);
				}
				item.add(view);
			}
		});
		dynamicForm.add(new DAjaxButton("search", new ResourceModel("label.search", "Search"), MetisIcon.SEARCH) {
			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				logger.debug("Map: {}", filters);
				gridDS.setEnabled(true);
				grid.loadData(target);
			}

		});
		searchPanel.add(dynamicForm);

		OColumnList<Map<String, Object>> columns = new OColumnList<>();
		for (XDSField dsField : xdsFieldList) {
			if (XDSFieldType.LookUp != dsField.getType()) {
				switch (dsField.getResultType()) {
					case None:
						break;
					case Shown:
						OColumn<Map<String, Object>> column = new OPropertyColumn<Map<String, Object>>(new Model<>(dsField.getSafeTitle()), dsField.getName())
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
						break;
					case Hidden:
						//TODO add hidden column, usefull for client interaction
						break;
				}
			}
		}

		OBaseGrid<Map<String, Object>> oBaseGrid;

		if (dataSource.getSelfRelPointerField() == null) {
			OGrid<Map<String, Object>> gridOptions = new OGrid<>();
			gridOptions
				.setGroupStyle("background-color:#dddddd")
				.setIdField(dataSource.getKeyField())
				.addToolbarButton(new OGridGroupingButton<Map<String, Object>>(MetisIcon.EXPAND, MetisIcon.COLLAPSE));

			oBaseGrid = gridOptions;
			mainTable.add(grid = new WDataGrid<>("grid", gridOptions, gridDS));
		} else {
			OTreeGrid<Map<String, Object>> gridOptions = new OTreeGrid<>();
			gridOptions
				.setParentIdField(dataSource.getSelfRelPointerField())
				.setTreeField(dataSource.getTitleField())
				.setIdField(dataSource.getKeyField())
				.addToolbarButton(new OTreeGridClientButton<Map<String, Object>>(MetisIcon.COLLAPSE));

			oBaseGrid = gridOptions;
			mainTable.add(grid = new WTreeGrid<>("grid", gridOptions, gridDS));
		}

		oBaseGrid
			.setColumns(columns)
			.setMultiSort(true)
			.setSelectionIndicator(true)
			.addToolbarButton(new OExportExcelButton<Map<String, Object>>(
				MetisIcon.EXPORT_EXCEL,
				String.format("%s-export.xlsx", dataSource.getName()),
				10000))
			.setFit(true)
		;
	}

	private List<XDSAbstractField> getFieldForFilter() {
		List<XDSAbstractField> result = new ArrayList<>();

		if (xdsParameterList != null) {
			result.addAll(xdsParameterList);
		}

		if (xdsFieldList != null) {
			for (XDSField field : xdsFieldList) {
				if (field.getInFilterPanel()) {
					result.add(field);
				}
			}
		}
		return result;
	}

	private Map<String, Object> getFilterMap() {
		Map<String, Object> filtersCloned = new HashMap<>();
		for (Map.Entry<String, Object> entry : filters.entrySet()) {
			if (entry.getValue() != null) {
				filtersCloned.put(entry.getKey(), entry.getValue());
			}
		}
		return filtersCloned;
	}

	private class SearchDataSource extends WTreeGridDataSource<Map<String, Object>> {
		@Override
		public List<Map<String, Object>> list(long pageIndex, long pageSize, List<WSortField> sortFieldList) {
			return dataSourceService.executeDataSource(dataSource.getName(), getFilterMap(), getSortFieldsMap(sortFieldList),
				pageIndex, pageSize);
		}

		@Override
		public List<Map<String, Object>> listByIds(Set<Serializable> ids, List<WSortField> sortFieldList) {
			Map<String, Object> filters = new HashMap<>();
			filters.put(dataSource.getKeyField(), ids);

			return dataSourceService.executeDataSource(dataSource.getName(), filters, getSortFieldsMap(sortFieldList),
				null, null);
		}

		@Override
		public List<Map<String, Object>> listByParent(Serializable parentId, List<WSortField> sortFieldList) {
			return dataSourceService.getChildrenOfParent(dataSource.getName(), parentId, getSortFieldsMap(sortFieldList));
		}

		@Override
		public boolean hasChildren(Map<String, Object> bean) {
			return true;
		}

		@Override
		public long count() {
			return dataSourceService.getCountForDataSource(dataSource.getName(), getFilterMap());
		}

		@Override
		public IModel<Map<String, Object>> model(final Map<String, Object> objectMap) {
			return new WModel<>(objectMap);
		}

		private Map<String, String> getSortFieldsMap(List<WSortField> sortFieldList) {
			Map<String, String> sortFieldsMap = null;
			if (sortFieldList != null && sortFieldList.size() > 0) {
				sortFieldsMap = new HashMap<>();
				for (WSortField sortField : sortFieldList) {
					sortFieldsMap.put(sortField.getField(), sortField.getOrder());
				}
			}
			return sortFieldsMap;
		}
	}
}
