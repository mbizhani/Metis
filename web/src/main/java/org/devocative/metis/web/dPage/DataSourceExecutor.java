package org.devocative.metis.web.dPage;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.core.request.handler.IPartialPageRequestHandler;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.adroit.ObjectBuilder;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.entity.dataSource.config.*;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.vo.async.DataSourceRVO;
import org.devocative.metis.web.MetisDModule;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.async.AsyncBehavior;
import org.devocative.wickomp.async.IAsyncResponseHandler;
import org.devocative.wickomp.form.*;
import org.devocative.wickomp.formatter.OBooleanFormatter;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.formatter.ONumberFormatter;
import org.devocative.wickomp.grid.*;
import org.devocative.wickomp.grid.column.OColumn;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OHiddenColumn;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.grid.toolbar.OExportExcelButton;
import org.devocative.wickomp.grid.toolbar.OGridGroupingButton;
import org.devocative.wickomp.grid.toolbar.OTreeGridClientButton;
import org.devocative.wickomp.html.WEasyLayout;
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

public class DataSourceExecutor extends DPage implements IAsyncResponseHandler {
	private static final Logger logger = LoggerFactory.getLogger(DataSourceExecutor.class);

	@Inject
	private IDataSourceService dataSourceService;

	@Inject
	private ISecurityService securityService;

	private Map<String, Object> filters;
	private WBaseGrid<Map<String, Object>> grid;
	private AsyncBehavior asyncBehavior;

	private DataSource dataSource;
	private List<XDSField> xdsFieldList;
	private List<XDSParameter> xdsParameterList;

	private WebMarkupContainer edit;

	public DataSourceExecutor(String id, DataSource ds) {
		this(id, new ArrayList<String>(), ds);
	}

	public DataSourceExecutor(String id, List<String> params) {
		this(id, params, null);
	}

	// Main Constructor
	public DataSourceExecutor(String id, List<String> params, DataSource ds) {
		super(id, params);

		WebMarkupContainer searchPanel = new WebMarkupContainer("searchPanel");

		WEasyLayout mainTable = new WEasyLayout("mainTable");
		mainTable.setWest(searchPanel);
		add(mainTable);

		dataSource = ds;
		if (params.size() > 0) {
			dataSource = dataSourceService.getDataSource(params.get(0));
		}

		String title;
		if (dataSource != null) {
			logger.info("DataSource param = {}", dataSource.getName());
			XDataSource xDataSource = dataSourceService.getXDataSource(dataSource);
			xdsFieldList = xDataSource.getFields();
			xdsParameterList = xDataSource.getParams();
			title = String.format("%s (%s)", dataSource.getTitle(), dataSource.getName());

			filters = new HashMap<>();

			String editUri = String.format("%s/%s", UrlUtil.createUri(DataSourceForm.class, true), dataSource.getName());
			edit = new ExternalLink("edit", editUri);
		} else {
			mainTable.setVisible(false);
			title = "No DataSource defined!"; // TODO use ResourceModel

			edit = new WebMarkupContainer("edit");
			edit.setVisible(false);
		}

		add(new Label("title", title));
		add(edit);

		Form<Map<String, Object>> dynamicForm = new Form<>("dynamicForm", new CompoundPropertyModel<>(filters));
		dynamicForm.add(new ListView<XDSAbstractField>("fields", getFieldForFilter()) {
			@Override
			protected void populateItem(ListItem<XDSAbstractField> item) {
				final XDSAbstractField dsField = item.getModelObject();
				item.add(new Label("label", dsField.getSafeTitle()));

				FormComponent fieldFormItem = null;
				switch (dsField.getType()) {

					case String:
						fieldFormItem = new WTextInput(dsField.getName())
							.setLabelVisible(false);
						break;

					case Integer:
						if (XDSFieldFilterType.Range == dsField.getFilterType()) {
							fieldFormItem = new WNumberRangeInput(dsField.getName(), Long.class)
								.setThousandSeparator(",")
								.setLabelVisible(false);
						} else {
							fieldFormItem = new WNumberInput(dsField.getName(), Long.class)
								.setThousandSeparator(",")
								.setLabelVisible(false);
						}
						break;

					case Real:
						if (XDSFieldFilterType.Range == dsField.getFilterType()) {
							fieldFormItem = new WNumberRangeInput(dsField.getName(), BigDecimal.class).setPrecision(2)
								.setThousandSeparator(",")
								.setPrecision(3)
								.setLabelVisible(false);
						} else {
							fieldFormItem = new WNumberInput(dsField.getName(), BigDecimal.class).setPrecision(2)
								.setThousandSeparator(",")
								.setPrecision(3)
								.setLabelVisible(false);
						}
						break;

					case Boolean:
						fieldFormItem = new WBooleanInput(dsField.getName())
							.setLabelVisible(false);
						break;

					case Date:
					case DateTime:
						if (XDSFieldFilterType.Range == dsField.getFilterType()) {
							fieldFormItem = new WDateRangeInput(dsField.getName()).setTimePartVisible(XDSFieldType.DateTime == dsField.getType())
								.setLabelVisible(false);
						} else {
							fieldFormItem = new WDateInput(dsField.getName()).setTimePartVisible(XDSFieldType.DateTime == dsField.getType())
								.setLabelVisible(false);
						}
						break;

					case LookUp:
						if (dsField.getFilterType() == XDSFieldFilterType.List) {
							List<KeyValueVO<Serializable, String>> lookUpList = dataSourceService.getLookUpList(dsField);
							fieldFormItem = new WSelectionInput(dsField.getName(), lookUpList, true)
								.setLabelVisible(false);
						} else {
							fieldFormItem = new WClientSearchableListInput<KeyValueVO<Serializable, String>>(dsField.getName()) {
								{
									getModalWindowOptions().setWidth(OSize.percent(80));
								}

								@Override
								protected Component createSelectionPanel(String selectionPanelId) {
									DataSource dataSource = dataSourceService.get(dsField.getTargetId());

									getModalWindowOptions().setTitle(String.format("%s: %s (%s)",
										dsField.getSafeTitle(), dataSource.getTitle(), dataSource.getName()));

									return
										new DataSourceExecutor(selectionPanelId, dataSource)
											.setSelectionJSCallback(getJSCallback())
											.setEditButtonVisible(false);
								}

								@Override
								protected KeyValueVO<Serializable, String> createServerObject(String key) {
									return new KeyValueVO<Serializable, String>(key, null);
								}
							}
								.setLabelVisible(false);
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
				logger.info("Execute search [{}] with parameters (User={}): {}",
					dataSource.getName(), securityService.getCurrentUser().getUsername(), filters);
				grid.setEnabled(true);
				grid.loadData(target);
			}
		});
		searchPanel.add(dynamicForm);

		OColumnList<Map<String, Object>> columns = new OColumnList<>();
		for (XDSField dsField : xdsFieldList) {
			switch (dsField.getResultType()) {
				case None:
					break;
				case Hidden:
				case Shown:
					OColumn<Map<String, Object>> column;
					if (dsField.getResultType() == XDSFieldResultType.Shown)
						column = new OPropertyColumn<Map<String, Object>>(new Model<>(dsField.getSafeTitle()), dsField.getName())
							.setSortable(true);
					else {
						column = new OHiddenColumn<>(dsField.getName());
					}

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
			}
		}

		OBaseGrid<Map<String, Object>> oBaseGrid;

		if (dataSource.getSelfRelPointerField() == null) {
			OGrid<Map<String, Object>> gridOptions = new OGrid<>();
			gridOptions
				.setGroupStyle("background-color:#dddddd")
				.addToolbarButton(new OGridGroupingButton<Map<String, Object>>(MetisIcon.EXPAND, MetisIcon.COLLAPSE));

			oBaseGrid = gridOptions;
			mainTable.add(grid = new WDataGrid<>("grid", gridOptions, new SearchDataSource()));
		} else {
			OTreeGrid<Map<String, Object>> gridOptions = new OTreeGrid<>();
			gridOptions
				.setParentIdField(dataSource.getSelfRelPointerField())
				.setTreeField(dataSource.getTitleField())
				.addToolbarButton(new OTreeGridClientButton<Map<String, Object>>(MetisIcon.COLLAPSE));

			oBaseGrid = gridOptions;
			//mainTable.add(grid = new WTreeGrid<>("grid", gridOptions, new SearchDataSource()));
		}

		grid.setEnabled(false);

		oBaseGrid
			.setColumns(columns)
			.setMultiSort(true)
			.setSelectionIndicator(true)
			.setIdField(dataSource.getKeyField())
			.setTitleField(dataSource.getTitleField())
			.addToolbarButton(new OExportExcelButton<Map<String, Object>>(
				MetisIcon.EXPORT_EXCEL,
				String.format("%s-export.xlsx", dataSource.getName()),
				10000))
			.setFit(true)
		;

		if (!getWebRequest().getRequestParameters().getParameterValue("window").isEmpty()) {
			oBaseGrid.setSelectionJSHandler("function(rows){parent.postMessage(JSON.stringify(rows),'*');}");
			setEditButtonVisible(false);
		}

		add(asyncBehavior = new AsyncBehavior(this));
	}

	public DataSourceExecutor setSelectionJSCallback(String jsCallback) {
		grid.getOptions().setSelectionJSHandler(jsCallback);
		return this;
	}

	public DataSourceExecutor setEditButtonVisible(boolean visible) {
		edit.setVisible(visible);
		return this;
	}

	@Override
	public void onAsyncResult(String handlerId, IPartialPageRequestHandler handler, Serializable result) {
		DataSourceRVO dataSourceRVO = (DataSourceRVO) result;
		grid.pushData(handler, dataSourceRVO.getList(), dataSourceRVO.getCount());
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

	private class SearchDataSource implements IGridAsyncDataSource<Map<String, Object>> {
		//------------------------ IGridDataSource

		@Override
		public void list(long pageIndex, long pageSize, List<WSortField> sortFieldList) {
			/*return dataSourceService.executeDataSource(dataSource.getName(), getFilterMap(), getSortFieldsMap(sortFieldList),
				pageIndex, pageSize);*/
			asyncBehavior.sendAsyncRequest(MetisDModule.EXEC_DATA_SOURCE,
				ObjectBuilder
					.createMap(new HashMap<>())
					.put("name", dataSource.getName())
					.put("pageIndex", pageIndex)
					.put("pageSize", pageSize)
					.put("sortFieldList", getSortFieldsMap(sortFieldList))
					.put("filter", getFilterMap())
					.get()
			);
		}

		/*@Override
		public long count() {
			return dataSourceService.getCountForDataSource(dataSource.getName(), getFilterMap());
		}*/

		@Override
		public IModel<Map<String, Object>> model(final Map<String, Object> objectMap) {
			return new WModel<>(objectMap);
		}

		//------------------------ ITreeGridDataSource

		/*@Override
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
		}*/

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
