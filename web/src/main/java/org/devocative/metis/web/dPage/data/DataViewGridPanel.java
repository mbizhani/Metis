package org.devocative.metis.web.dPage.data;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.core.request.handler.IPartialPageRequestHandler;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.util.string.StringValueConversionException;
import org.devocative.adroit.ConfigUtil;
import org.devocative.demeter.web.DPanel;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.data.config.XDSFieldResultType;
import org.devocative.metis.entity.data.config.XDVGridSelectionMode;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.async.DataViewQVO;
import org.devocative.metis.vo.async.DataViewRVO;
import org.devocative.metis.vo.query.QueryExecInfoRVO;
import org.devocative.metis.web.MetisDModule;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.MetisWebParam;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.async.AsyncBehavior;
import org.devocative.wickomp.async.IAsyncResponseHandler;
import org.devocative.wickomp.formatter.OBooleanFormatter;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.formatter.ONumberFormatter;
import org.devocative.wickomp.grid.*;
import org.devocative.wickomp.grid.column.OColumn;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OHiddenColumn;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.grid.toolbar.OGridGroupingButton;
import org.devocative.wickomp.grid.toolbar.OTreeGridClientButton;
import org.devocative.wickomp.html.WAjaxLink;
import org.devocative.wickomp.html.window.WModalWindow;
import org.devocative.wickomp.opt.OSize;
import org.devocative.wickomp.wrcs.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DataViewGridPanel extends DPanel implements ITreeGridAsyncDataSource<Map<String, Object>>, IAsyncResponseHandler {
	private static final long serialVersionUID = 6957270102281915596L;

	private static final Logger logger = LoggerFactory.getLogger(DataViewGridPanel.class);

	private DataVO dataVO;
	private Map<String, Object> filter;
	private AsyncBehavior asyncBehavior;
	private WBaseGrid<Map<String, Object>> grid;
	private OBaseGrid<Map<String, Object>> oBaseGrid;

	private String sentDBConnection;

	private List<QueryExecInfoRVO> queryExecInfoList;
	private WModalWindow modalWindow;

	// ------------------------------

	public DataViewGridPanel(String id, DataVO dataVO, Map<String, Object> filter) {
		super(id);
		this.dataVO = dataVO;
		this.filter = filter;

		add(asyncBehavior = new AsyncBehavior(this));
		add(modalWindow = new WModalWindow("modal"));

		OColumnList<Map<String, Object>> columns = createColumns(dataVO);

		DataFieldVO selfRelPointerField = dataVO.findSelfRelPointerField();
		DataFieldVO titleField = dataVO.findTitleField();
		DataFieldVO keyField = dataVO.findKeyField();

		if (selfRelPointerField == null) {
			OGrid<Map<String, Object>> gridOptions = new OGrid<>();
			gridOptions
				.setGroupStyle("background-color:#dddddd")
				.addToolbarButton(new OGridGroupingButton<Map<String, Object>>(MetisIcon.EXPAND, MetisIcon.COLLAPSE));

			oBaseGrid = gridOptions;
			add(grid = new WDataGrid<>("grid", gridOptions, this));
		} else {
			OTreeGrid<Map<String, Object>> gridOptions = new OTreeGrid<>();
			gridOptions
				.setParentIdField(selfRelPointerField.getName())
				.setTreeField(titleField != null ? titleField.getName() : null)
				.addToolbarButton(new OTreeGridClientButton<Map<String, Object>>(MetisIcon.COLLAPSE));

			oBaseGrid = gridOptions;
			add(grid = new WTreeGrid<>("grid", gridOptions, this));
		}

		grid.setEnabled(false);

		String returnField = getWebRequest().getRequestParameters().getParameterValue(MetisWebParam.RETURN_FIELD).toOptionalString();
		if (returnField != null) {
			logger.info("DataView [{}], changing return field to [{}]", dataVO.getName(), returnField);
		}

		oBaseGrid
			.setShowFooter(true)
			.setColumns(columns)
			.setMultiSort(true)
			.setSelectionIndicator(true)
			.setSingleSelect(dataVO.getSelectionModeSafely() == XDVGridSelectionMode.Single)
			.setIdField(keyField != null ? keyField.getName() : null)
			.setTitleField(titleField != null ? titleField.getName() : null)
			.setPageList(Arrays.asList(100, 200, 500, 1000))
			/*.addToolbarButton(new OExportExcelButton<Map<String, Object>>(
				MetisIcon.EXPORT_EXCEL,
				String.format("%s-export.xlsx", dataVO.getName()),
				10000))*/
			.setReturnField(returnField)
			.setHeight(OSize.fixed(dataVO.getGridHeightSafely().getHeight()))
			.setWidth(OSize.percent(100))
		;

		if (!getWebRequest().getRequestParameters().getParameterValue(MetisWebParam.WINDOW).isEmpty()) {
			oBaseGrid.setSelectionJSHandler("function(rows){parent.postMessage(JSON.stringify(rows),'*');}");
			//setEditButtonVisible(false);
		}

		if (ConfigUtil.hasKey(MetisConfigKey.DBConnParamName)) {
			sentDBConnection = getWebRequest()
				.getRequestParameters()
				.getParameterValue(ConfigUtil.getString(MetisConfigKey.DBConnParamName))
				.toOptionalString();
		}

		add(new WAjaxLink("debug") {
			private static final long serialVersionUID = 3303989238841000829L;

			@Override
			public void onClick(AjaxRequestTarget target) {
				modalWindow.setContent(new SearchDebugPanel(modalWindow.getContentId(), queryExecInfoList));
				modalWindow.show(target);
			}
		}.setVisible(ConfigUtil.getBoolean(MetisConfigKey.ShowSearchDebugger)));

		Boolean multiSelect;
		try {
			multiSelect = getWebRequest()
				.getRequestParameters()
				.getParameterValue(MetisWebParam.MULTI_SELECT)
				.toOptionalBoolean();
		} catch (StringValueConversionException e) {
			multiSelect = null;
		}

		if (multiSelect != null) {
			oBaseGrid.setSingleSelect(!multiSelect);
		}
	}

	// ------------------------------

	public DataViewGridPanel setSelectionJSCallback(String jsCallback) {
		grid.getOptions().setSelectionJSHandler(jsCallback);
		return this;
	}

	public DataViewGridPanel setMultiSelect(boolean multiSelect) {
		oBaseGrid.setSingleSelect(!multiSelect);
		return this;
	}

	public void loadData(AjaxRequestTarget target) {
		grid.setEnabled(true);
		grid.loadData(target);
	}

	public String getGridHtmlId() {
		return grid.getMarkupId();
	}

	// ------------------------------ IAsyncResponseHandler

	@Override
	public void onAsyncResult(String handlerId, IPartialPageRequestHandler handler, Serializable result) {
		DataViewRVO dataViewRVO = (DataViewRVO) result;

		queryExecInfoList = dataViewRVO.getQueryExecInfoList();

		if (MetisDModule.EXEC_DATA_VIEW.equals(handlerId)) {
			grid.pushData(handler, dataViewRVO.getList(), dataViewRVO.getCount(), dataViewRVO.getFooter());
		} else if (MetisDModule.EXEC_DATA_VIEW_CHILDREN.equals(handlerId)) {
			((WTreeGrid<Map<String, Object>>) grid).pushChildren(handler, dataViewRVO.getParentId(), dataViewRVO.getList());
		}
	}

	@Override
	public void onAsyncError(String handlerId, IPartialPageRequestHandler handler, Exception error) {
		if (error instanceof MetisException) {
			queryExecInfoList = ((MetisException) error).getExecInfoList();
		}

		grid.pushError(handler, error);
	}

	// ------------------------------ IGridAsyncDataSource

	@Override
	public void list(long pageIndex, long pageSize, List<WSortField> sortFields) {
		DataViewQVO dataViewQVO = new DataViewQVO();
		dataViewQVO
			.setName(dataVO.getName())
			.setPageIndex(pageIndex)
			.setPageSize(pageSize)
			.setSortFieldList(getSortFieldsMap(sortFields))
			.setFilter(getFilterMap())
			.setSentDBConnection(sentDBConnection);

		asyncBehavior.sendAsyncRequest(MetisDModule.EXEC_DATA_VIEW, dataViewQVO);
	}

	// ------------------------------ ITreeGridAsyncDataSource

	@Override
	public void listByParent(Serializable parentId, List<WSortField> sortFields) {
		DataViewQVO dataViewQVO = new DataViewQVO();
		dataViewQVO
			.setName(dataVO.getName())
			.setParentId(parentId)
			.setSortFieldList(getSortFieldsMap(sortFields))
			.setSentDBConnection(sentDBConnection);

		asyncBehavior.sendAsyncRequest(MetisDModule.EXEC_DATA_VIEW_CHILDREN, dataViewQVO);
	}

	@Override
	public boolean hasChildren(Map<String, Object> bean) {
		return true;
	}

	// ------------------------------ IDataSource

	@Override
	public IModel<Map<String, Object>> model(Map<String, Object> object) {
		return new WModel<>(object);
	}

	// ------------------------------

	@Override
	public void renderHead(IHeaderResponse response) {
		if (ConfigUtil.getBoolean(MetisConfigKey.ShowSearchDebugger)) {
			response.render(JavaScriptHeaderItem.forScript(String.format("%s = true;", Resource.WICKOMP_DEBUG_ENABLED_JS), "MetisEnableJsDebug"));
		}
	}

	// ------------------------------

	private OColumnList<Map<String, Object>> createColumns(DataVO dataVO) {
		OColumnList<Map<String, Object>> columns = new OColumnList<>();
		for (DataFieldVO fieldVO : dataVO.getFields()) {
			OColumn<Map<String, Object>> column;
			if (XDSFieldResultType.Shown.equals(fieldVO.getResultType())) {
				column = new OPropertyColumn<>(new Model<>(fieldVO.getTitleOrName()), fieldVO.getName());
				column.setSortable(true);

				if (fieldVO.getColumnWidth() != null) {
					column.setWidth(OSize.fixed(fieldVO.getColumnWidth()));
				}

				if (!fieldVO.getIsKeyFieldSafely() && !fieldVO.getIsSelfRelPointerFieldSafely()) {
					switch (fieldVO.getType()) {
						case Integer:
							column
								.setFormatter(ONumberFormatter.integer())
								.setStyle("direction:ltr");
							break;
						case Real:
							column
								.setFormatter(ONumberFormatter.real())
								.setStyle("direction:ltr");
							break;
						case Date:
							column
								.setFormatter(ODateFormatter.prDate())
								.setStyle("direction:ltr");
							break;
						case DateTime:
							column
								.setFormatter(ODateFormatter.prDateTime())
								.setStyle("direction:ltr");
							break;
						case Boolean:
							column.setFormatter(OBooleanFormatter.bool());
							break;
					}
				}

				column.setHasFooter(true);
				columns.add(column);

			} else if (XDSFieldResultType.Hidden.equals(fieldVO.getResultType())) {
				column = new OHiddenColumn<>(fieldVO.getName());
				columns.add(column);
			}
		}

		return columns;
	}

	private Map<String, Object> getFilterMap() {
		Map<String, Object> filtersCloned = new HashMap<>();
		for (Map.Entry<String, Object> entry : filter.entrySet()) {
			if (entry.getValue() != null) {
				filtersCloned.put(entry.getKey(), entry.getValue());
			}
		}
		return filtersCloned;
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
