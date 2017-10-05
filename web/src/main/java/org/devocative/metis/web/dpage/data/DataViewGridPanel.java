package org.devocative.metis.web.dpage.data;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.core.request.handler.IPartialPageRequestHandler;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.devocative.adroit.ConfigUtil;
import org.devocative.demeter.entity.EFileStatus;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.vo.filter.FileStoreFVO;
import org.devocative.demeter.web.DPanel;
import org.devocative.demeter.web.DTaskBehavior;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.dpage.FileStoreListDPage;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.XDSFieldResultType;
import org.devocative.metis.entity.data.config.XDVGridSelectionMode;
import org.devocative.metis.entity.data.config.XDVLink;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.data.IDataViewService;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.async.DataViewQVO;
import org.devocative.metis.vo.async.DataViewRVO;
import org.devocative.metis.vo.query.QueryExecInfoRVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.MetisWebParam;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.async.IAsyncResponse;
import org.devocative.wickomp.formatter.OBooleanFormatter;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.formatter.ONumberFormatter;
import org.devocative.wickomp.grid.*;
import org.devocative.wickomp.grid.column.OColumn;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OHiddenColumn;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.grid.column.link.OAjaxLinkColumn;
import org.devocative.wickomp.grid.toolbar.OAjaxLinkButton;
import org.devocative.wickomp.grid.toolbar.OGridGroupingButton;
import org.devocative.wickomp.grid.toolbar.OTreeGridClientButton;
import org.devocative.wickomp.html.WMessager;
import org.devocative.wickomp.html.icon.IconFont;
import org.devocative.wickomp.html.window.WModalWindow;
import org.devocative.wickomp.opt.OSize;
import org.devocative.wickomp.wrcs.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.io.Serializable;
import java.util.*;

public class DataViewGridPanel extends DPanel implements ITreeGridAsyncDataSource<Map<String, Object>>, IAsyncResponse {
	private static final long serialVersionUID = 6957270102281915596L;

	private static final Logger logger = LoggerFactory.getLogger(DataViewGridPanel.class);

	@Inject
	private IDataViewService dataViewService;

	@Inject
	private ISecurityService securityService;

	@Inject
	private IDataService dataService;

	private DataVO dataVO;
	private Map<String, Object> filter;
	private Map<String, String> sortFieldsMap;
	private DTaskBehavior taskBehavior;
	private WBaseGrid<Map<String, Object>> grid;

	private String sentDBConnection;
	private String selectionJSCallback;
	private Boolean multiSelect;
	private Map<String, List<String>> webParams;

	private List<QueryExecInfoRVO> queryExecInfoList;
	private WModalWindow modalWindow;

	// ------------------------------

	public DataViewGridPanel(String id, final DataVO dataVO, final Map<String, Object> filter) {
		super(id);

		this.dataVO = dataVO;
		this.filter = filter;
	}

	// ------------------------------

	public DataViewGridPanel setSelectionJSCallback(String selectionJSCallback) {
		this.selectionJSCallback = selectionJSCallback;
		return this;
	}

	public DataViewGridPanel setMultiSelect(boolean multiSelect) {
		this.multiSelect = multiSelect;
		return this;
	}

	public DataViewGridPanel setSentDBConnection(String sentDBConnection) {
		this.sentDBConnection = sentDBConnection;
		return this;
	}

	public DataViewGridPanel setWebParams(Map<String, List<String>> webParams) {
		this.webParams = webParams;
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
	public void onAsyncResult(IPartialPageRequestHandler handler, Object result) {
		DataViewRVO dataViewRVO = (DataViewRVO) result;

		queryExecInfoList = dataViewRVO.getQueryExecInfoList();

		if (dataViewRVO.getParentId() != null) {
			((WTreeGrid<Map<String, Object>>) grid).pushChildren(handler, dataViewRVO.getParentId(), dataViewRVO.getList());
		} else if (dataViewRVO.getFileId() == null) {
			grid.pushData(handler, dataViewRVO.getList(), dataViewRVO.getCount(), dataViewRVO.getFooter());
		} else {
			//TODO
			handler.appendJavaScript(String.format("location.href='%s';", UrlUtil.getFileUri(dataViewRVO.getFileId())));
		}
	}

	@Override
	public void onAsyncError(IPartialPageRequestHandler handler, Exception error) {
		if (error instanceof MetisException) {
			queryExecInfoList = ((MetisException) error).getExecInfoList();
		}

		grid.pushError(handler, error);
	}

	// ------------------------------ IGridAsyncDataSource

	@Override
	public void asyncList(long pageIndex, long pageSize, List<WSortField> sortFields) {
		sortFieldsMap = getSortFieldsMap(sortFields);

		DataViewQVO dataViewQVO = new DataViewQVO();
		dataViewQVO
			.setName(dataVO.getName())
			.setPageIndex(pageIndex)
			.setPageSize(pageSize)
			.setSortFieldList(sortFieldsMap)
			.setFilter(getFilterMap())
			.setSentDBConnection(sentDBConnection);

		dataService.executeDTask(dataViewQVO, taskBehavior);
	}

	// ------------------------------ ITreeGridAsyncDataSource

	@Override
	public void asyncListByParent(Serializable parentId, List<WSortField> sortFields) {
		DataViewQVO dataViewQVO = new DataViewQVO();
		dataViewQVO
			.setName(dataVO.getName())
			.setParentId(parentId)
			.setSortFieldList(sortFieldsMap)
			.setSentDBConnection(sentDBConnection);

		dataService.executeDTask(dataViewQVO, taskBehavior);
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

		if (dataVO.getSelectionValidationJS() != null) {
			String func = String.format("function %sSelValidJS(row){%s}", dataVO.getName(), dataVO.getSelectionValidationJS());
			response.render(JavaScriptHeaderItem.forScript(func, dataVO.getName()));
		}
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		modalWindow = new WModalWindow("modal");
		add(modalWindow);

		OColumnList<Map<String, Object>> columns = createColumns(dataVO);

		DataFieldVO selfRelPointerField = dataVO.findSelfRelPointerField();
		DataFieldVO titleField = dataVO.findTitleField();
		DataFieldVO keyField = dataVO.findKeyField();

		OBaseGrid<Map<String, Object>> oBaseGrid;
		if (selfRelPointerField == null) {
			OGrid<Map<String, Object>> gridOptions = new OGrid<>();
			gridOptions
				.setGroupStyle("background-color:#dddddd")
				.addToolbarButton(new OGridGroupingButton<>(MetisIcon.EXPAND, MetisIcon.COLLAPSE));

			oBaseGrid = gridOptions;
			grid = new WDataGrid<>("grid", gridOptions, this);
		} else {
			OTreeGrid<Map<String, Object>> gridOptions = new OTreeGrid<>();
			gridOptions
				.setParentIdField(selfRelPointerField.getName())
				.setTreeField(titleField != null ? titleField.getName() : null)
				.addToolbarButton(new OTreeGridClientButton<>(MetisIcon.COLLAPSE));

			oBaseGrid = gridOptions;
			grid = new WTreeGrid<>("grid", gridOptions, this);
		}

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
			.addToolbarButton(new OAjaxLinkButton<Map<String, Object>>(MetisIcon.EXPORT_EXCEL) {
				private static final long serialVersionUID = 3303989238841000829L;

				@Override
				public void onClick(AjaxRequestTarget target) {
					DataViewQVO dataViewQVO = new DataViewQVO();
					dataViewQVO
						.setName(DataViewGridPanel.this.dataVO.getName())
							//TODO .setSortFieldList(getSortFieldsMap(sortFields))
						.setFilter(getFilterMap())
						.setSortFieldList(sortFieldsMap)
						.setSentDBConnection(sentDBConnection)
						.setDoExport(true);

					dataService.executeDTask(dataViewQVO, taskBehavior);

					WMessager.show("Info", getString("msg.file.under.construction"), target);
				}
			})
			.addToolbarButton(new OAjaxLinkButton<Map<String, Object>>(MetisIcon.ATTACHMENT) {
				private static final long serialVersionUID = 8420976618508397333L;

				@Override
				public void onClick(AjaxRequestTarget target) {
					FileStoreFVO fvo = new FileStoreFVO();
					fvo.setTag(dataVO.getName());
					fvo.setCreatorUser(Collections.singletonList(securityService.getCurrentUser().toUser()));
					fvo.setStatus(Collections.singletonList(EFileStatus.VALID));

					modalWindow.setContent(
						new FileStoreListDPage(modalWindow.getContentId(), fvo)
							.setGridFit(true)
							.setFormVisible(false)
							.setRemoveColumns("mimeType", "storage", "status", "fileId",
								"creatorUser", "modificationDate", "modifierUser", "version", "EDIT")
					);
					modalWindow.getOptions()
						.setFit(null)
						.setWidth(OSize.fixed(700))
						.setHeight(OSize.fixed(400));
					modalWindow.show(target);
				}
			})
			.setReturnField(returnField)
			.setAsyncLoadingEnabled(ConfigUtil.getBoolean(MetisConfigKey.GridAsyncLoadingShow))
			.setFit(true)
		;

		if (ConfigUtil.getBoolean(MetisConfigKey.ShowSearchDebugger)) {
			oBaseGrid.addToolbarButton(new OAjaxLinkButton<Map<String, Object>>(MetisIcon.INFO) {
				private static final long serialVersionUID = 8420976618508397333L;

				@Override
				public void onClick(AjaxRequestTarget target) {
					modalWindow.setContent(new SearchDebugPanel(modalWindow.getContentId(), queryExecInfoList));
					modalWindow.getOptions()
						.setFit(null)
						.setWidth(OSize.fixed(900))
						.setHeight(OSize.fixed(600));
					modalWindow.show(target);
				}
			});
		}

		if (ConfigUtil.getBoolean(MetisConfigKey.GridNoResultShow)) {
			oBaseGrid.setNoResultMessage(getString("err.mts.NoResult"));
		}

		if (selectionJSCallback != null) {
			oBaseGrid.setSelectionJSHandler(selectionJSCallback);
		} else if (webParams.containsKey(MetisWebParam.WINDOW)) {

			//TODO wrap return object {type="search" data={}}

			if (dataVO.getSelectionValidationJS() == null) {
				oBaseGrid.setSelectionJSHandler("function(rows){parent.postMessage(JSON.stringify(rows),'*');}");
			} else {
				StringBuilder builder = new StringBuilder();
				builder.append("function(rows){")
					.append("for(var r=0;r<rows.length;r++){")
					.append("var row = rows[r];")
					.append(String.format("if(%1$sSelValidJS(row.row)){$.messager.alert('', %1$sSelValidJS(row.row));return;}", dataVO.getName()))
					.append("}")
					.append("parent.postMessage(JSON.stringify(rows),'*');}");
				oBaseGrid.setSelectionJSHandler(builder.toString());
			}
		}

		if (webParams.containsKey(MetisWebParam.MULTI_SELECT)) {
			multiSelect = getWebRequest()
				.getRequestParameters()
				.getParameterValue(MetisWebParam.MULTI_SELECT)
				.toBoolean();
		}
		oBaseGrid.setSingleSelect(multiSelect != null ? !multiSelect : null);

		add(grid);
		grid.setEnabled(false);

		taskBehavior = new DTaskBehavior(this);
		add(taskBehavior);
	}

	// ------------------------------

	private OColumnList<Map<String, Object>> createColumns(final DataVO dataVO) {
		List<String> disabledSortColumns = webParams.containsKey(MetisWebParam.DISABLE_SORT_COLUMN) ?
			webParams.get(MetisWebParam.DISABLE_SORT_COLUMN) :
			Collections.<String>emptyList();

		boolean disableAllSorts = false;
		if (webParams.containsKey(MetisWebParam.DISABLE_SORT_ALL_COLUMN)) {
			disableAllSorts = "1".equals(webParams.get(MetisWebParam.DISABLE_SORT_ALL_COLUMN).get(0));
		}

		OColumnList<Map<String, Object>> columns = new OColumnList<>();

		for (DataFieldVO fieldVO : dataVO.getFields()) {
			OColumn<Map<String, Object>> column;
			if (XDSFieldResultType.Shown.equals(fieldVO.getResultType())) {
				column = new OPropertyColumn<>(new Model<>(fieldVO.getTitleOrName()), fieldVO.getName());
				column.setSortable(!disabledSortColumns.contains(fieldVO.getName()) && !disableAllSorts);

				if (fieldVO.getColumnWidth() != null) {
					column.setWidth(OSize.fixed(fieldVO.getColumnWidth()));
				}

				/*
				TODO: the format and the read value of following types must be by user selection
				 */
				if (!fieldVO.getIsKeyFieldSafely() && !fieldVO.getIsSelfRelPointerFieldSafely()) {
					switch (fieldVO.getType()) {
						case Integer:
							column
								.setField("v_" + fieldVO.getName())
								.setFormatter(ONumberFormatter.integer())
								.setStyle("direction:ltr");

							columns.add(new OHiddenColumn<>(fieldVO.getName()));
							break;
						case Real:
							column
								.setField("v_" + fieldVO.getName())
								.setFormatter(ONumberFormatter.real())
								.setStyle("direction:ltr");

							columns.add(new OHiddenColumn<>(fieldVO.getName()));
							break;
						case Date:
							column
								.setField("v_" + fieldVO.getName())
								.setFormatter(ODateFormatter.prDate())
								.setStyle("direction:ltr");

							columns.add(
								new OHiddenColumn<Map<String, Object>>(fieldVO.getName())
									.setFormatter(ODateFormatter.millis()));
							break;
						case DateTime:
							column
								.setField("v_" + fieldVO.getName())
								.setFormatter(ODateFormatter.prDateTime())
								.setStyle("direction:ltr");

							columns.add(
								new OHiddenColumn<Map<String, Object>>(fieldVO.getName())
									.setFormatter(ODateFormatter.millis()));
							break;
						case Boolean:
							column.setFormatter(OBooleanFormatter.bool());
							break;
						default:
					}
				}

				column.setHasFooter(true);
				columns.add(column);

			} else if (XDSFieldResultType.Hidden.equals(fieldVO.getResultType())) {
				column = new OHiddenColumn<>(fieldVO.getName());
				columns.add(column);
			}
		}

		if (dataVO.getLinksToDV() != null) {
			for (final XDVLink xdvLink : dataVO.getLinksToDV()) {
				final DataView dataView = dataViewService.load(xdvLink.getTargetDVId());

				IconFont iconFont = MetisIcon.SEARCH.copyTo().setTooltip(new Model<>(dataView.getTitle()));

				columns.add(new OAjaxLinkColumn<Map<String, Object>>(new Model<>(xdvLink.getTitle()), iconFont) {
					private static final long serialVersionUID = 4824658945146230957L;

					@Override
					public void onClick(AjaxRequestTarget target, IModel<Map<String, Object>> rowData) {
						try {
							Map<String, Object> prevParamsMap = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
							prevParamsMap.putAll(webParams);
							prevParamsMap.putAll(filter);

							Map<String, Object> rowMap = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
							rowMap.putAll(rowData.getObject());

							Map<String, Object> targetFilter = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
							Map<String, List<String>> targetParams = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);

							dataService.processDynamicFilterAndParam(xdvLink.getSentData(), targetFilter, targetParams, rowMap, prevParamsMap);

							logger.info("Cross-Report: {} -> {}: params={}", dataVO.getName(), dataView.getName(), targetFilter);

							DataViewExecutorDPage dPage = new DataViewExecutorDPage(modalWindow.getContentId(), dataView.getName());
							dPage
								.setSentDBConnection(sentDBConnection)
								.setConsiderWebParams(false)
								.setWebParams(targetParams)
								.addToFilter(targetFilter);

							modalWindow.setContent(dPage);
							modalWindow
								.getOptions()
								.setFit(true);
							modalWindow.show(target);
						} catch (Exception e) {
							logger.error("Cross-Report: {} -> {}", dataVO.getName(), dataView.getName(), e);
							WMessager.show(e, target);
						}
					}
				});
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
		if (sortFieldList != null && !sortFieldList.isEmpty()) {
			sortFieldsMap = new LinkedHashMap<>();
			for (WSortField sortField : sortFieldList) {
				sortFieldsMap.put(sortField.getField(), sortField.getOrder());
			}
		}
		return sortFieldsMap;
	}
}
