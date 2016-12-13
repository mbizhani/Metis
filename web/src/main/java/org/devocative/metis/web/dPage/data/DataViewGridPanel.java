package org.devocative.metis.web.dPage.data;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.core.request.handler.IPartialPageRequestHandler;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.util.string.StringValueConversionException;
import org.devocative.adroit.ConfigUtil;
import org.devocative.demeter.entity.EFileStatus;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.iservice.template.IStringTemplate;
import org.devocative.demeter.iservice.template.IStringTemplateService;
import org.devocative.demeter.iservice.template.TemplateEngineType;
import org.devocative.demeter.vo.filter.FileStoreFVO;
import org.devocative.demeter.web.DPanel;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.dpage.FileStoreListDPage;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.XDSFieldResultType;
import org.devocative.metis.entity.data.config.XDVGridSelectionMode;
import org.devocative.metis.entity.data.config.XDVLink;
import org.devocative.metis.iservice.IDataViewService;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.async.DataViewQVO;
import org.devocative.metis.vo.async.DataViewRVO;
import org.devocative.metis.vo.query.QueryExecInfoRVO;
import org.devocative.metis.web.MetisDModule;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.MetisWebParam;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.WebUtil;
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
import org.devocative.wickomp.grid.column.link.OAjaxLinkColumn;
import org.devocative.wickomp.grid.toolbar.OAjaxLinkButton;
import org.devocative.wickomp.grid.toolbar.OGridGroupingButton;
import org.devocative.wickomp.grid.toolbar.OTreeGridClientButton;
import org.devocative.wickomp.html.WAjaxLink;
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

public class DataViewGridPanel extends DPanel implements ITreeGridAsyncDataSource<Map<String, Object>>, IAsyncResponseHandler {
	private static final long serialVersionUID = 6957270102281915596L;

	private static final Logger logger = LoggerFactory.getLogger(DataViewGridPanel.class);

	@Inject
	private IDataViewService dataViewService;

	@Inject
	private IStringTemplateService stringTemplateService;

	@Inject
	private ISecurityService securityService;

	private DataVO dataVO;
	private Map<String, Object> filter;
	private AsyncBehavior asyncBehavior;
	private WBaseGrid<Map<String, Object>> grid;
	private OBaseGrid<Map<String, Object>> oBaseGrid;

	private String sentDBConnection;

	private List<QueryExecInfoRVO> queryExecInfoList;
	private WModalWindow modalWindow;

	// ------------------------------

	public DataViewGridPanel(String id, final DataVO dataVO, final Map<String, Object> filter) {
		super(id);

		this.dataVO = dataVO;
		this.filter = filter;

		asyncBehavior = new AsyncBehavior(this);
		modalWindow = new WModalWindow("modal");

		add(asyncBehavior);
		add(modalWindow);

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
			grid = new WDataGrid<>("grid", gridOptions, this);
		} else {
			OTreeGrid<Map<String, Object>> gridOptions = new OTreeGrid<>();
			gridOptions
				.setParentIdField(selfRelPointerField.getName())
				.setTreeField(titleField != null ? titleField.getName() : null)
				.addToolbarButton(new OTreeGridClientButton<Map<String, Object>>(MetisIcon.COLLAPSE));

			oBaseGrid = gridOptions;
			grid = new WTreeGrid<>("grid", gridOptions, this);
		}

		add(grid);
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
			.addToolbarButton(new OAjaxLinkButton<Map<String, Object>>(MetisIcon.EXPORT_EXCEL) {
				private static final long serialVersionUID = 3303989238841000829L;

				@Override
				public void onClick(AjaxRequestTarget target) {
					DataViewQVO dataViewQVO = new DataViewQVO();
					dataViewQVO
						.setName(DataViewGridPanel.this.dataVO.getName())
							//TODO .setSortFieldList(getSortFieldsMap(sortFields))
						.setFilter(getFilterMap())
						.setSentDBConnection(sentDBConnection)
						.setDoExport(true);

					asyncBehavior.sendAsyncRequest(MetisDModule.EXEC_DATA_VIEW, dataViewQVO);

					WMessager.show("Info", getString("msg.file.under.construction"), target);
				}
			})
			.setReturnField(returnField)
			.setHeight(OSize.fixed(dataVO.getGridHeightSafely().getHeight()))
			.setWidth(OSize.percent(100))
		;

		if (!getWebRequest().getRequestParameters().getParameterValue(MetisWebParam.WINDOW).isEmpty()) {
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

		add(new WAjaxLink("info", MetisIcon.INFO) {
			private static final long serialVersionUID = 3303989238841000829L;

			@Override
			public void onClick(AjaxRequestTarget target) {
				modalWindow.setContent(new SearchDebugPanel(modalWindow.getContentId(), queryExecInfoList));
				modalWindow.getOptions()
					.setFit(null)
					.setWidth(OSize.fixed(900))
					.setHeight(OSize.fixed(600));
				modalWindow.show(target);
			}
		}.setVisible(ConfigUtil.getBoolean(MetisConfigKey.ShowSearchDebugger)));

		add(new WAjaxLink("attachment", MetisIcon.ATTACHMENT) {
			private static final long serialVersionUID = 2236601403443810728L;

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
		});

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

	public DataViewGridPanel setSentDBConnection(String sentDBConnection) {
		this.sentDBConnection = sentDBConnection;
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
			if (dataViewRVO.getFileId() == null) {
				grid.pushData(handler, dataViewRVO.getList(), dataViewRVO.getCount(), dataViewRVO.getFooter());
			} else {
				//TODO
				handler.appendJavaScript(String.format("location.href='%s';", UrlUtil.getFileUri(dataViewRVO.getFileId())));
			}
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
	public void asyncList(long pageIndex, long pageSize, List<WSortField> sortFields) {
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
	public void asyncListByParent(Serializable parentId, List<WSortField> sortFields) {
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

		if (dataVO.getSelectionValidationJS() != null) {
			String func = String.format("function %sSelValidJS(row){%s}", dataVO.getName(), dataVO.getSelectionValidationJS());
			response.render(JavaScriptHeaderItem.forScript(func, dataVO.getName()));
		}
	}

	// ------------------------------

	private OColumnList<Map<String, Object>> createColumns(final DataVO dataVO) {
		List<String> disabledSortColumns = WebUtil.listOf(MetisWebParam.DISABLE_SORT_COLUMN, false);

		OColumnList<Map<String, Object>> columns = new OColumnList<>();

		for (DataFieldVO fieldVO : dataVO.getFields()) {
			OColumn<Map<String, Object>> column;
			if (XDSFieldResultType.Shown.equals(fieldVO.getResultType())) {
				column = new OPropertyColumn<>(new Model<>(fieldVO.getTitleOrName()), fieldVO.getName());
				column.setSortable(!disabledSortColumns.contains(fieldVO.getName()));

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
							List<String> ignoredValues = ConfigUtil.getString(MetisConfigKey.IgnoreParameterValues) == null ?
								new ArrayList<String>() :
								Arrays.asList(ConfigUtil.getString(MetisConfigKey.IgnoreParameterValues).split("[,]"));

							Map<String, List<String>> urlParams = WebUtil.toMap(false, false);

							Map<String, Object> paramsMap = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
							for (Map.Entry<String, List<String>> entry : urlParams.entrySet()) {
								if (ignoredValues.isEmpty() || !ignoredValues.contains(entry.getValue().get(0))) {
									paramsMap.put(entry.getKey(), entry.getValue().get(0));
								} else {
									logger.warn("Cross-Report parameter [{}]=[{}] ignored!",
										entry.getKey(), entry.getValue().get(0));
								}
							}
							paramsMap.putAll(filter);

							Map<String, Object> targetFilter = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);

							Map<String, Object> rowMap = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
							rowMap.putAll(rowData.getObject());

							Map<String, Object> params = new HashMap<>();
							params.put("row", rowMap);
							params.put("params", paramsMap);
							params.put("filter", targetFilter);

							StringBuilder script = new StringBuilder();
							script
								.append("def range(l,u){new org.devocative.adroit.vo.RangeVO(l,u)}\n")
								.append("def now(){new Date()}\n")
								.append("def list(Object... p){def list=[]; p.each{list.add(it)}; return list}\n")
								.append(xdvLink.getSentData());

							IStringTemplate stringTemplate = stringTemplateService
								.create(script.toString(), TemplateEngineType.GroovyShell);

							stringTemplate.process(params);

							logger.info("Cross-Report: {} -> {}: params={}", dataVO.getName(), dataView.getName(), targetFilter);

							DataViewExecutorDPage dPage = new DataViewExecutorDPage(modalWindow.getContentId(), dataView.getName());
							dPage
								.setSentDBConnection(sentDBConnection)
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
			sortFieldsMap = new HashMap<>();
			for (WSortField sortField : sortFieldList) {
				sortFieldsMap.put(sortField.getField(), sortField.getOrder());
			}
		}
		return sortFieldsMap;
	}
}
