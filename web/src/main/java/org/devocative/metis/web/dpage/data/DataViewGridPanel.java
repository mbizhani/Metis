package org.devocative.metis.web.dpage.data;

import com.fasterxml.jackson.core.type.TypeReference;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.core.request.handler.IPartialPageRequestHandler;
import org.apache.wicket.markup.head.HeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.devocative.adroit.ConfigUtil;
import org.devocative.demeter.entity.EFileStatus;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.iservice.template.IStringTemplate;
import org.devocative.demeter.iservice.template.IStringTemplateService;
import org.devocative.demeter.iservice.template.TemplateEngineType;
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
import org.devocative.wickomp.WebUtil;
import org.devocative.wickomp.async.IAsyncResponse;
import org.devocative.wickomp.formatter.OBooleanFormatter;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.formatter.ONumberFormatter;
import org.devocative.wickomp.grid.*;
import org.devocative.wickomp.grid.column.*;
import org.devocative.wickomp.grid.column.link.OAjaxLinkColumn;
import org.devocative.wickomp.grid.toolbar.OAjaxLinkButton;
import org.devocative.wickomp.grid.toolbar.OGridGroupingButton;
import org.devocative.wickomp.grid.toolbar.OTreeGridClientButton;
import org.devocative.wickomp.html.WMessager;
import org.devocative.wickomp.html.icon.FontAwesome;
import org.devocative.wickomp.html.icon.IconFont;
import org.devocative.wickomp.html.window.WModalWindow;
import org.devocative.wickomp.opt.IStyler;
import org.devocative.wickomp.opt.OSize;
import org.devocative.wickomp.opt.OStyle;
import org.devocative.wickomp.wrcs.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.io.Serializable;
import java.util.*;

public class DataViewGridPanel extends DPanel implements ITreeGridAsyncDataSource<Map<String, Object>>, IAsyncResponse<DataViewRVO> {
	private static final long serialVersionUID = 6957270102281915596L;
	private static final Logger logger = LoggerFactory.getLogger(DataViewGridPanel.class);
	private static final HeaderItem GRID_SEND_JS = JavaScriptHeaderItem.forReference(
		new JavaScriptResourceReference(DataViewGridPanel.class, "res/DataViewGridPanel.js"));

	private static final String FIELD_PREFIX = "vv_";

	@Inject
	private IDataViewService dataViewService;

	@Inject
	private ISecurityService securityService;

	@Inject
	private IDataService dataService;

	@Inject
	private IStringTemplateService stringTemplateService;

	private DataVO dataVO;
	private Map<String, Object> filter;
	private Map<String, String> sortFieldsMap;
	private DTaskBehavior<DataViewRVO> taskBehavior;
	private WBaseGrid<Map<String, Object>> grid;

	private String selectionJSCallback;
	private Boolean multiSelect;
	private Map<String, List<String>> webParams;

	private List<QueryExecInfoRVO> queryExecInfoList;
	private WModalWindow modalWindow;
	private WebMarkupContainer layout;

	private String execTime = "?";

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
	public void onAsyncResult(IPartialPageRequestHandler handler, DataViewRVO result) {
		execTime = securityService.getCurrentUser().formatDate(new Date(), "yyyyMMddHHmm");

		queryExecInfoList = result.getQueryExecInfoList();

		if (result.getParentId() != null) {
			((WTreeGrid<Map<String, Object>>) grid).pushChildren(handler, result.getParentId(), result.getList());
		} else if (result.getFileId() == null) {
			grid.pushData(handler, result.getList(), result.getCount(), result.getFooter());
		} else {
			//TODO
			//handler.appendJavaScript(String.format("window.open('%s', '_blank');", UrlUtil.getFileUri(result.getFileId())));
			handler.appendJavaScript(String.format("$('<a>').attr('href', '%s').attr('target', '_blank')[0].click();",
				UrlUtil.getFileUri(result.getFileId())));
		}
	}

	@Override
	public void onAsyncError(IPartialPageRequestHandler handler, Exception error) {
		execTime = securityService.getCurrentUser().formatDate(new Date(), "yyyyMMddHHmm");

		if (error instanceof MetisException) {
			queryExecInfoList = ((MetisException) error).getExecInfoList();
			if (error.getCause() != null) {
				error = (Exception) error.getCause();
			}
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
		;

		dataService.executeDTask(dataViewQVO, taskBehavior);
	}

	// ------------------------------ ITreeGridAsyncDataSource

	@Override
	public void asyncListByParent(Serializable parentId, List<WSortField> sortFields) {
		DataViewQVO dataViewQVO = new DataViewQVO();
		dataViewQVO
			.setName(dataVO.getName())
			.setParentId(parentId)
			.setSortFieldList(sortFieldsMap);

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
			String func = String.format(
				"function %1$sSelValidJS(row){%2$s}\n" +
					"function %1$sSelValidJSAll(rows){" +
					" for(var r=0; r<rows.length; r++){" +
					"  var row = rows[r];" +
					"  if(%1$sSelValidJS(row.row)){" +
					"   $.messager.alert('', %1$sSelValidJS(row.row));" +
					"   return false;" +
					"  }" +
					" }" +
					" return true;" +
					"}",
				dataVO.getName(), dataVO.getSelectionValidationJS());
			response.render(JavaScriptHeaderItem.forScript(func, dataVO.getName()));
		}

		response.render(GRID_SEND_JS);
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		modalWindow = new WModalWindow("modal");
		add(modalWindow);

		if (multiSelect == null) {
			multiSelect = dataVO.getSelectionModeSafely() == XDVGridSelectionMode.Multiple;
		}

		if (webParams.containsKey(MetisWebParam.MULTI_SELECT)) {
			multiSelect = getWebRequest()
				.getRequestParameters()
				.getParameterValue(MetisWebParam.MULTI_SELECT)
				.toBoolean();
		}

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
			grid = new WTreeGrid<>("grid", gridOptions, this)
				.setAssertParentNotFound(ConfigUtil.getBoolean(MetisConfigKey.GridAssertInvalidParent));
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
			.setSingleSelect(!multiSelect)
			.setIdField(keyField != null ? keyField.getName() : null)
			.setTitleField(titleField != null ? titleField.getName() : null)
			.setPageList(Arrays.asList(100, 200, 500, 1000))
			.addToolbarButton(new OAjaxLinkButton<Map<String, Object>>(((FontAwesome) MetisIcon.EXPORT_EXCEL).setColor("green")) {
				private static final long serialVersionUID = 3303989238841000829L;

				@Override
				public void onClick(AjaxRequestTarget target) {
					DataViewQVO dataViewQVO = new DataViewQVO();
					dataViewQVO
						.setName(DataViewGridPanel.this.dataVO.getName())
						//TODO .setSortFieldList(getSortFieldsMap(sortFields))
						.setFilter(getFilterMap())
						.setSortFieldList(sortFieldsMap)
						.setPageSize(getPageSize())
						.setPageIndex(getPageNum())
						.setExportType(DataViewQVO.ExportType.Excel)
					;

					dataService.executeDTask(dataViewQVO, taskBehavior);

					WMessager.show("Info", getString("msg.file.under.construction"), target);
				}
			})
			.addToolbarButton(new OAjaxLinkButton<Map<String, Object>>(MetisIcon.PRINT) {
				private static final long serialVersionUID = 3303989238841000829L;

				@Override
				public void onClick(AjaxRequestTarget target) {
					DataViewQVO dataViewQVO = new DataViewQVO();
					dataViewQVO
						.setName(DataViewGridPanel.this.dataVO.getName())
						//TODO .setSortFieldList(getSortFieldsMap(sortFields))
						.setFilter(getFilterMap())
						.setSortFieldList(sortFieldsMap)
						.setPageSize(getPageSize())
						.setPageIndex(getPageNum())
						.setExportType(DataViewQVO.ExportType.Print)
					;

					dataService.executeDTask(dataViewQVO, taskBehavior);

					//WMessager.show("Info", getString("msg.file.under.construction"), target);
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
							.setAddVisible(false)
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
			.addToolbarButton(new OAjaxLinkButton<Map<String, Object>>(MetisIcon.INFO) {
				private static final long serialVersionUID = 8420976618508397333L;

				@Override
				public void onClick(AjaxRequestTarget target) {
					modalWindow.setContent(new SearchDebugPanel(
						modalWindow.getContentId(),
						queryExecInfoList,
						dataVO.getName(),
						securityService.getCurrentUser().getUsername(),
						execTime));

					modalWindow.show(target);
				}
			})
			.setReturnField(returnField)
			.setAsyncLoadingEnabled(ConfigUtil.getBoolean(MetisConfigKey.GridAsyncLoadingShow))
			.setFit(true)
		;

		if (ConfigUtil.getBoolean(MetisConfigKey.GridNoResultShow)) {
			oBaseGrid.setNoResultMessage(getString("err.mts.NoResult"));
		}

		if (dataVO.getRowStyler() != null) {
			// #TIP
			oBaseGrid.setRowStyler((IStyler<Map<String, Object>> & Serializable) (bean, id) -> {
				IStringTemplate template = stringTemplateService.create(dataVO.getRowStyler(), TemplateEngineType.GroovyScript);
				Map<String, Object> params = new HashMap<>();
				params.put("row", bean);
				Object result = template.process(params);
				return result != null ? OStyle.style(result.toString()) : null;
			});
		}

		WebMarkupContainer sendButtons = new WebMarkupContainer("sendButtons");
		sendButtons.setVisible(false);

		final String returnVer;
		if (webParams.containsKey(MetisWebParam.RETURN_VERSION)) {
			returnVer = webParams.get(MetisWebParam.RETURN_VERSION).get(0);
		} else {
			returnVer = ConfigUtil.getString(MetisConfigKey.GridReturnResultVersion);
		}

		if (selectionJSCallback == null && webParams.containsKey(MetisWebParam.WINDOW)) {
			if ("1".equals(returnVer)) { // LIST (just rows)
				if (dataVO.getSelectionValidationJS() == null) {
					selectionJSCallback = "function(rows){parent.postMessage(JSON.stringify(rows),'*');}";
				} else {
					selectionJSCallback = String.format(
						"function(rows){if(%1$sSelValidJSAll(rows)) parent.postMessage(JSON.stringify(rows),'*');}",
						dataVO.getName());
				}

			} else if ("2".equals(returnVer)) { // OBJECT with ACTION
				if (dataVO.getSelectionValidationJS() == null) {
					selectionJSCallback = "function(rows){parent.postMessage(JSON.stringify({\"action\":\"_DEFAULT_\",\"data\":rows}),'*');}";
				} else {
					selectionJSCallback = String.format(
						"function(rows){if(%1$sSelValidJSAll(rows)) parent.postMessage(JSON.stringify({\"action\":\"_DEFAULT_\",\"data\":rows}),'*');}",
						dataVO.getName());
				}
			} else {
				throw new RuntimeException("Invalid return version: " + returnVer);
			}
		}

		if (selectionJSCallback != null || webParams.containsKey(MetisWebParam.WINDOW)) {
			oBaseGrid.setSelectionJSHandler(selectionJSCallback);

			List<ActionBut> actions = new ArrayList<>();
			if (webParams.containsKey(MetisWebParam.ACTIONS)) {
				String sentActions = webParams.get(MetisWebParam.ACTIONS).get(0);
				List<ActionBut> list = WebUtil.fromJson(sentActions, new TypeReference<List<ActionBut>>() {
				});
				actions.addAll(list);
			} else {
				actions.add(new ActionBut("_DEFAULT_", getString("label.send"), null));
			}

			sendButtons.setVisible(true);
			sendButtons.add(new ListView<ActionBut>("buttons", actions) {
				private static final long serialVersionUID = 8248347500988483292L;

				@Override
				protected void populateItem(ListItem<ActionBut> item) {
					ActionBut action = item.getModelObject();
					WebMarkupContainer button = new WebMarkupContainer("button");
					button.add(new AttributeModifier("onclick", String.format("sendRows('%s','%s', '%s', '%s', %s)",
						action.getName(), grid.getMarkupId(), dataVO.getName(), returnVer, selectionJSCallback)));
					button.add(new Label("label", action.getTitle()));
					item.add(button);
				}
			});
		}

		layout = new WebMarkupContainer("layout");
		layout.setOutputMarkupId(true);
		add(layout);
		layout.add(sendButtons);
		layout.add(grid);
		grid
			.setAssertDuplicateKey(ConfigUtil.getBoolean(MetisConfigKey.GridAssertDuplicateId))
			.setEnabled(false);

		taskBehavior = new DTaskBehavior<>(this);
		add(taskBehavior);
	}

	@Override
	protected void onAfterRender() {
		super.onAfterRender();

		//TODO using WEasyLayout
		WebUtil.writeJQueryCall(String.format("$('#%s').layout();", layout.getMarkupId()), true);
	}

	// ------------------------------

	private OColumnList<Map<String, Object>> createColumns(final DataVO dataVO) {
		List<String> disabledSortColumns = webParams.getOrDefault(MetisWebParam.DISABLE_SORT_COLUMN, Collections.emptyList());

		boolean disableAllSorts = false;
		if (webParams.containsKey(MetisWebParam.DISABLE_SORT_ALL_COLUMN)) {
			disableAllSorts = "1".equals(webParams.get(MetisWebParam.DISABLE_SORT_ALL_COLUMN).get(0));
		}

		OColumnList<Map<String, Object>> columns = new OColumnList<>();

		String checkboxColumnMode = ConfigUtil.getString(MetisConfigKey.GridCheckboxColumnMode);
		if ("Always".equals(checkboxColumnMode) || (multiSelect && "MultiSelect".equals(checkboxColumnMode))) {
			columns.add(new OCheckboxColumn<>());
		}

		for (DataFieldVO fieldVO : dataVO.getFields()) {
			OColumn<Map<String, Object>> column;
			if (XDSFieldResultType.Shown.equals(fieldVO.getResultType())) {
				// NOTE: For OPropertyColumn the "property" is set by fieldVO.getName(), but when there are both
				// NOTE: formatted and unformatted output, "field" is set by FIELD_PREFIX + fieldVO.getName()
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
								.setField(FIELD_PREFIX + fieldVO.getName())
								.setFormatter(ONumberFormatter.of(fieldVO.getFormatSafely()))
								.setStyle("direction:ltr");

							columns.add(new OHiddenColumn<>(fieldVO.getName()));
							break;

						case Real:
							column
								.setField(FIELD_PREFIX + fieldVO.getName())
								.setFormatter(ONumberFormatter.of(fieldVO.getFormatSafely()))
								.setStyle("direction:ltr");

							columns.add(new OHiddenColumn<>(fieldVO.getName()));
							break;

						case Date:
							column
								.setField(FIELD_PREFIX + fieldVO.getName())
								.setFormatter(ODateFormatter.of(fieldVO.getFormatSafely()))
								.setStyle("direction:ltr");

							columns.add(
								new OHiddenColumn<Map<String, Object>>(fieldVO.getName())
									.setFormatter(ODateFormatter.millis()));
							break;

						case DateTime:
							column
								.setField(FIELD_PREFIX + fieldVO.getName())
								.setFormatter(ODateFormatter.of(fieldVO.getFormatSafely()))
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
				switch (fieldVO.getType()) {
					case Date:
					case DateTime:
						column.setFormatter(ODateFormatter.millis());
						break;
				}
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

	// ------------------------------

	public static class ActionBut implements Serializable {
		private static final long serialVersionUID = 5208870063013803094L;

		private String name;
		private String title;
		private String icon;

		// ---------------

		public ActionBut() {
		}

		public ActionBut(String name, String title, String icon) {
			this.name = name;
			this.title = title;
			this.icon = icon;
		}

		// ---------------

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public String getTitle() {
			return title;
		}

		public void setTitle(String title) {
			this.title = title;
		}

		public String getIcon() {
			return icon;
		}

		public void setIcon(String icon) {
			this.icon = icon;
		}

		@Override
		public String toString() {
			return "ActionBut{" +
				"name='" + name + '\'' +
				", title='" + title + '\'' +
				", icon='" + icon + '\'' +
				'}';
		}
	}
}
