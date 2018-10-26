package org.devocative.metis.service;

import org.devocative.adroit.ConfigUtil;
import org.devocative.adroit.ObjectUtil;
import org.devocative.adroit.date.TimeFieldVO;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.demeter.DLogCtx;
import org.devocative.demeter.entity.FileStore;
import org.devocative.demeter.iservice.IFileStoreService;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.iservice.task.ITaskResultCallback;
import org.devocative.demeter.iservice.task.ITaskService;
import org.devocative.demeter.iservice.template.IStringTemplate;
import org.devocative.demeter.iservice.template.IStringTemplateService;
import org.devocative.demeter.iservice.template.TemplateEngineType;
import org.devocative.demeter.vo.UserVO;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.*;
import org.devocative.metis.iservice.IDataEventHandler;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.iservice.data.IDataSourceService;
import org.devocative.metis.iservice.data.IDataViewService;
import org.devocative.metis.service.task.ExecuteDataViewDTask;
import org.devocative.metis.vo.*;
import org.devocative.metis.vo.async.DataViewQVO;
import org.devocative.metis.vo.async.DataViewRVO;
import org.devocative.metis.vo.query.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Service("mtsDataService")
public class DataService implements IDataService {
	private static final Logger logger = LoggerFactory.getLogger(DataService.class);

	private static final String DATE_PATTERN = "yyyyMMdd";
	private static final String DATE_TIME_PATTERN = "yyyyMMddHHmmss";
	private static final Pattern DEFAULT_TIME = Pattern.compile("(\\d\\d):(\\d\\d):(\\d\\d).(\\d\\d\\d)");

	private List<IDataEventHandler> handlers = Collections.synchronizedList(new ArrayList<>());

	@Autowired
	private IDBConnectionService dbConnectionService;

	@Autowired
	private IDataSourceService dataSourceService;

	@Autowired
	private IDataViewService dataViewService;

	@Autowired
	private ISecurityService securityService;

	@Autowired
	private IFileStoreService fileStoreService;

	@Autowired
	private IStringTemplateService stringTemplateService;

	@Autowired
	private ITaskService taskService;

	// ------------------------------ PUBLIC METHODS

	@Override
	public DataVO loadDataVO(String dataViewId) {
		DataView dataView = dataViewService.load(dataViewId);

		return loadDataVOByDataView(dataView);
	}

	@Override
	public DataVO loadDataVOByName(String dataViewName) {
		DataView dataView = dataViewService.loadByName(dataViewName);

		return loadDataVOByDataView(dataView);
	}

	@Override
	public DataVO createAnotherDataView(String dsName) {
		DataVO result = loadDataVOByName(dsName);
		if (result != null) {
			result.setDataViewId(null);
			result.setName(null);
			result.setTitle(null);
			result.setDataSourceEditable(false);
		}
		return result;
	}

	@Override
	public List<DataAbstractFieldVO> findFilteringFields(List<DataAbstractFieldVO> allFields) {
		List<DataAbstractFieldVO> result = new ArrayList<>();

		int idx = 0;
		for (DataAbstractFieldVO dataFieldVO : allFields) {
			if (dataFieldVO.getInFilterPanelSafely()) {
				if (dataFieldVO.getFilterPanelOrder() == null) {
					dataFieldVO.setFilterPanelOrder(idx++);
				}
				result.add(dataFieldVO);
			}
		}

		Collections.sort(result);

		return result;
	}

	@Override
	public List<DataAbstractFieldVO> findLookUpFields(DataVO dataVO) {
		List<DataAbstractFieldVO> result = new ArrayList<>();

		for (DataParameterVO parameterVO : dataVO.getParams()) {
			if (parameterVO.getType() == XDSFieldType.LookUp) {

				if (parameterVO.getTargetDSMultipleSelection() == null) {
					parameterVO.setTargetDSMultipleSelection(true);
				}

				result.add(parameterVO);
			}
		}

		for (DataFieldVO fieldVO : dataVO.getFields()) {
			if (fieldVO.getType() == XDSFieldType.LookUp) {

				if (fieldVO.getTargetDSMultipleSelection() == null) {
					fieldVO.setTargetDSMultipleSelection(true);
				}

				result.add(fieldVO);
			}
		}

		return result;
	}

	@Override
	public void updateParamsByQuery(List<DataParameterVO> currentParams, String... queries) {
		List<DataParameterVO> temp = new ArrayList<>();

		for (String query : queries) {
			if (query != null) {
				List<String> paramsInQuery = NamedParameterStatement.findParamsInQuery(query, true);

				for (String param : paramsInQuery) {
					DataParameterVO parameterVO = new DataParameterVO();
					parameterVO.setName(param);

					int idx = currentParams.indexOf(parameterVO);
					if (idx < 0) {
						temp.add(parameterVO);
					} else {
						temp.add(currentParams.get(idx));
					}
				}
			}
		}

		currentParams.clear();
		currentParams.addAll(temp);
	}

	@Override
	public void updateFieldsByQuery(DataVO dataVO) {
		List<DataFieldVO> temp = new ArrayList<>();
		List<DataFieldVO> fieldsFromDB;
		Map<String, Object> params = new HashMap<>();
		for (DataParameterVO paramVO : dataVO.getParams()) {
			params.put(paramVO.getName(), paramVO.getSampleData());
		}

		try {
			// first try to process the query without dynamic processing if enabled in the query
			String sql = dataSourceService.processQuery(
				dataVO.getConnectionId(),
				dataVO.getQuery().getText(),
				dataVO.getQuery().getMode()
			);

			fieldsFromDB = dbConnectionService.findFields(
				dataVO.getConnectionId(),
				sql,
				params);
		} catch (RuntimeException e) {
			if (ObjectUtil.isTrue(dataVO.getQuery().getDynamic())) {
				// second try to process the query with dynamic processing if enabled in the query
				String sql = dataSourceService.processQuery(
					dataVO.getConnectionId(),
					dataSourceService.processDynamicQuery(dataVO.getQuery().getText(), params),
					dataVO.getQuery().getMode()
				);

				fieldsFromDB = dbConnectionService.findFields(
					dataVO.getConnectionId(),
					sql,
					params);
			} else {
				throw e;
			}
		}

		List<String> nameClash = new ArrayList<>();
		for (DataParameterVO paramVO : dataVO.getParams()) {
			if (fieldsFromDB.contains(paramVO)) {
				nameClash.add(paramVO.getName());
			}
		}
		if (nameClash.size() > 0) {
			throw new MetisException(MetisErrorCode.ParameterFieldNameClash, nameClash.toString());
		}

		for (DataFieldVO fieldFromDB : fieldsFromDB) {
			int i = dataVO.getFields().indexOf(fieldFromDB);
			if (i > -1) {
				DataFieldVO currentField = dataVO.getFields().get(i);
				currentField.setDbType(fieldFromDB.getDbType());
				temp.add(currentField);
			} else {
				temp.add(fieldFromDB);
			}
		}

		dataVO.getFields().clear();
		dataVO.getFields().addAll(temp);
	}

	@Transactional
	@Override
	public void saveOrUpdate(DataVO dataVO) {
		if (dataVO.isDataSourceEditable()) {
			/*XDataSource xDataSource = dataVO.toXDataSource();
			DataSource dataSource = dataSourceService.saveOrUpdate(
				dataVO.getDataSourceId(),
				dataVO.getConnectionId(),
				dataVO.getTitle(),
				xDataSource);*/
			DataSource dataSource = dataSourceService.saveOrUpdate(dataVO);
			dataVO.setDataSourceId(dataSource.getId());
			dataVO.setDataSourceName(dataSource.getName());
		}

		XDataView xDataView = dataVO.toXDataView();
		xDataView.setDataSourceId(dataVO.getDataSourceId());

		dataViewService.saveOrUpdate(dataVO.getDataViewId(), dataVO.getTitle(), xDataView, dataVO.getGroups());

		for (IDataEventHandler handler : handlers) {
			handler.handleDataVoSaved(dataVO);
		}
	}

	// ---------------

	@Override
	public void executeDTask(DataViewQVO qvo, ITaskResultCallback callback) {
		taskService.start(ExecuteDataViewDTask.class, qvo, callback);
	}

	@Override
	public DataViewRVO executeDataView(DataViewQVO request) {
		DLogCtx
			.put("action", "execute")
			.put("dataView", request.getName())
		;

		logger.info("Executing DataView: DV=[{}] Usr=[{}]",
			request.getName(), securityService.getCurrentUser());
		long start = System.currentTimeMillis();

		DataView dataView = dataViewService.loadByName(request.getName());
		XDataView xDataView = dataView.getXDataView();

		List<String> selectFields = getSelectedFields(xDataView, true);

		DataViewRVO result = new DataViewRVO();


		// --------------- SELECT

		SelectQueryQVO selectQVO = new SelectQueryQVO(xDataView.getDataSourceId(), selectFields);
		selectQVO
			.setPagination(request.getPagination())
			.setSortFields(request.getSortFieldList())
			.setInputParams(request.getFilter())
		;
		DsQueryRVO<List<Map<String, Object>>> listRVO = dataSourceService.execute(selectQVO);
		result.setList(listRVO.getResult());
		result.addQueryExecInfo(listRVO.getQueryExecInfoList());

		// --------------- COUNT

		CountQueryQVO countQVO = new CountQueryQVO(xDataView.getDataSourceId());
		countQVO
			.setInputParams(request.getFilter())
		;
		DsQueryRVO<Long> countRVO = dataSourceService.execute(countQVO);
		result.setCount(countRVO.getResult());
		result.addQueryExecInfo(countRVO.getQueryExecInfoList());

		// --------------- FOOTER (AGG)

		Map<String, List<XDVAggregatorFunction>> agrFields = new HashMap<>();
		xDataView
			.getFields()
			.stream()
			.filter(xdvField -> xdvField.getFooter() != null && !xdvField.getFooter().isEmpty())
			.forEach(xdvField -> agrFields.put(xdvField.getName(), xdvField.getFooter()));
		if (!agrFields.isEmpty()) {
			AggregateQueryQVO agrQVO = new AggregateQueryQVO(xDataView.getDataSourceId(), agrFields);
			agrQVO
				.setInputParams(request.getFilter())
			;
			DsQueryRVO<List<Map<String, Object>>> footerRVO = dataSourceService.execute(agrQVO);
			result.setFooter(footerRVO.getResult());
			result.addQueryExecInfo(footerRVO.getQueryExecInfoList());
		}

		// --------------- Check After of DataSource

		result.addQueryExecInfo(
			dataSourceService.executeAfterIfAny(xDataView.getDataSourceId())
		);

		// ---------------

		long dur = System.currentTimeMillis() - start;
		DLogCtx.put("duration", dur);
		logger.info("Executed DataView: DV=[{}] Usr=[{}] Dur=[{}] Res#=[{}] Cnt=[{}] Ftr=[{}]",
			xDataView.getName(), securityService.getCurrentUser(),
			dur, result.getList().size(), result.getCount(),
			result.getFooter() != null ? result.getFooter().size() : null);

		return result;
	}

	@Override
	public DataViewRVO exportDataView(DataViewQVO request) {
		DLogCtx
			.put("action", "export")
			.put("dataView", request.getName())
		;

		UserVO currentUser = securityService.getCurrentUser();

		logger.info("Exporting DataView: DV=[{}] Usr=[{}]", request.getName(), currentUser);

		long start = System.currentTimeMillis();

		DataView dataView = dataViewService.loadByName(request.getName());
		XDataView xDataView = dataView.getXDataView();

		DataSource dataSource = dataSourceService.load(xDataView.getDataSourceId());
		XDataSource xDataSource = dataSource.getXDataSource();

		List<String> titleFields = xDataView.getFields()
			.stream()
			.filter(xdvField -> XDSFieldResultType.Shown == xdvField.getResultType())
			.map(xdvField -> xDataSource.getField(xdvField.getName()).getSafeTitle())
			.collect(Collectors.toList());

		List<String> selectFields = getSelectedFields(xDataView, false);
		SelectQueryQVO selectQVO = new SelectQueryQVO(xDataView.getDataSourceId(), selectFields);
		selectQVO
			//.setPagination(PaginationQVO.byPage(1L, 1000L))
			.setSortFields(request.getSortFieldList())
			.setInputParams(request.getFilter())
		;

		if (ConfigUtil.getLong(MetisConfigKey.ExportExcelMaxSize) > 1) {
			selectQVO.setPagination(PaginationQVO.byPage(1L, ConfigUtil.getLong(MetisConfigKey.ExportExcelMaxSize)));
		}

		DsQueryRVO<List<Map<String, Object>>> listRVO = dataSourceService.execute(selectQVO);

		ReportBuilder.Format format;
		switch (request.getExportType()) {
			case Data:
				format = ReportBuilder.Format.Data;
				break;
			case Print:
				format = ReportBuilder.Format.Print;
				break;
			case Excel:
				format = ReportBuilder.Format.Excel;
				break;
			case PDF:
				format = ReportBuilder.Format.PDF;
				break;
			default:
				format = ReportBuilder.Format.Print;
		}

		ReportBuilder rBuilder = new ReportBuilder(fileStoreService, dataView.getName(), currentUser, format)
			.setTitle(dataView.getTitle());

		for (int i = 0; i < selectFields.size(); i++) {
			final String field = selectFields.get(i);
			final String title = titleFields.get(i);
			rBuilder.addColumn(new ReportBuilder.Column(field, title, String.class));
		}

		for (Map<String, Object> map : listRVO.getResult()) {
			for (String field : selectFields) {
				final XDSField xdsField = xDataSource.getField(field);
				final Object cell = map.get(field);
				switch (xdsField.getType()) {
					case Date:
						if (cell instanceof Date) {
							map.put(field, currentUser.formatDate((Date) cell));
						}
						break;
					case DateTime:
						if (cell instanceof Date) {
							map.put(field, currentUser.formatDateTime((Date) cell));
						}
						break;
					default:
						map.put(field, cell != null ? cell.toString() : "");
				}
			}
		}

		rBuilder.setRows(listRVO.getResult());
		FileStore fileStore = rBuilder.build();

		long dur = System.currentTimeMillis() - start;
		DLogCtx.put("duration", dur);
		logger.info("Exported DataView: DV=[{}] Usr=[{}] Dur=[{}]",
			xDataView.getName(), securityService.getCurrentUser(), dur);

		return new DataViewRVO()
			.setFileId(fileStore.getFileId())
			.setInline(fileStore.getMimeType().isInline());
	}

	@Override
	public DataViewRVO executeDataViewForParent(DataViewQVO request) {
		DLogCtx
			.put("action", "execOfParent")
			.put("dataView", request.getName())
		;

		logger.info("Executing DataView of Parent: DV=[{}] Prnt=[{}] Usr=[{}]",
			request.getName(), request.getParentId(), securityService.getCurrentUser());
		long start = System.currentTimeMillis();

		DataView dataView = dataViewService.loadByName(request.getName());
		XDataView xDataView = dataView.getXDataView();

		List<String> selectFields = getSelectedFields(xDataView, true);

		SelectQueryQVO selectQVO = new SelectQueryQVO(xDataView.getDataSourceId(), selectFields);
		selectQVO
			.setSortFields(request.getSortFieldList());

		List<Map<String, Object>> list = dataSourceService.executeOfParent(selectQVO, request.getParentId()).getResult();

		DataViewRVO result = new DataViewRVO();
		result.setList(list);
		result.setParentId(request.getParentId().toString());

		long dur = System.currentTimeMillis() - start;
		DLogCtx.put("duration", dur);
		logger.info("Executed DataView of Parent: DV=[{}] Prnt=[{}] Usr=[{}] Dur=[{}]",
			request.getName(), request.getParentId(), securityService.getCurrentUser(), dur);

		return result;
	}

	@Override
	public List<Map<String, Object>> executeOData(DataViewQVO request) {
		DLogCtx
			.put("action", "odata")
			.put("dataView", request.getName());

		logger.info("Executing OData: DV=[{}] Usr=[{}]", request.getName(), securityService.getCurrentUser());

		long start = System.currentTimeMillis();

		try {
			DataView dataView = dataViewService.loadByName(request.getName());
			XDataView xDataView = dataView.getXDataView();

			List<String> selectFields = getSelectedFields(xDataView, true);

			Map<String, Object> inputParams = new HashMap<>();
			if (request.getFilter() != null) {
				for (Map.Entry<String, Object> entry : request.getFilter().entrySet()) {
					inputParams.put(entry.getKey().toLowerCase(), entry.getValue());
				}
			}

			SelectQueryQVO selectQVO = new SelectQueryQVO(xDataView.getDataSourceId(), selectFields);
			selectQVO
				.setConsiderParent(ConfigUtil.getBoolean(MetisConfigKey.ODataConsiderParentRelation)) //NOTE: in OData just fetch related records
				.setPagination(request.getPagination())
				.setSortFields(request.getSortFieldList())
				.setFilterExpression(request.getFilterExpression())
				.setInputParams(inputParams)
			//TODO .setExtraParams(request.getExtraParams())
			;

			List<Map<String, Object>> list = dataSourceService.execute(selectQVO).getResult();

			long dur = System.currentTimeMillis() - start;
			DLogCtx.put("duration", dur);
			logger.info("Executed OData: DV=[{}] Usr=[{}] Dur=[{}]",
				request.getName(), securityService.getCurrentUser(), dur);

			return list;
		} catch (MetisException e) {
			logger.error("Execute OData Error: DV=" + request.getName(), e);
			if (e.getCause() != null) {
				throw new RuntimeException(e.getCause());
			} else {
				throw e;
			}
		} catch (RuntimeException e) {
			logger.error("Execute OData Error: DV=" + request.getName(), e);
			throw e;
		} finally {
			DLogCtx
				.remove("action")
				.remove("dataView")
				.remove("duration");
		}
	}

	@Override
	public Long executeODataCount(DataViewQVO request) {
		DLogCtx
			.put("action", "odataCount")
			.put("dataView", request.getName());

		logger.info("Executing OData Count: DV=[{}] Usr=[{}]", request.getName(), securityService.getCurrentUser());
		long start = System.currentTimeMillis();

		try {
			DataView dataView = dataViewService.loadByName(request.getName());
			XDataView xDataView = dataView.getXDataView();

			Map<String, Object> inputParams = new HashMap<>();
			if (request.getFilter() != null) {
				for (Map.Entry<String, Object> entry : request.getFilter().entrySet()) {
					inputParams.put(entry.getKey().toLowerCase(), entry.getValue());
				}
			}

			CountQueryQVO countQVO = new CountQueryQVO(xDataView.getDataSourceId());
			countQVO
				.setConsiderParent(ConfigUtil.getBoolean(MetisConfigKey.ODataConsiderParentRelation))
				.setFilterExpression(request.getFilterExpression())
				.setInputParams(inputParams)
			;

			Long result = dataSourceService.execute(countQVO).getResult();

			logger.info("Executed OData Count: DV=[{}] Usr=[{}] Dur=[{}]",
				request.getName(), securityService.getCurrentUser(), System.currentTimeMillis() - start);

			return result;
		} catch (RuntimeException e) {
			logger.error("Execute OData Error: DV=" + request.getName(), e);
			throw e;
		} finally {
			DLogCtx
				.remove("action")
				.remove("dataView")
				.remove("duration");
		}
	}

	// ---------------

	@Override
	public void processDynamicFilterAndParam(String script, Map<String, ?> params, Map<String, ?> prevParams, RowInputVO rowInputVO) {
		Map<String, Object> bindings = new HashMap<>();
		bindings.put("filter", params);
		bindings.put("params", params);

		if (rowInputVO != null) {
			if (rowInputVO.getRow() != null) {
				bindings.put("row", rowInputVO.getRow());
			} else if (rowInputVO.getRowsKeys() != null && !rowInputVO.getRowsKeys().isEmpty()) {
				bindings.put("rows", rowInputVO);
			} else {
				throw new MetisException(MetisErrorCode.NoRowSelected);
			}
		}

		if (prevParams != null) {
			bindings.put("prevParams", prevParams);
		}

		bindings.put("user", securityService.getCurrentUser());

		StringBuilder finalScript = new StringBuilder();
		finalScript
			.append("def range(l,u){new org.devocative.adroit.vo.RangeVO(l,u)}\n")
			.append("def now(){new Date()}\n")
			.append("def list(Object... p){def list=[]; p.each{list.add(it)}; return list}\n")
			.append(script);

		IStringTemplate stringTemplate = stringTemplateService
			.create(finalScript.toString(), TemplateEngineType.GroovyScript);

		stringTemplate.process(bindings);
	}

	@Override
	public void addDataEventHandler(IDataEventHandler handler) {
		handlers.add(handler);
	}

	@Override
	public TimeFieldVO extractTimeFields(MetisConfigKey configKey) {
		String text = ConfigUtil.getString(configKey);
		final Matcher matcher = DEFAULT_TIME.matcher(text);
		if (matcher.find()) {
			return new TimeFieldVO(
				Integer.parseInt(matcher.group(1)),
				Integer.parseInt(matcher.group(2)),
				Integer.parseInt(matcher.group(3)),
				Integer.parseInt(matcher.group(4)));
		}

		throw new RuntimeException(String.format("Invalid format for %s: %s", configKey.getKey(), text));
	}

	// ------------------------------

	private DataVO loadDataVOByDataView(DataView dataView) {
		logger.info("Loading DataView data: DV=[{}] Usr=[{}]", dataView.getName(), securityService.getCurrentUser());

		DataVO result = new DataVO();

		XDataView xDataView = dataView.getXDataView();

		if (dataView.getDataSourceId() == null || !dataView.getDataSourceId().equals(xDataView.getDataSourceId())) {
			throw new MetisException(MetisErrorCode.InvalidDataViewState);
		}

		result.setTitle(dataView.getTitle());
		result.setGroups(dataView.getGroups());

		updateDataVOByDataSource(result, dataView.getDataSourceId());
		result.setDataViewId(dataView.getId());

		result.fromXDataView(xDataView);

		result.setDataSourceEditable(
			result.getName() != null && result.getName().equals(result.getDataSourceName())
		);
		return result;
	}

	private void updateDataVOByDataSource(DataVO dataVO, String dsId) {
		DataSource dataSource = dataSourceService.load(dsId);
		XDataSource xDataSource = dataSource.getXDataSource();

		dataVO.setDataSourceId(dataSource.getId());
		dataVO.setConnectionId(dataSource.getConnection().getId());
		dataVO.setConnectionSelection(dataSource.getConnectionSelection());
		dataVO.setConnectionHasMapping(dataSource.getConnection().getSafeConfigId() != null);

		dataVO.fromXDataSource(xDataSource);
	}

	private List<String> getSelectedFields(XDataView xDataView, boolean includeHidden) {
		List<String> selectFields = new ArrayList<>();
		for (XDVField xdvField : xDataView.getFields()) {
			if (xdvField.getResultType() != null) {
				if (xdvField.getResultType() == XDSFieldResultType.Shown ||
					(includeHidden && xdvField.getResultType() == XDSFieldResultType.Hidden)) {
					selectFields.add(xdvField.getName());
				}
			}
		}
		return selectFields;
	}
}
