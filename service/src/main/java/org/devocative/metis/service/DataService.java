package org.devocative.metis.service;

import org.devocative.adroit.*;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.adroit.vo.RangeVO;
import org.devocative.demeter.DLogCtx;
import org.devocative.demeter.entity.EFileStorage;
import org.devocative.demeter.entity.EMimeType;
import org.devocative.demeter.iservice.FileStoreHandler;
import org.devocative.demeter.iservice.IFileStoreService;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.demeter.iservice.template.IStringTemplate;
import org.devocative.demeter.iservice.template.IStringTemplateService;
import org.devocative.demeter.iservice.template.TemplateEngineType;
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
import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.async.DataViewQVO;
import org.devocative.metis.vo.async.DataViewRVO;
import org.devocative.metis.vo.query.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

@Service("mtsDataService")
public class DataService implements IDataService {
	private static final Logger logger = LoggerFactory.getLogger(DataService.class);

	private static final String DATE_PATTERN = "yyyyMMdd";
	private static final String DATE_TIME_PATTERN = "yyyyMMddHHmmss";

	private List<IDataEventHandler> handlers = Collections.synchronizedList(new ArrayList<>());

	@Autowired
	private IPersistorService persistorService;

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

	// ------------------------------ PUBLIC METHODS

	@Override
	public DataVO loadDataVO(Long dataViewId) {
		DataView dataView = dataViewService.load(dataViewId);

		return loadDataVOByDataView(dataView);
	}

	@Override
	public DataVO loadDataVO(String dataViewName) {
		DataView dataView = dataViewService.loadByName(dataViewName);

		return loadDataVOByDataView(dataView);
	}

	@Override
	public DataVO createAnotherDataView(String dsName) {
		DataVO result = loadDataVO(dsName);
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

	@Override
	public void saveOrUpdate(DataVO dataVO) {
		if (dataVO.isDataSourceEditable()) {
			XDataSource xDataSource = dataVO.toXDataSource();
			DataSource dataSource = dataSourceService.saveOrUpdate(
				dataVO.getDataSourceId(),
				dataVO.getConnectionId(),
				dataVO.getTitle(),
				xDataSource);
			dataVO.setDataSourceId(dataSource.getId());
			dataVO.setDataSourceName(dataSource.getName());
		}

		XDataView xDataView = dataVO.toXDataView();
		xDataView.setDataSourceId(dataVO.getDataSourceId());

		dataViewService.saveOrUpdate(dataVO.getDataViewId(), dataVO.getTitle(), xDataView, dataVO.getGroups());

		persistorService.commitOrRollback();

		for (IDataEventHandler handler : handlers) {
			handler.handleDataVoSaved(dataVO);
		}
	}

	// ---------------

	@Override
	public DataViewRVO executeDataView(DataViewQVO request) {
		DLogCtx
			.put("action", "execute")
			.put("dataView", request.getName())
			.put("sentDB", request.getSentDBConnection());

		logger.info("Executing DataView: DV=[{}] Usr=[{}] SentDB=[{}]",
			request.getName(), securityService.getCurrentUser(), request.getSentDBConnection());
		long start = System.currentTimeMillis();

		DataView dataView = dataViewService.loadByName(request.getName());
		XDataView xDataView = dataView.getXDataView();

		List<String> selectFields = getSelectedFields(xDataView, true);

		DataViewRVO result = new DataViewRVO();


		// --------------- SELECT

		SelectQueryQVO selectQVO = new SelectQueryQVO(xDataView.getDataSourceId(), selectFields);
		selectQVO
			.setPagination(PaginationQVO.byPage(request.getPageIndex(), request.getPageSize()))
			.setSortFields(request.getSortFieldList())
			.setInputParams(request.getFilter())
			.setSentDBConnection(request.getSentDBConnection());
		DsQueryRVO<List<Map<String, Object>>> listRVO = dataSourceService.execute(selectQVO);
		result.setList(listRVO.getResult());
		result.addQueryExecInfo(listRVO.getQueryExecInfoList());

		// --------------- COUNT

		CountQueryQVO countQVO = new CountQueryQVO(xDataView.getDataSourceId());
		countQVO
			.setInputParams(request.getFilter())
			.setSentDBConnection(request.getSentDBConnection());
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
				.setSentDBConnection(request.getSentDBConnection());
			DsQueryRVO<List<Map<String, Object>>> footerRVO = dataSourceService.execute(agrQVO);
			result.setFooter(footerRVO.getResult());
			result.addQueryExecInfo(footerRVO.getQueryExecInfoList());
		}

		// --------------- Check After of DataSource

		result.addQueryExecInfo(
			dataSourceService.executeAfterIfAny(xDataView.getDataSourceId(), request.getSentDBConnection())
		);

		// ---------------

		long dur = System.currentTimeMillis() - start;
		DLogCtx.put("duration", dur);
		logger.info("Executed DataView: DV=[{}] Usr=[{}] SentDB=[{}] Dur=[{}] Res#=[{}] Cnt=[{}] Ftr=[{}]",
			xDataView.getName(), securityService.getCurrentUser(), request.getSentDBConnection(),
			dur, result.getList().size(), result.getCount(),
			result.getFooter() != null ? result.getFooter().size() : null);

		return result;
	}

	@Override
	public DataViewRVO exportDataView(DataViewQVO request) {
		DLogCtx
			.put("action", "export")
			.put("dataView", request.getName())
			.put("sentDB", request.getSentDBConnection());

		logger.info("Exporting DataView: DV=[{}] Usr=[{}] SentDB=[{}]",
			request.getName(), securityService.getCurrentUser(), request.getSentDBConnection());

		long start = System.currentTimeMillis();

		DataView dataView = dataViewService.loadByName(request.getName());
		XDataView xDataView = dataView.getXDataView();

		DataSource dataSource = dataSourceService.load(xDataView.getDataSourceId());
		XDataSource xDataSource = dataSource.getXDataSource();

		List<String> titleFields = xDataView.getFields()
			.stream()
			.filter(xdvField -> XDSFieldResultType.Shown.equals(xdvField.getResultType()))
			.map(xdvField -> xDataSource.getField(xdvField.getName()).getSafeTitle())
			.collect(Collectors.toList());

		List<String> selectFields = getSelectedFields(xDataView, false);
		SelectQueryQVO selectQVO = new SelectQueryQVO(xDataView.getDataSourceId(), selectFields);
		selectQVO
			.setPagination(PaginationQVO.byPage(1L, 1000L))
			.setSortFields(request.getSortFieldList())
			.setInputParams(request.getFilter())
			.setSentDBConnection(request.getSentDBConnection());

		DsQueryRVO<List<Map<String, Object>>> listRVO = dataSourceService.execute(selectQVO);

		ExcelExporter exporter = new ExcelExporter(dataView.getTitle());
		exporter.setColumnsHeader(titleFields);

		for (Map<String, Object> map : listRVO.getResult()) {
			List<Object> row = selectFields
				.stream()
				.map(map::get)
				.collect(Collectors.toList());
			exporter.addRowData(row);
		}

		Date now = new Date();
		Date expire = CalendarUtil.add(now, Calendar.DATE,
			ConfigUtil.getInteger(MetisConfigKey.ExportReportExpireDays));
		//TODO using calendar from User
		String name = String.format("%s-%s.xlsx", dataView.getName(), CalendarUtil.toPersian(now, "yyyyMMdd-HHmmss"));
		FileStoreHandler fileStoreHandler = fileStoreService.create(
			name,
			EFileStorage.DISK,
			EMimeType.EXCEL,
			expire,
			dataView.getName());

		try {
			exporter.generate(fileStoreHandler);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}

		long dur = System.currentTimeMillis() - start;
		DLogCtx.put("duration", dur);
		logger.info("Exported DataView: DV=[{}] Usr=[{}] SentDB=[{}] Dur=[{}]",
			xDataView.getName(), securityService.getCurrentUser(), request.getSentDBConnection(), dur);

		return new DataViewRVO().setFileId(fileStoreHandler.getFileStore().getFileId());
	}

	@Override
	public DataViewRVO executeDataViewForParent(DataViewQVO request) {
		DLogCtx
			.put("action", "execOfParent")
			.put("dataView", request.getName())
			.put("sentDB", request.getSentDBConnection());

		logger.info("Executing DataView of Parent: DV=[{}] Prnt=[{}] Usr=[{}]",
			request.getName(), request.getParentId(), securityService.getCurrentUser());
		long start = System.currentTimeMillis();

		DataView dataView = dataViewService.loadByName(request.getName());
		XDataView xDataView = dataView.getXDataView();

		List<String> selectFields = getSelectedFields(xDataView, true);

		SelectQueryQVO selectQVO = new SelectQueryQVO(xDataView.getDataSourceId(), selectFields);
		selectQVO
			.setSortFields(request.getSortFieldList())
			.setSentDBConnection(request.getSentDBConnection());

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
	public List<Map<String, Object>> executeOData(ODataQVO request) {
		DLogCtx
			.put("action", "odata")
			.put("dataView", request.getName());

		logger.info("Executing OData: DV=[{}] Usr=[{}]", request.getName(), securityService.getCurrentUser());

		long start = System.currentTimeMillis();

		try {
			DataView dataView = dataViewService.loadByName(request.getName());
			XDataView xDataView = dataView.getXDataView();

			List<String> selectFields = getSelectedFields(xDataView, false);

			Map<String, Object> inputParams = new HashMap<>();
			if (request.getInputParams() != null) {
				for (Map.Entry<String, Object> entry : request.getInputParams().entrySet()) {
					inputParams.put(entry.getKey().toLowerCase(), entry.getValue());
				}
			}

			SelectQueryQVO selectQVO = new SelectQueryQVO(xDataView.getDataSourceId(), selectFields);
			selectQVO
				.setPagination(PaginationQVO.byResult(request.getFirstResult(), request.getMaxResults()))
				.setSortFields(request.getOrderBy())
				.setFilterExpression(request.getFilterExpression())
				.setInputParams(inputParams);

			List<Map<String, Object>> list = dataSourceService.execute(selectQVO).getResult();

			long dur = System.currentTimeMillis() - start;
			DLogCtx.put("duration", dur);
			logger.info("Executed OData: DV=[{}] Usr=[{}] Dur=[{}]",
				request.getName(), securityService.getCurrentUser(), dur);

			return list;
		} catch (MetisException e) {
			logger.error("Execute OData Error: DV=" + request.getName(), e);
			throw new RuntimeException(e.getCause());
		} catch (RuntimeException e) {
			logger.error("Execute OData Error: DV=" + request.getName(), e);
			throw e;
		}
	}

	@Override
	public Long executeODataCount(ODataQVO request) {
		DLogCtx
			.put("action", "odataCount")
			.put("dataView", request.getName());

		logger.info("Executing OData Count: DV=[{}] Usr=[{}]", request.getName(), securityService.getCurrentUser());
		long start = System.currentTimeMillis();

		try {
			DataView dataView = dataViewService.loadByName(request.getName());
			XDataView xDataView = dataView.getXDataView();

			Map<String, Object> inputParams = new HashMap<>();
			if (request.getInputParams() != null) {
				for (Map.Entry<String, Object> entry : request.getInputParams().entrySet()) {
					inputParams.put(entry.getKey().toLowerCase(), entry.getValue());
				}
			}

			CountQueryQVO countQVO = new CountQueryQVO(xDataView.getDataSourceId());
			countQVO
				.setFilterExpression(request.getFilterExpression())
				.setInputParams(inputParams);

			Long result = dataSourceService.execute(countQVO).getResult();

			logger.info("Executed OData Count: DV=[{}] Usr=[{}] Dur=[{}]",
				request.getName(), securityService.getCurrentUser(), System.currentTimeMillis() - start);

			return result;
		} catch (RuntimeException e) {
			logger.error("Execute OData Error: DV=" + request.getName(), e);
			throw e;
		}
	}

	// ---------------

	@Override
	public Set<String> convertSimpleParamsToFilter(
		Map<String, Object> result,
		Long dataSourceId,
		List<DataAbstractFieldVO> fields,
		Map<String, List<String>> params,
		String sentDBConnection) {

		Set<String> finalWebParams = new HashSet<>();

		for (DataAbstractFieldVO fieldVO : fields) {
			String fieldName = fieldVO.getName();

			// NOTE:
			// based on the workflow of filling the filter map in DataViewFilterPanel,
			// it is possible that convertFilterToFilter() is called before this method.
			// so if a key is set in result, further processing must be ignored!
			if (result.containsKey(fieldName)) {
				continue;
			}

			List<String> values = params.get(fieldName);

			try {
				if (values != null && values.size() > 0) {
					switch (fieldVO.getFilterType()) {
						case Equal:
							if (fieldVO.getInFilterPanelSafely()) {
								result.put(fieldName, convertQueryParam(fieldVO.getType(), values.get(0)));
							} else {
								result.put(fieldName, convertQueryParam(fieldVO.getType(), values));
							}
							break;

						case Contain:
							String paramValue = values.get(0);
							result.put(fieldName, convertQueryParam(fieldVO.getType(), paramValue));
							break;

						case List:
						case Search:
							DataSource targetDS = dataSourceService.load(fieldVO.getTargetDSId());
							XDataSource targetXDS = targetDS.getXDataSource();

							Map<String, Object> lookUpFilter = new HashMap<>();

							if (fieldVO.getTargetDSFilter() != null) {
								Map<String, Object> filterTargetDS = createMapOfFilterTargetDS(fieldVO.getTargetDSFilter(), targetXDS.getAllFields());
								lookUpFilter.putAll(filterTargetDS);
							}

							XDSField keyField = targetXDS.getField(targetDS.getKeyField());
							lookUpFilter.put(keyField.getName(), convertQueryParam(keyField.getType(), values));

							List<KeyValueVO<Serializable, String>> filtered = dataSourceService.executeLookUp(
								dataSourceId,
								fieldVO.getTargetDSId(),
								sentDBConnection,
								lookUpFilter
							).getResult();

							boolean multiple = fieldVO.getTargetDSMultipleSelection() == null || fieldVO.getTargetDSMultipleSelection();
							if (multiple) {
								result.put(fieldName, filtered);
							} else if (filtered.size() > 0) {
								result.put(fieldName, filtered.get(0));
							}
							break;
					}

					if (result.containsKey(fieldName)) {
						finalWebParams.add(fieldName);
					}
				} else if (params.containsKey(fieldName + "_u") || params.containsKey(fieldName + "_l")) {
					if (fieldVO.getFilterType().equals(XDSFieldFilterType.Range)) {
						Serializable lower = null;
						if (params.containsKey(fieldName + "_l")) {
							lower = convertQueryParam(fieldVO.getType(), params.get(fieldName + "_l").get(0));
						}

						Serializable upper = null;
						if (params.containsKey(fieldName + "_u")) {
							upper = convertQueryParam(fieldVO.getType(), params.get(fieldName + "_u").get(0));
						}

						RangeVO rangeVO = new RangeVO<>(lower, upper);
						result.put(fieldName, rangeVO);

						finalWebParams.add(fieldName);
					}
				} else if (fieldVO.getTargetDSFilter() != null && fieldVO.getFilterType() == XDSFieldFilterType.List) { //NOTE: before fieldVO.getType() == XDSFieldType.LookUp
					DataSource targetDS = dataSourceService.load(fieldVO.getTargetDSId());
					XDataSource targetXDS = targetDS.getXDataSource();
					Map<String, Object> filterTargetDS = createMapOfFilterTargetDS(fieldVO.getTargetDSFilter(), targetXDS.getAllFields());
					List<KeyValueVO<Serializable, String>> filtered = dataSourceService.executeLookUp(
						dataSourceId,
						fieldVO.getTargetDSId(),
						sentDBConnection,
						filterTargetDS
					).getResult();
					result.put(fieldName, filtered);
				}
			} catch (Exception e) {
				logger.warn("Converting URL parameter value={} to filter, field=[{}], dsId=[{}], error=[{}]",
					values, fieldName, dataSourceService.load(dataSourceId), e);
				throw new MetisException(MetisErrorCode.InvalidFilterValue, fieldName, e);
			}
		}

		return finalWebParams;
	}

	@Override
	public Map<String, Object> convertFilterToFilter(
		Long dataSourceId,
		List<DataAbstractFieldVO> fields,
		Map<String, Object> filter,
		String sentDBConnection) {

		Map<String, Object> result = new HashMap<>();

		for (DataAbstractFieldVO fieldVO : fields) {
			String fieldName = fieldVO.getName();
			Object value = filter.get(fieldName);

			try {
				if (value != null) {
					switch (fieldVO.getFilterType()) {
						case Equal:
							result.put(fieldName, convertFilterParam(fieldVO.getType(), value, true));
							break;

						case Contain:
							result.put(fieldName, convertFilterParam(fieldVO.getType(), value, false));
							break;

						case Range:
							if (value instanceof RangeVO) {
								RangeVO sentRange = (RangeVO) value;
								RangeVO<Object> newRange = new RangeVO<>(
									convertFilterParam(fieldVO.getType(), sentRange.getLower(), false),
									convertFilterParam(fieldVO.getType(), sentRange.getUpper(), false)
								);
								result.put(fieldName, newRange);
							}
							break;

						case List:
						case Search:
							DataSource targetDS = dataSourceService.load(fieldVO.getTargetDSId());
							XDataSource targetXDS = targetDS.getXDataSource();

							Map<String, Object> lookUpFilter = new HashMap<>();

							if (fieldVO.getTargetDSFilter() != null) {
								Map<String, Object> filterTargetDS = createMapOfFilterTargetDS(fieldVO.getTargetDSFilter(), targetXDS.getAllFields());
								lookUpFilter.putAll(filterTargetDS);
							}

							XDSField keyField = targetXDS.getField(targetDS.getKeyField());
							lookUpFilter.put(keyField.getName(), convertFilterParam(keyField.getType(), value, true));

							List<KeyValueVO<Serializable, String>> filtered = dataSourceService.executeLookUp(
								dataSourceId,
								fieldVO.getTargetDSId(),
								sentDBConnection,
								lookUpFilter
							).getResult();

							boolean multiple = fieldVO.getTargetDSMultipleSelection() == null || fieldVO.getTargetDSMultipleSelection();
							if (multiple) {
								result.put(fieldName, filtered);
							} else if (filtered.size() > 0) {
								result.put(fieldName, filtered.get(0));
							}
							break;
					}
				}
			} catch (Exception e) {
				logger.warn("Converting filter parameter value={} to filter, field=[{}], dsId=[{}], error=[{}]",
					value, fieldName, dataSourceService.load(dataSourceId), e);
				throw new MetisException(MetisErrorCode.InvalidFilterValue, fieldName, e);
			}
		}

		return result;
	}

	// ---------------

	@Override
	public void processDynamicFilterAndParam(String script, Map<String, ?> filter, Map<String, ?> params, Map<String, ?> row, Map<String, ?> prevParams) {
		Map<String, Object> bindings = new HashMap<>();
		bindings.put("filter", filter);
		bindings.put("params", params);
		if (row != null) {
			bindings.put("row", row);
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
			.create(finalScript.toString(), TemplateEngineType.GroovyShell);

		stringTemplate.process(bindings);
	}

	@Override
	public void addDataEventHandler(IDataEventHandler handler) {
		handlers.add(handler);
	}

	// ------------------------------ PRIVATE METHODS

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

	private void updateDataVOByDataSource(DataVO dataVO, Long dsId) {
		DataSource dataSource = dataSourceService.load(dsId);
		XDataSource xDataSource = dataSource.getXDataSource();

		dataVO.setDataSourceId(dataSource.getId());
		dataVO.setConnectionId(dataSource.getConnection().getId());
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

	private Object convertQueryParam(XDSFieldType fieldType, List<String> values) {
		if (values.size() > 1) {
			List<Object> convertedValues = new ArrayList<>();
			for (String value : values) {
				convertedValues.add(convertQueryParam(fieldType, value));
			}
			return convertedValues;
		} else {
			return convertQueryParam(fieldType, values.get(0));
		}
	}

	private Object convertFilterParam(XDSFieldType fieldType, Object value, boolean canReturnList) {
		Object result = null;

		if (value != null) {
			if (value instanceof List) {
				List list = (List) value;
				if (canReturnList) {
					List<Object> newList = new ArrayList<>();
					for (Object item : list) {
						newList.add(convertFilterParam(fieldType, item, false));
					}
					result = newList;
				} else {
					result = convertFilterParam(fieldType, list.get(0), false);
				}
			} else if (value instanceof KeyValueVO) {
				KeyValueVO keyValueVO = (KeyValueVO) value;
				result = convertFilterParam(fieldType, keyValueVO.getKey(), false);
			} else {
				switch (fieldType) {
					case String:
						result = value.toString();
						break;

					case Integer:
					case Real:
						if (value instanceof Number) {
							result = value;
						} else if (value instanceof String) {
							String str = (String) value;
							if (fieldType == XDSFieldType.Integer) {
								result = Long.parseLong(str);
							} else {
								result = new BigDecimal(str);
							}
						} else {
							throw new RuntimeException("Not Number/String Value");
						}
						break;

					case Date:
					case DateTime:
						if (value instanceof Date) {
							result = value;
						} else if (value instanceof String) {
							String str = (String) value;
							if (fieldType == XDSFieldType.Date) {
								result = CalendarUtil.parseDate(str, DATE_PATTERN);
							} else {
								result = CalendarUtil.parseDate(str, DATE_TIME_PATTERN);
							}
						} else {
							throw new RuntimeException("Not Date/String Value");
						}
						break;

					case Boolean:
						result = Boolean.valueOf(value.toString());
						break;

					default:
						throw new RuntimeException("Invalid value for target filter");
				}
			}
		}

		return result;
	}

	private Serializable convertQueryParam(XDSFieldType fieldType, String value) {
		Serializable result = null;

		if (value != null) {
			switch (fieldType) {
				case String:
					result = value;
					break;

				case Integer:
					result = Long.valueOf(value);
					break;

				case Real:
					result = new BigDecimal(value);
					break;

				case Date:
					result = CalendarUtil.parseDate(value, DATE_PATTERN);
					break;

				case DateTime:
					result = CalendarUtil.parseDate(value, DATE_TIME_PATTERN);
					break;

				case Boolean:
					result = Boolean.valueOf(value);
					break;

				case LookUp:
					break;
			}
		}

		return result;
	}

	private Map<String, Object> createMapOfFilterTargetDS(String filter, List<XDSAbstractField> xdsFields) {
		Map<String, List<String>> paramsMap = toMap(filter);

		Map<String, Object> result = new HashMap<>();
		for (XDSAbstractField xdsField : xdsFields) {
			String fieldName = xdsField.getName();
			List<String> values = paramsMap.get(fieldName);

			if (values != null && values.size() > 0) {
				switch (xdsField.getFilterType()) {
					case Equal:
						result.put(fieldName, convertQueryParam(xdsField.getType(), values));
						break;

					case Contain:
						String paramValue = values.get(0);
						result.put(fieldName, convertQueryParam(xdsField.getType(), paramValue));
						break;

					case List:
					case Search:
						List<KeyValueVO<String, String>> keyValueVOs = new ArrayList<>();
						for (String value : values) {
							keyValueVOs.add(new KeyValueVO<>(value, value));
						}
						result.put(fieldName, keyValueVOs);

						//NOTE old one that is wrong: result.put(fieldName, values);
						break;
				}
			} else if (paramsMap.containsKey(fieldName + "_u") || paramsMap.containsKey(fieldName + "_l")) {
				if (xdsField.getFilterType().equals(XDSFieldFilterType.Range)) {
					Serializable lower = null;
					Serializable upper = null;
					if (paramsMap.containsKey(fieldName + "_l")) {
						lower = convertQueryParam(xdsField.getType(), paramsMap.get(fieldName + "_l").get(0));
					}
					if (paramsMap.containsKey(fieldName + "_u")) {
						upper = convertQueryParam(xdsField.getType(), paramsMap.get(fieldName + "_u").get(0));
					}
					RangeVO rangeVO = new RangeVO<>(lower, upper);
					result.put(fieldName, rangeVO);
				} else {
					logger.error("FilterTargetDS has parameter sent as range with invalid filter type: field=[{}] filter=[{}]",
						xdsField.getName(), filter);
				}
			}
		}

		return result;
	}

	/**
	 * It is based on WebUtil.toMap()
	 *
	 * @param paramsAsUrl
	 * @return
	 */
	private Map<String, List<String>> toMap(String paramsAsUrl) {
		Map<String, List<String>> result = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);

		String[] paramValueArr = paramsAsUrl.split("[&]");

		for (String paramValue : paramValueArr) {
			int i = paramValue.indexOf('=');
			if (i > 0) {
				String param = paramValue.substring(0, i);
				String value = paramValue.substring(i + 1);

				if (!result.containsKey(param)) {
					result.put(param, new AdroitList<>(String.CASE_INSENSITIVE_ORDER));
				}

				List<String> values = result.get(param);
				if (!value.isEmpty()) {
					values.add(value);
				}
			}
		}

		HashSet<String> keys = new HashSet<>(result.keySet());
		for (String key : keys) {
			if (result.get(key).size() == 0) {
				result.remove(key);
			}
		}
		return result;
	}
}
