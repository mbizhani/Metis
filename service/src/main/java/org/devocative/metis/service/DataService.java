package org.devocative.metis.service;

import org.devocative.adroit.CalendarUtil;
import org.devocative.adroit.ObjectUtil;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.adroit.vo.RangeVO;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.*;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.iservice.IDataViewService;
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

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.*;

@Service("mtsDataService")
public class DataService implements IDataService {
	private static final Logger logger = LoggerFactory.getLogger(DataService.class);

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

	// ------------------------------ PUBLIC METHODS

	@Override
	public DataVO loadDataVO(String dataViewName) {
		logger.info("Loading DataView data: DV=[{}] Usr=[{}]", dataViewName, securityService.getCurrentUser());

		DataView dataView = dataViewService.loadByName(dataViewName);

		DataVO result = null;
		if (dataView != null) {
			result = new DataVO();

			XDataView xDataView = dataViewService.getXDataView(dataView);

			if (dataView.getDataSourceId() == null || !dataView.getDataSourceId().equals(xDataView.getDataSourceId())) {
				throw new MetisException(MetisErrorCode.InvalidDataViewState);
			}

			result.setTitle(dataView.getTitle());

			updateDataVOByDataSource(result, dataView.getDataSourceId());
			result.setDataViewId(dataView.getId());

			result.fromXDataView(xDataView);

			result.setDataSourceEditable(
				result.getName() != null && result.getName().equals(result.getDataSourceName())
			);
		}
		return result;
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
				result.add(parameterVO);
			}
		}

		for (DataFieldVO fieldVO : dataVO.getFields()) {
			if (fieldVO.getType() == XDSFieldType.LookUp) {
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

		String sql = dataSourceService.processQuery(
			dataVO.getConnectionId(),
			dataVO.getQuery().getText(),
			dataVO.getQuery().getMode()
		);

		fieldsFromDB = dbConnectionService.findFields(
			dataVO.getConnectionId(),
			sql,
			params);

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

		dataViewService.saveOrUpdate(dataVO.getDataViewId(), dataVO.getTitle(), xDataView);

		persistorService.commitOrRollback();
	}

	@Override
	public DataViewRVO executeDataView(DataViewQVO request) {
		logger.info("Executing DataView: DV=[{}] Usr=[{}] SentDB=[{}]",
			request.getName(), securityService.getCurrentUser(), request.getSentDBConnection());
		long start = System.currentTimeMillis();

		DataView dataView = dataViewService.loadByName(request.getName());
		XDataView xDataView = dataViewService.getXDataView(dataView);

		List<String> selectFields = getSelectedFields(xDataView);

		DataViewRVO result = new DataViewRVO();

		// --------------- SELECT

		SelectQueryQVO selectQVO = new SelectQueryQVO(xDataView.getDataSourceName(), selectFields);
		selectQVO
			.setPageIndex(request.getPageIndex())
			.setPageSize(request.getPageSize())
			.setSortFields(request.getSortFieldList())
			.setInputParams(request.getFilter())
			.setSentDBConnection(request.getSentDBConnection());
		DsQueryRVO<List<Map<String, Object>>> listRVO = dataSourceService.execute(selectQVO);
		result.setList(listRVO.getResult());
		result.addQueryExecInfo(listRVO.getQueryExecInfo());

		// --------------- COUNT

		CountQueryQVO countQVO = new CountQueryQVO(xDataView.getDataSourceName());
		countQVO
			.setInputParams(request.getFilter())
			.setSentDBConnection(request.getSentDBConnection());
		DsQueryRVO<Long> countRVO = dataSourceService.execute(countQVO);
		result.setCount(countRVO.getResult());
		result.addQueryExecInfo(countRVO.getQueryExecInfo());

		// --------------- FOOTER (AGG)

		Map<String, List<XDVAggregatorFunction>> agrFields = new HashMap<>();
		for (XDVField xdvField : xDataView.getFields()) {
			if (xdvField.getFooter() != null && xdvField.getFooter().size() > 0) {
				agrFields.put(xdvField.getName(), xdvField.getFooter());
			}
		}
		if (agrFields.size() > 0) {
			AggregateQueryQVO agrQVO = new AggregateQueryQVO(xDataView.getDataSourceName(), agrFields);
			agrQVO
				.setInputParams(request.getFilter())
				.setSentDBConnection(request.getSentDBConnection());
			DsQueryRVO<List<Map<String, Object>>> footerRVO = dataSourceService.execute(agrQVO);
			result.setFooter(footerRVO.getResult());
			result.addQueryExecInfo(footerRVO.getQueryExecInfo());
		}

		logger.info("Executed DataView: DV=[{}] Usr=[{}] SentDB=[{}] Dur=[{}] Res#=[{}] Cnt=[{}] Ftr=[{}]",
			xDataView.getName(), securityService.getCurrentUser(), request.getSentDBConnection(),
			System.currentTimeMillis() - start, result.getList().size(), result.getCount(),
			result.getFooter() != null ? result.getFooter().size() : null);

		return result;
	}

	@Override
	public DataViewRVO executeDataViewForParent(DataViewQVO request) {
		logger.info("Executing DataView of Parent: DV=[{}] Prnt=[{}] Usr=[{}]",
			request.getName(), request.getParentId(), securityService.getCurrentUser());
		long start = System.currentTimeMillis();

		DataView dataView = dataViewService.loadByName(request.getName());
		XDataView xDataView = dataViewService.getXDataView(dataView);

		List<String> selectFields = getSelectedFields(xDataView);

		SelectQueryQVO selectQVO = new SelectQueryQVO(xDataView.getDataSourceName(), selectFields);
		selectQVO
			.setSortFields(request.getSortFieldList())
			.setSentDBConnection(request.getSentDBConnection());

		List<Map<String, Object>> list = dataSourceService.executeOfParent(selectQVO, request.getParentId()).getResult();

		DataViewRVO result = new DataViewRVO();
		result.setList(list);
		result.setParentId(request.getParentId().toString());

		logger.info("Executed DataView of Parent: DV=[{}] Prnt=[{}] Usr=[{}] Dur=[{}]",
			request.getName(), request.getParentId(), securityService.getCurrentUser(), System.currentTimeMillis() - start);

		return result;
	}

	@Override
	public List<Map<String, Object>> executeOData(ODataQVO request) {
		logger.info("Executing OData: DV=[{}] Usr=[{}]", request.getName(), securityService.getCurrentUser());
		long start = System.currentTimeMillis();

		try {
			DataView dataView = dataViewService.loadByName(request.getName());
			XDataView xDataView = dataViewService.getXDataView(dataView);

			List<String> selectFields = getSelectedFields(xDataView);

			Map<String, Object> inputParams = new HashMap<>();
			if (request.getInputParams() != null) {
				for (Map.Entry<String, Object> entry : request.getInputParams().entrySet()) {
					inputParams.put(entry.getKey().toLowerCase(), entry.getValue());
				}
			}

			SelectQueryQVO selectQVO = new SelectQueryQVO(xDataView.getDataSourceName(), selectFields);
			selectQVO
				.setPageIndex(request.getPageIndex())
				.setPageSize(request.getPageSize())
				.setFilterExpression(request.getFilterExpression())
				.setInputParams(inputParams);

			List<Map<String, Object>> list = dataSourceService.execute(selectQVO).getResult();

			logger.info("Executed OData: DV=[{}] Usr=[{}] Dur=[{}]",
				request.getName(), securityService.getCurrentUser(), System.currentTimeMillis() - start);

			return list;
		} catch (RuntimeException e) {
			logger.error("Execute OData Error: DV=" + request.getName(), e);
			throw e;
		}
	}

	@Override
	public Map<String, Object> convertSimpleParamsToFilter(
		Long dataSourceId,
		List<DataAbstractFieldVO> fields,
		Map<String, List<String>> params,
		String sentDBConnection) {

		Map<String, Object> result = new HashMap<>();

		for (DataAbstractFieldVO fieldVO : fields) {
			String fieldName = fieldVO.getName();
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
							XDataSource targetXDS = dataSourceService.getXDataSource(targetDS);

							Map<String, Object> lookUpFilter = new HashMap<>();

							if (fieldVO.getTargetDSFilter() != null) {
								Map<String, Object> filterTargetDS = createMapOfFilterTargetDS(fieldVO.getTargetDSFilter(), targetXDS.getFields());
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
							result.put(fieldName, filtered);
							break;
					}
				} else if (params.containsKey(fieldName + "_u") || params.containsKey(fieldName + "_l")) {
					if (fieldVO.getFilterType().equals(XDSFieldFilterType.Range)) {
						Serializable lower = convertQueryParam(fieldVO.getType(), params.get(fieldName + "_l").get(0));
						Serializable upper = convertQueryParam(fieldVO.getType(), params.get(fieldName + "_u").get(0));
						RangeVO rangeVO = new RangeVO<>(lower, upper);
						result.put(fieldName, rangeVO);
					}
				} else if (fieldVO.getTargetDSFilter() != null) {
					DataSource targetDS = dataSourceService.load(fieldVO.getTargetDSId());
					XDataSource targetXDS = dataSourceService.getXDataSource(targetDS);
					Map<String, Object> filterTargetDS = createMapOfFilterTargetDS(fieldVO.getTargetDSFilter(), targetXDS.getFields());
					List<KeyValueVO<Serializable, String>> filtered = dataSourceService.executeLookUp(
						dataSourceId,
						fieldVO.getTargetDSId(),
						sentDBConnection,
						filterTargetDS
					).getResult();
					result.put(fieldName, filtered);

				}
			} catch (Exception e) {
				logger.warn("Converting sent parameter value={} to filter, field=[{}], dsId=[{}], error=[{}]",
					values, fieldName, dataSourceService.load(dataSourceId), e.toString());
			}
		}

		return result;
	}


	// ------------------------------ PRIVATE METHODS

	private void updateDataVOByDataSource(DataVO dataVO, Long dsId) {
		DataSource dataSource = dataSourceService.load(dsId);
		XDataSource xDataSource = dataSourceService.getXDataSource(dataSource);

		dataVO.setDataSourceId(dataSource.getId());
		dataVO.setConnectionId(dataSource.getConnection().getId());
		dataVO.setConnectionHasMapping(dataSource.getConnection().getSafeConfigId() != null);

		dataVO.fromXDataSource(xDataSource);
	}

	private List<String> getSelectedFields(XDataView xDataView) {
		List<String> selectFields = new ArrayList<>();
		for (XDVField xdvField : xDataView.getFields()) {
			if (xdvField.getResultType() != null) {
				switch (xdvField.getResultType()) {
					case Shown:
					case Hidden:
						selectFields.add(xdvField.getName());
						break;
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
					result = CalendarUtil.toGregorian(value, "yyyyMMdd");
					break;

				case DateTime:
					result = CalendarUtil.toGregorian(value, "yyyyMMddHHmmss");
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

	private Map<String, Object> createMapOfFilterTargetDS(String filter, List<XDSField> xdsFields) {
		Map<String, List<String>> paramsMap = new HashMap<>();

		String[] params = filter.split("[&]");
		for (String paramValue : params) {
			int i = paramValue.indexOf("=");
			String param = paramValue.substring(0, i).toLowerCase();
			String value = paramValue.substring(i + 1);
			if (paramsMap.containsKey(param)) {
				paramsMap.get(param).add(value);
			} else {
				paramsMap.put(param, ObjectUtil.asList(value));
			}
		}

		Map<String, Object> result = new HashMap<>();
		for (XDSField xdsField : xdsFields) {
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
						result.put(fieldName, values);
						break;
				}
			} else if (paramsMap.containsKey(fieldName + "_u") || paramsMap.containsKey(fieldName + "_l")) {
				if (xdsField.getFilterType().equals(XDSFieldFilterType.Range)) {
					Serializable lower = convertQueryParam(xdsField.getType(), paramsMap.get(fieldName + "_l").get(0));
					Serializable upper = convertQueryParam(xdsField.getType(), paramsMap.get(fieldName + "_u").get(0));
					RangeVO rangeVO = new RangeVO<>(lower, upper);
					result.put(fieldName, rangeVO);
				}
			}
		}

		return result;
	}
}
