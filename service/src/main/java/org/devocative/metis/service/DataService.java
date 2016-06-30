package org.devocative.metis.service;

import org.devocative.adroit.sql.NamedParameterStatement;
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
import org.devocative.metis.vo.query.AggregateQueryQVO;
import org.devocative.metis.vo.query.CountQueryQVO;
import org.devocative.metis.vo.query.ODataQVO;
import org.devocative.metis.vo.query.SelectQueryQVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
		DataView dataView = dataViewService.loadByName(dataViewName);

		DataVO result = null;
		if (dataView != null) {
			result = new DataVO();

			XDataView xDataView = dataViewService.getXDataView(dataView);
			result.setTitle(dataView.getTitle());

			updateDataVOByDataSource(result, xDataView.getDataSourceName());
			result.setDataViewId(dataView.getId());

			result.fromXDataView(xDataView);
		}
		return result;
	}

	@Override
	public void updateDataVOByDataSource(DataVO dataVO, String dsName) {
		DataSource dataSource = dataSourceService.loadByName(dsName);
		XDataSource xDataSource = dataSourceService.getXDataSource(dataSource);

		dataVO.setDataSourceId(dataSource.getId());
		dataVO.setConnectionId(dataSource.getConnection().getId());
		dataVO.setConnectionHasMapping(dataSource.getConnection().getSafeConfigId() != null);

		dataVO.fromXDataSource(xDataSource);
	}

	@Override
	public List<DataAbstractFieldVO> findFilteringFields(List<DataAbstractFieldVO> allFields) {
		List<DataAbstractFieldVO> result = new ArrayList<>();

		for (DataAbstractFieldVO dataFieldVO : allFields) {
			if (dataFieldVO.getInFilterPanelSafely()) {
				result.add(dataFieldVO);
			}
		}
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
	public void updateParamsByQuery(String query, List<DataParameterVO> currentParams) {
		List<DataParameterVO> temp = new ArrayList<>();

		List<String> paramsInQuery = NamedParameterStatement.findParamsInQuery(query);

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
			xDataSource.setName(dataVO.getName());
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
		xDataView.setDataSourceName(dataVO.getDataSourceName());

		dataViewService.saveOrUpdate(dataVO.getDataViewId(), dataVO.getTitle(), xDataView);

		persistorService.commitOrRollback();
	}

	@Override
	public DataViewRVO executeDataView(DataViewQVO request) {
		logger.info("Executing DataView: DV=[{}] Usr=[{}]",
			request.getName(), securityService.getCurrentUser());
		long start = System.currentTimeMillis();

		DataView dataView = dataViewService.loadByName(request.getName());
		XDataView xDataView = dataViewService.getXDataView(dataView);

		List<String> selectFields = getSelectedFields(xDataView);

		SelectQueryQVO selectQVO = new SelectQueryQVO(xDataView.getDataSourceName(), selectFields);
		selectQVO
			.setPageIndex(request.getPageIndex())
			.setPageSize(request.getPageSize())
			.setSortFields(request.getSortFieldList())
			.setInputParams(request.getFilter());
		List<Map<String, Object>> list = dataSourceService.execute(selectQVO);

		CountQueryQVO countQVO = new CountQueryQVO(xDataView.getDataSourceName());
		countQVO.setInputParams(request.getFilter());
		long cnt = dataSourceService.execute(countQVO);

		List<Map<String, Object>> footer = null;
		Map<String, List<XDVAggregatorFunction>> agrFields = new HashMap<>();
		for (XDVField xdvField : xDataView.getFields()) {
			if (xdvField.getFooter() != null && xdvField.getFooter().size() > 0) {
				agrFields.put(xdvField.getName(), xdvField.getFooter());
			}
		}
		if (agrFields.size() > 0) {
			AggregateQueryQVO agrQVO = new AggregateQueryQVO(xDataView.getDataSourceName(), agrFields);
			agrQVO.setInputParams(request.getFilter());
			footer = dataSourceService.execute(agrQVO);
		}

		DataViewRVO result = new DataViewRVO();
		result.setList(list);
		result.setFooter(footer);
		result.setCount(cnt);

		logger.info("Executed DataView: DV=[{}] Usr=[{}] Dur=[{}]",
			xDataView.getName(), securityService.getCurrentUser(), System.currentTimeMillis() - start);

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
		selectQVO.setSortFields(request.getSortFieldList());

		List<Map<String, Object>> list = dataSourceService.executeOfParent(selectQVO, request.getParentId());

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

		DataView dataView = dataViewService.loadByName(request.getName());
		XDataView xDataView = dataViewService.getXDataView(dataView);

		List<String> selectFields = getSelectedFields(xDataView);

		SelectQueryQVO selectQVO = new SelectQueryQVO(xDataView.getDataSourceName(), selectFields);
		selectQVO
			.setPageIndex(1L)
			.setPageSize(100L)
			.setFilterExpression(request.getFilterExpression())
			.setInputParams(request.getInputParams());

		List<Map<String, Object>> list = dataSourceService.execute(selectQVO);

		logger.info("Executed OData: DV=[{}] Usr=[{}] Dur=[{}]",
			request.getName(), securityService.getCurrentUser(), System.currentTimeMillis() - start);

		return list;
	}

	// ------------------------------ PRIVATE METHODS

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

}
