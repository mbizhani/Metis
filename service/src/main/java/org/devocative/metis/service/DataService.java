package org.devocative.metis.service;

import com.thoughtworks.xstream.XStream;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.entity.data.config.XDataSource;
import org.devocative.metis.entity.data.config.XDataView;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.iservice.IDataViewService;
import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.metis.vo.DataVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service("mtsDataService")
public class DataService implements IDataService {

	private XStream dsXStream, dvXStream;

	@Autowired
	private IPersistorService persistorService;

	@Autowired
	private IDBConnectionService dbConnectionService;

	@Autowired
	private IDataSourceService dataSourceService;

	@Autowired
	private IDataViewService dataViewService;

	{
		dsXStream = new XStream();
		dsXStream.processAnnotations(XDataSource.class);

		dvXStream = new XStream();
		dvXStream.processAnnotations(XDataView.class);
	}

	@Override
	public DataVO loadDataVO(String dataViewName) {
		DataView dataView = dataViewService.loadByName(dataViewName);

		DataVO result = null;
		if (dataView != null) {
			result = new DataVO();

			XDataView xDataView = (XDataView) dvXStream.fromXML(dataView.getConfig().getValue());
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
		XDataSource xDataSource = (XDataSource) dsXStream.fromXML(dataSource.getConfig().getValue());

		dataVO.setDataSourceId(dataSource.getId());
		dataVO.setConnectionId(dataSource.getConnection().getId());
		dataVO.setConnectionHasMapping(dataSource.getConnection().getSafeConfigId() != null);

		dataVO.fromXDataSource(xDataSource);
	}

	@Override
	public List<DataAbstractFieldVO> findFilteringFields(DataVO dataVO) {
		List<DataAbstractFieldVO> result = new ArrayList<>();
		result.addAll(dataVO.getParams());

		for (DataFieldVO dataFieldVO : dataVO.getFields()) {
			if (dataFieldVO.getInFilterPanel()) {
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

		Pattern p = Pattern.compile("(['].*?['])|[:]([\\w\\d_]+)");
		Matcher matcher = p.matcher(query);

		while (matcher.find()) {
			if (matcher.group(1) == null) {
				String param = matcher.group(2).toLowerCase();

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

		currentParams.clear();
		currentParams.addAll(temp);
	}

	@Override
	public void updateFieldsByQuery(DataVO dataVO) {
		List<DataFieldVO> temp = new ArrayList<>();
		List<DataFieldVO> fieldsFromDB;
		try {
			Map<String, Object> params = new HashMap<>();
			for (DataParameterVO paramVO : dataVO.getParams()) {
				params.put(paramVO.getName(), paramVO.getSampleData());
			}

			String sql = dataSourceService.processQuery(
				dataVO.getConnectionId(),
				dataVO.getQuery().getMode(),
				dataVO.getQuery().getText()
			);

			fieldsFromDB = dbConnectionService.getFields(
				dataVO.getConnectionId(),
				sql,
				params);
		} catch (SQLException e) {
			throw new MetisException(MetisErrorCode.SQLExecution, e.getMessage(), e);
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
}
