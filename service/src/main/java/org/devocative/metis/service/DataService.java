package org.devocative.metis.service;

import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.ObjectUtil;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.entity.data.config.XDataSource;
import org.devocative.metis.entity.data.config.XDataView;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.metis.vo.DataVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service("mtsDataService")
public class DataService implements IDataService {

	private XStream dsXStream, dvXStream;

	@Autowired
	private IPersistorService persistorService;

	{
		dsXStream = new XStream();
		dsXStream.processAnnotations(XDataSource.class);

		dvXStream = new XStream();
		dvXStream.processAnnotations(XDataView.class);
	}

	@Override
	public DataVO loadDataVO(String dataViewName) {
		DataView dataView = persistorService
			.createQueryBuilder()
			.addFrom(DataView.class, "ent")
			.addWhere("and ent.name=:name")
			.addParam("name", dataViewName)
			.object();

		DataVO result = null;
		if (dataView != null) {
			result = new DataVO();

			XDataView xDataView = (XDataView) dvXStream.fromXML(dataView.getConfig().getValue());

			updateDataVOByDataSource(result, xDataView.getDataSourceName());
			result.setDataViewId(dataView.getId());

			ObjectUtil.merge(result, xDataView, true);
		}
		return result;
	}

	@Override
	public void updateDataVOByDataSource(DataVO dataVO, String dsName) {
		DataSource dataSource = getDataSource(dsName);
		XDataSource xDataSource = (XDataSource) dsXStream.fromXML(dataSource.getConfig().getValue());

		dataVO.setDataSourceId(dataSource.getId());
		dataVO.setDbConnectionId(dataSource.getConnectionId());
		dataVO.setDbConnectionHasMapping(dataSource.getConnection().getSafeConfigId() != null);

		ObjectUtil.merge(dataVO, xDataSource, true);
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

	public List<DataParameterVO> createParams(String query, List<DataParameterVO> currentParams) {
		List<DataParameterVO> result = new ArrayList<>();
		Pattern p = Pattern.compile("(['].*?['])|[:]([\\w\\d_]+)");
		Matcher matcher = p.matcher(query);
		while (matcher.find()) {
			if (matcher.group(1) == null) {
				String param = matcher.group(2).toLowerCase();

				DataParameterVO parameterVO = new DataParameterVO();
				parameterVO.setName(param);
				parameterVO.setRequired(true);

				int idx = currentParams.indexOf(parameterVO);
				if (idx > -1) {
					result.add(currentParams.get(idx));
				} else {
					result.add(parameterVO);
				}
			}
		}
		return result;
	}

	private DataSource getDataSource(String name) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DataSource.class, "ent")
			.addWhere("and ent.name = :name")
			.addParam("name", name)
			.object();
	}
}
