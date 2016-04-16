package org.devocative.metis.iservice;

import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.metis.vo.DataVO;

import java.util.List;

public interface IDataService {
	DataVO loadDataVO(String dataViewName);

	void updateDataVOByDataSource(DataVO dataVO, String dsName);

	List<DataAbstractFieldVO> findFilteringFields(List<DataAbstractFieldVO> allFields);

	List<DataAbstractFieldVO> findLookUpFields(DataVO dataVO);

	void updateParamsByQuery(String query, List<DataParameterVO> existingParams);

	void updateFieldsByQuery(DataVO dataVO);

	void saveOrUpdate(DataVO dataVO);
}
