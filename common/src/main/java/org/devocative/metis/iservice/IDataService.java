package org.devocative.metis.iservice;

import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.async.DataViewQVO;
import org.devocative.metis.vo.async.DataViewRVO;

import java.util.List;

public interface IDataService {
	DataVO loadDataVO(String dataViewName);

	void updateDataVOByDataSource(DataVO dataVO, String dsName);

	List<DataAbstractFieldVO> findFilteringFields(List<DataAbstractFieldVO> allFields);

	List<DataAbstractFieldVO> findLookUpFields(DataVO dataVO);

	void updateParamsByQuery(String query, List<DataParameterVO> existingParams);

	void updateFieldsByQuery(DataVO dataVO);

	void saveOrUpdate(DataVO dataVO);

	DataViewRVO executeDataView(DataViewQVO request);
}
