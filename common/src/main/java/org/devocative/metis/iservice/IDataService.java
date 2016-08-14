package org.devocative.metis.iservice;

import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.async.DataViewQVO;
import org.devocative.metis.vo.async.DataViewRVO;
import org.devocative.metis.vo.query.ODataQVO;

import java.util.List;
import java.util.Map;

public interface IDataService {
	DataVO loadDataVO(String dataViewName);

	void updateDataVOByDataSource(DataVO dataVO, String dsName);

	List<DataFieldVO> findFilteringFields(List<DataFieldVO> allFields);

	List<DataAbstractFieldVO> findLookUpFields(DataVO dataVO);

	void updateParamsByQuery(List<DataParameterVO> currentParams, String... query);

	void updateFieldsByQuery(DataVO dataVO);

	void saveOrUpdate(DataVO dataVO);

	DataViewRVO executeDataView(DataViewQVO request);

	DataViewRVO executeDataViewForParent(DataViewQVO request);

	List<Map<String, Object>> executeOData(ODataQVO request);

	Map<String, Object> convertSimpleParamsToFilter(
		Long dataSourceId,
		List<DataFieldVO> fields,
		Map<String, List<String>> params,
		String sentDBConnection);
}
