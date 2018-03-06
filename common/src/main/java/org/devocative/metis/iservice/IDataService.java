package org.devocative.metis.iservice;

import org.devocative.demeter.iservice.task.ITaskResultCallback;
import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.async.DataViewQVO;
import org.devocative.metis.vo.async.DataViewRVO;
import org.devocative.metis.vo.query.ODataQVO;

import java.util.List;
import java.util.Map;
import java.util.Set;

public interface IDataService {
	DataVO loadDataVO(Long dataViewId);

	DataVO loadDataVO(String dataViewName);

	DataVO createAnotherDataView(String dsName);

	List<DataAbstractFieldVO> findFilteringFields(List<DataAbstractFieldVO> allFields);

	List<DataAbstractFieldVO> findLookUpFields(DataVO dataVO);

	void updateParamsByQuery(List<DataParameterVO> currentParams, String... query);

	void updateFieldsByQuery(DataVO dataVO);

	void saveOrUpdate(DataVO dataVO);

	void executeDTask(DataViewQVO qvo, ITaskResultCallback callback);

	DataViewRVO executeDataView(DataViewQVO request);

	DataViewRVO exportDataView(DataViewQVO request);

	DataViewRVO executeDataViewForParent(DataViewQVO request);

	List<Map<String, Object>> executeOData(ODataQVO request);

	Long executeODataCount(ODataQVO request);

	Set<String> convertSimpleParamsToFilter(
		Map<String, Object> result,
		String dataSourceId,
		List<DataAbstractFieldVO> fields,
		Map<String, List<String>> params,
		String sentDBConnection);

	Map<String, Object> convertFilterToFilter(
		String dataSourceId,
		List<DataAbstractFieldVO> fields,
		Map<String, Object> filter,
		String sentDBConnection);

	void processDynamicFilterAndParam(String script, Map<String, ?> filter, Map<String, ?> params, Map<String, ?> row, Map<String, ?> prevParams);

	void addDataEventHandler(IDataEventHandler handler);
}
