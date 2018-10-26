package org.devocative.metis.iservice;

import org.devocative.adroit.date.TimeFieldVO;
import org.devocative.demeter.iservice.task.ITaskResultCallback;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.RowInputVO;
import org.devocative.metis.vo.async.DataViewQVO;
import org.devocative.metis.vo.async.DataViewRVO;

import java.util.List;
import java.util.Map;

public interface IDataService {
	DataVO loadDataVO(String dataViewId);

	DataVO loadDataVOByName(String dataViewName);

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

	List<Map<String, Object>> executeOData(DataViewQVO request);

	Long executeODataCount(DataViewQVO request);

	void processDynamicFilterAndParam(String script, Map<String, ?> params, Map<String, ?> prevParams, RowInputVO rowInputVO);

	void addDataEventHandler(IDataEventHandler handler);

	TimeFieldVO extractTimeFields(MetisConfigKey configKey);
}
