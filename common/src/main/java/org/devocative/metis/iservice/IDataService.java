package org.devocative.metis.iservice;

import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataVO;

import java.util.List;

public interface IDataService {
	DataVO loadDataVO(String dataViewName);

	void updateDataVOByDataSource(DataVO dataVO, String dsName);

	List<DataAbstractFieldVO> findFilteringFields(DataVO dataVO);

	List<DataAbstractFieldVO> findLookUpFields(DataVO dataVO);
}
