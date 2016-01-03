package org.devocative.metis.iservice;

import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.metis.entity.dataSource.DSField;
import org.devocative.metis.entity.dataSource.DataSource;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

public interface IDataSourceService {
	DataSource getDataSource(String name);

	long getCountForDataSource(String name, Map<String, Object> filters);

	List<Map<String, Object>> executeDataSource(String name,
												Map<String, Object> filters,
												Map<String, String> sortFields,
												Long pageIndex,
												Long pageSize);

	List<KeyValueVO<Serializable, String>> getLookUpList(DataSource dataSource, DSField field);
}
