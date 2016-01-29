package org.devocative.metis.iservice;

import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.entity.dataSource.config.XDSField;
import org.devocative.metis.entity.dataSource.config.XDSQuery;
import org.devocative.metis.entity.dataSource.config.XDataSource;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

public interface IDataSourceService {
	void saveOrUpdate(DataSource dataSource, XDSQuery xdsQuery, List<XDSField> fields);

	List<DataSource> search(long firstResult, long maxResults);

	long count();

	List<DataSource> getListForLookup();

	DataSource getDataSource(String name);

	XDataSource getXDataSource(DataSource dataSource);

	XDataSource getXDataSource(String name);

	long getCountForDataSource(String name, Map<String, Object> filters);

	List<XDSField> createFields(List<XDSField> currentFields, XDSQuery xdsQuery, Long connectionId);

	List<Map<String, Object>> executeDataSource(String name,
												Map<String, Object> filters,
												Map<String, String> sortFields,
												Long pageIndex,
												Long pageSize);

	List<KeyValueVO<Serializable, String>> getLookUpList(XDSField field);

	List<Map<String, Object>> getChildrenOfParent(String name, Serializable parentId, Map<String, String> sortFields);
}
