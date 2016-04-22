package org.devocative.metis.iservice;

import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.config.*;
import org.devocative.metis.vo.filter.DataSourceFVO;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

public interface IDataSourceService {
	DataSource load(Long id);

	DataSource loadByName(String name);

	List<DataSource> list();

	DataSource saveOrUpdate(Long dataSourceId, Long dbConnId, String title, XDataSource xDataSource);

	List<DataSource> search(DataSourceFVO filter, long pageIndex, long pageSize);

	long count(DataSourceFVO filter);

	List<DataSource> getListForLookup();

	XDataSource getXDataSource(DataSource dataSource);

	XDataSource getXDataSource(String name);

	long executeCountForDataSource(String name, Map<String, Object> filters);

	Long findProperDBConnection(String dataSourceName);

	List<XDSField> createFields(List<XDSField> currentFields, XDSQuery xdsQuery, Long connectionId,
								List<XDSParameter> xdsParameters);

	List<XDSParameter> createParams(String query, List<XDSParameter> currentParams);

	List<Map<String, Object>> executeDataSource(String name,
												Map<String, Object> filters,
												Map<String, String> sortFields,
												Long pageIndex,
												Long pageSize);

	List<KeyValueVO<Serializable, String>> getLookUpList(Long targetDataSourceId);

	List<Map<String, Object>> getChildrenOfParent(String name, Serializable parentId, Map<String, String> sortFields);

	String processQuery(Long dbConnId, XDSQueryMode mode, String query);
}
