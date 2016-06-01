package org.devocative.metis.iservice;

import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.config.XDSQueryMode;
import org.devocative.metis.entity.data.config.XDataSource;
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

	List<DataSource> getAllDataSourcesAsLookup();

	XDataSource getXDataSource(DataSource dataSource);

	XDataSource getXDataSource(String name);

	Long findProperDBConnection(String dataSourceName);

	List<KeyValueVO<Serializable, String>> getLookUpList(Long targetDataSourceId);

	List<Map<String, Object>> executeDataSourceForParent(String queryCode,
														 String name,
														 List<String> selectFields,
														 Serializable parentId,
														 Map<String, String> sortFields);

	String processQuery(Long dbConnId, XDSQueryMode mode, String query);
}
