package org.devocative.metis.iservice;

import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.config.XDSQueryMode;
import org.devocative.metis.entity.data.config.XDataSource;
import org.devocative.metis.vo.filter.DataSourceFVO;
import org.devocative.metis.vo.query.AggregateQueryQVO;
import org.devocative.metis.vo.query.CountQueryQVO;
import org.devocative.metis.vo.query.EQLMetaDataVO;
import org.devocative.metis.vo.query.SelectQueryQVO;

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

	// ---------------
	String processQuery(Long dbConnId, String query, XDSQueryMode mode);

	EQLMetaDataVO processEntityQuery(Long dbConnId, String query);

	List<Map<String, Object>> execute(SelectQueryQVO queryQVO);

	List<KeyValueVO<Serializable, String>> executeLookUp(Long dataSourceId, Long targetDataSourceId, String sentDBConnection, Map<String, Object> filter);

	List<Map<String, Object>> executeOfParent(SelectQueryQVO queryQVO, Serializable parentId);

	long execute(CountQueryQVO queryQVO);

	List<Map<String, Object>> execute(AggregateQueryQVO queryQVO);

	Map<String, Object> convertSimpleParamsToFilter(Long dsId, Map<String, List<String>> params, boolean usedInList);
}
