package org.devocative.metis.iservice.data;

import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.config.XDSQueryMode;
import org.devocative.metis.entity.data.config.XDataSource;
import org.devocative.metis.vo.filter.data.DataSourceFVO;
import org.devocative.metis.vo.query.*;

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

	// ---------------

	String processQuery(Long dbConnId, String query, XDSQueryMode mode);

	EQLMetaDataVO processEntityQuery(Long dbConnId, String query);

	String processDynamicQuery(String text, Map<String, Object> params);

	DsQueryRVO<List<Map<String, Object>>> execute(SelectQueryQVO queryQVO);

	DsQueryRVO<List<KeyValueVO<Serializable, String>>> executeLookUp(Long dataSourceId, Long targetDataSourceId, String sentDBConnection, Map<String, Object> filter);

	DsQueryRVO<List<Map<String, Object>>> executeOfParent(SelectQueryQVO queryQVO, Serializable parentId);

	DsQueryRVO<Long> execute(CountQueryQVO queryQVO);

	DsQueryRVO<List<Map<String, Object>>> execute(AggregateQueryQVO queryQVO);

	List<QueryExecInfoRVO> executeAfterIfAny(Long dsId, String sentDBConnection);
}
