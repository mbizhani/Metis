package org.devocative.metis.iservice.data;

import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.demeter.entity.User;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.config.XDSQueryMode;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.filter.data.DataSourceFVO;
import org.devocative.metis.vo.query.*;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

public interface IDataSourceService {
	void saveOrUpdate(DataSource entity);

	DataSource load(String id);

	DataSource loadByName(String name);

	List<DataSource> list();

	List<DataSource> search(DataSourceFVO filter, long pageIndex, long pageSize);

	long count(DataSourceFVO filter);

	List<DBConnection> getConnectionList();

	List<User> getCreatorUserList();

	List<User> getModifierUserList();

	// ==============================

	String CACHE_KEY = "MTS_DATA_SOURCE";

	DataSource saveOrUpdate(DataVO dataVO);

	List<DataSource> getAllDataSourcesAsLookup();

	String processQuery(Long dbConnId, String query, XDSQueryMode mode);

	EQLMetaDataVO processEntityQuery(Long dbConnId, String query);

	String processDynamicQuery(String text, Map<String, Object> params);

	DsQueryRVO<List<Map<String, Object>>> execute(SelectQueryQVO queryQVO);

	DsQueryRVO<List<KeyValueVO<Serializable, String>>> execute(LookupQueryQVO queryQVO);

	DsQueryRVO<List<Map<String, Object>>> executeOfParent(SelectQueryQVO queryQVO, Serializable parentId);

	DsQueryRVO<Long> execute(CountQueryQVO queryQVO);

	DsQueryRVO<List<Map<String, Object>>> execute(AggregateQueryQVO queryQVO);

	List<QueryExecInfoRVO> executeAfterIfAny(String dsId);
}
