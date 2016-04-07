package org.devocative.metis.iservice;

import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.mapping.XSchema;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.QueryResultVO;

import java.io.Serializable;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

public interface IDBConnectionService {
	List<DBConnection> search(long pageIndex, long pageSize);

	long count();

	void saveOrUpdate(DBConnection dbConnection, String mappingXML);

	List<DBConnection> list();

	DBConnection getByName(String name);

	List<DataFieldVO> getFields(Long id, String sql, Map<String, Object> params) throws SQLException;

	Connection getConnection(Long id);

	List<Map<String, Object>> executeQuery(Long id, String query, Map<String, Object> params, String comment) throws SQLException;

	List<KeyValueVO<Serializable, String>> executeQueryAsKeyValues(Long id, String query) throws SQLException;

	QueryResultVO executeQuery(Long id, String query, Map<String, Object> params);

	void closeAllPools();

	boolean isOracle(Long id);

	boolean isMySQL(Long id);

	XSchema getSchemaOfMapping(Long id);

	boolean checkConnection(Long id);

	void groupChanged(Long groupId);
}
