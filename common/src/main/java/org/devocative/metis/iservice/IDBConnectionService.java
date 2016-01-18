package org.devocative.metis.iservice;

import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.metis.entity.DBConnection;
import org.devocative.metis.entity.dataSource.config.XDSField;

import java.io.Serializable;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

public interface IDBConnectionService {
	void saveOrUpdate(DBConnection connectionInfo);

	List<DBConnection> list();

	List<XDSField> getFields(Long id, String sql) throws SQLException;

	Connection getConnection(Long id);

	List<Map<String, Object>> executeQuery(Long id, String query, Map<String, Object> params) throws SQLException;

	List<KeyValueVO<Serializable, String>> executeQueryAsKeyValues(Long id, String query) throws SQLException;

	void closeAllPools();

	boolean isOracle(Long id);

	boolean isMySQL(Long id);
}
