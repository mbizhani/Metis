package org.devocative.metis.iservice;

import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.metis.entity.DBConnectionInfo;

import java.io.Serializable;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

public interface IDBConnectionService {
	void saveOrUpdate(DBConnectionInfo connectionInfo);

	Connection getConnection(Long id);

	List<Map<String, Object>> executeQuery(Long id, String query, Map<String, Object> params) throws SQLException;

	List<KeyValueVO<Serializable, String>> executeQueryAsKeyValues(Long id, String query) throws SQLException;

	void closeAllPools();
}
