package org.devocative.metis.iservice;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

public interface IDBConnectionService {
	Connection getConnection(String name);

	List<Map<String, Object>> executeQuery(String name, String query, Map<String, Object> params) throws SQLException;

	void closeAllPools();
}
