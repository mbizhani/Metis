package org.devocative.metis.service;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.metis.entity.DBConnectionInfo;
import org.devocative.metis.iservice.IDBConnectionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DBConnectionService implements IDBConnectionService {
	private static final Logger logger = LoggerFactory.getLogger(DBConnectionService.class);

	private static final Map<String, DBConnectionInfo> CONNECTION_MAP = new HashMap<>();
	private static final Map<String, ComboPooledDataSource> CONNECTION_POOL_MAP = new HashMap<>();

	static {
		XStream xstream = new XStream();
		xstream.processAnnotations(DBConnectionInfo.class);
		List<DBConnectionInfo> list = (List<DBConnectionInfo>) xstream.fromXML(DBConnectionService.class.getResourceAsStream("/dbConnections.xml"));
		for (DBConnectionInfo connection : list) {
			logger.debug("DBConnectionInfo: {}", connection);
			CONNECTION_MAP.put(connection.getName(), connection);
		}
	}

	//------------------------------- SINGLETON
	private static DBConnectionService instance = new DBConnectionService();

	private DBConnectionService() {
	}

	public static DBConnectionService get() {
		return instance;
	}

	//------------------------------- METHODS

	public Connection getConnection(String name) {
		if (!CONNECTION_MAP.containsKey(name)) {
			throw new RuntimeException("Invalid connection name: " + name);
		}

		try {
			if (!CONNECTION_POOL_MAP.containsKey(name)) {
				DBConnectionInfo info = CONNECTION_MAP.get(name);

				ComboPooledDataSource cpds = new ComboPooledDataSource();
				cpds.setDriverClass(info.getDriver());
				cpds.setJdbcUrl(info.getUrl());
				cpds.setUser(info.getUsername());
				cpds.setPassword(info.getPassword());

				CONNECTION_POOL_MAP.put(name, cpds);
			}

			return CONNECTION_POOL_MAP.get(name).getConnection();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public void closeAllPools() {
		for (ComboPooledDataSource pool : CONNECTION_POOL_MAP.values()) {
			pool.close();
		}
	}

	public List<Map<String, Object>> executeQuery(String name, String query, Map<String, Object> params) throws SQLException {

		try (Connection connection = getConnection(name)) {
			NamedParameterStatement nps = new NamedParameterStatement(connection);
			nps.setSchema(CONNECTION_MAP.get(name).getSchema());
			nps.setQuery(query);
			if (params != null) {
				nps.setParameters(params);
			}

			ResultSet rs = nps.executeQuery();
			ResultSetMetaData metaData = rs.getMetaData();

			List<String> columns = new ArrayList<>();
			for (int i = 1; i <= metaData.getColumnCount(); i++) {
				columns.add(metaData.getColumnName(i).toLowerCase());
			}

			List<Map<String, Object>> result = new ArrayList<>();
			while (rs.next()) {
				Map<String, Object> row = new HashMap<>();
				for (String column : columns) {
					row.put(column, rs.getObject(column));
				}
				result.add(row);
			}
			return result;
		}
	}
}
