package org.devocative.metis.service;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.entity.DBConnection;
import org.devocative.metis.iservice.IDBConnectionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.sql.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service("mtsDBConnectionService")
public class DBConnectionService implements IDBConnectionService {
	private static final Logger logger = LoggerFactory.getLogger(DBConnectionService.class);
	private static final Map<Long, ComboPooledDataSource> CONNECTION_POOL_MAP = new HashMap<>();
	private static final Map<Long, String> CONNECTION_SCHEMA_MAP = new HashMap<>();

	@Autowired
	private IPersistorService persistorService;

	public void saveOrUpdate(DBConnection connectionInfo) {
		persistorService.saveOrUpdate(connectionInfo);
		persistorService.commitOrRollback();
	}

	public Connection getConnection(Long id) {
		try {
			if (!CONNECTION_POOL_MAP.containsKey(id)) {
				DBConnection info = persistorService.get(DBConnection.class, id);

				ComboPooledDataSource cpds = new ComboPooledDataSource();
				cpds.setDriverClass(info.getDriver());
				cpds.setJdbcUrl(info.getUrl());
				cpds.setUser(info.getUsername());
				cpds.setPassword(info.getPassword());

				CONNECTION_POOL_MAP.put(id, cpds);
				CONNECTION_SCHEMA_MAP.put(id, info.getSchema());
			}

			return CONNECTION_POOL_MAP.get(id).getConnection();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public void closeAllPools() {
		for (ComboPooledDataSource pool : CONNECTION_POOL_MAP.values()) {
			pool.close();
		}
	}

	public List<Map<String, Object>> executeQuery(Long id, String query, Map<String, Object> params) throws SQLException {

		try (Connection connection = getConnection(id)) {
			NamedParameterStatement nps = new NamedParameterStatement(connection);
			nps.setSchema(CONNECTION_SCHEMA_MAP.get(id));
			nps.setQuery(query);
			nps.setDateClassReplacement(Timestamp.class);
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

	public List<KeyValueVO<Serializable, String>> executeQueryAsKeyValues(Long id, String query) throws SQLException {
		List<KeyValueVO<Serializable, String>> result = new ArrayList<>();

		try (Connection connection = getConnection(id)) {
			NamedParameterStatement nps = new NamedParameterStatement(connection);
			nps.setSchema(CONNECTION_SCHEMA_MAP.get(id));
			nps.setQuery(query);
			nps.setDateClassReplacement(Timestamp.class);

			ResultSet rs = nps.executeQuery();
			ResultSetMetaData metaData = rs.getMetaData();
			while (rs.next()) {
				Serializable key = (Serializable) rs.getObject(1);
				String value = metaData.getColumnCount() > 1 ?
					rs.getString(2) :
					rs.getString(1);
				result.add(new KeyValueVO<>(key, value));
			}
		}
		return result;
	}
}
