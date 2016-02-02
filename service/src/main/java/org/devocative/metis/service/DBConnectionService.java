package org.devocative.metis.service;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.mapping.XSchema;
import org.devocative.metis.entity.dataSource.config.XDSField;
import org.devocative.metis.entity.dataSource.config.XDSFieldType;
import org.devocative.metis.iservice.IDBConnectionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.sql.*;
import java.util.*;

@Service("mtsDBConnectionService")
public class DBConnectionService implements IDBConnectionService {
	private static final Logger logger = LoggerFactory.getLogger(DBConnectionService.class);

	private static final Map<Long, ComboPooledDataSource> CONNECTION_POOL_MAP = new HashMap<>();
	private static final Map<Long, DBConnection> CONNECTION_MAP = new HashMap<>();
	private static final Map<Long, XSchema> CONNECTION_MAPPING_MAP = new HashMap<>();

	private static final List<Integer> STRING_TYPES = Arrays.asList(Types.VARCHAR, Types.CHAR, Types.NVARCHAR,
		Types.NCHAR, Types.LONGNVARCHAR, Types.CLOB, Types.NCLOB);

	private static final List<Integer> INTEGER_TYPES = Arrays.asList(Types.INTEGER, Types.BIGINT, Types.NUMERIC,
		Types.SMALLINT, Types.TINYINT);

	private static final List<Integer> REAL_TYPES = Arrays.asList(Types.DOUBLE, Types.FLOAT, Types.DECIMAL);

	private static final List<Integer> DATE_TYPES = Arrays.asList(Types.DATE, Types.TIME, Types.TIMESTAMP);

	private XStream xstream;

	@Autowired
	private IPersistorService persistorService;

	public DBConnectionService() {
		xstream = new XStream();
		xstream.processAnnotations(XSchema.class);
	}

	@Override
	public void saveOrUpdate(DBConnection dbConnection, String mappingXML) {
		ConfigLob configLob = new ConfigLob();
		configLob.setId(dbConnection.getConfigId());
		configLob.setValue(mappingXML);
		persistorService.saveOrUpdate(configLob);

		dbConnection.setConfig(configLob);
		persistorService.saveOrUpdate(dbConnection);
		persistorService.commitOrRollback();

		CONNECTION_MAP.remove(dbConnection.getId());
		CONNECTION_MAPPING_MAP.remove(dbConnection.getId());
		// TODO CONNECTION_POOL_MAP.remove(dbConnection.getId());
	}

	@Override
	public List<DBConnection> list() {
		return persistorService.list(DBConnection.class);
	}

	public List<XDSField> getFields(Long id, String sql) throws SQLException {
		List<XDSField> result = new ArrayList<>();

		try (Connection connection = getConnection(id)) {
			NamedParameterStatement nps = new NamedParameterStatement(connection, sql, CONNECTION_MAP.get(id).getSchema());
			nps.setFetchSize(1);
			ResultSet rs = nps.executeQuery();
			ResultSetMetaData metaData = rs.getMetaData();
			for (int i = 1; i <= metaData.getColumnCount(); i++) {
				XDSField field = new XDSField();
				field
					.setName(metaData.getColumnName(i))
					.setDbType(metaData.getColumnTypeName(i))
					.setDbSize(metaData.getColumnDisplaySize(i));

				if (STRING_TYPES.contains(metaData.getColumnType(i))) {
					field.setType(XDSFieldType.String);
				} else if (DATE_TYPES.contains(metaData.getColumnType(i))) {
					field.setType(XDSFieldType.Date);
				} else if (INTEGER_TYPES.contains(metaData.getColumnType(i))) {
					field.setType(XDSFieldType.Integer);
				} else if (REAL_TYPES.contains(metaData.getColumnType(i))) {
					field.setType(XDSFieldType.Real);
				} else {
					logger.warn("Unknown type: name={}, id={}", metaData.getColumnTypeName(i), metaData.getColumnType(i));
				}
				result.add(field);
			}
			nps.close();
		}

		return result;
	}

	@Override
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
				CONNECTION_MAP.put(id, info);
			}

			return CONNECTION_POOL_MAP.get(id).getConnection();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public void closeAllPools() {
		for (ComboPooledDataSource pool : CONNECTION_POOL_MAP.values()) {
			pool.close();
		}
	}

	@Override
	public List<Map<String, Object>> executeQuery(Long id, String query, Map<String, Object> params, String comment) throws SQLException {

		try (Connection connection = getConnection(id)) {
			if (comment != null) {
				query = String.format("/*%s*/ %s", comment, query);
			}
			NamedParameterStatement nps = new NamedParameterStatement(connection, query, CONNECTION_MAP.get(id).getSchema());
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

	@Override
	public List<KeyValueVO<Serializable, String>> executeQueryAsKeyValues(Long id, String query) throws SQLException {
		List<KeyValueVO<Serializable, String>> result = new ArrayList<>();

		try (Connection connection = getConnection(id)) {
			NamedParameterStatement nps = new NamedParameterStatement(connection, query, CONNECTION_MAP.get(id).getSchema());
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

	@Override
	public boolean isOracle(Long id) {
		try (Connection ignored = getConnection(id)) {
			DBConnection connection = CONNECTION_MAP.get(id);
			return connection.getDriver().contains("OracleDriver") || connection.getUrl().startsWith("jdbc:oracle");
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public boolean isMySQL(Long id) {
		try (Connection ignored = getConnection(id)) {
			DBConnection connection = CONNECTION_MAP.get(id);
			return connection.getDriver().contains("OracleDriver") || connection.getUrl().startsWith("jdbc:mysql");
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public XSchema getSchemaOfMapping(Long id) {
		if (!CONNECTION_MAPPING_MAP.containsKey(id)) {
			String config = persistorService.createQueryBuilder().addSelect("select cfg.value from DBConnection ent join ent.config cfg")
				.addWhere("and ent.id = :id")
				.addParam("id", id)
				.object();

			if (config != null) {
				XSchema xSchema = (XSchema) xstream.fromXML(config);
				CONNECTION_MAPPING_MAP.put(id, xSchema);
			}
		}
		return CONNECTION_MAPPING_MAP.get(id);
	}
}
