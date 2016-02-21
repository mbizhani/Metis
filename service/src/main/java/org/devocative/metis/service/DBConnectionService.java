package org.devocative.metis.service;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.mapping.XMany2One;
import org.devocative.metis.entity.connection.mapping.XOne2Many;
import org.devocative.metis.entity.connection.mapping.XProperty;
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
		xstream.processAnnotations(XProperty.class);
		xstream.processAnnotations(XMany2One.class);
		xstream.processAnnotations(XOne2Many.class);
	}

	@Override
	public List<DBConnection> search(long firstResult, long maxResults) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DBConnection.class, "ent")
			.list((firstResult - 1) * maxResults, firstResult * maxResults);
	}

	@Override
	public long count() {
		return persistorService
			.createQueryBuilder()
			.addSelect("select count(1)")
			.addFrom(DBConnection.class, "ent")
			.object();
	}

	@Override
	public void saveOrUpdate(DBConnection dbConnection, String mappingXML) {
		if (mappingXML != null) {
			ConfigLob configLob = dbConnection.getConfigId() == null ?
				new ConfigLob() :
				persistorService.get(ConfigLob.class, dbConnection.getConfigId());
			configLob.setValue(mappingXML);
			persistorService.saveOrUpdate(configLob);
			dbConnection.setConfig(configLob);
		}

		//TODO encrypt database password before save

		persistorService.saveOrUpdate(dbConnection);
		persistorService.commitOrRollback();

		connectionChanged(dbConnection.getId());
	}

	@Override
	public List<DBConnection> list() {
		return persistorService.list(DBConnection.class);
	}

	@Override
	public DBConnection getByName(String name) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DBConnection.class, "ent")
			.addWhere("and ent.name=:name")
			.addParam("name", name)
			.object();
	}

	@Override
	public List<XDSField> getFields(Long dbConnId, String sql, Map<String, Object> params) throws SQLException {
		List<XDSField> result = new ArrayList<>();

		try (Connection connection = getConnection(dbConnId)) {
			NamedParameterStatement nps = new NamedParameterStatement(connection, sql, getSchemaForDB(dbConnId));
			nps.setFetchSize(1);
			nps.setParameters(params);

			ResultSet rs = nps.executeQuery();
			ResultSetMetaData metaData = rs.getMetaData();
			for (int i = 1; i <= metaData.getColumnCount(); i++) {
				XDSField field = new XDSField();
				field
					.setDbType(metaData.getColumnTypeName(i))
					.setDbSize(metaData.getColumnDisplaySize(i))
					.setName(metaData.getColumnName(i))
				;

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
	public Connection getConnection(Long dbConnId) {
		int retry = 0;
		Connection result = null;
		Exception last = null;
		do {
			try {
				result = getUnsureConnection(dbConnId);
				break;
			} catch (Exception e) {
				logger.error("getUnsureConnection: " + dbConnId, e);
				last = e;
				retry++;
				closePoolSafely(dbConnId);
			}
		} while (retry < 3);

		if (result == null) {
			throw new MetisException(MetisErrorCode.DBConnection, String.format("%s (%s)", dbConnId, last), last);
		}

		return result;
	}

	@Override
	public void closeAllPools() {
		for (ComboPooledDataSource pool : CONNECTION_POOL_MAP.values()) {
			pool.close();
		}
	}

	@Override
	public List<Map<String, Object>> executeQuery(Long dbConnId, String query, Map<String, Object> params, String comment) throws SQLException {

		try (Connection connection = getConnection(dbConnId)) {
			if (comment != null) {
				query = String.format("/*%s*/ %s", comment, query);
			}
			NamedParameterStatement nps = new NamedParameterStatement(connection, query, getSchemaForDB(dbConnId));
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
				for (int i = 0; i < columns.size(); i++) {
					String column = columns.get(i);
					Object value;
					switch (metaData.getColumnType(i + 1)) {
						case Types.DATE:
							value = rs.getDate(column);
							break;
						case Types.TIME:
							value = rs.getTime(column);
							break;
						case Types.TIMESTAMP:
							value = rs.getTimestamp(column);
							break;
						default:
							value = rs.getObject(column);
					}
					row.put(column, value);
				}
				result.add(row);
			}
			return result;
		}
	}

	@Override
	public List<KeyValueVO<Serializable, String>> executeQueryAsKeyValues(Long dbConnId, String query) throws SQLException {
		List<KeyValueVO<Serializable, String>> result = new ArrayList<>();

		try (Connection connection = getConnection(dbConnId)) {
			NamedParameterStatement nps = new NamedParameterStatement(connection, query, getSchemaForDB(dbConnId));
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
	public boolean isOracle(Long dbConnId) {
		DBConnection connection = getDBConnection(dbConnId);
		return connection.getSafeDriver().contains("OracleDriver") || connection.getSafeUrl().startsWith("jdbc:oracle");
	}

	@Override
	public boolean isMySQL(Long dbConnId) {
		DBConnection connection = getDBConnection(dbConnId);
		return connection.getSafeDriver().contains("OracleDriver") || connection.getSafeUrl().startsWith("jdbc:mysql");
	}

	@Override
	public XSchema getSchemaOfMapping(Long dbConnId) {
		if (!CONNECTION_MAPPING_MAP.containsKey(dbConnId)) {
			Long safeConfigId = getDBConnection(dbConnId).getSafeConfigId();
			String config = persistorService.createQueryBuilder()
				.addSelect("select ent.value")
				.addFrom(ConfigLob.class, "ent")
				.addWhere("and ent.id = :id")
				.addParam("id", safeConfigId)
				.object();

			if (config != null) {
				XSchema xSchema = (XSchema) xstream.fromXML(config);
				CONNECTION_MAPPING_MAP.put(dbConnId, xSchema);
			}
		}
		return CONNECTION_MAPPING_MAP.get(dbConnId);
	}

	@Override
	public boolean checkConnection(Long id) {
		DBConnection dbConnection = getDBConnection(id);
		if (dbConnection.getSafeTestQuery() != null) {
			try (Connection connection = getConnection(id)) {
				Statement st = connection.createStatement();
				st.executeQuery(dbConnection.getSafeTestQuery());
				st.close();
				return true;
			} catch (SQLException e) {
				return false;
			}
		}
		return false;
	}

	@Override
	public void groupChanged(Long groupId) {
		logger.info("DBConnection(s) updating: DBConnectionGroup changed = {}", groupId);
		List<Long> ids = persistorService
			.createQueryBuilder()
			.addSelect("select ent.id")
			.addFrom(DBConnection.class, "ent")
			.addWhere("and ent.group.id = :groupId")
			.addParam("groupId", groupId)
			.list();

		for (Long id : ids) {
			connectionChanged(id);
		}
		logger.info(" DBConnectionGroup changed = {} => DBConnection(s) updated = {} ", groupId, ids.size());
	}

	private Connection getUnsureConnection(Long dbConnId) throws Exception {
		DBConnection dbConnection = getDBConnection(dbConnId);
		if (!CONNECTION_POOL_MAP.containsKey(dbConnId)) {
			ComboPooledDataSource cpds = new ComboPooledDataSource();
			cpds.setDriverClass(dbConnection.getSafeDriver());
			cpds.setJdbcUrl(dbConnection.getSafeUrl());
			cpds.setUser(dbConnection.getUsername());
			cpds.setPassword(dbConnection.getPassword());

			CONNECTION_POOL_MAP.put(dbConnId, cpds);
		}

		Connection connection = CONNECTION_POOL_MAP.get(dbConnId).getConnection();
		if (dbConnection.getSafeTestQuery() != null) {
			Statement st = connection.createStatement();
			st.executeQuery(dbConnection.getSafeTestQuery());
			st.close();
		}
		return connection;
	}

	private DBConnection getDBConnection(Long dbConnId) {
		if (!CONNECTION_MAP.containsKey(dbConnId)) {
			DBConnection info = persistorService.get(DBConnection.class, dbConnId);
			CONNECTION_MAP.put(dbConnId, info);
		}

		return CONNECTION_MAP.get(dbConnId);
	}

	private String getSchemaForDB(Long dbConnId) {
		return getDBConnection(dbConnId).getSchema();
	}

	private void connectionChanged(Long id) {
		CONNECTION_MAP.remove(id);
		CONNECTION_MAPPING_MAP.remove(id);

		// TODO only necessary changed data need close
		closePoolSafely(id);
		logger.info("DBConnection changed: {}", id);
	}

	private synchronized void closePoolSafely(Long dbConnId) {
		ComboPooledDataSource pool = CONNECTION_POOL_MAP.get(dbConnId);
		if (pool != null) {
			//TODO assert safely closing
			pool.close();
			CONNECTION_POOL_MAP.remove(dbConnId);
		}
	}
}
