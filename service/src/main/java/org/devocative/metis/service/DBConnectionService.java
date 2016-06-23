package org.devocative.metis.service;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.demeter.vo.UserVO;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.MetisUserProfile;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.mapping.XMany2One;
import org.devocative.metis.entity.connection.mapping.XOne2Many;
import org.devocative.metis.entity.connection.mapping.XProperty;
import org.devocative.metis.entity.connection.mapping.XSchema;
import org.devocative.metis.entity.data.config.XDSFieldFilterType;
import org.devocative.metis.entity.data.config.XDSFieldResultType;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.query.QueryRVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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

	private static final String METIS_DEFAULT_CONNECTION = "METIS_DEFAULT_CONNECTION";

	private XStream xstream;

	@Autowired
	private IPersistorService persistorService;

	@Autowired
	private ISecurityService securityService;

	// ------------------------------

	public DBConnectionService() {
		xstream = new XStream();
		xstream.processAnnotations(XSchema.class);
		xstream.processAnnotations(XProperty.class);
		xstream.processAnnotations(XMany2One.class);
		xstream.processAnnotations(XOne2Many.class);
	}

	// ------------------------------

	@Override
	public DBConnection get(Long id) {
		return getDBConnection(id);
	}

	@Override
	public List<DBConnection> search(long pageIndex, long pageSize) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DBConnection.class, "ent")
			.list((pageIndex - 1) * pageSize, pageSize);
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
	public DBConnection loadByName(String name) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DBConnection.class, "ent")
			.addWhere("and ent.name=:name")
			.addParam("name", name)
			.object();
	}

	// ---------------

	@Override
	public List<DataFieldVO> findFields(Long dbConnId, String sql, Map<String, Object> params) {
		List<DataFieldVO> result = new ArrayList<>();

		try (Connection connection = getConnection(dbConnId)) {
			NamedParameterStatement nps = new NamedParameterStatement(connection, sql, getSchemaForDB(dbConnId));
			nps.setFetchSize(1);
			nps.setParameters(params);

			ResultSet rs = nps.executeQuery();
			ResultSetMetaData metaData = rs.getMetaData();
			for (int i = 1; i <= metaData.getColumnCount(); i++) {
				DataFieldVO fieldVO = new DataFieldVO();
				fieldVO.setName(metaData.getColumnName(i).toLowerCase());
				fieldVO.setDbType(String.format("%s [%s]",
					metaData.getColumnTypeName(i).toLowerCase(),
					metaData.getColumnType(i)));

				if (STRING_TYPES.contains(metaData.getColumnType(i))) {
					fieldVO.setType(XDSFieldType.String);
				} else if (DATE_TYPES.contains(metaData.getColumnType(i))) {
					fieldVO.setType(XDSFieldType.Date);
				} else if (INTEGER_TYPES.contains(metaData.getColumnType(i))) {
					fieldVO.setType(XDSFieldType.Integer);
				} else if (REAL_TYPES.contains(metaData.getColumnType(i))) {
					fieldVO.setType(XDSFieldType.Real);
				} else {
					fieldVO.setType(XDSFieldType.Unknown);
					fieldVO.setFilterType(XDSFieldFilterType.Unknown);
					fieldVO.setResultType(XDSFieldResultType.None);
				}
				result.add(fieldVO);
			}
			nps.close();
		} catch (SQLException e) {
			logger.error("findFields", e);
			throw new MetisException(MetisErrorCode.SQLExecution, e);
		}

		return result;
	}

	@Override
	public void closeAllPools() {
		for (ComboPooledDataSource pool : CONNECTION_POOL_MAP.values()) {
			pool.close();
		}
	}

	// ---------------

	@Override
	public QueryRVO executeQuery(
		Long dbConnId,
		String query,
		String comment,
		Map<String, Object> params) {

		return executeQuery(dbConnId, query, comment, params, null, null);
	}

	@Override
	public QueryRVO executeQuery(
		Long dbConnId,
		String query,
		String comment,
		Long pageIndex,
		Long pageSize) {
		return executeQuery(dbConnId, query, comment, null, pageIndex, pageSize);
	}

	// Main Method
	@Override
	public QueryRVO executeQuery(
		Long dbConnId,
		String query,
		String comment,
		Map<String, Object> params,
		Long pageIndex,
		Long pageSize) {

		try (Connection connection = getConnection(dbConnId)) {
			query = String.format("/*%s*/ %s", comment, query);

			NamedParameterStatement nps = new NamedParameterStatement(connection, query, getSchemaForDB(dbConnId));
			nps
				.setDateClassReplacement(Timestamp.class)
				.setPageIndex(pageIndex)
				.setPageSize(pageSize);

			if (params != null) {
				nps.setParameters(params);
			}

			ResultSet rs = nps.executeQuery();
			ResultSetMetaData metaData = rs.getMetaData();

			QueryRVO result = new QueryRVO();
			for (int i = 1; i <= metaData.getColumnCount(); i++) {
				result.addHeader(metaData.getColumnName(i).toLowerCase());
			}

			while (rs.next()) {
				List<Object> row = new ArrayList<>();
				for (int i = 0; i < result.getHeader().size(); i++) {
					String column = result.getHeader().get(i);
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
					row.add(value);
				}
				result.addRow(row);
			}
			return result;
		} catch (SQLException e) {
			logger.error("executeQuery: " + comment, e);
			throw new MetisException(MetisErrorCode.SQLExecution, e);
		}
	}

	// ---------------

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

	@Override
	public void setDefaultConnectionForCurrentUser(Long id) {
		DBConnection dbConnection = getDBConnection(id);
		if (dbConnection != null) {
			UserVO currentUser = securityService.getCurrentUser();

			MetisUserProfile metisUserProfile = persistorService.get(MetisUserProfile.class, currentUser.getUserId());
			if (metisUserProfile != null) {
				metisUserProfile.setDefaultConnection(dbConnection);
				persistorService.update(metisUserProfile);
			} else {
				metisUserProfile = new MetisUserProfile(currentUser.getUserId());
				metisUserProfile.setDefaultConnection(dbConnection);
				persistorService.save(metisUserProfile);
			}

			currentUser.addOtherProfileInfo(METIS_DEFAULT_CONNECTION, dbConnection.getId());
		}
	}

	@Override
	public DBConnection getDefaultConnectionOfCurrentUser() {
		UserVO currentUser = securityService.getCurrentUser();
		Long defaultConn = (Long) currentUser.getOtherProfileInfo(METIS_DEFAULT_CONNECTION);
		if (defaultConn == null) {
			MetisUserProfile metisUserProfile = persistorService.get(MetisUserProfile.class, currentUser.getUserId());
			if (metisUserProfile != null && metisUserProfile.getDefaultConnectionId() != null) {
				defaultConn = metisUserProfile.getDefaultConnectionId();
			} else {
				defaultConn = -1L;
			}
			currentUser.addOtherProfileInfo(METIS_DEFAULT_CONNECTION, defaultConn);
		}
		return defaultConn != -1 ? getDBConnection(defaultConn) : null;
	}

	@Override
	public void removeDefaultConnectionOfCurrentUser() {
		UserVO currentUser = securityService.getCurrentUser();
		MetisUserProfile metisUserProfile = persistorService.get(MetisUserProfile.class, currentUser.getUserId());
		if (metisUserProfile != null && metisUserProfile.getDefaultConnectionId() != null) {
			metisUserProfile.setDefaultConnection(null);
			persistorService.update(metisUserProfile);
			currentUser.removeOtherProfileInfo(METIS_DEFAULT_CONNECTION);
		}
	}

	// ------------------------------

	private Connection getConnection(Long dbConnId) {
		int retry = 0;
		Connection result = null;
		Exception last = null;
		while (retry < 3) {
			try {
				result = getUnsureConnection(dbConnId);
				break;
			} catch (Exception e) {
				logger.error("getUnsureConnection: " + dbConnId, e);
				last = e;
				retry++;
				closePoolSafely(dbConnId);
			}
		}

		if (result == null) {
			throw new MetisException(MetisErrorCode.DBConnection, String.format("%s (%s)", dbConnId, last), last);
		}

		return result;
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
