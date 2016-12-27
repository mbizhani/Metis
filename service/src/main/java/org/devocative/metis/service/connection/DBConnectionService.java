package org.devocative.metis.service.connection;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.cache.ICache;
import org.devocative.adroit.cache.IMissedHitHandler;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.demeter.iservice.ApplicationLifecyclePriority;
import org.devocative.demeter.iservice.ICacheService;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.demeter.vo.UserVO;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.MetisUserProfile;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.entity.connection.mapping.XMany2One;
import org.devocative.metis.entity.connection.mapping.XOne2Many;
import org.devocative.metis.entity.connection.mapping.XProperty;
import org.devocative.metis.entity.connection.mapping.XSchema;
import org.devocative.metis.entity.data.config.XDSFieldFilterType;
import org.devocative.metis.entity.data.config.XDSFieldResultType;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.query.DbQueryRVO;
import org.devocative.metis.vo.query.PaginationQVO;
import org.devocative.metis.vo.query.QueryExecInfoRVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.sql.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Service("mtsDBConnectionService")
public class DBConnectionService implements IDBConnectionService {
	private static final Logger logger = LoggerFactory.getLogger(DBConnectionService.class);

	private static final List<Integer> STRING_TYPES = Arrays.asList(Types.VARCHAR, Types.CHAR, Types.NVARCHAR,
		Types.NCHAR, Types.LONGNVARCHAR, Types.CLOB, Types.NCLOB);

	private static final List<Integer> INTEGER_TYPES = Arrays.asList(Types.INTEGER, Types.BIGINT, Types.NUMERIC,
		Types.SMALLINT, Types.TINYINT);

	private static final List<Integer> REAL_TYPES = Arrays.asList(Types.DOUBLE, Types.FLOAT, Types.DECIMAL);

	private static final List<Integer> DATE_TYPES = Arrays.asList(Types.DATE, Types.TIME, Types.TIMESTAMP);

	private static final String METIS_DEFAULT_CONNECTION = "METIS_DEFAULT_CONNECTION";

	private XStream xstream;
	private ICache<Long, DBConnection> dbConnectionCache;
	private ICache<Long, XSchema> xSchemaCache;
	private final Map<Long, ComboPooledDataSource> CONNECTION_POOL_MAP = new ConcurrentHashMap<>();

	@Autowired
	private IPersistorService persistorService;

	@Autowired
	private ISecurityService securityService;

	@Autowired
	private ICacheService cacheService;

	// ------------------------------

	@PostConstruct
	public void initDBConnectionService() {
		xstream = new XStream();
		xstream.processAnnotations(XSchema.class);
		xstream.processAnnotations(XProperty.class);
		xstream.processAnnotations(XMany2One.class);
		xstream.processAnnotations(XOne2Many.class);

		dbConnectionCache = cacheService.create("MTS_DB_CONNECTION", 20);
		dbConnectionCache.setMissedHitHandler(new IMissedHitHandler<Long, DBConnection>() {
			@Override
			public DBConnection loadForCache(Long key) {
				return persistorService.get(DBConnection.class, key);
			}
		});

		xSchemaCache = cacheService.create("MTS_DB_X_SCHEMA", 5);
		xSchemaCache.setMissedHitHandler(new IMissedHitHandler<Long, XSchema>() {
			@Override
			public XSchema loadForCache(Long key) {
				String config = persistorService.createQueryBuilder()
					.addSelect("select ent.value")
					.addFrom(ConfigLob.class, "ent")
					.addWhere("and ent.id = :id")
					.addParam("id", key)
					.object();

				if (config != null) {
					return (XSchema) xstream.fromXML(config);
				}
				return null;
			}
		});
	}

	// ------------------------------ IApplicationLifecycle implementation

	@Override
	public void init() {
	}

	@Override
	public void shutdown() {
		closeAllPools();
	}

	@Override
	public ApplicationLifecyclePriority getLifecyclePriority() {
		return ApplicationLifecyclePriority.Medium;
	}

	// ------------------------------

	@Override
	public DBConnection load(Long id) {
		return dbConnectionCache.get(id);
	}

	@Override
	public List<DBConnection> search(long pageIndex, long pageSize) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DBConnection.class, "ent")
			.setOrderBy("ent.name")
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

			xSchemaCache.update(configLob.getId(), (XSchema) xstream.fromXML(mappingXML));
		}

		//TODO encrypt database password before save

		persistorService.saveOrUpdate(dbConnection);
		persistorService.commitOrRollback();

		dbConnectionCache.update(dbConnection.getId(), dbConnection);

		connectionChanged(dbConnection.getId());
	}

	@Override
	public List<DBConnection> list() {
		return persistorService.list(DBConnection.class);
	}

	@Override
	public DBConnection loadByName(String name) {
		DBConnection result = dbConnectionCache.findByProperty("name", name);
		if (result == null) {
			result = persistorService
				.createQueryBuilder()
				.addFrom(DBConnection.class, "ent")
				.addWhere("and ent.name=:name")
				.addParam("name", name)
				.object();
			if (result != null) {
				dbConnectionCache.put(result.getId(), result);
			}
		}
		return result;
	}

	// ---------------

	@Override
	public List<DataFieldVO> findFields(Long dbConnId, String sql, Map<String, Object> params) {
		List<DataFieldVO> result = new ArrayList<>();

		try (Connection connection = getConnection(dbConnId)) {
			NamedParameterStatement nps = new NamedParameterStatement(connection, sql, getSchemaForDB(dbConnId));
			nps.setFetchSize(1);
			nps.setParameters(params)
				.setIgnoreExtraPassedParam(true) //TODO
				.setIgnoreMissedParam(true); //TODO

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
		logger.info("Closing all db connection pools");

		for (Map.Entry<Long, ComboPooledDataSource> entry : CONNECTION_POOL_MAP.entrySet()) {
			try {
				entry.getValue().close();
				logger.info("DB connection pool closed: {}", entry.getKey());
			} catch (Exception e) {
				logger.warn("Closing db connection pool problem: {}", entry.getKey(), e);
			}
		}
	}

	// ---------------

	@Override
	public DbQueryRVO executeQuery(
		Long dbConnId,
		String query,
		String comment,
		Map<String, Object> params) {

		return executeQuery(dbConnId, query, comment, params, null);
	}

	@Override
	public DbQueryRVO executeQuery(
		Long dbConnId,
		String query,
		String comment,
		PaginationQVO pagination) {
		return executeQuery(dbConnId, query, comment, null, pagination);
	}

	// Main Method
	@Override
	public DbQueryRVO executeQuery(
		Long dbConnId,
		String query,
		String comment,
		Map<String, Object> params,
		PaginationQVO pagination) {

		NamedParameterStatement nps = null;
		DbQueryRVO result = new DbQueryRVO();

		String dbConnName = load(dbConnId).getName();
		result.getQueryExecInfo().setDbConnName(dbConnName);
		long start = System.currentTimeMillis();
		logger.info("Executing Query: Cmnt=[{}] User=[{}] Conn=[{}]",
			comment, securityService.getCurrentUser(), dbConnName);

		try (Connection connection = getConnection(dbConnId)) {
			query = String.format("/*%s*/ %s", comment, query);

			nps = new NamedParameterStatement(connection, query, getSchemaForDB(dbConnId));
			nps
				.setDateClassReplacement(Timestamp.class)
				.setIgnoreExtraPassedParam(true) //TODO
				.setIgnoreMissedParam(true); //TODO

			if (pagination != null) {
				nps
					.setFirstResult(pagination.getFirstResult())
					.setMaxResults(pagination.getMaxResults());
			}

			if (params != null) {
				nps.setParameters(params);
			}

			ResultSet rs = nps.executeQuery();
			ResultSetMetaData metaData = rs.getMetaData();

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

			logger.info("Executed Query: Cmnt=[{}] User=[{}] Conn=[{}] Dur=[{}] Res#=[{}]",
				comment, securityService.getCurrentUser(), load(dbConnId).getName(),
				result.getQueryExecInfo().getDuration(), result.getRows().size());

			result.getQueryExecInfo()
				.fromNamedParameterStatement(nps)
				.setDuration(System.currentTimeMillis() - start);

			return result;
		} catch (SQLException e) {
			logger.error("executeQuery: " + comment, e);
			result.getQueryExecInfo()
				.setException(e)
				.fromNamedParameterStatement(nps);

			throw new MetisException(MetisErrorCode.SQLExecution, e)
				.setExecInfoList(result.getQueryExecInfo());
		}
	}

	@Override
	public QueryExecInfoRVO execute(Long dbConnId,
									String query,
									String comment,
									Map<String, Object> params) {
		long start = System.currentTimeMillis();
		NamedParameterStatement nps = null;
		QueryExecInfoRVO execInfo = new QueryExecInfoRVO();

		try (Connection connection = getConnection(dbConnId)) {
			nps = new NamedParameterStatement(connection, query, getSchemaForDB(dbConnId));
			nps
				.setDateClassReplacement(Timestamp.class)
				.setIgnoreExtraPassedParam(true) //TODO
				.setIgnoreMissedParam(true); //TODO

			if (params != null) {
				nps.setParameters(params);
			}

			nps.executeQuery();

			execInfo.fromNamedParameterStatement(nps);
			execInfo.setDuration(System.currentTimeMillis() - start);

			return execInfo;
		} catch (SQLException e) {
			logger.error("executeQuery: " + comment, e);
			execInfo.setException(e);
			execInfo.fromNamedParameterStatement(nps);
			throw new MetisException(MetisErrorCode.SQLExecution, e)
				.setExecInfoList(execInfo);
		}
	}

	// ---------------

	@Override
	public XSchema getSchemaOfMapping(Long dbConnId) {
		XSchema result = null;
		Long safeConfigId = load(dbConnId).getSafeConfigId();
		if (safeConfigId != null) {
			result = xSchemaCache.get(safeConfigId);
		}
		return result;
	}

	@Override
	public boolean checkConnection(Long id) {
		DBConnection dbConnection = load(id);
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
	public void groupChanged(DBConnectionGroup group) {
		logger.info("DBConnection(s) updating: DBConnectionGroup changed = [{}]", group.getName());

		if (group.getConfig() != null) {
			XSchema xSchema = (XSchema) xstream.fromXML(group.getConfig().getValue());
			xSchemaCache.update(group.getConfig().getId(), xSchema);
		}

		List<Long> ids = persistorService
			.createQueryBuilder()
			.addSelect("select ent.id")
			.addFrom(DBConnection.class, "ent")
			.addWhere("and ent.group.id = :groupId")
			.addParam("groupId", group.getId())
			.list();

		for (Long id : ids) {
			connectionChanged(id);
		}
		logger.info(" DBConnectionGroup changed = [{}] => DBConnection(s) updated = [{}] ", group.getName(), ids.size());
	}

	@Override
	public void setDefaultConnectionForCurrentUser(Long id) {
		DBConnection dbConnection = load(id);
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
		return defaultConn != -1 ? load(defaultConn) : null;
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
				logger.error(String.format("Get Connection: Conn=[%s] User=[%s]",
					load(dbConnId).getName(), securityService.getCurrentUser()), e);
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
		DBConnection dbConnection = load(dbConnId);
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

	private String getSchemaForDB(Long dbConnId) {
		return load(dbConnId).getSchema();
	}

	private void connectionChanged(Long id) {
		closePoolSafely(id);
		logger.info("DBConnection changed: {}", load(id).getName());
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