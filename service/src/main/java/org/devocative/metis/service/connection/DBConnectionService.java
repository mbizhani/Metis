package org.devocative.metis.service.connection;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.ConfigUtil;
import org.devocative.adroit.StringEncryptorUtil;
import org.devocative.adroit.cache.ICache;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.adroit.sql.plugin.ObjectNavigationPlugin;
import org.devocative.adroit.sql.plugin.PaginationPlugin;
import org.devocative.adroit.sql.plugin.SchemaPlugin;
import org.devocative.adroit.sql.result.EColumnNameCase;
import org.devocative.adroit.sql.result.ResultSetProcessor;
import org.devocative.adroit.xml.AdroitXStream;
import org.devocative.demeter.DBConstraintViolationException;
import org.devocative.demeter.DLogCtx;
import org.devocative.demeter.DemeterConfigKey;
import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.ApplicationLifecyclePriority;
import org.devocative.demeter.iservice.ICacheService;
import org.devocative.demeter.iservice.IRequestLifecycle;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.iservice.persistor.EJoinMode;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.demeter.vo.UserVO;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.MetisUserProfile;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.DBConnectionAlias;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.entity.connection.EAliasMode;
import org.devocative.metis.entity.connection.mapping.XMany2One;
import org.devocative.metis.entity.connection.mapping.XOne2Many;
import org.devocative.metis.entity.connection.mapping.XProperty;
import org.devocative.metis.entity.connection.mapping.XSchema;
import org.devocative.metis.entity.data.config.XDSFieldFilterType;
import org.devocative.metis.entity.data.config.XDSFieldResultType;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.iservice.connection.IDBConnectionAliasService;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.filter.connection.DBConnectionFVO;
import org.devocative.metis.vo.query.DbQueryRVO;
import org.devocative.metis.vo.query.PaginationQVO;
import org.devocative.metis.vo.query.QueryExecInfoRVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import javax.transaction.Transactional;
import java.sql.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

@Service("mtsDBConnectionService")
public class DBConnectionService implements IDBConnectionService, IRequestLifecycle {
	private static final Logger logger = LoggerFactory.getLogger(DBConnectionService.class);

	private static final ThreadLocal<ConnectionInfo> currentConnection = new ThreadLocal<>();

	private static final List<Integer> STRING_TYPES = Arrays.asList(Types.VARCHAR, Types.CHAR, Types.NVARCHAR,
		Types.NCHAR, Types.LONGNVARCHAR, Types.CLOB, Types.NCLOB);

	private static final List<Integer> INTEGER_TYPES = Arrays.asList(Types.INTEGER, Types.BIGINT, Types.NUMERIC,
		Types.SMALLINT, Types.TINYINT);

	private static final List<Integer> REAL_TYPES = Arrays.asList(Types.DOUBLE, Types.FLOAT, Types.DECIMAL);

	private static final List<Integer> DATE_TYPES = Arrays.asList(Types.DATE, Types.TIME, Types.TIMESTAMP);

	private static final String METIS_DEFAULT_CONNECTION = "METIS_DEFAULT_CONNECTION";

	private XStream xstream;
	private ICache<Long, DBConnection> dbConnectionCache;
	private ICache<String, XSchema> xSchemaCache;
	private final Map<Long, ComboPooledDataSource> CONNECTION_POOL_MAP = new ConcurrentHashMap<>();
	private final Map<Long, AtomicInteger> CONNECTION_POOL_CLOSE_COUNTER_MAP = new ConcurrentHashMap<>();

	private ThreadPoolExecutor timeOutExecutor;

	// ---------------

	@Autowired
	private IPersistorService persistorService;

	@Autowired
	private ISecurityService securityService;

	@Autowired
	private ICacheService cacheService;

	@Autowired
	private IDBConnectionAliasService aliasService;

	// ------------------------------

	@PostConstruct
	public void initDBConnectionService() {
		xstream = new AdroitXStream();
		xstream.processAnnotations(XSchema.class);
		xstream.processAnnotations(XProperty.class);
		xstream.processAnnotations(XMany2One.class);
		xstream.processAnnotations(XOne2Many.class);

		dbConnectionCache = cacheService.create(CACHE_KEY_DB_CONNECTION, 20, key -> persistorService.createQueryBuilder()
			.addFrom(DBConnection.class, "ent")
			.addJoin("grp", "ent.group", EJoinMode.LeftFetch)
			.addWhere("and ent.id = :id")
			.addParam("id", key)
			.object());

		xSchemaCache = cacheService.create(CACHE_KEY_X_SCHEMA, 5, key -> {
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
		});
	}

	// ------------------------------ IApplicationLifecycle implementation

	@Override
	public void init() {
		if (ConfigUtil.getBoolean(MetisConfigKey.DatabaseCheckTimeoutEnabled)) {
			timeOutExecutor = new ThreadPoolExecutor(
				ConfigUtil.getInteger(MetisConfigKey.DatabaseCheckTimeoutMin),
				ConfigUtil.getInteger(MetisConfigKey.DatabaseCheckTimeoutMax),
				1, TimeUnit.MINUTES,
				new ArrayBlockingQueue<>(ConfigUtil.getInteger(MetisConfigKey.DatabaseCheckTimeoutList)));

			logger.info("DBConnectionService -> TimeOutExecutor Created.");
		}
	}

	@Override
	public void shutdown() {
		closeAllPools();

		if (timeOutExecutor != null) {
			timeOutExecutor.shutdown();
		}
	}

	@Override
	public ApplicationLifecyclePriority getLifecyclePriority() {
		return ApplicationLifecyclePriority.Third;
	}

	// ------------------------------ IRequestLifecycle

	@Override
	public void beforeRequest() {
	}

	@Override
	public void afterResponse() {
		closeCurrentConnection();
	}

	// ------------------------------

	@Transactional
	@Override
	public void saveOrUpdate(DBConnection entity) {
		try {
			persistorService.saveOrUpdate(entity);
		} catch (DBConstraintViolationException e) {
			if (e.isConstraint(DBConnection.UQ_CONST)) {
				throw new MetisException(MetisErrorCode.DuplicateDBConnectionName, entity.getName());
			}
		}

		DBConnectionAlias alias = aliasService.loadByConnMode(entity.getId(), EAliasMode.NORMAL);
		if (alias == null) {
			alias = new DBConnectionAlias();
			alias.setName(entity.getName());
			alias.setMode(EAliasMode.NORMAL);
			alias.setConnection(entity);
			aliasService.saveOrUpdate(alias);
		} else if (!alias.getName().equals(entity.getName())) {
			logger.error("Invalid alias to connection: alias={} conn={}", alias.getName(), entity.getName());
			alias.setName(entity.getName());
			aliasService.saveOrUpdate(alias);
		}
	}

	@Override
	public DBConnection load(Long id) {
		return dbConnectionCache.get(id);
	}

	@Override
	public DBConnection loadByName(String name) {
		DBConnection result = dbConnectionCache.findByProperty("name", name);

		if (result == null) {
			result = persistorService
				.createQueryBuilder()
				.addFrom(DBConnection.class, "ent")
				.addJoin("grp", "ent.group", EJoinMode.LeftFetch)
				.addWhere("and ent.name = :name")
				.addParam("name", name)
				.object();
			if (result != null) {
				dbConnectionCache.put(result.getId(), result);
			}
		}
		return result;
	}

	@Override
	public List<DBConnection> list() {
		return persistorService.list(DBConnection.class);
	}

	@Override
	public List<DBConnection> search(DBConnectionFVO filter, long pageIndex, long pageSize) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select ent")
			.addFrom(DBConnection.class, "ent")
			.applyFilter(DBConnection.class, "ent", filter)
			.list((pageIndex - 1) * pageSize, pageSize);
	}

	@Override
	public long count(DBConnectionFVO filter) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select count(1)")
			.addFrom(DBConnection.class, "ent")
			.applyFilter(DBConnection.class, "ent", filter)
			.object();
	}

	@Override
	public List<DBConnectionGroup> getGroupList() {
		return persistorService.list(DBConnectionGroup.class);
	}

	@Override
	public List<User> getCreatorUserList() {
		return persistorService.list(User.class);
	}

	@Override
	public List<User> getModifierUserList() {
		return persistorService.list(User.class);
	}

	// ==============================

	@Transactional
	@Override
	public void saveOrUpdate(DBConnection dbConnection, String mappingXML, String password) {
		try {
			if (password != null) {
				if (ConfigUtil.getBoolean(MetisConfigKey.ConnectionEncryptPassword)) {
					password = StringEncryptorUtil.encrypt(password);
				}
				dbConnection.setPassword(password);
			}

			if (ConfigUtil.getBoolean(MetisConfigKey.ConnectionCheckUserPassOnSave)) {
				try (Connection simpleConnection = createSimpleConnection(dbConnection)) {
					Statement statement = simpleConnection.createStatement();
					statement.executeQuery(dbConnection.getSafeTestQuery());
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}

			if (mappingXML != null) {
				ConfigLob configLob = dbConnection.getConfigId() == null ?
					new ConfigLob() :
					persistorService.get(ConfigLob.class, dbConnection.getConfigId());
				configLob.setValue(mappingXML);
				persistorService.saveOrUpdate(configLob);
				dbConnection.setConfig(configLob);

				xSchemaCache.remove(configLob.getId());
			}

			saveOrUpdate(dbConnection);
		} finally {
			dbConnectionCache.remove(dbConnection.getId());
			connectionChanged(dbConnection);
		}
	}

	// ---------------

	@Override
	public List<DataFieldVO> findFields(Long dbConnId, String sql, Map<String, Object> params) {
		List<DataFieldVO> result = new ArrayList<>();

		try {
			Connection connection = getConnection(dbConnId);

			NamedParameterStatement nps = new NamedParameterStatement(connection, sql);
			nps.setFetchSize(1);
			nps.setParameters(params)
				.setIgnoreExtraPassedParam(true) //TODO
				.setIgnoreMissedParam(true); //TODO

			nps.addPlugin(new ObjectNavigationPlugin());
			if (getSchemaForDB(dbConnId) != null) {
				nps.addPlugin(new SchemaPlugin(getSchemaForDB(dbConnId)));
			}

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
		logger.info("Closing All DBConnection Pools, size=[{}]", CONNECTION_POOL_MAP.size());

		synchronized (CONNECTION_POOL_MAP) {
			for (Map.Entry<Long, ComboPooledDataSource> entry : CONNECTION_POOL_MAP.entrySet()) {
				final String name = load(entry.getKey()).getName();
				try {
					logger.info("DBConnection Pool Closing: id=[{}] name=[{}]", entry.getKey(), name);
					entry.getValue().close();
					logger.info("DBConnection Pool Closed: id=[{}] name=[{}]", entry.getKey(), name);
				} catch (Exception e) {
					logger.warn("Closing DBConnection Pool Problem: id=[{}] name=[{}]", entry.getKey(), name, e);
				}
			}

			CONNECTION_POOL_MAP.clear();
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
		String dbConnName = load(dbConnId).getName();

		DbQueryRVO result = new DbQueryRVO();
		result.getQueryExecInfo().setDbConnName(dbConnName);

		long start = System.currentTimeMillis();
		logger.info("Executing Query: Cmnt=[{}] User=[{}] Conn=[{}]",
			comment, securityService.getCurrentUser(), dbConnName);

		DLogCtx
			.put("dbConnName", dbConnName)
			.put("sqlComment", comment);

		try {
			Connection connection = getConnection(dbConnId);
			query = String.format("/*%s*/ %s", comment, query);

			nps = new NamedParameterStatement(connection, query);
			nps
				.setDateClassReplacement(Timestamp.class)
				.setIgnoreExtraPassedParam(true) //TODO
				.setIgnoreMissedParam(true); //TODO

			nps.addPlugin(new ObjectNavigationPlugin());
			if (getSchemaForDB(dbConnId) != null) {
				nps.addPlugin(new SchemaPlugin(getSchemaForDB(dbConnId)));
			}

			if (pagination != null) {
				nps.addPlugin(PaginationPlugin.of(
					connection,
					pagination.getFirstResult(),
					pagination.getMaxResults()
				));
			}

			if (params != null) {
				nps.setParameters(params);
			}

			ResultSet rs = nps.executeQuery();
			result
				.setQueryVO(ResultSetProcessor.process(rs, EColumnNameCase.LOWER))
				.getQueryExecInfo()
				.fromNamedParameterStatement(nps)
				.setDuration(System.currentTimeMillis() - start);

			logger.info("Executed Query: Cmnt=[{}] User=[{}] Conn=[{}] Dur=[{}] Res#=[{}]",
				comment, securityService.getCurrentUser(), dbConnName,
				result.getQueryExecInfo().getDuration(), result.getRows().size());

			return result;
		} catch (Exception e) {
			logger.error("Execute Query: Cmnt=[{}] User=[{}] Conn=[{}] Query=[{}]",
				comment, securityService.getCurrentUser(), dbConnName, query, e);

			result.getQueryExecInfo()
				.setException(e)
				.fromNamedParameterStatement(nps);

			throw new MetisException(MetisErrorCode.SQLExecution, e)
				.setExecInfoList(result.getQueryExecInfo());
		} finally {
			DLogCtx
				.remove("dbConnName")
				.remove("sqlComment")
				.remove("connSelection"); // this key is set in DataSourceService.findProperDBConnection()
		}
	}

	@Override
	public QueryExecInfoRVO execute(Long dbConnId,
									String query,
									String comment,
									Map<String, Object> params) {
		long start = System.currentTimeMillis();

		NamedParameterStatement nps = null;

		String dbConnName = load(dbConnId).getName();

		QueryExecInfoRVO execInfo = new QueryExecInfoRVO();
		execInfo.setDbConnName(dbConnName);

		logger.info("Executing SQL: Cmnt=[{}] User=[{}] Conn=[{}]",
			comment, securityService.getCurrentUser(), dbConnName);

		try {
			Connection connection = getConnection(dbConnId);
			nps = new NamedParameterStatement(connection, query);
			nps
				.setDateClassReplacement(Timestamp.class)
				.setIgnoreExtraPassedParam(true) //TODO
				.setIgnoreMissedParam(true); //TODO

			nps.addPlugin(new ObjectNavigationPlugin());
			if (getSchemaForDB(dbConnId) != null) {
				nps.addPlugin(new SchemaPlugin(getSchemaForDB(dbConnId)));
			}

			if (params != null) {
				nps.setParameters(params);
			}

			nps.executeQuery();

			execInfo
				.fromNamedParameterStatement(nps)
				.setDuration(System.currentTimeMillis() - start);

			logger.info("Executed SQL: Cmnt=[{}] User=[{}] Conn=[{}] Dur=[{}]",
				comment, securityService.getCurrentUser(), dbConnName,
				execInfo.getDuration());

			return execInfo;
		} catch (SQLException e) {
			logger.error("Execute SQL: Cmnt=[{}] User=[{}] Conn=[{}] Query=[{}]",
				comment, securityService.getCurrentUser(), dbConnName, query, e);

			execInfo
				.setException(e)
				.fromNamedParameterStatement(nps);

			throw new MetisException(MetisErrorCode.SQLExecution, e)
				.setExecInfoList(execInfo);
		}
	}

	// ---------------

	@Override
	public XSchema getSchemaOfMapping(Long dbConnId) {
		XSchema result = null;
		String safeConfigId = load(dbConnId).getSafeConfigId();
		if (safeConfigId != null) {
			result = xSchemaCache.get(safeConfigId);
		}
		return result;
	}

	@Override
	public boolean checkConnection(Long id) {
		DBConnection dbConnection = load(id);
		if (dbConnection.getSafeTestQuery() != null) {
			try {
				Connection connection = getConnection(id);
				Statement st = connection.createStatement();
				st.executeQuery(dbConnection.getSafeTestQuery());
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
			xSchemaCache.remove(group.getConfig().getId());
		}

		List<DBConnection> connections = persistorService
			.createQueryBuilder()
			.addFrom(DBConnection.class, "ent")
			.addWhere("and ent.group.id = :groupId")
			.addParam("groupId", group.getId())
			.list();

		connections.forEach(this::connectionChanged);
		logger.info(" DBConnectionGroup changed = [{}] => DBConnection(s) updated = [{}] ", group.getName(), connections.size());
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
		Long defaultConn = (Long) currentUser.getOtherDataByKey(METIS_DEFAULT_CONNECTION);
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

	@Override
	public void updateCustomParam1(Map<String, String> params) {
		try {
			persistorService.startTrx();
			for (Map.Entry<String, String> entry : params.entrySet()) {
				persistorService
					.createQueryBuilder()
					.addSelect("update DBConnection ent set ent.customParam1 = :param where ent.name = :name")
					.addParam("name", entry.getKey())
					.addParam("param", entry.getValue())
					.update();
			}
			persistorService.commitOrRollback();
		} finally {
			dbConnectionCache.clear();
		}
	}

	@Override
	public DBConnection findByName(String name) {
		DBConnectionAlias alias = aliasService.loadByNameMode(name, EAliasMode.NORMAL);
		if (alias != null) {
			return load(alias.getConnectionId());
		}

		logger.warn("No alias for DBConnection: {}", name);

		return loadByName(name);
	}

	@Override
	public String getPoolInfo(Long id) {
		String result = "-";
		if (CONNECTION_POOL_MAP.containsKey(id)) {
			final ComboPooledDataSource cpds = CONNECTION_POOL_MAP.get(id);
			try {
				result = String.format("%s/%s - %s", cpds.getNumBusyConnections(), cpds.getNumConnections(), cpds.getNumIdleConnections());
			} catch (SQLException e) {
				logger.error("getPoolInfo", e);
				result = "Err";
			}
		}

		if (CONNECTION_POOL_CLOSE_COUNTER_MAP.containsKey(id)) {
			result += " #" + CONNECTION_POOL_CLOSE_COUNTER_MAP.get(id).get();
		}
		return result;
	}

	// ------------------------------

	private Connection getConnection(long dbConnId) {
		ConnectionInfo info = currentConnection.get();
		if (info != null) {
			if (info.dbConnId == dbConnId) {
				logger.debug("Getting Current Connection: {}", dbConnId);
				return info.connection;
			} else {
				logger.warn("Different DBConnection: requested=[{}] current=[{}]", dbConnId, info.dbConnId);
				closeCurrentConnection();
			}
		}

		DBConnection dbConnection = load(dbConnId);

		int noOfRetry = 0;
		Connection result = null;
		Exception last = null;
		while (noOfRetry < 3) {
			try {
				result = getUnsureConnection(dbConnection);
				break;
			} catch (Exception e) {
				logger.error("Get Connection: Conn=[{}] User=[{}]",
					dbConnection.getName(), securityService.getCurrentUser(), e);
				last = e;
				noOfRetry++;
				closePoolSafely(dbConnection);
			}
		}

		if (result == null) {
			throw new MetisException(MetisErrorCode.DBConnection, String.format("name = %s (err = %s)",
				dbConnection.getName(), last != null ? last.getMessage() : "UNKNOWN"), last);
		}

		logger.debug("Setting current connection: {}", dbConnId);
		currentConnection.set(new ConnectionInfo(dbConnId, result));

		return result;
	}

	private Connection getUnsureConnection(DBConnection dbConnection) throws Exception {
		if (!CONNECTION_POOL_MAP.containsKey(dbConnection.getId())) {
			ComboPooledDataSource cpds = new ComboPooledDataSource();
			cpds.setDriverClass(dbConnection.getSafeDriver());
			cpds.setJdbcUrl(dbConnection.getSafeUrl());
			cpds.setUser(dbConnection.getUsername());
			cpds.setPassword(getPasswordOf(dbConnection));

			CONNECTION_POOL_MAP.put(dbConnection.getId(), cpds);
		}

		Connection connection = CONNECTION_POOL_MAP.get(dbConnection.getId()).getConnection();
		if (dbConnection.getSafeTestQuery() != null) {
			if (ConfigUtil.getBoolean(MetisConfigKey.DatabaseCheckTimeoutEnabled)) {
				final Future<Boolean> submit = timeOutExecutor.submit(new TimeOut(connection, dbConnection.getSafeTestQuery()));
				final Boolean result = submit.get(ConfigUtil.getInteger(DemeterConfigKey.DatabaseCheckTimeoutDur), TimeUnit.SECONDS);
				if (!result) {
					throw new RuntimeException("Checking-Query Timeout!");
				}

			} else {
				Statement st = connection.createStatement();
				st.executeQuery(dbConnection.getSafeTestQuery());
				st.close();
			}
		}
		return connection;
	}

	private String getSchemaForDB(Long dbConnId) {
		return load(dbConnId).getSchema();
	}

	private void connectionChanged(DBConnection dbConnection) {
		closePoolSafely(dbConnection);
		logger.info("DBConnection Changed: id=[{}] name=[{}]", dbConnection.getName(), dbConnection.getId());
	}

	private void closePoolSafely(DBConnection dbConnection) {
		synchronized (CONNECTION_POOL_MAP) {
			Long dbConnId = dbConnection.getId();

			if (dbConnId != null && CONNECTION_POOL_MAP.containsKey(dbConnId)) {
				logger.warn("ClosePoolSafely: id=[{}] name=[{}] info=[{}]",
					dbConnection.getId(), dbConnection.getName(), getPoolInfo(dbConnId));

				if (!CONNECTION_POOL_CLOSE_COUNTER_MAP.containsKey(dbConnId)) {
					CONNECTION_POOL_CLOSE_COUNTER_MAP.put(dbConnId, new AtomicInteger(0));
				}
				CONNECTION_POOL_CLOSE_COUNTER_MAP.get(dbConnId).incrementAndGet();

				ComboPooledDataSource pool = CONNECTION_POOL_MAP.get(dbConnId);
				CONNECTION_POOL_MAP.remove(dbConnId);
				dbConnectionCache.remove(dbConnId);

				if (pool != null) {
					pool.close();
				}
			}
		}
	}

	private void closeCurrentConnection() {
		ConnectionInfo info = currentConnection.get();

		try {
			if (info != null) {
				if (!info.connection.isClosed()) {
					logger.debug("Closing current connection: {}", info.dbConnId);
					info.connection.close();
				} else {
					logger.warn("There is current connection, but not closed, dbConnId=[{}]", info.dbConnId);
				}
			}
		} catch (SQLException e) {
			logger.warn("DBConnectionService.closeCurrentConnection: ", e);
		}

		currentConnection.remove();
	}

	private Connection createSimpleConnection(DBConnection dbConnection) throws ClassNotFoundException, SQLException {
		Class.forName(dbConnection.getSafeDriver());

		return DriverManager.getConnection(dbConnection.getSafeUrl(), dbConnection.getUsername(), getPasswordOf(dbConnection));
	}

	private String getPasswordOf(DBConnection dbConnection) {
		return ConfigUtil.getBoolean(MetisConfigKey.ConnectionEncryptPassword) ?
			StringEncryptorUtil.decrypt(dbConnection.getPassword()) :
			dbConnection.getPassword();
	}

	// ------------------------------

	private class ConnectionInfo {
		private long dbConnId;
		private Connection connection;

		private ConnectionInfo(long dbConnId, Connection connection) {
			this.dbConnId = dbConnId;
			this.connection = connection;
		}
	}

	// ------------------------------

	private class TimeOut implements Callable<Boolean> {
		private Connection connection;
		private String checkQuery;

		private TimeOut(Connection connection, String checkQuery) {
			this.connection = connection;
			this.checkQuery = checkQuery;
		}

		@Override
		public Boolean call() {
			try {
				Statement st = connection.createStatement();
				st.executeQuery(checkQuery);
				st.close();

				return true;
			} catch (Exception e) {
				logger.error("Check-Query Timeout Thread Exception: {}", e.getMessage());
			}

			return false;
		}
	}
}
