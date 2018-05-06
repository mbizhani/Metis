package org.devocative.metis.service.data;

import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.CalendarUtil;
import org.devocative.adroit.ConfigUtil;
import org.devocative.adroit.ObjectUtil;
import org.devocative.adroit.cache.ICache;
import org.devocative.adroit.cache.IMissedHitHandler;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.adroit.vo.RangeVO;
import org.devocative.adroit.xml.AdroitXStream;
import org.devocative.demeter.DLogCtx;
import org.devocative.demeter.iservice.ICacheService;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.iservice.persistor.EJoinMode;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.demeter.iservice.template.IStringTemplateService;
import org.devocative.demeter.iservice.template.TemplateEngineType;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.mapping.*;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataSourceRelation;
import org.devocative.metis.entity.data.EConnectionSelection;
import org.devocative.metis.entity.data.config.*;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.iservice.data.IDataSourceService;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.filter.data.DataSourceFVO;
import org.devocative.metis.vo.query.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service("mtsDataSourceService")
public class DataSourceService implements IDataSourceService, IMissedHitHandler<String, DataSource> {
	private static final Logger logger = LoggerFactory.getLogger(DataSourceService.class);

	private static final String EMBED_FILTER_EXPRESSION = "%FILTER_EXPR%";

	private XStream xStream;
	private ICache<String, DataSource> dataSourceCache;

	@Autowired
	private IDBConnectionService dbConnectionService;

	@Autowired
	private IPersistorService persistorService;

	@Autowired
	private ICacheService cacheService;

	@Autowired
	private ISecurityService securityService;

	@Autowired
	private IStringTemplateService stringTemplateService;

	// ------------------------------

	@PostConstruct
	public void initDataSourceService() {
		xStream = new AdroitXStream();
		xStream.processAnnotations(XDataSource.class);

		dataSourceCache = cacheService.create(CACHE_KEY, 70);
		dataSourceCache.setMissedHitHandler(this);
	}

	// ------------------------------

	@Override
	public DataSource load(String id) {
		return dataSourceCache.get(id);
	}

	@Override
	public DataSource loadByName(String name) {
		DataSource ds = dataSourceCache.findByProperty("name", name);
		if (ds == null) {
			ds = persistorService
				.createQueryBuilder()
				.addFrom(DataSource.class, "ent")
				.addJoin("cfg", "ent.config", EJoinMode.LeftFetch)
				.addJoin("con", "ent.connection", EJoinMode.LeftFetch)
				.addWhere("and ent.name = :name")
				.addParam("name", name)
				.object();
			ds.setXDataSource((XDataSource) xStream.fromXML(ds.getConfig().getValue()));
			dataSourceCache.put(ds.getId(), ds);
		}
		return ds;
	}

	// IMissedHitHandler
	@Override
	public DataSource loadForCache(String key) {
		DataSource ds = persistorService
			.createQueryBuilder()
			.addFrom(DataSource.class, "ent")
			.addJoin("cfg", "ent.config", EJoinMode.LeftFetch)
			.addJoin("con", "ent.connection", EJoinMode.LeftFetch)
			.addWhere("and ent.id = :id")
			.addParam("id", key)
			.object();

		ds.setXDataSource((XDataSource) xStream.fromXML(ds.getConfig().getValue()));

		return ds;
	}

	@Override
	public List<DataSource> list() {
		return persistorService.list(DataSource.class);
	}

	@Override
	public DataSource saveOrUpdate(DataVO dataVO) {
		String dataSourceId = dataVO.getDataSourceId();
		XDataSource xDataSource = dataVO.toXDataSource();

		DataSource dataSource;
		ConfigLob config;

		if (dataSourceId == null) {
			dataSource = new DataSource();
			config = new ConfigLob();
		} else {
			dataSource = load(dataSourceId);
			config = dataSource.getConfig();
		}

		dataSource.setName(xDataSource.getName());
		dataSource.setTitle(dataVO.getTitle());
		dataSource.setConnection(dbConnectionService.load(dataVO.getConnectionId()));
		dataSource.setConnectionSelection(dataVO.getConnectionSelection());

		List<DataSourceRelation> newRelations = new ArrayList<>();

		if (dataSource.getId() == null) {
			checkDuplicateDataSource(dataSource.getName());
		}

		dataSource.setKeyField(null);
		dataSource.setTitleField(null);
		dataSource.setSelfRelPointerField(null);

		// ---- Processing Fields
		for (XDSField xdsField : xDataSource.getFields()) {
			if (ObjectUtil.isTrue(xdsField.getIsKeyField())) {
				dataSource.setKeyField(xdsField.getName());
			}

			if (ObjectUtil.isTrue(xdsField.getIsTitleField())) {
				dataSource.setTitleField(xdsField.getName());
			}

			if (ObjectUtil.isTrue(xdsField.getIsSelfRelPointerField())) {
				dataSource.setSelfRelPointerField(xdsField.getName());
			}

			if (XDSFieldType.LookUp == xdsField.getType()) {
				DataSourceRelation rel = new DataSourceRelation();
				rel.setSource(dataSource);
				rel.setTarget(new DataSource().setId(xdsField.getTargetDSId()));
				rel.setSourcePointerField(xdsField.getName());

				newRelations.add(rel);
			} else {
				xdsField.setTargetDSId(null);
				xdsField.setTargetDSName(null);
				xdsField.setTargetDSMultipleSelection(null);
			}
		}

		// ---- Processing Parameters
		for (XDSParameter xdsParameter : xDataSource.getParams()) {
			if (XDSFieldType.LookUp == xdsParameter.getType()) {
				DataSourceRelation rel = new DataSourceRelation();
				rel.setSource(dataSource);
				rel.setTarget(new DataSource().setId(xdsParameter.getTargetDSId()));
				rel.setSourcePointerField(xdsParameter.getName());
				newRelations.add(rel);
			} else {
				xdsParameter.setTargetDSId(null);
				xdsParameter.setTargetDSName(null);
				xdsParameter.setTargetDSMultipleSelection(null);
			}
		}

		config.setValue(xStream.toXML(xDataSource));

		dataSource.setConfig(config);

		try {
			persistorService.startTrx();

			persistorService.saveOrUpdate(config);
			persistorService.saveOrUpdate(dataSource);

			persistorService.createQueryBuilder()
				.addSelect("delete from DataSourceRelation ent where ent.source.id = :dsId")
				.addParam("dsId", dataSource.getId())
				.update();
			newRelations.forEach(persistorService::saveOrUpdate);

			persistorService.commitOrRollback();
		} finally {
			//NOTE: it is important to remove, since it is in the middle of a trx and the result may not persist in DB
			dataSourceCache.remove(dataSource.getId());
		}

		return dataSource;
	}

	@Override
	public List<DataSource> search(DataSourceFVO filter, long pageIndex, long pageSize) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select ent")
			.addFrom(DataSource.class, "ent")
			.applyFilter(DataSource.class, "ent", filter)
			.list((pageIndex - 1) * pageSize, pageSize);
	}

	@Override
	public long count(DataSourceFVO filter) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select count(1)")
			.addFrom(DataSource.class, "ent")
			.applyFilter(DataSource.class, "ent", filter)
			.object();
	}

	// ---------------

	@Override
	public List<DataSource> getAllDataSourcesAsLookup() {
		return persistorService
			.createQueryBuilder()
			.addFrom(DataSource.class, "ent")
			.addWhere("and ent.keyField is not null")
			.list();
	}

	// ---------------

	@Override
	public String processQuery(Long dbConnId, String query, XDSQueryMode mode) {
		String finalQuery = null;

		switch (mode) {
			case Sql:
				finalQuery = query;
				break;
			case Eql:
				finalQuery = processEntityQuery(dbConnId, query).getSql();
				break;
			/*case SqlAndEql:
				throw new RuntimeException("Mode SqlAndEql not implemented!"); //TODO*/
			//break;
		}

		if (finalQuery.contains(EMBED_FILTER_EXPRESSION)) {
			finalQuery = finalQuery.replace(EMBED_FILTER_EXPRESSION, "1=1");
		}

		logger.debug("Process Query: FINAL = {}", finalQuery);
		return finalQuery;
	}

	@Override
	public EQLMetaDataVO processEntityQuery(Long dbConnId, String query) {
		XSchema xSchema = dbConnectionService.getSchemaOfMapping(dbConnId);

		if (xSchema == null) {
			throw new MetisException(MetisErrorCode.NoMappingForConnection, dbConnectionService.load(dbConnId).getName());
		}

		EQLMetaDataVO metaDataVO = new EQLMetaDataVO();
		metaDataVO.setEql(query);

		Map<String, XEntity> aliasToXEntityMap = new HashMap<>();

		StringBuffer tableReplacerBuffer = new StringBuffer();
		Pattern tablePattern = Pattern.compile("(from|join)\\s+(\\w+\\.)?(\\w+)(\\s+(\\w+))?", Pattern.CASE_INSENSITIVE);
		Matcher tableMatcher = tablePattern.matcher(query);
		while (tableMatcher.find()) {
			String schema = tableMatcher.group(2);
			if (schema != null) {
				//throw new MetisException(MetisErrorCode.SchemaInEql, schema);
				metaDataVO.addError(MetisErrorCode.SchemaInEql, schema);
			}

			String alias = tableMatcher.group(5);
			if (aliasToXEntityMap.containsKey(alias)) {
				//throw new MetisException(MetisErrorCode.DuplicateAlias, alias);
				metaDataVO.addError(MetisErrorCode.DuplicateAlias, alias);
			}
			String entity = tableMatcher.group(3);

			XEntity xEntity = xSchema.findEntity(entity);
			if (xEntity == null || xEntity.getTable() == null) {
				//throw new MetisException(MetisErrorCode.EntityWithoutMapping, entity);
				metaDataVO.addError(MetisErrorCode.EntityWithoutMapping, entity);
			} else {
				aliasToXEntityMap.put(alias, xEntity);
				String replacement = String.format("%s %s %s", tableMatcher.group(1), xEntity.getTable(), alias);
				tableMatcher.appendReplacement(tableReplacerBuffer, replacement);
			}
		}
		tableMatcher.appendTail(tableReplacerBuffer);

		StringBuffer joinCondReplacerBuffer = new StringBuffer();
		Pattern joinCondPattern = Pattern.compile("on\\s+(\\w+([.]\\w+)?)\\s*[~]\\s*(\\w+([.]\\w+)?)", Pattern.CASE_INSENSITIVE);
		Matcher joinCondMatcher = joinCondPattern.matcher(tableReplacerBuffer.toString());
		while (joinCondMatcher.find()) {
			String leftAlias = null;
			String leftColumn = null;
			String rightAlias = null;
			String rightColumn = null;

			boolean validJoin = true;
			try {
				if (joinCondMatcher.group(2) != null && joinCondMatcher.group(4) != null) {
					throw new MetisException(MetisErrorCode.EqlInvalidJoin, joinCondMatcher.group(0));
					//metaDataVO.addError(MetisErrorCode.EqlInvalidJoin, joinCondMatcher.group(0));
					//validJoin = false;
				} else if (joinCondMatcher.group(2) == null && joinCondMatcher.group(4) == null) { // one PK is FK to another one
					leftAlias = joinCondMatcher.group(1);
					leftColumn = findXEntity(aliasToXEntityMap, leftAlias).getId().getColumn();
					rightAlias = joinCondMatcher.group(3);
					rightColumn = findXEntity(aliasToXEntityMap, rightAlias).getId().getColumn();
				} else if (joinCondMatcher.group(2) != null) {
					String[] split = joinCondMatcher.group(1).split("\\.");
					leftAlias = split[0].trim();
					XAbstractProperty xAProp = findXEntity(aliasToXEntityMap, leftAlias).findProperty(split[1].trim());

					checkJoinProperty(xAProp, joinCondMatcher.group(1));

					if (xAProp instanceof XMany2One) {
						XMany2One xMany2One = (XMany2One) xAProp;
						leftColumn = xMany2One.getColumn();
						rightAlias = joinCondMatcher.group(3);
						rightColumn = findXEntity(aliasToXEntityMap, rightAlias).getId().getColumn();
					} else { // XOne2Many
						XOne2Many xOne2Many = (XOne2Many) xAProp;
						leftColumn = findXEntity(aliasToXEntityMap, leftAlias).getId().getColumn();
						rightAlias = joinCondMatcher.group(3);
						rightColumn = xOne2Many.getManySideColumn();
					}
				} else { //joinCondMatcher.group(4) != null
					String[] split = joinCondMatcher.group(3).split("\\.");
					rightAlias = split[0].trim();
					XAbstractProperty xAProp = findXEntity(aliasToXEntityMap, rightAlias).findProperty(split[1].trim());

					checkJoinProperty(xAProp, joinCondMatcher.group(3));

					if (xAProp instanceof XMany2One) {
						XMany2One xMany2One = (XMany2One) xAProp;
						rightColumn = xMany2One.getColumn();
						leftAlias = joinCondMatcher.group(1);
						leftColumn = findXEntity(aliasToXEntityMap, rightAlias).getId().getColumn();
					} else { // XOne2Many
						XOne2Many xOne2Many = (XOne2Many) xAProp;
						rightColumn = findXEntity(aliasToXEntityMap, rightAlias).getId().getColumn();
						leftAlias = joinCondMatcher.group(1);
						leftColumn = xOne2Many.getManySideColumn();
					}
				}
			} catch (MetisException e) {
				if (!MetisErrorCode.UnknownAlias.equals(e.getErrorCode())) {
					metaDataVO.addError((MetisErrorCode) e.getErrorCode(), e.getErrorParameter());
				}
				validJoin = false;
			}

			// Using ~ char instead of . to be ignored in the column replacement (next paragraph),
			// and then at the end the ~ replaced with .
			if (validJoin) {
				joinCondMatcher.appendReplacement(joinCondReplacerBuffer,
					String.format("on %s~%s=%s~%s", leftAlias, leftColumn, rightAlias, rightColumn));
			}
		}
		joinCondMatcher.appendTail(joinCondReplacerBuffer);

		StringBuffer columnReplacerBuffer = new StringBuffer();
		Pattern columnPattern = Pattern.compile("(\\w+)\\.(\\w+)", Pattern.CASE_INSENSITIVE);
		Matcher columnMatcher = columnPattern.matcher(joinCondReplacerBuffer.toString());
		while (columnMatcher.find()) {
			String alias = columnMatcher.group(1);
			String prop = columnMatcher.group(2);
			if (aliasToXEntityMap.containsKey(alias)) {
				XEntity xEntity = aliasToXEntityMap.get(alias);
				if (xEntity.findProperty(prop) == null) {
					//throw new MetisException(MetisErrorCode.EqlUnknownProperty, String.format("%s.%s", alias, prop));
					metaDataVO.addError(MetisErrorCode.EqlUnknownProperty, String.format("%s.%s", alias, prop));
				} else {
					XAbstractProperty xAProp = xEntity.findProperty(prop);
					if (xAProp instanceof XProperty) {
						XProperty xProperty = (XProperty) xAProp;
						String replacement = String.format("%s.%s", alias, xProperty.getColumn());
						columnMatcher.appendReplacement(columnReplacerBuffer, replacement);
					} else {
						//throw new MetisException(MetisErrorCode.EqlInvalidAssociationUsage, String.format("%s.%s", alias, prop));
						metaDataVO.addError(MetisErrorCode.EqlInvalidAssociationUsage, String.format("%s.%s", alias, prop));
					}
				}
			}
		}
		columnMatcher.appendTail(columnReplacerBuffer);

		metaDataVO.setAliasToXEntityMap(aliasToXEntityMap);
		metaDataVO.setSql(columnReplacerBuffer.toString().replace('~', '.'));
		return metaDataVO;
	}

	@Override
	public String processDynamicQuery(String text, Map<String, Object> params) {
		return (String) stringTemplateService.create(text, TemplateEngineType.FreeMarker).process(params);
	}

	// ---------------

	@Override
	public DsQueryRVO<List<Map<String, Object>>> execute(SelectQueryQVO queryQVO) {
		DataSource dataSource = load(queryQVO.getDataSourceId());
		XDataSource xDataSource = dataSource.getXDataSource();

		DSQueryBuilder queryBuilder = new DSQueryBuilder(xDataSource, queryQVO, dataSource.getSelfRelPointerField())
			.appendSelect(queryQVO.getSelectFields())
			.appendFrom()
			.appendWhere()
			.appendSort(queryQVO.getSortFields());

		Long dbConnId = findProperDBConnection(queryQVO.getSentDBConnection(), dataSource);

		String comment = String.format("DsExc[%s]", dataSource.getName());

		QueryExecInfoRVO beforeExecInfo = null;
		if (xDataSource.getQuery().getBefore() != null) {
			beforeExecInfo = dbConnectionService.execute(dbConnId, xDataSource.getQuery().getBefore(), "B4" + comment, queryBuilder.getQueryParams());
		}

		DsQueryRVO<List<Map<String, Object>>> list = dbConnectionService.executeQuery(
			dbConnId,
			processQuery(
				dbConnId,
				queryBuilder.getQuery(),
				xDataSource.getQuery().getMode()),
			comment,
			queryBuilder.getQueryParams(),
			queryQVO.getPagination()
		).toListOfMap();

		if (beforeExecInfo != null) {
			list.addQueryExecInfo(0, beforeExecInfo);
		}

		if (queryQVO.isConsiderParent() && dataSource.getSelfRelPointerField() != null) {
			Set<Object> parentIds = extractParentIds(dataSource.getSelfRelPointerField(), list.getResult());

			if (parentIds.size() > 0) {
				SelectQueryQVO selectQueryQVO = new SelectQueryQVO(
					queryQVO.getDataSourceId(),
					queryQVO.getSelectFields());
				selectQueryQVO
					.setSortFields(queryQVO.getSortFields())
					.setSentDBConnection(queryQVO.getSentDBConnection());
				list.getResult().addAll(findParentsToRoot(dataSource, selectQueryQVO, parentIds));
			}
		}

		return list;
	}

	@Override
	public DsQueryRVO<List<KeyValueVO<Serializable, String>>> executeLookUp(String dataSourceId, String targetDataSourceId, String sentDBConnection, Map<String, Object> filter) {
		DataSource dataSource = load(dataSourceId);
		DataSource targetDataSource = load(targetDataSourceId);

		XDataSource targetXDataSource = targetDataSource.getXDataSource();

		List<String> select = new ArrayList<>();
		select.add(targetDataSource.getKeyField());
		select.add(targetDataSource.getTitleField() != null ? targetDataSource.getTitleField() : targetDataSource.getKeyField());

		DSQueryBuilder builder = new DSQueryBuilder(targetXDataSource)
			.appendSelect(select)
			.appendFrom()
			.appendWhere(filter);

		Long dbConnId = findProperDBConnection(sentDBConnection, dataSource);
		String comment = String.format("DsLkUp[%s > %s]", dataSource.getName(), targetDataSource.getName());
		return dbConnectionService.executeQuery(
			dbConnId,
			processQuery(
				dbConnId,
				builder.getQuery(),
				targetXDataSource.getQuery().getMode()),
			comment,
			builder.getQueryParams(),
			PaginationQVO.byPage(1L, 50L)
		).toListOfKeyValues();
	}

	@Override
	public DsQueryRVO<List<Map<String, Object>>> executeOfParent(SelectQueryQVO queryQVO, Serializable parentId) {
		DataSource dataSource = load(queryQVO.getDataSourceId());
		XDataSource xDataSource = dataSource.getXDataSource();

		Map<String, Object> params = new HashMap<>();
		params.put("parentId", parentId);

		queryQVO
			.setFilterExpression(dataSource.getSelfRelPointerField() + " = :parentId")
			.setInputParams(params);

		DSQueryBuilder builder = new DSQueryBuilder(xDataSource, queryQVO, dataSource.getSelfRelPointerField())
			.appendSelect(queryQVO.getSelectFields())
			.appendFrom()
			.appendWhere()
			.appendSort(queryQVO.getSortFields());

		String comment = String.format("DsChld[%s]", dataSource.getName());
		Long dbConnId = findProperDBConnection(queryQVO.getSentDBConnection(), dataSource);

		return dbConnectionService.executeQuery(
			dbConnId,
			processQuery(
				dbConnId,
				builder.getQuery(),
				xDataSource.getQuery().getMode()),
			comment,
			params
		).toListOfMap();
	}

	@Override
	public DsQueryRVO<Long> execute(CountQueryQVO queryQVO) {
		DataSource dataSource = load(queryQVO.getDataSourceId());
		XDataSource xDataSource = dataSource.getXDataSource();

		DSQueryBuilder builderVO = new DSQueryBuilder(xDataSource, queryQVO, dataSource.getSelfRelPointerField())
			.appendSelect(Collections.singletonList("count(1) as cnt"))
			.appendFrom()
			.appendWhere();

		String main = builderVO.getQuery();

		Long dbConnId = findProperDBConnection(queryQVO.getSentDBConnection(), dataSource);

		String comment = String.format("DsCnt[%s]", dataSource.getName());

		DsQueryRVO<List<Map<String, Object>>> list = dbConnectionService.executeQuery(
			dbConnId,
			processQuery(
				dbConnId,
				main,
				xDataSource.getQuery().getMode()),
			comment,
			builderVO.queryParams
		).toListOfMap();

		return new DsQueryRVO<>(
			((BigDecimal) list.getResult().get(0).get("cnt")).longValue(),
			list.getQueryExecInfoList()
		);
	}

	@Override
	public DsQueryRVO<List<Map<String, Object>>> execute(AggregateQueryQVO queryQVO) {
		DataSource dataSource = load(queryQVO.getDataSourceId());
		XDataSource xDataSource = dataSource.getXDataSource();

		List<String> select = new ArrayList<>();
		for (Map.Entry<String, List<XDVAggregatorFunction>> entry : queryQVO.getSelectFields().entrySet()) {
			for (XDVAggregatorFunction function : entry.getValue()) {
				select.add(String.format("%2$s(%1$s) as %2$s___%1$s", entry.getKey(), function));
			}
		}

		DSQueryBuilder builderVO = new DSQueryBuilder(xDataSource, queryQVO, dataSource.getSelfRelPointerField())
			.appendSelect(select)
			.appendFrom()
			.appendWhere();

		String main = builderVO.getQuery();

		Long dbConnId = findProperDBConnection(queryQVO.getSentDBConnection(), dataSource);

		String comment = String.format("DsAgr[%s]", dataSource.getName());

		DsQueryRVO<List<Map<String, Object>>> list = dbConnectionService.executeQuery(
			dbConnId,
			processQuery(
				dbConnId,
				main,
				xDataSource.getQuery().getMode()),
			comment,
			builderVO.queryParams
		).toListOfMap();

		Map<String, Map<String, Object>> map = new LinkedHashMap<>();
		for (XDVAggregatorFunction function : XDVAggregatorFunction.values()) {
			map.put(function.name().toLowerCase(), new HashMap<String, Object>());
		}

		Map<String, Object> row = list.getResult().get(0);
		for (Map.Entry<String, Object> entry : row.entrySet()) {
			String[] parts = entry.getKey().split("___");
			String func = parts[0];
			String field = parts[1];
			map.get(func).put(field, entry.getValue());
		}

		List<Map<String, Object>> finalList = new ArrayList<>();
		for (Map.Entry<String, Map<String, Object>> entry : map.entrySet()) {
			if (entry.getValue().size() > 0) {
				if (dataSource.getKeyField() != null) {
					entry.getValue().put(dataSource.getKeyField(), -XDVAggregatorFunction.valueOf(entry.getKey()).ordinal() - 1);
				}
				if (dataSource.getTitleField() != null) {
					entry.getValue().put(dataSource.getTitleField(), entry.getKey());
				}
				finalList.add(entry.getValue());
			}
		}

		return new DsQueryRVO<>(finalList, list.getQueryExecInfoList());
	}

	@Override
	public List<QueryExecInfoRVO> executeAfterIfAny(String dsId, String sentDBConnection) {
		DataSource dataSource = load(dsId);
		XDataSource xDataSource = dataSource.getXDataSource();

		Long dbConnId = findProperDBConnection(sentDBConnection, dataSource);
		String comment = String.format("AF_DsExc[%s]", dataSource.getName());

		List<QueryExecInfoRVO> afterExecInfo = new ArrayList<>();
		if (xDataSource.getQuery().getAfter() != null) {
			String[] queries = xDataSource.getQuery().getAfter().split("[;]");
			for (String query : queries) {
				if (!query.isEmpty()) {
					afterExecInfo.add(dbConnectionService.execute(dbConnId, query, comment, new HashMap<String, Object>()));
				}
			}
		}

		return afterExecInfo;
	}

	// ------------------------------ PRIVATE METHODS

	private void checkDuplicateDataSource(String name) {
		long count = persistorService
			.createQueryBuilder()
			.addSelect("select count(1)")
			.addFrom(DataSource.class, "ent")
			.addWhere("and ent.name = :name")
			.addParam("name", name)
			.object();

		if (count > 0) {
			throw new MetisException(MetisErrorCode.DuplicateDataSourceName, name);
		}
	}

	private void checkJoinProperty(XAbstractProperty xAProp, String joinClause) {
		if (xAProp == null) {
			throw new MetisException(MetisErrorCode.EqlUnknownProperty, joinClause);
		}
		if (!(xAProp instanceof XMany2One) && !(xAProp instanceof XOne2Many)) {
			throw new MetisException(MetisErrorCode.EqlJoinOnProperty, joinClause);
		}
	}

	private Long findProperDBConnection(String sentDBConn, DataSource dataSource) {
		DLogCtx.put("connSelection", dataSource.getConnectionSelection());

		if (EConnectionSelection.THREE_STEPS.equals(dataSource.getConnectionSelection())) {
			if (sentDBConn != null) {
				logger.info("Sent DB Conn: User=[{}] Conn=[{}]", securityService.getCurrentUser(), sentDBConn);

				DBConnection dbConnection = dbConnectionService.findByName(sentDBConn);
				if (dbConnection == null) {
					logger.error("Invalid sent db connection: {}", sentDBConn);
					throw new MetisException(MetisErrorCode.InvalidDBConnection, sentDBConn);
				} else {
					return dbConnection.getId();
				}
			}

			DBConnection defaultConnection = dbConnectionService.getDefaultConnectionOfCurrentUser();
			return defaultConnection != null ? defaultConnection.getId() : dataSource.getConnectionId();
		} else if (EConnectionSelection.FIXED.equals(dataSource.getConnectionSelection())) {
			return dataSource.getConnectionId();
		} else {
			throw new RuntimeException(String.format("Invalid connection selection: dataSource=[%s] value=[%s]",
				dataSource.getName(), dataSource.getConnectionSelection()));
		}
	}

	private XEntity findXEntity(Map<String, XEntity> map, String alias) {
		if (!map.containsKey(alias)) {
			throw new MetisException(MetisErrorCode.UnknownAlias, alias);
		}
		return map.get(alias);
	}

	private Set<Object> extractParentIds(String selfRelPointerField, List<Map<String, Object>> list) {
		Set<Object> parentIds = new LinkedHashSet<>();
		for (Map<String, Object> map : list) {
			if (map.get(selfRelPointerField) != null) {
				parentIds.add(map.get(selfRelPointerField));
			}
		}
		return parentIds;
	}

	private List<Map<String, Object>> findParentsToRoot(DataSource dataSource,
														SelectQueryQVO queryQVO,
														Set<Object> parentIds) {

		XDataSource xDataSource = dataSource.getXDataSource();

		queryQVO.setFilterExpression(dataSource.getKeyField() + " in (:ids)");

		DSQueryBuilder builderVO = new DSQueryBuilder(xDataSource, queryQVO, dataSource.getSelfRelPointerField())
			.appendSelect(queryQVO.getSelectFields())
			.appendFrom()
			.appendWhere()
			.appendSort(queryQVO.getSortFields());

		Long dbConnId = findProperDBConnection(queryQVO.getSentDBConnection(), dataSource);

		String comment = String.format("DsPar[%s]", dataSource.getName());

		String query = builderVO.getQuery();
		Set<Object> visitedParents = new HashSet<>();
		List<Map<String, Object>> result = new ArrayList<>();

		while (parentIds.size() > 0) {
			Map<String, Object> queryParams = new HashMap<>();
			queryParams.put("ids", parentIds);

			List<Map<String, Object>> list = dbConnectionService.executeQuery(
				dbConnId,
				processQuery(
					dbConnId,
					query,
					xDataSource.getQuery().getMode()),
				comment,
				queryParams
			).toListOfMap().getResult(); //TODO

			result.addAll(list);

			visitedParents.addAll(parentIds);

			parentIds = extractParentIds(dataSource.getSelfRelPointerField(), list);
			parentIds.removeAll(visitedParents);
		}

		return result;
	}

	// ------------------------------ PRIVATE INNER CLASS

	private class DSQueryBuilder {
		private StringBuilder query = new StringBuilder();
		private Map<String, Object> queryParams = new HashMap<>();

		private XDataSource xDataSource;
		private AbstractQueryQVO queryQVO;
		private String selfRelationField;

		// ---------------

		public DSQueryBuilder(XDataSource xDataSource) {
			this(xDataSource, null, null);
		}

		public DSQueryBuilder(XDataSource xDataSource, AbstractQueryQVO queryQVO, String selfRelationField) {
			this.xDataSource = xDataSource;
			this.queryQVO = queryQVO;
			this.selfRelationField = selfRelationField;
		}

		// ---------------

		public String getQuery() {
			if (ObjectUtil.isTrue(xDataSource.getQuery().getDynamic())) {
				return processDynamicQuery(query.toString(), queryParams);
			} else {
				return query.toString();
			}
		}

		public Map<String, Object> getQueryParams() {
			return queryParams;
		}

		// ---------------

		public DSQueryBuilder appendSelect(List<String> selectFields) {
			query
				.append("select\n")
				.append("\t")
				.append(selectFields.get(0))
				.append("\n");

			for (int i = 1; i < selectFields.size(); i++) {
				query
					.append("\t,")
					.append(selectFields.get(i))
					.append("\n");
			}

			return this;
		}

		public DSQueryBuilder appendFrom() {
			/*Map<String, Object> params;
			if (queryQVO != null && queryQVO.getInputParams() != null) {
				params = queryQVO.getInputParams();
			} else {
				params = new HashMap<>();
			}*/

			query
				.append("from (\n")
				//.append(processDynamicQuery(xDataSource.getName(), xDataSource.getQuery(), params))
				.append(xDataSource.getQuery().getText())
				.append("\n)\n");
			return this;
		}

		public DSQueryBuilder appendWhere() {
			if (queryQVO == null) {
				throw new RuntimeException("No AbstractQueryQVO!");
			}

			StringBuilder whereClause = new StringBuilder();
			if (queryQVO.getFilterExpression() != null) {
				whereClause
					.append(queryQVO.getFilterExpression())
					.append("\n");

				List<String> paramsInQuery = NamedParameterStatement.findParamsInQuery(queryQVO.getFilterExpression(), true);
				if (queryQVO.getInputParams() != null) {
					for (String param : paramsInQuery) {
						queryParams.put(param, queryQVO.getInputParams().get(param));
					}
				}
			} else {
				whereClause.append("1=1\n");
			}

			whereClause.append(appendFilters(xDataSource, queryQVO.getInputParams()));

			if (selfRelationField != null && queryQVO.getFilterExpression() == null && queryQVO.getInputParams().size() == 0) {
				whereClause.append(" and ").append(selfRelationField).append(" is null");
			}

			int idx = query.indexOf(EMBED_FILTER_EXPRESSION);
			if (idx < 0) {
				query.append("where\n\t").append(whereClause);
			} else {
				query.replace(idx, idx + EMBED_FILTER_EXPRESSION.length(), whereClause.toString());
			}

			return this;
		}

		public DSQueryBuilder appendWhere(Map<String, Object> filter) {
			if (filter != null && filter.size() > 0) {
				query
					.append("where\n\t1=1\n")
					.append(appendFilters(xDataSource, filter));
			}
			return this;
		}

		public DSQueryBuilder appendSort(Map<String, String> sortFields) {
			if (sortFields != null && sortFields.size() > 0) {
				query.append("order by\n");
				boolean firstAdded = false;
				for (Map.Entry<String, String> sortField : sortFields.entrySet()) {
					//TODO check field
					if (firstAdded) {
						query.append(",\n");
					} else {
						firstAdded = true;
					}
					query
						.append("\t")
						.append(sortField.getKey())
						.append(" ")
						.append(sortField.getValue());
				}
				query.append("\n");
			}

			return this;
		}

		// ---------------

		private String appendFilters(XDataSource dataSource, Map<String, Object> filters) {
			StringBuilder filterClauses = new StringBuilder();
			boolean equalOnUpper = ConfigUtil.getBoolean(MetisConfigKey.UseEqualOnUpperBound);

			if (filters != null && filters.size() > 0) {
				for (Map.Entry<String, Object> filter : filters.entrySet()) {
					XDSField xdsField = dataSource.getField(filter.getKey());
					if (xdsField != null) {
						switch (xdsField.getFilterType()) {

							case Equal: // All types
								Object value = filter.getValue();
								if (value instanceof KeyValueVO) {
									value = ((KeyValueVO) value).getKey();
								}
								if (value instanceof Collection) { //TODO?
									filterClauses.append(String.format("\tand %1$s in (:%1$s)\n", xdsField.getName()));
								} else {
									filterClauses.append(String.format("\tand %1$s  = :%1$s\n", xdsField.getName()));
								}
								queryParams.put(xdsField.getName(), value);
								break;

							case Contain: // Only String
								if (xDataSource.getCaseSensitiveFilter() == null || !xDataSource.getCaseSensitiveFilter()) {
									filterClauses.append(String.format("\tand lower(%1$s) like lower(:%1$s)\n", xdsField.getName()));
								} else {
									filterClauses.append(String.format("\tand %1$s like :%1$s\n", xdsField.getName()));
								}
								queryParams.put(xdsField.getName(), filter.getValue());
								break;

							case Range: // Date & Number
								RangeVO rangeVO = (RangeVO) filter.getValue();
								if (rangeVO.getLower() != null) {
									filterClauses.append(String.format("\tand %1$s >= :%1$s_l\n", xdsField.getName()));
									queryParams.put(xdsField.getName() + "_l", rangeVO.getLower());
								}
								if (rangeVO.getUpper() != null) {
									if (equalOnUpper && xdsField.getType() != XDSFieldType.Date) {
										filterClauses.append(String.format("\tand %1$s <= :%1$s_u\n", xdsField.getName()));
									} else {
										filterClauses.append(String.format("\tand %1$s < :%1$s_u\n", xdsField.getName()));
									}

									Object upper;
									if (equalOnUpper && xdsField.getType() == XDSFieldType.Date) {
										Date upperDt = (Date) rangeVO.getUpper();
										upper = CalendarUtil.add(upperDt, Calendar.DATE, 1);
									} else {
										upper = rangeVO.getUpper();
									}
									queryParams.put(xdsField.getName() + "_u", upper);
								}
								break;

							case List: // All types (except boolean)
							case Search:
								if (filter.getValue() instanceof List) {
									filterClauses.append(String.format("\tand %1$s in (:%1$s)\n", xdsField.getName()));
									List<Serializable> items = new ArrayList<>();
									List<KeyValueVO<Serializable, String>> list = (List<KeyValueVO<Serializable, String>>) filter.getValue();
									for (KeyValueVO<Serializable, String> keyValue : list) {
										items.add(keyValue.getKey());
									}
									queryParams.put(xdsField.getName(), items);
								} else {
									filterClauses.append(String.format("\tand %1$s = :%1$s\n", xdsField.getName()));
									KeyValueVO<Serializable, String> keyValueVO = (KeyValueVO<Serializable, String>) filter.getValue();
									queryParams.put(xdsField.getName(), keyValueVO.getKey());
								}
								break;
						}
					}
				}

				for (XDSParameter xdsParameter : dataSource.getParams()) {
					if (filters.containsKey(xdsParameter.getName())) {
						Object value = filters.get(xdsParameter.getName());

						if (xdsParameter.getFilterType() != null) {
							switch (xdsParameter.getFilterType()) {
								case Equal:
									if (value instanceof KeyValueVO) {
										value = ((KeyValueVO) value).getKey();
									}
									break;
								case Contain:
									break;
								case Range:
									throw new RuntimeException("Invalid parameter as range: " + xdsParameter.getName());
								case List:
								case Search:
									if (value instanceof List) {
										List<Serializable> items = new ArrayList<>();
										List<KeyValueVO<Serializable, String>> list = (List<KeyValueVO<Serializable, String>>) value;
										for (KeyValueVO<Serializable, String> keyValue : list) {
											items.add(keyValue.getKey());
										}
										value = items;
									} else {
										value = ((KeyValueVO<Serializable, String>) value).getKey();
									}
									break;
							}
						}
						queryParams.put(xdsParameter.getName(), value);
					}
				}
			}

			return filterClauses.toString();
		}
	}
}
