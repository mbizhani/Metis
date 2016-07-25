package org.devocative.metis.service;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.core.util.QuickWriter;
import com.thoughtworks.xstream.io.xml.PrettyPrintWriter;
import freemarker.template.Configuration;
import freemarker.template.Template;
import org.devocative.adroit.CalendarUtil;
import org.devocative.adroit.ObjectUtil;
import org.devocative.adroit.cache.ICache;
import org.devocative.adroit.cache.IMissedHitHandler;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.adroit.vo.RangeVO;
import org.devocative.demeter.iservice.ICacheService;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.iservice.persistor.EJoinMode;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.mapping.*;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataSourceRelation;
import org.devocative.metis.entity.data.config.*;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.vo.filter.DataSourceFVO;
import org.devocative.metis.vo.query.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.io.Serializable;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service("mtsDataSourceService")
public class DataSourceService implements IDataSourceService, IMissedHitHandler<String, DataSource> {
	private static final Logger logger = LoggerFactory.getLogger(DataSourceService.class);

	private XStream xstream;
	private Configuration freeMarkerCfg;
	private ICache<String, DataSource> dataSourceCache;

	@Autowired
	private IDBConnectionService dbConnectionService;

	@Autowired
	private IPersistorService persistorService;

	@Autowired
	private ICacheService cacheService;

	@Autowired
	private ISecurityService securityService;

	// ------------------------------

	@PostConstruct
	public void initDataSourceService() {
		xstream = new XStream();
		xstream.processAnnotations(XDataSource.class);

		freeMarkerCfg = new Configuration(Configuration.VERSION_2_3_23);

		dataSourceCache = cacheService.create("MTS_DATA_SOURCE", 50);
		dataSourceCache.setMissedHitHandler(this);
	}

	// ------------------------------

	@Override
	public DataSource load(Long id) {
		DataSource ds = dataSourceCache.findByProperty("id", id);
		if (ds == null) {
			ds = persistorService
				.createQueryBuilder()
				.addFrom(DataSource.class, "ent")
				.addJoin("cfg", "ent.config", EJoinMode.LeftFetch)
				.addWhere("and ent.id = :id")
				.addParam("id", id)
				.object();
			dataSourceCache.put(ds.getName(), ds);
		}
		return ds;
	}

	@Override
	public DataSource loadByName(String name) {
		return dataSourceCache.get(name);
	}

	// IMissedHitHandler
	@Override
	public DataSource loadForCache(String key) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DataSource.class, "ent")
			.addJoin("cfg", "ent.config", EJoinMode.LeftFetch)
			.addWhere("and ent.name = :name")
			.addParam("name", key)
			.object();
	}

	@Override
	public List<DataSource> list() {
		return persistorService.list(DataSource.class);
	}

	@Override
	public DataSource saveOrUpdate(Long dataSourceId, Long dbConnId, String title, XDataSource xDataSource) {
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
		dataSource.setTitle(title);
		dataSource.setConnection(dbConnectionService.load(dbConnId));

		Map<String, DataSourceRelation> relationsMap = new HashMap<>();
		List<DataSourceRelation> newRelations = new ArrayList<>();

		if (dataSource.getId() != null) {
			markRelationsAsDeleted(dataSource.getId());
			loadRelationsToMap(dataSource.getId(), relationsMap);
		} else {
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
				DataSourceRelation rel = relationsMap.get(xdsField.getName());
				if (rel == null) {
					rel = new DataSourceRelation();
				}
				rel.setSource(dataSource);
				rel.setTarget(new DataSource(xdsField.getTargetDSId()));
				rel.setSourcePointerField(xdsField.getName());
				rel.setDeleted(false);

				newRelations.add(rel);
			}
		}

		// ---- Processing Parameters
		for (XDSParameter xdsParameter : xDataSource.getParams()) {
			if (XDSFieldType.LookUp == xdsParameter.getType()) {
				DataSourceRelation rel = relationsMap.get(xdsParameter.getName());
				if (rel == null) {
					rel = new DataSourceRelation();
				}
				rel.setSource(dataSource);
				rel.setTarget(new DataSource(xdsParameter.getTargetDSId()));
				rel.setSourcePointerField(xdsParameter.getName());
				rel.setDeleted(false);
				newRelations.add(rel);
			}
		}

		String query = xDataSource.getQuery().getText().trim();
		if (!query.startsWith("\n<![CDATA[\n")) {
			xDataSource.getQuery().setText(String.format("\n<![CDATA[\n%s\n]]>\n", query));
		}

		StringWriter writer = new StringWriter();
		xstream.marshal(xDataSource, new MyWriter(writer));
		config.setValue(writer.toString());

		dataSource.setConfig(config);

		persistorService.saveOrUpdate(config);
		persistorService.saveOrUpdate(dataSource);
		for (DataSourceRelation relation : newRelations) {
			persistorService.saveOrUpdate(relation);
		}

		dataSourceCache.update(dataSource.getName(), dataSource);

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

	@Override
	public XDataSource getXDataSource(DataSource dataSource) {
		return (XDataSource) xstream.fromXML(dataSource.getConfig().getValue());
	}

	@Override
	public XDataSource getXDataSource(String name) {
		DataSource dataSource = loadByName(name);
		return getXDataSource(dataSource);
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
			if (joinCondMatcher.group(2) != null && joinCondMatcher.group(4) != null) {
				//throw new MetisException(MetisErrorCode.EqlInvalidJoin, joinCondMatcher.group(0));
				metaDataVO.addError(MetisErrorCode.EqlInvalidJoin, joinCondMatcher.group(0));
				validJoin = false;
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

	// ---------------

	@Override
	public List<Map<String, Object>> execute(SelectQueryQVO queryQVO) {
		/*logger.info("Executing DataSource: DS=[{}] Usr=[{}]",
			queryQVO.getDataSourceName(), securityService.getCurrentUser());
		long start = System.currentTimeMillis();*/

		DataSource dataSource = loadByName(queryQVO.getDataSourceName());
		XDataSource xDataSource = getXDataSource(dataSource);

		DSQueryBuilder queryBuilder = new DSQueryBuilder(xDataSource, queryQVO)
			.appendSelect(queryQVO.getSelectFields())
			.appendFrom()
			.appendWhere()
			.appendSort(queryQVO.getSortFields());

		Long dbConnId = findProperDBConnection(queryQVO.getSentDBConnection(), dataSource);

		String comment = String.format("DsExc[%s]", dataSource.getName());

		List<Map<String, Object>> list = dbConnectionService.executeQuery(
			dbConnId,
			processQuery(
				dbConnId,
				queryBuilder.getQuery().toString(),
				xDataSource.getQuery().getMode()),
			comment,
			queryBuilder.getQueryParams(),
			queryQVO.getPageIndex(),
			queryQVO.getPageSize()
		).toListOfMap();

		if (dataSource.getSelfRelPointerField() != null) {
			List<Object> parentIds = extractParentIds(dataSource.getSelfRelPointerField(), list);

			if (parentIds.size() > 0) {
				SelectQueryQVO selectQueryQVO = new SelectQueryQVO(
					queryQVO.getDataSourceName(),
					queryQVO.getSelectFields());
				selectQueryQVO
					.setSortFields(queryQVO.getSortFields())
					.setSentDBConnection(queryQVO.getSentDBConnection());
				list.addAll(findParentsToRoot(dataSource, selectQueryQVO, parentIds));
			}
		}

		/*logger.info("Executed DataSource: DS=[{}] Usr=[{}] Dur=[{}] Rs#=[{}]",
			queryQVO.getDataSourceName(), securityService.getCurrentUser(),
			System.currentTimeMillis() - start, list.size());*/

		return list;
	}

	@Override
	public List<KeyValueVO<Serializable, String>> executeLookUp(Long dataSourceId, Long targetDataSourceId, String sentDBConnection, Map<String, Object> filter) {
		DataSource dataSource = load(dataSourceId);
		DataSource targetDataSource = load(targetDataSourceId);

		XDataSource targetXDataSource = getXDataSource(targetDataSource);

		List<String> select = new ArrayList<>();
		select.add(targetDataSource.getKeyField());
		select.add(targetDataSource.getTitleField() != null ? targetDataSource.getTitleField() : targetDataSource.getKeyField());

		Map<String, String> sort = new HashMap<>();
		sort.put(select.get(1), "asc");

		DSQueryBuilder builder = new DSQueryBuilder(targetXDataSource)
			.appendSelect(select)
			.appendFrom()
			.appendWhere(filter)
			.appendSort(sort);

		Long dbConnId = findProperDBConnection(sentDBConnection, dataSource);
		String comment = String.format("DsLkUp[%s > %s]", dataSource.getName(), targetDataSource.getName());
		List<KeyValueVO<Serializable, String>> result;
		try {
			result = dbConnectionService.executeQuery(
				dbConnId,
				processQuery(
					dbConnId,
					builder.getQuery().toString(),
					targetXDataSource.getQuery().getMode()),
				comment,
				builder.getQueryParams(),
				1L,
				50L
			).toListOfKeyValues();
		} catch (Exception e) {
			logger.error(String.format("LookUp exec error: source=%s, target=%s",
				dataSource.getName(), targetDataSource.getName()), e);
			result = new ArrayList<>();
			result.add(new KeyValueVO<Serializable, String>("--Error--", "--Error--"));
		}

		return result;
	}

	@Override
	public List<Map<String, Object>> executeOfParent(SelectQueryQVO queryQVO, Serializable parentId) {
		/*logger.info("Executing OfParent: DS=[{}] Prnt=[{}] Usr=[{}]",
			queryQVO.getDataSourceName(), parentId, securityService.getCurrentUser());
		long start = System.currentTimeMillis();*/

		DataSource dataSource = loadByName(queryQVO.getDataSourceName());
		XDataSource xDataSource = getXDataSource(dataSource);

		Map<String, Object> params = new HashMap<>();
		params.put("parentId", parentId);

		queryQVO
			.setFilterExpression(dataSource.getSelfRelPointerField() + " = :parentId")
			.setInputParams(params);

		DSQueryBuilder builder = new DSQueryBuilder(xDataSource, queryQVO)
			.appendSelect(queryQVO.getSelectFields())
			.appendFrom()
			.appendWhere()
			.appendSort(queryQVO.getSortFields());

		String comment = String.format("DsChld[%s]", dataSource.getName());
		Long dbConnId = findProperDBConnection(queryQVO.getSentDBConnection(), dataSource);
		List<Map<String, Object>> result = dbConnectionService.executeQuery(
			dbConnId,
			processQuery(
				dbConnId,
				builder.getQuery().toString(),
				xDataSource.getQuery().getMode()),
			comment,
			params
		).toListOfMap();

		/*logger.info("Executed OfParent: DS=[{}] Prnt=[{}] Usr=[{}] Dur=[{}] Rs#=[{}]",
			queryQVO.getDataSourceName(), parentId, securityService.getCurrentUser(),
			System.currentTimeMillis() - start, result.size());*/

		return result;
	}

	@Override
	public long execute(CountQueryQVO queryQVO) {
		/*logger.info("Executing Count: DS=[{}] Usr=[{}]",
			queryQVO.getDataSourceName(), securityService.getCurrentUser());
		long start = System.currentTimeMillis();*/

		DataSource dataSource = loadByName(queryQVO.getDataSourceName());
		XDataSource xDataSource = getXDataSource(dataSource);

		DSQueryBuilder builderVO = new DSQueryBuilder(xDataSource, queryQVO)
			.appendSelect(Collections.singletonList("count(1) as cnt"))
			.appendFrom()
			.appendWhere();

		StringBuilder main = builderVO.getQuery();

		Long dbConnId = findProperDBConnection(queryQVO.getSentDBConnection(), dataSource);

		String comment = String.format("DsCnt[%s]", dataSource.getName());

		List<Map<String, Object>> list = dbConnectionService.executeQuery(
			dbConnId,
			processQuery(
				dbConnId,
				main.toString(),
				xDataSource.getQuery().getMode()),
			comment,
			builderVO.queryParams
		).toListOfMap();

		long result = ((BigDecimal) list.get(0).get("cnt")).longValue();

		/*logger.info("Executed Count: DS=[{}] Usr=[{}] Dur=[{}] #=[{}]",
			queryQVO.getDataSourceName(), securityService.getCurrentUser(), System.currentTimeMillis() - start, result);*/

		return result;
	}

	@Override
	public List<Map<String, Object>> execute(AggregateQueryQVO queryQVO) {
		/*logger.info("Executing Aggregate: DS=[{}] Usr=[{}]",
			queryQVO.getDataSourceName(), securityService.getCurrentUser());
		long start = System.currentTimeMillis();*/

		DataSource dataSource = loadByName(queryQVO.getDataSourceName());
		XDataSource xDataSource = getXDataSource(dataSource);

		List<String> select = new ArrayList<>();
		for (Map.Entry<String, List<XDVAggregatorFunction>> entry : queryQVO.getSelectFields().entrySet()) {
			for (XDVAggregatorFunction function : entry.getValue()) {
				select.add(String.format("%2$s(%1$s) as %2$s___%1$s", entry.getKey(), function));
			}
		}

		DSQueryBuilder builderVO = new DSQueryBuilder(xDataSource, queryQVO)
			.appendSelect(select)
			.appendFrom()
			.appendWhere();

		StringBuilder main = builderVO.getQuery();

		Long dbConnId = findProperDBConnection(queryQVO.getSentDBConnection(), dataSource);

		String comment = String.format("DsAgr[%s]", dataSource.getName());

		Map<String, Object> row = dbConnectionService.executeQuery(
			dbConnId,
			processQuery(
				dbConnId,
				main.toString(),
				xDataSource.getQuery().getMode()),
			comment,
			builderVO.queryParams
		).toListOfMap()
			.get(0);

		Map<String, Map<String, Object>> map = new LinkedHashMap<>();
		for (XDVAggregatorFunction function : XDVAggregatorFunction.values()) {
			map.put(function.name().toLowerCase(), new HashMap<String, Object>());
		}

		for (Map.Entry<String, Object> entry : row.entrySet()) {
			String[] parts = entry.getKey().split("___");
			String func = parts[0];
			String field = parts[1];
			map.get(func).put(field, entry.getValue());
		}

		List<Map<String, Object>> result = new ArrayList<>();
		for (Map.Entry<String, Map<String, Object>> entry : map.entrySet()) {
			if (entry.getValue().size() > 0) {
				if (dataSource.getKeyField() != null) {
					entry.getValue().put(dataSource.getKeyField(), -XDVAggregatorFunction.valueOf(entry.getKey()).ordinal() - 1);
				}
				if (dataSource.getTitleField() != null) {
					entry.getValue().put(dataSource.getTitleField(), entry.getKey());
				}
				result.add(entry.getValue());
			}
		}

		/*logger.info("Executed Aggregate: DS=[{}] Usr=[{}] Dur=[{}]",
			queryQVO.getDataSourceName(), securityService.getCurrentUser(), System.currentTimeMillis() - start);*/

		return result;
	}

	@Override
	public Map<String, Object> convertSimpleParamsToFilter(Long dsId, Map<String, List<String>> params, boolean usedInList) {
		Map<String, Object> result = new HashMap<>();

		DataSource ds = load(dsId);
		XDataSource xDataSource = getXDataSource(ds);

		for (XDSField xdsField : xDataSource.getFields()) {
			String fieldName = xdsField.getName();
			List<String> values = params.get(fieldName);
			if (values != null && values.size() > 0) {
				switch (xdsField.getFilterType()) {
					case Equal:
						if (usedInList) {
							List<Object> convertedValues = new ArrayList<>();
							for (String value : values) {
								convertedValues.add(convertQueryParam(xdsField.getType(), value));
							}
							result.put(fieldName, convertedValues);
						} else {
							result.put(fieldName, convertQueryParam(xdsField.getType(), values.get(0)));
						}
						break;

					case Contain:
						String paramValue = values.get(0);
						result.put(fieldName, convertQueryParam(xdsField.getType(), paramValue));
						break;

					case List:
					case Search:
						result.put(fieldName, values);
						break;
				}
			} else if (params.containsKey(fieldName + "_u") || params.containsKey(fieldName + "_l")) {
				if (xdsField.getFilterType().equals(XDSFieldFilterType.Range)) {
					Serializable lower = convertQueryParam(xdsField.getType(), params.get(fieldName + "_l").get(0));
					Serializable upper = convertQueryParam(xdsField.getType(), params.get(fieldName + "_u").get(0));
					RangeVO rangeVO = new RangeVO(lower, upper);
					result.put(fieldName, rangeVO);
				}
			}
		}

		return result;
	}

	// -------------------------- PRIVATE METHODS

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

	private void markRelationsAsDeleted(Long sourceId) {
		persistorService
			.createQueryBuilder()
			.addSelect("update DataSourceRelation ent set ent.deleted = true where ent.source.id = :srcId")
			.addParam("srcId", sourceId)
			.update();
	}

	private void loadRelationsToMap(Long sourceId, Map<String, DataSourceRelation> map) {
		List<DataSourceRelation> relations = persistorService
			.createQueryBuilder()
			.addFrom(DataSourceRelation.class, "ent")
			.addWhere("and ent.source.id = :srcId")
			.addParam("srcId", sourceId)
			.list();

		for (DataSourceRelation relation : relations) {
			map.put(relation.getSourcePointerField(), relation);
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
		if (sentDBConn != null) {
			logger.info("Sent DB Conn: User=[{}] Conn=[{}]", securityService.getCurrentUser(), sentDBConn);

			DBConnection dbConnection = dbConnectionService.loadByName(sentDBConn);
			if (dbConnection == null) {
				logger.error("Invalid sent db connection: {}", sentDBConn);
				throw new MetisException(MetisErrorCode.InvalidDBConnection, sentDBConn);
			} else {
				return dbConnection.getId();
			}
		}

		DBConnection defaultConnection = dbConnectionService.getDefaultConnectionOfCurrentUser();
		return defaultConnection != null ? defaultConnection.getId() : dataSource.getConnectionId();
	}

	private String processDynamicQuery(String queryCode, XDSQuery xdsQuery, Map<String, Object> params) {
		if (ObjectUtil.isTrue(xdsQuery.getDynamic())) {
			StringWriter out = new StringWriter();
			try {
				Template template = new Template(queryCode, xdsQuery.getText(), freeMarkerCfg); //TODO cache template
				template.process(params, out);
				return out.toString();
			} catch (Exception e) {
				logger.warn("processDynamicQuery", e);
				throw new MetisException(MetisErrorCode.DynamicQuery);
			}
		}
		return xdsQuery.getText();
	}

	private XEntity findXEntity(Map<String, XEntity> map, String alias) {
		if (!map.containsKey(alias)) {
			throw new MetisException(MetisErrorCode.UnknownAlias, alias);
		}
		return map.get(alias);
	}

	private List<Object> extractParentIds(String selfRelPointerField, List<Map<String, Object>> list) {
		List<Object> parentIds = new ArrayList<>();
		for (Map<String, Object> map : list) {
			if (map.get(selfRelPointerField) != null) {
				parentIds.add(map.get(selfRelPointerField));
			}
		}
		return parentIds;
	}

	private List<Map<String, Object>> findParentsToRoot(DataSource dataSource,
														SelectQueryQVO queryQVO,
														List<Object> parentIds) {

		XDataSource xDataSource = getXDataSource(dataSource);

		queryQVO.setFilterExpression(dataSource.getKeyField() + " in (:ids)");

		DSQueryBuilder builderVO = new DSQueryBuilder(xDataSource, queryQVO)
			.appendSelect(queryQVO.getSelectFields())
			.appendFrom()
			.appendWhere()
			.appendSort(queryQVO.getSortFields());

		Long dbConnId = findProperDBConnection(queryQVO.getSentDBConnection(), dataSource);

		String comment = String.format("DsPar[%s]", dataSource.getName());

		String query = builderVO.getQuery().toString();
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
			).toListOfMap();

			result.addAll(list);

			visitedParents.addAll(parentIds);

			parentIds = extractParentIds(dataSource.getSelfRelPointerField(), list);

			for (int i = parentIds.size() - 1; i >= 0; i--) {
				if (visitedParents.contains(parentIds.get(i))) {
					parentIds.remove(i);
				}
			}
		}

		return result;
	}

	private Serializable convertQueryParam(XDSFieldType fieldType, String value) {
		Serializable result = null;

		if (value != null) {
			switch (fieldType) {
				case String:
					result = value;
					break;

				case Integer:
					result = Long.valueOf(value);
					break;

				case Real:
					result = new BigDecimal(value);
					break;

				case Date:
					result = CalendarUtil.toGregorian(value, "yyyyMMdd");
					break;

				case DateTime:
					result = CalendarUtil.toGregorian(value, "yyyyMMddHHmmss");
					break;

				case Boolean:
					result = Boolean.valueOf(value);
					break;

				case LookUp:
					break;
			}
		}

		return result;
	}

	// ------------------------------ PRIVATE INNER CLASS

	private class MyWriter extends PrettyPrintWriter {
		public MyWriter(Writer writer) {
			super(writer);
		}

		@Override
		protected void writeText(QuickWriter writer, String text) {
			writer.write(text);
		}
	}

	private class DSQueryBuilder {
		private StringBuilder query = new StringBuilder();
		private Map<String, Object> queryParams = new HashMap<>();

		private XDataSource xDataSource;
		private AbstractQueryQVO queryQVO;

		// ---------------

		public DSQueryBuilder(XDataSource xDataSource) {
			this(xDataSource, null);
		}

		public DSQueryBuilder(XDataSource xDataSource, AbstractQueryQVO queryQVO) {
			this.xDataSource = xDataSource;
			this.queryQVO = queryQVO;
		}

		// ---------------

		public StringBuilder getQuery() {
			return query;
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
			Map<String, Object> params;
			if (queryQVO != null && queryQVO.getInputParams() != null) {
				params = queryQVO.getInputParams();
			} else {
				params = new HashMap<>();
			}

			query
				.append("from (\n")
				.append(processDynamicQuery(xDataSource.getName(), xDataSource.getQuery(), params))
				.append("\n)\n");
			return this;
		}

		public DSQueryBuilder appendWhere() {
			if (queryQVO == null) {
				throw new RuntimeException("No AbstractQueryQVO!");
			}

			query.append("where\n\t");

			if (queryQVO.getFilterExpression() != null) {
				query
					.append(queryQVO.getFilterExpression())
					.append("\n");

				List<String> paramsInQuery = NamedParameterStatement.findParamsInQuery(queryQVO.getFilterExpression());
				if (queryQVO.getInputParams() != null) {
					for (String param : paramsInQuery) {
						queryParams.put(param, queryQVO.getInputParams().get(param));
					}
				}
			} else {
				query.append("1=1\n");
			}

			appendFilters(xDataSource, queryQVO.getInputParams());
			return this;
		}

		public DSQueryBuilder appendWhere(String filter) {
			if (filter != null) {
				query.append("where\n\t");
				if (filter.toLowerCase().startsWith("or") || filter.toLowerCase().startsWith("and")) {
					query.append("1=1");
				}
				query.append(filter);
			}
			return this;
		}

		public DSQueryBuilder appendWhere(Map<String, Object> filter) {
			if (filter != null && filter.size() > 0) {
				query.append("where\n\t1=1\n");
				appendFilters(xDataSource, filter);
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

		private void appendFilters(XDataSource dataSource, Map<String, Object> filters) {
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
									query.append(String.format("\tand %1$s in (:%1$s)\n", xdsField.getName()));
								} else {
									query.append(String.format("\tand %1$s  = :%1$s\n", xdsField.getName()));
								}
								queryParams.put(xdsField.getName(), value);
								break;

							case Contain: // Only String
								query.append(String.format("\tand %1$s like :%1$s\n", xdsField.getName()));
								queryParams.put(xdsField.getName(), filter.getValue());
								break;

							case Range: // Date & Number
								RangeVO rangeVO = (RangeVO) filter.getValue();
								if (rangeVO.getLower() != null) {
									query.append(String.format("\tand %1$s >= :%1$s_l\n", xdsField.getName()));
									queryParams.put(xdsField.getName() + "_l", rangeVO.getLower());
								}
								if (rangeVO.getUpper() != null) {
									query.append(String.format("\tand %1$s < :%1$s_u\n", xdsField.getName()));
									queryParams.put(xdsField.getName() + "_u", rangeVO.getUpper());
								}
								break;

							case List: // All types (except boolean)
							case Search:
								query.append(String.format("\tand %1$s in (:%1$s)\n", xdsField.getName()));
								List<Serializable> items = new ArrayList<>();
								List<KeyValueVO<Serializable, String>> list = (List<KeyValueVO<Serializable, String>>) filter.getValue();
								for (KeyValueVO<Serializable, String> keyValue : list) {
									items.add(keyValue.getKey());
								}
								queryParams.put(xdsField.getName(), items);
								break;
						}
					}
				}

				for (XDSParameter xdsParameter : dataSource.getParams()) {
					if (filters.containsKey(xdsParameter.getName()) || ObjectUtil.isTrue(xdsParameter.getRequired())) {
						queryParams.put(xdsParameter.getName(), filters.get(xdsParameter.getName()));
					}
				}
			}
		}
	}
}
