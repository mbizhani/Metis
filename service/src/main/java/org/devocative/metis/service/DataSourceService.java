package org.devocative.metis.service;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.core.util.QuickWriter;
import com.thoughtworks.xstream.io.xml.PrettyPrintWriter;
import freemarker.template.Configuration;
import freemarker.template.Template;
import org.devocative.adroit.ObjectUtil;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.adroit.vo.RangeVO;
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
import org.devocative.metis.vo.query.AbstractQueryQVO;
import org.devocative.metis.vo.query.CountQueryQVO;
import org.devocative.metis.vo.query.SelectQueryQVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service("mtsDataSourceService")
public class DataSourceService implements IDataSourceService {
	private static final Logger logger = LoggerFactory.getLogger(DataSourceService.class);

	private XStream xstream;
	private Configuration freeMarkerCfg;

	@Autowired
	private IDBConnectionService dbConnectionService;

	@Autowired
	private IPersistorService persistorService;

	// ------------------------------

	public DataSourceService() {
		xstream = new XStream();
		xstream.processAnnotations(XDataSource.class);

		freeMarkerCfg = new Configuration(Configuration.VERSION_2_3_23);
	}

	// ------------------------------

	@Override
	public DataSource load(Long id) {
		return persistorService.get(DataSource.class, id);
	}

	@Override
	public DataSource loadByName(String name) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DataSource.class, "ent")
			.addWhere("and ent.name = :name")
			.addParam("name", name)
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
			dataSource.setConnection(new DBConnection(dbConnId));
			config = new ConfigLob();
		} else {
			dataSource = load(dataSourceId);
			config = dataSource.getConfig();
		}

		dataSource.setName(xDataSource.getName());
		dataSource.setTitle(title);

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

	@Override
	public Long findProperDBConnection(String dataSourceName) {
		return persistorService.createQueryBuilder()
			.addSelect("select ent.connectionId")
			.addFrom(DataSource.class, "ent")
			.addWhere("and ent.name = :name")
			.addParam("name", dataSourceName)
			.object();
	}

	@Override
	public List<KeyValueVO<Serializable, String>> getLookUpList(Long targetDataSourceId) {
		DataSource dataSource = persistorService.get(DataSource.class, targetDataSourceId);
		XDataSource xDataSource = getXDataSource(dataSource);

		StringBuilder queryBuilder = new StringBuilder();
		queryBuilder.append("select ").append(dataSource.getKeyField()).append(",");
		if (dataSource.getTitleField() != null) {
			queryBuilder.append(dataSource.getTitleField());
		} else {
			queryBuilder.append(dataSource.getKeyField());
		}
		queryBuilder.append(" from (")
			.append(processDynamicQuery(xDataSource.getName(), xDataSource.getQuery(), null))
			.append("\n)");

		Long dbConnId = dataSource.getConnectionId();
		return dbConnectionService.executeQuery(
			dbConnId,
			processQuery(
				dbConnId,
				xDataSource.getQuery().getMode(),
				queryBuilder.toString()),
			"").toListOfKeyValues();
	}

	@Override
	public List<Map<String, Object>> executeDataSourceForParent(String queryCode,
																String name,
																List<String> selectFields,
																Serializable parentId,
																Map<String, String> sortFields) {
		DataSource dataSource = loadByName(name);

		XDataSource xDataSource = getXDataSource(dataSource);
		StringBuilder mainQueryBuilder = appendSelect(queryCode, selectFields, xDataSource.getQuery(), null);

		mainQueryBuilder.append(" where ").append(dataSource.getSelfRelPointerField()).append(" = :parentId");

		appendSortFields(sortFields, mainQueryBuilder);

		Map<String, Object> params = new HashMap<>();
		params.put("parentId", parentId);

		String comment = String.format("CHILDREN[%s, %s]", queryCode, parentId);
		Long dbConnId = findProperDBConnection(name);
		return dbConnectionService.executeQuery(
			dbConnId,
			processQuery(
				dbConnId,
				xDataSource.getQuery().getMode(),
				mainQueryBuilder.toString()),
			params,
			comment).toListOfMap();
	}

	@Override
	public String processQuery(Long dbConnId, XDSQueryMode mode, String query) {
		String finalQuery = null;

		switch (mode) {
			case Sql:
				finalQuery = query;
				break;
			case Eql:
				finalQuery = processEntityQuery(dbConnId, query);
				break;
			/*case SqlAndEql:
				throw new RuntimeException("Mode SqlAndEql not implemented!"); //TODO*/
			//break;
		}
		logger.debug("Process Query: FINAL = {}", finalQuery);
		return finalQuery;
	}

	public List<Map<String, Object>> execute(SelectQueryQVO queryQVO, boolean findParentsIfDefined) {
		DataSource dataSource = loadByName(queryQVO.getDataSourceName());
		XDataSource xDataSource = getXDataSource(dataSource);

		QueryBuilderVO builderVO = buildUpBase(queryQVO, xDataSource);

		appendSelect(queryQVO.getSelectFields(), builderVO.select);

		StringBuilder main = builderVO.getQuery();

		appendSortFields(queryQVO.getSortFields(), main);

		Long dbConnId = findProperDBConnection(queryQVO.getDataSourceName());

		String comment = String.format("LIST[%s]", queryQVO.getQueryCode());

		List<Map<String, Object>> list = dbConnectionService.executeQuery(
			dbConnId,
			processQuery(
				dbConnId,
				xDataSource.getQuery().getMode(),
				main.toString()),
			builderVO.queryParams,
			comment,
			queryQVO.getPageIndex(),
			queryQVO.getPageSize()
		).toListOfMap();

		if (findParentsIfDefined && dataSource.getSelfRelPointerField() != null) {
			List<Object> parentIds = extractParentIds(dataSource.getSelfRelPointerField(), list);
			if (parentIds.size() > 0) {
				SelectQueryQVO selectQueryQVO = new SelectQueryQVO(
					queryQVO.getQueryCode(),
					queryQVO.getDataSourceName(),
					queryQVO.getSelectFields());
				selectQueryQVO
					.setSortFields(queryQVO.getSortFields())
					.setFilterExpression(dataSource.getKeyField() + " in (:ids)");
				list.addAll(findParentsToRoot(xDataSource, selectQueryQVO, parentIds, dataSource.getSelfRelPointerField()));
			}
		}

		return list;
	}

	public long execute(CountQueryQVO queryQVO) {
		XDataSource xDataSource = getXDataSource(queryQVO.getDataSourceName());

		QueryBuilderVO builderVO = buildUpBase(queryQVO, xDataSource);

		appendSelect(Collections.singletonList("count(1)"), builderVO.select);

		StringBuilder main = builderVO.getQuery();

		Long dbConnId = findProperDBConnection(queryQVO.getDataSourceName());

		String comment = String.format("COUNT[%s]", queryQVO.getQueryCode());

		List<Map<String, Object>> list = dbConnectionService.executeQuery(
			dbConnId,
			processQuery(
				dbConnId,
				xDataSource.getQuery().getMode(),
				main.toString()),
			builderVO.queryParams,
			comment
		).toListOfMap();

		return ((BigDecimal) list.get(0).get("cnt")).longValue();
	}

	// -------------------------- PRIVATE METHODS


	private QueryBuilderVO buildUpBase(AbstractQueryQVO queryQVO, XDataSource xDataSource) {
		QueryBuilderVO builderVO = new QueryBuilderVO();

		builderVO.select
			.append("select\n");

		builderVO.from_where
			.append("from (\n")
			.append(processDynamicQuery(queryQVO.getQueryCode(), xDataSource.getQuery(), queryQVO.getInputParams()))
			.append("\n)\n");

		if (queryQVO.getFilterExpression() != null) {
			builderVO.from_where
				.append("where\n\t")
				.append(queryQVO.getFilterExpression())
				.append("\n");
			// TODO check query embedded params with XDataSource
			builderVO.queryParams = queryQVO.getInputParams();
		} else {
			builderVO.queryParams = appendWhere(queryQVO.getInputParams(), xDataSource, builderVO.from_where);
		}

		return builderVO;
	}

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

	private String processEntityQuery(Long dbConnId, String query) {
		XSchema xSchema = dbConnectionService.getSchemaOfMapping(dbConnId);
		Map<String, XEntity> aliasToXEntityMap = new HashMap<>();

		StringBuffer tableReplacerBuffer = new StringBuffer();
		Pattern tablePattern = Pattern.compile("(from|join)\\s+(\\w+\\.)?(\\w+)(\\s+(\\w+))?", Pattern.CASE_INSENSITIVE);
		Matcher tableMatcher = tablePattern.matcher(query);
		while (tableMatcher.find()) {
			String schema = tableMatcher.group(2);
			if (schema != null) {
				throw new MetisException(MetisErrorCode.SchemaInEql, schema);
			}

			String alias = tableMatcher.group(5);
			if (aliasToXEntityMap.containsKey(alias)) {
				throw new MetisException(MetisErrorCode.DuplicateAlias, alias);
			}
			String entity = tableMatcher.group(3);

			XEntity xEntity = xSchema.findEntity(entity);
			if (xEntity == null || xEntity.getTable() == null) {
				throw new MetisException(MetisErrorCode.EntityWithoutMapping, entity);
			}
			aliasToXEntityMap.put(alias, xEntity);
			String replacement = String.format("%s %s %s", tableMatcher.group(1), xEntity.getTable(), alias);
			tableMatcher.appendReplacement(tableReplacerBuffer, replacement);
		}
		tableMatcher.appendTail(tableReplacerBuffer);

		StringBuffer joinCondReplacerBuffer = new StringBuffer();
		Pattern joinCondPattern = Pattern.compile("on\\s+(\\w+([.]\\w+)?)\\s*[~]\\s*(\\w+([.]\\w+)?)", Pattern.CASE_INSENSITIVE);
		Matcher joinCondMatcher = joinCondPattern.matcher(tableReplacerBuffer.toString());
		while (joinCondMatcher.find()) {
			String leftAlias;
			String leftColumn;
			String rightAlias;
			String rightColumn;

			if (joinCondMatcher.group(2) != null && joinCondMatcher.group(4) != null) {
				throw new MetisException(MetisErrorCode.EqlInvalidJoin, joinCondMatcher.group(0));
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
			joinCondMatcher.appendReplacement(joinCondReplacerBuffer,
				String.format("on %s~%s=%s~%s", leftAlias, leftColumn, rightAlias, rightColumn));
		}
		joinCondMatcher.appendTail(joinCondReplacerBuffer);

		StringBuffer columnReplacerBuffer = new StringBuffer();
		Pattern columnPattern = Pattern.compile("(\\w+)\\.(\\w+)", Pattern.CASE_INSENSITIVE);
		Matcher columnMatcher = columnPattern.matcher(joinCondReplacerBuffer.toString());
		while (columnMatcher.find()) {
			String alias = columnMatcher.group(1);
			String prop = columnMatcher.group(2);
			XEntity xEntity = findXEntity(aliasToXEntityMap, alias);
			if (xEntity.findProperty(prop) == null) {
				throw new MetisException(MetisErrorCode.EqlUnknownProperty, String.format("%s.%s", alias, prop));
			}
			XAbstractProperty xAProp = xEntity.findProperty(prop);
			if (xAProp instanceof XProperty) {
				XProperty xProperty = (XProperty) xAProp;
				String replacement = String.format("%s.%s", alias, xProperty.getColumn());
				columnMatcher.appendReplacement(columnReplacerBuffer, replacement);
			} else {
				throw new MetisException(MetisErrorCode.EqlInvalidAssociationUsage, String.format("%s.%s", alias, prop));
			}
		}
		columnMatcher.appendTail(columnReplacerBuffer);

		return columnReplacerBuffer.toString().replace('~', '.');
	}

	private void checkJoinProperty(XAbstractProperty xAProp, String joinClause) {
		if (xAProp == null) {
			throw new MetisException(MetisErrorCode.EqlUnknownProperty, joinClause);
		}
		if (!(xAProp instanceof XMany2One) && !(xAProp instanceof XOne2Many)) {
			throw new MetisException(MetisErrorCode.EqlJoinOnProperty, joinClause);
		}
	}

	private void appendSelect(List<String> selectFields, StringBuilder select) {
		select
			.append("\t")
			.append(selectFields.get(0))
			.append("\n");

		for (int i = 1; i < selectFields.size(); i++) {
			select
				.append("\t,")
				.append(selectFields.get(i))
				.append("\n");
		}
	}

	private Map<String, Object> appendWhere(Map<String, Object> filters, XDataSource dataSource, StringBuilder builder) {
		Map<String, Object> queryParams = new HashMap<>();

		if (filters != null && filters.size() > 0) {
			builder.append("where 1=1\n");
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
								builder.append(String.format("\tand %1$s in (:%1$s)\n", xdsField.getName()));
							} else {
								builder.append(String.format("\tand %1$s  = :%1$s\n", xdsField.getName()));
							}
							queryParams.put(xdsField.getName(), value);
							break;

						case Contain: // Only String
							builder.append(String.format("\tand %1$s like :%1$s\n", xdsField.getName()));
							queryParams.put(xdsField.getName(), filter.getValue());
							break;

						case Range: // Date & Number
							RangeVO rangeVO = (RangeVO) filter.getValue();
							if (rangeVO.getLower() != null) {
								builder.append(String.format("\tand %1$s >= :%1$s_l\n", xdsField.getName()));
								queryParams.put(xdsField.getName() + "_l", rangeVO.getLower());
							}
							if (rangeVO.getUpper() != null) {
								builder.append(String.format("\tand %1$s < :%1$s_u\n", xdsField.getName()));
								queryParams.put(xdsField.getName() + "_u", rangeVO.getUpper());
							}
							break;

						case List: // All types (except boolean)
						case Search:
							builder.append(String.format("\tand %1$s in (:%1$s)\n", xdsField.getName()));
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

		return queryParams;
	}

	private void appendSortFields(Map<String, String> sortFields, StringBuilder mainQueryBuilder) {
		if (sortFields != null && sortFields.size() > 0) {
			mainQueryBuilder.append(" order by ");
			boolean firstAdded = false;
			for (Map.Entry<String, String> sortField : sortFields.entrySet()) {
				//TODO check field
				if (firstAdded) {
					mainQueryBuilder.append(",");
				} else {
					firstAdded = true;
				}
				mainQueryBuilder
					.append(sortField.getKey())
					.append(" ")
					.append(sortField.getValue());
			}
		}
	}

	private XEntity findXEntity(Map<String, XEntity> map, String alias) {
		if (!map.containsKey(alias)) {
			throw new MetisException(MetisErrorCode.UnknownAlias, alias);
		}
		return map.get(alias);
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

	private List<Object> extractParentIds(String selfRelPointerField, List<Map<String, Object>> list) {
		List<Object> parentIds = new ArrayList<>();
		for (Map<String, Object> map : list) {
			if (map.get(selfRelPointerField) != null) {
				parentIds.add(map.get(selfRelPointerField));
			}
		}
		return parentIds;
	}

	private List<Map<String, Object>> findParentsToRoot(XDataSource xDataSource, SelectQueryQVO queryQVO,
														List<Object> parentIds, String selfRelPointerField) {
		List<Map<String, Object>> result = new ArrayList<>();

		QueryBuilderVO builderVO = buildUpBase(queryQVO, xDataSource);

		appendSelect(queryQVO.getSelectFields(), builderVO.select);

		StringBuilder main = builderVO.getQuery();

		appendSortFields(queryQVO.getSortFields(), main);

		Long dbConnId = findProperDBConnection(queryQVO.getDataSourceName());

		String comment = String.format("LIST[%s]", queryQVO.getQueryCode());

		while (parentIds.size() > 0) {
			Map<String, Object> queryParams = new HashMap<>();
			queryParams.put("ids", parentIds);

			List<Map<String, Object>> list = dbConnectionService.executeQuery(
				dbConnId,
				processQuery(
					dbConnId,
					xDataSource.getQuery().getMode(),
					main.toString()),
				queryParams,
				comment
			).toListOfMap();

			result.addAll(list);

			parentIds = extractParentIds(selfRelPointerField, list);
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

	private class QueryBuilderVO {
		public StringBuilder select = new StringBuilder();
		public StringBuilder from_where = new StringBuilder();
		public Map<String, Object> queryParams;

		public StringBuilder getQuery() {
			return new StringBuilder()
				.append(select.toString())
				.append(from_where.toString());
		}
	}
}
