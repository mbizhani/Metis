package org.devocative.metis.service;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.core.util.QuickWriter;
import com.thoughtworks.xstream.io.xml.PrettyPrintWriter;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.adroit.vo.RangeVO;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.connection.mapping.XSchema;
import org.devocative.metis.entity.connection.mapping.XTable;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.entity.dataSource.DataSourceRelation;
import org.devocative.metis.entity.dataSource.config.*;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.iservice.IDataSourceService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.sql.SQLException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service("mtsDataSourceService")
public class DataSourceService implements IDataSourceService {
	private static final Logger logger = LoggerFactory.getLogger(DataSourceService.class);

	private XStream xstream;

	@Autowired
	private IDBConnectionService dbConnectionService;

	@Autowired
	private IPersistorService persistorService;

	public DataSourceService() {
		xstream = new XStream();
		xstream.processAnnotations(XDataSource.class);
	}

	@Override
	public void saveOrUpdate(DataSource dataSource, XDSQuery xdsQuery, List<XDSField> fields) {
		Map<String, DataSourceRelation> relationsMap = new HashMap<>();

		if (dataSource.getId() != null) {
			persistorService
				.createQueryBuilder()
				.addSelect("update DataSourceRelation ent set ent.deleted = true where ent.source.id = :srcId")
				.addParam("srcId", dataSource.getId())
				.update();

			List<DataSourceRelation> relations = persistorService
				.createQueryBuilder()
				.addFrom(DataSourceRelation.class, "ent")
				.addWhere("and ent.source.id = :srcId")
				.addParam("srcId", dataSource.getId())
				.list();

			for (DataSourceRelation relation : relations) {
				relationsMap.put(relation.getSourcePointerField(), relation);
			}
		}

		for (XDSField xdsField : fields) {
			if (xdsField.getIsKeyField()) {
				dataSource.setKeyField(xdsField.getName());
			} else {
				xdsField.setIsKeyField(null);
			}

			if (xdsField.getIsTitleField()) {
				dataSource.setTitleField(xdsField.getName());
			} else {
				xdsField.setIsTitleField(null);
			}

			if (xdsField.getIsSelfRelPointerField()) {
				dataSource.setSelfRelPointerField(xdsField.getName());
			} else {
				xdsField.setIsSelfRelPointerField(null);
			}

			if (XDSFieldType.LookUp == xdsField.getType()) {
				xdsField.setTargetId(xdsField.getTarget().getId());

				DataSourceRelation rel = relationsMap.get(xdsField.getName());
				if (rel == null) {
					rel = new DataSourceRelation();
				}
				rel.setSourcePointerField(xdsField.getName());
				rel.setDeleted(false);
				rel.setSource(dataSource);
				rel.setTarget(xdsField.getTarget());
				relationsMap.put(xdsField.getName(), rel);
			}
		}

		xdsQuery.setText(String.format("\n<![CDATA[\n%s\n]]>\n", xdsQuery.getText().trim()));

		XDataSource xDataSource = new XDataSource();
		xDataSource.setQuery(xdsQuery);
		xDataSource.setFields(fields);

		ConfigLob config = dataSource.getConfig();
		if (config == null) {
			config = new ConfigLob();
		}
		StringWriter writer = new StringWriter();
		xstream.marshal(xDataSource, new MyWriter(writer));
		config.setValue(writer.toString());

		dataSource.setConfig(config);

		persistorService.saveOrUpdate(config);
		persistorService.saveOrUpdate(dataSource);
		for (DataSourceRelation relation : relationsMap.values()) {
			persistorService.saveOrUpdate(relation);
		}
		persistorService.commitOrRollback();
	}

	@Override
	public List<DataSource> search(long firstResult, long maxResults) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DataSource.class, "ent")
			.list((firstResult - 1) * maxResults, firstResult * maxResults);
	}

	@Override
	public long count() {
		return persistorService
			.createQueryBuilder()
			.addSelect("select count(1)")
			.addFrom(DataSource.class, "ent")
			.object();
	}

	@Override
	public List<DataSource> getListForLookup() {
		return persistorService
			.createQueryBuilder()
			.addFrom(DataSource.class, "ent")
			.addWhere("and ent.keyField is not null")
			.list();
	}

	@Override
	public DataSource getDataSource(String name) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DataSource.class, "ent")
			.addWhere("and ent.name = :name")
			.addParam("name", name)
			.object();
	}

	@Override
	public XDataSource getXDataSource(DataSource dataSource) {
		List<DataSourceRelation> relations = persistorService
			.createQueryBuilder()
			.addFrom(DataSourceRelation.class, "ent")
			.addWhere("and ent.source.id = :srcId and ent.deleted = false")
			.addParam("srcId", dataSource.getId())
			.list();

		XDataSource xDataSource = (XDataSource) xstream.fromXML(dataSource.getConfig().getValue());
		for (DataSourceRelation relation : relations) {
			XDSField field = xDataSource.getField(relation.getSourcePointerField());
			field.setTarget(relation.getTarget());
		}
		xDataSource.setConnectionInfoId(dataSource.getConnectionId());
		return xDataSource;
	}

	@Override
	public XDataSource getXDataSource(String name) {
		DataSource dataSource = getDataSource(name);
		return getXDataSource(dataSource);
	}

	@Override
	public long getCountForDataSource(String name, Map<String, Object> filters) {
		XDataSource xDataSource = getXDataSource(name);

		StringBuilder builder = new StringBuilder();
		builder
			.append("select count(1) cnt from (")
			.append(xDataSource.getQuery().getText()) //TODO
			.append(")");

		Map<String, Object> queryParams = appendWhere(filters, xDataSource, builder);

		logger.debug("executeDataSource: FINAL SQL: {}", builder.toString());

		try {
			String comment = String.format("COUNT[%s]", name);
			Long dbConnId = xDataSource.getConnectionInfoId();
			List<Map<String, Object>> list = dbConnectionService.executeQuery(
				dbConnId,
				processQuery(
					dbConnId,
					xDataSource.getQuery().getMode(),
					builder.toString()),
				queryParams,
				comment);
			return ((BigDecimal) list.get(0).get("cnt")).longValue();
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<XDSField> createFields(List<XDSField> currentFields, XDSQuery xdsQuery, Long connectionId) {
		List<XDSField> result = new ArrayList<>();
		List<XDSField> fieldsFromDB;
		try {
			fieldsFromDB = dbConnectionService.getFields(
				connectionId,
				processQuery(
					connectionId,
					xdsQuery.getMode(),
					xdsQuery.getText()));
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}

		for (XDSField fieldFromDB : fieldsFromDB) {
			int i = currentFields.indexOf(fieldFromDB);
			if (i > -1) {
				XDSField currentField = currentFields.get(i);
				currentField
					.setDbType(fieldFromDB.getDbType())
					.setDbSize(fieldFromDB.getDbSize());
				result.add(currentField);
			} else {
				result.add(fieldFromDB);
			}
		}
		return result;
	}

	@Override
	public List<Map<String, Object>> executeDataSource(String name,
													   Map<String, Object> filters,
													   Map<String, String> sortFields,
													   Long pageIndex,
													   Long pageSize) {
		XDataSource xDataSource = getXDataSource(name);

		StringBuilder mainQueryBuilder = createSelectAndFrom(xDataSource);

		Map<String, Object> queryParams = appendWhere(filters, xDataSource, mainQueryBuilder);

		applySortFields(sortFields, mainQueryBuilder);

		String mainQuery = mainQueryBuilder.toString();
		logger.debug("executeDataSource: MAIN SQL: {}", mainQuery);

		Long dbConnId = xDataSource.getConnectionInfoId();
		if (pageIndex != null && pageSize != null) {
			if (dbConnectionService.isOracle(dbConnId)) {
				mainQuery = String.format("select * from (select rownum rnum_pg, a.* from ( %s ) a) where rnum_pg between :pg_first and :pg_last", mainQuery);

				queryParams.put("pg_first", (pageIndex - 1) * pageSize + 1);
				queryParams.put("pg_last", pageIndex * pageSize);
			} else if (dbConnectionService.isMySQL(dbConnId)) {
				mainQuery = String.format("select * from (%s) limit :pg_first , :pg_size", mainQuery);

				queryParams.put("pg_first", (pageIndex - 1) * pageSize + 1);
				queryParams.put("pg_size", pageSize);
			} else {
				//TODO add other databases
				throw new RuntimeException("Database type not supported for pagination: " + dbConnId);
			}

			logger.debug("executeDataSource: PAGING SQL: {}", mainQuery);
		}

		try {
			String comment = String.format("EXEC[%s]", name);
			return dbConnectionService.executeQuery(
				dbConnId,
				processQuery(
					dbConnId,
					xDataSource.getQuery().getMode(),
					mainQuery),
				queryParams,
				comment);
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<KeyValueVO<Serializable, String>> getLookUpList(XDSField field) {
		Long dataSrcId = field.getTarget().getId();
		DataSource dataSource = persistorService.get(DataSource.class, dataSrcId);
		XDataSource xDataSource = getXDataSource(dataSource);

		StringBuilder queryBuilder = new StringBuilder();
		queryBuilder.append("select ").append(dataSource.getKeyField()).append(",");
		if (dataSource.getTitleField() != null) {
			queryBuilder.append(dataSource.getTitleField());
		} else {
			queryBuilder.append(dataSource.getKeyField());
		}
		queryBuilder.append(" from (").append(xDataSource.getQuery().getText()).append(")"); //TODO

		try {
			Long dbConnId = dataSource.getConnectionId();
			return dbConnectionService.executeQueryAsKeyValues(
				dbConnId,
				processQuery(
					dbConnId,
					xDataSource.getQuery().getMode(),
					queryBuilder.toString()));
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Map<String, Object>> getChildrenOfParent(String name, Serializable parentId, Map<String, String> sortFields) {
		DataSource dataSource = getDataSource(name);

		XDataSource xDataSource = getXDataSource(dataSource);
		StringBuilder mainQueryBuilder = createSelectAndFrom(xDataSource);

		mainQueryBuilder.append(" where ").append(dataSource.getSelfRelPointerField()).append(" = :parentId");

		applySortFields(sortFields, mainQueryBuilder);

		Map<String, Object> params = new HashMap<>();
		params.put("parentId", parentId);
		try {
			String comment = String.format("CHILD[%s, %s]", name, parentId);
			Long dbConnId = xDataSource.getConnectionInfoId();
			return dbConnectionService.executeQuery(
				dbConnId,
				processQuery(
					dbConnId,
					xDataSource.getQuery().getMode(),
					mainQueryBuilder.toString()),
				params,
				comment);
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	public String processQuery(Long dbConnId, XDSQueryMode mode, String query) {
		String finalQuery = null;
		//TODO apply FreeMarker for query modification

		switch (mode) {
			case Sql:
				finalQuery = query;
				break;
			case Mapped:
				finalQuery = processMappedQuery(dbConnId, query);
				break;
			case SqlAndMapped:
				throw new RuntimeException("Mode SqlAndMapped not implemented!");
				//break;
		}
		logger.debug("Process Query: FINAL = {}", finalQuery);
		return finalQuery;
	}

	private String processMappedQuery(Long dbConnId, String query) {
		XSchema xSchema = dbConnectionService.getSchemaOfMapping(dbConnId);
		Map<String, XTable> aliasToXTableMap = new HashMap<>();

		StringBuffer tableReplacerBuffer = new StringBuffer();
		Pattern tablePattern = Pattern.compile("(from|join)\\s+(\\w+\\.)?(\\w+)(\\s+(\\w+))?", Pattern.CASE_INSENSITIVE);
		Matcher tableMatcher = tablePattern.matcher(query);
		while (tableMatcher.find()) {
			String schema = tableMatcher.group(2);
			if (schema != null) {
				throw new RuntimeException("Wrong schema set in query: " + schema);
			}

			String alias = tableMatcher.group(5);
			if (aliasToXTableMap.containsKey(alias)) {
				throw new RuntimeException("Duplicate alias: " + alias);
			}
			String table = tableMatcher.group(3);

			XTable xTable = xSchema.findByFrom(table);
			if (xTable == null || xTable.getTo() == null) {
				throw new RuntimeException("No mapping for: " + table);
			}
			aliasToXTableMap.put(alias, xTable);
			String replacement = String.format("%s %s %s", tableMatcher.group(1), xTable.getTo(), alias);
			tableMatcher.appendReplacement(tableReplacerBuffer, replacement);
		}
		tableMatcher.appendTail(tableReplacerBuffer);


		StringBuffer columnReplacerBuffer = new StringBuffer();
		Pattern columnPattern = Pattern.compile("(\\w+)\\.(\\w+)", Pattern.CASE_INSENSITIVE);
		Matcher columnMatcher = columnPattern.matcher(tableReplacerBuffer.toString());
		while (columnMatcher.find()) {
			String alias = columnMatcher.group(1);
			String column = columnMatcher.group(2);
			if (!aliasToXTableMap.containsKey(alias)) {
				throw new RuntimeException("Unknown alias: " + alias);
			}
			XTable xTable = aliasToXTableMap.get(alias);
			if (xTable.findColumnByFrom(column) == null) {
				throw new RuntimeException(String.format("Unknown column mapping: %s.%s", alias, column));
			}
			String replacement = String.format("%s.%s", alias, xTable.findColumnByFrom(column).getTo());
			columnMatcher.appendReplacement(columnReplacerBuffer, replacement);
		}
		columnMatcher.appendTail(columnReplacerBuffer);

		return columnReplacerBuffer.toString();
	}

	private StringBuilder createSelectAndFrom(XDataSource xDataSource) {
		List<XDSField> selectFields = new ArrayList<>();
		for (XDSField field : xDataSource.getFields()) {
			switch (field.getResultType()) {
				case Shown:
				case Hidden:
					selectFields.add(field);
			}
		}

		StringBuilder mainQueryBuilder = new StringBuilder();
		mainQueryBuilder.append("select ").append(selectFields.get(0).getName());

		for (int i = 1; i < selectFields.size(); i++) {
			XDSField field = selectFields.get(i);
			mainQueryBuilder.append(",").append(field.getName());
		}

		mainQueryBuilder
			.append(" from (")
			.append(xDataSource.getQuery().getText()) //TODO: the sql must be process by FreeMarker template engine
			.append(")");
		return mainQueryBuilder;
	}

	private Map<String, Object> appendWhere(Map<String, Object> filters, XDataSource dataSource, StringBuilder builder) {
		Map<String, Object> queryParams = new HashMap<>();

		if (filters != null && filters.size() > 0) {
			builder.append(" where 1=1 ");
			for (Map.Entry<String, Object> filter : filters.entrySet()) {
				XDSField dsField = dataSource.getField(filter.getKey());
				if (dsField != null) {
					switch (dsField.getFilterType()) {

						case Equal: // All types
							Object value = filter.getValue();
							if (value instanceof KeyValueVO) {
								value = ((KeyValueVO) value).getKey();
							}
							if (value instanceof Collection) { //TODO?
								builder.append(String.format("and %1$s in (:%1$s) ", dsField.getName()));
							} else {
								builder.append(String.format("and %1$s  = :%1$s ", dsField.getName()));
							}
							queryParams.put(dsField.getName(), value);
							break;

						case Contain: // Only String
							builder.append(String.format("and %1$s like :%1$s ", dsField.getName()));
							queryParams.put(dsField.getName(), filter.getValue());
							break;

						case Range: // Date & Number
							RangeVO rangeVO = (RangeVO) filter.getValue();
							if (rangeVO.getLower() != null) {
								builder.append(String.format("and %1$s >= :%1$s_l ", dsField.getName()));
								queryParams.put(dsField.getName() + "_l", rangeVO.getLower());
							}
							if (rangeVO.getUpper() != null) {
								builder.append(String.format("and %1$s < :%1$s_u ", dsField.getName()));
								queryParams.put(dsField.getName() + "_u", rangeVO.getUpper());
							}
							break;

						case List: // All types (except boolean)
							builder.append(String.format("and %1$s in (:%1$s) ", dsField.getName()));
							List<Serializable> items = new ArrayList<>();
							List<KeyValueVO<Serializable, String>> list = (List<KeyValueVO<Serializable, String>>) filter.getValue();
							for (KeyValueVO<Serializable, String> keyValue : list) {
								items.add(keyValue.getKey());
							}
							queryParams.put(dsField.getName(), items);
							break;
					}
				} // TODO else check in params
			}
		}

		return queryParams;
	}

	private void applySortFields(Map<String, String> sortFields, StringBuilder mainQueryBuilder) {
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


	private class MyWriter extends PrettyPrintWriter {
		public MyWriter(Writer writer) {
			super(writer);
		}

		@Override
		protected void writeText(QuickWriter writer, String text) {
			writer.write(text);
		}
	}
}
