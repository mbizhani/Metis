package org.devocative.metis.service;

import com.thoughtworks.xstream.XStream;
import org.devocative.metis.entity.dataSource.DSField;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.iservice.IDataSourceService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DataSourceService implements IDataSourceService {
	private static final Logger logger = LoggerFactory.getLogger(DataSourceService.class);
	private static final Map<String, DataSource> DATA_SOURCE_MAP = new HashMap<>();

	static {
		XStream xstream = new XStream();
		xstream.processAnnotations(DataSource.class);
		List<DataSource> list = (List<DataSource>) xstream.fromXML(DBConnectionService.class.getResourceAsStream("/dataSources.xml"));
		for (DataSource dataSource : list) {
			logger.debug("DataSource: {}", dataSource);
			DATA_SOURCE_MAP.put(dataSource.getName(), dataSource);
		}
	}

	//------------------------ SINGLETON
	private static DataSourceService instance = new DataSourceService();

	private DataSourceService() {
	}

	public static DataSourceService get() {
		return instance;
	}

	//------------------------ METHODS

	private IDBConnectionService dbConnectionService = DBConnectionService.get();

	public DataSource getDataSource(String name) {
		return DATA_SOURCE_MAP.get(name);
	}

	public long getCountForDataSource(String name, Map<String, Object> filters) throws SQLException {
		DataSource dataSource = DATA_SOURCE_MAP.get(name);

		StringBuilder builder = new StringBuilder();
		builder
			.append("select count(1) cnt from (")
			.append(dataSource.getSql())
			.append(")");

		appendWhere(filters, dataSource, builder);

		logger.debug("executeDataSource: FINAL SQL: {}", builder.toString());

		List<Map<String, Object>> list = dbConnectionService.executeQuery(dataSource.getDbConnectionRef(), builder.toString(), filters);
		return ((BigDecimal) list.get(0).get("cnt")).longValue();
	}

	public List<Map<String, Object>> executeDataSource(String name,
													   Map<String, Object> filters,
													   Map<String, String> sortFields,
													   Long pageIndex,
													   Long pageSize) throws SQLException {
		DataSource dataSource = DATA_SOURCE_MAP.get(name);

		List<DSField> selectFields = new ArrayList<>();
		for (DSField field : dataSource.getFields()) {
			switch (field.getPlaceType()) {
				case Result:
				case Both:
					selectFields.add(field);
			}
		}

		StringBuilder mainQueryBuilder = new StringBuilder();
		mainQueryBuilder.append("select ").append(selectFields.get(0).getName());

		for (int i = 1; i < selectFields.size(); i++) {
			DSField field = selectFields.get(i);
			mainQueryBuilder.append(",").append(field.getName());
		}

		mainQueryBuilder
			.append(" from (")
			.append(dataSource.getSql()) //TODO: the sql must be process by FreeMarker template engine
			.append(")");

		appendWhere(filters, dataSource, mainQueryBuilder);

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

		logger.debug("executeDataSource: MAIN SQL: {}", mainQueryBuilder.toString());

		if (pageIndex != null && pageSize != null) {
			mainQueryBuilder.insert(0, "select * from (select rownum rnum_pg, a.* from (");
			mainQueryBuilder.append(") a) where rnum_pg between :pg_first and :pg_last");

			filters = new HashMap<>(filters);
			filters.put("pg_first", (pageIndex - 1) * pageSize + 1);
			filters.put("pg_last", pageIndex * pageSize);

			logger.debug("executeDataSource: PAGING SQL: {}", mainQueryBuilder.toString());
		}

		return dbConnectionService.executeQuery(dataSource.getDbConnectionRef(), mainQueryBuilder.toString(), filters);
	}

	private void appendWhere(Map<String, Object> filters, DataSource dataSource, StringBuilder builder) {
		if (filters != null && filters.size() > 0) {
			builder.append(" where ");
			boolean firstAdded = false;
			for (Map.Entry<String, Object> filter : filters.entrySet()) {
				DSField field = dataSource.getField(filter.getKey());
				if (field != null) {
					//TODO: consider field.filterType
					if (firstAdded) {
						builder.append(" and ");
					} else {
						firstAdded = true;
					}
					builder.append(field.getName());
					switch (field.getFilterType()) {

						case Equal:
							builder.append(" = :").append(field.getName());
							break;
						case Contain:
							builder.append(" like :").append(field.getName());
							break;
						case Range:
							//TODO
							break;
						case List:
							builder.append(" in :").append(field.getName());
							break;
					}
				} // TODO else check in params
			}
		}
	}
}
