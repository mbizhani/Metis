package org.devocative.metis.service;

import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.adroit.vo.RangeVO;
import org.devocative.metis.entity.dataSource.DSField;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.metis.iservice.IDataSourceService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
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

	public long getCountForDataSource(String name, Map<String, Object> filters) {
		DataSource dataSource = DATA_SOURCE_MAP.get(name);

		StringBuilder builder = new StringBuilder();
		builder
			.append("select count(1) cnt from (")
			.append(dataSource.getSql())
			.append(")");

		Map<String, Object> queryParams = appendWhere(filters, dataSource, builder);

		logger.debug("executeDataSource: FINAL SQL: {}", builder.toString());

		try {
			List<Map<String, Object>> list = dbConnectionService.executeQuery(dataSource.getDbConnectionRef(), builder.toString(), queryParams);
			return ((BigDecimal) list.get(0).get("cnt")).longValue();
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	public List<Map<String, Object>> executeDataSource(String name,
													   Map<String, Object> filters,
													   Map<String, String> sortFields,
													   Long pageIndex,
													   Long pageSize) {
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

		Map<String, Object> queryParams = appendWhere(filters, dataSource, mainQueryBuilder);

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

			queryParams.put("pg_first", (pageIndex - 1) * pageSize + 1);
			queryParams.put("pg_last", pageIndex * pageSize);

			logger.debug("executeDataSource: PAGING SQL: {}", mainQueryBuilder.toString());
		}

		try {
			return dbConnectionService.executeQuery(dataSource.getDbConnectionRef(), mainQueryBuilder.toString(), queryParams);
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	public List<KeyValueVO<Serializable, String>> getLookUpList(DataSource dataSource, DSField field) {
		try {
			return dbConnectionService.executeQueryAsKeyValues(dataSource.getDbConnectionRef(), field.getSqlOpt());
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	private Map<String, Object> appendWhere(Map<String, Object> filters, DataSource dataSource, StringBuilder builder) {
		Map<String, Object> queryParams = new HashMap<>();

		if (filters != null && filters.size() > 0) {
			builder.append(" where 1=1 ");
			for (Map.Entry<String, Object> filter : filters.entrySet()) {
				DSField dsField = dataSource.getField(filter.getKey());
				if (dsField != null) {
					switch (dsField.getFilterType()) {

						case Equal: // All types
							Object value = filter.getValue();
							if (value instanceof KeyValueVO) {
								value = ((KeyValueVO) value).getKey();
							}
							builder.append(String.format("and %1$s  = :%1$s ", dsField.getName()));
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
}
