package org.devocative.metis.service;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.core.util.QuickWriter;
import com.thoughtworks.xstream.io.xml.PrettyPrintWriter;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.adroit.vo.RangeVO;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.entity.dataSource.config.XDSField;
import org.devocative.metis.entity.dataSource.config.XDataSource;
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
	public void saveOrUpdate(DataSource dataSource, String sql, List<XDSField> fields) {
		XDataSource xDataSource = new XDataSource();
		xDataSource.setSql(String.format("\n<![CDATA[\n%s\n]]>\n", sql));
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
		return (XDataSource) xstream.fromXML(dataSource.getConfig().getValue());
	}

	@Override
	public XDataSource getXDataSource(String name) {
		DataSource dataSource = getDataSource(name);
		XDataSource xDataSource = getXDataSource(dataSource);
		xDataSource.setConnectionInfoId(dataSource.getConnectionId());
		return xDataSource;
	}

	@Override
	public long getCountForDataSource(String name, Map<String, Object> filters) {
		XDataSource dataSource = getXDataSource(name);

		StringBuilder builder = new StringBuilder();
		builder
			.append("select count(1) cnt from (")
			.append(dataSource.getSql())
			.append(")");

		Map<String, Object> queryParams = appendWhere(filters, dataSource, builder);

		logger.debug("executeDataSource: FINAL SQL: {}", builder.toString());

		try {
			List<Map<String, Object>> list = dbConnectionService.executeQuery(dataSource.getConnectionInfoId(), builder.toString(), queryParams);
			return ((BigDecimal) list.get(0).get("cnt")).longValue();
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<XDSField> createFields(List<XDSField> currentFields, String sql, Long connectionId) {
		List<XDSField> result = new ArrayList<>();
		List<XDSField> fieldsFromDB;
		try {
			fieldsFromDB = dbConnectionService.getFields(connectionId, sql);
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}

		for (XDSField fieldFromDB : fieldsFromDB) {
			int i = currentFields.indexOf(fieldFromDB);
			result.add(i > -1 ? currentFields.get(i) : fieldFromDB);
		}
		return result;
	}

	@Override
	public List<Map<String, Object>> executeDataSource(String name,
													   Map<String, Object> filters,
													   Map<String, String> sortFields,
													   Long pageIndex,
													   Long pageSize) {
		XDataSource dataSource = getXDataSource(name);

		List<XDSField> selectFields = new ArrayList<>();
		for (XDSField field : dataSource.getFields()) {
			switch (field.getPlaceType()) {
				case Result:
				case Both:
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

		String mainQuery = mainQueryBuilder.toString();
		logger.debug("executeDataSource: MAIN SQL: {}", mainQuery);

		if (pageIndex != null && pageSize != null) {
			if (dbConnectionService.isOracle(dataSource.getConnectionInfoId())) {
				mainQuery = String.format("select * from (select rownum rnum_pg, a.* from ( %s ) a) where rnum_pg between :pg_first and :pg_last", mainQuery);

				queryParams.put("pg_first", (pageIndex - 1) * pageSize + 1);
				queryParams.put("pg_last", pageIndex * pageSize);
			} else if (dbConnectionService.isMySQL(dataSource.getConnectionInfoId())) {
				mainQuery = String.format("select * from (%s) limit :pg_first , :pg_size", mainQuery);

				queryParams.put("pg_first", (pageIndex - 1) * pageSize + 1);
				queryParams.put("pg_size", pageSize);
			} else {
				//TODO add other databases
				throw new RuntimeException("Database type not supported for pagination: " + dataSource.getConnectionInfoId());
			}

			logger.debug("executeDataSource: PAGING SQL: {}", mainQuery);
		}

		try {
			return dbConnectionService.executeQuery(dataSource.getConnectionInfoId(), mainQuery, queryParams);
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<KeyValueVO<Serializable, String>> getLookUpList(XDataSource dataSource, XDSField field) {
		try {
			return dbConnectionService.executeQueryAsKeyValues(dataSource.getConnectionInfoId(), field.getSqlOpt());
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
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
