package org.devocative.metis.iservice;

import org.devocative.metis.entity.dataSource.DSField;
import org.devocative.metis.entity.dataSource.DataSource;

import java.sql.SQLException;
import java.util.List;
import java.util.Map;

public interface IDataSourceService {
	DataSource getDataSource(String name);

	long getCountForDataSource(String name, Map<String, Object> filters) throws SQLException;

	List<Map<String, Object>> executeDataSource(String name,
												List<DSField> columns,
												Map<String, Object> filters,
												Map<String, String> sortFields,
												Long first,
												Long size) throws SQLException;
}
