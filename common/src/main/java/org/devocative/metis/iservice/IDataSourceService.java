package org.devocative.metis.iservice;

import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.entity.dataSource.config.*;
import org.devocative.metis.vo.filter.DataSourceFVO;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

public interface IDataSourceService {
	DataSource get(Long id);

	void saveOrUpdate(DataSource dataSource, XDSQuery xdsQuery, List<XDSField> fields, List<XDSParameter> parameters);

	List<DataSource> search(DataSourceFVO filter, long pageIndex, long pageSize);

	long count(DataSourceFVO filter);

	List<DataSource> getListForLookup();

	DataSource getDataSource(String name);

	XDataSource getXDataSource(DataSource dataSource);

	XDataSource getXDataSource(String name);

	long getCountForDataSource(String name, Map<String, Object> filters);

	List<XDSField> createFields(List<XDSField> currentFields, XDSQuery xdsQuery, Long connectionId,
								List<XDSParameter> xdsParameters);

	List<XDSParameter> createParams(String query, List<XDSParameter> currentParams);

	List<Map<String, Object>> executeDataSource(String name,
												Map<String, Object> filters,
												Map<String, String> sortFields,
												Long pageIndex,
												Long pageSize);

	List<KeyValueVO<Serializable, String>> getLookUpList(XDSAbstractField field);

	List<Map<String, Object>> getChildrenOfParent(String name, Serializable parentId, Map<String, String> sortFields);

	String processQuery(Long dbConnId, XDSQueryMode mode, String query);
}
