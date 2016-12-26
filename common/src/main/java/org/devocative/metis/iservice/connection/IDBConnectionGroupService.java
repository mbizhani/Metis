package org.devocative.metis.iservice.connection;

import org.devocative.metis.entity.connection.DBConnectionGroup;

import java.util.List;

public interface IDBConnectionGroupService {
	List<DBConnectionGroup> search(long pageIndex, long pageSize);

	long count();

	void saveOrUpdate(DBConnectionGroup dbConnectionGroup, String mappingXML);

	DBConnectionGroup getByName(String name);

	List<DBConnectionGroup> list();
}
