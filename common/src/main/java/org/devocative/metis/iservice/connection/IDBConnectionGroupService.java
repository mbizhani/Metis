package org.devocative.metis.iservice.connection;

import org.devocative.demeter.entity.User;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.vo.filter.connection.DBConnectionGroupFVO;

import java.util.List;

public interface IDBConnectionGroupService {
	void saveOrUpdate(DBConnectionGroup entity);

	DBConnectionGroup load(Long id);

	DBConnectionGroup loadByName(String name);

	List<DBConnectionGroup> list();

	List<DBConnectionGroup> search(DBConnectionGroupFVO filter, long pageIndex, long pageSize);

	long count(DBConnectionGroupFVO filter);

	List<User> getCreatorUserList();

	List<User> getModifierUserList();

	// ==============================

	void saveOrUpdate(DBConnectionGroup dbConnectionGroup, String mappingXML);
}