package org.devocative.metis.iservice.connection;

import org.devocative.demeter.entity.User;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.DBConnectionAlias;
import org.devocative.metis.entity.connection.EAliasMode;
import org.devocative.metis.vo.filter.connection.DBConnectionAliasFVO;

import java.util.List;

public interface IDBConnectionAliasService {
	void saveOrUpdate(DBConnectionAlias entity);

	DBConnectionAlias load(Long id);

	List<DBConnectionAlias> list();

	List<DBConnectionAlias> search(DBConnectionAliasFVO filter, long pageIndex, long pageSize);

	long count(DBConnectionAliasFVO filter);

	List<DBConnection> getConnectionList();

	List<User> getCreatorUserList();

	List<User> getModifierUserList();

	// ==============================

	DBConnectionAlias loadByNameMode(String name, EAliasMode mode);

	DBConnectionAlias loadByConnMode(Long dbConnId, EAliasMode mode);
}