package org.devocative.metis.iservice.connection;

import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.IApplicationLifecycle;
import org.devocative.demeter.iservice.IEntityService;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.entity.connection.mapping.XSchema;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.filter.connection.DBConnectionFVO;
import org.devocative.metis.vo.query.DbQueryRVO;
import org.devocative.metis.vo.query.PaginationQVO;
import org.devocative.metis.vo.query.QueryExecInfoRVO;

import java.util.List;
import java.util.Map;

public interface IDBConnectionService extends IApplicationLifecycle, IEntityService<DBConnection> {
	String CACHE_KEY_DB_CONNECTION = "MTS_DB_CONNECTION";
	String CACHE_KEY_X_SCHEMA = "MTS_DB_X_SCHEMA";

	// ---------------

	void saveOrUpdate(DBConnection entity);

	DBConnection load(Long id);

	DBConnection loadByName(String name);

	List<DBConnection> list();

	List<DBConnection> search(DBConnectionFVO filter, long pageIndex, long pageSize);

	long count(DBConnectionFVO filter);

	List<DBConnectionGroup> getGroupList();

	List<User> getCreatorUserList();

	List<User> getModifierUserList();

	// ==============================

	void saveOrUpdate(DBConnection dbConnection, String mappingXML, String password);

	List<DataFieldVO> findFields(Long id, String sql, Map<String, Object> params);

	DbQueryRVO executeQuery(
		Long dbConnId,
		String query,
		String comment,
		Map<String, Object> params);

	DbQueryRVO executeQuery(
		Long dbConnId,
		String query,
		String comment,
		PaginationQVO pagination);

	// Main Method
	DbQueryRVO executeQuery(
		Long dbConnId,
		String query,
		String comment,
		Map<String, Object> params,
		PaginationQVO pagination);

	void closeAllPools();

	QueryExecInfoRVO execute(Long dbConnId,
							 String query,
							 String comment,
							 Map<String, Object> params);

	XSchema getSchemaOfMapping(Long id);

	boolean checkConnection(Long id);

	void groupChanged(DBConnectionGroup dbConnectionGroup);

	void setDefaultConnectionForCurrentUser(Long id);

	DBConnection getDefaultConnectionOfCurrentUser();

	void removeDefaultConnectionOfCurrentUser();

	void updateCustomParam1(Map<String, String> params);

	DBConnection findByName(String name);
}
