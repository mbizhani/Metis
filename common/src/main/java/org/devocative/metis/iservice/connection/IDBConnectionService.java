package org.devocative.metis.iservice.connection;

import org.devocative.demeter.iservice.IApplicationLifecycle;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.entity.connection.mapping.XSchema;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.query.DbQueryRVO;
import org.devocative.metis.vo.query.PaginationQVO;
import org.devocative.metis.vo.query.QueryExecInfoRVO;

import java.util.List;
import java.util.Map;

public interface IDBConnectionService extends IApplicationLifecycle {
	DBConnection load(Long id);

	List<DBConnection> search(long pageIndex, long pageSize);

	long count();

	void saveOrUpdate(DBConnection dbConnection, String mappingXML);

	List<DBConnection> list();

	DBConnection loadByName(String name);

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
}
