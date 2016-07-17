package org.devocative.metis.iservice;

import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.mapping.XSchema;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.query.QueryRVO;

import java.util.List;
import java.util.Map;

public interface IDBConnectionService {
	DBConnection load(Long id);

	List<DBConnection> search(long pageIndex, long pageSize);

	long count();

	void saveOrUpdate(DBConnection dbConnection, String mappingXML);

	List<DBConnection> list();

	DBConnection loadByName(String name);

	List<DataFieldVO> findFields(Long id, String sql, Map<String, Object> params);

	QueryRVO executeQuery(
		Long dbConnId,
		String query,
		String comment,
		Map<String, Object> params);

	QueryRVO executeQuery(
		Long dbConnId,
		String query,
		String comment,
		Long pageIndex,
		Long pageSize);

	// Main Method
	QueryRVO executeQuery(
		Long dbConnId,
		String query,
		String comment,
		Map<String, Object> params,
		Long pageIndex,
		Long pageSize);

	void closeAllPools();

	XSchema getSchemaOfMapping(Long id);

	boolean checkConnection(Long id);

	void groupChanged(Long groupId);

	void setDefaultConnectionForCurrentUser(Long id);

	DBConnection getDefaultConnectionOfCurrentUser();

	void removeDefaultConnectionOfCurrentUser();
}
