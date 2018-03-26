package org.devocative.metis.service.connection;

import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.iservice.connection.IDBConnectionGroupService;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.vo.filter.connection.DBConnectionGroupFVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service("mtsDBConnectionGroupService")
public class DBConnectionGroupService implements IDBConnectionGroupService {
	private static final Logger logger = LoggerFactory.getLogger(DBConnectionGroupService.class);

	@Autowired
	private IPersistorService persistorService;

	@Autowired
	private IDBConnectionService connectionService;

	// ------------------------------

	@Override
	public void saveOrUpdate(DBConnectionGroup entity) {
		persistorService.saveOrUpdate(entity);
	}

	@Override
	public DBConnectionGroup load(String id) {
		return persistorService.get(DBConnectionGroup.class, id);
	}

	@Override
	public DBConnectionGroup loadByName(String name) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DBConnectionGroup.class, "ent")
			.addWhere("and ent.name = :name")
			.addParam("name", name)
			.object();
	}

	@Override
	public List<DBConnectionGroup> list() {
		return persistorService.list(DBConnectionGroup.class);
	}

	@Override
	public List<DBConnectionGroup> search(DBConnectionGroupFVO filter, long pageIndex, long pageSize) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select ent")
			.addFrom(DBConnectionGroup.class, "ent")
			.applyFilter(DBConnectionGroup.class, "ent", filter)
			.list((pageIndex - 1) * pageSize, pageSize);
	}

	@Override
	public long count(DBConnectionGroupFVO filter) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select count(1)")
			.addFrom(DBConnectionGroup.class, "ent")
			.applyFilter(DBConnectionGroup.class, "ent", filter)
			.object();
	}

	@Override
	public List<User> getCreatorUserList() {
		return persistorService.list(User.class);
	}

	@Override
	public List<User> getModifierUserList() {
		return persistorService.list(User.class);
	}

	// ==============================

	@Override
	public void saveOrUpdate(DBConnectionGroup dbConnectionGroup, String mappingXML) {
		try {
			persistorService.startTrx();

			if (mappingXML != null) {
				ConfigLob configLob = dbConnectionGroup.getConfigId() == null ?
					new ConfigLob() :
					persistorService.get(ConfigLob.class, dbConnectionGroup.getConfigId());
				configLob.setValue(mappingXML);
				persistorService.saveOrUpdate(configLob);
				dbConnectionGroup.setConfig(configLob);
			}

			persistorService.saveOrUpdate(dbConnectionGroup);
			persistorService.commitOrRollback();
		} finally {
			connectionService.groupChanged(dbConnectionGroup);
		}
	}
}