package org.devocative.metis.service;

import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.iservice.IDBConnectionGroupService;
import org.devocative.metis.iservice.IDBConnectionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service("mtsDBConnectionGroupService")
public class DBConnectionGroupService implements IDBConnectionGroupService {

	@Autowired
	private IPersistorService persistorService;

	@Autowired
	private IDBConnectionService connectionService;

	@Override
	public List<DBConnectionGroup> search(long firstResult, long maxResults) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DBConnectionGroup.class, "ent")
			.list((firstResult - 1) * maxResults, firstResult * maxResults);
	}

	@Override
	public long count() {
		return persistorService
			.createQueryBuilder()
			.addSelect("select count(1)")
			.addFrom(DBConnectionGroup.class, "ent")
			.object();
	}

	@Override
	public void saveOrUpdate(DBConnectionGroup dbConnectionGroup, String mappingXML) {
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

		connectionService.groupChanged(dbConnectionGroup.getId());
	}

	@Override
	public DBConnectionGroup getByName(String name) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DBConnectionGroup.class, "ent")
			.addWhere("and ent.name=:name")
			.addParam("name", name)
			.object();
	}

	@Override
	public List<DBConnectionGroup> list() {
		return persistorService.list(DBConnectionGroup.class);
	}
}
