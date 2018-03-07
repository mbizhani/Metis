package org.devocative.metis.service.connection;

import org.devocative.demeter.DBConstraintViolationException;
import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.DBConnectionAlias;
import org.devocative.metis.entity.connection.EAliasMode;
import org.devocative.metis.iservice.connection.IDBConnectionAliasService;
import org.devocative.metis.vo.filter.connection.DBConnectionAliasFVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service("mtsDBConnectionAliasService")
public class DBConnectionAliasService implements IDBConnectionAliasService {
	private static final Logger logger = LoggerFactory.getLogger(DBConnectionAliasService.class);

	@Autowired
	private IPersistorService persistorService;

	// ------------------------------

	@Override
	public void saveOrUpdate(DBConnectionAlias entity) {
		try {
			persistorService.saveOrUpdate(entity);
		} catch (DBConstraintViolationException e) {
			if (e.isConstraint(DBConnectionAlias.UQ_CONST)) {
				throw new MetisException(MetisErrorCode.DuplicateDBConnectionAlias, entity.getName());
			}
		}
	}

	@Override
	public DBConnectionAlias load(Long id) {
		return persistorService.get(DBConnectionAlias.class, id);
	}

	@Override
	public List<DBConnectionAlias> list() {
		return persistorService.list(DBConnectionAlias.class);
	}

	@Override
	public List<DBConnectionAlias> search(DBConnectionAliasFVO filter, long pageIndex, long pageSize) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select ent")
			.addFrom(DBConnectionAlias.class, "ent")
			.applyFilter(DBConnectionAlias.class, "ent", filter)
			.list((pageIndex - 1) * pageSize, pageSize);
	}

	@Override
	public long count(DBConnectionAliasFVO filter) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select count(1)")
			.addFrom(DBConnectionAlias.class, "ent")
			.applyFilter(DBConnectionAlias.class, "ent", filter)
			.object();
	}

	@Override
	public List<DBConnection> getConnectionList() {
		return persistorService.list(DBConnection.class);
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
	public DBConnectionAlias loadByNameMode(String name, EAliasMode mode) {
		return persistorService.createQueryBuilder()
			.addFrom(DBConnectionAlias.class, "ent")
			.addWhere("and ent.name = :name", "name", name)
			.addWhere("and ent.mode = :mode", "mode", mode)
			.object();
	}

	@Override
	public DBConnectionAlias loadByConnMode(Long dbConnId, EAliasMode mode) {
		List<DBConnectionAlias> list = persistorService.createQueryBuilder()
			.addFrom(DBConnectionAlias.class, "ent")
			.addWhere("and ent.connection.id = :connId", "connId", dbConnId)
			.addWhere("and ent.mode = :mode", "mode", mode)
			.setOrderBy("ent.creationDate")
			.list();

		if (list.size() > 0) {
			return list.get(0);
		}

		return null;
	}
}