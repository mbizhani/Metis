//overwrite
package org.devocative.metis.service.data;

import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.iservice.data.IDataGroupService;
import org.devocative.metis.vo.filter.data.DataGroupFVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service("mtsDataGroupService")
public class DataGroupService implements IDataGroupService {

	@Autowired
	private IPersistorService persistorService;

	// ------------------------------

	@Override
	public void saveOrUpdate(DataGroup entity) {
		persistorService.saveOrUpdate(entity);
	}

	@Override
	public DataGroup load(String id) {
		return persistorService.get(DataGroup.class, id);
	}

	@Override
	public DataGroup loadByName(String name) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DataGroup.class, "ent")
			.addWhere("and ent.name = :name")
			.addParam("name", name)
			.object();
	}

	@Override
	public List<DataGroup> list() {
		return persistorService.list(DataGroup.class);
	}

	@Override
	public List<DataGroup> search(DataGroupFVO filter, long pageIndex, long pageSize) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select ent")
			.addFrom(DataGroup.class, "ent")
			.applyFilter(DataGroup.class, "ent", filter)
			.list((pageIndex - 1) * pageSize, pageSize);
	}

	@Override
	public long count(DataGroupFVO filter) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select count(1)")
			.addFrom(DataGroup.class, "ent")
			.applyFilter(DataGroup.class, "ent", filter)
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
}