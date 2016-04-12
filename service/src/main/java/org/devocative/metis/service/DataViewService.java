package org.devocative.metis.service;

import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.iservice.IDataViewService;
import org.devocative.metis.vo.filter.DataViewFVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service("mtsDataViewService")
public class DataViewService implements IDataViewService {
	@Autowired
	private IPersistorService persistorService;

	@Override
	public DataView load(Long id) {
		return persistorService.get(DataView.class, id);
	}

	@Override
	public DataView loadByName(String name) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DataView.class, "ent")
			.addWhere("and ent.name = :name")
			.addParam("name", name)
			.object();
	}

	@Override
	public List<DataView> list() {
		return persistorService.list(DataView.class);
	}

	@Override
	public List<DataView> search(DataViewFVO filter, long pageIndex, long pageSize) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select ent")
			.addFrom(DataView.class, "ent")
			.applyFilter(DataView.class, "ent", filter)
			.list((pageIndex - 1) * pageSize, pageSize);
	}

	@Override
	public long count(DataViewFVO filter) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select count(1)")
			.addFrom(DataView.class, "ent")
			.applyFilter(DataView.class, "ent", filter)
			.object();
	}
}
