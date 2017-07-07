package org.devocative.metis.service.data;

import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.cache.ICache;
import org.devocative.adroit.cache.IMissedHitHandler;
import org.devocative.adroit.xml.AdroitXStream;
import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.ICacheService;
import org.devocative.demeter.iservice.persistor.EJoinMode;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.XDataView;
import org.devocative.metis.iservice.data.IDataSourceService;
import org.devocative.metis.iservice.data.IDataViewService;
import org.devocative.metis.vo.filter.data.DataViewFVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.List;

@Service("mtsDataViewService")
public class DataViewService implements IDataViewService, IMissedHitHandler<Long, DataView> {
	private XStream xStream;
	private ICache<Long, DataView> dataViewCache;

	@Autowired
	private IPersistorService persistorService;

	@Autowired
	private ICacheService cacheService;

	@Autowired
	private IDataSourceService dataSourceService;

	@PostConstruct
	public void initDataViewService() {
		xStream = new AdroitXStream();
		xStream.processAnnotations(XDataView.class);

		dataViewCache = cacheService.create("MTS_DATA_VIEW", 50);
		dataViewCache.setMissedHitHandler(this);
	}

	// ------------------------------

	@Override
	public void saveOrUpdate(DataView entity) {
		persistorService.saveOrUpdate(entity);
	}

	@Override
	public DataView load(Long id) {
		return dataViewCache.get(id);
	}

	@Override
	public DataView loadByName(String name) {
		DataView dv = dataViewCache.findByProperty("name", name);
		if (dv == null) {
			dv = persistorService
				.createQueryBuilder()
				.addFrom(DataView.class, "ent")
				.addJoin("cfg", "ent.config", EJoinMode.LeftFetch)
				.addJoin("grp", "ent.groups", EJoinMode.LeftFetch)
				.addWhere("and ent.name = :name")
				.addParam("name", name)
				.object();

			if (dv != null) {
				dv.setXDataView((XDataView) xStream.fromXML(dv.getConfig().getValue()));
				dataViewCache.put(dv.getId(), dv);
			} else {
				throw new MetisException(MetisErrorCode.InvalidDataViewName, name);
			}
		}
		return dv;
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

	@Override
	public List<DataSource> getDataSourceList() {
		return persistorService.list(DataSource.class);
	}

	@Override
	public List<DataGroup> getGroupsList() {
		return persistorService.list(DataGroup.class);
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

	// IMissedHitHandler
	@Override
	public DataView loadForCache(Long key) {
		DataView dv = persistorService
			.createQueryBuilder()
			.addFrom(DataView.class, "ent")
			.addJoin("cfg", "ent.config", EJoinMode.LeftFetch)
			.addJoin("grp", "ent.groups", EJoinMode.LeftFetch)
			.addWhere("and ent.id = :id")
			.addParam("id", key)
			.object();
		dv.setXDataView((XDataView) xStream.fromXML(dv.getConfig().getValue()));
		return dv;
	}

	@Override
	public void saveOrUpdate(Long dataViewId, String title, XDataView xDataView, List<DataGroup> groups) {
		DataView dataView;
		ConfigLob config;

		if (dataViewId == null) {
			dataView = new DataView();
			config = new ConfigLob();
		} else {
			dataView = load(dataViewId);
			config = dataView.getConfig();
		}

		config.setValue(xStream.toXML(xDataView));

		dataView.setName(xDataView.getName());
		dataView.setTitle(title);
		dataView.setConfig(config);
		dataView.setDataSource(dataSourceService.load(xDataView.getDataSourceId()));
		dataView.setGroups(groups);

		persistorService.saveOrUpdate(config);
		persistorService.saveOrUpdate(dataView);

		dataView.setXDataView(xDataView);
		dataViewCache.update(dataView.getId(), dataView);
	}

	@Override
	public List<String> listForOData() {
		return persistorService
			.createQueryBuilder().addSelect("select ent.name")
			.addFrom(DataView.class, "ent")
			.list();
	}

}