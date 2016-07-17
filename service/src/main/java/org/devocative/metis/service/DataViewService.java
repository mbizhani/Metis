package org.devocative.metis.service;

import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.cache.ICache;
import org.devocative.adroit.cache.IMissedHitHandler;
import org.devocative.demeter.iservice.ICacheService;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.XDataView;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.iservice.IDataViewService;
import org.devocative.metis.vo.filter.DataViewFVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.List;

@Service("mtsDataViewService")
public class DataViewService implements IDataViewService, IMissedHitHandler<String, DataView> {
	private XStream xStream;
	private ICache<String, DataView> dataViewCache;

	@Autowired
	private IPersistorService persistorService;

	@Autowired
	private ICacheService cacheService;

	@Autowired
	private IDataSourceService dataSourceService;

	@PostConstruct
	public void initDataViewService() {
		xStream = new XStream();
		xStream.processAnnotations(XDataView.class);

		dataViewCache = cacheService.create("MTS_DATA_VIEW", 50);
		dataViewCache.setMissedHitHandler(this);
	}

	// ------------------------------ PUBLIC METHODS

	@Override
	public DataView load(Long id) {
		DataView dv = dataViewCache.findByProperty("id", id);
		if (dv == null) {
			dv = persistorService.get(DataView.class, id);
			dataViewCache.put(dv.getName(), dv);
		}
		return dv;
	}

	@Override
	public DataView loadByName(String name) {
		return dataViewCache.get(name);
	}

	// IMissedHitHandler
	@Override
	public DataView loadForCache(String key) {
		return persistorService
			.createQueryBuilder()
			.addFrom(DataView.class, "ent")
			.addWhere("and ent.name = :name")
			.addParam("name", key)
			.object();
	}

	@Override
	public XDataView getXDataView(DataView dataView) {
		return (XDataView) xStream.fromXML(dataView.getConfig().getValue());
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
	public void saveOrUpdate(Long dataViewId, String title, XDataView xDataView) {
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

		persistorService.saveOrUpdate(config);
		persistorService.saveOrUpdate(dataView);

		dataViewCache.update(dataView.getName(), dataView);
	}

	@Override
	public List<String> listForOData() {
		return persistorService
			.createQueryBuilder().addSelect("select ent.name")
			.addFrom(DataView.class, "ent")
			.list();
	}

	// ------------------------------ PRIVATE METHODS
}
