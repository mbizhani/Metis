package org.devocative.metis.service;

import com.thoughtworks.xstream.XStream;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.XDataView;
import org.devocative.metis.iservice.IDataViewService;
import org.devocative.metis.vo.filter.DataViewFVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service("mtsDataViewService")
public class DataViewService implements IDataViewService {
	private XStream xStream;

	@Autowired
	private IPersistorService persistorService;

	{
		xStream = new XStream();
		xStream.processAnnotations(XDataView.class);
	}

	// ---------------------- PUBLIC METHODS

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
		dataView.setDataSource(new DataSource(xDataView.getDataSourceId()));

		persistorService.saveOrUpdate(config);
		persistorService.saveOrUpdate(dataView);
	}

	// ---------------------- PRIVATE METHODS

	private Long loadConfigId(Long dataViewId) {
		if (dataViewId != null) {
			return persistorService
				.createQueryBuilder()
				.addSelect("select ent.config.id")
				.addFrom(DataView.class, "ent")
				.addWhere("and ent.id = :id")
				.addParam("id", dataViewId)
				.object();
		}
		return null;
	}

}
