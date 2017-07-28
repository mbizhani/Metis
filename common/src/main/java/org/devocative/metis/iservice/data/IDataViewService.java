package org.devocative.metis.iservice.data;

import org.devocative.demeter.entity.User;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.XDataView;
import org.devocative.metis.vo.filter.data.DataViewFVO;

import java.io.InputStream;
import java.util.List;

public interface IDataViewService {
	String CACHE_KEY = "MTS_DATA_VIEW";

	// ---------------

	void saveOrUpdate(DataView entity);

	DataView load(Long id);

	DataView loadByName(String name);

	List<DataView> list();

	List<DataView> search(DataViewFVO filter, long pageIndex, long pageSize);

	long count(DataViewFVO filter);

	List<DataSource> getDataSourceList();

	List<DataGroup> getGroupsList();

	List<User> getCreatorUserList();

	List<User> getModifierUserList();

	// ==============================

	void saveOrUpdate(Long dataViewId, String title, XDataView xDataView, List<DataGroup> groups);

	List<String> listForOData();

	String exportAll();

	void importAll(InputStream stream);
}