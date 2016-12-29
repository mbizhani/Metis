package org.devocative.metis.iservice.data;

import org.devocative.demeter.entity.User;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.XDataView;
import org.devocative.metis.vo.filter.data.DataViewFVO;

import java.util.List;

public interface IDataViewService {
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

	XDataView getXDataView(DataView dataView);

	void saveOrUpdate(Long dataViewId, String title, XDataView xDataView, List<DataGroup> groups);

	List<String> listForOData();
}