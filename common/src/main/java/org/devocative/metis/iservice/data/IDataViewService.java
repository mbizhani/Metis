package org.devocative.metis.iservice.data;

import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.XDataView;
import org.devocative.metis.vo.filter.data.DataViewFVO;

import java.util.List;

public interface IDataViewService {
	DataView load(Long id);

	DataView loadByName(String name);

	XDataView getXDataView(DataView dataView);

	List<DataView> list();

	List<DataView> search(DataViewFVO filter, long pageIndex, long pageSize);

	long count(DataViewFVO filter);

	void saveOrUpdate(Long dataViewId, String title, XDataView xDataView);

	List<String> listForOData();
}
