//overwrite
package org.devocative.metis.iservice.data;

import org.devocative.demeter.entity.User;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.vo.filter.data.ReportFVO;

import java.util.List;

public interface IReportService {
	void saveOrUpdate(Report entity);

	Report load(Long id);

	List<Report> list();

	List<Report> search(ReportFVO filter, long pageIndex, long pageSize);

	long count(ReportFVO filter);

	List<DataView> getDataViewList();

	List<DataGroup> getGroupsList();

	List<User> getCreatorUserList();

	List<User> getModifierUserList();

	// ==============================
}