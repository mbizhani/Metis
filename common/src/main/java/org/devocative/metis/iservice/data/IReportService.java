package org.devocative.metis.iservice.data;

import org.devocative.demeter.entity.User;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.vo.filter.data.ReportFVO;

import java.util.List;
import java.util.Map;

public interface IReportService {
	void saveOrUpdate(Report entity);

	Report load(String id);

	List<Report> list();

	List<Report> search(ReportFVO filter, long pageIndex, long pageSize);

	long count(ReportFVO filter);

	List<DataView> getDataViewList();

	List<DataGroup> getGroupsList();

	List<User> getCreatorUserList();

	List<User> getModifierUserList();

	// ==============================

	Map<DataGroup, List<Report>> listPerGroup(String sentDBConnection);

	void assertReportAuthorization(Report report, String sentDBConnection);
}