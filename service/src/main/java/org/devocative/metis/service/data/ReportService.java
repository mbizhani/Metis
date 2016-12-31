//overwrite
package org.devocative.metis.service.data;

import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.iservice.data.IReportService;
import org.devocative.metis.vo.filter.data.ReportFVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service("mtsReportService")
public class ReportService implements IReportService {

	@Autowired
	private IPersistorService persistorService;

	// ------------------------------

	@Override
	public void saveOrUpdate(Report entity) {
		persistorService.saveOrUpdate(entity);
	}

	@Override
	public Report load(Long id) {
		return persistorService.get(Report.class, id);
	}

	@Override
	public List<Report> list() {
		return persistorService.list(Report.class);
	}

	@Override
	public List<Report> search(ReportFVO filter, long pageIndex, long pageSize) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select ent")
			.addFrom(Report.class, "ent")
			.applyFilter(Report.class, "ent", filter)
			.list((pageIndex - 1) * pageSize, pageSize);
	}

	@Override
	public long count(ReportFVO filter) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select count(1)")
			.addFrom(Report.class, "ent")
			.applyFilter(Report.class, "ent", filter)
			.object();
	}

	@Override
	public List<DataView> getDataViewList() {
		return persistorService.list(DataView.class);
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
}