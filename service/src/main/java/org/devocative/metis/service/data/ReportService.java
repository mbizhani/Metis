package org.devocative.metis.service.data;

import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.iservice.persistor.EJoinMode;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.iservice.IExternalAuthorizationService;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.iservice.data.IReportService;
import org.devocative.metis.vo.filter.data.ReportFVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

@Service("mtsReportService")
public class ReportService implements IReportService {

	@Autowired
	private IPersistorService persistorService;

	@Autowired
	private IDBConnectionService dbConnectionService;

	@Autowired
	private ISecurityService securityService;

	@Autowired(required = false)
	private IExternalAuthorizationService externalAuthorizationService;

	// ------------------------------

	@Override
	public void saveOrUpdate(Report entity) {
		persistorService.saveOrUpdate(entity);
	}

	@Override
	public Report load(String id) {
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

	@Override
	public Map<DataGroup, List<Report>> listPerGroup(String sentDBConnection) {
		List<Report> reports = persistorService
			.createQueryBuilder()
			.addSelect("select distinct ent")
			.addFrom(Report.class, "ent")
			.addJoin("grp", "ent.groups", EJoinMode.LeftFetch)
			.setOrderBy("ent.title")
			.list();

		Long dbConnectionId = null;
		if (sentDBConnection != null) {
			final DBConnection dbConnection = dbConnectionService.loadByName(sentDBConnection);
			if (dbConnection != null) {
				dbConnectionId = dbConnection.getId();
			}
		}

		final Long userId = securityService.getCurrentUser().getUserId();

		Map<DataGroup, List<Report>> result = new TreeMap<>();
		for (Report report : reports) {
			if (externalAuthorizationService == null ||
				externalAuthorizationService.authorizeReport(report, dbConnectionId, userId)) {
				for (DataGroup group : report.getGroups()) {
					if (!result.containsKey(group)) {
						result.put(group, new ArrayList<>());
					}
					result.get(group).add(report);
				}
			}
		}

		return result;
	}

	@Override
	public void assertReportAuthorization(Report report, String sentDBConnection) {
		Long dbConnectionId = null;
		if (sentDBConnection != null) {
			final DBConnection dbConnection = dbConnectionService.loadByName(sentDBConnection);
			if (dbConnection != null) {
				dbConnectionId = dbConnection.getId();
			}
		}

		final Long userId = securityService.getCurrentUser().getUserId();

		if (externalAuthorizationService != null &&
			!externalAuthorizationService.authorizeReport(report, dbConnectionId, userId)) {
			throw new MetisException(MetisErrorCode.ReportAccessDenied, report.getTitle());
		}

	}
}