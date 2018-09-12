package org.devocative.metis.service.data;

import com.ibm.icu.text.Collator;
import com.ibm.icu.util.ULocale;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.iservice.persistor.EJoinMode;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.demeter.vo.UserVO;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.iservice.IMetisExternalService;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.iservice.data.IReportService;
import org.devocative.metis.vo.filter.data.ReportFVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service("mtsReportService")
public class ReportService implements IReportService {

	@Autowired
	private IPersistorService persistorService;

	@Autowired
	private IDBConnectionService dbConnectionService;

	@Autowired
	private ISecurityService securityService;

	@Autowired(required = false)
	private IMetisExternalService metisExternalService;

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
		final List<Report> reports = persistorService
			.createQueryBuilder()
			.addSelect("select distinct ent")
			.addFrom(Report.class, "ent")
			.addJoin("grp", "ent.groups", EJoinMode.LeftFetch)
			.setOrderBy("ent.title")
			.list();

		final Long dbConnectionId;
		if (sentDBConnection != null) {
			final DBConnection dbConnection = dbConnectionService.loadByName(sentDBConnection);
			if (dbConnection != null) {
				dbConnectionId = dbConnection.getId();
			} else {
				dbConnectionId = null;
			}
		} else {
			dbConnectionId = null;
		}

		final UserVO currentUser = securityService.getCurrentUser();
		final LocaleStringComparator<DataGroup> ldc = new LocaleStringComparator<>(currentUser.getLocale().getCode(), DataGroup::getName);

		return reports.parallelStream()
			.filter(report ->
				metisExternalService == null ||
					metisExternalService.authorizeReport(report, dbConnectionId, currentUser.getUserId())
			)
			.flatMap(report -> report.getGroups().stream().map(dataGroup -> new KeyValueVO<>(dataGroup, report)))
			.collect(Collectors.groupingBy(
				KeyValueVO::getKey,
				() -> new TreeMap<>(ldc),
				Collectors.mapping(
					KeyValueVO::getValue,
					Collectors.toList()))
			);
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

		if (metisExternalService != null &&
			!metisExternalService.authorizeReport(report, dbConnectionId, userId)) {
			throw new MetisException(MetisErrorCode.ReportAccessDenied, report.getTitle());
		}

	}

	// ------------------------------

	interface AFunction<T, R> extends Function<T, R>, Serializable {
	}

	private static class LocaleStringComparator<T> implements Comparator<T>, Serializable {
		private static final long serialVersionUID = 61690144572014573L;

		private final String locale;
		private final AFunction<T, String> stringExtractor;

		// ------------------------------

		LocaleStringComparator(String locale, AFunction<T, String> stringExtractor) {
			this.locale = locale;
			this.stringExtractor = stringExtractor;
		}

		// ------------------------------

		@Override
		public int compare(T o1, T o2) {
			Collator collator = Collator.getInstance(new ULocale(locale));
			return collator.compare(stringExtractor.apply(o1), stringExtractor.apply(o2));
		}
	}

}