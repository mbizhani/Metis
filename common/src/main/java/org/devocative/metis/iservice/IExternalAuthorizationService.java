package org.devocative.metis.iservice;

import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.data.Report;

import java.util.List;

public interface IExternalAuthorizationService {
	boolean authorizeReport(Report report, Long dbConnectionId, Long userId);

	void importedReports(List<String> importedReports, DBConnection dbConnection);
}
