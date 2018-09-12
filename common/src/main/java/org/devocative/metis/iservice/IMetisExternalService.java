package org.devocative.metis.iservice;

import org.devocative.adroit.sql.SqlHelper;
import org.devocative.adroit.sql.ei.ExportImportHelper;
import org.devocative.adroit.sql.plugin.INpsPlugin;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.data.Report;

import java.util.List;

public interface IMetisExternalService {
	boolean authorizeReport(Report report, Long dbConnectionId, Long userId);

	void afterImportedReports(List<String> importedReports, DBConnection dbConnection);

	void onExportReport(ExportImportHelper helper, SqlHelper sqlHelper, INpsPlugin... plugins);

	void onImportReport(ExportImportHelper helper, SqlHelper sqlHelper);
}
