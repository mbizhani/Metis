package org.devocative.metis.iservice;

import org.devocative.metis.entity.data.Report;

public interface IExternalAuthorizationService {
	boolean authorizeReport(Report report, Long dbConnectionId, Long userId);
}
