package org.devocative.metis.vo.query;

import org.devocative.adroit.sql.NamedParameterStatement;

import java.io.Serializable;
import java.util.Map;

public class QueryExecInfoRVO implements Serializable {
	private static final long serialVersionUID = -1236246614681461307L;

	private String finalSQL;

	private Map<Integer, Object> finalParams;

	private Long duration;

	private String exception;

	private String dbConnName;

	// ------------------------------

	public String getFinalSQL() {
		return finalSQL;
	}

	public Map<Integer, Object> getFinalParams() {
		return finalParams;
	}

	public Long getDuration() {
		return duration;
	}

	public QueryExecInfoRVO setDuration(Long duration) {
		this.duration = duration;
		return this;
	}

	public String getException() {
		return exception;
	}

	public QueryExecInfoRVO setException(Exception exception) {
		this.exception = exception.getMessage();
		return this;
	}

	public String getDbConnName() {
		return dbConnName;
	}

	public QueryExecInfoRVO setDbConnName(String dbConnName) {
		this.dbConnName = dbConnName;
		return this;
	}

	// ------------------------------

	public QueryExecInfoRVO fromNamedParameterStatement(NamedParameterStatement nps) {
		if (nps != null) {
			finalSQL = nps.getFinalIndexedQuery();
			finalParams = nps.getFinalParams();
		}
		return this;
	}
}
