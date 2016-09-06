package org.devocative.metis.vo.query;

import java.io.Serializable;
import java.util.Map;

public class QueryExecInfoRVO implements Serializable {
	private static final long serialVersionUID = -1236246614681461307L;

	private String finalSQL;

	private Map<Integer, Object> finalParams;

	private Long duration;

	private String exception;

	// ------------------------------

	public String getFinalSQL() {
		return finalSQL;
	}

	public void setFinalSQL(String finalSQL) {
		this.finalSQL = finalSQL;
	}

	public Map<Integer, Object> getFinalParams() {
		return finalParams;
	}

	public void setFinalParams(Map<Integer, Object> finalParams) {
		this.finalParams = finalParams;
	}

	public Long getDuration() {
		return duration;
	}

	public void setDuration(Long duration) {
		this.duration = duration;
	}

	public String getException() {
		return exception;
	}

	public void setException(String exception) {
		this.exception = exception;
	}
}