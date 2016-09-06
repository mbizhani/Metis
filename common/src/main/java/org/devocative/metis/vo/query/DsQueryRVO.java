package org.devocative.metis.vo.query;

import java.io.Serializable;

public class DsQueryRVO<T> implements Serializable {
	private static final long serialVersionUID = -8617228035127201395L;

	private T result;

	private QueryExecInfoRVO queryExecInfo;

	// ------------------------------

	public DsQueryRVO(T result) {
		this.result = result;
	}

	public DsQueryRVO(T result, QueryExecInfoRVO queryExecInfo) {
		this.result = result;
		this.queryExecInfo = queryExecInfo;
	}

	// ------------------------------

	public T getResult() {
		return result;
	}

	public QueryExecInfoRVO getQueryExecInfo() {
		return queryExecInfo;
	}

	public void setQueryExecInfo(QueryExecInfoRVO queryExecInfo) {
		this.queryExecInfo = queryExecInfo;
	}
}
