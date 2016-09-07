package org.devocative.metis.vo.query;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class DsQueryRVO<T> implements Serializable {
	private static final long serialVersionUID = -8617228035127201395L;

	private T result;

	private List<QueryExecInfoRVO> queryExecInfoList = new ArrayList<>();

	// ------------------------------

	public DsQueryRVO(T result) {
		this.result = result;
	}

	public DsQueryRVO(T result, QueryExecInfoRVO queryExecInfo) {
		this.result = result;
		this.queryExecInfoList.add(queryExecInfo);
	}

	public DsQueryRVO(T result, List<QueryExecInfoRVO> queryExecInfoList) {
		this.result = result;
		this.queryExecInfoList.addAll(queryExecInfoList);
	}

	// ------------------------------

	public T getResult() {
		return result;
	}

	public List<QueryExecInfoRVO> getQueryExecInfoList() {
		return queryExecInfoList;
	}

	// ------------------------------

	public void addQueryExecInfo(QueryExecInfoRVO queryExecInfoRVO) {
		queryExecInfoList.add(queryExecInfoRVO);
	}

	public void addQueryExecInfo(int index, QueryExecInfoRVO queryExecInfoRVO) {
		queryExecInfoList.add(index, queryExecInfoRVO);
	}
}
