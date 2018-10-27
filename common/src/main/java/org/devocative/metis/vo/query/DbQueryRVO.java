package org.devocative.metis.vo.query;

import org.devocative.adroit.sql.result.QueryVO;
import org.devocative.adroit.vo.KeyValueVO;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class DbQueryRVO implements Serializable {
	private static final long serialVersionUID = -1129436995334750688L;

	private QueryVO queryVO;

	// ---------------

	private QueryExecInfoRVO queryExecInfo = new QueryExecInfoRVO();

	// ------------------------------

	public DbQueryRVO setQueryVO(QueryVO queryVO) {
		this.queryVO = queryVO;
		return this;
	}

	public List<String> getHeader() {
		return queryVO.getHeader();
	}

	public List<List<Object>> getRows() {
		return queryVO.getRows();
	}

	// ---------------

	public QueryExecInfoRVO getQueryExecInfo() {
		return queryExecInfo;
	}

	// ------------------------------

	public DsQueryRVO<List<Map<String, Object>>> toListOfMap() {
		return new DsQueryRVO<>(new ArrayList<>(queryVO.toListOfMap()), queryExecInfo);
	}

	public DsQueryRVO<List<KeyValueVO<Serializable, String>>> toListOfKeyValues() {
		List<KeyValueVO<Serializable, String>> list = getRows()
			.stream()
			.filter(it -> it.size() > 1)
			.map(it -> new KeyValueVO<>((Serializable) it.get(0), it.get(1) != null ? it.get(1).toString() : ""))
			.collect(Collectors.toList());

		return new DsQueryRVO<>(list, queryExecInfo);
	}
}
