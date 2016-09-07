package org.devocative.metis.vo.query;

import org.devocative.adroit.vo.KeyValueVO;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DbQueryRVO implements Serializable {
	private static final long serialVersionUID = -1129436995334750688L;

	private List<String> header = new ArrayList<>();

	private List<List<Object>> rows = new ArrayList<>();

	// ---------------

	private QueryExecInfoRVO queryExecInfo = new QueryExecInfoRVO();

	// ------------------------------

	public List<String> getHeader() {
		return header;
	}

	public void addHeader(String header) {
		this.header.add(header);
	}

	public void addRow(List<Object> row) {
		rows.add(row);
	}

	public List<List<Object>> getRows() {
		return rows;
	}

	// ---------------

	public QueryExecInfoRVO getQueryExecInfo() {
		return queryExecInfo;
	}

	// ------------------------------

	public DsQueryRVO<List<Map<String, Object>>> toListOfMap() {
		List<Map<String, Object>> list = new ArrayList<>();
		for (List<Object> row : rows) {
			Map<String, Object> rowAsMap = new HashMap<>();
			for (int i = 0; i < header.size(); i++) {
				rowAsMap.put(header.get(i), row.get(i));
			}
			list.add(rowAsMap);
		}

		return new DsQueryRVO<>(list, queryExecInfo);
	}

	public DsQueryRVO<List<KeyValueVO<Serializable, String>>> toListOfKeyValues() {
		List<KeyValueVO<Serializable, String>> list = new ArrayList<>();
		for (List<Object> row : rows) {
			KeyValueVO<Serializable, String> rowAsKeyValue = new KeyValueVO<>();
			rowAsKeyValue.setKey((Serializable) row.get(0));
			rowAsKeyValue.setValue(row.get(1) != null ? row.get(1).toString() : "");
			list.add(rowAsKeyValue);
		}

		return new DsQueryRVO<>(list, queryExecInfo);
	}
}
