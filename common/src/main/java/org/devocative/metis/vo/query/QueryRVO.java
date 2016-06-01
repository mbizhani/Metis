package org.devocative.metis.vo.query;

import org.devocative.adroit.vo.KeyValueVO;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class QueryRVO implements Serializable {
	private List<String> header = new ArrayList<>();

	private List<List<Object>> rows = new ArrayList<>();

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

	public List<Map<String, Object>> toListOfMap() {
		List<Map<String, Object>> result = new ArrayList<>();
		for (List<Object> row : rows) {
			Map<String, Object> rowAsMap = new HashMap<>();
			for (int i = 0; i < header.size(); i++) {
				rowAsMap.put(header.get(i), row.get(i));
			}
			result.add(rowAsMap);
		}
		return result;
	}

	public List<KeyValueVO<Serializable, String>> toListOfKeyValues() {
		List<KeyValueVO<Serializable, String>> result = new ArrayList<>();
		for (List<Object> row : rows) {
			KeyValueVO<Serializable, String> rowAsKeyValue = new KeyValueVO<>();
			rowAsKeyValue.setKey((Serializable) row.get(0));
			rowAsKeyValue.setValue(row.get(1) != null ? row.get(1).toString() : "");
			result.add(rowAsKeyValue);
		}
		return result;
	}
}
