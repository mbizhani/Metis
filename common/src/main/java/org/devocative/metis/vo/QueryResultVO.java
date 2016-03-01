package org.devocative.metis.vo;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class QueryResultVO implements Serializable {
	private List<String> header = new ArrayList<>();

	private List<List<String>> rows = new ArrayList<>();

	public List<String> getHeader() {
		return header;
	}

	public void addHeader(String header) {
		this.header.add(header);
	}

	public void addRow(List<String> row) {
		rows.add(row);
	}

	public List<List<String>> getRows() {
		return rows;
	}
}
