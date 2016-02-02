package org.devocative.metis.entity.connection.mapping;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@XStreamAlias("schema")
public class XSchema implements Serializable {
	@XStreamAsAttribute
	private String name;

	private List<XTable> tables;

	// ------------------- ACCESSORS

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<XTable> getTables() {
		return tables;
	}

	public void setTables(List<XTable> tables) {
		this.tables = tables;
	}

	// ------------------- GENERAL METHODS

	public XTable findByFrom(String from) {
		for (XTable xTable : tables) {
			if (xTable.getFrom().equalsIgnoreCase(from)) {
				return xTable;
			}
		}
		return null;
	}

	public Map<String, Map<String, String>> getHierarchy() {
		Map<String, Map<String, String>> result = new HashMap<>();
		for (XTable xTable : tables) {
			Map<String, String> cols = new HashMap<>();
			for (XColumn xColumn : xTable.getColumns()) {
				cols.put(xColumn.getFrom(), null);
			}
			result.put(xTable.getFrom(), cols);
		}
		return result;
	}
}
