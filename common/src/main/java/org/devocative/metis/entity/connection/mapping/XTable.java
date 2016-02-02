package org.devocative.metis.entity.connection.mapping;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;
import java.util.List;

@XStreamAlias("table")
public class XTable implements Serializable {
	@XStreamAsAttribute
	private String from;

	@XStreamAsAttribute
	private String to;

	private List<XColumn> columns;

	// ------------------- ACCESSORS

	public String getFrom() {
		return from;
	}

	public void setFrom(String from) {
		this.from = from;
	}

	public String getTo() {
		return to;
	}

	public void setTo(String to) {
		this.to = to;
	}

	public List<XColumn> getColumns() {
		return columns;
	}

	public void setColumns(List<XColumn> columns) {
		this.columns = columns;
	}

	// ------------------- GENERAL METHODS

	public XColumn findColumnByFrom(String from) {
		for (XColumn xColumn : columns) {
			if (xColumn.getFrom().equalsIgnoreCase(from)) {
				return xColumn;
			}
		}

		return null;
	}
}
