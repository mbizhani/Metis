package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;
import java.util.List;

@XStreamAlias("dataView")
public class XDataView implements Serializable {
	@XStreamAsAttribute
	private String name;

	@XStreamAsAttribute
	private String dataSource;

	private List<XDVField> fields;

	private List<XDVDetail> details;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDataSource() {
		return dataSource;
	}

	public void setDataSource(String dataSource) {
		this.dataSource = dataSource;
	}

	public List<XDVField> getFields() {
		return fields;
	}

	public void setFields(List<XDVField> fields) {
		this.fields = fields;
	}

	public List<XDVDetail> getDetails() {
		return details;
	}

	public void setDetails(List<XDVDetail> details) {
		this.details = details;
	}
}
