package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@XStreamAlias("dataView")
public class XDataView implements Serializable {
	@XStreamAsAttribute
	private String name;

	@XStreamAsAttribute
	private Long dataSourceId;

	@XStreamAsAttribute
	private String dataSourceName;

	@XStreamAsAttribute
	private XDVGridSelectionMode selectionMode;

	@XStreamAsAttribute
	private XDVGridHeight gridHeight;

	private List<XDVField> fields;

	private List<XDVDetail> details;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Long getDataSourceId() {
		return dataSourceId;
	}

	public void setDataSourceId(Long dataSourceId) {
		this.dataSourceId = dataSourceId;
	}

	public String getDataSourceName() {
		return dataSourceName;
	}

	public void setDataSourceName(String dataSourceName) {
		this.dataSourceName = dataSourceName;
	}

	public XDVGridSelectionMode getSelectionMode() {
		return selectionMode;
	}

	public void setSelectionMode(XDVGridSelectionMode selectionMode) {
		this.selectionMode = selectionMode;
	}

	public XDVGridHeight getGridHeight() {
		return gridHeight;
	}

	public void setGridHeight(XDVGridHeight gridHeight) {
		this.gridHeight = gridHeight;
	}

	public List<XDVField> getFields() {
		if (fields == null) {
			fields = new ArrayList<>();
		}
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
