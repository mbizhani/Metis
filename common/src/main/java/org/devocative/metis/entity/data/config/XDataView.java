package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@XStreamAlias("dataView")
public class XDataView implements Serializable {
	private static final long serialVersionUID = 8271286562496660889L;

	// ---------------

	@XStreamAsAttribute
	private String name;

	@XStreamAsAttribute
	private Long dataSourceId;

	@XStreamAsAttribute
	private String dataSourceName;

	@XStreamAsAttribute
	private XDVGridSelectionMode selectionMode;

	//@XStreamAsAttribute
	//private XDVGridHeight gridHeight;

	// ---------------

	private String selectionValidationJS;

	private String rowStyler;

	// ---------------
	private List<XDVField> fields;

	private List<XDVParameter> params;

	private List<XDVLink> links;

	// ------------------------------

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

	/*public XDVGridHeight getGridHeight() {
		return gridHeight;
	}

	public void setGridHeight(XDVGridHeight gridHeight) {
		this.gridHeight = gridHeight;
	}*/

	public String getSelectionValidationJS() {
		return selectionValidationJS;
	}

	public void setSelectionValidationJS(String selectionValidationJS) {
		this.selectionValidationJS = selectionValidationJS;
	}

	public String getRowStyler() {
		return rowStyler;
	}

	public void setRowStyler(String rowStyler) {
		this.rowStyler = rowStyler;
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

	public List<XDVParameter> getParams() {
		if (params == null) {
			params = new ArrayList<>();
		}
		return params;
	}

	public void setParams(List<XDVParameter> params) {
		this.params = params;
	}

	public List<XDVLink> getLinks() {
		return links;
	}

	public void setLinks(List<XDVLink> links) {
		this.links = links;
	}
}
