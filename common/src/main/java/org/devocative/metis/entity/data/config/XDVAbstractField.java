package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;

public class XDVAbstractField implements Serializable {
	private static final long serialVersionUID = 8412336560156364611L;

	@XStreamAsAttribute
	protected String name;

	@XStreamAsAttribute
	private Boolean inFilterPanel;

	@XStreamAsAttribute
	private Integer filterPanelOrder;

	/*@XStreamAsAttribute
	private Long targetDVId;

	@XStreamAsAttribute
	private String targetDVName;*/

	@XStreamAsAttribute
	private String targetDSFilter;

	// ------------------------------ ACCESSORS

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Boolean getInFilterPanel() {
		return inFilterPanel;
	}

	public void setInFilterPanel(Boolean inFilterPanel) {
		this.inFilterPanel = inFilterPanel;
	}

	public Integer getFilterPanelOrder() {
		return filterPanelOrder;
	}

	public void setFilterPanelOrder(Integer filterPanelOrder) {
		this.filterPanelOrder = filterPanelOrder;
	}

	/*public Long getTargetDVId() {
		return targetDVId;
	}

	public void setTargetDVId(Long targetDVId) {
		this.targetDVId = targetDVId;
	}

	public String getTargetDVName() {
		return targetDVName;
	}

	public void setTargetDVName(String targetDVName) {
		this.targetDVName = targetDVName;
	}*/

	public String getTargetDSFilter() {
		return targetDSFilter;
	}

	public void setTargetDSFilter(String targetDSFilter) {
		this.targetDSFilter = targetDSFilter;
	}

	// ------------------------------ OBJECT METHODS

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof XDVAbstractField)) return false;

		XDVAbstractField that = (XDVAbstractField) o;

		return !(getName() != null ? !getName().equals(that.getName()) : that.getName() != null);

	}

	@Override
	public int hashCode() {
		return getName() != null ? getName().hashCode() : 0;
	}

	@Override
	public String toString() {
		return getName();
	}
}
