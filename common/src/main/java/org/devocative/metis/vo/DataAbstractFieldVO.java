package org.devocative.metis.vo;

import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.config.XDSFieldFilterType;
import org.devocative.metis.entity.data.config.XDSFieldType;

import java.io.Serializable;

public abstract class DataAbstractFieldVO implements Serializable {
	private String name;

	private String title;

	private Boolean required;

	private XDSFieldType type;

	private XDSFieldFilterType filterType;

	private Long targetId;

	// --------------------- ACCESSORS

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public Boolean getRequired() {
		return required;
	}

	public void setRequired(Boolean required) {
		this.required = required;
	}

	public XDSFieldType getType() {
		return type;
	}

	public void setType(XDSFieldType type) {
		this.type = type;
	}

	public XDSFieldFilterType getFilterType() {
		return filterType;
	}

	public void setFilterType(XDSFieldFilterType filterType) {
		this.filterType = filterType;
	}

	public Long getTargetId() {
		return targetId;
	}

	public void setTargetId(Long targetId) {
		this.targetId = targetId;
	}

	public DataSource getTarget() {
		return new DataSource(getTargetId());
	}

	public void setTarget(DataSource target) {
		setTargetId(target.getId());
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof DataAbstractFieldVO)) return false;

		DataAbstractFieldVO that = (DataAbstractFieldVO) o;

		return !(getName() != null ? !getName().equals(that.getName()) : that.getName() != null);

	}

	@Override
	public int hashCode() {
		return getName() != null ? getName().hashCode() : 0;
	}
}
