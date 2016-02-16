package org.devocative.metis.entity.dataSource.config;

import com.thoughtworks.xstream.annotations.XStreamAsAttribute;
import com.thoughtworks.xstream.annotations.XStreamOmitField;
import org.devocative.metis.entity.dataSource.DataSource;

import java.io.Serializable;

public abstract class XDSAbstractField implements Serializable {
	@XStreamAsAttribute
	protected String name;

	@XStreamAsAttribute
	protected String title;

	@XStreamAsAttribute
	protected Boolean required;

	@XStreamAsAttribute
	protected XDSFieldType type;

	@XStreamAsAttribute
	protected XDSFieldFilterType filterType;

	@XStreamAsAttribute
	protected Long targetId;

	// -- Volatile Fields

	@XStreamOmitField
	protected DataSource target;

	public String getName() {
		return name != null ? name.toLowerCase() : null;
	}

	public XDSAbstractField setName(String name) {
		this.name = name;
		return this;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getSafeTitle() {
		return title != null ? title : getName();
	}

	// Has Default
	public Boolean getRequired() {
		return required != null ? required : false;
	}

	public XDSAbstractField setRequired(Boolean required) {
		this.required = required;
		return this;
	}

	public XDSFieldType getType() {
		return type;
	}

	public void setType(XDSFieldType type) {
		this.type = type;
	}

	// Has Default
	public XDSFieldFilterType getFilterType() {
		return filterType != null ? filterType : XDSFieldFilterType.Equal;
	}

	public XDSAbstractField setFilterType(XDSFieldFilterType filterType) {
		this.filterType = filterType;
		return this;
	}

	public Long getTargetId() {
		return targetId;
	}

	public void setTargetId(Long targetId) {
		this.targetId = targetId;
	}

	// ---------------------- VOLATILE PROPERTIES
	public DataSource getTarget() {
		return target;
	}

	public void setTarget(DataSource target) {
		this.target = target;
	}

	// ---------------------- Object METHODS

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof XDSAbstractField)) return false;

		XDSAbstractField field = (XDSAbstractField) o;

		return !(getName() != null ? !getName().equals(field.getName()) : field.getName() != null);

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
