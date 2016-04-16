package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAsAttribute;
import com.thoughtworks.xstream.annotations.XStreamConverter;
import org.devocative.metis.IgnoreFalseConverter;

import java.io.Serializable;

public abstract class XDSAbstractField implements Serializable {
	@XStreamAsAttribute
	protected String name;

	@XStreamAsAttribute
	protected String title;

	@XStreamAsAttribute
	@XStreamConverter(IgnoreFalseConverter.class)
	protected Boolean required;

	@XStreamAsAttribute
	protected XDSFieldType type;

	@XStreamAsAttribute
	protected XDSFieldFilterType filterType;

	@Deprecated
	@XStreamAsAttribute
	protected Long targetId;

	@XStreamAsAttribute
	protected Long targetDSId;

	@XStreamAsAttribute
	protected String targetDSName;

	public String getName() {
		return name;
	}

	public XDSAbstractField setName(String name) {
		this.name = name;
		return this;
	}

	public String getTitle() {
		return title;
	}

	public XDSAbstractField setTitle(String title) {
		this.title = title;
		return this;
	}

	public String getSafeTitle() {
		return title != null ? title : getName();
	}

	public Boolean getRequired() {
		return required;
	}

	public XDSAbstractField setRequired(Boolean required) {
		this.required = required;
		return this;
	}

	public XDSFieldType getType() {
		return type;
	}

	public XDSAbstractField setType(XDSFieldType type) {
		this.type = type;
		return this;
	}

	public XDSFieldFilterType getFilterType() {
		return filterType;
	}

	public XDSAbstractField setFilterType(XDSFieldFilterType filterType) {
		this.filterType = filterType;
		return this;
	}

	@Deprecated
	public Long getTargetId() {
		return targetId;
	}

	@Deprecated
	public XDSAbstractField setTargetId(Long targetId) {
		this.targetId = targetId;
		return this;
	}

	public Long getTargetDSId() {
		return targetDSId;
	}

	public XDSAbstractField setTargetDSId(Long targetDSId) {
		this.targetDSId = targetDSId;
		return this;
	}

	public String getTargetDSName() {
		return targetDSName;
	}

	public XDSAbstractField setTargetDSName(String targetDSName) {
		this.targetDSName = targetDSName;
		return this;
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
