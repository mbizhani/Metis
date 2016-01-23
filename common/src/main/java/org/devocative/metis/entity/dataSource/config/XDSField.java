package org.devocative.metis.entity.dataSource.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;
import com.thoughtworks.xstream.annotations.XStreamOmitField;

import java.io.Serializable;

@XStreamAlias("field")
public class XDSField implements Serializable {
	@XStreamAsAttribute
	private String name;

	@XStreamAsAttribute
	private String title;

	@XStreamAsAttribute
	private XDSFieldType type;

	@XStreamAsAttribute
	private String format;

	@XStreamAsAttribute
	private XDSFieldFilterType filterType;

	@XStreamAsAttribute
	private XDSFieldPlaceType placeType;

	@XStreamOmitField
	private String dbType;

	@XStreamOmitField
	private Integer dbSize;

	public String getName() {
		return name != null ? name.toLowerCase() : null;
	}

	public XDSField setName(String name) {
		this.name = name;
		return this;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public XDSFieldType getType() {
		return type;
	}

	public void setType(XDSFieldType type) {
		this.type = type;
	}

	public String getFormat() {
		return format;
	}

	public void setFormat(String format) {
		this.format = format;
	}

	// Has Default
	public XDSFieldFilterType getFilterType() {
		return filterType != null ? filterType : XDSFieldFilterType.Equal;
	}

	public XDSField setFilterType(XDSFieldFilterType filterType) {
		this.filterType = filterType;
		return this;
	}

	// Has Default
	public XDSFieldPlaceType getPlaceType() {
		return placeType != null ? placeType : XDSFieldPlaceType.Both;
	}

	public XDSField setPlaceType(XDSFieldPlaceType placeType) {
		this.placeType = placeType;
		return this;
	}

	// ---------------------- VOLATILE PROPERTIES

	public String getDbType() {
		return dbType;
	}

	public XDSField setDbType(String dbType) {
		this.dbType = dbType;
		return this;
	}

	public Integer getDbSize() {
		return dbSize;
	}

	public XDSField setDbSize(Integer dbSize) {
		this.dbSize = dbSize;
		return this;
	}


	// ---------------------- Object METHODS

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof XDSField)) return false;

		XDSField field = (XDSField) o;

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
