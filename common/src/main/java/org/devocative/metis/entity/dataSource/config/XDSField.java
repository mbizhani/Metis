package org.devocative.metis.entity.dataSource.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

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

	private String mapOpt;

	private String listOpt;

	private String sqlOpt;

	public String getName() {
		return name != null ? name.toLowerCase() : null;
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

	public String getMapOpt() {
		return mapOpt;
	}

	public XDSField setMapOpt(String mapOpt) {
		this.mapOpt = mapOpt;
		return this;
	}

	public String getListOpt() {
		return listOpt;
	}

	public XDSField setListOpt(String listOpt) {
		this.listOpt = listOpt;
		return this;
	}

	public String getSqlOpt() {
		return sqlOpt;
	}

	public XDSField setSqlOpt(String sqlOpt) {
		this.sqlOpt = sqlOpt;
		return this;
	}
}
