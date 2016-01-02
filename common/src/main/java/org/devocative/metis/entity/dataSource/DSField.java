package org.devocative.metis.entity.dataSource;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;

@XStreamAlias("field")
public class DSField implements Serializable {
	@XStreamAsAttribute
	private String name;

	@XStreamAsAttribute
	private String title;

	@XStreamAsAttribute
	private DSFieldType type;

	@XStreamAsAttribute
	private String format;

	@XStreamAsAttribute
	private DSFieldFilterType filterType;

	@XStreamAsAttribute
	private DSFieldPlaceType placeType;

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

	public DSFieldType getType() {
		return type;
	}

	public void setType(DSFieldType type) {
		this.type = type;
	}

	public String getFormat() {
		return format;
	}

	public void setFormat(String format) {
		this.format = format;
	}

	// Has Default
	public DSFieldFilterType getFilterType() {
		return filterType != null ? filterType : DSFieldFilterType.Equal;
	}

	public DSField setFilterType(DSFieldFilterType filterType) {
		this.filterType = filterType;
		return this;
	}

	// Has Default
	public DSFieldPlaceType getPlaceType() {
		return placeType != null ? placeType : DSFieldPlaceType.Both;
	}

	public DSField setPlaceType(DSFieldPlaceType placeType) {
		this.placeType = placeType;
		return this;
	}

	public String getMapOpt() {
		return mapOpt;
	}

	public DSField setMapOpt(String mapOpt) {
		this.mapOpt = mapOpt;
		return this;
	}

	public String getListOpt() {
		return listOpt;
	}

	public DSField setListOpt(String listOpt) {
		this.listOpt = listOpt;
		return this;
	}

	public String getSqlOpt() {
		return sqlOpt;
	}

	public DSField setSqlOpt(String sqlOpt) {
		this.sqlOpt = sqlOpt;
		return this;
	}
}
