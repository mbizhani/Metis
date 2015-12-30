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
	private DSFieldFilterType filterType = DSFieldFilterType.Equal;

	@XStreamAsAttribute
	private DSFieldPlaceType placeType = DSFieldPlaceType.Both;

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

	public DSFieldFilterType getFilterType() {
		return filterType;
	}

	public DSField setFilterType(DSFieldFilterType filterType) {
		this.filterType = filterType;
		return this;
	}
}
