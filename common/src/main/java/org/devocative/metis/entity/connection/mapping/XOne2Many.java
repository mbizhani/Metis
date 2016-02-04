package org.devocative.metis.entity.connection.mapping;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

@XStreamAlias("one2many")
public class XOne2Many extends XAbstractProperty {
	@XStreamAsAttribute
	private String manySideColumn;

	@XStreamAsAttribute
	private String manySideEntity;

	public String getManySideColumn() {
		return manySideColumn;
	}

	public void setManySideColumn(String manySideColumn) {
		this.manySideColumn = manySideColumn;
	}

	public String getManySideEntity() {
		return manySideEntity;
	}

	public void setManySideEntity(String manySideEntity) {
		this.manySideEntity = manySideEntity;
	}
}
