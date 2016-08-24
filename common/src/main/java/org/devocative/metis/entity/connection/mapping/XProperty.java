package org.devocative.metis.entity.connection.mapping;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

@XStreamAlias("property")
public class XProperty extends XAbstractProperty {
	private static final long serialVersionUID = 1315318236170000003L;

	@XStreamAsAttribute
	private String column;

	@XStreamAsAttribute
	private Boolean isId;

	public String getColumn() {
		return column;
	}

	public void setColumn(String column) {
		this.column = column;
	}

	public Boolean getIsId() {
		return isId != null ? isId : false;
	}

	public void setIsId(Boolean isId) {
		this.isId = isId;
	}
}
