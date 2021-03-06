package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

@XStreamAlias("field")
public class XDSField extends XDSAbstractField {
	private static final long serialVersionUID = -4721484096163722504L;

	@XStreamAsAttribute
	private Boolean isKeyField;

	@XStreamAsAttribute
	private Boolean isTitleField;

	@XStreamAsAttribute
	private Boolean isSelfRelPointerField;

	// ---------------------------------- ACCESSORS

	public Boolean getIsKeyField() {
		return isKeyField;
	}

	public XDSField setIsKeyField(Boolean isKeyField) {
		this.isKeyField = isKeyField;
		return this;
	}

	public Boolean getIsTitleField() {
		return isTitleField;
	}

	public XDSField setIsTitleField(Boolean isTitleField) {
		this.isTitleField = isTitleField;
		return this;
	}

	public Boolean getIsSelfRelPointerField() {
		return isSelfRelPointerField;
	}

	public XDSField setIsSelfRelPointerField(Boolean isSelfRelPointerField) {
		this.isSelfRelPointerField = isSelfRelPointerField;
		return this;
	}
}
