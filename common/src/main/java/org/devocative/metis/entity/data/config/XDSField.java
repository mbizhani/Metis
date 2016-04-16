package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;
import com.thoughtworks.xstream.annotations.XStreamConverter;
import org.devocative.metis.IgnoreFalseConverter;

@XStreamAlias("field")
public class XDSField extends XDSAbstractField {
	@Deprecated
	@XStreamAsAttribute
	@XStreamConverter(IgnoreFalseConverter.class)
	private Boolean inFilterPanel;

	@Deprecated
	@XStreamAsAttribute
	private XDSFieldResultType resultType;

	@XStreamAsAttribute
	@XStreamConverter(IgnoreFalseConverter.class)
	private Boolean isKeyField;

	@XStreamAsAttribute
	@XStreamConverter(IgnoreFalseConverter.class)
	private Boolean isTitleField;

	@XStreamAsAttribute
	@XStreamConverter(IgnoreFalseConverter.class)
	private Boolean isSelfRelPointerField;

	@Deprecated
	public Boolean getInFilterPanel() {
		return inFilterPanel;
	}

	@Deprecated
	public XDSField setInFilterPanel(Boolean inFilterPanel) {
		this.inFilterPanel = inFilterPanel;
		return this;
	}

	// Has Default
	@Deprecated
	public XDSFieldResultType getResultType() {
		return resultType;
	}

	@Deprecated
	public XDSField setResultType(XDSFieldResultType resultType) {
		this.resultType = resultType;
		return this;
	}

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
