package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;
import com.thoughtworks.xstream.annotations.XStreamConverter;
import org.devocative.metis.IgnoreFalseConverter;

@XStreamAlias("field")
public class XDSField extends XDSAbstractField {
	private static final long serialVersionUID = -4721484096163722504L;

	@XStreamAsAttribute
	private XDSFieldFilterType filterType;

	@XStreamAsAttribute
	@XStreamConverter(IgnoreFalseConverter.class)
	private Boolean isKeyField;

	@XStreamAsAttribute
	@XStreamConverter(IgnoreFalseConverter.class)
	private Boolean isTitleField;

	@XStreamAsAttribute
	@XStreamConverter(IgnoreFalseConverter.class)
	private Boolean isSelfRelPointerField;

	// ---------------------------------- ACCESSORS

	public XDSFieldFilterType getFilterType() {
		return filterType;
	}

	public XDSField setFilterType(XDSFieldFilterType filterType) {
		this.filterType = filterType;
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
