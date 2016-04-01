package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;
import com.thoughtworks.xstream.annotations.XStreamOmitField;

@XStreamAlias("field")
public class XDSField extends XDSAbstractField {
	@Deprecated
	@XStreamAsAttribute
	private String format;

	@Deprecated
	@XStreamAsAttribute
	private Boolean inFilterPanel;

	@Deprecated
	@XStreamAsAttribute
	private XDSFieldResultType resultType;

	@XStreamAsAttribute
	private Boolean isKeyField;

	@XStreamAsAttribute
	private Boolean isTitleField;

	@XStreamAsAttribute
	private Boolean isSelfRelPointerField;

	// -- Volatile Fields

	@Deprecated
	@XStreamOmitField
	private String dbType;

	@Deprecated
	@XStreamOmitField
	private Integer dbSize;

	@Deprecated
	public String getFormat() {
		return format;
	}

	@Deprecated
	public XDSField setFormat(String format) {
		this.format = format;
		return this;
	}

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
	public XDSFieldResultType getResultType() {
		return resultType != null ? resultType : XDSFieldResultType.Shown;
	}

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

	// ---------------------- VOLATILE PROPERTIES

	@Deprecated
	public String getDbType() {
		return dbType;
	}

	@Deprecated
	public XDSField setDbType(String dbType) {
		this.dbType = dbType;
		return this;
	}

	@Deprecated
	public Integer getDbSize() {
		return dbSize;
	}

	@Deprecated
	public XDSField setDbSize(Integer dbSize) {
		this.dbSize = dbSize;
		return this;
	}
}
