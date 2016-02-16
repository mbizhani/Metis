package org.devocative.metis.entity.dataSource.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;
import com.thoughtworks.xstream.annotations.XStreamOmitField;

@XStreamAlias("field")
public class XDSField extends XDSAbstractField {
	@XStreamAsAttribute
	private String format;

	@XStreamAsAttribute
	private Boolean inFilterPanel;

	@XStreamAsAttribute
	private XDSFieldResultType resultType;

	@XStreamAsAttribute
	private Boolean isKeyField;

	@XStreamAsAttribute
	private Boolean isTitleField;

	@XStreamAsAttribute
	private Boolean isSelfRelPointerField;

	// -- Volatile Fields

	@XStreamOmitField
	private String dbType;

	@XStreamOmitField
	private Integer dbSize;

	public String getFormat() {
		return format;
	}

	public void setFormat(String format) {
		this.format = format;
	}

	public Boolean getInFilterPanel() {
		return inFilterPanel;
	}

	public void setInFilterPanel(Boolean inFilterPanel) {
		this.inFilterPanel = inFilterPanel;
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

	public void setIsKeyField(Boolean isKeyField) {
		this.isKeyField = isKeyField;
	}

	public Boolean getIsTitleField() {
		return isTitleField;
	}

	public void setIsTitleField(Boolean isTitleField) {
		this.isTitleField = isTitleField;
	}

	public Boolean getIsSelfRelPointerField() {
		return isSelfRelPointerField;
	}

	public void setIsSelfRelPointerField(Boolean isSelfRelPointerField) {
		this.isSelfRelPointerField = isSelfRelPointerField;
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
}
