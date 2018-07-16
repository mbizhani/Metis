package org.devocative.metis.vo;

import org.devocative.adroit.ObjectUtil;
import org.devocative.metis.entity.data.config.XDSField;
import org.devocative.metis.entity.data.config.XDSFieldResultType;
import org.devocative.metis.entity.data.config.XDVAggregatorFunction;
import org.devocative.metis.entity.data.config.XDVField;

import java.util.List;

public class DataFieldVO extends DataAbstractFieldVO {
	private static final long serialVersionUID = 3865016457590792360L;

	/**
	 * XDSField.isKeyField
	 */
	private Boolean isKeyField;

	/**
	 * XDSField.isTitleField
	 */
	private Boolean isTitleField;

	/**
	 * XDSField.isSelfRelPointerField
	 */
	private Boolean isSelfRelPointerField;

	private String dbType;

	/**
	 * XDVField.format
	 */
	private String format;

	/**
	 * XDVField.columnWidth
	 */
	private Integer columnWidth;

	/**
	 * XDVField.resultType
	 */
	private XDSFieldResultType resultType;

	/**
	 * XDVField.footer
	 */
	private List<XDVAggregatorFunction> footer;

	// --------------------- ACCESSORS

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

	public String getDbType() {
		return dbType;
	}

	public void setDbType(String dbType) {
		this.dbType = dbType;
	}

	public String getFormat() {
		return format;
	}

	public void setFormat(String format) {
		this.format = format;
	}

	public Integer getColumnWidth() {
		return columnWidth;
	}

	public void setColumnWidth(Integer columnWidth) {
		this.columnWidth = columnWidth;
	}

	public XDSFieldResultType getResultType() {
		return resultType;
	}

	public void setResultType(XDSFieldResultType resultType) {
		this.resultType = resultType;
	}

	public List<XDVAggregatorFunction> getFooter() {
		return footer;
	}

	public void setFooter(List<XDVAggregatorFunction> footer) {
		this.footer = footer;
	}

	// --------------------- PUBLIC METHODS

	public boolean getIsKeyFieldSafely() {
		return ObjectUtil.isTrue(getIsKeyField());
	}

	public boolean getIsSelfRelPointerFieldSafely() {
		return ObjectUtil.isTrue(getIsSelfRelPointerField());
	}

	public Boolean getIsTitleFieldSafely() {
		return ObjectUtil.isTrue(isTitleField);
	}

	public String getFormatSafely() {
		return getFormat() != null ? getFormat() : getType().getFormat();
	}


	public XDSField toXDSField() {
		XDSField xdsField = new XDSField();
		ObjectUtil.merge(xdsField, this, true);
		return xdsField;
	}

	public XDVField toXDVField() {
		XDVField xdvField = new XDVField();
		ObjectUtil.merge(xdvField, this, true);

		if (!getIsKeyFieldSafely() && getFormat() != null && !getFormat().equals(getType().getFormat()) && getResultType() == XDSFieldResultType.Shown) {
			xdvField.setFormat(getFormat());
		} else {
			xdvField.setFormat(null);
		}

		return xdvField;
	}

	public DataFieldVO fromXDSField(XDSField xdsField) {
		ObjectUtil.merge(this, xdsField, true);
		return this;
	}

	public DataFieldVO fromXDVField(XDVField xdvField) {
		ObjectUtil.merge(this, xdvField, true);
		return this;
	}
}
