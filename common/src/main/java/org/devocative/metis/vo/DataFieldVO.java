package org.devocative.metis.vo;

import org.devocative.metis.entity.data.config.XDSFieldResultType;
import org.devocative.metis.entity.data.config.XDVAggregatorFunction;

import java.util.List;

public class DataFieldVO extends DataAbstractFieldVO {
	private Boolean isKeyField;

	private Boolean isTitleField;

	private Boolean isSelfRelPointerField;

	private String dbType;

	private Integer dbSize;

	private String format;

	private Boolean inFilterPanel;

	private XDSFieldResultType resultType;

	private List<XDVAggregatorFunction> footer;

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

	public Integer getDbSize() {
		return dbSize;
	}

	public void setDbSize(Integer dbSize) {
		this.dbSize = dbSize;
	}

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
}
