package org.devocative.metis.entity.dataSource.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;
import com.thoughtworks.xstream.annotations.XStreamOmitField;
import org.devocative.metis.entity.dataSource.DataSource;

import java.io.Serializable;

@XStreamAlias("field")
public class XDSField implements Serializable {
	@XStreamAsAttribute
	private String name;

	@XStreamAsAttribute
	private String title;

	@XStreamAsAttribute
	private XDSFieldType type;

	@XStreamAsAttribute
	private String format;

	@XStreamAsAttribute
	private Boolean inFilterPanel;

	@XStreamAsAttribute
	private XDSFieldFilterType filterType;

	@XStreamAsAttribute
	private XDSFieldResultType resultType;

	@XStreamAsAttribute
	private Boolean isKeyField;

	@XStreamAsAttribute
	private Boolean isTitleField;

	@XStreamAsAttribute
	private Boolean isSelfRelPointerField;

	@XStreamAsAttribute
	private Long targetId;

	// -- Volatile Fields

	@XStreamOmitField
	private String dbType;

	@XStreamOmitField
	private Integer dbSize;

	@XStreamOmitField
	private DataSource target;

	public String getName() {
		return name != null ? name.toLowerCase() : null;
	}

	public XDSField setName(String name) {
		this.name = name;
		return this;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public XDSFieldType getType() {
		return type;
	}

	public void setType(XDSFieldType type) {
		this.type = type;
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

	// Has Default
	public XDSFieldFilterType getFilterType() {
		return filterType != null ? filterType : XDSFieldFilterType.Equal;
	}

	public XDSField setFilterType(XDSFieldFilterType filterType) {
		this.filterType = filterType;
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

	public Long getTargetId() {
		return targetId;
	}

	public void setTargetId(Long targetId) {
		this.targetId = targetId;
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

	public DataSource getTarget() {
		return target;
	}

	public void setTarget(DataSource target) {
		this.target = target;
	}

	// ---------------------- Object METHODS

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof XDSField)) return false;

		XDSField field = (XDSField) o;

		return !(getName() != null ? !getName().equals(field.getName()) : field.getName() != null);

	}

	@Override
	public int hashCode() {
		return getName() != null ? getName().hashCode() : 0;
	}

	@Override
	public String toString() {
		return getName();
	}
}
