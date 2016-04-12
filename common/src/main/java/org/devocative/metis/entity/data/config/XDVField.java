package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;
import java.util.List;

@XStreamAlias("field")
public class XDVField implements Serializable {
	@XStreamAsAttribute
	protected String name;

	@XStreamAsAttribute
	private String format;

	@XStreamAsAttribute
	private Boolean inFilterPanel;

	@XStreamAsAttribute
	private XDSFieldResultType resultType;

	@XStreamAsAttribute
	private Long targetDVId;

	@XStreamAsAttribute
	private String targetDVName;

	private List<XDVAggregatorFunction> footer;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
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

	public Long getTargetDVId() {
		return targetDVId;
	}

	public void setTargetDVId(Long targetDVId) {
		this.targetDVId = targetDVId;
	}

	public String getTargetDVName() {
		return targetDVName;
	}

	public void setTargetDVName(String targetDVName) {
		this.targetDVName = targetDVName;
	}

	public List<XDVAggregatorFunction> getFooter() {
		return footer;
	}

	public void setFooter(List<XDVAggregatorFunction> footer) {
		this.footer = footer;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof XDVField)) return false;

		XDVField xdvField = (XDVField) o;

		return !(getName() != null ? !getName().equals(xdvField.getName()) : xdvField.getName() != null);
	}

	@Override
	public int hashCode() {
		return getName() != null ? getName().hashCode() : 0;
	}
}
