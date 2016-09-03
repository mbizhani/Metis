package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.util.List;

@XStreamAlias("field")
public class XDVField extends XDVAbstractField {
	private static final long serialVersionUID = 2115358019135124711L;

	@XStreamAsAttribute
	private String format;

	@XStreamAsAttribute
	private Integer columnWidth;

	@XStreamAsAttribute
	private XDSFieldResultType resultType;

	private List<XDVAggregatorFunction> footer;

	// ------------------------------ ACCESSORS

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
}
