package org.devocative.metis.entity.dataSource.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;

@XStreamAlias("query")
public class XDSQuery implements Serializable {
	@XStreamAsAttribute
	private XDSQueryMode mode;

	@XStreamAsAttribute
	private Boolean dynamic;

	private String text;

	public XDSQueryMode getMode() {
		return mode;
	}

	public void setMode(XDSQueryMode mode) {
		this.mode = mode;
	}

	// Has Default
	public Boolean getDynamic() {
		return dynamic != null ? dynamic : false;
	}

	public void setDynamic(Boolean dynamic) {
		this.dynamic = dynamic;
	}

	public String getText() {
		return text != null ? text.trim() : null;
	}

	public void setText(String text) {
		this.text = text;
	}
}
