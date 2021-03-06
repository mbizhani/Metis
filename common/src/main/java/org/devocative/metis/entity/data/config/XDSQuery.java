package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;

@XStreamAlias("query")
public class XDSQuery implements Serializable {
	private static final long serialVersionUID = -3162775747402040477L;

	@XStreamAsAttribute
	private XDSQueryMode mode;

	@XStreamAsAttribute
	private Boolean dynamic;

	private String before;

	private String text;

	private String after;

	public XDSQueryMode getMode() {
		return mode;
	}

	public void setMode(XDSQueryMode mode) {
		this.mode = mode;
	}

	public Boolean getDynamic() {
		return dynamic;
	}

	public void setDynamic(Boolean dynamic) {
		this.dynamic = dynamic;
	}

	public String getBefore() {
		return before;
	}

	public void setBefore(String before) {
		this.before = before;
	}

	public String getText() {
		return text != null ? text.trim() : null;
	}

	public void setText(String text) {
		this.text = text;
	}

	public String getAfter() {
		return after;
	}

	public void setAfter(String after) {
		this.after = after;
	}
}
