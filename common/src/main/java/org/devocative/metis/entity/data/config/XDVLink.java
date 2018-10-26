package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;
import org.devocative.metis.entity.data.DataView;

import java.io.Serializable;

@XStreamAlias("link")
public class XDVLink implements Serializable {
	private static final long serialVersionUID = -5590133159321453960L;

	@XStreamAsAttribute
	private String title;

	@XStreamAsAttribute
	private String targetDVId;

	@XStreamAsAttribute
	private String targetDVName;

	@XStreamAsAttribute
	private XDVLinkType linkType;

	private String sentData;

	// ------------------------------

	public String getTitle() {
		return title;
	}

	public XDVLink setTitle(String title) {
		this.title = title;
		return this;
	}

	public String getTargetDVId() {
		return targetDVId;
	}

	public XDVLink setTargetDVId(String targetDVId) {
		this.targetDVId = targetDVId;
		return this;
	}

	public String getTargetDVName() {
		return targetDVName;
	}

	public XDVLink setTargetDVName(String targetDVName) {
		this.targetDVName = targetDVName;
		return this;
	}

	public XDVLinkType getLinkType() {
		return linkType == null ? XDVLinkType.SingleRow : linkType;
	}

	public XDVLink setLinkType(XDVLinkType linkType) {
		this.linkType = linkType;
		return this;
	}

	public String getSentData() {
		return sentData;
	}

	public XDVLink setSentData(String sentData) {
		this.sentData = sentData;
		return this;
	}

	// ------------------------------

	public DataView getTargetDV() {
		DataView result = new DataView();
		result.setId(targetDVId);
		return result;
	}

	public void setTargetDV(DataView dataView) {
		targetDVId = dataView.getId();
		targetDVName = dataView.getName();
	}
}
