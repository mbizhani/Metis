package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

@XStreamAlias("param")
public class XDSParameter extends XDSAbstractField {
	private static final long serialVersionUID = -8827549726729155924L;

	@XStreamAsAttribute
	private String sampleData;

	public String getSampleData() {
		return sampleData;
	}

	public XDSParameter setSampleData(String sampleData) {
		this.sampleData = sampleData;
		return this;
	}
}
