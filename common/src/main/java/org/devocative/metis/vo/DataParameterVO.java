package org.devocative.metis.vo;

import org.devocative.adroit.ObjectUtil;
import org.devocative.metis.entity.data.config.XDSParameter;

public class DataParameterVO extends DataAbstractFieldVO {
	/**
	 * XDSParameter.sampleData
	 */
	private String sampleData;

	public String getSampleData() {
		return sampleData;
	}

	public void setSampleData(String sampleData) {
		this.sampleData = sampleData;
	}

	public XDSParameter toXDSParameter() {
		XDSParameter xdsParameter = new XDSParameter();
		ObjectUtil.merge(xdsParameter, this, true);
		return xdsParameter;
	}

	public DataParameterVO fromXDSParameter(XDSParameter xdsParameter) {
		ObjectUtil.merge(this, xdsParameter, true);
		return this;
	}
}
