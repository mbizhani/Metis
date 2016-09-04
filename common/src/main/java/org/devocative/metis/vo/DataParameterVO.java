package org.devocative.metis.vo;

import org.devocative.adroit.ObjectUtil;
import org.devocative.metis.entity.data.config.XDSParameter;
import org.devocative.metis.entity.data.config.XDVParameter;

public class DataParameterVO extends DataAbstractFieldVO {
	private static final long serialVersionUID = 2904570466535233997L;

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

	@Override
	public boolean getInFilterPanelSafely() {
		return getInFilterPanel() == null || getInFilterPanel(); //TODO
//		return ObjectUtil.isTrue(getInFilterPanel());
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

	public XDVParameter toXDVParameter() {
		XDVParameter xdvParameter = new XDVParameter();
		ObjectUtil.merge(xdvParameter, this, true);
		return xdvParameter;
	}

	public DataParameterVO fromXDVParameter(XDVParameter xdvParameter) {
		ObjectUtil.merge(this, xdvParameter, true);
		return this;
	}
}
