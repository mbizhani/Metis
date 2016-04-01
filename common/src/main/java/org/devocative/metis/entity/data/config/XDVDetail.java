package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;

@XStreamAlias("detail")
public class XDVDetail implements Serializable {
	@XStreamAsAttribute
	private Integer level;

	@XStreamAsAttribute
	private String dataView;

	public Integer getLevel() {
		return level;
	}

	public void setLevel(Integer level) {
		this.level = level;
	}

	public String getDataView() {
		return dataView;
	}

	public void setDataView(String dataView) {
		this.dataView = dataView;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof XDVDetail)) return false;

		XDVDetail xdvDetail = (XDVDetail) o;

		if (getLevel() != null ? !getLevel().equals(xdvDetail.getLevel()) : xdvDetail.getLevel() != null) return false;
		return !(getDataView() != null ? !getDataView().equals(xdvDetail.getDataView()) : xdvDetail.getDataView() != null);

	}

	@Override
	public int hashCode() {
		int result = getLevel() != null ? getLevel().hashCode() : 0;
		result = 31 * result + (getDataView() != null ? getDataView().hashCode() : 0);
		return result;
	}
}
