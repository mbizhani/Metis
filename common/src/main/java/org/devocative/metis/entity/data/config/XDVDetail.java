package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;

@XStreamAlias("detail")
public class XDVDetail implements Serializable {
	private static final long serialVersionUID = 5447124632143685942L;

	@XStreamAsAttribute
	private Integer level;

	@XStreamAsAttribute
	private Long dataViewId;

	@XStreamAsAttribute
	private String dataViewName;

	public Integer getLevel() {
		return level;
	}

	public void setLevel(Integer level) {
		this.level = level;
	}

	public Long getDataViewId() {
		return dataViewId;
	}

	public void setDataViewId(Long dataViewId) {
		this.dataViewId = dataViewId;
	}

	public String getDataViewName() {
		return dataViewName;
	}

	public void setDataViewName(String dataViewName) {
		this.dataViewName = dataViewName;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof XDVDetail)) return false;

		XDVDetail xdvDetail = (XDVDetail) o;

		if (getLevel() != null ? !getLevel().equals(xdvDetail.getLevel()) : xdvDetail.getLevel() != null) return false;
		return !(getDataViewName() != null ? !getDataViewName().equals(xdvDetail.getDataViewName()) : xdvDetail.getDataViewName() != null);

	}

	@Override
	public int hashCode() {
		int result = getLevel() != null ? getLevel().hashCode() : 0;
		result = 31 * result + (getDataViewName() != null ? getDataViewName().hashCode() : 0);
		return result;
	}
}
