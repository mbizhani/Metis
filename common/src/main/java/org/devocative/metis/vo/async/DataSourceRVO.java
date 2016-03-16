package org.devocative.metis.vo.async;

import java.io.Serializable;
import java.util.List;

public class DataSourceRVO implements Serializable {
	private List list;
	private Long count;

	public DataSourceRVO(List list, Long count) {
		this.list = list;
		this.count = count;
	}

	public List getList() {
		return list;
	}

	public Long getCount() {
		return count;
	}
}
