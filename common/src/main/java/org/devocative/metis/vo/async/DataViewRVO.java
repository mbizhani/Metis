package org.devocative.metis.vo.async;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

public class DataViewRVO implements Serializable {
	private List<Map<String, Object>> list;
	private Long count;
	private String parentId;

	public List<Map<String, Object>> getList() {
		return list;
	}

	public DataViewRVO setList(List<Map<String, Object>> list) {
		this.list = list;
		return this;
	}

	public Long getCount() {
		return count;
	}

	public DataViewRVO setCount(Long count) {
		this.count = count;
		return this;
	}

	public String getParentId() {
		return parentId;
	}

	public DataViewRVO setParentId(String parentId) {
		this.parentId = parentId;
		return this;
	}
}