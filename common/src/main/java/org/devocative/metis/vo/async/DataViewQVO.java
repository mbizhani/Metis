package org.devocative.metis.vo.async;

import java.io.Serializable;
import java.util.Map;

public class DataViewQVO implements Serializable {
	private String name;
	private long pageIndex;
	private long pageSize;
	private Map<String, String> sortFieldList;
	private Map<String, Object> filter;

	private Serializable parentId;

	public String getName() {
		return name;
	}

	public DataViewQVO setName(String name) {
		this.name = name;
		return this;
	}

	public long getPageIndex() {
		return pageIndex;
	}

	public DataViewQVO setPageIndex(long pageIndex) {
		this.pageIndex = pageIndex;
		return this;
	}

	public long getPageSize() {
		return pageSize;
	}

	public DataViewQVO setPageSize(long pageSize) {
		this.pageSize = pageSize;
		return this;
	}

	public Map<String, String> getSortFieldList() {
		return sortFieldList;
	}

	public DataViewQVO setSortFieldList(Map<String, String> sortFieldList) {
		this.sortFieldList = sortFieldList;
		return this;
	}

	public Map<String, Object> getFilter() {
		return filter;
	}

	public DataViewQVO setFilter(Map<String, Object> filter) {
		this.filter = filter;
		return this;
	}

	public Serializable getParentId() {
		return parentId;
	}

	public DataViewQVO setParentId(Serializable parentId) {
		this.parentId = parentId;
		return this;
	}
}
