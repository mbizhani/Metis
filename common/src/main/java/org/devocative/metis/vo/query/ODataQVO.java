package org.devocative.metis.vo.query;

import java.util.Map;

public class ODataQVO {
	private String name;

	private String filterExpression;
	private Map<String, Object> inputParams;

	private Map<String, String> orderBy;

	private long pageIndex;
	private long pageSize;

	// ------------------------------

	public ODataQVO(String name) {
		this.name = name;
	}

	// ------------------------------

	public String getName() {
		return name;
	}

	public String getFilterExpression() {
		return filterExpression;
	}

	public ODataQVO setFilterExpression(String filterExpression) {
		this.filterExpression = filterExpression;
		return this;
	}

	public Map<String, Object> getInputParams() {
		return inputParams;
	}

	public ODataQVO setInputParams(Map<String, Object> inputParams) {
		this.inputParams = inputParams;
		return this;
	}

	public Map<String, String> getOrderBy() {
		return orderBy;
	}

	public ODataQVO setOrderBy(Map<String, String> orderBy) {
		this.orderBy = orderBy;
		return this;
	}

	public long getPageIndex() {
		return pageIndex;
	}

	public ODataQVO setPageIndex(long pageIndex) {
		this.pageIndex = pageIndex;
		return this;
	}

	public long getPageSize() {
		return pageSize;
	}

	public ODataQVO setPageSize(long pageSize) {
		this.pageSize = pageSize;
		return this;
	}
}
