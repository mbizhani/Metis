package org.devocative.metis.vo;

import java.util.Map;

public class ODataQVO {
	private String name;

	private Map<String, Object> customFilter;
	private String filterExpression;
	private Map<String, Object> filterExpressionParams;

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

	public Map<String, Object> getCustomFilter() {
		return customFilter;
	}

	public ODataQVO setCustomFilter(Map<String, Object> customFilter) {
		this.customFilter = customFilter;
		return this;
	}

	public String getFilterExpression() {
		return filterExpression;
	}

	public ODataQVO setFilterExpression(String filterExpression) {
		this.filterExpression = filterExpression;
		return this;
	}

	public Map<String, Object> getFilterExpressionParams() {
		return filterExpressionParams;
	}

	public ODataQVO setFilterExpressionParams(Map<String, Object> filterExpressionParams) {
		this.filterExpressionParams = filterExpressionParams;
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
