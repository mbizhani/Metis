package org.devocative.metis.vo.query;

import java.util.Map;

public class ODataQVO {
	private String name;

	private String filterExpression;
	private Map<String, Object> inputParams;

	private Map<String, String> orderBy;

	private long firstResult;
	private long maxResults;

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

	public long getFirstResult() {
		return firstResult;
	}

	public ODataQVO setFirstResult(long firstResult) {
		this.firstResult = firstResult;
		return this;
	}

	public long getMaxResults() {
		return maxResults;
	}

	public ODataQVO setMaxResults(long maxResults) {
		this.maxResults = maxResults;
		return this;
	}
}
