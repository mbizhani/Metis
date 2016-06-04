package org.devocative.metis.vo.query;

import java.util.Map;

public abstract class AbstractQueryQVO {
	private String dataSourceName;
	private Map<String, Object> inputParams;
	private String filterExpression;

	// ------------------------------

	protected AbstractQueryQVO(String dataSourceName) {
		this.dataSourceName = dataSourceName;
	}

	// ------------------------------

	public String getDataSourceName() {
		return dataSourceName;
	}

	public Map<String, Object> getInputParams() {
		return inputParams;
	}

	public AbstractQueryQVO setInputParams(Map<String, Object> inputParams) {
		this.inputParams = inputParams;
		return this;
	}

	public String getFilterExpression() {
		return filterExpression;
	}

	public AbstractQueryQVO setFilterExpression(String filterExpression) {
		this.filterExpression = filterExpression;
		return this;
	}

}
