package org.devocative.metis.vo.query;

import java.util.Map;

public abstract class AbstractQueryQVO {
	private Long dataSourceId;
	private Map<String, Object> inputParams;
	private String filterExpression;

	private String sentDBConnection;

	// ------------------------------

	protected AbstractQueryQVO(Long dataSourceId) {
		this.dataSourceId = dataSourceId;
	}

	// ------------------------------

	public Long getDataSourceId() {
		return dataSourceId;
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

	public String getSentDBConnection() {
		return sentDBConnection;
	}

	public AbstractQueryQVO setSentDBConnection(String sentDBConnection) {
		this.sentDBConnection = sentDBConnection;
		return this;
	}
}
