package org.devocative.metis.vo.query;

import java.util.Map;

public abstract class AbstractQueryQVO {
	private String dataSourceId;
	private Map<String, Object> inputParams;
	private String filterExpression;

	private String sentDBConnection;

	private Map<String, Object> extraParams;

	// ------------------------------

	protected AbstractQueryQVO(String dataSourceId) {
		this.dataSourceId = dataSourceId;
	}

	// ------------------------------

	public String getDataSourceId() {
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

	public Map<String, Object> getExtraParams() {
		return extraParams;
	}

	public AbstractQueryQVO setExtraParams(Map<String, Object> extraParams) {
		this.extraParams = extraParams;
		return this;
	}
}
