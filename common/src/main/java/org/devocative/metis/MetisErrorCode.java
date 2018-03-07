package org.devocative.metis;

import org.devocative.demeter.imodule.DErrorCode;

public enum MetisErrorCode implements DErrorCode {
	SQLExecution("SQL Execution"),
	ParameterFieldNameClash("Same name for both field(s) and parameter(s)"),
	SchemaInEql("No schema name should be prepend for any table in Eql"),
	NoMappingForConnection("No mapping for connection to execute Eql"),
	DuplicateAlias("Duplicate alias"),
	EntityWithoutMapping("Entity without mapping"),
	EqlInvalidJoin("Eql invalid join"),
	UnknownAlias("Unknown alias"),
	EqlUnknownProperty("Unknown property"),
	EqlJoinOnProperty("Join on property"),
	EqlInvalidAssociationUsage("Invalid association usage"),

	DBConnection("Can't get connection from database"),
	InvalidDBConnection("Invalid DB Connection"),
	DuplicateDBConnectionAlias("Duplicate DBConnectionAlias"),
	DuplicateDBConnectionName("Duplicate DBConnection Name"),

	DuplicateDataSourceName("Duplicate DataSource name"),

	InvalidDataViewName("Invalid data view name"),
	NoDataViewName("No data view name"),
	InvalidDataViewState("Invalid DataView State"),

	InvalidFilterValue("Invalid filter value");

	private String defaultDescription;

	MetisErrorCode(String defaultDescription) {
		this.defaultDescription = defaultDescription;
	}

	@Override
	public String getCode() {
		return name();
	}

	@Override
	public String getDefaultDescription() {
		return defaultDescription;
	}
}
