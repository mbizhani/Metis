package org.devocative.metis;

import org.devocative.demeter.imodule.DErrorCode;

public enum MetisErrorCode implements DErrorCode {
	SQLExecution("SQL Execution"),
	DBTypeNotSupported("Database type not supported"),
	SchemaInEql("No schema should be prepend for any table in Eql"),
	DuplicateAlias("Duplicate alias"),
	EntityWithoutMapping("Entity without mapping"),
	EqlInvalidJoin("Eql invalid join"),
	UnknownAlias("Unknown alias"),
	EqlUnknownProperty("Unknown property"),
	EqlInvalidAssociationUsage("Invalid association usage"),
	DBConnection("Can't get connection from database");

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
