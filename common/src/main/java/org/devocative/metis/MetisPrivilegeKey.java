package org.devocative.metis;

import org.devocative.demeter.entity.IPrivilegeKey;

public enum MetisPrivilegeKey implements IPrivilegeKey {
	DBConnectionGroupAdd, DBConnectionGroupEdit,
	DBConnectionAdd, DBConnectionEdit, DBConnectionCloseAll,
	DataGroupAdd, DataGroupEdit,
	ReportAdd, ReportEdit,
	DataViewExport,
	DataViewImport,
	DBConnectionAliasAdd, DBConnectionAliasEdit,
	ReportExport,
	ReportImport;

	private String name;

	@Override
	public String getName() {
		return name;
	}

	@Override
	public void setModule(String module) {
		name = String.format("%s.%s", module, name());
	}
}
