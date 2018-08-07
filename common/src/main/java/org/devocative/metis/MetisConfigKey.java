package org.devocative.metis;

import org.devocative.adroit.IConfigKey;

import java.util.Arrays;
import java.util.List;

public enum MetisConfigKey implements IConfigKey {
	DatabaseCheckTimeoutEnabled("mts.db.check.timeout.enabled", true, Arrays.asList(true, false)),
	DatabaseCheckTimeoutDur("mts.db.check.timeout.dur", 15),
	DatabaseCheckTimeoutMin("mts.db.check.timeout.min", 5),
	DatabaseCheckTimeoutMax("mts.db.check.timeout.max", 15),
	DatabaseCheckTimeoutList("mts.db.check.timeout.list", 10),
	DatabaseCheckTimeoutAlive("mts.db.check.timeout.alive", 10), //minutes

	DBConnParamName("mts.dv.conn.param.name"),
	UseEqualOnUpperBound("mts.range.equal.upper", false, Arrays.asList(true, false)),
	ShowSearchDebugger("mts.debugger.visible", true, Arrays.asList(true, false)),
	IgnoreParameterValues("mts.ignore.param.values"),
	ShowParamsInIssueKey("mts.issue.params.visible", true, Arrays.asList(true, false)),

	ExportReportExpireDays("mts.export.days", 3),
	ExportExcelMaxSize("mts.export.excel.max.size", 10000L),
	ExportAllWriteMeta("mts.export.all.meta.data", true, Arrays.asList(true, false)),
	ImportAllVerify("mts.import.all.verify", false, Arrays.asList(true, false)),
	ExportReportWriteMeta("mts.export.report.meta.data", true, Arrays.asList(true, false)),
	ImportReportVerify("mts.import.report.verify", true, Arrays.asList(true, false)),

	ConnectionEncryptPassword("mts.conn.enc.pass", false, Arrays.asList(true, false)),
	ConnectionCheckUserPassOnSave("mts.conn.check.onsave", true, Arrays.asList(true, false)),

	GridAsyncLoadingShow("mts.grid.async.loading.show", true, Arrays.asList(true, false)),
	GridNoResultShow("mts.grid.no.result.show", false, Arrays.asList(true, false)),
	GridReturnResultVersion("mts.grid.return.result.ver", "1", Arrays.asList("1", "2")),
	GridAssertDuplicateId("mts.grid.assert.duplicate.id", false, Arrays.asList(true, false)),
	GridAssertInvalidParent("mts.grid.assert.invalid.parent", true, Arrays.asList(true, false)),
	GridCheckboxColumnMode("mts.grid.column.checkbox.mode", "MultiSelect", Arrays.asList("Never", "MultiSelect", "Always")),
	GridFormatInteger("mts.grid.format.integer", "#,###"),
	GridFormatReal("mts.grid.format.real", "#,##0.000"),
	GridFormatDate("mts.grid.format.date", "yyyy/MM/dd"),
	GridFormatDateTime("mts.grid.format.datetime", "yyyy/MM/dd HH:mm:ss"),

	FormDateDefaultTime("mts.form.date.default.time", "12:00:00.000"),
	FormDateTimeDefaultFrom("mts.form.datetime.default.from", "00:00:00.000"),
	FormDateTimeDefaultTo("mts.form.datetime.default.to", "23:59:59.999"),

	ODataReplaceCharForNonSystemParam("mts.odata.char.replace.non.system", "~"),
	ODataConsiderParentRelation("mts.odata.consider.parent.relation", false, Arrays.asList(true, false));

	// ------------------------------

	private String key;
	private boolean validate = false;
	private Object defaultValue;
	private List<?> possibilities;

	MetisConfigKey(String key) {
		this(false, key, null);
	}

	MetisConfigKey(String key, List<?> possibilities) {
		this(false, key, possibilities);
	}

	MetisConfigKey(boolean validate, String key) {
		this(validate, key, null);
	}

	// Main Constructor 1
	MetisConfigKey(boolean validate, String key, List<?> possibilities) {
		this.key = key;
		this.validate = validate;
		this.possibilities = possibilities;
	}

	MetisConfigKey(String key, Object defaultValue) {
		this(key, defaultValue, null);
	}

	// Main Constructor 2
	MetisConfigKey(String key, Object defaultValue, List<?> possibilities) {
		this.key = key;
		this.defaultValue = defaultValue;
		this.possibilities = possibilities;
	}

	@Override
	public String getKey() {
		return key;
	}

	@Override
	public boolean getValidate() {
		return validate;
	}

	@Override
	public Object getDefaultValue() {
		return defaultValue;
	}

	@Override
	public List<?> getPossibleValues() {
		return possibilities;
	}
}
