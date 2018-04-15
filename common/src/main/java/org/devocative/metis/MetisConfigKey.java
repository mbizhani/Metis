package org.devocative.metis;

import org.devocative.adroit.IConfigKey;

import java.util.Arrays;
import java.util.List;

public enum MetisConfigKey implements IConfigKey {
	DBConnParamName("mts.dv.conn.param.name"),
	UseEqualOnUpperBound("mts.range.equal.upper", false, Arrays.asList(true, false)),
	ShowSearchDebugger("mts.search.debugger.visible", true, Arrays.asList(true, false)),
	IgnoreParameterValues("mts.ignore.param.values"),

	ExportReportExpireDays("mts.export.days", 3),
	ExportExcelMaxSize("mts.excel.export.max.size", 10000L),

	ConnectionEncryptPassword("mts.conn.enc.pass", false, Arrays.asList(true, false)),
	ConnectionCheckUserPassOnSave("mts.conn.check.onsave", true, Arrays.asList(true, false)),

	GridAsyncLoadingShow("mts.grid.async.loading.show", true, Arrays.asList(true, false)),
	GridNoResultShow("mts.grid.no.result.show", false, Arrays.asList(true, false)),
	GridReturnResultVersion("mts.grid.return.result.ver", "1", Arrays.asList("1", "2")),
	GridAssertDuplicateId("mts.grid.assert.duplicate.id", false, Arrays.asList(true, false)),
	GridAssertInvalidParent("mts.grid.assert.invalid.parent", true, Arrays.asList(true, false)),

	FormDateDefaultHour("mts.form.date.default.hour", 12),
	FormDateDefaultMinute("mts.form.date.default.minute", 0),
	FormDateDefaultSecond("mts.form.date.default.second", 0),

	ODataReplaceCharForNonSystemParam("mts.odata.char.replace.non.system", "~"),
	ODataConsiderParentRelation("mts.odata.consider.parent.relation", false, Arrays.asList(true, false));

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
