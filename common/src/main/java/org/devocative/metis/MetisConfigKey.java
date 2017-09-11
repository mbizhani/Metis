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

	GridAsyncLoadingEnabled("mts.loading.async.enabled", true, Arrays.asList(true, false))
	;

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
