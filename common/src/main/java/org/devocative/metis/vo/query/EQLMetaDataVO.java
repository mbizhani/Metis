package org.devocative.metis.vo.query;

import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.entity.connection.mapping.XEntity;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class EQLMetaDataVO implements Serializable {
	private String eql;
	private String sql;
	private Map<String, XEntity> aliasToXEntityMap;
	private Map<MetisErrorCode, List<String>> errors = new HashMap<>();

	// ------------------------------

	public String getEql() {
		return eql;
	}

	public void setEql(String eql) {
		this.eql = eql;
	}

	public String getSql() {
		return sql;
	}

	public void setSql(String sql) {
		this.sql = sql;
	}

	public Map<String, XEntity> getAliasToXEntityMap() {
		return aliasToXEntityMap;
	}

	public void setAliasToXEntityMap(Map<String, XEntity> aliasToXEntityMap) {
		this.aliasToXEntityMap = aliasToXEntityMap;
	}

	public Map<MetisErrorCode, List<String>> getErrors() {
		return errors;
	}

	// ------------------------------

	public void addError(MetisErrorCode errorCode, String error) {
		if (!errors.containsKey(errorCode)) {
			errors.put(errorCode, new ArrayList<String>());
		}

		errors.get(errorCode).add(error);
	}
}
