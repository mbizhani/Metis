package org.devocative.metis.entity.dataSource;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@XStreamAlias("dataSource")
public class DataSource implements Serializable {
	@XStreamAsAttribute
	private String name;

	@XStreamAsAttribute
	private String dbConnectionRef;

	private String sql;

	private List<DSField> fields;

	private transient Map<String, DSField> fieldMap;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDbConnectionRef() {
		return dbConnectionRef;
	}

	public void setDbConnectionRef(String dbConnectionRef) {
		this.dbConnectionRef = dbConnectionRef;
	}

	public String getSql() {
		return sql;
	}

	public void setSql(String sql) {
		this.sql = sql;
	}

	public List<DSField> getFields() {
		return fields;
	}

	public void setFields(List<DSField> fields) {
		this.fields = fields;
	}

	public DSField getField(String name) {
		if (fieldMap == null) {
			fieldMap = new HashMap<>();
			for (DSField field : fields) {
				fieldMap.put(field.getName(), field);
			}
		}
		return fieldMap.get(name);
	}

	@Override
	public String toString() {
		return String.format("DataSource: name=[%s], dbConnectionRef=[%s], #fields=[%s]",
			getName(), getDbConnectionRef(), getFields().size());
	}
}
