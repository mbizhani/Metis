package org.devocative.metis.entity.dataSource.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamOmitField;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@XStreamAlias("dataSource")
public class XDataSource implements Serializable {
	private XDSQuery query;

	private List<XDSField> fields;

	private List<XDSParameter> params;

	private transient Map<String, XDSField> fieldMap;

	@XStreamOmitField
	private Long connectionInfoId;

	public XDSQuery getQuery() {
		return query;
	}

	public void setQuery(XDSQuery query) {
		this.query = query;
	}

	public List<XDSField> getFields() {
		return fields;
	}

	public void setFields(List<XDSField> fields) {
		this.fields = fields;
	}

	public List<XDSParameter> getParams() {
		return params;
	}

	public void setParams(List<XDSParameter> params) {
		this.params = params;
	}

	public Long getConnectionInfoId() {
		return connectionInfoId;
	}

	public void setConnectionInfoId(Long connectionInfoId) {
		this.connectionInfoId = connectionInfoId;
	}

	public XDSField getField(String name) {
		if (fieldMap == null) {
			fieldMap = new HashMap<>();
			for (XDSField field : fields) {
				fieldMap.put(field.getName(), field);
			}
		}
		return fieldMap.get(name);
	}

}
