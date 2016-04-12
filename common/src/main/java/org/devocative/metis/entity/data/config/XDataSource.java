package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;
import com.thoughtworks.xstream.annotations.XStreamOmitField;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@XStreamAlias("dataSource")
public class XDataSource implements Serializable {
	@XStreamAsAttribute
	private String name;

	private XDSQuery query;

	private List<XDSField> fields;

	private List<XDSParameter> params;

	@XStreamOmitField
	private Map<String, XDSField> fieldMap;

	@Deprecated
	@XStreamOmitField
	private Long connectionInfoId;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	// Has Default
	public XDSQuery getQuery() {
		if (query == null) {
			query = new XDSQuery();
		}
		return query;
	}

	public void setQuery(XDSQuery query) {
		this.query = query;
	}

	// Has Default
	public List<XDSField> getFields() {
		if (fields == null) {
			fields = new ArrayList<>();
		}
		return fields;
	}

	public void setFields(List<XDSField> fields) {
		this.fields = fields;
	}

	// Has Default
	public List<XDSParameter> getParams() {
		if (params == null) {
			params = new ArrayList<>();
		}
		return params;
	}

	public void setParams(List<XDSParameter> params) {
		this.params = params;
	}

	@Deprecated
	public Long getConnectionInfoId() {
		return connectionInfoId;
	}

	@Deprecated
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
