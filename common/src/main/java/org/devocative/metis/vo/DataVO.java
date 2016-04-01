package org.devocative.metis.vo;

import org.devocative.metis.entity.data.config.XDSQuery;
import org.devocative.metis.entity.data.config.XDVDetail;

import java.io.Serializable;
import java.util.List;

public class DataVO implements Serializable {
	private String name;

	private String title;

	private String dataSource;

	private Long connectionInfoId;

	private XDSQuery query;

	private List<DataFieldVO> fields;

	private List<DataParameterVO> params;

	private List<XDVDetail> details;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getDataSource() {
		return dataSource;
	}

	public void setDataSource(String dataSource) {
		this.dataSource = dataSource;
	}

	public Long getConnectionInfoId() {
		return connectionInfoId;
	}

	public void setConnectionInfoId(Long connectionInfoId) {
		this.connectionInfoId = connectionInfoId;
	}

	public XDSQuery getQuery() {
		return query;
	}

	public void setQuery(XDSQuery query) {
		this.query = query;
	}

	public List<DataFieldVO> getFields() {
		return fields;
	}

	public void setFields(List<DataFieldVO> fields) {
		this.fields = fields;
	}

	public List<DataParameterVO> getParams() {
		return params;
	}

	public void setParams(List<DataParameterVO> params) {
		this.params = params;
	}

	public List<XDVDetail> getDetails() {
		return details;
	}

	public void setDetails(List<XDVDetail> details) {
		this.details = details;
	}
}
