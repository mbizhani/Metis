package org.devocative.metis.vo;

import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.data.config.XDSQuery;
import org.devocative.metis.entity.data.config.XDVDetail;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class DataVO implements Serializable {
	private Long dataViewId;

	private Long dataSourceId;

	private Long dbConnectionId;

	private Boolean dbConnectionHasMapping = false;

	// ----

	private String name;

	private String title;

	private String dataSourceName;

	private XDSQuery query;

	private List<DataFieldVO> fields;

	private List<DataParameterVO> params;

	private List<XDVDetail> details;

	// --------------------- ACCESSORS

	public Long getDataViewId() {
		return dataViewId;
	}

	public void setDataViewId(Long dataViewId) {
		this.dataViewId = dataViewId;
	}

	public Long getDataSourceId() {
		return dataSourceId;
	}

	public void setDataSourceId(Long dataSourceId) {
		this.dataSourceId = dataSourceId;
	}

	public Long getDbConnectionId() {
		return dbConnectionId;
	}

	public void setDbConnectionId(Long dbConnectionId) {
		this.dbConnectionId = dbConnectionId;
	}

	public Boolean getDbConnectionHasMapping() {
		return dbConnectionHasMapping;
	}

	public void setDbConnectionHasMapping(Boolean dbConnectionHasMapping) {
		this.dbConnectionHasMapping = dbConnectionHasMapping;
	}

	public DBConnection getConnection() {
		return getDbConnectionId() != null ? new DBConnection(getDbConnectionId()) : null;
	}

	public void setConnection(DBConnection connection) {
		setDbConnectionId(connection.getId());
	}

	// ----

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

	public String getDataSourceName() {
		return dataSourceName;
	}

	public void setDataSourceName(String dataSourceName) {
		this.dataSourceName = dataSourceName;
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
		if (params == null) {
			params = new ArrayList<>();
		}
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
