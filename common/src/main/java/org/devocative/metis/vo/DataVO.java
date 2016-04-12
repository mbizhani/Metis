package org.devocative.metis.vo;

import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.data.config.XDSQuery;
import org.devocative.metis.entity.data.config.XDVDetail;
import org.devocative.metis.entity.data.config.XDataSource;
import org.devocative.metis.entity.data.config.XDataView;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class DataVO implements Serializable {
	/**
	 * DataView.id
	 */
	private Long dataViewId;

	/**
	 * DataSource.id
	 */
	private Long dataSourceId;

	/**
	 * DataSource.connectionId
	 */
	private Long connectionId;

	/**
	 * DataSource.connection.configId != null
	 */
	private Boolean connectionHasMapping = false;

	// ----

	/**
	 * XDataView.name = DataView.name
	 */
	private String name;

	/**
	 * DataView.title
	 */
	private String title;

	/**
	 * XDataView.dataSourceName
	 */
	private String dataSourceName;

	/**
	 * XDataSource.query
	 */
	private XDSQuery query;

	/**
	 * merge of XDataView.fields & XDataSource.fields
	 */
	private List<DataFieldVO> fields;

	/**
	 * XDataSource.params
	 */
	private List<DataParameterVO> params;

	/**
	 * XDataView.details
	 */
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

	public Long getConnectionId() {
		return connectionId;
	}

	public void setConnectionId(Long connectionId) {
		this.connectionId = connectionId;
	}

	public Boolean getConnectionHasMapping() {
		return connectionHasMapping;
	}

	public void setConnectionHasMapping(Boolean connectionHasMapping) {
		this.connectionHasMapping = connectionHasMapping;
	}

	public DBConnection getConnection() {
		return getConnectionId() != null ? new DBConnection(getConnectionId()) : null;
	}

	public void setConnection(DBConnection connection) {
		setConnectionId(connection.getId());
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
		if (query == null) {
			query = new XDSQuery();
		}
		return query;
	}

	public void setQuery(XDSQuery query) {
		this.query = query;
	}

	public List<DataFieldVO> getFields() {
		if (fields == null) {
			fields = new ArrayList<>();
		}
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

	// --------------------- BIZ METHODS

	public boolean isDataSourceEditable() {
		return getDataSourceId() == null || getName().equals(getDataSourceName());
	}

	public XDataSource toXDataSource() {
		XDataSource xDataSource = new XDataSource();
		xDataSource.setName(getDataSourceName());
		xDataSource.setQuery(getQuery());

		for (DataFieldVO fieldVO : getFields()) {
			xDataSource.getFields().add(fieldVO.toXDSField());
		}

		for (DataParameterVO paramVO : getParams()) {
			xDataSource.getParams().add(paramVO.toXDSParameter());
		}

		return xDataSource;
	}

	public XDataView toXDataView() {
		XDataView xDataView = new XDataView();
		xDataView.setName(getName());
		xDataView.setDataSourceId(getDataSourceId());
		xDataView.setDataSourceName(getDataSourceName());

		for (DataFieldVO fieldVO : getFields()) {
			xDataView.getFields().add(fieldVO.toXDVField());
		}

		xDataView.setDetails(getDetails());

		return xDataView;
	}
}
