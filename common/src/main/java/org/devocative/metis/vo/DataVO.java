package org.devocative.metis.vo;

import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.EConnectionSelection;
import org.devocative.metis.entity.data.config.*;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DataVO implements Serializable {
	private static final long serialVersionUID = -1306564276626018062L;

	/**
	 * DataView.id
	 */
	private Long dataViewId;

	/**
	 * DataSource.id
	 */
	private Long dataSourceId;

	/**
	 * DataSource.connectionSelection
	 */
	private EConnectionSelection connectionSelection;

	/**
	 * DataSource.connectionId
	 */
	private Long connectionId;

	/**
	 * DataSource.connection.configId != null
	 */
	private Boolean connectionHasMapping = false;

	// ---------------

	/**
	 * XDataView.name = DataView.name
	 */
	private String name;

	/**
	 * DataView.title
	 */
	private String title;

	/**
	 * DataView.groups
	 */
	private List<DataGroup> groups;

	/**
	 * XDataView.dataSourceName
	 */
	private String dataSourceName;

	/**
	 * XDataSource.caseSensitiveFilter
	 */
	private Boolean caseSensitiveFilter;

	/**
	 * XDataSource.query
	 */
	private XDSQuery query;

	/**
	 * XDataView.selectionMode
	 */
	private XDVGridSelectionMode selectionMode;

	/**
	 * XDataView.gridHeight
	 */
	private XDVGridHeight gridHeight;

	/**
	 * XDataView.selectionValidationJS
	 */
	private String selectionValidationJS;

	/**
	 * XDataView.rowStyler
	 */
	private String rowStyler;

	/**
	 * merge of XDataView.fields & XDataSource.fields
	 */
	private List<DataFieldVO> fields;

	/**
	 * XDataSource.params
	 */
	private List<DataParameterVO> params;

	/**
	 * XDataView.linksToDV
	 */
	private List<XDVLink> linksToDV;

	/**
	 * XDataView.details
	 */
	//TODO private List<XDVDetail> details;

	// ---------------

	private boolean dataSourceEditable = true;

	// ------------------------------ ACCESSORS

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

	public EConnectionSelection getConnectionSelection() {
		return connectionSelection;
	}

	public void setConnectionSelection(EConnectionSelection connectionSelection) {
		this.connectionSelection = connectionSelection;
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

	// ---------------

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

	public List<DataGroup> getGroups() {
		return groups;
	}

	public void setGroups(List<DataGroup> groups) {
		this.groups = groups;
	}

	public String getDataSourceName() {
		return dataSourceName;
	}

	public void setDataSourceName(String dataSourceName) {
		this.dataSourceName = dataSourceName;
	}

	public Boolean getCaseSensitiveFilter() {
		return caseSensitiveFilter;
	}

	public void setCaseSensitiveFilter(Boolean caseSensitiveFilter) {
		this.caseSensitiveFilter = caseSensitiveFilter;
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

	public XDVGridSelectionMode getSelectionMode() {
		return selectionMode;
	}

	public void setSelectionMode(XDVGridSelectionMode selectionMode) {
		this.selectionMode = selectionMode;
	}

	public XDVGridHeight getGridHeight() {
		return gridHeight;
	}

	public void setGridHeight(XDVGridHeight gridHeight) {
		this.gridHeight = gridHeight;
	}

	public String getSelectionValidationJS() {
		return selectionValidationJS;
	}

	public void setSelectionValidationJS(String selectionValidationJS) {
		this.selectionValidationJS = selectionValidationJS;
	}

	public String getRowStyler() {
		return rowStyler;
	}

	public void setRowStyler(String rowStyler) {
		this.rowStyler = rowStyler;
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

	public List<XDVLink> getLinksToDV() {
		return linksToDV;
	}

	public DataVO setLinksToDV(List<XDVLink> linksToDV) {
		this.linksToDV = linksToDV;
		return this;
	}

	public boolean isDataSourceEditable() {
		return dataSourceEditable;
	}

	public void setDataSourceEditable(boolean dataSourceEditable) {
		this.dataSourceEditable = dataSourceEditable;
	}

	// ------------------------------ BIZ METHODS

	public XDVGridSelectionMode getSelectionModeSafely() {
		return selectionMode != null ? selectionMode : XDVGridSelectionMode.Multiple;
	}

	public XDVGridHeight getGridHeightSafely() {
		return gridHeight != null ? gridHeight : XDVGridHeight.Short;
	}

	public List<DataAbstractFieldVO> getAllFields() {
		List<DataAbstractFieldVO> fieldVOs = new ArrayList<>();
		fieldVOs.addAll(getParams());
		fieldVOs.addAll(getFields());
		return fieldVOs;
	}

	public DataFieldVO findSelfRelPointerField() {
		for (DataFieldVO fieldVO : getFields()) {
			if (fieldVO.getIsSelfRelPointerFieldSafely()) {
				return fieldVO;
			}
		}
		return null;
	}

	public DataFieldVO findTitleField() {
		for (DataFieldVO fieldVO : getFields()) {
			if (fieldVO.getIsTitleFieldSafely()) {
				return fieldVO;
			}
		}
		return null;
	}

	public DataFieldVO findKeyField() {
		for (DataFieldVO fieldVO : getFields()) {
			if (fieldVO.getIsKeyFieldSafely()) {
				return fieldVO;
			}
		}
		return null;
	}


	public XDataSource toXDataSource() {
		XDataSource xDataSource = new XDataSource();
		xDataSource.setName(getName());
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
		xDataView.setDataSourceName(isDataSourceEditable() ? getName() : getDataSourceName());
		xDataView.setSelectionMode(getSelectionMode());
		xDataView.setGridHeight(getGridHeight());
		xDataView.setLinks(getLinksToDV());
		xDataView.setSelectionValidationJS(getSelectionValidationJS());
		xDataView.setRowStyler(getRowStyler());

		for (DataFieldVO fieldVO : getFields()) {
			xDataView.getFields().add(fieldVO.toXDVField());
		}

		for (DataParameterVO parameterVO : getParams()) {
			xDataView.getParams().add(parameterVO.toXDVParameter());
		}

		//TODO xDataView.setDetails(getDetails());

		return xDataView;
	}

	public void fromXDataSource(XDataSource xDataSource) {
		setDataSourceName(xDataSource.getName());
		setQuery(xDataSource.getQuery());

		for (XDSField xdsField : xDataSource.getFields()) {
			getFields().add(new DataFieldVO().fromXDSField(xdsField));
		}

		for (XDSParameter xdsParameter : xDataSource.getParams()) {
			getParams().add(new DataParameterVO().fromXDSParameter(xdsParameter));
		}
	}

	public void fromXDataView(XDataView xDataView) {
		setName(xDataView.getName());
		setSelectionMode(xDataView.getSelectionMode());
		setGridHeight(xDataView.getGridHeight());
		setLinksToDV(xDataView.getLinks());
		setSelectionValidationJS(xDataView.getSelectionValidationJS());
		setRowStyler(xDataView.getRowStyler());

		Map<String, DataFieldVO> fieldsMap = new HashMap<>();
		for (DataFieldVO fieldVO : getFields()) {
			fieldsMap.put(fieldVO.getName(), fieldVO);
		}
		for (XDVField xdvField : xDataView.getFields()) {
			if (fieldsMap.containsKey(xdvField.getName())) {
				fieldsMap.get(xdvField.getName()).fromXDVField(xdvField);
			}
		}

		Map<String, DataParameterVO> paramsMap = new HashMap<>();
		for (DataParameterVO parameterVO : getParams()) {
			paramsMap.put(parameterVO.getName(), parameterVO);
		}
		for (XDVParameter xdvParameter : xDataView.getParams()) {
			if (paramsMap.containsKey(xdvParameter.getName())) {
				paramsMap.get(xdvParameter.getName()).fromXDVParameter(xdvParameter);
			}
		}
	}
}
