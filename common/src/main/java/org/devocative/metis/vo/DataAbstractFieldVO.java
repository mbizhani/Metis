package org.devocative.metis.vo;

import org.devocative.adroit.ObjectUtil;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.XDSFieldFilterType;
import org.devocative.metis.entity.data.config.XDSFieldResultType;
import org.devocative.metis.entity.data.config.XDSFieldType;

import java.io.Serializable;

public abstract class DataAbstractFieldVO implements Serializable, Comparable<DataAbstractFieldVO> {
	private static final long serialVersionUID = 3087772339561606073L;

	/**
	 * XDSAbstractField.name
	 * XDVField.name
	 */
	private String name;

	/**
	 * XDSAbstractField.title
	 */
	private String title;

	/**
	 * XDSAbstractField.required
	 */
	private Boolean required;

	/**
	 * XDSAbstractField.type
	 */
	private XDSFieldType type;

	/**
	 * XDSAbstractField.filterType
	 */
	private XDSFieldFilterType filterType;

	/**
	 * XDVAbstractField.inFilterPanel
	 */
	private Boolean inFilterPanel;

	/**
	 * XDVAbstractField.filterPanelOrder
	 */
	private Integer filterPanelOrder;

	/**
	 * XDSAbstractField.targetDSId
	 */
	private Long targetDSId;

	/**
	 * XDSAbstractField.targetDSName
	 */
	private String targetDSName;

	/**
	 * XDSAbstractField.targetDSMultipleSelection
	 */
	private Boolean targetDSMultipleSelection;

	/**
	 * XDVField.targetDVId
	 */
	private Long targetDVId;

	/**
	 * XDVField.targetDVName
	 */
	private String targetDVName;

	/**
	 * XDVField.targetDSFilter
	 */
	private String targetDSFilter;

	// ------------------------------ ACCESSORS

	public abstract XDSFieldResultType getResultType();

	// ------------------------------ ACCESSORS

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

	public Boolean getRequired() {
		return required;
	}

	public void setRequired(Boolean required) {
		this.required = required;
	}

	public XDSFieldType getType() {
		return type;
	}

	public void setType(XDSFieldType type) {
		this.type = type;
	}

	public XDSFieldFilterType getFilterType() {
		return filterType;
	}

	public void setFilterType(XDSFieldFilterType filterType) {
		this.filterType = filterType;
	}

	public Boolean getInFilterPanel() {
		return inFilterPanel;
	}

	public void setInFilterPanel(Boolean inFilterPanel) {
		this.inFilterPanel = inFilterPanel;
	}

	public Integer getFilterPanelOrder() {
		return filterPanelOrder;
	}

	public void setFilterPanelOrder(Integer filterPanelOrder) {
		this.filterPanelOrder = filterPanelOrder;
	}

	public Long getTargetDSId() {
		return targetDSId;
	}

	public void setTargetDSId(Long targetDSId) {
		this.targetDSId = targetDSId;
	}

	public Long getTargetDVId() {
		return targetDVId;
	}

	public void setTargetDVId(Long targetDVId) {
		this.targetDVId = targetDVId;
	}

	public String getTargetDSName() {
		return targetDSName;
	}

	public void setTargetDSName(String targetDSName) {
		this.targetDSName = targetDSName;
	}

	public Boolean getTargetDSMultipleSelection() {
		return targetDSMultipleSelection;
	}

	public void setTargetDSMultipleSelection(Boolean targetDSMultipleSelection) {
		this.targetDSMultipleSelection = targetDSMultipleSelection;
	}

	public String getTargetDVName() {
		return targetDVName;
	}

	public void setTargetDVName(String targetDVName) {
		this.targetDVName = targetDVName;
	}

	public String getTargetDSFilter() {
		return targetDSFilter;
	}

	public void setTargetDSFilter(String targetDSFilter) {
		this.targetDSFilter = targetDSFilter;
	}

	// ------------------------------ HELPER METHODS

	public DataSource getTargetDS() {
		return new DataSource(getTargetDSId());
	}

	public void setTargetDS(DataSource target) {
		setTargetDSId(target.getId());
		setTargetDSName(target.getName());
	}

	public DataView getTargetDV() {
		return new DataView(getTargetDVId());
	}

	public void setTargetDV(DataView dataView) {
		setTargetDVId(dataView.getId());
		setTargetDVName(dataView.getName());
	}

	public boolean getRequiredSafely() {
		return ObjectUtil.isTrue(getRequired());
	}

	public String getTitleOrName() {
		return getTitle() != null ? getTitle() : getName();
	}

	public boolean getInFilterPanelSafely() {
		//return getInFilterPanel() == null || getInFilterPanel(); //TODO
		return ObjectUtil.isTrue(getInFilterPanel());
	}

	public String getUiName() {
		String result;
		if (getTitle() != null) {
			result = String.format("%s (%s)", getName(), getTitle());
		} else {
			result = getName();
		}
		return result;
	}

	// ------------------------------ OBJECT METHODS

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof DataAbstractFieldVO)) return false;

		DataAbstractFieldVO that = (DataAbstractFieldVO) o;

		return !(getName() != null ? !getName().equals(that.getName()) : that.getName() != null);

	}

	@Override
	public int hashCode() {
		return getName() != null ? getName().hashCode() : 0;
	}

	@Override
	public String toString() {
		String result = getUiName();
		return result != null ? result : "[?]";
	}

	@Override
	public int compareTo(DataAbstractFieldVO other) {
		if (getFilterPanelOrder() != null && other != null && other.getFilterPanelOrder() != null) {
			return getFilterPanelOrder().compareTo(other.getFilterPanelOrder());
		}
		return 0;
	}
}
