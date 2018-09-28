package org.devocative.metis.vo.async;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

public class DataViewQVO implements Serializable {
	private static final long serialVersionUID = 5547889135754963068L;

	public enum ExportType {Data, Print, Excel, PDF}

	private String name;
	private long pageIndex;
	private long pageSize;
	private Map<String, String> sortFieldList;
	private Map<String, Object> filter;

	private Serializable parentId;

	private ExportType exportType;

	private List<String> selectedRowsKeys;

	private String reportId;

	// ------------------------------

	public String getName() {
		return name;
	}

	public DataViewQVO setName(String name) {
		this.name = name;
		return this;
	}

	public long getPageIndex() {
		return pageIndex;
	}

	public DataViewQVO setPageIndex(long pageIndex) {
		this.pageIndex = pageIndex;
		return this;
	}

	public long getPageSize() {
		return pageSize;
	}

	public DataViewQVO setPageSize(long pageSize) {
		this.pageSize = pageSize;
		return this;
	}

	public Map<String, String> getSortFieldList() {
		return sortFieldList;
	}

	public DataViewQVO setSortFieldList(Map<String, String> sortFieldList) {
		this.sortFieldList = sortFieldList;
		return this;
	}

	public Map<String, Object> getFilter() {
		return filter;
	}

	public DataViewQVO setFilter(Map<String, Object> filter) {
		this.filter = filter;
		return this;
	}

	public Serializable getParentId() {
		return parentId;
	}

	public DataViewQVO setParentId(Serializable parentId) {
		this.parentId = parentId;
		return this;
	}

	public boolean isDoExport() {
		return exportType != null;
	}

	public ExportType getExportType() {
		return exportType;
	}

	public DataViewQVO setExportType(ExportType exportType) {
		this.exportType = exportType;
		return this;
	}

	public List<String> getSelectedRowsKeys() {
		return selectedRowsKeys;
	}

	public DataViewQVO setSelectedRowsKeys(List<String> selectedRowsKeys) {
		this.selectedRowsKeys = selectedRowsKeys;
		return this;
	}

	public String getReportId() {
		return reportId;
	}

	public DataViewQVO setReportId(String reportId) {
		this.reportId = reportId;
		return this;
	}
}
