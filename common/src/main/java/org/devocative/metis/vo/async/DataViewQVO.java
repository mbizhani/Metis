package org.devocative.metis.vo.async;

import org.devocative.metis.vo.query.PaginationQVO;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

public class DataViewQVO implements Serializable {
	private static final long serialVersionUID = 5547889135754963068L;

	public enum TargetType {Main, ByParent, LookUp, Export, OData, ODataCount}
	public enum ExportType {Data, Print, Excel, PDF}

	private final TargetType target;
	private final String name;

	private PaginationQVO pagination;
	private Map<String, String> sortFieldList;
	private Map<String, Object> filter;
	private String filterExpression;

	private Serializable parentId;

	private ExportType exportType;

	private List<String> selectedRowsKeys;

	private String reportId;

	// ------------------------------

	public DataViewQVO(TargetType target, String name) {
		this.target = target;
		this.name = name;
	}

	// ------------------------------

	public TargetType getTarget() {
		return target;
	}

	public String getName() {
		return name;
	}

	public PaginationQVO getPagination() {
		return pagination;
	}

	public DataViewQVO setPagination(PaginationQVO pagination) {
		this.pagination = pagination;
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

	public String getFilterExpression() {
		return filterExpression;
	}

	public DataViewQVO setFilterExpression(String filterExpression) {
		this.filterExpression = filterExpression;
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
