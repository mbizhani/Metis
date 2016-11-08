package org.devocative.metis.vo.query;

import java.util.List;
import java.util.Map;

public class SelectQueryQVO extends AbstractQueryQVO {
	private List<String> selectFields;
	private Map<String, String> sortFields;
	private PaginationQVO pagination;

	// ------------------------------

	public SelectQueryQVO(String dataSourceName, List<String> selectFields) {
		super(dataSourceName);
		this.selectFields = selectFields;
	}

	// ------------------------------

	public List<String> getSelectFields() {
		return selectFields;
	}

	public Map<String, String> getSortFields() {
		return sortFields;
	}

	public SelectQueryQVO setSortFields(Map<String, String> sortFields) {
		this.sortFields = sortFields;
		return this;
	}

	public PaginationQVO getPagination() {
		return pagination;
	}

	public SelectQueryQVO setPagination(PaginationQVO pagination) {
		this.pagination = pagination;
		return this;
	}
}
