package org.devocative.metis.vo.query;

import java.util.List;
import java.util.Map;

public class SelectQueryQVO extends AbstractQueryQVO {
	private List<String> selectFields;
	private Map<String, String> sortFields;
	private PaginationQVO pagination;

	private boolean considerParent = true;

	// ------------------------------

	public SelectQueryQVO(Long dataSourceId, List<String> selectFields) {
		super(dataSourceId);
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

	public boolean isConsiderParent() {
		return considerParent;
	}

	public SelectQueryQVO setConsiderParent(boolean considerParent) {
		this.considerParent = considerParent;
		return this;
	}
}
