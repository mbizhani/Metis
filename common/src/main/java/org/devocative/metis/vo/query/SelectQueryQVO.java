package org.devocative.metis.vo.query;

import java.util.List;
import java.util.Map;

public class SelectQueryQVO extends AbstractQueryQVO {
	private List<String> selectFields;
	private Map<String, String> sortFields;
	private Long pageIndex;
	private Long pageSize;

	// ------------------------------

	public SelectQueryQVO(String queryCode, String dataSourceName, List<String> selectFields) {
		super(queryCode, dataSourceName);
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

	public Long getPageIndex() {
		return pageIndex;
	}

	public SelectQueryQVO setPageIndex(Long pageIndex) {
		this.pageIndex = pageIndex;
		return this;
	}

	public Long getPageSize() {
		return pageSize;
	}

	public SelectQueryQVO setPageSize(Long pageSize) {
		this.pageSize = pageSize;
		return this;
	}
}
