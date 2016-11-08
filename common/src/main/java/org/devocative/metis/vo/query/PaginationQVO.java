package org.devocative.metis.vo.query;

import java.io.Serializable;

public class PaginationQVO implements Serializable {
	private static final long serialVersionUID = -7300778106753773189L;

	private Long pageIndex;

	private Long pageSize;

	private Long firstResult;

	private Long maxResults;

	// ------------------------------

	public PaginationQVO setPageIndex(Long pageIndex) {
		this.pageIndex = pageIndex;
		return this;
	}

	public PaginationQVO setPageSize(Long pageSize) {
		this.pageSize = pageSize;
		return this;
	}

	public Long getFirstResult() {
		if (firstResult != null) {
			return firstResult;
		} else {
			return (pageIndex - 1) * pageSize + 1;
		}
	}

	public PaginationQVO setFirstResult(Long firstResult) {
		this.firstResult = firstResult;
		return this;
	}

	public Long getMaxResults() {
		if (maxResults != null) {
			return maxResults;
		} else {
			return pageSize;
		}
	}

	public PaginationQVO setMaxResults(Long maxResults) {
		this.maxResults = maxResults;
		return this;
	}

	// ------------------------------

	public static PaginationQVO byPage(Long pageIndex, Long pageSize) {
		return new PaginationQVO()
			.setPageIndex(pageIndex)
			.setPageSize(pageSize);
	}

	public static PaginationQVO byResult(Long firstResult, Long maxResults) {
		return new PaginationQVO()
			.setFirstResult(firstResult)
			.setMaxResults(maxResults);
	}
}
