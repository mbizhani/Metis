package org.devocative.metis.vo.async;

import org.devocative.metis.vo.query.QueryExecInfoRVO;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class DataViewRVO implements Serializable {
	private static final long serialVersionUID = 1739759418739333193L;

	private List<Map<String, Object>> list;
	private List<Map<String, Object>> footer;
	private Long count;
	private String parentId;

	private List<QueryExecInfoRVO> queryExecInfoList = new ArrayList<>();

	private String fileId;

	// ------------------------------

	public List<Map<String, Object>> getList() {
		return list;
	}

	public DataViewRVO setList(List<Map<String, Object>> list) {
		this.list = list;
		return this;
	}

	public List<Map<String, Object>> getFooter() {
		return footer;
	}

	public DataViewRVO setFooter(List<Map<String, Object>> footer) {
		this.footer = footer;
		return this;
	}

	public Long getCount() {
		return count;
	}

	public DataViewRVO setCount(Long count) {
		this.count = count;
		return this;
	}

	public String getParentId() {
		return parentId;
	}

	public DataViewRVO setParentId(String parentId) {
		this.parentId = parentId;
		return this;
	}

	public List<QueryExecInfoRVO> getQueryExecInfoList() {
		return queryExecInfoList;
	}

	public String getFileId() {
		return fileId;
	}

	public DataViewRVO setFileId(String fileId) {
		this.fileId = fileId;
		return this;
	}

	// ------------------------------

	public void addQueryExecInfo(QueryExecInfoRVO queryExecInfo) {
		queryExecInfoList.add(queryExecInfo);
	}

	public void addQueryExecInfo(List<QueryExecInfoRVO> queryExecInfoList) {
		this.queryExecInfoList.addAll(queryExecInfoList);
	}
}
