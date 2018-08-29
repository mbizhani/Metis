package org.devocative.metis.vo.query;

public class LookupQueryQVO extends AbstractQueryQVO {
	private String targetDataSourceId;

	// ------------------------------

	public LookupQueryQVO(String dataSourceId, String targetDataSourceId) {
		super(dataSourceId);

		this.targetDataSourceId = targetDataSourceId;
	}

	// ------------------------------

	public String getTargetDataSourceId() {
		return targetDataSourceId;
	}
}
