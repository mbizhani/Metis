package org.devocative.metis.vo.query;

public class CountQueryQVO extends AbstractQueryQVO {
	private boolean considerParent = true;

	// ------------------------------

	public CountQueryQVO(String dataSourceId) {
		super(dataSourceId);
	}

	// ------------------------------

	public boolean isConsiderParent() {
		return considerParent;
	}

	public CountQueryQVO setConsiderParent(boolean considerParent) {
		this.considerParent = considerParent;
		return this;
	}
}
