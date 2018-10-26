package org.devocative.metis.vo;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class RowInputVO implements Serializable {
	private static final long serialVersionUID = 6862093805855073553L;

	private Map<String, ?> row;

	private List<String> rowsKeys;

	// ------------------------------

	public RowInputVO(Map<String, ?> row) {
		this.row = row;
	}

	public RowInputVO(List<String> rowsKeys) {
		this.rowsKeys = rowsKeys;
	}

	// ------------------------------

	public Map<String, ?> getRow() {
		return row;
	}

	public List<String> getRowsKeys() {
		return new ArrayList<>(rowsKeys);
	}

	// ---------------

	public List<String> keys() {
		return new ArrayList<>(rowsKeys);
	}
}
