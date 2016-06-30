package org.devocative.metis.vo.query;

import org.devocative.metis.entity.data.config.XDVAggregatorFunction;

import java.util.List;
import java.util.Map;

public class AggregateQueryQVO extends AbstractQueryQVO {
	private Map<String, List<XDVAggregatorFunction>> selectFields;

	public AggregateQueryQVO(String dataSourceName, Map<String, List<XDVAggregatorFunction>> selectFields) {
		super(dataSourceName);
		this.selectFields = selectFields;
	}

	public Map<String, List<XDVAggregatorFunction>> getSelectFields() {
		return selectFields;
	}
}
