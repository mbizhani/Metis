package org.devocative.metis.entity.data.config;

import com.thoughtworks.xstream.annotations.XStreamAlias;

@XStreamAlias("aggFunc")
public enum XDVAggregatorFunction {
	Sum, Min, Max, Avg
}
