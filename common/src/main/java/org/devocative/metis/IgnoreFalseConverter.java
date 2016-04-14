package org.devocative.metis;

import com.thoughtworks.xstream.converters.basic.BooleanConverter;

public class IgnoreFalseConverter extends BooleanConverter {
	@Override
	public String toString(Object obj) {
		if (Boolean.FALSE.equals(obj)) {
			return null;
		}
		return super.toString(obj);
	}
}
