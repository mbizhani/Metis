package org.devocative.metis.entity.data.config;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public enum XDSFieldType {
	String(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Contain, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Contain, XDSFieldFilterType.Range}
	),
	Integer(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal}
	),
	Real(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal}
	),
	Date(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal}
	),
	DateTime(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal}
	),
	Boolean(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal}
	),
	LookUp(
		new XDSFieldFilterType[]{XDSFieldFilterType.List, XDSFieldFilterType.Search},
		new XDSFieldFilterType[]{XDSFieldFilterType.List, XDSFieldFilterType.Search}
	),

	Unknown(new XDSFieldFilterType[]{XDSFieldFilterType.Unknown}, null);

	// ------------------------------

	private XDSFieldFilterType[] fieldProperFilterTypes;

	private XDSFieldFilterType[] paramProperFilterTypes;

	XDSFieldType(XDSFieldFilterType[] fieldProperFilterTypes, XDSFieldFilterType[] paramProperFilterTypes) {
		this.fieldProperFilterTypes = fieldProperFilterTypes;
		this.paramProperFilterTypes = paramProperFilterTypes;
	}

	public XDSFieldFilterType[] getFieldProperFilterTypes() {
		return fieldProperFilterTypes;
	}

	public XDSFieldFilterType[] getParamProperFilterTypes() {
		return paramProperFilterTypes;
	}

	public boolean isNumerical() {
		return this == Integer || this == Real;
	}

	public static List<XDSFieldType> getParameterProperTypes() {
		List<XDSFieldType> xdsFieldTypes = new ArrayList<>();
		Collections.addAll(xdsFieldTypes, XDSFieldType.values());
		xdsFieldTypes.remove(XDSFieldType.Unknown);
		return xdsFieldTypes;
	}
}
