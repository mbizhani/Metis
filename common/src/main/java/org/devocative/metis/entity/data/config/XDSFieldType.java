package org.devocative.metis.entity.data.config;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public enum XDSFieldType {
	String(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Contain, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Contain, XDSFieldFilterType.Range},
		null
	),
	Integer(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal},
		"#,###"
	),
	Real(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal},
		"#,###.###"
	),
	Date(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal},
		"yyyy/MM/dd"
	),
	DateTime(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal},
		"yyyy/MM/dd HH:mm:ss"
	),
	Boolean(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal},
		null
	),
	LookUp(
		new XDSFieldFilterType[]{XDSFieldFilterType.List, XDSFieldFilterType.Search},
		new XDSFieldFilterType[]{XDSFieldFilterType.List, XDSFieldFilterType.Search},
		null
	),

	Unknown(new XDSFieldFilterType[]{XDSFieldFilterType.Unknown}, null, null);

	// ------------------------------

	private XDSFieldFilterType[] fieldProperFilterTypes;

	private XDSFieldFilterType[] paramProperFilterTypes;

	private String format;

	XDSFieldType(XDSFieldFilterType[] fieldProperFilterTypes, XDSFieldFilterType[] paramProperFilterTypes, String format) {
		this.fieldProperFilterTypes = fieldProperFilterTypes;
		this.paramProperFilterTypes = paramProperFilterTypes;
		this.format = format;
	}

	public XDSFieldFilterType[] getFieldProperFilterTypes() {
		return fieldProperFilterTypes;
	}

	public XDSFieldFilterType[] getParamProperFilterTypes() {
		return paramProperFilterTypes;
	}

	public java.lang.String getFormat() {
		return format;
	}

	public boolean isNumerical() {
		return this == Integer || this == Real;
	}

	public boolean isFormatted() {
		return format != null;
	}

	public static List<XDSFieldType> getParameterProperTypes() {
		List<XDSFieldType> xdsFieldTypes = new ArrayList<>();
		Collections.addAll(xdsFieldTypes, XDSFieldType.values());
		xdsFieldTypes.remove(XDSFieldType.Unknown);
		return xdsFieldTypes;
	}
}
