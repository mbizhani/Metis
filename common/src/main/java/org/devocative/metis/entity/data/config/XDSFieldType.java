package org.devocative.metis.entity.data.config;

import org.devocative.adroit.ConfigUtil;
import org.devocative.adroit.IConfigKey;
import org.devocative.metis.MetisConfigKey;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public enum XDSFieldType {
	String(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Contain, XDSFieldFilterType.TextSearch, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal},
		null
	),
	Integer(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal},
		MetisConfigKey.GridFormatInteger
	),
	Real(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal},
		MetisConfigKey.GridFormatReal
	),
	Date(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal},
		MetisConfigKey.GridFormatDate
	),
	DateTime(
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal, XDSFieldFilterType.Range},
		new XDSFieldFilterType[]{XDSFieldFilterType.Equal},
		MetisConfigKey.GridFormatDateTime
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

	private IConfigKey format;

	XDSFieldType(XDSFieldFilterType[] fieldProperFilterTypes, XDSFieldFilterType[] paramProperFilterTypes, IConfigKey format) {
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

	public String getFormat() {
		return ConfigUtil.getString(format);
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
