package org.devocative.metis.entity.data.config;

public enum XDSFieldType {
	String(XDSFieldFilterType.Equal, XDSFieldFilterType.Contain),
	Integer(XDSFieldFilterType.Equal, XDSFieldFilterType.Range),
	Real(XDSFieldFilterType.Equal, XDSFieldFilterType.Range),
	Date(XDSFieldFilterType.Equal, XDSFieldFilterType.Range),
	DateTime(XDSFieldFilterType.Equal, XDSFieldFilterType.Range),
	Boolean(XDSFieldFilterType.Equal),
	LookUp(XDSFieldFilterType.List, XDSFieldFilterType.Search);

	private XDSFieldFilterType[] properFilterTypes;

	XDSFieldType(XDSFieldFilterType... properFilterTypes) {
		this.properFilterTypes = properFilterTypes;
	}

	public XDSFieldFilterType[] getProperFilterTypes() {
		return properFilterTypes;
	}
}
