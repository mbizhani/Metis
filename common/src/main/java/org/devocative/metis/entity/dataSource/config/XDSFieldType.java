package org.devocative.metis.entity.dataSource.config;

public enum XDSFieldType {
	String(XDSFieldFilterType.None, XDSFieldFilterType.Equal, XDSFieldFilterType.Contain),
	Integer(XDSFieldFilterType.None, XDSFieldFilterType.Equal, XDSFieldFilterType.Range),
	Real(XDSFieldFilterType.None, XDSFieldFilterType.Equal, XDSFieldFilterType.Range),
	Date(XDSFieldFilterType.None, XDSFieldFilterType.Equal, XDSFieldFilterType.Range),
	DateTime(XDSFieldFilterType.None, XDSFieldFilterType.Equal, XDSFieldFilterType.Range),
	Boolean(XDSFieldFilterType.None, XDSFieldFilterType.Equal),
	LookUp(XDSFieldFilterType.List, XDSFieldFilterType.Search);

	private XDSFieldFilterType[] properFilterTypes;

	XDSFieldType(XDSFieldFilterType... properFilterTypes) {
		this.properFilterTypes = properFilterTypes;
	}

	public XDSFieldFilterType[] getProperFilterTypes() {
		return properFilterTypes;
	}
}
