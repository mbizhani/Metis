package org.devocative.metis.vo;

import org.devocative.metis.entity.data.config.XDSFieldFilterType;
import org.devocative.metis.entity.data.config.XDSFieldType;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

public class FilterItemVO implements Serializable {
	private static final long serialVersionUID = -9179829849945087859L;

	public enum ComponentType {
		TextContain(XDSFieldType.String, XDSFieldFilterType.Contain),
		TextRange(XDSFieldType.String, XDSFieldFilterType.Range),
		TextSimple(XDSFieldType.String, XDSFieldFilterType.Equal),

		IntRange(XDSFieldType.Integer, XDSFieldFilterType.Range),
		IntSimple(XDSFieldType.Integer, XDSFieldFilterType.Equal),

		RealRange(XDSFieldType.Real, XDSFieldFilterType.Range),
		RealSimple(XDSFieldType.Real, XDSFieldFilterType.Equal),

		DateRange(XDSFieldType.Date, XDSFieldFilterType.Range),
		DateSimple(XDSFieldType.Date, XDSFieldFilterType.Equal),

		DateTimeRange(XDSFieldType.DateTime, XDSFieldFilterType.Range),
		DateTimeSimple(XDSFieldType.DateTime, XDSFieldFilterType.Equal),

		Boolean(XDSFieldType.Boolean, XDSFieldFilterType.Equal),

		LookUpList(XDSFieldType.LookUp, XDSFieldFilterType.List),
		LookUpSearch(XDSFieldType.LookUp, XDSFieldFilterType.Search);

		private final XDSFieldType type;
		private final XDSFieldFilterType filterType;

		ComponentType(XDSFieldType type, XDSFieldFilterType filterType) {
			this.type = type;
			this.filterType = filterType;
		}

		public boolean match(DataAbstractFieldVO field) {
			return field.getType() == type && field.getFilterType() == filterType;
		}
	}

	public enum PresentationMode {
		Default,
		Required,
		Disabled,
		Invisible,
		ExpectingSetData // TIP: complex mode
	}

	// ------------------------------

	private String name;
	private String caption;
	private ComponentType type;
	private PresentationMode mode;

	private Object defaultValue;

	private Boolean multipleSelection;
	private List listValues;

	private String error;

	private TargetSearchConfig targetSearchConfig;

	// ------------------------------

	public String getName() {
		return name;
	}

	public FilterItemVO setName(String name) {
		this.name = name;
		return this;
	}

	public String getCaption() {
		return caption;
	}

	public FilterItemVO setCaption(String caption) {
		this.caption = caption;
		return this;
	}

	public ComponentType getType() {
		return type;
	}

	public FilterItemVO setType(ComponentType type) {
		this.type = type;
		return this;
	}

	public PresentationMode getMode() {
		return mode;
	}

	public FilterItemVO setMode(PresentationMode mode) {
		this.mode = mode;
		return this;
	}

	public Object getDefaultValue() {
		return defaultValue;
	}

	public FilterItemVO setDefaultValue(Object defaultValue) {
		this.defaultValue = defaultValue;
		return this;
	}

	public Boolean getMultipleSelection() {
		return multipleSelection;
	}

	public FilterItemVO setMultipleSelection(Boolean multipleSelection) {
		this.multipleSelection = multipleSelection;
		return this;
	}

	public List getListValues() {
		return listValues;
	}

	public FilterItemVO setListValues(List listValues) {
		this.listValues = listValues;
		return this;
	}

	public String getError() {
		return error;
	}

	public FilterItemVO setError(String error) {
		this.error = error;
		return this;
	}

	public TargetSearchConfig getTargetSearchConfig() {
		return targetSearchConfig;
	}

	public FilterItemVO setTargetSearchConfig(TargetSearchConfig targetSearchConfig) {
		this.targetSearchConfig = targetSearchConfig;
		return this;
	}

	// ---------------

	@Override
	public String toString() {
		return getName();
	}

	// ------------------------------

	public static class TargetSearchConfig implements Serializable {
		private static final long serialVersionUID = -5242332590723676526L;

		private String targetId;
		private Map<String, List<String>> targetParam;

		public TargetSearchConfig(String targetId, Map<String, List<String>> targetParam) {
			this.targetId = targetId;
			this.targetParam = targetParam;
		}

		public String getTargetId() {
			return targetId;
		}

		public Map<String, List<String>> getTargetParam() {
			return targetParam;
		}
	}
}
