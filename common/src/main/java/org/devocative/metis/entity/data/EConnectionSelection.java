package org.devocative.metis.entity.data;

import javax.persistence.AttributeConverter;
import java.util.Arrays;
import java.util.List;

public enum EConnectionSelection {
	THREE_STEPS(1, "Three-Step"),
	FIXED(2, "Fixed");

	// ------------------------------

	private Integer id;

	private String name;

	// ------------------------------

	EConnectionSelection(Integer id, String name) {
		this.id = id;
		this.name = name;
	}

	// ------------------------------

	public Integer getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	// ------------------------------

	@Override
	public String toString() {
		return getName();
	}

	// ------------------------------

	public static List<EConnectionSelection> list() {
		return Arrays.asList(values());
	}

	// ------------------------------

	public static class Converter implements AttributeConverter<EConnectionSelection, Integer> {
		@Override
		public Integer convertToDatabaseColumn(EConnectionSelection eConnectionSelection) {
			return eConnectionSelection != null ? eConnectionSelection.getId() : null;
		}

		@Override
		public EConnectionSelection convertToEntityAttribute(Integer integer) {
			for (EConnectionSelection literal : values()) {
				if (literal.getId().equals(integer)) {
					return literal;
				}
			}
			return null;
		}
	}
}
