package org.devocative.metis.entity.connection;

import javax.persistence.AttributeConverter;
import java.util.Arrays;
import java.util.List;

public enum EAliasMode {
	NORMAL(1, "Normal"),
	REPORT(2, "Report");

	// ------------------------------

	private Integer id;

	private String name;

	// ------------------------------

	EAliasMode(Integer id, String name) {
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

	public static List<EAliasMode> list() {
		return Arrays.asList(values());
	}

	// ------------------------------

	public static class Converter implements AttributeConverter<EAliasMode, Integer> {
		@Override
		public Integer convertToDatabaseColumn(EAliasMode eAliasMode) {
			return eAliasMode != null ? eAliasMode.getId() : null;
		}

		@Override
		public EAliasMode convertToEntityAttribute(Integer integer) {
			for (EAliasMode literal : values()) {
				if (literal.getId().equals(integer)) {
					return literal;
				}
			}
			return null;
		}
	}
}
