package org.devocative.metis.entity.connection;

import javax.persistence.Transient;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class EAliasMode implements Serializable {
	private static final long serialVersionUID = -7322451942960866911L;

	private static final Map<Integer, EAliasMode> ID_TO_LIT = new LinkedHashMap<>();

	// ------------------------------

	public static final EAliasMode NORMAL = new EAliasMode(1, "Normal");
	public static final EAliasMode REPORT = new EAliasMode(2, "Report");

	// ------------------------------

	private Integer id;

	@Transient
	private String name;

	// ------------------------------

	private EAliasMode(Integer id, String name) {
		this.id = id;
		this.name = name;

		ID_TO_LIT.put(id, this);
	}

	public EAliasMode() {
	}

	// ------------------------------

	public Integer getId() {
		return id;
	}

	public String getName() {
		return ID_TO_LIT.get(getId()).name;
	}

	// ------------------------------

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof EAliasMode)) return false;

		EAliasMode that = (EAliasMode) o;

		return !(getId() != null ? !getId().equals(that.getId()) : that.getId() != null);

	}

	@Override
	public int hashCode() {
		return getId() != null ? getId().hashCode() : 0;
	}

	@Override
	public String toString() {
		return getName();
	}

	// ------------------------------

	public static List<EAliasMode> list() {
		return new ArrayList<>(ID_TO_LIT.values());
	}
}
