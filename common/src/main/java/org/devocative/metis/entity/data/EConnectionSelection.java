package org.devocative.metis.entity.data;

import javax.persistence.Transient;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class EConnectionSelection implements Serializable {
	private static final long serialVersionUID = -3442466365149076064L;

	private static final Map<Integer, EConnectionSelection> ID_TO_LIT = new LinkedHashMap<>();

	// ------------------------------

	public static final EConnectionSelection THREE_STEPS = new EConnectionSelection(1, "Three-Step");
	public static final EConnectionSelection FIXED = new EConnectionSelection(2, "Fixed");

	// ------------------------------

	private Integer id;

	@Transient
	private String name;

	// ------------------------------

	private EConnectionSelection(Integer id, String name) {
		this.id = id;
		this.name = name;

		ID_TO_LIT.put(id, this);
	}

	public EConnectionSelection() {
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
		if (!(o instanceof EConnectionSelection)) return false;

		EConnectionSelection that = (EConnectionSelection) o;

		if (getId() != null ? !getId().equals(that.getId()) : that.getId() != null) return false;

		return true;
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

	public static List<EConnectionSelection> list() {
		return new ArrayList<>(ID_TO_LIT.values());
	}
}
