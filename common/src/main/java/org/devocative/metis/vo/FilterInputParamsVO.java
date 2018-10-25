package org.devocative.metis.vo;

import java.io.Serializable;
import java.util.*;

public class FilterInputParamsVO implements Serializable {
	private static final long serialVersionUID = -7392528932552543861L;

	private Map<String, Object> params = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);

	// ------------------------------

	public boolean containsKey(String key) {
		return params.containsKey(key);
	}

	public Object get(String key) {
		return params.get(key);
	}

	public void putAll(Map<String, ?> map) {
		if (map != null) {
			map.entrySet().stream()
				.filter(it -> it.getValue() != null)
				.forEach(it -> params.put(it.getKey(), it.getValue()));
		}
	}

	// ---------------

	public Object getAsSingle(String key) {
		if (params.containsKey(key)) {
			final Object value = params.get(key);

			if (value instanceof Collection) {
				final Iterator it = ((Collection) value).iterator();
				if (it.hasNext()) {
					return it.next();
				}
			}
		}
		return null;
	}

	public String getAsString(String key) {
		return (String) getAsSingle(key);
	}

	public String getAsStringOrDefault(String key, String def) {
		if (params.containsKey(key)) {
			return (String) getAsSingle(key);
		}
		return def;
	}

	public List<String> getAsStringList(String key) {
		List<String> result = new ArrayList<>();

		if (params.containsKey(key)) {
			final Object value = params.get(key);
			if (value instanceof Collection) {
				result.addAll((Collection) value);
			} else {
				throw new RuntimeException("Invalid list for key: " + key);
			}
		}

		return result;
	}

	public Map<String, Object> unwrap() {
		return new HashMap<>(params);
	}

	// ---------------

	@Override
	public String toString() {
		return params.toString();
	}
}
