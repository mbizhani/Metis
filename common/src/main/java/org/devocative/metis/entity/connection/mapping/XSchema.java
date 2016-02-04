package org.devocative.metis.entity.connection.mapping;

import com.thoughtworks.xstream.annotations.XStreamAlias;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@XStreamAlias("schema")
public class XSchema implements Serializable {
	private List<XEntity> entities;

	// ------------------- ACCESSORS

	public List<XEntity> getEntities() {
		return entities;
	}

	public void setEntities(List<XEntity> entities) {
		this.entities = entities;
	}

// ------------------- GENERAL METHODS

	public XEntity findEntity(String name) {
		for (XEntity xEntity : entities) {
			if (xEntity.getName().equals(name)) {
				return xEntity;
			}
		}
		return null;
	}

	public Map<String, Map<String, String>> getHierarchy() {
		Map<String, Map<String, String>> result = new HashMap<>();
		for (XEntity xEntity : entities) {
			Map<String, String> props = new HashMap<>();
			for (XAbstractProperty xProp : xEntity.getProperties()) {
				props.put(xProp.getName(), null);
			}
			result.put(xEntity.getName(), props);
		}
		return result;
	}
}
