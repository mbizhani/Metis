package org.devocative.metis.entity.connection.mapping;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;
import com.thoughtworks.xstream.annotations.XStreamOmitField;

import java.io.Serializable;
import java.util.List;

@XStreamAlias("entity")
public class XEntity implements Serializable {
	private static final long serialVersionUID = -3497361196657706842L;

	@XStreamAsAttribute
	private String name;

	@XStreamAsAttribute
	private String table;

	private List<XAbstractProperty> properties;

	@XStreamOmitField
	private XProperty id;

	// ------------------- ACCESSORS

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getTable() {
		return table;
	}

	public void setTable(String table) {
		this.table = table;
	}

	public List<XAbstractProperty> getProperties() {
		return properties;
	}

	public void setProperties(List<XAbstractProperty> properties) {
		this.properties = properties;
	}

	public XProperty getId() {
		if (id == null) {
			for (XAbstractProperty xAProp : properties) {
				if (xAProp instanceof XProperty) {
					XProperty xProperty = (XProperty) xAProp;
					if (xProperty.getIsId()) {
						id = xProperty;
						break;
					}
				}
			}
		}
		return id;
	}


	// ------------------- GENERAL METHODS

	public XAbstractProperty findProperty(String name) {
		for (XAbstractProperty xProp : properties) {
			if (xProp.getName().equals(name)) {
				return xProp;
			}
		}

		return null;
	}
}
