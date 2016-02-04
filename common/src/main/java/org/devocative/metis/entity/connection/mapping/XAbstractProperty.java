package org.devocative.metis.entity.connection.mapping;

import com.thoughtworks.xstream.annotations.XStreamAsAttribute;

import java.io.Serializable;

public abstract class XAbstractProperty implements Serializable {
	@XStreamAsAttribute
	private String name;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
