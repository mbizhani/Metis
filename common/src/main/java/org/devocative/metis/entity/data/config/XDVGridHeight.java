package org.devocative.metis.entity.data.config;

public enum XDVGridHeight {
	Short(350), Medium(500), Long(650);

	private int height;

	XDVGridHeight(int height) {
		this.height = height;
	}

	public int getHeight() {
		return height;
	}
}
