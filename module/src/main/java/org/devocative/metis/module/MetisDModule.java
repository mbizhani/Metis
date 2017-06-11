package org.devocative.metis.module;

import org.devocative.demeter.imodule.DModule;
import org.devocative.demeter.iservice.ApplicationLifecyclePriority;

public class MetisDModule implements DModule {
	@Override
	public void init() {
	}

	@Override
	public void shutdown() {
	}

	@Override
	public ApplicationLifecyclePriority getLifecyclePriority() {
		return ApplicationLifecyclePriority.Fourth;
	}
}
