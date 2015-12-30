package org.devocative.metis.web;

import org.apache.wicket.Page;
import org.apache.wicket.protocol.http.WebApplication;

public class MetisWebApplication extends WebApplication {

	@Override
	public Class<? extends Page> getHomePage() {
		return Index.class;
	}

}
