package org.devocative.metis.web;

import org.apache.wicket.Page;
import org.apache.wicket.Session;
import org.apache.wicket.protocol.http.WebApplication;
import org.apache.wicket.request.Request;
import org.apache.wicket.request.Response;

public class MetisWebApplication extends WebApplication {

	@Override
	public Class<? extends Page> getHomePage() {
		return Index.class;
	}

	@Override
	public Session newSession(Request request, Response response) {
		return new MetisWebSession(request);
	}
}
