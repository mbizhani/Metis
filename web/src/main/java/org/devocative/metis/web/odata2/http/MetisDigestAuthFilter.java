package org.devocative.metis.web.odata2.http;

import org.devocative.demeter.web.http.DemeterHttpAuthFilter;

import javax.servlet.annotation.WebFilter;

@WebFilter(
	filterName = "MetisDigestAuthFilter",
	urlPatterns = "/odata.svc/*",
	servletNames = {"MetisODataServlet"}) //NOTE: "servletNames" is very important!
public class MetisDigestAuthFilter extends DemeterHttpAuthFilter {
}
