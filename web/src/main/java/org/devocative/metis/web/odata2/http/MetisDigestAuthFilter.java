package org.devocative.metis.web.odata2.http;

import org.devocative.demeter.web.http.DemeterHttpAuthFilter;

import javax.servlet.annotation.WebFilter;

@WebFilter(
	filterName = "MetisOData2AuthFilter",
	urlPatterns = "/odata2.svc/*",
	servletNames = {"MetisOData2Servlet"}) //NOTE: "servletNames" is very important!
public class MetisDigestAuthFilter extends DemeterHttpAuthFilter {
}
