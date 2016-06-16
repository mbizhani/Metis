package org.devocative.metis.web.odata2.http;

import org.apache.olingo.odata2.core.servlet.ODataServlet;

import javax.servlet.annotation.WebInitParam;
import javax.servlet.annotation.WebServlet;

@WebServlet(
	name = "MetisOData2Servlet",
	urlPatterns = "/odata2.svc/*",
	initParams = {
		@WebInitParam(
			name = "org.apache.olingo.odata2.service.factory",
			value = "org.devocative.metis.web.odata2.MetisServiceFactory"
		)
	}
)
public class MetisODataServlet extends ODataServlet {
}
