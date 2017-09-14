package org.devocative.metis.web.odata2.http;

import org.apache.olingo.odata2.core.servlet.ODataServlet;
import org.devocative.adroit.ConfigUtil;
import org.devocative.metis.MetisConfigKey;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.annotation.WebInitParam;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Map;

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
	private static final long serialVersionUID = -8425872756837260540L;
	private static final Logger logger = LoggerFactory.getLogger(MetisODataServlet.class);

	@Override
	protected void service(HttpServletRequest req, HttpServletResponse resp) throws IOException {
		try {
			super.service(new ODataHttpServletRequestWrapper(req), resp);
		} catch (IOException e) {
			logger.error("MetisODataServlet.service", e);
			throw e;
		}
	}

	// ------------------------------

	private static class ODataHttpServletRequestWrapper extends HttpServletRequestWrapper {
		public ODataHttpServletRequestWrapper(HttpServletRequest request) {
			super(request);
		}

		@Override
		public String getParameter(String name) {
			throw new RuntimeException("Olingo: ODataHttpServletRequestWrapper.getParameter() should not be called!");
		}

		@Override
		public Map<String, String[]> getParameterMap() {
			throw new RuntimeException("Olingo: ODataHttpServletRequestWrapper.getParameterMap() should not be called!");
		}

		@Override
		public Enumeration<String> getParameterNames() {
			throw new RuntimeException("Olingo: ODataHttpServletRequestWrapper.getParameterMap() should not be called!");
		}

		@Override
		public String getQueryString() {
			String queryString = super.getQueryString();
			String dbConnParamName = ConfigUtil.getString(MetisConfigKey.DBConnParamName);
			if(queryString == null || dbConnParamName == null || !dbConnParamName.startsWith("$")) {
				return queryString;
			} else {
				//NOTE: Olingo dose not allow custom parameters to start with '$' character, so here we replace one with '~'
				return queryString.replaceAll("\\" + dbConnParamName, "~" + dbConnParamName.substring(1));
			}
		}
	}
}
