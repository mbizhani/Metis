package org.devocative.metis.web.odata2.http;

import org.apache.olingo.odata2.core.servlet.ODataServlet;
import org.apache.olingo.odata2.core.servlet.RestUtil;
import org.apache.olingo.odata2.core.uri.SystemQueryOption;
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
import java.util.List;
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

			if (queryString != null) {
				Map<String, List<String>> parameters = RestUtil.extractAllQueryParameters(queryString);
				for (Map.Entry<String, List<String>> paramEntry : parameters.entrySet()) {
					String param = paramEntry.getKey();
					if (param.startsWith("$") && !isSystemQuery(param)) {
						//NOTE: Olingo dose not allow custom parameters to start with '$' character, so here we replace one with '~'
						queryString = queryString.replaceAll(
							String.format("\\%s=", param),
							String.format("%s%s=",
								ConfigUtil.getString(MetisConfigKey.ODataReplaceCharForNonSystemParam),
								param.substring(1)
							)
						);
					}
				}
			}

			return queryString;
		}

		// ------------------------------

		private boolean isSystemQuery(String param) {
			boolean result;

			try {
				result = SystemQueryOption.valueOf(param) != null;
			} catch (IllegalArgumentException e) {
				result = false;
			}

			return result;
		}
	}
}
