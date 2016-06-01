package org.devocative.metis.web.odata2;

import org.apache.olingo.odata2.api.ODataService;
import org.apache.olingo.odata2.api.ODataServiceFactory;
import org.apache.olingo.odata2.api.exception.ODataException;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MetisServiceFactory extends ODataServiceFactory {
	private static final Logger logger = LoggerFactory.getLogger(MetisServiceFactory.class);

	@Override
	public ODataService createService(ODataContext ctx) throws ODataException {
		try {
			return createODataSingleProcessorService(MetisEdmProvider.get(), MetisODataSingleProcessor.get());
		} catch (Exception e) {
			logger.error("Odata.MetisServiceFactory", e);
			throw new ODataException(e);
		}
	}
}
