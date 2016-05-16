package org.devocative.metis.web.odata2;

import org.apache.olingo.odata2.api.ODataService;
import org.apache.olingo.odata2.api.ODataServiceFactory;
import org.apache.olingo.odata2.api.exception.ODataException;
import org.apache.olingo.odata2.api.processor.ODataContext;

public class MetisServiceFactory extends ODataServiceFactory {
	@Override
	public ODataService createService(ODataContext ctx) throws ODataException {
		return createODataSingleProcessorService(MetisEdmProvider.get(), MetisODataSingleProcessor.get());
	}
}
