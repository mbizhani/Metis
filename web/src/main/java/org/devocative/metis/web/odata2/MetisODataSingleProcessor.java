package org.devocative.metis.web.odata2;

import org.apache.olingo.odata2.api.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.ep.EntityProvider;
import org.apache.olingo.odata2.api.ep.EntityProviderWriteProperties;
import org.apache.olingo.odata2.api.exception.ODataException;
import org.apache.olingo.odata2.api.processor.ODataResponse;
import org.apache.olingo.odata2.api.processor.ODataSingleProcessor;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.devocative.demeter.core.ModuleLoader;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.ODataQVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

public class MetisODataSingleProcessor extends ODataSingleProcessor {
	private static final Logger logger = LoggerFactory.getLogger(MetisODataSingleProcessor.class);

	private static MetisODataSingleProcessor INSTANCE;

	private IDataService dataService;
	private ISecurityService securityService;

	// ------------------------------

	private MetisODataSingleProcessor() {
		dataService = ModuleLoader.getApplicationContext().getBean(IDataService.class);
		securityService = ModuleLoader.getApplicationContext().getBean(ISecurityService.class);
	}

	// ------------------------------

	public static MetisODataSingleProcessor get() {
		if (INSTANCE == null) {
			INSTANCE = new MetisODataSingleProcessor();
		}

		return INSTANCE;
	}

	// ------------------------------

	@Override
	public ODataResponse readEntitySet(GetEntitySetUriInfo uriInfo, String contentType) throws ODataException {
		EdmEntitySet entitySet = uriInfo.getStartEntitySet();

		logger.info("OData: DataList: DataView=[{}] User=[{}]",
			entitySet.getEntityType().getName(), securityService.getCurrentUser());

		ODataQVO dataQVO = new ODataQVO(entitySet.getEntityType().getName())
			.setPageIndex(1)
			.setPageSize(10);

		if (uriInfo.getFilter() != null) {
			SQLExpressionVisitor visitor = new SQLExpressionVisitor();
			Object accept = uriInfo.getFilter().accept(visitor);

			logger.info("###>> getFilter = {}", accept);
			logger.info("###>> getFilter Params = {}", visitor.getParamsValue());

			dataQVO
				.setFilterExpression(accept.toString())
				.setFilterExpressionParams(visitor.getParamsValue());
		}


		List<Map<String, Object>> list = dataService.executeOData(dataQVO);

		return EntityProvider.writeFeed(
			contentType,
			entitySet,
			list,
			EntityProviderWriteProperties
				.serviceRoot(getContext().getPathInfo().getServiceRoot())
				.build()
		);
	}
}
