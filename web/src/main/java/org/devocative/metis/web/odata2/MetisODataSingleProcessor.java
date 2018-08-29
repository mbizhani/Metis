package org.devocative.metis.web.odata2;

import org.apache.olingo.odata2.api.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.ep.EntityProvider;
import org.apache.olingo.odata2.api.ep.EntityProviderWriteProperties;
import org.apache.olingo.odata2.api.exception.ODataException;
import org.apache.olingo.odata2.api.processor.ODataResponse;
import org.apache.olingo.odata2.api.processor.ODataSingleProcessor;
import org.apache.olingo.odata2.api.uri.KeyPredicate;
import org.apache.olingo.odata2.api.uri.expression.OrderExpression;
import org.apache.olingo.odata2.api.uri.info.*;
import org.devocative.demeter.core.DemeterCore;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.query.ODataQVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class MetisODataSingleProcessor extends ODataSingleProcessor {
	private static final Logger logger = LoggerFactory.getLogger(MetisODataSingleProcessor.class);

	private static MetisODataSingleProcessor INSTANCE;

	private IDataService dataService;
	private ISecurityService securityService;

	// ------------------------------

	private MetisODataSingleProcessor() {
		dataService = DemeterCore.get().getApplicationContext().getBean(IDataService.class);
		securityService = DemeterCore.get().getApplicationContext().getBean(ISecurityService.class);
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
		try {
			return selectQuery(uriInfo, contentType);
		} catch (Exception e) {
			logger.error("MetisODataSingleProcessor.readEntitySet", e);
			throw e;
		}
	}

	@Override
	public ODataResponse countEntitySet(GetEntitySetCountUriInfo uriInfo, String contentType) throws ODataException {
		try {
			return countQuery(uriInfo);
		} catch (Exception e) {
			logger.error("MetisODataSingleProcessor.countEntitySet", e);
			throw e;
		}
	}

	// --------------- NOT IMPLEMENTED!

	@Override
	public ODataResponse readEntity(GetEntityUriInfo uriInfo, String contentType) throws ODataException {
		logger.debug("OData.readEntity: {}", uriInfo);
		//throw new RuntimeException("NI: readEntity!");
		return selectRow(uriInfo, contentType);
	}

	@Override
	public ODataResponse readEntitySimpleProperty(GetSimplePropertyUriInfo uriInfo, String contentType) throws ODataException {
		logger.debug("OData.readEntitySimpleProperty: {}", uriInfo);
		throw new RuntimeException("NI: readEntitySimpleProperty!");
	}

	@Override
	public ODataResponse readEntitySimplePropertyValue(GetSimplePropertyUriInfo uriInfo, String contentType) throws ODataException {
		logger.debug("OData.readEntitySimplePropertyValue: {}", uriInfo);
		throw new RuntimeException("NI: readEntitySimplePropertyValue!");
	}

	@Override
	public ODataResponse readEntityComplexProperty(GetComplexPropertyUriInfo uriInfo, String contentType) throws ODataException {
		logger.debug("OData.readEntityComplexProperty: {}", uriInfo);
		throw new RuntimeException("NI: readEntityComplexProperty!");
	}

	// ------------------------------

	private ODataResponse selectQuery(GetEntitySetUriInfo uriInfo, String contentType) throws ODataException {
		EdmEntitySet entitySet = uriInfo.getStartEntitySet();

		Map<String, String> customQueryOptions = uriInfo.getCustomQueryOptions();

		logger.info("OData: DataList: DataView=[{}] User=[{}] CustomQueryParams=[{}]",
			entitySet.getEntityType().getName(), securityService.getCurrentUser(), customQueryOptions);

		Integer top = 2000;
		Integer skip = 0;

		if (uriInfo.getSkip() != null) {
			skip = uriInfo.getSkip();
		}

		if (uriInfo.getTop() != null) {
			top = uriInfo.getTop();
		}

		ODataQVO dataQVO = new ODataQVO(entitySet.getEntityType().getName())
			.setFirstResult(skip + 1)
			.setMaxResults(top - skip);

		if (uriInfo.getOrderBy() != null) {
			Map<String, String> orderBy = new LinkedHashMap<>();
			for (OrderExpression orderExpression : uriInfo.getOrderBy().getOrders()) {
				orderBy.put(orderExpression.getExpression().getUriLiteral(), orderExpression.getSortOrder().toString());
			}
			dataQVO.setOrderBy(orderBy);
		}

		Map<String, Object> inputParams = new HashMap<>();

		if (uriInfo.getFilter() != null) {
			DataVO dataVO = dataService.loadDataVOByName(entitySet.getEntityType().getName());
			List<DataParameterVO> params = dataVO.getParams();
			List<String> mainQueryParams = new ArrayList<>();
			for (DataParameterVO param : params) {
				mainQueryParams.add(param.getName());
			}

			SQLExpressionVisitor visitor = new SQLExpressionVisitor(inputParams, mainQueryParams);
			Object accept = uriInfo.getFilter().accept(visitor);

			logger.info("\tOData: Filter Expr=[{}] Params=[{}]", accept, visitor.getParamsValue());

			dataQVO.setFilterExpression(accept.toString());
		}

		/*
		if (uriInfo.getCustomQueryOptions() != null) {
			inputParams.putAll(uriInfo.getCustomQueryOptions());
		}
		*/

		dataQVO.setInputParams(inputParams);
		//processCommonOfUri(customQueryOptions, dataQVO);

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

	private ODataResponse countQuery(GetEntitySetCountUriInfo uriInfo) throws ODataException {
		EdmEntitySet entitySet = uriInfo.getStartEntitySet();

		Map<String, String> customQueryOptions = uriInfo.getCustomQueryOptions();

		logger.info("OData Count: DataView=[{}] User=[{}] CustomQueryParams=[{}]",
			entitySet.getEntityType().getName(), securityService.getCurrentUser(), customQueryOptions);

		ODataQVO dataQVO = new ODataQVO(entitySet.getEntityType().getName());

		Map<String, Object> inputParams = new HashMap<>();

		if (uriInfo.getFilter() != null) {
			DataVO dataVO = dataService.loadDataVOByName(entitySet.getEntityType().getName());
			List<DataParameterVO> params = dataVO.getParams();
			List<String> mainQueryParams = new ArrayList<>();
			for (DataParameterVO param : params) {
				mainQueryParams.add(param.getName());
			}

			SQLExpressionVisitor visitor = new SQLExpressionVisitor(inputParams, mainQueryParams);
			Object accept = uriInfo.getFilter().accept(visitor);

			logger.info("\tOData Count: Filter Expr=[{}] Params=[{}]", accept, visitor.getParamsValue());

			dataQVO.setFilterExpression(accept.toString());
		}

		dataQVO.setInputParams(inputParams);

		//processCommonOfUri(customQueryOptions, dataQVO);

		Long dataCount = dataService.executeODataCount(dataQVO);

		return EntityProvider.writeText(String.valueOf(dataCount));
	}

	private ODataResponse selectRow(GetEntityUriInfo uriInfo, String contentType) throws ODataException {
		List<KeyPredicate> keyPredicates = uriInfo.getKeyPredicates();
		if (keyPredicates != null && !keyPredicates.isEmpty()) {
			EdmEntitySet entitySet = uriInfo.getStartEntitySet();
			KeyPredicate keyPredicate = keyPredicates.get(0);
			Map<String, String> customQueryOptions = uriInfo.getCustomQueryOptions();

			logger.info("OData: SingleData: DataView=[{}] User=[{}] CustomQueryParams=[{}]",
				entitySet.getEntityType().getName(), securityService.getCurrentUser(), customQueryOptions);

			Map<String, Object> inputParams = new HashMap<>();
			inputParams.put(keyPredicate.getProperty().getName(), keyPredicate.getLiteral());

			ODataQVO dataQVO = new ODataQVO(entitySet.getEntityType().getName())
				.setFirstResult(1)
				.setMaxResults(1);
			dataQVO.setInputParams(inputParams);

			//processCommonOfUri(customQueryOptions, dataQVO);

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

		return null;
	}

	/*private void processCommonOfUri(Map<String, String> customQueryOptions, ODataQVO dataQVO) {
		//NOTE: Olingo dose not allow custom parameters to start with '$' character, so here we replace one with '~'
		String dbConnParamName = ConfigUtil.getString(MetisConfigKey.DBConnParamName);
		if (dbConnParamName != null) {
			if (dbConnParamName.startsWith("$")) {
				dbConnParamName = "~" + dbConnParamName.substring(1);
			}
			dataQVO.setSentDBConnection(customQueryOptions.get(dbConnParamName));
		}
	}*/
}
