package org.devocative.metis.web.odata2;

import org.apache.olingo.odata2.api.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.ep.EntityProvider;
import org.apache.olingo.odata2.api.ep.EntityProviderWriteProperties;
import org.apache.olingo.odata2.api.exception.ODataException;
import org.apache.olingo.odata2.api.processor.ODataResponse;
import org.apache.olingo.odata2.api.processor.ODataSingleProcessor;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.devocative.demeter.core.ModuleLoader;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.async.DataViewQVO;
import org.devocative.metis.vo.async.DataViewRVO;

import java.util.HashMap;

public class MetisODataSingleProcessor extends ODataSingleProcessor {
	private static MetisODataSingleProcessor INSTANCE;

	public static MetisODataSingleProcessor get() {
		if (INSTANCE == null) {
			INSTANCE = new MetisODataSingleProcessor();
		}

		return INSTANCE;
	}

	private IDataService dataService;

	public MetisODataSingleProcessor() {
		dataService = ModuleLoader.getApplicationContext().getBean(IDataService.class);
	}

	// ------------------------------

	@Override
	public ODataResponse readEntitySet(GetEntitySetUriInfo uriInfo, String contentType) throws ODataException {
		EdmEntitySet entitySet = uriInfo.getStartEntitySet();
		DataViewQVO request = new DataViewQVO();
		request
			.setName(entitySet.getEntityType().getName())
			.setFilter(new HashMap<String, Object>())
			.setPageIndex(1)
			.setPageSize(10);

		DataViewRVO dataViewRVO = dataService.executeDataView(request);

		return EntityProvider.writeFeed(
			contentType,
			entitySet,
			dataViewRVO.getList(),
			EntityProviderWriteProperties
				.serviceRoot(getContext().getPathInfo().getServiceRoot())
				.build()
		);
	}
}
