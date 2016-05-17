package org.devocative.metis.web.odata2;

import org.apache.olingo.odata2.api.edm.EdmSimpleTypeKind;
import org.apache.olingo.odata2.api.edm.FullQualifiedName;
import org.apache.olingo.odata2.api.edm.provider.*;
import org.apache.olingo.odata2.api.exception.ODataException;
import org.devocative.demeter.core.ModuleLoader;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.IDataViewService;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class MetisEdmProvider extends EdmProvider {
	private static final Logger logger = LoggerFactory.getLogger(MetisEdmProvider.class);

	private static final String NAMESPACE = "Metis";
	private static final String ENTITY_CONTAINER = "MetisEntityContainer";

	private static MetisEdmProvider INSTANCE;

	private IDataService dataService;
	private IDataViewService dataViewService;
	private ISecurityService securityService;

	// ------------------------------

	private MetisEdmProvider() {
		dataService = ModuleLoader.getApplicationContext().getBean(IDataService.class);
		dataViewService = ModuleLoader.getApplicationContext().getBean(IDataViewService.class);

		securityService = ModuleLoader.getApplicationContext().getBean(ISecurityService.class);
	}

	// ------------------------------

	public static MetisEdmProvider get() {
		if (INSTANCE == null) {
			INSTANCE = new MetisEdmProvider();
		}
		return INSTANCE;
	}

	// ------------------------------

	@Override
	public List<Schema> getSchemas() throws ODataException {
		logger.info("OData: GetSchema: User=[{}]", securityService.getCurrentUser());

		EntityContainer entityContainer = new EntityContainer()
			.setName(ENTITY_CONTAINER)
			.setDefaultEntityContainer(true);

		List<EntityType> entityTypeList = new ArrayList<>();
		List<EntitySet> entitySetList = new ArrayList<EntitySet>();

		List<String> dataViewsForOData = dataViewService.listForOData();
		for (String dataViewName : dataViewsForOData) {
			FullQualifiedName edmFQName = new FullQualifiedName(NAMESPACE, dataViewName);

			EntityType entityType = getEntityType(edmFQName);
			entityTypeList.add(entityType);

			entitySetList.add(getEntitySet(ENTITY_CONTAINER, dataViewName));
		}

		entityContainer.setEntitySets(entitySetList);

		Schema schema = new Schema()
			.setNamespace(NAMESPACE)
			.setEntityTypes(entityTypeList)
			.setEntityContainers(Collections.singletonList(entityContainer));
		return Collections.singletonList(schema);
	}

	@Override
	public EntityType getEntityType(FullQualifiedName edmFQName) throws ODataException {
		if (NAMESPACE.equals(edmFQName.getNamespace())) {
			DataVO dataVO = dataService.loadDataVO(edmFQName.getName());
			if (dataVO != null) {
				List<Property> properties = new ArrayList<>();

				PropertyRef key = new PropertyRef().setName(dataVO.getFields().get(0).getName());

				for (DataFieldVO fieldVO : dataVO.getFields()) {
					EdmSimpleTypeKind type = EdmSimpleTypeKind.String;

					/*switch (fieldVO.getType()) {
						case Integer:
							type = EdmSimpleTypeKind.Int64;
							break;
						case Real:
							type = EdmSimpleTypeKind.Double;
							break;
						case Date:
						case DateTime:
							type = EdmSimpleTypeKind.DateTime;
							break;
						case Boolean:
							type = EdmSimpleTypeKind.Boolean;
							break;
					}*/

					Property property = new SimpleProperty()
						.setName(fieldVO.getName())
						.setType(type);
					properties.add(property);

					// TODO Navigation Properties

					if (fieldVO.getIsKeyFieldSafely()) {
						key.setName(fieldVO.getName());
					}
				}

				return new EntityType()
					.setName(dataVO.getName())
					.setProperties(properties)
					.setKey(new Key().setKeys(Collections.singletonList(key)));
			}
		}
		return null;
	}

	@Override
	public EntitySet getEntitySet(final String entityContainer, final String name) throws ODataException {
		if (ENTITY_CONTAINER.equals(entityContainer)) {
			return new EntitySet()
				.setName(name)
				.setEntityType(new FullQualifiedName(NAMESPACE, name));
		}
		return null;
	}

	@Override
	public EntityContainerInfo getEntityContainerInfo(final String name) throws ODataException {
		return new EntityContainerInfo()
			.setName(ENTITY_CONTAINER)
			.setDefaultEntityContainer(true);
	}
}
