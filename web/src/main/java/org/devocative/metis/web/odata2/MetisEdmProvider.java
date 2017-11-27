package org.devocative.metis.web.odata2;

import org.apache.olingo.odata2.api.edm.EdmSimpleTypeKind;
import org.apache.olingo.odata2.api.edm.FullQualifiedName;
import org.apache.olingo.odata2.api.edm.provider.*;
import org.apache.olingo.odata2.api.exception.ODataException;
import org.devocative.demeter.core.DemeterCore;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.metis.entity.data.config.XDSFieldResultType;
import org.devocative.metis.iservice.IDataEventHandler;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.data.IDataViewService;
import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class MetisEdmProvider extends EdmProvider implements IDataEventHandler {
	private static final Logger logger = LoggerFactory.getLogger(MetisEdmProvider.class);

	private static final String NAMESPACE = "Metis";
	private static final String ENTITY_CONTAINER = "MetisEntityContainer";

	private static MetisEdmProvider INSTANCE;

	private List<Schema> SCHEMA;
	private ODataException creationException;

	private Map<String, EntityType> ENTITY_TYPE_MAP = new LinkedHashMap<>();
	private Map<String, EntitySet> ENTITY_SET_MAP = new LinkedHashMap<>();

	private IDataService dataService;
	private IDataViewService dataViewService;
	private ISecurityService securityService;

	// ------------------------------

	private MetisEdmProvider() {
		dataService = DemeterCore.get().getApplicationContext().getBean(IDataService.class);
		dataService.addDataEventHandler(this);

		dataViewService = DemeterCore.get().getApplicationContext().getBean(IDataViewService.class);
		securityService = DemeterCore.get().getApplicationContext().getBean(ISecurityService.class);

		try {
			createSchema();
		} catch (ODataException e) {
			logger.error("MetisEdmProvider()", e);
			creationException = e;
		}
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

		if (creationException != null) {
			throw creationException;
		}

		return SCHEMA;
	}

	@Override
	public EntityType getEntityType(FullQualifiedName edmFQName) throws ODataException {
		if (NAMESPACE.equals(edmFQName.getNamespace())) {
			return ENTITY_TYPE_MAP.get(edmFQName.getName());
		}
		return null;
	}

	@Override
	public EntitySet getEntitySet(final String entityContainer, final String name) throws ODataException {
		if (ENTITY_CONTAINER.equals(entityContainer)) {
			return ENTITY_SET_MAP.get(name);
		}
		return null;
	}

	@Override
	public EntityContainerInfo getEntityContainerInfo(final String name) throws ODataException {
		return new EntityContainerInfo()
			.setName(ENTITY_CONTAINER)
			.setDefaultEntityContainer(true);
	}

	// ---------------

	@Override
	public void handleDataVoSaved(DataVO dataVO) {
		logger.info("MetisEdmProvider.DataViewChanged: {}", dataVO.getName());

		ENTITY_TYPE_MAP.put(dataVO.getName(), getEntityType(dataVO));

		if (!ENTITY_SET_MAP.containsKey(dataVO.getName())) {
			ENTITY_SET_MAP.put(dataVO.getName(), getEntitySet(dataVO.getName()));
		}

		createAndSetSchemaObject();
	}

	// ------------------------------

	private void createSchema() throws ODataException {
		try {
			List<String> dataViewsForOData = dataViewService.listForOData();
			for (String dataViewName : dataViewsForOData) {
				try {
					DataVO dataVO = dataService.loadDataVO(dataViewName);
					EntityType entityType = getEntityType(dataVO);
					ENTITY_TYPE_MAP.put(dataViewName, entityType);

					ENTITY_SET_MAP.put(dataViewName, getEntitySet(dataViewName));
				} catch (Exception e) {
					logger.error("OData: GetSchema: EntityType(DataView) addition problem = {}", dataViewName, e);
				}
			}

			createAndSetSchemaObject();
		} catch (Exception e) {
			logger.error("OData: GetSchema", e);
			throw new ODataException(e);
		}
	}

	private void createAndSetSchemaObject() {
		EntityContainer entityContainer = new EntityContainer()
			.setName(ENTITY_CONTAINER)
			.setDefaultEntityContainer(true)
			.setEntitySets(new ArrayList<>(ENTITY_SET_MAP.values()));

		Schema schema = new Schema()
			.setNamespace(NAMESPACE)
			.setEntityTypes(new ArrayList<>(ENTITY_TYPE_MAP.values()))
			.setEntityContainers(Collections.singletonList(entityContainer));
		SCHEMA = Collections.singletonList(schema);
	}

	private EntityType getEntityType(DataVO dataVO) {
		List<Property> properties = new ArrayList<>();

		PropertyRef key = new PropertyRef().setName(dataVO.getFields().get(0).getName());

		for (DataAbstractFieldVO fieldVO : dataVO.getAllFields()) {
			if (fieldVO.getResultType() != XDSFieldResultType.None) {
				EdmSimpleTypeKind type = EdmSimpleTypeKind.String;

				switch (fieldVO.getType()) {
					case Integer:
						type = EdmSimpleTypeKind.Decimal;
						break;
					case Real:
						type = EdmSimpleTypeKind.Double;
						break;
					case Date:
					case DateTime:
						type = EdmSimpleTypeKind.DateTime;
						break;
				}

				Property property = new SimpleProperty()
					.setName(fieldVO.getName())
					.setType(type);
				properties.add(property);

				// TODO Navigation Properties

				if (fieldVO instanceof DataFieldVO) {
					DataFieldVO dataFieldVO = (DataFieldVO) fieldVO;
					if (dataFieldVO.getIsKeyFieldSafely()) {
						key.setName(dataFieldVO.getName());
					}
				} else {
					property.setDocumentation(new Documentation().setSummary("SQL Parameter"));
				}
			}
		}

		return new EntityType()
			.setName(dataVO.getName())
			.setProperties(properties)
			.setKey(new Key().setKeys(Collections.singletonList(key)));
	}

	private EntitySet getEntitySet(String name) {
		return new EntitySet()
			.setName(name)
			.setEntityType(new FullQualifiedName(NAMESPACE, name));
	}
}
