package org.devocative.metis.service.data;

import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.CalendarUtil;
import org.devocative.adroit.cache.ICache;
import org.devocative.adroit.cache.IMissedHitHandler;
import org.devocative.adroit.obuilder.ObjectBuilder;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.adroit.sql.SqlHelper;
import org.devocative.adroit.sql.ei.ExportImportHelper;
import org.devocative.adroit.sql.ei.Importer;
import org.devocative.adroit.sql.filter.FilterType;
import org.devocative.adroit.sql.filter.FilterValue;
import org.devocative.adroit.sql.plugin.FilterPlugin;
import org.devocative.adroit.xml.AdroitXStream;
import org.devocative.demeter.entity.EFileStorage;
import org.devocative.demeter.entity.EMimeType;
import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.FileStoreHandler;
import org.devocative.demeter.iservice.ICacheService;
import org.devocative.demeter.iservice.IFileStoreService;
import org.devocative.demeter.iservice.ISecurityService;
import org.devocative.demeter.iservice.persistor.EJoinMode;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.demeter.iservice.template.IStringTemplateService;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.XDataView;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.iservice.data.IDataSourceService;
import org.devocative.metis.iservice.data.IDataViewService;
import org.devocative.metis.vo.filter.data.DataViewFVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.*;

import static org.devocative.adroit.sql.XQuery.sql;

@Service("mtsDataViewService")
public class DataViewService implements IDataViewService, IMissedHitHandler<String, DataView> {
	private static final Logger logger = LoggerFactory.getLogger(DataViewService.class);

	// ------------------------------

	private XStream xStream;
	private ICache<String, DataView> dataViewCache;

	@Autowired
	private IPersistorService persistorService;

	@Autowired
	private ICacheService cacheService;

	@Autowired
	private IDataSourceService dataSourceService;

	@Autowired
	private ISecurityService securityService;

	@Autowired
	private IFileStoreService fileStoreService;

	// ------------------------------

	@PostConstruct
	public void initDataViewService() {
		xStream = new AdroitXStream();
		xStream.processAnnotations(XDataView.class);

		dataViewCache = cacheService.create(CACHE_KEY, 50);
		dataViewCache.setMissedHitHandler(this);
	}

	// ------------------------------

	@Override
	public void saveOrUpdate(DataView entity) {
		persistorService.saveOrUpdate(entity);
	}

	@Override
	public DataView load(String id) {
		return dataViewCache.get(id);
	}

	@Override
	public DataView loadByName(String name) {
		DataView dv = dataViewCache.findByProperty("name", name);
		if (dv == null) {
			dv = persistorService
				.createQueryBuilder()
				.addFrom(DataView.class, "ent")
				.addJoin("cfg", "ent.config", EJoinMode.LeftFetch)
				.addJoin("grp", "ent.groups", EJoinMode.LeftFetch)
				.addWhere("and ent.name = :name")
				.addParam("name", name)
				.object();

			if (dv != null) {
				dv.setXDataView((XDataView) xStream.fromXML(dv.getConfig().getValue()));
				dataViewCache.put(dv.getId(), dv);
			} else {
				throw new MetisException(MetisErrorCode.InvalidDataViewName, name);
			}
		}
		return dv;
	}

	@Override
	public List<DataView> list() {
		return persistorService.list(DataView.class);
	}

	@Override
	public List<DataView> search(DataViewFVO filter, long pageIndex, long pageSize) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select ent")
			.addFrom(DataView.class, "ent")
			.applyFilter(DataView.class, "ent", filter)
			.list((pageIndex - 1) * pageSize, pageSize);
	}

	@Override
	public long count(DataViewFVO filter) {
		return persistorService
			.createQueryBuilder()
			.addSelect("select count(1)")
			.addFrom(DataView.class, "ent")
			.applyFilter(DataView.class, "ent", filter)
			.object();
	}

	@Override
	public List<DataSource> getDataSourceList() {
		return persistorService.list(DataSource.class);
	}

	@Override
	public List<DataGroup> getGroupsList() {
		return persistorService.list(DataGroup.class);
	}

	@Override
	public List<User> getCreatorUserList() {
		return persistorService.list(User.class);
	}

	@Override
	public List<User> getModifierUserList() {
		return persistorService.list(User.class);
	}

	// ==============================

	// IMissedHitHandler
	@Override
	public DataView loadForCache(String key) {
		DataView dv = persistorService
			.createQueryBuilder()
			.addFrom(DataView.class, "ent")
			.addJoin("cfg", "ent.config", EJoinMode.LeftFetch)
			.addJoin("grp", "ent.groups", EJoinMode.LeftFetch)
			.addWhere("and ent.id = :id")
			.addParam("id", key)
			.object();
		dv.setXDataView((XDataView) xStream.fromXML(dv.getConfig().getValue()));
		return dv;
	}

	@Override
	public void saveOrUpdate(String dataViewId, String title, XDataView xDataView, List<DataGroup> groups) {
		DataView dataView;
		ConfigLob config;

		if (dataViewId == null) {
			dataView = new DataView();
			config = new ConfigLob();
		} else {
			dataView = load(dataViewId);
			config = dataView.getConfig();
		}

		config.setValue(xStream.toXML(xDataView));

		dataView.setName(xDataView.getName());
		dataView.setTitle(title);
		dataView.setConfig(config);
		dataView.setDataSource(dataSourceService.load(xDataView.getDataSourceId()));
		dataView.setGroups(groups);

		try {
			persistorService.startTrx();

			persistorService.saveOrUpdate(config);
			persistorService.saveOrUpdate(dataView);

			persistorService.commitOrRollback();
			dataView.setXDataView(xDataView);
		} finally {
			//NOTE: it is important to remove, since it is in the middle of a trx and the result may not persist in DB
			dataViewCache.remove(dataView.getId());
		}
	}

	@Override
	public List<String> listForOData() {
		return persistorService
			.createQueryBuilder().addSelect("select ent.name")
			.addFrom(DataView.class, "ent")
			.list();
	}

	@Override
	public String exportAll(DataGroup dataGroup, DBConnectionGroup dbConnectionGroup, String dataViewNames) {
		logger.info("**");
		logger.info("*** Exporting DataView Start: user=[{}]", securityService.getCurrentUser());

		Map<String, Object> params = new HashMap<>();

		FilterPlugin filter = new FilterPlugin();
		if (dataGroup != null) {
			filter.add("grp.c_code", new FilterValue(dataGroup.getCode(), FilterType.Equal));
		}
		if (dataViewNames != null) {
			String[] lines = dataViewNames.split("[\n]");
			List<String> names = new ArrayList<>();
			for (String line : lines) {
				if (line.trim().length() > 0) {
					names.add(line.trim());
				}
			}
			filter.add("dv.c_name", new FilterValue(names, FilterType.Equal));
		}

		try {
			Connection connection = persistorService.createSqlConnection();

			ExportImportHelper helper = new ExportImportHelper(connection);

			SqlHelper sqlHelper = new SqlHelper(connection);
			sqlHelper.setXMLQueryFile(DataViewService.class.getResourceAsStream("/export_sql.xml"));

			if (dbConnectionGroup != null) {
				helper.exportBySql("dbConnGrp",
					sqlHelper.selectAll("dbConnGrp", ObjectBuilder.<String, Object>createDefaultMap()
						.put("dbConnGrpName", dbConnectionGroup.getName())
						.get())
						.toListOfMap()
				);
			}

			helper.exportBySql("group",
				sqlHelper.selectAll("group").toListOfMap()
			);


			helper.exportBySql("dataSource",
				sqlHelper.selectAll("dataSource", params, filter).toListOfMap()
			);

			helper.exportBySql("dataSrcRel",
				sqlHelper.selectAll("dataSrcRel").toListOfMap()
			);


			helper.exportBySql("dataView",
				sqlHelper.selectAll("dataView", params, filter).toListOfMap()
			);

			helper.exportBySql("group_dataView",
				sqlHelper.selectAll("group_dataView").toListOfMap()
			);


			helper.exportBySql("report",
				sqlHelper.selectAll("report", params, filter).toListOfMap()
			);

			helper.exportBySql("group_report",
				sqlHelper.selectAll("group_report").toListOfMap()
			);

			Date now = new Date();
			//TODO using calendar from User
			String fileName = String.format("exportDataView-%s.xml", CalendarUtil.toPersian(now, "yyyyMMdd"));
			FileStoreHandler fileStoreHandler = fileStoreService.create(
				fileName,
				EFileStorage.DISK,
				EMimeType.XML,
				CalendarUtil.add(now, Calendar.DATE, 15),
				"exportDataView"
			);
			helper.exportTo(fileStoreHandler);
			fileStoreHandler.close();
			connection.close();

			logger.info("*** Exporting DataView Finished: user=[{}]", securityService.getCurrentUser());
			logger.info("**");

			return fileStoreHandler.getFileStore().getFileId();
		} catch (Exception e) {
			logger.error("exportAll", e);
			throw new RuntimeException(e);
		}
	}

	@Override
	public void importAll(InputStream stream) {
		logger.info("**");
		logger.info("*** Importing DataView Start: user=[{}]", securityService.getCurrentUser());

		try (Connection connection = persistorService.createSqlConnection()) {
			try {
				connection.setAutoCommit(false);

				ExportImportHelper helper = new ExportImportHelper(connection);
				helper.importFrom(stream);

				SqlHelper sqlHelper = new SqlHelper(connection);
				sqlHelper.setXMLQueryFile(DataViewService.class.getResourceAsStream("/export_sql.xml"));

				Date now = new Date();
				Map<String, Object> other = new HashMap<>();
				other.put("d_creation", now);
				other.put("f_creator_user", securityService.getCurrentUser().getUserId());
				other.put("d_modification", now);
				other.put("f_modifier_user", securityService.getCurrentUser().getUserId());

				helper.setCommonData(other);

				dbConn(helper, sqlHelper);
				group(helper, sqlHelper);

				dataSrc(helper, sqlHelper);
				dataSrcRel(helper, sqlHelper);

				dataView(helper, sqlHelper);
				group_dataView(helper, sqlHelper);

				report(helper, sqlHelper);
				group_report(helper, sqlHelper);

				connection.commit();
			} catch (Exception e) {
				connection.rollback();
				throw e;
			}

			cacheService.clear(IDataSourceService.CACHE_KEY);
			cacheService.clear(IDataViewService.CACHE_KEY);
			cacheService.clear(IDBConnectionService.CACHE_KEY_DB_CONNECTION);
			cacheService.clear(IDBConnectionService.CACHE_KEY_X_SCHEMA);
			cacheService.clear(IStringTemplateService.CACHE_KEY);

			logger.info("*** Importing DataView Finished: user=[{}]", securityService.getCurrentUser());
			logger.info("**");
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	// ------------------------------

	private void dbConn(ExportImportHelper helper, SqlHelper sqlHelper) throws SQLException {
		Map<Object, Object> currentDbConnGrp = sqlHelper.twoCellsAsMap(sql("select id, n_version from t_mts_db_conn_grp"));
		logger.info("Current DbConnGrp: size=[{}]", currentDbConnGrp.size());

		Importer dbConnGrp = helper.createImporter("t_mts_db_conn_grp",
			Arrays.asList("id", "n_version", "c_name", "c_driver", "c_test_query", "f_config", "d_creation", "f_creator_user"),
			Arrays.asList("n_version", "c_name", "c_driver", "c_test_query", "f_config", "d_modification", "f_modifier_user"),
			Collections.singletonList("id")
		);

		Importer dbConnGrpLob = helper.createImporter("t_mts_cfg_lob",
			Arrays.asList("f_config:id", "n_version", "c_value", "d_creation", "f_creator_user"),
			Arrays.asList("n_version", "c_value", "d_modification", "f_modifier_user"),
			Collections.singletonList("f_config:id")
		);

		helper.merge("dbConnGrp", "id", "n_version", currentDbConnGrp, dbConnGrpLob, dbConnGrp);


		Object f_connection = sqlHelper.firstCell(sql("select id from t_mts_db_conn where c_name='default'"));
		if (f_connection == null) {
			logger.info("No default db connection, creating one!");
			f_connection = 0; //NOTE: not using sequence, supposing related sequence has been started form one!
			Object midRPId = sqlHelper.firstCell(sql("select id from t_mts_db_conn_grp order by d_creation"));

			if (midRPId == null) {
				throw new RuntimeException("No DBConnectionGroup Exits!");
			}

			Map<String, Object> defaultDbConn = new HashMap<>();
			defaultDbConn.put("id", f_connection);
			defaultDbConn.put("c_name", "default");
			defaultDbConn.put("c_username", "default");
			defaultDbConn.put("f_group", midRPId);
			defaultDbConn.put("n_version", 1);

			Importer db_conn = helper.createImporter("t_mts_db_conn",
				Arrays.asList("id", "c_name", "c_username", "f_group", "n_version", "d_creation", "f_creator_user"));
			db_conn.addInsert(defaultDbConn, helper.getCommonData());
			db_conn.executeBatch();
		}
		logger.info("Default db connection id=[{}]", f_connection);
		helper.getCommonData().put("f_connection", f_connection);
	}

	private void dataSrc(ExportImportHelper helper, SqlHelper sqlHelper) throws SQLException {
		Map<Object, Object> currentDataSrc = sqlHelper.twoCellsAsMap(sql("select id, n_version from t_mts_data_src"));
		logger.info("Current DataSrc: size=[{}]", currentDataSrc.size());

		Importer dataSrc = helper.createImporter("t_mts_data_src",
			Arrays.asList("id", "n_version", "c_name", "c_title", "c_title_field", "c_key_field", "c_self_rel_pointer_field", "e_conn_selection", "f_config", "f_connection", "d_creation", "f_creator_user"),
			Arrays.asList("n_version", "c_name", "c_title", "c_title_field", "c_key_field", "c_self_rel_pointer_field", "e_conn_selection", "d_modification", "f_modifier_user"),
			Collections.singletonList("id")
		);

		Importer dataSrcLob = helper.createImporter("t_mts_cfg_lob",
			Arrays.asList("f_config:id", "n_version", "c_value", "d_creation", "f_creator_user"),
			Arrays.asList("n_version", "c_value", "d_modification", "f_modifier_user"),
			Collections.singletonList("f_config:id")
		);

		helper.merge("dataSource", "id", "n_version", currentDataSrc, dataSrcLob, dataSrc);
	}

	private void dataSrcRel(ExportImportHelper helper, SqlHelper sqlHelper) throws SQLException {
		List<Object> dsIds = sqlHelper.firstColAsList(sql("select id from t_mts_data_src"));

		NamedParameterStatement nps = sqlHelper.createNPS("impDataSrcRel");
		for (Map<String, Object> row : helper.getDataSets().get("dataSrcRel")) {
			Object f_src_datasrc = row.get("f_src_datasrc");
			Object f_tgt_datasrc = row.get("f_tgt_datasrc");
			if (dsIds.contains(f_src_datasrc) && dsIds.contains(f_tgt_datasrc)) {
				row.putAll(helper.getCommonData());
				row.put("id", UUID.randomUUID().toString());

				nps.setParameters(row);
				nps.addBatch();
			}
		}
		nps.executeBatch();
	}

	private void dataView(ExportImportHelper helper, SqlHelper sqlHelper) throws SQLException {
		Map<Object, Object> currentDataView = sqlHelper.twoCellsAsMap(sql("select id, n_version from t_mts_data_view"));
		logger.info("Current DataView: size=[{}]", currentDataView.size());

		Importer dataView = helper.createImporter("t_mts_data_view",
			Arrays.asList("id", "n_version", "c_name", "c_title", "f_config", "f_data_src", "d_creation", "f_creator_user"),
			Arrays.asList("n_version", "c_name", "c_title", "d_modification", "f_modifier_user"),
			Collections.singletonList("id")
		);

		Importer dataViewLob = helper.createImporter("t_mts_cfg_lob",
			Arrays.asList("f_config:id", "n_version", "c_value", "d_creation", "f_creator_user"),
			Arrays.asList("n_version", "c_value", "d_modification", "f_modifier_user"),
			Collections.singletonList("f_config:id")
		);

		helper.merge("dataView", "id", "n_version", currentDataView, dataViewLob, dataView);
	}

	private void report(ExportImportHelper helper, SqlHelper sqlHelper) throws SQLException {
		Map<Object, Object> currentReport = sqlHelper.twoCellsAsMap(sql("select id, n_version from t_mts_report"));
		logger.info("Current Report: size=[{}]", currentReport.size());

		Importer report = helper.createImporter("t_mts_report",
			Arrays.asList("id", "n_version", "c_title", "c_config", "f_data_view", "d_creation", "f_creator_user"),
			Arrays.asList("n_version", "c_title", "c_config", "f_data_view", "d_modification", "f_modifier_user"),
			Collections.singletonList("id")
		);

		helper.merge("report", "id", "n_version", currentReport, report);
	}

	private void group(ExportImportHelper helper, SqlHelper sqlHelper) throws SQLException {
		Map<Object, Object> currentGroup = sqlHelper.twoCellsAsMap(sql("select id, n_version from t_mts_data_group"));
		logger.info("Current DataGroup: size=[{}]", currentGroup.size());

		Importer group = helper.createImporter("t_mts_data_group",
			Arrays.asList("id", "n_version", "c_name", "c_code", "d_creation", "f_creator_user"),
			Arrays.asList("n_version", "c_name", "c_code", "d_modification", "f_modifier_user"),
			Collections.singletonList("id")
		);

		helper.merge("group", "id", "n_version", currentGroup, group);
	}

	private void group_report(ExportImportHelper helper, SqlHelper sqlHelper) throws SQLException {
		List<Object> grpIds = sqlHelper.firstColAsList(sql("select id from t_mts_report"));

		NamedParameterStatement nps = sqlHelper.createNPS("imp_group_report");
		for (Map<String, Object> row : helper.getDataSets().get("group_report")) {
			Object f_report = row.get("f_report");
			if (grpIds.contains(f_report)) {
				row.putAll(helper.getCommonData());
				nps.setParameters(row);
				nps.addBatch();
			}
		}

		nps.executeBatch();
	}

	private void group_dataView(ExportImportHelper helper, SqlHelper sqlHelper) throws SQLException {
		List<Object> dvIds = sqlHelper.firstColAsList(sql("select id from t_mts_data_view"));

		NamedParameterStatement nps = sqlHelper.createNPS("imp_group_dataView");

		for (Map<String, Object> row : helper.getDataSets().get("group_dataView")) {
			Object f_data_view = row.get("f_data_view");
			if (dvIds.contains(f_data_view)) {
				row.putAll(helper.getCommonData());
				nps.setParameters(row);
				nps.addBatch();
			}
		}
		nps.executeBatch();
	}
}