package org.devocative.metis.service.data;

import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.CalendarUtil;
import org.devocative.adroit.cache.ICache;
import org.devocative.adroit.cache.IMissedHitHandler;
import org.devocative.adroit.xml.AdroitXStream;
import org.devocative.demeter.ei.ExportImportHelper;
import org.devocative.demeter.ei.Importer;
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

@Service("mtsDataViewService")
public class DataViewService implements IDataViewService, IMissedHitHandler<Long, DataView> {
	private static final Logger logger = LoggerFactory.getLogger(DataViewService.class);

	// ------------------------------

	private XStream xStream;
	private ICache<Long, DataView> dataViewCache;

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
	public DataView load(Long id) {
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
	public DataView loadForCache(Long key) {
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
	public void saveOrUpdate(Long dataViewId, String title, XDataView xDataView, List<DataGroup> groups) {
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

		persistorService.saveOrUpdate(config);
		persistorService.saveOrUpdate(dataView);

		dataView.setXDataView(xDataView);
		dataViewCache.update(dataView.getId(), dataView);
	}

	@Override
	public List<String> listForOData() {
		return persistorService
			.createQueryBuilder().addSelect("select ent.name")
			.addFrom(DataView.class, "ent")
			.list();
	}

	@Override
	public String exportAll() {
		logger.info("**");
		logger.info("*** Exporting DataView Start: user=[{}]", securityService.getCurrentUser());

		try {
			Connection connection = persistorService.createSqlConnection();

			ExportImportHelper helper = new ExportImportHelper(connection);

			helper.exportBySql("dbConnGrp",
				"select " +
					"  grp.id, " +
					"  grp.c_name, " +
					"  grp.c_driver, " +
					"  grp.c_test_query, " +
					"  grp.f_config, " +
					"  grp.n_version, " +
					"  cfg.c_value " +
					"from t_mts_db_conn_grp grp " +
					"join t_mts_cfg_lob cfg on cfg.id = grp.f_config " +
					"where grp.c_name='MidRP'"); //TODO: MidRP is hard-coded!!!

			helper.exportBySql("dataSource",
				"select " +
					"  ds.id, " +
					"  ds.c_name, " +
					"  ds.c_title, " +
					"  ds.n_version, " +
					"  ds.c_title_field, " +
					"  ds.c_key_field, " +
					"  ds.c_self_rel_pointer_field, " +
					"  ds.e_conn_selection, " +
					"  ds.f_config, " +
					"  cfg.c_value " +
					"from t_mts_data_src ds " +
					"join t_mts_cfg_lob cfg on cfg.id = ds.f_config");

			helper.exportBySql("dataView",
				"select " +
					"  dv.id, " +
					"  dv.c_name, " +
					"  dv.c_title, " +
					"  dv.f_data_src, " +
					"  dv.f_config, " +
					"  dv.n_version, " +
					"  cfg.c_value " +
					"from t_mts_data_view dv " +
					"join t_mts_cfg_lob cfg on cfg.id = dv.f_config");

			helper.exportBySql("dataSrcRel",
				"select " +
					"  id, " +
					"  b_deleted, " +
					"  f_src_datasrc, " +
					"  c_src_ptr_field, " +
					"  f_tgt_datasrc, " +
					"  n_version " +
					"from t_mts_data_src_rel " +
					"where b_deleted=0");

			helper.exportBySql("report",
				"select " +
					"  id, " +
					"  c_title, " +
					"  c_config, " +
					"  f_data_view, " +
					"  n_version " +
					"from t_mts_report");

			helper.exportBySql("group",
				"select " +
					"  id, " +
					"  c_name, " +
					"  n_version " +
					"from t_mts_data_group");

			helper.exportBySql("group_report",
				"select " +
					"  f_report, " +
					"  f_group " +
					"from mt_mts_report_group");

			helper.exportBySql("group_dataView",
				"select " +
					"  f_data_view, " +
					"  f_group " +
					"from mt_mts_dataview_group");

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

		try {
			Connection connection = persistorService.createSqlConnection();
			connection.setAutoCommit(false);

			ExportImportHelper helper = new ExportImportHelper(connection);
			helper.importFrom(stream);

			Date now = new Date();
			Map<String, Object> other = new HashMap<>();
			other.put("d_creation", now);
			other.put("f_creator_user", securityService.getCurrentUser().getUserId());
			other.put("d_modification", now);
			other.put("f_modifier_user", securityService.getCurrentUser().getUserId());

			helper.setCommonData(other);

			dbConn(helper);

			dataSrc(helper);
			dataSrcRel(helper);
			dataView(helper);
			report(helper);

			group(helper);
			group_report(helper);
			group_dataView(helper);

			connection.commit();

			cacheService.clear(IDataSourceService.CACHE_KEY);
			cacheService.clear(IDataViewService.CACHE_KEY);
			cacheService.clear(IDBConnectionService.CACHE_KEY_DB_CONNECTION);
			cacheService.clear(IDBConnectionService.CACHE_KEY_X_SCHEMA);
			cacheService.clear(IStringTemplateService.CACHE_KEY);

			logger.info("*** Importing DataView Finished: user=[{}]", securityService.getCurrentUser());
			logger.info("**");
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	// ------------------------------

	private void dbConn(ExportImportHelper helper) throws SQLException {
		Map<Object, Object> currentDbConnGrp = helper.selectAsMap("select id, n_version from t_mts_db_conn_grp");
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


		Object f_connection = helper.selectFirstCell("select id from t_mts_db_conn where c_name='default'");
		if (f_connection == null) {
			logger.info("No default db connection, creating one!");
			f_connection = 0; //NOTE: not using sequence, supposing related sequence has been started form one!
			Object midRPId = helper.selectFirstCell("select id from t_mts_db_conn_grp where c_name='MidRP'");

			Map<String, Object> defaultDbConn = new HashMap<>();
			defaultDbConn.put("id", f_connection);
			defaultDbConn.put("c_name", "default");
			defaultDbConn.put("c_username", "default");
			defaultDbConn.put("f_group", midRPId);
			defaultDbConn.put("n_version", 1);

			Importer db_conn = helper.createImporter("t_mts_db_conn",
				Arrays.asList("id", "c_name", "c_username", "f_group", "f_group", "n_version", "d_creation", "f_creator_user"));
			db_conn.addInsert(defaultDbConn, helper.getCommonData());
			db_conn.executeBatch();
		}
		logger.info("Default db connection id=[{}]", f_connection);
		helper.getCommonData().put("f_connection", f_connection);
	}

	private void dataSrc(ExportImportHelper helper) throws SQLException {
		Map<Object, Object> currentDataSrc = helper.selectAsMap("select id, n_version from t_mts_data_src");
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

	private void dataSrcRel(ExportImportHelper helper) throws SQLException {
		helper.clearTable("t_mts_data_src_rel");

		Importer dataSrcRel = helper.createImporter("t_mts_data_src_rel",
			Arrays.asList("id", "n_version", "b_deleted", "f_src_datasrc", "c_src_ptr_field", "f_tgt_datasrc", "d_creation", "f_creator_user")
		);

		for (Map<String, Object> row : helper.getDataSets().get("dataSrcRel")) {
			dataSrcRel.addInsert(row, helper.getCommonData());
		}

		dataSrcRel.executeBatch();
	}

	private void dataView(ExportImportHelper helper) throws SQLException {
		Map<Object, Object> currentDataView = helper.selectAsMap("select id, n_version from t_mts_data_view");
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

	private void report(ExportImportHelper helper) throws SQLException {
		Map<Object, Object> currentReport = helper.selectAsMap("select id, n_version from t_mts_report");
		logger.info("Current Report: size=[{}]", currentReport.size());

		Importer report = helper.createImporter("t_mts_report",
			Arrays.asList("id", "n_version", "c_title", "c_config", "f_data_view", "d_creation", "f_creator_user"),
			Arrays.asList("n_version", "c_title", "c_config", "f_data_view", "d_modification", "f_modifier_user"),
			Collections.singletonList("id")
		);

		helper.merge("report", "id", "n_version", currentReport, report);
	}

	private void group(ExportImportHelper helper) throws SQLException {
		Map<Object, Object> currentGroup = helper.selectAsMap("select id, n_version from t_mts_data_group");
		logger.info("Current DataGroup: size=[{}]", currentGroup.size());

		Importer group = helper.createImporter("t_mts_data_group",
			Arrays.asList("id", "n_version", "c_name", "d_creation", "f_creator_user"),
			Arrays.asList("n_version", "c_name", "d_modification", "f_modifier_user"),
			Collections.singletonList("id")
		);

		helper.merge("group", "id", "n_version", currentGroup, group);
	}

	private void group_report(ExportImportHelper helper) throws SQLException {
		helper.clearTable("mt_mts_report_group");

		Importer report = helper.createImporter("mt_mts_report_group",
			Arrays.asList("f_report", "f_group")
		);

		for (Map<String, Object> row : helper.getDataSets().get("group_report")) {
			report.addInsert(row);
		}

		report.executeBatch();
	}

	private void group_dataView(ExportImportHelper helper) throws SQLException {
		helper.clearTable("mt_mts_dataview_group");

		Importer report = helper.createImporter("mt_mts_dataview_group",
			Arrays.asList("f_data_view", "f_group")
		);

		for (Map<String, Object> row : helper.getDataSets().get("group_dataView")) {
			report.addInsert(row);
		}

		report.executeBatch();
	}

}