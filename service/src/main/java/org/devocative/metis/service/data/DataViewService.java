package org.devocative.metis.service.data;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.XStreamException;
import com.thoughtworks.xstream.annotations.XStreamAlias;
import org.devocative.adroit.ConfigUtil;
import org.devocative.adroit.StringEncryptorUtil;
import org.devocative.adroit.cache.ICache;
import org.devocative.adroit.date.UniDate;
import org.devocative.adroit.obuilder.ObjectBuilder;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.adroit.sql.SqlHelper;
import org.devocative.adroit.sql.ei.ExportImportHelper;
import org.devocative.adroit.sql.ei.Importer;
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
import org.devocative.demeter.vo.UserVO;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.MetisErrorCode;
import org.devocative.metis.MetisException;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.entity.data.config.XDataView;
import org.devocative.metis.iservice.IMetisExternalService;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.iservice.data.IDataSourceService;
import org.devocative.metis.iservice.data.IDataViewService;
import org.devocative.metis.vo.filter.data.DataViewFVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.*;
import java.util.stream.Collectors;

import static org.devocative.adroit.sql.XQuery.sql;

@Service("mtsDataViewService")
public class DataViewService implements IDataViewService {
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

	@Autowired(required = false)
	private IMetisExternalService metisExternalService;

	@Autowired
	private IDBConnectionService dbConnectionService;

	// ------------------------------

	@PostConstruct
	public void initDataViewService() {
		xStream = new AdroitXStream();
		xStream.processAnnotations(XDataView.class);

		dataViewCache = cacheService.create(CACHE_KEY, 50, key -> {
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
		});
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

		//NOTE: weired error in audit table persistence! try to insert null in A_MT_MTS_DATAVIEW_GROUP.F_DATA_VIEW!!! Now it is ok!
		dataView.setGroups(new ArrayList<>(groups));

		try {
			persistorService.startTrx();

			persistorService.saveOrUpdate(config);
			persistorService.saveOrUpdate(dataView);

			persistorService.commitOrRollback();
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
	public String exportAll(List<DataGroup> dataGroups, DBConnectionGroup dbConnectionGroup, String dataViewNames) {
		logger.info("**");
		logger.info("*** Exporting All Start: user=[{}]", securityService.getCurrentUser());

		Map<String, Object> params = new HashMap<>();

		//Set<String> groupIds = new HashSet<>();
		Set<String> dvNames = new HashSet<>();

		try (Connection connection = persistorService.createSqlConnection()) {
			ExportImportHelper helper = new ExportImportHelper(connection);

			SqlHelper sqlHelper = new SqlHelper(connection);
			sqlHelper.addXMLQueryFile(getClass().getResourceAsStream("/export_sql.xml"));

			if (dataGroups != null) {
				List<String> groupIds = dataGroups.stream().map(DataGroup::getId).collect(Collectors.toList());

				List<String> dv = sqlHelper.firstColAsList("dataViewsByGroup", ObjectBuilder
					.<String, Object>map()
					.put("grp", groupIds)
					.get());
				dvNames.addAll(dv);
			}

			if (dataViewNames != null) {
				String[] lines = dataViewNames.split("[\n]");
				List<String> names = Arrays.stream(lines).filter(s -> s.trim().length() > 0).collect(Collectors.toList());
				dvNames.addAll(names);

				/*List<String> grpIds = sqlHelper.firstColAsList("dataGroupsByDataView",
					ObjectBuilder
						.<String, Object>map()
						.put("dv_name", names)
						.get());
				groupIds.addAll(grpIds);*/
			}

			// ---------------

			if (dbConnectionGroup != null) {
				helper.exportBySql("dbConnGrp",
					sqlHelper.selectAll("dbConnGrp", ObjectBuilder.<String, Object>map()
						.put("dbConnGrpName", dbConnectionGroup.getName())
						.get())
						.toListOfMap()
				);
			}

			helper.exportBySql("group",
				sqlHelper.selectAll("group").toListOfMap()
			);

			FilterPlugin filter = new FilterPlugin();
			if (!dvNames.isEmpty()) {
				filter.add("dv.c_name", FilterValue.equal(dvNames));
			}

			helper.exportBySql("dataSource",
				sqlHelper.selectAll("dataSource", params, filter).toListOfMap()
			);

			helper.exportBySql("dataSrcRel",
				sqlHelper.selectAll("dataSrcRel", ObjectBuilder.<String, Object>map().put("dv_name", dvNames).get())
					.toListOfMap()
			);


			helper.exportBySql("dataView",
				sqlHelper.selectAll("dataView", params, filter).toListOfMap()
			);

			helper.exportBySql("group_dataView",
				sqlHelper.selectAll("group_dataView", params, filter).toListOfMap()
			);


			helper.exportBySql("report",
				sqlHelper.selectAll("report", params, filter).toListOfMap()
			);

			helper.exportBySql("group_report",
				sqlHelper.selectAll("group_report", params, filter).toListOfMap()
			);

			if (metisExternalService != null) {
				metisExternalService.onExportReport(helper, sqlHelper, filter);
			}

			String fileId = export2("exportDataView", "ALL", helper, ConfigUtil.getBoolean(MetisConfigKey.ExportAllWriteMeta));

			logger.info("*** Exporting All Finished: user=[{}]", securityService.getCurrentUser());
			logger.info("**");

			return fileId;
		} catch (Exception e) {
			logger.error("exportAll", e);
			throw new RuntimeException(e);
		}
	}

	@Override
	public void importAll(InputStream stream) {
		logger.info("**");
		logger.info("*** Importing All Start: user=[{}]", securityService.getCurrentUser());

		String xml = readText("ALL", stream, ConfigUtil.getBoolean(MetisConfigKey.ImportAllVerify));

		try (Connection connection = persistorService.createSqlConnection()) {
			try {
				connection.setAutoCommit(false);

				ExportImportHelper helper = new ExportImportHelper(connection);
				helper.importFrom(new ByteArrayInputStream(xml.getBytes("UTF-8")));

				SqlHelper sqlHelper = new SqlHelper(connection);
				sqlHelper.addXMLQueryFile(DataViewService.class.getResourceAsStream("/export_sql.xml"));

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

				if (metisExternalService != null) {
					metisExternalService.onImportReport(helper, sqlHelper);
				}

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
			logger.error("importAll", e);
			throw new RuntimeException(e);
		}
	}

	@Override
	public String exportReport(List<DataGroup> dataGroups, List<Report> reports) {
		logger.info("**");
		logger.info("*** Exporting Report Start: user=[{}]", securityService.getCurrentUser());

		Map<String, Object> params = new HashMap<>();

		//Set<String> groupIds = new HashSet<>();
		Set<String> reportIds = new HashSet<>();

		try (Connection connection = persistorService.createSqlConnection()) {
			ExportImportHelper helper = new ExportImportHelper(connection);

			SqlHelper sqlHelper = new SqlHelper(connection);
			sqlHelper.addXMLQueryFile(DataViewService.class.getResourceAsStream("/export_sql.xml"));

			if (dataGroups != null) {
				List<String> groupIds = dataGroups.stream().map(DataGroup::getId).collect(Collectors.toList());

				List<String> rptIds = sqlHelper.firstColAsList("reportsByDataGroup",
					ObjectBuilder
						.<String, Object>map()
						.put("grp_id", groupIds)
						.get());
				reportIds.addAll(rptIds);
			}

			if (reports != null) {
				List<String> rptIds = reports.stream().map(Report::getId).collect(Collectors.toList());
				reportIds.addAll(rptIds);
			}

			/*List<String> grpIds = sqlHelper.firstColAsList("dataGroupsByReport",
				ObjectBuilder
					.<String, Object>map()
					.put("rp_id", reportIds)
					.get());
			groupIds.addAll(grpIds);*/


			helper.exportBySql("group",
				sqlHelper.selectAll("group").toListOfMap()
			);


			// ---------------

			List<String> dataViewIds = sqlHelper.firstColAsList("dataViewsByReport",
				ObjectBuilder
					.<String, Object>map()
					.put("rp_id", reportIds)
					.get());

			FilterPlugin filter = new FilterPlugin();
			filter.add("dv.id", FilterValue.equal(dataViewIds));

			helper.exportBySql("dataSource",
				sqlHelper.selectAll("dataSource", params, filter).toListOfMap()
			);

			helper.exportBySql("dataView",
				sqlHelper.selectAll("dataView", params, filter).toListOfMap()
			);

			helper.exportBySql("group_dataView",
				sqlHelper.selectAll("group_dataView", params, filter).toListOfMap()
			);

			filter = new FilterPlugin().add("rp.id", FilterValue.equal(reportIds));
			helper.exportBySql("report",
				sqlHelper.selectAll("report", params, filter).toListOfMap()
			);

			helper.exportBySql("group_report",
				sqlHelper.selectAll("group_report", params, filter).toListOfMap()
			);

			if (metisExternalService != null) {
				metisExternalService.onExportReport(helper, sqlHelper, filter);
			}

			String fileId = export2("export-report", "REPORT", helper, ConfigUtil.getBoolean(MetisConfigKey.ExportReportWriteMeta));

			logger.info("*** Exporting Report Finished: user=[{}]", securityService.getCurrentUser());
			logger.info("**");

			return fileId;
		} catch (Exception e) {
			logger.error("exportReport", e);
			throw new RuntimeException(e);
		}
	}

	@Override
	public void importReport(InputStream stream, String sentDBConnection) {
		final UserVO currentUser = securityService.getCurrentUser();


		logger.info("**");
		logger.info("*** Importing Report Start: user=[{}]", currentUser);

		List<String> importedReports = new ArrayList<>();
		String xml = readText("REPORT", stream, ConfigUtil.getBoolean(MetisConfigKey.ImportReportVerify));

		try (Connection connection = persistorService.createSqlConnection()) {
			try {
				connection.setAutoCommit(false);

				ExportImportHelper helper = new ExportImportHelper(connection);
				helper.importFrom(new ByteArrayInputStream(xml.getBytes("UTF-8")));
				List<? extends Map<String, Object>> reports = helper.getDataSets().get("report");
				for (Map<String, Object> report : reports) {
					importedReports.add(report.get("id").toString());
				}

				SqlHelper sqlHelper = new SqlHelper(connection);
				sqlHelper.addXMLQueryFile(DataViewService.class.getResourceAsStream("/export_sql.xml"));

				Date now = new Date();
				Map<String, Object> other = new HashMap<>();
				other.put("d_creation", now);
				other.put("f_creator_user", currentUser.getUserId());
				other.put("d_modification", now);
				other.put("f_modifier_user", currentUser.getUserId());

				helper.setCommonData(other);

				dbConn(helper, sqlHelper);
				group(helper, sqlHelper);

				dataSrc(helper, sqlHelper);
				//dataSrcRel(helper, sqlHelper);

				dataView(helper, sqlHelper);
				group_dataView(helper, sqlHelper);

				report(helper, sqlHelper);
				group_report(helper, sqlHelper);

				if (metisExternalService != null) {
					metisExternalService.onImportReport(helper, sqlHelper);
				}

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

			logger.info("*** Importing Report Finished: user=[{}]", currentUser);
			logger.info("**");

			if (metisExternalService != null) {
				DBConnection dbConnection = dbConnectionService.loadByName(sentDBConnection);
				metisExternalService.afterImportedReports(importedReports, dbConnection);
			}

		} catch (Exception e) {
			logger.error("importReport", e);
			throw new RuntimeException(e);
		}
	}

	// ------------------------------

	private String export2(String name, String target, ExportImportHelper helper, boolean writeMeta) {
		try {
			final UserVO currentUser = securityService.getCurrentUser();

			String fileName = String.format("%s-%s.xml", name,
				securityService.getCurrentUser().formatDate(new Date(), "yyyyMMdd"));


			FileStoreHandler fileStoreHandler = fileStoreService.create(
				fileName,
				EFileStorage.DISK,
				EMimeType.XML,
				UniDate.now().updateDay(5).toDate(),
				name
			);

			XStream xStream = new XStream();
			xStream.autodetectAnnotations(true);
			String xml = xStream.toXML(helper.getDataSets());

			if (writeMeta) {
				EMD emd = new EMD("1", target, currentUser.getUsername(), new Date().getTime(),
					StringEncryptorUtil.hash(xml));

				String header = StringEncryptorUtil.encodeBase64(xStream.toXML(emd).getBytes("UTF-8"));

				fileStoreHandler.write("<!-- ".getBytes("UTF-8"));
				fileStoreHandler.write(header.getBytes("UTF-8"));
				fileStoreHandler.write(" -->\n".getBytes("UTF-8"));
			}
			fileStoreHandler.write(xml.getBytes("UTF-8"));

			fileStoreHandler.close();

			return fileStoreHandler.getFileStore().getFileId();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	private String readText(String target, InputStream stream, boolean checkHeader) {
		try (BufferedReader reader = new BufferedReader(new InputStreamReader(stream))) {
			String text = reader.lines().collect(Collectors.joining("\n"));

			if (checkHeader || text.startsWith("<!-- ")) {
				if (checkHeader && !text.startsWith("<!-- ")) {
					throw new RuntimeException("Invalid Import File: No Header!");
				}

				final int lrIdx = text.indexOf('\n');
				String header = text.substring(0, lrIdx);
				header = header.substring(5, header.length() - 4);
				logger.info("Import - Raw Header = [{}]", header);
				header = StringEncryptorUtil.decodeBase64(header);
				logger.info("Import - XML Header = [{}]", header);

				XStream xStream = new XStream();
				xStream.processAnnotations(EMD.class);
				EMD emd = (EMD) xStream.fromXML(header);
				logger.info("Import - Raw Meta Data = [{}] - [{}]", emd, target);

				if (!target.equals(emd.getT())) {
					throw new RuntimeException(String.format("Invalid Import Target: %s, Expected %s", emd.getT(), target));
				}

				text = text.substring(lrIdx + 1);
				String chk = StringEncryptorUtil.hash(text);

				if (!chk.equals(emd.getC())) {
					throw new RuntimeException("Invalid Import File: Checksum Error");
				}

				return text;
			}

			return text;
		} catch (XStreamException e) {
			logger.error("checkImport, XStream Conversion", e);
			throw new RuntimeException("Invalid Import Header");
		} catch (Exception e) {
			logger.error("checkImport", e);
			throw new RuntimeException(e);
		}
	}

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
				logger.info("No DBConnectionGroup, creating one!");
				midRPId = 0;

				Map<String, Object> defDbConnGrp = ObjectBuilder.<String, Object>map()
					.put("id", midRPId)
					.put("c_name", "DefaultGroup")
					.put("n_version", 1)
					.get();

				Importer db_conn_grp = helper.createImporter("t_mts_db_conn_grp",
					Arrays.asList("id", "c_name", "n_version", "d_creation", "f_creator_user"));
				db_conn_grp.addInsert(defDbConnGrp, helper.getCommonData());
				db_conn_grp.executeBatch();
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

	// ------------------------------

	@XStreamAlias("emd")
	private static class EMD {
		private String v;
		private String t;
		private String u;
		private Long d;
		private String c;

		public EMD() {
		}

		public EMD(String v, String t, String u, Long d, String c) {
			this.v = v;
			this.t = t;
			this.u = u;
			this.d = d;
			this.c = c;
		}

		public String getV() {
			return v;
		}

		public String getT() {
			return t;
		}

		public String getU() {
			return u;
		}

		public Long getD() {
			return d;
		}

		public String getC() {
			return c;
		}

		@Override
		public String toString() {
			Date dt = new Date(d);
			return String.format("Ver=%s, User=%s, Date=%s", v, u, dt);
		}
	}

}