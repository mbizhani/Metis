package org.devocative.metis;

import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.ConfigUtil;
import org.devocative.adroit.obuilder.ObjectBuilder;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.adroit.sql.SqlHelper;
import org.devocative.adroit.sql.mig.PKMigrate;
import org.devocative.adroit.xml.AdroitXStream;
import org.devocative.metis.entity.data.config.*;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Map;

public class PkXmlMigrate {

	public static void main(String[] args) throws SQLException {
		Connection connection = createConnection("/config.properties");

		SqlHelper helper = new SqlHelper(connection);
		helper.setXMLQueryFile(PkXmlMigrate.class.getResourceAsStream("/oracle_xml_mig.xml"));

		connection.setAutoCommit(false);

		try {
			PKMigrate.migrate(connection,
				"t_mts_db_conn_grp", "t_mts_data_group", "t_mts_cfg_lob", "t_mts_data_src", "t_mts_data_src_rel",
				"t_mts_data_view", "t_mts_report");

			// after dataSource mig
			dataSrcTarget(helper);
			dataViewXmlForDataSrc(helper);

			// after dataView mig
			dataViewXml(helper);

			//TODO: drop audit tables and regenerate all
		} catch (Exception e) {
			e.printStackTrace();
			connection.rollback();
		} finally {
			connection.close();
		}
	}

	private static void dataViewXml(SqlHelper helper) throws SQLException {
		XStream xStream = new AdroitXStream();
		xStream.processAnnotations(XDataView.class);

		Map<String, String> dvMap = helper.twoCellsAsMap("dvMap");
		Map<BigDecimal, String> dvIdMap = helper.twoCellsAsMap("dvIdMap");

		NamedParameterStatement updateCfg = helper.createNPS("updateCfg");

		Map<String, String> dvXml = helper.twoCellsAsMap("dvXml");
		for (Map.Entry<String, String> entry : dvXml.entrySet()) {
			String cfgId = entry.getKey();
			String xml = entry.getValue();

			XDataView xDataView = (XDataView) xStream.fromXML(xml);
			if (xDataView.getLinks() != null) {
				for (XDVLink xdvLink : xDataView.getLinks()) {
					BigDecimal oldId = new BigDecimal(xdvLink.getTargetDVId());
					String id = dvIdMap.get(oldId);
					String name = dvMap.get(id);

					if (id == null) {
						throw new RuntimeException(String.format("Invalid cfgId=%s: dvName=%s, newDvName=%s",
							cfgId, xDataView.getName(), name));
					}

					if (!xDataView.getName().equals(name)) {
						System.out.println(String.format("Invalid cfgId=%s: dvName=%s, newDvName=%s",
							cfgId, xDataView.getName(), name));
					}

					xdvLink.setTargetDVId(id);
					xdvLink.setTargetDVName(name);
				}

				String newXml = xStream.toXML(xDataView);
				updateCfg
					.setParameters(ObjectBuilder.<String, Object>map()
						.put("xml", newXml)
						.put("id", cfgId)
						.get())
					.executeUpdate();
			}
		}
	}

	private static void dataViewXmlForDataSrc(SqlHelper helper) throws SQLException {
		XStream xStream = new AdroitXStream();
		xStream.processAnnotations(XDataView.class);

		Map<String, String> dsMap = helper.twoCellsAsMap("dsMap");
		Map<BigDecimal, String> dsIdMap = helper.twoCellsAsMap("dsIdMap");

		NamedParameterStatement updateCfg = helper.createNPS("updateCfg");

		Map<String, String> dvXml = helper.twoCellsAsMap("dvXml");
		for (Map.Entry<String, String> entry : dvXml.entrySet()) {
			String cfgId = entry.getKey();
			String xml = entry.getValue();

			XDataView xDataView = (XDataView) xStream.fromXML(xml);

			BigDecimal oldId;
			try {
				oldId = new BigDecimal(xDataView.getDataSourceId());
			} catch (Exception e) {
				continue;
			}
			String id = dsIdMap.get(oldId);
			String name = dsMap.get(id);

			if (id == null || !xDataView.getDataSourceName().equals(name)) {
				throw new RuntimeException(String.format("Invalid cfgId=%s: dsId=%s dsName=%s, newDsName=%s",
					cfgId, id, xDataView.getDataSourceName(), name));
			}

			xDataView.setDataSourceId(id);

			String newXml = xStream.toXML(xDataView);

			updateCfg
				.setParameters(ObjectBuilder.<String, Object>map()
					.put("xml", newXml)
					.put("id", cfgId)
					.get())
				.executeUpdate();
		}
	}

	private static void dataSrcTarget(SqlHelper helper) throws SQLException {
		XStream xstream = new AdroitXStream();
		xstream.processAnnotations(XDataSource.class);

		Map<String, String> dsMap = helper.twoCellsAsMap("dsMap");
		Map<BigDecimal, String> dsIdMap = helper.twoCellsAsMap("dsIdMap");

		NamedParameterStatement updateCfg = helper.createNPS("updateCfg");

		Map<String, String> dsXml = helper.twoCellsAsMap("dsXml");
		for (Map.Entry<String, String> entry : dsXml.entrySet()) {
			String cfgId = entry.getKey();
			String xml = entry.getValue();

			XDataSource xDataSource = (XDataSource) xstream.fromXML(xml);

			for (XDSField xdsField : xDataSource.getFields()) {
				if (xdsField.getTargetDSId() != null) {
					BigDecimal oldId;
					try {
						oldId = new BigDecimal(xdsField.getTargetDSId());
					} catch (Exception e) {
						continue;
					}
					String id = dsIdMap.get(oldId);
					String name = dsMap.get(id);

					if (id == null) {
						throw new RuntimeException(String.format("Invalid cfgId=%s: dsName=%s, newDsName=%s",
							cfgId, xdsField.getTargetDSName(), name));
					}

					if (!xdsField.getTargetDSName().equals(name)) {
						System.out.println(String.format("Invalid cfgId=%s: dsId=%s dsName=%s, newDsName=%s",
							cfgId, id, xdsField.getTargetDSName(), name));
					}

					xdsField.setTargetDSId(id);
					xdsField.setTargetDSName(name);
				}
			}

			for (XDSParameter xdsParameter : xDataSource.getParams()) {
				if (xdsParameter.getTargetDSId() != null) {
					BigDecimal oldId;
					try {
						oldId = new BigDecimal(xdsParameter.getTargetDSId());
					} catch (Exception e) {
						continue;
					}
					String id = dsIdMap.get(oldId);
					String name = dsMap.get(id);

					if (id == null) {
						throw new RuntimeException(String.format("Invalid cfgId=%s: dsName=%s, newDsName=%s",
							cfgId, xdsParameter.getTargetDSName(), name));
					}

					if (!xdsParameter.getTargetDSName().equals(name)) {
						System.out.println(String.format("Invalid cfgId=%s: dsName=%s, newDsName=%s",
							cfgId, xdsParameter.getTargetDSName(), name));
					}

					xdsParameter.setTargetDSId(id);
					xdsParameter.setTargetDSName(name);
				}
			}

			String newXml = xstream.toXML(xDataSource);

			updateCfg
				.setParameters(ObjectBuilder.<String, Object>map()
					.put("xml", newXml)
					.put("id", cfgId)
					.get())
				.executeUpdate();
		}
	}

	private static Connection createConnection(String file) {
		ConfigUtil.load(PkXmlMigrate.class.getResourceAsStream(file));

		try {
			Class.forName(ConfigUtil.getString(true, "dmt.db.driver"));

			return DriverManager.getConnection(
				ConfigUtil.getString(true, "dmt.db.url"),
				ConfigUtil.getString(true, "dmt.db.username"),
				ConfigUtil.getString(true, "dmt.db.password"));
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
}
