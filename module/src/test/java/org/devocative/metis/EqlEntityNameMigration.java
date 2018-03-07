package org.devocative.metis;

import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.ConfigUtil;
import org.devocative.adroit.obuilder.ObjectBuilder;
import org.devocative.adroit.sql.NamedParameterStatement;
import org.devocative.adroit.sql.SqlHelper;
import org.devocative.adroit.sql.plugin.SchemaPlugin;
import org.devocative.adroit.xml.AdroitXStream;
import org.devocative.metis.entity.data.config.XDSQueryMode;
import org.devocative.metis.entity.data.config.XDataSource;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;

public class EqlEntityNameMigration {

	public static void main(String[] args) throws Exception {
		Map<String, String> old2new = oldToNewEntity();
		System.out.println("old2new = " + old2new);

		XStream xstream = new AdroitXStream();
		xstream.processAnnotations(XDataSource.class);

		Connection connection = createConnection("/config.properties");

		SqlHelper helper = new SqlHelper(connection);
		helper.setXMLQueryFile(PkXmlMigrate.class.getResourceAsStream("/oracle_xml_mig.xml"));

		NamedParameterStatement updateCfg = helper.createNPS("updateCfg");

		connection.setAutoCommit(false);

		Map<String, String> dsXmlEQL = helper.twoCellsAsMap("dsXmlEQL");
		for (Map.Entry<String, String> entry : dsXmlEQL.entrySet()) {
			String cfgId = entry.getKey();
			String xml = entry.getValue();

			XDataSource xDataSource = (XDataSource) xstream.fromXML(xml);

			if (XDSQueryMode.Eql.equals(xDataSource.getQuery().getMode())) {
				String query = xDataSource.getQuery().getText();

				StringBuffer tableReplacerBuffer = new StringBuffer();
				Matcher tableMatcher = SchemaPlugin.SCHEMA_PATTERN.matcher(query);

				boolean validEntity = true;

				while (tableMatcher.find()) {
					if (tableMatcher.group(6) != null && tableMatcher.group(7) != null) {
						if (tableMatcher.group(7).contains(".")) {// has schema
							throw new RuntimeException("DS has schema: " + xDataSource.getName());
						} else {
							String entity = tableMatcher.group(7);
							String newEntity = old2new.get(entity);

							if (newEntity != null) {
								String replacement = String.format("%s %s", tableMatcher.group(6), newEntity);
								tableMatcher.appendReplacement(tableReplacerBuffer, replacement);
							} else {
								//System.err.printf("Entity not found: entity=%s ds=%s cfgId=%s\n", entity, xDataSource.getName(), cfgId);
								validEntity = false;
								break;
							}
						}
					}
				}

				if (validEntity) {
					tableMatcher.appendTail(tableReplacerBuffer);
					xDataSource.getQuery().setText(tableReplacerBuffer.toString());

					String newXml = xstream.toXML(xDataSource);

					updateCfg
						.setParameters(ObjectBuilder.<String, Object>createDefaultMap()
							.put("xml", newXml)
							.put("id", cfgId)
							.get())
						.executeUpdate();

					System.out.printf("DS changed: ds=%s cfgId=%s\n", xDataSource.getName(), cfgId);
				} else {
					System.err.printf("DS has invalid entity: ds=%s cfgId=%s\n", xDataSource.getName(), cfgId);
				}
			}
		}

		connection.close();
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

	private static Map<String, String> oldToNewEntity() throws IOException {
		Map<String, String> result = new HashMap<>();
		Set<String> newEntity = new HashSet<>();

		try (BufferedReader reader = new BufferedReader(new InputStreamReader(EqlEntityNameMigration.class.getResourceAsStream("/EQL_Mapping_Name_final.txt")))) {
			reader.lines().forEach(line -> {
				String[] split = line.split("[\t]");
				if (split.length == 2) {
					if (!result.containsKey(split[0])) {
						if (!newEntity.contains(split[1])) {
							result.put(split[0], split[1]);
							newEntity.add(split[1]);
						} else {
							throw new RuntimeException("Duplicate value: " + line);
						}
					} else {
						throw new RuntimeException("Duplicate key: " + line);
					}
				} else {
					throw new RuntimeException("Invalid mapping: " + line);
				}
			});
		}

		return result;
	}
}