package org.devocative.metis;

import com.thoughtworks.xstream.XStream;
import org.devocative.demeter.core.ModuleLoader;
import org.devocative.demeter.iservice.persistor.IPersistorService;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.entity.data.config.*;
import org.springframework.context.ApplicationContext;

import java.util.List;

public class Migrate {
	static IPersistorService persistorService;

	public static void main(String[] args) {
		XStream dsXStream = new XStream();
		dsXStream.processAnnotations(XDataSource.class);

		XStream dvXStream = new XStream();
		dvXStream.processAnnotations(XDataView.class);


		ModuleLoader.init();

		ApplicationContext ctx = ModuleLoader.getApplicationContext();

		persistorService = ctx.getBean(IPersistorService.class);

		List<DataSource> list = persistorService
			.createQueryBuilder()
			.addFrom(DataSource.class, "ent")
			.addWhere("and ent.id not in (select dv.dataSourceId from DataView dv)")
			.list();

		for (DataSource dataSource : list) {
			System.out.println(dataSource);

			XDataSource xDataSource = (XDataSource) dsXStream.fromXML(dataSource.getConfig().getValue());
			xDataSource.setName(dataSource.getName());

			XDataView xDataView = create(dataSource, xDataSource);

			System.out.println(xDataView);

			dataSource.getConfig().setValue(dsXStream.toXML(xDataSource));
			persistorService.saveOrUpdate(dataSource.getConfig());

			ConfigLob configLob = new ConfigLob();
			configLob.setValue(dvXStream.toXML(xDataView));
			persistorService.saveOrUpdate(configLob);

			DataView dataView = new DataView();
			dataView.setName(dataSource.getName());
			dataView.setTitle(dataSource.getTitle());
			dataView.setConfig(configLob);
			dataView.setDataSource(dataSource);
			dataView.setCreationDate(dataSource.getCreationDate());
			dataView.setModificationDate(dataSource.getModificationDate());
			dataView.setCreatorUserId(dataSource.getCreatorUserId());
			dataView.setModifierUserId(dataSource.getModifierUserId());

			persistorService.saveOrUpdate(dataView);
		}

		persistorService.commitOrRollback();
	}

	private static XDataView create(DataSource dataSource, XDataSource xDataSource) {
		XDataView xDataView = new XDataView();

		xDataView.setName(dataSource.getName());
		xDataView.setDataSourceId(dataSource.getId());
		xDataView.setDataSourceName(dataSource.getName());

		xDataView.setGridHeight(XDVGridHeight.Short);
		xDataView.setSelectionMode(XDVGridSelectionMode.Multiple);

		for (XDSField xdsField : xDataSource.getFields()) {
			XDVField xdvField = createField(xdsField);
			xDataView.getFields().add(xdvField);
		}

		return xDataView;
	}

	private static XDVField createField(XDSField xdsField) {
		XDVField xdvField = new XDVField();

		xdvField.setName(xdsField.getName());
		xdvField.setInFilterPanel(xdsField.getInFilterPanel());
		xdvField.setResultType(xdsField.getResultType());

		if (xdsField.getTargetId() != null) {
			xdsField.setTargetDSId(xdsField.getTargetId());
			xdsField.setTargetDSName(persistorService.get(DataSource.class, xdsField.getTargetId()).getName());
		}

		xdsField.setTargetId(null);
		xdsField.setResultType(null);
		xdsField.setInFilterPanel(null);

		return xdvField;
	}
}
