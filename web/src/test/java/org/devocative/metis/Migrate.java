package org.devocative.metis;

import com.thoughtworks.xstream.XStream;
import org.devocative.adroit.ConfigUtil;
import org.devocative.adroit.ObjectUtil;
import org.devocative.demeter.core.DemeterCore;
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

		ConfigUtil.addKey("dmt.db.interceptor", "none", false);

		DemeterCore.get().init();

		ApplicationContext ctx = DemeterCore.get().getApplicationContext();

		persistorService = ctx.getBean(IPersistorService.class);

		List<DataSource> list = persistorService
			.createQueryBuilder()
			.addFrom(DataSource.class, "ent")
			.addWhere("and ent.id not in (select dv.dataSourceId from DataView dv)")
			.list();

		for (DataSource dataSource : list) {
			System.out.println("Loading DS =" + dataSource);

			XDataSource xDataSource = (XDataSource) dsXStream.fromXML(dataSource.getConfig().getValue());
			xDataSource.setName(dataSource.getName());

			XDataView xDataView = create(dataSource, xDataSource);

			System.out.println("Creating DV = " + xDataView);

			dataSource.getConfig().setValue(dsXStream.toXML(xDataSource));
			persistorService.saveOrUpdate(dataSource.getConfig());

			ConfigLob configLob = new ConfigLob();
			configLob.setValue(dvXStream.toXML(xDataView));
			configLob.setCreationDate(dataSource.getCreationDate());
			configLob.setCreatorUserId(dataSource.getCreatorUserId());
			configLob.setModificationDate(dataSource.getModificationDate());
			configLob.setModifierUserId(dataSource.getModifierUserId());
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

		persistorService.endSession();
		DemeterCore.get().shutdown();
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

		xdvField.setName(xdsField.getName().toLowerCase());
		//xdvField.setInFilterPanel(xdsField.getInFilterPanel());
		//xdvField.setResultType(xdsField.getResultType());

		if (xdvField.getInFilterPanel() == null) {
			xdvField.setInFilterPanel(true);
		}

		if (xdvField.getResultType() == null) {
			xdvField.setResultType(XDSFieldResultType.Shown);
		}

		/*if (xdsField.getTargetId() != null) {
			xdsField.setTargetDSId(xdsField.getTargetId());
			xdsField.setTargetDSName(persistorService.load(DataSource.class, xdsField.getTargetId()).getName());
		}*/

		if (ObjectUtil.isTrue(xdsField.getIsKeyField()) || ObjectUtil.isTrue(xdsField.getIsSelfRelPointerField())) {
			xdsField.setFilterType(XDSFieldFilterType.Equal);
		}

		if (xdsField.getFilterType() == null && xdsField.getType() != null) {
			XDSFieldFilterType filterType = null;
			switch (xdsField.getType()) {
				case String:
					filterType = XDSFieldFilterType.Contain;
					break;
				case Integer:
					filterType = XDSFieldFilterType.Range;
					break;
				case Real:
					filterType = XDSFieldFilterType.Range;
					break;
				case Date:
					filterType = XDSFieldFilterType.Range;
					break;
				case DateTime:
					filterType = XDSFieldFilterType.Range;
					break;
				case Boolean:
					filterType = XDSFieldFilterType.Equal;
					break;
				case LookUp:
					filterType = XDSFieldFilterType.Search;
					break;
				case Unknown:
					filterType = XDSFieldFilterType.Unknown;
					break;
			}
			xdsField.setFilterType(filterType);
		}

		xdsField.setName(xdsField.getName().toLowerCase());
		//xdsField.setTargetId(null);
		//xdsField.setResultType(null);
		//xdsField.setInFilterPanel(null);

		return xdvField;
	}
}
