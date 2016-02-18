package org.devocative.metis.web.dPage;

import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.component.grid.ORESTLinkColumn;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.data.WGridDataSource;
import org.devocative.wickomp.data.WSortField;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.grid.OGrid;
import org.devocative.wickomp.grid.WDataGrid;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.util.List;

public class DataSourceList extends DPage {

	@Inject
	private IDataSourceService dataSourceService;

	public DataSourceList(String id, List<String> params) {
		super(id, params);

		OColumnList<DataSource> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("DataSource.name", "Name"), "name"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("DataSource.title", "Title"), "title"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("DataSource.connection", "DB Connection"), "connection"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("DataSource.keyField", "DB Connection"), "keyField"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("DataSource.titleField", "DB Connection"), "titleField"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("DataSource.selfRelPointerField", "DB Connection"), "selfRelPointerField"));

		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("entity.creationDate", "Creation Date"), "creationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference()));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("entity.creatorUser", "Creator User"), "creatorUser"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("entity.modificationDate", "Modification Date"), "modificationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference()));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("entity.modifierUser", "Modifier User"), "modifierUser"));

		columnList.add(new ORESTLinkColumn<DataSource>(new Model<String>(), DataSourceForm.class, "name", MetisIcon.EDIT));
		columnList.add(new ORESTLinkColumn<DataSource>(new Model<String>(), DataSourceExecutor.class, "name", MetisIcon.EXECUTE));

		OGrid<DataSource> oGrid = new OGrid<>();
		oGrid
			.setColumns(columnList)
			.setMultiSort(false)
			.setHeight(OSize.fixed(350))
			.setWidth(OSize.percent(100));

		add(new WDataGrid<>("grid", oGrid, new WGridDataSource<DataSource>() {
			@Override
			public List<DataSource> list(long pageIndex, long pageSize, List<WSortField> sortFields) {
				return dataSourceService.search(pageIndex, pageSize);
			}

			@Override
			public long count() {
				return dataSourceService.count();
			}

			@Override
			public IModel<DataSource> model(DataSource object) {
				return new WModel<>(object);
			}
		}));
	}
}
