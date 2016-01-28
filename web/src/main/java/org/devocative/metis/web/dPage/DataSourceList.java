package org.devocative.metis.web.dPage;

import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.devocative.demeter.web.DPage;
import org.devocative.metis.entity.dataSource.DataSource;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.web.component.grid.ORESTLinkColumn;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.data.WGridDataSource;
import org.devocative.wickomp.data.WSortField;
import org.devocative.wickomp.grid.OGrid;
import org.devocative.wickomp.grid.WDataGrid;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.html.icon.FontAwesome;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.util.List;

public class DataSourceList extends DPage {

	@Inject
	private IDataSourceService dataSourceService;

	public DataSourceList(String id, List<String> params) {
		super(id, params);

		OColumnList<DataSource> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<DataSource>(new Model<>("Name"), "name"));
		columnList.add(new OPropertyColumn<DataSource>(new Model<>("Connection"), "connection"));
		columnList.add(new ORESTLinkColumn<DataSource>(new Model<String>(), DataSourceForm.class, "name",
			new FontAwesome("pencil", "green", new Model<>("Edit"))));
		columnList.add(new ORESTLinkColumn<DataSource>(new Model<String>(), DataSourceViewer.class, "name",
			new FontAwesome("cogs", "red", new Model<>("Execute"))));

		OGrid<DataSource> oGrid = new OGrid<>();
		oGrid
			.setColumns(columnList)
			.setMultiSort(false)
			.setHeight(OSize.fixed(350));

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
