package org.devocative.metis.web.dPage;

import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.devocative.demeter.web.DPage;
import org.devocative.metis.entity.dataSource.DataSourceInfo;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.data.WDataSource;
import org.devocative.wickomp.data.WSortField;
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

		OColumnList<DataSourceInfo> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<DataSourceInfo>(new Model<>("name"), "name"));
		columnList.add(new OPropertyColumn<DataSourceInfo>(new Model<>("connectionInfo"), "connectionInfo"));

		OGrid<DataSourceInfo> oGrid = new OGrid<>();
		oGrid
			.setColumns(columnList)
			.setMultiSort(false)
			.setHeight(OSize.fixed(350));

		add(new WDataGrid<>("grid", oGrid, new WDataSource<DataSourceInfo>() {
			@Override
			public List<DataSourceInfo> list(long pageIndex, long pageSize, List<WSortField> sortFields) {
				return dataSourceService.search(pageIndex, pageSize);
			}

			@Override
			public long count() {
				return dataSourceService.count();
			}

			@Override
			public IModel<DataSourceInfo> model(DataSourceInfo object) {
				return new WModel<>(object);
			}
		}));
	}
}
