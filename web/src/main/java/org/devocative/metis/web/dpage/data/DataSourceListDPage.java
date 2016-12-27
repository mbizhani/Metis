package org.devocative.metis.web.dpage.data;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.IUserService;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.demeter.web.component.grid.ORESTLinkColumn;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.iservice.data.IDataSourceService;
import org.devocative.metis.vo.filter.data.DataSourceFVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.dpage.data.form.DataViewFormDPage;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.range.WDateRangeInput;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.grid.IGridDataSource;
import org.devocative.wickomp.grid.OGrid;
import org.devocative.wickomp.grid.WDataGrid;
import org.devocative.wickomp.grid.WSortField;
import org.devocative.wickomp.grid.column.OColumn;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.util.List;

public class DataSourceListDPage extends DPage {
	private static final long serialVersionUID = -9027432645938136642L;

	private DataSourceFVO filter = new DataSourceFVO();

	private WDataGrid<DataSource> grid;

	@Inject
	private IDataSourceService dataSourceService;

	@Inject
	private IDBConnectionService dbConnectionService;

	@Inject
	private IUserService userService;

	public DataSourceListDPage(String id, List<String> params) {
		super(id, params);

		List<User> users = userService.list();

		Form<DataSourceFVO> form = new Form<>("form", new CompoundPropertyModel<>(filter));
		WFloatTable floatTable = new WFloatTable("floatTable");
		floatTable.setEqualWidth(true);
		floatTable.add(new WTextInput("name").setLabel(new ResourceModel("DataSource.name")));
		floatTable.add(new WTextInput("title").setLabel(new ResourceModel("DataSource.title")));
		floatTable.add(new WSelectionInput("connection", dbConnectionService.list(), true).setLabel(new ResourceModel("DataSource.connection")));

		floatTable.add(new WDateRangeInput("creationDate").setLabel(new ResourceModel("entity.creationDate")));
		floatTable.add(new WSelectionInput("creatorUser", users, true).setLabel(new ResourceModel("entity.creatorUser")));
		floatTable.add(new WDateRangeInput("modificationDate").setLabel(new ResourceModel("entity.modificationDate")));
		floatTable.add(new WSelectionInput("modifierUser", users, true).setLabel(new ResourceModel("entity.modifierUser")));
		form.add(floatTable);
		form.add(new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
			private static final long serialVersionUID = 8552310566824028749L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				grid.setEnabled(true);
				grid.loadData(target);
			}
		});
		add(form);

		OColumnList<DataSource> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("DataSource.name"), "name"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("DataSource.title"), "title"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("DataSource.connection"), "connection"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("DataSource.keyField"), "keyField"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("DataSource.titleField"), "titleField"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("DataSource.selfRelPointerField"), "selfRelPointerField"));

		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("entity.creationDate"), "creationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference()));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("entity.creatorUser"), "creatorUser"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("entity.modificationDate"), "modificationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference()));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("entity.modifierUser"), "modifierUser"));

		columnList.add(new ORESTLinkColumn<DataSource>(new Model<String>(), DataViewFormDPage.class, "name", MetisIcon.EDIT));
		columnList.add(new ORESTLinkColumn<DataSource>(new Model<String>(), DataViewExecutorDPage.class, "name", MetisIcon.EXECUTE));
		columnList.add(new OColumn<DataSource>(new Model<String>()) {
			private static final long serialVersionUID = 2676890534453105099L;

			@Override
			public String cellValue(DataSource bean, String id, int colNo, String url) {
				String baseUri = UrlUtil.createUri(DataViewFormDPage.class, true);
				return String.format("<a href=\"%s?dsName=%s\">%s</a>", baseUri, bean.getName(), MetisIcon.ADD);
			}

			@Override
			public String footerCellValue(Object bean, int colNo, String url) {
				throw new RuntimeException("Footer not supported!");
			}
		});

		OGrid<DataSource> oGrid = new OGrid<>();
		oGrid
			.setColumns(columnList)
			.setMultiSort(false)
			.setHeight(OSize.fixed(500))
			.setWidth(OSize.percent(100));

		add(grid = new WDataGrid<>("grid", oGrid, new IGridDataSource<DataSource>() {
			private static final long serialVersionUID = -7922746882157931742L;

			@Override
			public List<DataSource> list(long pageIndex, long pageSize, List<WSortField> sortFields) {
				return dataSourceService.search(filter, pageIndex, pageSize);
			}

			@Override
			public long count() {
				return dataSourceService.count(filter);
			}

			@Override
			public IModel<DataSource> model(DataSource object) {
				return new WModel<>(object);
			}
		}));
		grid.setEnabled(false);
	}
}