package org.devocative.metis.web.dPage.data;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.IUserService;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.demeter.web.component.grid.ORESTLinkColumn;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.iservice.data.IDataSourceService;
import org.devocative.metis.iservice.data.IDataViewService;
import org.devocative.metis.vo.filter.data.DataViewFVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.dPage.data.form.DataViewFormDPage;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.range.WDateRangeInput;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.grid.IGridDataSource;
import org.devocative.wickomp.grid.OGrid;
import org.devocative.wickomp.grid.WDataGrid;
import org.devocative.wickomp.grid.WSortField;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.util.List;

public class DataViewListDPage extends DPage implements IGridDataSource<DataView> {
	private static final long serialVersionUID = 642936567605429259L;

	private DataViewFVO filter = new DataViewFVO();

	private WDataGrid<DataView> grid;

	@Inject
	private IDataViewService dataViewService;

	@Inject
	private IDataSourceService dataSourceService;

	@Inject
	private IUserService userService;

	public DataViewListDPage(String id, List<String> params) {
		super(id, params);

		List<User> users = userService.list();

		Form<DataViewFVO> form = new Form<>("form", new CompoundPropertyModel<>(filter));
		WFloatTable floatTable = new WFloatTable("floatTable");
		floatTable.setEqualWidth(true);
		floatTable.add(new WTextInput("name").setLabel(new ResourceModel("DataView.name")));
		floatTable.add(new WTextInput("title").setLabel(new ResourceModel("DataView.title")));
		floatTable.add(new WSelectionInput("dataSource", dataSourceService.list(), true)
			.setLabel(new ResourceModel("DataView.dataSource")));

		floatTable.add(new WDateRangeInput("creationDate").setLabel(new ResourceModel("entity.creationDate")));
		floatTable.add(new WSelectionInput("creatorUser", users, true).setLabel(new ResourceModel("entity.creatorUser")));
		floatTable.add(new WDateRangeInput("modificationDate").setLabel(new ResourceModel("entity.modificationDate")));
		floatTable.add(new WSelectionInput("modifierUser", users, true).setLabel(new ResourceModel("entity.modifierUser")));
		form.add(floatTable);
		form.add(new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
			private static final long serialVersionUID = 8018122257048239845L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				grid.setEnabled(true);
				grid.loadData(target);
			}
		});
		add(form);

		OColumnList<DataView> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<DataView>(new ResourceModel("DataView.name"), "name"));
		columnList.add(new OPropertyColumn<DataView>(new ResourceModel("DataView.title"), "title"));
		columnList.add(new OPropertyColumn<DataView>(new ResourceModel("DataView.dataSource"), "dataSource"));

		columnList.add(new OPropertyColumn<DataView>(new ResourceModel("entity.creationDate"), "creationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference()));
		columnList.add(new OPropertyColumn<DataView>(new ResourceModel("entity.creatorUser"), "creatorUser"));
		columnList.add(new OPropertyColumn<DataView>(new ResourceModel("entity.modificationDate"), "modificationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference()));
		columnList.add(new OPropertyColumn<DataView>(new ResourceModel("entity.modifierUser"), "modifierUser"));

		columnList.add(new ORESTLinkColumn<DataView>(new Model<String>(), DataViewFormDPage.class, "name", MetisIcon.EDIT));
		columnList.add(new ORESTLinkColumn<DataView>(new Model<String>(), DataViewExecutorDPage.class, "name", MetisIcon.EXECUTE));

		OGrid<DataView> oGrid = new OGrid<>();
		oGrid
			.setColumns(columnList)
			.setMultiSort(false)
			.setHeight(OSize.fixed(500))
			.setWidth(OSize.percent(100));

		add(grid = new WDataGrid<>("grid", oGrid, this));
		grid.setEnabled(false);

	}

	public List<DataView> list(long pageIndex, long pageSize, List<WSortField> sortFields) {
		return dataViewService.search(filter, pageIndex, pageSize);
	}

	@Override
	public long count() {
		return dataViewService.count(filter);
	}

	@Override
	public IModel<DataView> model(DataView object) {
		return new WModel<>(object);
	}
}
