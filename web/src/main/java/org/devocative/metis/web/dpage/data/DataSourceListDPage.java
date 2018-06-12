package org.devocative.metis.web.dpage.data;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.iservice.IUserService;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.demeter.web.component.grid.ORESTLinkColumn;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.EConnectionSelection;
import org.devocative.metis.iservice.data.IDataSourceService;
import org.devocative.metis.vo.filter.data.DataSourceFVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.dpage.data.form.DataViewFormDPage;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.range.WDateRangeInput;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.formatter.ONumberFormatter;
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
import java.util.Collections;
import java.util.List;

public class DataSourceListDPage extends DPage implements IGridDataSource<DataSource> {
	private static final long serialVersionUID = -9027432645938136642L;

	@Inject
	private IDataSourceService dataSourceService;

	@Inject
	private IUserService userService;

	private DataSourceFVO filter;
	private boolean formVisible = true;
	private String[] invisibleFormItems;

	private WDataGrid<DataSource> grid;
	private String[] removeColumns;

	private Boolean gridFit;
	private boolean gridEnabled = false;
	private OSize gridHeight = OSize.fixed(500);
	private OSize gridWidth = OSize.percent(100);

	// ------------------------------

	// Panel Call - New Filter
	public DataSourceListDPage(String id) {
		this(id, Collections.emptyList(), new DataSourceFVO());
	}

	// Panel Call - Open Filter
	public DataSourceListDPage(String id, DataSourceFVO filter) {
		this(id, Collections.emptyList(), filter);
	}

	// REST Call - New Filter
	public DataSourceListDPage(String id, List<String> params) {
		this(id, params, new DataSourceFVO());
	}

	// Main Constructor
	private DataSourceListDPage(String id, List<String> params, DataSourceFVO filter) {
		super(id, params);

		this.filter = filter;
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		WFloatTable floatTable = new WFloatTable("floatTable");
		floatTable.add(new WTextInput("name")
			.setLabel(new ResourceModel("DataSource.name", "name")));
		floatTable.add(new WTextInput("title")
			.setLabel(new ResourceModel("DataSource.title", "title")));
		floatTable.add(new WTextInput("keyField")
			.setLabel(new ResourceModel("DataSource.keyField", "keyField")));
		floatTable.add(new WTextInput("titleField")
			.setLabel(new ResourceModel("DataSource.titleField", "titleField")));
		floatTable.add(new WTextInput("selfRelPointerField")
			.setLabel(new ResourceModel("DataSource.selfRelPointerField", "selfRelPointerField")));
		floatTable.add(new WSelectionInput("connectionSelection", EConnectionSelection.list(), true)
			.setLabel(new ResourceModel("DataSource.connectionSelection", "connectionSelection")));
		floatTable.add(new WSelectionInput("connection", dataSourceService.getConnectionList(), true)
			.setLabel(new ResourceModel("DataSource.connection", "connection")));
		floatTable.add(new WDateRangeInput("creationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.creationDate", "creationDate")));
		floatTable.add(new WSelectionInput("creatorUser", dataSourceService.getCreatorUserList(), true)
			.setLabel(new ResourceModel("entity.creatorUser", "creatorUser")));
		floatTable.add(new WDateRangeInput("modificationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.modificationDate", "modificationDate")));
		floatTable.add(new WSelectionInput("modifierUser", dataSourceService.getModifierUserList(), true)
			.setLabel(new ResourceModel("entity.modifierUser", "modifierUser")));

		Form<DataSourceFVO> form = new Form<>("form", new CompoundPropertyModel<>(filter));
		form.add(floatTable);
		form.add(new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
			private static final long serialVersionUID = -1837376903L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				grid.setEnabled(true);
				grid.loadData(target);
			}
		});
		add(form);

		OColumnList<DataSource> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<>(new ResourceModel("DataSource.name", "name"), "name"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DataSource.title", "title"), "title"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DataSource.keyField", "keyField"), "keyField"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DataSource.titleField", "titleField"), "titleField"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DataSource.selfRelPointerField", "selfRelPointerField"), "selfRelPointerField"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DataSource.connectionSelection", "connectionSelection"), "connectionSelection"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DataSource.connection", "connection"), "connection"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("entity.creationDate", "creationDate"), "creationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.creatorUser", "creatorUser"), "creatorUser"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("entity.modificationDate", "modificationDate"), "modificationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.modifierUser", "modifierUser"), "modifierUser"));
		columnList.add(new OPropertyColumn<DataSource>(new ResourceModel("entity.version", "version"), "version")
			.setFormatter(ONumberFormatter.integer())
			.setStyle("direction:ltr"));

		columnList.add(new ORESTLinkColumn<>(new Model<>(), DataViewFormDPage.class, "name", MetisIcon.EDIT));
		columnList.add(new ORESTLinkColumn<>(new Model<>(), DataViewExecutorDPage.class, "name", MetisIcon.EXECUTE));
		columnList.add(new OColumn<DataSource>(new Model<>()) {
			private static final long serialVersionUID = 2676890534453105099L;

			@Override
			public String cellValue(DataSource bean, String id, int colNo, String url) {
				String baseUri = UrlUtil.createUri(DataViewFormDPage.class, true);
				return String.format("<a href=\"%s?dsName=%s\">%s</a>", baseUri, bean.getName(), MetisIcon.ADD);
			}
		});

		OGrid<DataSource> oGrid = new OGrid<>();
		oGrid
			.setColumns(columnList)
			.setMultiSort(false)
			.setHeight(gridHeight)
			.setWidth(gridWidth)
			.setFit(gridFit);

		grid = new WDataGrid<>("grid", oGrid, this);
		add(grid);

		// ---------------

		form.setVisible(formVisible);
		grid.setEnabled(gridEnabled || !formVisible);

		if (invisibleFormItems != null) {
			for (String formItem : invisibleFormItems) {
				floatTable.get(formItem).setVisible(false);
			}
		}

		if (removeColumns != null) {
			for (String column : removeColumns) {
				columnList.removeColumn(column);
			}
		}
	}

	// ------------------------------

	public DataSourceListDPage setFormVisible(boolean formVisible) {
		this.formVisible = formVisible;
		return this;
	}

	public DataSourceListDPage setInvisibleFormItems(String... invisibleFormItems) {
		this.invisibleFormItems = invisibleFormItems;
		return this;
	}

	public DataSourceListDPage setGridHeight(OSize gridHeight) {
		this.gridHeight = gridHeight;
		return this;
	}

	public DataSourceListDPage setGridWidth(OSize gridWidth) {
		this.gridWidth = gridWidth;
		return this;
	}

	public DataSourceListDPage setGridFit(Boolean gridFit) {
		this.gridFit = gridFit;
		return this;
	}

	public DataSourceListDPage setGridEnabled(boolean gridEnabled) {
		this.gridEnabled = gridEnabled;
		return this;
	}

	public DataSourceListDPage setRemoveColumns(String... removeColumns) {
		this.removeColumns = removeColumns;
		return this;
	}

	// ------------------------------ IGridDataSource

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
}
