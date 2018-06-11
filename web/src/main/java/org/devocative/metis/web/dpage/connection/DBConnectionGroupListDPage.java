package org.devocative.metis.web.dpage.connection;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.demeter.web.component.grid.OEditAjaxColumn;
import org.devocative.metis.MetisPrivilegeKey;
import org.devocative.metis.entity.connection.DBConnectionGroup;
import org.devocative.metis.iservice.connection.IDBConnectionGroupService;
import org.devocative.metis.vo.filter.connection.DBConnectionGroupFVO;
import org.devocative.metis.web.MetisIcon;
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
import org.devocative.wickomp.html.WAjaxLink;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.html.window.WModalWindow;
import org.devocative.wickomp.opt.OHorizontalAlign;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.util.Collections;
import java.util.List;

public class DBConnectionGroupListDPage extends DPage implements IGridDataSource<DBConnectionGroup> {
	private static final long serialVersionUID = 1920643421L;

	@Inject
	private IDBConnectionGroupService dBConnectionGroupService;

	private DBConnectionGroupFVO filter;
	private boolean formVisible = true;
	private String[] invisibleFormItems;

	private WDataGrid<DBConnectionGroup> grid;
	private String[] removeColumns;

	private Boolean gridFit;
	private boolean gridEnabled = false;
	private OSize gridHeight = OSize.fixed(500);
	private OSize gridWidth = OSize.percent(100);

	// ------------------------------

	// Panel Call - New Filter
	public DBConnectionGroupListDPage(String id) {
		this(id, Collections.<String>emptyList(), new DBConnectionGroupFVO());
	}

	// Panel Call - Open Filter
	public DBConnectionGroupListDPage(String id, DBConnectionGroupFVO filter) {
		this(id, Collections.<String>emptyList(), filter);
	}

	// REST Call - New Filter
	public DBConnectionGroupListDPage(String id, List<String> params) {
		this(id, params, new DBConnectionGroupFVO());
	}

	// Main Constructor
	private DBConnectionGroupListDPage(String id, List<String> params, DBConnectionGroupFVO filter) {
		super(id, params);

		this.filter = filter;
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		final WModalWindow window = new WModalWindow("window");
		add(window);

		add(new WAjaxLink("add", MetisIcon.ADD) {
			private static final long serialVersionUID = -1797220256L;

			@Override
			public void onClick(AjaxRequestTarget target) {
				window.setContent(new DBConnectionGroupFormDPage(window.getContentId()));
				window.show(target);
			}
		}.setVisible(hasPermission(MetisPrivilegeKey.DBConnectionGroupAdd)));

		WFloatTable floatTable = new WFloatTable("floatTable");
		floatTable.add(new WTextInput("name")
			.setLabel(new ResourceModel("DBConnectionGroup.name")));
		floatTable.add(new WTextInput("driver")
			.setLabel(new ResourceModel("DBConnectionGroup.driver")));
		floatTable.add(new WTextInput("url")
			.setLabel(new ResourceModel("DBConnectionGroup.url")));
		/*floatTable.add(new WTextInput("testQuery")
			.setLabel(new ResourceModel("DBConnectionGroup.testQuery")));*/
		floatTable.add(new WDateRangeInput("creationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.creationDate")));
		floatTable.add(new WSelectionInput("creatorUser", dBConnectionGroupService.getCreatorUserList(), true)
			.setLabel(new ResourceModel("entity.creatorUser")));
		floatTable.add(new WDateRangeInput("modificationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.modificationDate")));
		floatTable.add(new WSelectionInput("modifierUser", dBConnectionGroupService.getModifierUserList(), true)
			.setLabel(new ResourceModel("entity.modifierUser")));

		Form<DBConnectionGroupFVO> form = new Form<>("form", new CompoundPropertyModel<>(filter));
		form.add(floatTable);
		form.add(new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
			private static final long serialVersionUID = -1044493883L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				grid.setEnabled(true);
				grid.loadData(target);
			}
		});
		add(form);

		OColumnList<DBConnectionGroup> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<>(new ResourceModel("DBConnectionGroup.name"), "name"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DBConnectionGroup.driver"), "driver"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DBConnectionGroup.url"), "url"));
		columnList.add(new OColumn<DBConnectionGroup>(new ResourceModel("DBConnectionGroup.testQuery")) {
			private static final long serialVersionUID = -6779049575847536279L;

			@Override
			public String cellValue(DBConnectionGroup bean, String id, int colNo, String url) {
				return bean.getTestQuery() != null ? MetisIcon.TRUE.toString() : MetisIcon.FALSE.toString();
			}
		}.setAlign(OHorizontalAlign.Center));
		columnList.add(new OColumn<DBConnectionGroup>(new ResourceModel("DBConnectionGroup.config")) {
			private static final long serialVersionUID = 7514001254778483999L;

			@Override
			public String cellValue(DBConnectionGroup bean, String id, int colNo, String url) {
				return bean.getConfigId() != null ? MetisIcon.TRUE.toString() : MetisIcon.FALSE.toString();
			}
		}.setAlign(OHorizontalAlign.Center));
		columnList.add(new OPropertyColumn<DBConnectionGroup>(new ResourceModel("entity.creationDate"), "creationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.creatorUser"), "creatorUser"));
		columnList.add(new OPropertyColumn<DBConnectionGroup>(new ResourceModel("entity.modificationDate"), "modificationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.modifierUser"), "modifierUser"));
		columnList.add(new OPropertyColumn<DBConnectionGroup>(new ResourceModel("entity.version"), "version")
			.setFormatter(ONumberFormatter.integer())
			.setStyle("direction:ltr"));

		if (hasPermission(MetisPrivilegeKey.DBConnectionGroupEdit)) {
			columnList.add(new OEditAjaxColumn<DBConnectionGroup>() {
				private static final long serialVersionUID = -118822178L;

				@Override
				public void onClick(AjaxRequestTarget target, IModel<DBConnectionGroup> rowData) {
					window.setContent(new DBConnectionGroupFormDPage(window.getContentId(), rowData.getObject()));
					window.show(target);
				}
			});
		}

		OGrid<DBConnectionGroup> oGrid = new OGrid<>();
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

	public DBConnectionGroupListDPage setFormVisible(boolean formVisible) {
		this.formVisible = formVisible;
		return this;
	}

	public DBConnectionGroupListDPage setInvisibleFormItems(String... invisibleFormItems) {
		this.invisibleFormItems = invisibleFormItems;
		return this;
	}

	public DBConnectionGroupListDPage setGridHeight(OSize gridHeight) {
		this.gridHeight = gridHeight;
		return this;
	}

	public DBConnectionGroupListDPage setGridWidth(OSize gridWidth) {
		this.gridWidth = gridWidth;
		return this;
	}

	public DBConnectionGroupListDPage setGridFit(Boolean gridFit) {
		this.gridFit = gridFit;
		return this;
	}

	public DBConnectionGroupListDPage setGridEnabled(boolean gridEnabled) {
		this.gridEnabled = gridEnabled;
		return this;
	}

	public DBConnectionGroupListDPage setRemoveColumns(String... removeColumns) {
		this.removeColumns = removeColumns;
		return this;
	}

	// ------------------------------ IGridDataSource

	@Override
	public List<DBConnectionGroup> list(long pageIndex, long pageSize, List<WSortField> sortFields) {
		return dBConnectionGroupService.search(filter, pageIndex, pageSize);
	}

	@Override
	public long count() {
		return dBConnectionGroupService.count(filter);
	}

	@Override
	public IModel<DBConnectionGroup> model(DBConnectionGroup object) {
		return new WModel<>(object);
	}
}