package org.devocative.metis.web.dpage.connection;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.demeter.web.component.grid.OEditAjaxColumn;
import org.devocative.demeter.web.model.DEntityLazyLoadModel;
import org.devocative.metis.MetisPrivilegeKey;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.vo.filter.connection.DBConnectionFVO;
import org.devocative.metis.web.MetisIcon;
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
import org.devocative.wickomp.grid.column.link.OAjaxLinkColumn;
import org.devocative.wickomp.html.WAjaxLink;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.html.WMessager;
import org.devocative.wickomp.html.window.WModalWindow;
import org.devocative.wickomp.opt.OHorizontalAlign;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.util.Collections;
import java.util.List;

public class DBConnectionListDPage extends DPage implements IGridDataSource<DBConnection> {
	private static final long serialVersionUID = -612033362L;

	@Inject
	private IDBConnectionService dBConnectionService;

	private DBConnectionFVO filter;
	private boolean formVisible = true;
	private String[] invisibleFormItems;

	private WDataGrid<DBConnection> grid;
	private String[] removeColumns;

	private Boolean gridFit;
	private boolean gridEnabled = false;
	private OSize gridHeight = OSize.fixed(500);
	private OSize gridWidth = OSize.percent(100);

	private Label userDefaultConn;
	private WAjaxLink removeDefaultConn;

	// ------------------------------

	// Panel Call - New Filter
	public DBConnectionListDPage(String id) {
		this(id, Collections.<String>emptyList(), new DBConnectionFVO());
	}

	// Panel Call - Open Filter
	public DBConnectionListDPage(String id, DBConnectionFVO filter) {
		this(id, Collections.<String>emptyList(), filter);
	}

	// REST Call - New Filter
	public DBConnectionListDPage(String id, List<String> params) {
		this(id, params, new DBConnectionFVO());
	}

	// Main Constructor
	private DBConnectionListDPage(String id, List<String> params, DBConnectionFVO filter) {
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
			private static final long serialVersionUID = 1820185201L;

			@Override
			public void onClick(AjaxRequestTarget target) {
				window.setContent(new DBConnectionFormDPage(window.getContentId()));
				window.show(target);
			}
		}.setVisible(hasPermission(MetisPrivilegeKey.DBConnectionAdd)));

		DBConnection defaultConn = dBConnectionService.getDefaultConnectionOfCurrentUser();
		userDefaultConn = new Label("userDefaultConn", defaultConn != null ? defaultConn.getName() : "-");
		userDefaultConn.setOutputMarkupId(true);
		add(userDefaultConn);

		removeDefaultConn = new WAjaxLink("removeDefaultConn", new Model<>(""), MetisIcon.REMOVE) {
			private static final long serialVersionUID = 4008256506453353775L;

			@Override
			public void onClick(AjaxRequestTarget target) {
				dBConnectionService.removeDefaultConnectionOfCurrentUser();
				removeDefaultConn.setEnabled(false);
				target.add(removeDefaultConn);
				userDefaultConn.setDefaultModelObject("-");
				target.add(userDefaultConn);
			}
		};
		removeDefaultConn.setEnabled(defaultConn != null);
		add(removeDefaultConn);

		WFloatTable floatTable = new WFloatTable("floatTable");
		floatTable.add(new WTextInput("name")
			.setLabel(new ResourceModel("DBConnection.name")));
		floatTable.add(new WTextInput("driver")
			.setLabel(new ResourceModel("DBConnection.driver")));
		floatTable.add(new WTextInput("url")
			.setLabel(new ResourceModel("DBConnection.url")));
		floatTable.add(new WTextInput("username")
			.setLabel(new ResourceModel("DBConnection.username")));
		floatTable.add(new WTextInput("schema")
			.setLabel(new ResourceModel("DBConnection.schema")));
		floatTable.add(new WSelectionInput("group", dBConnectionService.getGroupList(), true)
			.setLabel(new ResourceModel("DBConnection.group")));
		floatTable.add(new WTextInput("customParam1")
			.setLabel(new ResourceModel("DBConnection.customParam1")));
		floatTable.add(new WDateRangeInput("creationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.creationDate")));
		floatTable.add(new WSelectionInput("creatorUser", dBConnectionService.getCreatorUserList(), true)
			.setLabel(new ResourceModel("entity.creatorUser")));
		floatTable.add(new WDateRangeInput("modificationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.modificationDate")));
		floatTable.add(new WSelectionInput("modifierUser", dBConnectionService.getModifierUserList(), true)
			.setLabel(new ResourceModel("entity.modifierUser")));

		Form<DBConnectionFVO> form = new Form<>("form", new CompoundPropertyModel<>(filter));
		form.add(floatTable);
		form.add(new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
			private static final long serialVersionUID = 653607830L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				grid.setEnabled(true);
				grid.loadData(target);
			}
		});
		form.add(new WAjaxLink("closeAll") {
			@Override
			public void onClick(AjaxRequestTarget target) {
				dBConnectionService.closeAllPools();
			}
		}.setConfirmationMessage(new ResourceModel("label.confirm"))
			.setVisible(hasPermission(MetisPrivilegeKey.DBConnectionCloseAll)));
		add(form);

		OColumnList<DBConnection> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<>(new ResourceModel("DBConnection.name"), "name"));
		columnList.add(new OColumn<DBConnection>(new Model<>("Pool")) {
			@Override
			public String cellValue(DBConnection bean, String id, int colNo, String url) {
				return dBConnectionService.getPoolInfo(bean.getId());
			}
		}.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("DBConnection.url"), "safeUrl")
			.setWidth(OSize.fixed(180)));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DBConnection.username"), "username"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DBConnection.schema"), "schema"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DBConnection.group"), "group"));

		columnList.add(new OColumn<DBConnection>(new ResourceModel("DBConnection.testQuery")) {
			private static final long serialVersionUID = 1282233385153044702L;

			@Override
			public String cellValue(DBConnection bean, String id, int colNo, String url) {
				return bean.getSafeTestQuery() != null ? MetisIcon.TRUE.toString() : MetisIcon.FALSE.toString();
			}
		}.setAlign(OHorizontalAlign.Center));
		columnList.add(new OColumn<DBConnection>(new ResourceModel("DBConnection.config")) {
			private static final long serialVersionUID = 1389013353865170084L;

			@Override
			public String cellValue(DBConnection bean, String id, int colNo, String url) {
				return bean.getSafeConfigId() != null ? MetisIcon.TRUE.toString() : MetisIcon.FALSE.toString();
			}
		}.setAlign(OHorizontalAlign.Center));

		columnList.add(new OPropertyColumn<>(new ResourceModel("DBConnection.customParam1"), "customParam1"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("DBConnection.driver"), "safeDriver")
			.setWidth(OSize.fixed(130)));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("entity.creationDate"), "creationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.creatorUser"), "creatorUser"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("entity.modificationDate"), "modificationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.modifierUser"), "modifierUser"));
		columnList.add(new OPropertyColumn<DBConnection>(new ResourceModel("entity.version"), "version")
			.setFormatter(ONumberFormatter.integer())
			.setStyle("direction:ltr"));

		if (hasPermission(MetisPrivilegeKey.DBConnectionEdit)) {
			columnList.add(new OEditAjaxColumn<DBConnection>() {
				private static final long serialVersionUID = -1820421073L;

				@Override
				public void onClick(AjaxRequestTarget target, IModel<DBConnection> rowData) {
					window.setContent(new DBConnectionFormDPage(window.getContentId(), rowData.getObject()));
					window.show(target);
				}
			});
		}

		columnList.add(new OAjaxLinkColumn<DBConnection>(new Model<>(), MetisIcon.CHECK_CONNECTION) {
			private static final long serialVersionUID = 2470902620673067344L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<DBConnection> rowData) {
				boolean b = dBConnectionService.checkConnection(rowData.getObject().getId());
				String msg = b ? getString("label.true") : getString("label.false");
				WMessager.show(" ", msg, target);
			}
		});
		columnList.add(new OAjaxLinkColumn<DBConnection>(new Model<>(), MetisIcon.DEFAULT_CONNECTION) {
			private static final long serialVersionUID = 4438846282287451624L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<DBConnection> rowData) {
				DBConnection dbConn = rowData.getObject();
				dBConnectionService.setDefaultConnectionForCurrentUser(dbConn.getId());

				userDefaultConn.setDefaultModel(new Model<>(dbConn.getName()));
				target.add(userDefaultConn);

				removeDefaultConn.setEnabled(true);
				target.add(removeDefaultConn);
			}
		});

		OGrid<DBConnection> oGrid = new OGrid<>();
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

	public DBConnectionListDPage setFormVisible(boolean formVisible) {
		this.formVisible = formVisible;
		return this;
	}

	public DBConnectionListDPage setInvisibleFormItems(String... invisibleFormItems) {
		this.invisibleFormItems = invisibleFormItems;
		return this;
	}

	public DBConnectionListDPage setGridHeight(OSize gridHeight) {
		this.gridHeight = gridHeight;
		return this;
	}

	public DBConnectionListDPage setGridWidth(OSize gridWidth) {
		this.gridWidth = gridWidth;
		return this;
	}

	public DBConnectionListDPage setGridFit(Boolean gridFit) {
		this.gridFit = gridFit;
		return this;
	}

	public DBConnectionListDPage setGridEnabled(boolean gridEnabled) {
		this.gridEnabled = gridEnabled;
		return this;
	}

	public DBConnectionListDPage setRemoveColumns(String... removeColumns) {
		this.removeColumns = removeColumns;
		return this;
	}

	// ------------------------------ IGridDataSource

	@Override
	public List<DBConnection> list(long pageIndex, long pageSize, List<WSortField> sortFields) {
		return dBConnectionService.search(filter, pageIndex, pageSize);
	}

	@Override
	public long count() {
		return dBConnectionService.count(filter);
	}

	@Override
	public IModel<DBConnection> model(DBConnection object) {
		return new DEntityLazyLoadModel<>(object.getId(), dBConnectionService);
	}
}