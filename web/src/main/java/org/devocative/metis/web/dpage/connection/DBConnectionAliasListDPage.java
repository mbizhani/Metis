//overwrite
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
import org.devocative.metis.entity.connection.DBConnectionAlias;
import org.devocative.metis.entity.connection.EAliasMode;
import org.devocative.metis.iservice.connection.IDBConnectionAliasService;
import org.devocative.metis.vo.filter.connection.DBConnectionAliasFVO;
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
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.html.WAjaxLink;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.html.window.WModalWindow;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.util.Collections;
import java.util.List;

public class DBConnectionAliasListDPage extends DPage implements IGridDataSource<DBConnectionAlias> {
	private static final long serialVersionUID = -1660269396L;

	@Inject
	private IDBConnectionAliasService dBConnectionAliasService;

	private DBConnectionAliasFVO filter;
	private boolean formVisible = true;
	private String[] invisibleFormItems;

	private WDataGrid<DBConnectionAlias> grid;
	private String[] removeColumns;

	private Boolean gridFit;
	private boolean gridEnabled = false;
	private OSize gridHeight = OSize.fixed(500);
	private OSize gridWidth = OSize.percent(100);

	// ------------------------------

	// Panel Call - New Filter
	public DBConnectionAliasListDPage(String id) {
		this(id, Collections.<String>emptyList(), new DBConnectionAliasFVO());
	}

	// Panel Call - Open Filter
	public DBConnectionAliasListDPage(String id, DBConnectionAliasFVO filter) {
		this(id, Collections.<String>emptyList(), filter);
	}

	// REST Call - New Filter
	public DBConnectionAliasListDPage(String id, List<String> params) {
		this(id, params, new DBConnectionAliasFVO());
	}

	// Main Constructor
	private DBConnectionAliasListDPage(String id, List<String> params, DBConnectionAliasFVO filter) {
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
			private static final long serialVersionUID = -2088919825L;

			@Override
			public void onClick(AjaxRequestTarget target) {
				window.setContent(new DBConnectionAliasFormDPage(window.getContentId()));
				window.show(target);
			}
		}.setVisible(hasPermission(MetisPrivilegeKey.DBConnectionAliasAdd)));

		WFloatTable floatTable = new WFloatTable("floatTable");
		floatTable.add(new WTextInput("name")
			.setLabel(new ResourceModel("DBConnectionAlias.name", "name")));
		floatTable.add(new WSelectionInput("mode", EAliasMode.list(), true)
			.setLabel(new ResourceModel("DBConnectionAlias.mode", "mode")));
		floatTable.add(new WSelectionInput("connection", dBConnectionAliasService.getConnectionList(), true)
			.setLabel(new ResourceModel("DBConnectionAlias.connection", "connection")));
		floatTable.add(new WDateRangeInput("creationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.creationDate", "creationDate")));
		floatTable.add(new WSelectionInput("creatorUser", dBConnectionAliasService.getCreatorUserList(), true)
			.setLabel(new ResourceModel("entity.creatorUser", "creatorUser")));
		floatTable.add(new WDateRangeInput("modificationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.modificationDate", "modificationDate")));
		floatTable.add(new WSelectionInput("modifierUser", dBConnectionAliasService.getModifierUserList(), true)
			.setLabel(new ResourceModel("entity.modifierUser", "modifierUser")));

		Form<DBConnectionAliasFVO> form = new Form<>("form", new CompoundPropertyModel<>(filter));
		form.add(floatTable);
		form.add(new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
			private static final long serialVersionUID = 2100061844L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				grid.setEnabled(true);
				grid.loadData(target);
			}
		});
		add(form);

		OColumnList<DBConnectionAlias> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<>(new ResourceModel("DBConnectionAlias.name", "name"), "name"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DBConnectionAlias.mode", "mode"), "mode"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DBConnectionAlias.connection", "connection"), "connection"));
		columnList.add(new OPropertyColumn<DBConnectionAlias>(new ResourceModel("entity.creationDate", "creationDate"), "creationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.creatorUser", "creatorUser"), "creatorUser"));
		columnList.add(new OPropertyColumn<DBConnectionAlias>(new ResourceModel("entity.modificationDate", "modificationDate"), "modificationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.modifierUser", "modifierUser"), "modifierUser"));
		columnList.add(new OPropertyColumn<DBConnectionAlias>(new ResourceModel("entity.version", "version"), "version")
			.setFormatter(ONumberFormatter.integer())
			.setStyle("direction:ltr"));

		if (hasPermission(MetisPrivilegeKey.DBConnectionAliasEdit)) {
			columnList.add(new OEditAjaxColumn<DBConnectionAlias>() {
				private static final long serialVersionUID = -481294291L;

				@Override
				public void onClick(AjaxRequestTarget target, IModel<DBConnectionAlias> rowData) {
					window.setContent(new DBConnectionAliasFormDPage(window.getContentId(), rowData.getObject()));
					window.show(target);
				}
			});
		}

		OGrid<DBConnectionAlias> oGrid = new OGrid<>();
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

	public DBConnectionAliasListDPage setFormVisible(boolean formVisible) {
		this.formVisible = formVisible;
		return this;
	}

	public DBConnectionAliasListDPage setInvisibleFormItems(String... invisibleFormItems) {
		this.invisibleFormItems = invisibleFormItems;
		return this;
	}

	public DBConnectionAliasListDPage setGridHeight(OSize gridHeight) {
		this.gridHeight = gridHeight;
		return this;
	}

	public DBConnectionAliasListDPage setGridWidth(OSize gridWidth) {
		this.gridWidth = gridWidth;
		return this;
	}

	public DBConnectionAliasListDPage setGridFit(Boolean gridFit) {
		this.gridFit = gridFit;
		return this;
	}

	public DBConnectionAliasListDPage setGridEnabled(boolean gridEnabled) {
		this.gridEnabled = gridEnabled;
		return this;
	}

	public DBConnectionAliasListDPage setRemoveColumns(String... removeColumns) {
		this.removeColumns = removeColumns;
		return this;
	}

	// ------------------------------ IGridDataSource

	@Override
	public List<DBConnectionAlias> list(long pageIndex, long pageSize, List<WSortField> sortFields) {
		return dBConnectionAliasService.search(filter, pageIndex, pageSize);
	}

	@Override
	public long count() {
		return dBConnectionAliasService.count(filter);
	}

	@Override
	public IModel<DBConnectionAlias> model(DBConnectionAlias object) {
		return new WModel<>(object);
	}
}