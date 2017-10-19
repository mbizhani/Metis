package org.devocative.metis.web.dpage.data;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.demeter.web.component.grid.ORESTLinkColumn;
import org.devocative.metis.MetisPrivilegeKey;
import org.devocative.metis.entity.data.DataView;
import org.devocative.metis.iservice.data.IDataViewService;
import org.devocative.metis.vo.filter.data.DataViewFVO;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.dpage.data.form.DataViewFormDPage;
import org.devocative.metis.web.panel.ExportImportPanel;
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

public class DataViewListDPage extends DPage implements IGridDataSource<DataView> {
	private static final long serialVersionUID = -262282617L;

	@Inject
	private IDataViewService dataViewService;

	private DataViewFVO filter;
	private boolean formVisible = true;
	private String[] invisibleFormItems;

	private WDataGrid<DataView> grid;
	private String[] removeColumns;

	private Boolean gridFit;
	private boolean gridEnabled = false;
	private OSize gridHeight = OSize.fixed(500);
	private OSize gridWidth = OSize.percent(100);

	private WModalWindow modalWindow;

	// ------------------------------

	// Panel Call - New Filter
	public DataViewListDPage(String id) {
		this(id, Collections.<String>emptyList(), new DataViewFVO());
	}

	// Panel Call - Open Filter
	public DataViewListDPage(String id, DataViewFVO filter) {
		this(id, Collections.<String>emptyList(), filter);
	}

	// REST Call - New Filter
	public DataViewListDPage(String id, List<String> params) {
		this(id, params, new DataViewFVO());
	}

	// Main Constructor
	private DataViewListDPage(String id, List<String> params, DataViewFVO filter) {
		super(id, params);

		this.filter = filter;
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		modalWindow = new WModalWindow("modalWindow");
		add(modalWindow);

		WFloatTable floatTable = new WFloatTable("floatTable");
		//floatTable.setEqualWidth(true);
		floatTable.add(new WTextInput("name")
			.setLabel(new ResourceModel("DataView.name")));
		floatTable.add(new WTextInput("title")
			.setLabel(new ResourceModel("DataView.title")));
		floatTable.add(new WSelectionInput("dataSource", dataViewService.getDataSourceList(), true)
			.setLabel(new ResourceModel("DataView.dataSource")));
		floatTable.add(new WSelectionInput("groups", dataViewService.getGroupsList(), true)
			.setLabel(new ResourceModel("DataView.groups")));
		floatTable.add(new WDateRangeInput("creationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.creationDate")));
		floatTable.add(new WSelectionInput("creatorUser", dataViewService.getCreatorUserList(), true)
			.setLabel(new ResourceModel("entity.creatorUser")));
		floatTable.add(new WDateRangeInput("modificationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.modificationDate")));
		floatTable.add(new WSelectionInput("modifierUser", dataViewService.getModifierUserList(), true)
			.setLabel(new ResourceModel("entity.modifierUser")));

		Form<DataViewFVO> form = new Form<>("form", new CompoundPropertyModel<>(filter));
		form.add(floatTable);
		form.add(new DAjaxButton("search", new ResourceModel("label.search")) {
			private static final long serialVersionUID = -1193341969L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				grid.setEnabled(true);
				grid.loadData(target);
			}
		});
		add(form);

		OColumnList<DataView> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<>(new ResourceModel("DataView.name"), "name"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DataView.title"), "title"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DataView.dataSource"), "dataSource"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DataView.groups"), "groups"));
		columnList.add(new OPropertyColumn<DataView>(new ResourceModel("entity.creationDate"), "creationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.creatorUser"), "creatorUser"));
		columnList.add(new OPropertyColumn<DataView>(new ResourceModel("entity.modificationDate"), "modificationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.modifierUser"), "modifierUser"));
		columnList.add(new OPropertyColumn<DataView>(new ResourceModel("entity.version"), "version")
			.setFormatter(ONumberFormatter.integer())
			.setStyle("direction:ltr"));

		if (hasPermission(DataViewFormDPage.class)) {
			columnList.add(new ORESTLinkColumn<>(new Model<>(), DataViewFormDPage.class, "name", MetisIcon.EDIT));
		}
		columnList.add(new ORESTLinkColumn<>(new Model<>(), DataViewExecutorDPage.class, "name", MetisIcon.EXECUTE));

		OGrid<DataView> oGrid = new OGrid<>();
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

		// ---------------

		add(new WAjaxLink("exportImport", MetisIcon.EXPORT_IMPORT) {
			private static final long serialVersionUID = -1334810219991176112L;

			@Override
			public void onClick(AjaxRequestTarget target) {
				modalWindow.setContent(new ExportImportPanel(modalWindow.getContentId()));
				modalWindow.show(target);
			}
		}.setVisible(hasPermission(MetisPrivilegeKey.DataViewExportImport)));
	}

	// ------------------------------

	public DataViewListDPage setFormVisible(boolean formVisible) {
		this.formVisible = formVisible;
		return this;
	}

	public DataViewListDPage setInvisibleFormItems(String... invisibleFormItems) {
		this.invisibleFormItems = invisibleFormItems;
		return this;
	}

	public DataViewListDPage setGridHeight(OSize gridHeight) {
		this.gridHeight = gridHeight;
		return this;
	}

	public DataViewListDPage setGridWidth(OSize gridWidth) {
		this.gridWidth = gridWidth;
		return this;
	}

	public DataViewListDPage setGridFit(Boolean gridFit) {
		this.gridFit = gridFit;
		return this;
	}

	public DataViewListDPage setGridEnabled(boolean gridEnabled) {
		this.gridEnabled = gridEnabled;
		return this;
	}

	public DataViewListDPage setRemoveColumns(String... removeColumns) {
		this.removeColumns = removeColumns;
		return this;
	}

	// ------------------------------ IGridDataSource

	@Override
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