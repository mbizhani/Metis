//overwrite
package org.devocative.metis.web.dpage.data;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.demeter.web.component.grid.OEditAjaxColumn;
import org.devocative.metis.MetisPrivilegeKey;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.iservice.data.IDataGroupService;
import org.devocative.metis.vo.filter.data.DataGroupFVO;
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

public class DataGroupListDPage extends DPage implements IGridDataSource<DataGroup> {
	private static final long serialVersionUID = 20641183L;

	@Inject
	private IDataGroupService dataGroupService;

	private DataGroupFVO filter;
	private boolean formVisible = true;
	private String[] invisibleFormItems;

	private WDataGrid<DataGroup> grid;
	private String[] removeColumns;

	private Boolean gridFit;
	private boolean gridEnabled = false;
	private OSize gridHeight = OSize.fixed(500);
	private OSize gridWidth = OSize.percent(100);

	// ------------------------------

	// Panel Call - New Filter
	public DataGroupListDPage(String id) {
		this(id, Collections.<String>emptyList(), new DataGroupFVO());
	}

	// Panel Call - Open Filter
	public DataGroupListDPage(String id, DataGroupFVO filter) {
		this(id, Collections.<String>emptyList(), filter);
	}

	// REST Call - New Filter
	public DataGroupListDPage(String id, List<String> params) {
		this(id, params, new DataGroupFVO());
	}

	// Main Constructor
	private DataGroupListDPage(String id, List<String> params, DataGroupFVO filter) {
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
			private static final long serialVersionUID = -703466974L;

			@Override
			public void onClick(AjaxRequestTarget target) {
				window.setContent(new DataGroupFormDPage(window.getContentId()));
				window.show(target);
			}
		}.setVisible(hasPermission(MetisPrivilegeKey.DataGroupAdd)));

		WFloatTable floatTable = new WFloatTable("floatTable");
		floatTable.add(new WTextInput("name")
			.setLabel(new ResourceModel("DataGroup.name", "name")));
		floatTable.add(new WTextInput("code")
			.setLabel(new ResourceModel("DataGroup.code", "code")));
		floatTable.add(new WDateRangeInput("creationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.creationDate", "creationDate")));
		floatTable.add(new WSelectionInput("creatorUser", dataGroupService.getCreatorUserList(), true)
			.setLabel(new ResourceModel("entity.creatorUser", "creatorUser")));
		floatTable.add(new WDateRangeInput("modificationDate")
			.setTimePartVisible(true)
			.setLabel(new ResourceModel("entity.modificationDate", "modificationDate")));
		floatTable.add(new WSelectionInput("modifierUser", dataGroupService.getModifierUserList(), true)
			.setLabel(new ResourceModel("entity.modifierUser", "modifierUser")));

		Form<DataGroupFVO> form = new Form<>("form", new CompoundPropertyModel<>(filter));
		form.add(floatTable);
		form.add(new DAjaxButton("search", new ResourceModel("label.search"), MetisIcon.SEARCH) {
			private static final long serialVersionUID = 2080389895L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				grid.setEnabled(true);
				grid.loadData(target);
			}
		});
		add(form);

		OColumnList<DataGroup> columnList = new OColumnList<>();
		columnList.add(new OPropertyColumn<>(new ResourceModel("DataGroup.name", "name"), "name"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("DataGroup.code", "code"), "code"));
		columnList.add(new OPropertyColumn<DataGroup>(new ResourceModel("entity.creationDate", "creationDate"), "creationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.creatorUser", "creatorUser"), "creatorUser"));
		columnList.add(new OPropertyColumn<DataGroup>(new ResourceModel("entity.modificationDate", "modificationDate"), "modificationDate")
			.setFormatter(ODateFormatter.getDateTimeByUserPreference())
			.setStyle("direction:ltr"));
		columnList.add(new OPropertyColumn<>(new ResourceModel("entity.modifierUser", "modifierUser"), "modifierUser"));
		columnList.add(new OPropertyColumn<DataGroup>(new ResourceModel("entity.version", "version"), "version")
			.setFormatter(ONumberFormatter.integer())
			.setStyle("direction:ltr"));

		if (hasPermission(MetisPrivilegeKey.DataGroupEdit)) {
			columnList.add(new OEditAjaxColumn<DataGroup>() {
				private static final long serialVersionUID = -227644640L;

				@Override
				public void onClick(AjaxRequestTarget target, IModel<DataGroup> rowData) {
					window.setContent(new DataGroupFormDPage(window.getContentId(), rowData.getObject()));
					window.show(target);
				}
			});
		}

		OGrid<DataGroup> oGrid = new OGrid<>();
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

	public DataGroupListDPage setFormVisible(boolean formVisible) {
		this.formVisible = formVisible;
		return this;
	}

	public DataGroupListDPage setInvisibleFormItems(String... invisibleFormItems) {
		this.invisibleFormItems = invisibleFormItems;
		return this;
	}

	public DataGroupListDPage setGridHeight(OSize gridHeight) {
		this.gridHeight = gridHeight;
		return this;
	}

	public DataGroupListDPage setGridWidth(OSize gridWidth) {
		this.gridWidth = gridWidth;
		return this;
	}

	public DataGroupListDPage setGridFit(Boolean gridFit) {
		this.gridFit = gridFit;
		return this;
	}

	public DataGroupListDPage setGridEnabled(boolean gridEnabled) {
		this.gridEnabled = gridEnabled;
		return this;
	}

	public DataGroupListDPage setRemoveColumns(String... removeColumns) {
		this.removeColumns = removeColumns;
		return this;
	}

	// ------------------------------ IGridDataSource

	@Override
	public List<DataGroup> list(long pageIndex, long pageSize, List<WSortField> sortFields) {
		return dataGroupService.search(filter, pageIndex, pageSize);
	}

	@Override
	public long count() {
		return dataGroupService.count(filter);
	}

	@Override
	public IModel<DataGroup> model(DataGroup object) {
		return new WModel<>(object);
	}
}