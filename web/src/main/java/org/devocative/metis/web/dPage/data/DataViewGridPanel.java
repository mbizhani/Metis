package org.devocative.metis.web.dPage.data;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.core.request.handler.IPartialPageRequestHandler;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.devocative.demeter.web.DPanel;
import org.devocative.metis.entity.data.config.XDSFieldResultType;
import org.devocative.metis.entity.data.config.XDVGridSelectionMode;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.async.DataViewQVO;
import org.devocative.metis.vo.async.DataViewRVO;
import org.devocative.metis.web.MetisDModule;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.WModel;
import org.devocative.wickomp.async.AsyncBehavior;
import org.devocative.wickomp.async.IAsyncResponseHandler;
import org.devocative.wickomp.formatter.OBooleanFormatter;
import org.devocative.wickomp.formatter.ODateFormatter;
import org.devocative.wickomp.formatter.ONumberFormatter;
import org.devocative.wickomp.grid.*;
import org.devocative.wickomp.grid.column.OColumn;
import org.devocative.wickomp.grid.column.OColumnList;
import org.devocative.wickomp.grid.column.OHiddenColumn;
import org.devocative.wickomp.grid.column.OPropertyColumn;
import org.devocative.wickomp.grid.toolbar.OGridGroupingButton;
import org.devocative.wickomp.grid.toolbar.OTreeGridClientButton;
import org.devocative.wickomp.opt.OSize;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DataViewGridPanel extends DPanel implements ITreeGridAsyncDataSource<Map<String, Object>>, IAsyncResponseHandler {
	private DataVO dataVO;
	private Map<String, Object> filter;
	private AsyncBehavior asyncBehavior;
	private WBaseGrid<Map<String, Object>> grid;

	public DataViewGridPanel(String id, DataVO dataVO, Map<String, Object> filter) {
		super(id);
		this.dataVO = dataVO;
		this.filter = filter;

		add(asyncBehavior = new AsyncBehavior(this));

		OColumnList<Map<String, Object>> columns = createColumns(dataVO);

		DataFieldVO selfRelPointerField = dataVO.findSelfRelPointerField();
		DataFieldVO titleField = dataVO.findTitleField();
		DataFieldVO keyField = dataVO.findKeyField();

		OBaseGrid<Map<String, Object>> oBaseGrid;
		if (selfRelPointerField == null) {
			OGrid<Map<String, Object>> gridOptions = new OGrid<>();
			gridOptions
				.setGroupStyle("background-color:#dddddd")
				.addToolbarButton(new OGridGroupingButton<Map<String, Object>>(MetisIcon.EXPAND, MetisIcon.COLLAPSE));

			oBaseGrid = gridOptions;
			add(grid = new WDataGrid<>("grid", gridOptions, this));
		} else {
			OTreeGrid<Map<String, Object>> gridOptions = new OTreeGrid<>();
			gridOptions
				.setParentIdField(selfRelPointerField.getName())
				.setTreeField(titleField != null ? titleField.getName() : null)
				.addToolbarButton(new OTreeGridClientButton<Map<String, Object>>(MetisIcon.COLLAPSE));

			oBaseGrid = gridOptions;
			add(grid = new WTreeGrid<>("grid", gridOptions, this));
		}

		grid.setEnabled(false);

		oBaseGrid
			.setColumns(columns)
			.setMultiSort(true)
			.setSelectionIndicator(true)
			.setSingleSelect(dataVO.getSelectionModeSafely() == XDVGridSelectionMode.Single)
			.setIdField(keyField != null ? keyField.getName() : null)
			.setTitleField(titleField != null ? titleField.getName() : null)
			/*.addToolbarButton(new OExportExcelButton<Map<String, Object>>(
				MetisIcon.EXPORT_EXCEL,
				String.format("%s-export.xlsx", dataVO.getName()),
				10000))*/
			.setHeight(OSize.fixed(dataVO.getGridHeightSafely().getHeight()))
			.setWidth(OSize.percent(100))
		;

		if (!getWebRequest().getRequestParameters().getParameterValue("window").isEmpty()) {
			oBaseGrid.setSelectionJSHandler("function(rows){parent.postMessage(JSON.stringify(rows),'*');}");
			//setEditButtonVisible(false);
		}
	}

	public DataViewGridPanel setSelectionJSCallback(String jsCallback) {
		grid.getOptions().setSelectionJSHandler(jsCallback);
		return this;
	}

	public void loadData(AjaxRequestTarget target) {
		grid.setEnabled(true);
		grid.loadData(target);
	}

	// IAsyncResponseHandler
	@Override
	public void onAsyncResult(String handlerId, IPartialPageRequestHandler handler, Serializable result) {
		DataViewRVO dataViewRVO = (DataViewRVO) result;

		if (MetisDModule.EXEC_DATA_VIEW.equals(handlerId)) {
			grid.pushData(handler, dataViewRVO.getList(), dataViewRVO.getCount());
		} else if (MetisDModule.EXEC_DATA_VIEW_CHILDREN.equals(handlerId)) {
			((WTreeGrid<Map<String, Object>>) grid).pushChildren(handler, dataViewRVO.getParentId(), dataViewRVO.getList());
		}
	}

	@Override
	public void onAsyncError(String handlerId, IPartialPageRequestHandler handler, Exception error) {
		grid.pushError(handler, error);
	}

	// IGridAsyncDataSource
	@Override
	public void list(long pageIndex, long pageSize, List<WSortField> sortFields) {
		DataViewQVO dataViewQVO = new DataViewQVO();
		dataViewQVO
			.setName(dataVO.getName())
			.setPageIndex(pageIndex)
			.setPageSize(pageSize)
			.setSortFieldList(getSortFieldsMap(sortFields))
			.setFilter(getFilterMap());

		asyncBehavior.sendAsyncRequest(MetisDModule.EXEC_DATA_VIEW, dataViewQVO);
	}

	// ITreeGridAsyncDataSource
	@Override
	public void listByParent(Serializable parentId, List<WSortField> sortFields) {
		DataViewQVO dataViewQVO = new DataViewQVO();
		dataViewQVO
			.setName(dataVO.getName())
			.setParentId(parentId)
			.setSortFieldList(getSortFieldsMap(sortFields));

		asyncBehavior.sendAsyncRequest(MetisDModule.EXEC_DATA_VIEW_CHILDREN, dataViewQVO);
	}

	// ITreeGridAsyncDataSource
	@Override
	public boolean hasChildren(Map<String, Object> bean) {
		return true;
	}

	// IDataSource
	@Override
	public IModel<Map<String, Object>> model(Map<String, Object> object) {
		return new WModel<>(object);
	}

	private OColumnList<Map<String, Object>> createColumns(DataVO dataVO) {
		OColumnList<Map<String, Object>> columns = new OColumnList<>();
		for (DataFieldVO fieldVO : dataVO.getFields()) {
			OColumn<Map<String, Object>> column = null;
			if (XDSFieldResultType.Shown.equals(fieldVO.getResultType())) {
				column = new OPropertyColumn<>(new Model<>(fieldVO.getTitleOrName()), fieldVO.getName());
				column.setSortable(true);
			} else if (XDSFieldResultType.Hidden.equals(fieldVO.getResultType())) {
				column = new OHiddenColumn<>(fieldVO.getName());
			}

			if (column != null) {
				columns.add(column);

				switch (fieldVO.getType()) {
					case Integer:
						column.setFormatter(ONumberFormatter.integer());
						break;
					case Real:
						column.setFormatter(ONumberFormatter.real());
						break;
					case Date:
						column.setFormatter(ODateFormatter.prDate());
						break;
					case DateTime:
						column.setFormatter(ODateFormatter.prDateTime());
						break;
					case Boolean:
						column.setFormatter(OBooleanFormatter.bool());
						break;
				}
			}
		}

		return columns;
	}

	private Map<String, Object> getFilterMap() {
		Map<String, Object> filtersCloned = new HashMap<>();
		for (Map.Entry<String, Object> entry : filter.entrySet()) {
			if (entry.getValue() != null) {
				filtersCloned.put(entry.getKey(), entry.getValue());
			}
		}
		return filtersCloned;
	}

	private Map<String, String> getSortFieldsMap(List<WSortField> sortFieldList) {
		Map<String, String> sortFieldsMap = null;
		if (sortFieldList != null && sortFieldList.size() > 0) {
			sortFieldsMap = new HashMap<>();
			for (WSortField sortField : sortFieldList) {
				sortFieldsMap.put(sortField.getField(), sortField.getOrder());
			}
		}
		return sortFieldsMap;
	}
}
