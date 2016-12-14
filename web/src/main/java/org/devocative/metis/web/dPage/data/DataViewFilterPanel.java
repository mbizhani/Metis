package org.devocative.metis.web.dPage.data;

import org.apache.wicket.Component;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.Model;
import org.devocative.adroit.ConfigUtil;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.demeter.web.DPanel;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.entity.data.config.XDSFieldFilterType;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.web.MetisWebParam;
import org.devocative.wickomp.WDefaults;
import org.devocative.wickomp.WebUtil;
import org.devocative.wickomp.form.*;
import org.devocative.wickomp.form.range.WDateRangeInput;
import org.devocative.wickomp.form.range.WNumberRangeInput;
import org.devocative.wickomp.form.range.WTextRangeInput;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.opt.OSize;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.*;

public class DataViewFilterPanel extends DPanel {
	private static final long serialVersionUID = -8467382200091757194L;

	private static final Logger logger = LoggerFactory.getLogger(DataViewFilterPanel.class);

	private Map<String, Object> filter;
	private Long dataSourceId;
	private String sentDBConnection;
	private List<DataAbstractFieldVO> fields;
	private Map<String, List<String>> webParams;
	private List<String> disabledFilterInputs;
	private List<String> invisibleFilterInputs;

	@Inject
	private IDataService dataService;

	@Inject
	private IDataSourceService dataSourceService;

	// ------------------------------

	// Main Constructor
	public DataViewFilterPanel(String id, final Long dataSourceId, final Map<String, Object> filter, List<DataAbstractFieldVO> fields) {
		super(id);

		this.dataSourceId = dataSourceId;
		this.filter = filter;
		this.fields = fields;

		setDefaultModel(new CompoundPropertyModel<>(filter));
	}

	// ------------------------------

	public DataViewFilterPanel setSentDBConnection(String sentDBConnection) {
		this.sentDBConnection = sentDBConnection;
		return this;
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		if (ConfigUtil.hasKey(MetisConfigKey.IgnoreParameterValues)) {
			List<String> ignoredValues = Arrays.asList(ConfigUtil.getString(MetisConfigKey.IgnoreParameterValues).split("[,]"));
			webParams = WebUtil.toMap(true, true, ignoredValues);
		} else {
			webParams = WebUtil.toMap(true, true);
		}

		try {
			filter.putAll(dataService.convertSimpleParamsToFilter(dataSourceId, fields, webParams, sentDBConnection));
		} catch (Exception e) {
			logger.error("DataViewFilterPanel -> convertSimpleParamsToFilter()", e);

			error(WDefaults.getExceptionToMessageHandler().handleMessage(this, e));
		}

		disabledFilterInputs = webParams.get(MetisWebParam.DISABLED_FILTER_INPUT) != null ?
			webParams.get(MetisWebParam.DISABLED_FILTER_INPUT) :
			Collections.<String>emptyList();
		invisibleFilterInputs = webParams.get(MetisWebParam.INVISIBLE_FILTER_INPUT) != null ?
			webParams.get(MetisWebParam.INVISIBLE_FILTER_INPUT) :
			Collections.<String>emptyList();

		WFloatTable floatTable = new WFloatTable("floatTable");
		floatTable.setEqualWidth(true);
		add(floatTable);

		floatTable.add(new ListView<DataAbstractFieldVO>("fields", dataService.findFilteringFields(fields)) {
			private static final long serialVersionUID = -2835258374896015122L;

			@Override
			protected void populateItem(ListItem<DataAbstractFieldVO> item) {
				DataAbstractFieldVO fieldVO = item.getModelObject();

				FormComponent fieldFormItem = createFieldFormComponent(fieldVO);

				RepeatingView view = new RepeatingView("field");
				if (fieldFormItem != null) {
					fieldFormItem
						.setLabel(new Model<>(fieldVO.getTitleOrName()))
						.setRequired(fieldVO.getRequiredSafely());

					if (fieldVO.getType().equals(XDSFieldType.LookUp)) {
						fieldFormItem.setRequired(fieldVO.getRequiredSafely() || filter.containsKey(fieldVO.getName().toLowerCase()));
					} else if (
						filter.containsKey(fieldVO.getName().toLowerCase()) ||
							disabledFilterInputs.contains(fieldVO.getName().toLowerCase())) {
						fieldFormItem.setEnabled(false);
					}

					view.add(fieldFormItem);
				}
				item.add(view);
				item.setVisible(!invisibleFilterInputs.contains(fieldVO.getName().toLowerCase()));
			}
		});
	}

	// ------------------------------

	private FormComponent createFieldFormComponent(final DataAbstractFieldVO fieldVO) {
		FormComponent fieldFormItem = null;

		switch (fieldVO.getType()) {

			case String:
				if (XDSFieldFilterType.Contain == fieldVO.getFilterType()) {
					fieldFormItem = new WSqlStringInput(fieldVO.getName());
				} else if (XDSFieldFilterType.Range == fieldVO.getFilterType()) {
					fieldFormItem = new WTextRangeInput(fieldVO.getName());
				} else {
					fieldFormItem = new WTextInput(fieldVO.getName());
				}
				break;

			case Integer:
				if (XDSFieldFilterType.Range == fieldVO.getFilterType()) {
					fieldFormItem = new WNumberRangeInput(fieldVO.getName(), Long.class)
						.setThousandSeparator(',');
				} else {
					fieldFormItem = new WNumberInput(fieldVO.getName(), Long.class)
						.setThousandSeparator(',');
				}
				break;

			case Real:
				if (XDSFieldFilterType.Range == fieldVO.getFilterType()) {
					fieldFormItem = new WNumberRangeInput(fieldVO.getName(), BigDecimal.class)
						.setPrecision(2)
						.setThousandSeparator(',')
						.setPrecision(3);
				} else {
					fieldFormItem = new WNumberInput(fieldVO.getName(), BigDecimal.class)
						.setPrecision(2)
						.setThousandSeparator(',')
						.setPrecision(3);
				}
				break;

			case Date:
			case DateTime:
				if (XDSFieldFilterType.Range == fieldVO.getFilterType()) {
					fieldFormItem = new WDateRangeInput(fieldVO.getName())
						.setTimePartVisible(XDSFieldType.DateTime == fieldVO.getType());
				} else {
					fieldFormItem = new WDateInput(fieldVO.getName())
						.setTimePartVisible(XDSFieldType.DateTime == fieldVO.getType());
				}
				break;

			case Boolean:
				fieldFormItem = new WBooleanInput(fieldVO.getName());
				break;

			case LookUp:
				final boolean multiple = fieldVO.getTargetDSMultipleSelection() == null || fieldVO.getTargetDSMultipleSelection();

				if (fieldVO.getFilterType() == XDSFieldFilterType.List) {
					List<KeyValueVO<Serializable, String>> lookUpList = null;

					if (filter.containsKey(fieldVO.getName())) {
						/**
						 * NOTE:
						 * Since DataService.convertSimpleParamsToFilter() is called in the beginning of onInitialize(),
						 * so the sent of params from URL or method or targetDSFilter is calculated and added
						 * correctly to the filter!
						 */
						if (filter.get(fieldVO.getName()) instanceof List) {
							lookUpList = (List<KeyValueVO<Serializable, String>>) filter.get(fieldVO.getName());
						} else {
							KeyValueVO<Serializable, String> keyValueVO = (KeyValueVO<Serializable, String>) filter.get(fieldVO.getName());
							lookUpList = Collections.singletonList(keyValueVO);
						}

						/*
						if the lookup is filtered by passing targetDSFilter, the result should not be selected
						*/
						if (fieldVO.getTargetDSFilter() != null && !webParams.containsKey(fieldVO.getName().toLowerCase())) {
							filter.remove(fieldVO.getName());
						}
					}

					try {
						if (lookUpList == null) {
							lookUpList = dataSourceService.executeLookUp(
								dataSourceId,
								fieldVO.getTargetDSId(),
								sentDBConnection,
								new HashMap<String, Object>()
							).getResult();
						}
					} catch (Exception e) {
						logger.error("DataViewFilterPanel -> createFieldFormComponent() for lookUp", e);

						String err = WDefaults.getExceptionToMessageHandler().handleMessage(this, e);
						error(String.format("%s: %s", fieldVO.getTitle(), err));

						lookUpList = new ArrayList<>();
						lookUpList.add(new KeyValueVO<Serializable, String>("?", "-- Err: " + err));
					}

					fieldFormItem = new WSelectionInput(fieldVO.getName(), lookUpList, multiple);
				} else {
					fieldFormItem = new WClientSearchableListInput<KeyValueVO<String, String>>(fieldVO.getName()) {
						private static final long serialVersionUID = 9122156586999811309L;

						{
							getModalWindowOptions().setWidth(OSize.percent(80));
						}

						@Override
						protected Component createSelectionPanel(String selectionPanelId) {
							String targetDSName = dataSourceService.load(fieldVO.getTargetDSId()).getName();
							return new DataViewExecutorDPage(
								selectionPanelId,
								Collections.singletonList(targetDSName))
								.setSelectionJSCallback(getJSCallback())
								.setMultiSelect(multiple);
						}

						@Override
						protected KeyValueVO<String, String> createServerObject(String key) {
							return new KeyValueVO<>(key, null);
						}

						@Override
						protected List<KeyValueVO<String, String>> createClientOptions(List<KeyValueVO<String, String>> list) {
							return list;
						}
					}.setOpenModalLinkVisible(!filter.containsKey(fieldVO.getName()));
				}
				break;
			case Unknown:
				break;
		}
		return fieldFormItem;
	}
}
