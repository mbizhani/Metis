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
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.wickomp.form.*;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class DataViewFilterPanel extends DPanel {
	private static final long serialVersionUID = -8467382200091757194L;

	private Map<String, Object> filter;
	//private boolean disableFilledFilter;
	private Long dataSourceId;

	private String sentDBConnection;

	@Inject
	private IDataService dataService;

	@Inject
	private IDataSourceService dataSourceService;

	// Main Constructor
	public DataViewFilterPanel(String id, final Long dataSourceId, final Map<String, Object> filter, List<DataAbstractFieldVO> fields) {
		super(id);
		this.dataSourceId = dataSourceId;

		setDefaultModel(new CompoundPropertyModel<>(filter));

		this.filter = filter;

		/*disableFilledFilter = getWebRequest()
			.getQueryParameters()
			.getParameterValue("search")
			.toBoolean(true);*/

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
						fieldFormItem.setRequired(fieldVO.getRequiredSafely() || filter.containsKey(fieldVO.getName()));
					} else {
						fieldFormItem.setEnabled(/*!disableFilledFilter || */!filter.containsKey(fieldVO.getName()));
					}

					view.add(fieldFormItem);
				}
				item.add(view);
			}
		});

		if (ConfigUtil.hasKey(MetisConfigKey.DBConnParamName)) {
			sentDBConnection = getWebRequest()
				.getRequestParameters()
				.getParameterValue(ConfigUtil.getString(MetisConfigKey.DBConnParamName))
				.toOptionalString();
		}
	}

	private FormComponent createFieldFormComponent(final DataAbstractFieldVO fieldVO) {
		FormComponent fieldFormItem = null;

		switch (fieldVO.getType()) {

			case String:
				if (XDSFieldFilterType.Contain == fieldVO.getFilterType()) {
					fieldFormItem = new WSqlStringInput(fieldVO.getName());
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
				if (fieldVO.getFilterType() == XDSFieldFilterType.List) {
					List<KeyValueVO<Serializable, String>> lookUpList;
					if (filter.containsKey(fieldVO.getName())) {
						lookUpList = (List<KeyValueVO<Serializable, String>>) filter.get(fieldVO.getName());
					} else {
						lookUpList = dataSourceService.executeLookUp(
							dataSourceId,
							fieldVO.getTargetDSId(),
							sentDBConnection,
							null
						);
					}

					fieldFormItem = new WSelectionInput(fieldVO.getName(), lookUpList, true);
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
								.setMultiSelect(true);
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
		}
		return fieldFormItem;
	}

	private FormComponent createParamFormComponent(DataParameterVO parameterVO) {
		FormComponent fieldFormItem = null;
		switch (parameterVO.getType()) {

			case String:
				fieldFormItem = new WTextInput(parameterVO.getName());
				break;

			case Integer:
				fieldFormItem = new WNumberInput(parameterVO.getName(), Long.class)
					.setThousandSeparator(',');
				break;

			case Real:
				fieldFormItem = new WNumberInput(parameterVO.getName(), BigDecimal.class)
					.setPrecision(2)
					.setThousandSeparator(',')
					.setPrecision(3);
				break;

			case Date:
			case DateTime:
				fieldFormItem = new WDateInput(parameterVO.getName())
					.setTimePartVisible(XDSFieldType.DateTime == parameterVO.getType());

				break;

			case Boolean:
				fieldFormItem = new WBooleanInput(parameterVO.getName());
				break;

			case LookUp:
				break;
		}
		return fieldFormItem;
	}
}
