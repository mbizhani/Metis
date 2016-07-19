package org.devocative.metis.web.dPage.data;

import org.apache.wicket.Component;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.IRequestParameters;
import org.apache.wicket.util.string.StringValue;
import org.devocative.adroit.CalendarUtil;
import org.devocative.adroit.ConfigUtil;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.adroit.vo.RangeVO;
import org.devocative.demeter.web.DPanel;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.entity.data.config.XDSFieldFilterType;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.wickomp.form.*;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.*;

public class DataViewFilterPanel extends DPanel {
	private Map<String, Object> filter;
	//private boolean disableFilledFilter;
	private Long dataSourceId;

	private String sentDBConnection;

	@Inject
	private IDataService dataService;

	@Inject
	private IDataSourceService dataSourceService;

	// Main Constructor
	public DataViewFilterPanel(String id, final Long dataSourceId, final Map<String, Object> filter, List<DataFieldVO> fields, List<DataParameterVO> params) {
		super(id);
		this.dataSourceId = dataSourceId;

		setDefaultModel(new CompoundPropertyModel<>(filter));

		this.filter = filter;

		fillFilterMapByRequestParams(fields, params);

		/*disableFilledFilter = getWebRequest()
			.getQueryParameters()
			.getParameterValue("search")
			.toBoolean(true);*/

		WFloatTable floatTable = new WFloatTable("floatTable");
		floatTable.setEqualWidth(true);
		add(floatTable);

		floatTable.add(new ListView<DataParameterVO>("params", params) {
			@Override
			protected void populateItem(ListItem<DataParameterVO> item) {
				DataParameterVO parameterVO = item.getModelObject();

				FormComponent paramFormItem = createParamFormComponent(parameterVO);

				RepeatingView view = new RepeatingView("param");
				if (paramFormItem != null) {
					paramFormItem
						.setLabel(new Model<>(parameterVO.getTitleOrName()))
						.setRequired(parameterVO.getRequiredSafely())
						.setEnabled(/*!disableFilledFilter || */!filter.containsKey(parameterVO.getName()));
					view.add(paramFormItem);
				}
				item.add(view);
			}
		});

		floatTable.add(new ListView<DataFieldVO>("fields", dataService.findFilteringFields(fields)) {
			@Override
			protected void populateItem(ListItem<DataFieldVO> item) {
				DataFieldVO fieldVO = item.getModelObject();

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

	private FormComponent createFieldFormComponent(final DataFieldVO fieldVO) {
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
					List<KeyValueVO<Serializable, String>> lookUpList = dataSourceService.executeLookUp(dataSourceId, fieldVO.getTargetDSId(), sentDBConnection);
					if (filter.containsKey(fieldVO.getName())) {
						List<KeyValueVO<Serializable, String>> onlySentOnes = new ArrayList<>();
						List<String> sentKeys = (List<String>) filter.get(fieldVO.getName());
						for (String sentKey : sentKeys) {
							for (KeyValueVO<Serializable, String> vo : lookUpList) {
								if (sentKey.equals(vo.getKey().toString())) {
									onlySentOnes.add(vo);
									break;
								}
							}
						}
						lookUpList = onlySentOnes;
						filter.put(fieldVO.getName(), onlySentOnes);
					}
					fieldFormItem = new WSelectionInput(fieldVO.getName(), lookUpList, true);
				} else {
					//TODO if filter.containsKey(fieldVO.getName()), only show those, disable/enable popup based on disableFilledFilter
					fieldFormItem = new WClientSearchableListInput<KeyValueVO<Serializable, String>>(fieldVO.getName()) {
						{
							getModalWindowOptions().setWidth(OSize.percent(80));
						}

						@Override
						protected Component createSelectionPanel(String selectionPanelId) {
							String targetDSName = dataSourceService.load(fieldVO.getTargetDSId()).getName();
							return new DataViewExecutorDPage(
								selectionPanelId,
								Collections.singletonList(targetDSName))
								.setSelectionJSCallback(getJSCallback());
						}

						@Override
						protected KeyValueVO<Serializable, String> createServerObject(String key) {
							return new KeyValueVO<Serializable, String>(key, null);
						}
					};
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

	private void fillFilterMapByRequestParams(List<DataFieldVO> allFields, List<DataParameterVO> params) {
		IRequestParameters queryParameters = getWebRequest().getQueryParameters();
		Set<String> parameterNames = queryParameters.getParameterNames();

		for (DataFieldVO fieldVO : allFields) {
			String fieldName = fieldVO.getName();
			if (parameterNames.contains(fieldName)) {
				switch (fieldVO.getFilterType()) {
					case Equal:
					case Contain:
						String paramValue = queryParameters.getParameterValue(fieldName).toString();
						filter.put(fieldName, convertQueryParam(fieldVO.getType(), paramValue));
						break;

					case List:
					case Search:
						List<StringValue> values = queryParameters.getParameterValues(fieldName);
						List<String> list = new ArrayList<>();
						for (StringValue value : values) {
							list.add(value.toString());
						}
						filter.put(fieldName, list);
						break;
				}
			} else if (parameterNames.contains(fieldName + "_u") || parameterNames.contains(fieldName + "_l")) {
				if (fieldVO.getFilterType().equals(XDSFieldFilterType.Range)) {
					Serializable lower = convertQueryParam(fieldVO.getType(), queryParameters.getParameterValue(fieldName + "_l").toOptionalString());
					Serializable upper = convertQueryParam(fieldVO.getType(), queryParameters.getParameterValue(fieldName + "_u").toOptionalString());
					RangeVO rangeVO = new RangeVO(lower, upper);
					filter.put(fieldName, rangeVO);
				}
			}
		}

		for (DataParameterVO parameterVO : params) {
			String paramName = parameterVO.getName();
			if (parameterNames.contains(paramName)) {
				String paramValue = queryParameters.getParameterValue(paramName).toString();
				filter.put(paramName, convertQueryParam(parameterVO.getType(), paramValue));
			}
		}
	}

	private Serializable convertQueryParam(XDSFieldType fieldType, String value) {
		Serializable result = null;

		if (value != null) {
			switch (fieldType) {
				case String:
					result = value;
					break;

				case Integer:
					result = Long.valueOf(value);
					break;

				case Real:
					result = new BigDecimal(value);
					break;

				case Date:
					result = CalendarUtil.toGregorian(value, "yyyyMMdd");
					break;

				case DateTime:
					result = CalendarUtil.toGregorian(value, "yyyyMMddHHmmss");
					break;

				case Boolean:
					result = Boolean.valueOf(value);
					break;

				case LookUp:
					break;
			}
		}

		return result;
	}
}
