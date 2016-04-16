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
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.adroit.vo.RangeVO;
import org.devocative.demeter.web.DPanel;
import org.devocative.metis.entity.data.config.XDSFieldFilterType;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.*;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class DataViewFilterPanel extends DPanel {
	private Map<String, Object> filter;
	private boolean disableFilledFilter;

	@Inject
	private IDataService dataService;

	// Main Constructor
	public DataViewFilterPanel(String id, final Map<String, Object> filter, DataVO dataVO) {
		super(id);
		setDefaultModel(new CompoundPropertyModel<>(filter));

		this.filter = filter;

		fillFilterMapByRequestParams(dataVO);

		disableFilledFilter = getWebRequest()
			.getQueryParameters()
			.getParameterValue("search")
			.toBoolean(true);

		WFloatTable floatTable = new WFloatTable("floatTable");
		floatTable.setEqualWidth(true);
		add(floatTable);

		floatTable.add(new ListView<DataAbstractFieldVO>("fields", dataService.findFilteringFields(dataVO)) {
			@Override
			protected void populateItem(ListItem<DataAbstractFieldVO> item) {
				DataAbstractFieldVO fieldVO = item.getModelObject();

				FormComponent fieldFormItem = createFormField(fieldVO);

				RepeatingView view = new RepeatingView("field");
				if (fieldFormItem != null) {
					fieldFormItem
						.setLabel(new Model<>(fieldVO.getTitleOrName()))
						.setRequired(fieldVO.getRequiredSafely());

					if (!fieldVO.getType().equals(XDSFieldType.LookUp)) {
						fieldFormItem.setEnabled(!disableFilledFilter || !filter.containsKey(fieldVO.getName()));
					}

					view.add(fieldFormItem);
				}
				item.add(view);
			}
		});
	}

	private FormComponent createFormField(final DataAbstractFieldVO fieldVO) {
		FormComponent fieldFormItem = null;

		switch (fieldVO.getType()) {

			case String:
				fieldFormItem = new WTextInput(fieldVO.getName());
				break;

			case Integer:
				if (XDSFieldFilterType.Range == fieldVO.getFilterType()) {
					fieldFormItem = new WNumberRangeInput(fieldVO.getName(), Long.class)
						.setThousandSeparator(",");
				} else {
					fieldFormItem = new WNumberInput(fieldVO.getName(), Long.class)
						.setThousandSeparator(",");
				}
				break;

			case Real:
				if (XDSFieldFilterType.Range == fieldVO.getFilterType()) {
					fieldFormItem = new WNumberRangeInput(fieldVO.getName(), BigDecimal.class)
						.setPrecision(2)
						.setThousandSeparator(",")
						.setPrecision(3);
				} else {
					fieldFormItem = new WNumberInput(fieldVO.getName(), BigDecimal.class)
						.setPrecision(2)
						.setThousandSeparator(",")
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
					List<KeyValueVO<Serializable, String>> lookUpList = null; //TODO
					if (filter.containsKey(fieldVO.getName())) {
						List<String> keys = (List<String>) filter.get(fieldVO.getName());
						List<KeyValueVO<Serializable, String>> onlySentOnes = new ArrayList<>();
						for (KeyValueVO<Serializable, String> keyValueVO : lookUpList) {
							if (keys.contains(keyValueVO.getKey().toString())) {
								onlySentOnes.add(keyValueVO);
							}
						}
						lookUpList = onlySentOnes;
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
							return null; //TODO
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

	private void fillFilterMapByRequestParams(DataVO dataVO) {
		IRequestParameters queryParameters = getWebRequest().getQueryParameters();
		Set<String> parameterNames = queryParameters.getParameterNames();

		for (DataFieldVO fieldVO : dataVO.getFields()) {
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
