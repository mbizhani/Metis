package org.devocative.metis.web.dpage.data;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.Component;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.Model;
import org.devocative.adroit.date.TimeFieldVO;
import org.devocative.demeter.web.DPanel;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.iservice.IDataRenderService;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.FilterInputParamsVO;
import org.devocative.metis.vo.FilterItemVO;
import org.devocative.metis.vo.FilterItemVO.PresentationMode;
import org.devocative.wickomp.form.*;
import org.devocative.wickomp.form.range.WDateRangeInput;
import org.devocative.wickomp.form.range.WNumberRangeInput;
import org.devocative.wickomp.form.range.WTextRangeInput;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class DataViewFilterPanel2 extends DPanel {
	private static final long serialVersionUID = -8467382200091757194L;

	private final DataVO dataVO;
	private final Map<String, Object> filter;
	private FilterInputParamsVO inputParamsVO;

	private final TimeFieldVO dateDefTime;
	private final TimeFieldVO dateTimeFromDefTime;
	private final TimeFieldVO dateTimeToDefTime;

	@Inject
	private IDataRenderService dataRenderService;

	@Inject
	private IDataService dataService;

	// ------------------------------

	// Main Constructor
	public DataViewFilterPanel2(String id, DataVO dataVO, Map<String, Object> filter) {
		super(id);

		this.dataVO = dataVO;
		this.filter = filter;

		setDefaultModel(new CompoundPropertyModel<>(filter));

		dateDefTime = dataService.extractTimeFields(MetisConfigKey.FormDateDefaultTime);
		dateTimeFromDefTime = dataService.extractTimeFields(MetisConfigKey.FormDateTimeDefaultFrom);
		dateTimeToDefTime = dataService.extractTimeFields(MetisConfigKey.FormDateTimeDefaultTo);
	}

	// ------------------------------

	public DataViewFilterPanel2 setInputParamsVO(FilterInputParamsVO inputParamsVO) {
		this.inputParamsVO = inputParamsVO;
		return this;
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		WFloatTable floatTable = new WFloatTable("floatTable");
		add(floatTable);

		final List<FilterItemVO> allFilterItems = dataRenderService.createFilterItems(dataVO, inputParamsVO);

		allFilterItems.stream()
			.filter(it -> it.getDefaultValue() != null)
			.forEach(it -> filter.put(it.getName(), it.getDefaultValue()));

		final List<FilterItemVO> visibleItems = allFilterItems.stream()
			.filter(it -> it.getMode() != PresentationMode.Invisible)
			.collect(Collectors.toList());

		floatTable.add(new ListView<FilterItemVO>("fields", visibleItems) {
			private static final long serialVersionUID = -2835258374896015122L;

			@Override
			protected void populateItem(ListItem<FilterItemVO> item) {
				FilterItemVO fieldVO = item.getModelObject();

				FormComponent fieldFormItem = createFieldFormComponent(fieldVO);

				fieldFormItem.setLabel(new Model<>(fieldVO.getCaption()));
				switch (fieldVO.getMode()) {
					case Required:
						fieldFormItem.setRequired(true);
						break;

					case Disabled:
						fieldFormItem.setEnabled(false);
						break;

					case ExpectingSetData:
						switch (fieldVO.getType()) {
							case LookUpList:
							case LookUpSearch:
								fieldFormItem.setRequired(true);
								break;

							default:
								fieldFormItem.setEnabled(false);
						}
						break;
				}

				if (fieldVO.hasError()) {
					fieldFormItem
						.setRequired(false)
						.setEnabled(false)
						.add(new AttributeModifier("style", "border: 2px solid red"))
						.add(new AttributeModifier("title", fieldVO.getError()));
				}

				RepeatingView view = new RepeatingView("field");
				view.add(fieldFormItem);
				item.add(view);
			}
		});

		allFilterItems
			.stream()
			.filter(it -> it.getError() != null)
			.forEach(it -> error(it.getError()));
	}

	// ------------------------------

	private FormComponent createFieldFormComponent(final FilterItemVO fieldVO) {
		FormComponent fieldFormItem = null;

		String name = fieldVO.getName();

		switch (fieldVO.getType()) {
			case TextContain:
				fieldFormItem = new WSqlStringInput(name);
				break;
			case TextRange:
				fieldFormItem = new WTextRangeInput(name);
				break;
			case TextSimple:
				fieldFormItem = new WTextInput(name);
				break;
			case IntRange:
				fieldFormItem = new WNumberRangeInput(name, Long.class)
					.setThousandSeparator(',');
				break;
			case IntSimple:
				fieldFormItem = new WNumberInput(name, Long.class)
					.setThousandSeparator(',');
				break;
			case RealRange:
				fieldFormItem = new WNumberRangeInput(name, BigDecimal.class)
					.setThousandSeparator(',')
					.setPrecision(3);
				break;
			case RealSimple:
				fieldFormItem = new WNumberInput(name, BigDecimal.class)
					.setThousandSeparator(',')
					.setPrecision(3);
				break;
			case DateRange:
				fieldFormItem = new WDateRangeInput(name)
					.setFromDefaultTime(dateDefTime)
					.setToDefaultTime(dateDefTime)
					.setTimePartVisible(false);
				break;
			case DateSimple:
				fieldFormItem = new WDateInput(name)
					.setDefaultTime(dateDefTime)
					.setTimePartVisible(false);
				break;
			case DateTimeRange:
				fieldFormItem = new WDateRangeInput(name)
					.setFromDefaultTime(dateTimeFromDefTime)
					.setToDefaultTime(dateTimeToDefTime)
					.setTimePartVisible(true);
				break;
			case DateTimeSimple:
				fieldFormItem = new WDateInput(name)
					.setDefaultTime(dateDefTime)
					.setTimePartVisible(true);
				break;
			case Boolean:
				fieldFormItem = new WBooleanInput(name);
				break;
			case LookUpList:
				fieldFormItem = new WSelectionInput(name, fieldVO.getListValues(), fieldVO.getMultipleSelection());
				break;
			case LookUpSearch:
				fieldFormItem = new WClientSearchableListInput(name, fieldVO.getMultipleSelection()) {
					private static final long serialVersionUID = -3250464405477849335L;

					{
						getModalWindowOptions()
							.setWidth(OSize.percent(80))
							.setHeight(OSize.percent(80));

						setOpenModalLinkVisible(
							fieldVO.getMode() == PresentationMode.Default ||
								fieldVO.getMode() == PresentationMode.Required);
					}

					@Override
					protected Component createSelectionPanel(String selectionPanelId) {
						final FilterItemVO.TargetSearchConfig targetCfg = fieldVO.getTargetSearchConfig();
						return new DataViewExecutorDPage(selectionPanelId, targetCfg.getTargetId())
							.setSelectionJSCallback(getJSCallback())
							.setMultiSelect(fieldVO.getMultipleSelection())
							.addParams(targetCfg.getTargetParam())
							;
					}
				};
				break;
		}

		return fieldFormItem;
	}
}
