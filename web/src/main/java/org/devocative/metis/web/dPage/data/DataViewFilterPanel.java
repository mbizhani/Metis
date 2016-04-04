package org.devocative.metis.web.dPage.data;

import org.apache.wicket.Component;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.Model;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.demeter.web.DFormInputPanel;
import org.devocative.metis.entity.data.config.XDSFieldFilterType;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.*;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.opt.OSize;

import javax.inject.Inject;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DataViewFilterPanel extends DFormInputPanel<Map<String, Object>> {
	private Map<String, Object> filter;

	@Inject
	private IDataService dataService;

	public DataViewFilterPanel(String id, DataVO dataVO) {
		this(id, new HashMap<String, Object>(), dataVO);
	}

	// Main Constructor
	public DataViewFilterPanel(String id, Map<String, Object> filter, DataVO dataVO) {
		super(id, new CompoundPropertyModel<>(filter));

		this.filter = filter;

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
						.setLabel(new Model<>(fieldVO.getTitle()))
						.setRequired(fieldVO.getRequired());
					view.add(fieldFormItem);
				}
				item.add(view);
			}
		});
	}

	@Override
	public void convertInput() {
		setConvertedInput(filter);
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
					fieldFormItem = new WSelectionInput(fieldVO.getName(), lookUpList, true);
				} else {
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

}
