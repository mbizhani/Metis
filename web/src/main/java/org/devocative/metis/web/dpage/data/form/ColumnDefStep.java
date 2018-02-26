package org.devocative.metis.web.dpage.data.form;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WSelectionInputAjaxUpdatingBehavior;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;
import org.devocative.wickomp.html.WMessager;

import java.util.Arrays;

class ColumnDefStep extends WWizardStepPanel {
	private static final long serialVersionUID = 5055227586735243534L;

	private DataVO dataVO;

	public ColumnDefStep(DataVO dataVO) {
		this.dataVO = dataVO;
	}

	@Override
	protected void onInit() {
		setEnabled(dataVO.isDataSourceEditable());


		add(new ListView<DataFieldVO>("fields", dataVO.getFields()) {
			private static final long serialVersionUID = 6095184285857491645L;

			@Override
			protected void populateItem(ListItem<DataFieldVO> item) {
				DataFieldVO fieldVO = item.getModelObject();
				boolean enb = !XDSFieldType.Unknown.equals(fieldVO.getType());

				final WSelectionInput type, filterType;

				item.add(new Label("name", fieldVO.getName()));
				item.add(new Label("dbType", fieldVO.getDbType()));
				item.add(new WTextInput("title", new PropertyModel<>(fieldVO, "title"))
					.setLabelVisible(false));
				item.add(type = new WSelectionInput("type", new PropertyModel<String>(fieldVO, "type"),
					Arrays.asList(XDSFieldType.values()), false));
				item.add(new CheckBox("required", new PropertyModel<>(fieldVO, "required"))
					.setEnabled(enb));
				item.add(filterType = new WSelectionInput("filterType", new PropertyModel<String>(fieldVO, "filterType"),
					Arrays.asList(fieldVO.getType().getFieldProperFilterTypes()), false));
				item.add(new CheckBox("isKeyField", new PropertyModel<>(fieldVO, "isKeyField"))
					.setEnabled(enb)
					.add(new AttributeModifier("group", "isKeyField")));
				item.add(new CheckBox("isTitleField", new PropertyModel<>(fieldVO, "isTitleField"))
					.setEnabled(enb)
					.add(new AttributeModifier("group", "isTitleField")));
				item.add(new CheckBox("isSelfRelPointerField", new PropertyModel<>(fieldVO, "isSelfRelPointerField"))
					.setEnabled(enb)
					.add(new AttributeModifier("group", "isSelfRelField")));

				type.addToChoices(new WSelectionInputAjaxUpdatingBehavior() {
					private static final long serialVersionUID = -8045527399676785760L;

					@Override
					protected void onUpdate(AjaxRequestTarget target) {
						XDSFieldType type = (XDSFieldType) getComponent().getDefaultModelObject();
						filterType.updateChoices(target, Arrays.asList(type.getFieldProperFilterTypes()));
					}
				});

				type
					.setLabelVisible(false)
					.setRequired(true)
					.setLabel(new Model<>(getString("XDSField.type") + " " + fieldVO.getName()))
					.setEnabled(enb);
				filterType
					.setLabelVisible(false)
					.setLabel(new Model<>(getString("XDSField.filterType") + " " + fieldVO.getName()))
					.setRequired(true)
					.setOutputMarkupId(true)
					.setEnabled(enb);
			}
		});
	}

	@Override
	public boolean onStepSubmit(AjaxRequestTarget target) {
		boolean hasKeyField = false;
		boolean hasTitleField = false;
		boolean hasParentField = false;

		for (DataFieldVO fieldVO : dataVO.getFields()) {
			if (fieldVO.getIsKeyFieldSafely()) {
				hasKeyField = true;
			}

			if (fieldVO.getIsTitleFieldSafely()) {
				hasTitleField = true;
			}

			if (fieldVO.getIsSelfRelPointerFieldSafely()) {
				hasParentField = true;
			}
		}

		if (hasParentField) {
			if (!hasKeyField || !hasTitleField) {
				WMessager.show(
					getString("label.error", null, "Error"),
					getString("XDSField.err.defineKeyAndTitle", null, "Key field and Title field are mandatory for tree presentation"),
					target);
				return false;
			}
		}
		return true;
	}
}
