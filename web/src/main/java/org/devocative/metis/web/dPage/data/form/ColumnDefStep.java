package org.devocative.metis.web.dPage.data.form;

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

import java.util.Arrays;

class ColumnDefStep extends WWizardStepPanel {
	private DataVO dataVO;
	private boolean newDataSource;

	public ColumnDefStep(DataVO dataVO, boolean newDataSource) {
		this.dataVO = dataVO;
		this.newDataSource = newDataSource;
	}

	@Override
	protected void onInit() {
		setEnabled(newDataSource);

		add(new ListView<DataFieldVO>("fields", dataVO.getFields()) {
			@Override
			protected void populateItem(ListItem<DataFieldVO> item) {
				DataFieldVO fieldVO = item.getModelObject();


				final WSelectionInput type, filterType;
				//final CheckBox required;

				item.add(new Label("name", fieldVO.getName()));
				item.add(new Label("dbType", fieldVO.getDbType()));
				item.add(new Label("dbSize", fieldVO.getDbSize()));
				item.add(new WTextInput("title", new PropertyModel<String>(fieldVO, "title")).setLabelVisible(false));
				item.add(type = new WSelectionInput("type", new PropertyModel<String>(fieldVO, "type"), Arrays.asList(XDSFieldType.values()), false));

				item.add(new CheckBox("required", new PropertyModel<Boolean>(fieldVO, "required")));
				item.add(filterType = new WSelectionInput("filterType", new PropertyModel<String>(fieldVO, "filterType"), Arrays.asList(fieldVO.getType().getProperFilterTypes()), false));
				/*item.add(new AjaxCheckBox("inFilterPanel", new PropertyModel<Boolean>(fieldVO, "inFilterPanel")) {
					@Override
					protected void onUpdate(AjaxRequestTarget target) {
						IModel<?> defaultModel = getDefaultModel();
						Boolean bool = (Boolean) defaultModel.getObject();
						if (bool == null) {
							bool = false;
						}
						target.add(required.setVisible(bool));
						target.add(filterType.setVisible(bool));
					}
				});*/

				/*item.add(new WSelectionInput("resultType", new PropertyModel<String>(fieldVO, "resultType"), Arrays.asList(XDSFieldResultType.values()), false)
					.setLabelVisible(false)
					.setRequired(true)
					.setLabel(new Model<>(getString("XDSField.resultType") + " " + fieldVO.getName())));*/
				item.add(new CheckBox("isKeyField", new PropertyModel<Boolean>(fieldVO, "isKeyField"))
					.add(new AttributeModifier("group", "isKeyField")));
				item.add(new CheckBox("isTitleField", new PropertyModel<Boolean>(fieldVO, "isTitleField"))
					.add(new AttributeModifier("group", "isTitleField")));
				item.add(new CheckBox("isSelfRelPointerField", new PropertyModel<Boolean>(fieldVO, "isSelfRelPointerField"))
					.add(new AttributeModifier("group", "isSelfRelField")));

				type.addToChoices(new WSelectionInputAjaxUpdatingBehavior() {
					@Override
					protected void onUpdate(AjaxRequestTarget target) {
						XDSFieldType type = (XDSFieldType) getComponent().getDefaultModelObject();
						filterType.updateChoices(target, Arrays.asList(type.getProperFilterTypes()));
					}
				});

				boolean needFilter = fieldVO.getInFilterPanel() != null && fieldVO.getInFilterPanel();
				type
					.setLabelVisible(false)
					.setRequired(true)
					.setLabel(new Model<>(getString("XDSField.type") + " " + fieldVO.getName()));
				filterType
					.setLabelVisible(false)
					.setLabel(new Model<>(getString("XDSField.filterType") + " " + fieldVO.getName()))
					.setRequired(needFilter)
					.setVisible(needFilter)
					.setOutputMarkupId(true)
					.setOutputMarkupPlaceholderTag(true);
				/*required
					.setOutputMarkupId(true)
					.setOutputMarkupPlaceholderTag(true)
					.setVisible(needFilter);*/
			}
		});
	}
}
