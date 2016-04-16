package org.devocative.metis.web.dPage.data.form;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.devocative.metis.entity.data.config.XDSFieldResultType;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.entity.data.config.XDVAggregatorFunction;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class ColumnUIStep extends WWizardStepPanel {
	private DataVO dataVO;

	public ColumnUIStep(DataVO dataVO) {
		this.dataVO = dataVO;
	}

	@Override
	protected void onInit() {
		add(new ListView<DataFieldVO>("fields", dataVO.getFields()) {
			@Override
			protected void populateItem(ListItem<DataFieldVO> item) {
				DataFieldVO fieldVO = item.getModelObject();
				boolean enb = !XDSFieldType.Unknown.equals(fieldVO.getType());

				item.add(new Label("name", fieldVO.getName()));
				item.add(new Label("type", fieldVO.getType()));

				item.add(new CheckBox("inFilterPanel", new PropertyModel<Boolean>(fieldVO, "inFilterPanel"))
					.setRequired(fieldVO.getRequiredSafely())
					.setLabel(new Model<>(getString("XDVField.inFilterPanel", null, "inFilterPanel") + " " + fieldVO.getName()))
					.setEnabled(enb));

				List<XDSFieldResultType> resultTypeList = new ArrayList<>();
				if (!fieldVO.getIsKeyFieldSafely() && !fieldVO.getIsSelfRelPointerFieldSafely()) {
					resultTypeList.add(XDSFieldResultType.None);
				}
				resultTypeList.add(XDSFieldResultType.Hidden);
				resultTypeList.add(XDSFieldResultType.Shown);
				item.add(new WSelectionInput("resultType", new PropertyModel<String>(fieldVO, "resultType"), resultTypeList, false)
						.setLabelVisible(false)
						.setRequired(true)
						.setLabel(new Model<>(getString("XDVField.resultType", null, "resultType") + " " + fieldVO.getName()))
						.setEnabled(enb)
				);

				boolean showFooter = fieldVO.getType().isNumerical() &&
					!fieldVO.getIsKeyFieldSafely() &&
					!fieldVO.getIsSelfRelPointerFieldSafely();

				item.add(new WSelectionInput("footer", new PropertyModel<String>(fieldVO, "footer"),
						Arrays.asList(XDVAggregatorFunction.values()), true)
						.setLabelVisible(false)
						.setVisible(showFooter)
				);
			}
		});
	}
}
