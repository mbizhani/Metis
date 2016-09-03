package org.devocative.metis.web.dPage.data.form;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.validation.validator.RangeValidator;
import org.devocative.metis.entity.data.config.XDSFieldResultType;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.entity.data.config.XDVAggregatorFunction;
import org.devocative.metis.vo.DataFieldVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.WNumberInput;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WSelectionInputAjaxUpdatingBehavior;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class ColumnUIStep extends WWizardStepPanel {
	private static final long serialVersionUID = -1788789595236589905L;

	private DataVO dataVO;

	public ColumnUIStep(DataVO dataVO) {
		this.dataVO = dataVO;
	}

	@Override
	protected void onInit() {
		add(new ListView<DataFieldVO>("fields", dataVO.getFields()) {
			private static final long serialVersionUID = 7157602000394481452L;

			@Override
			protected void populateItem(ListItem<DataFieldVO> item) {
				DataFieldVO fieldVO = item.getModelObject();
				boolean enb = !XDSFieldType.Unknown.equals(fieldVO.getType());

				if (fieldVO.getType() == XDSFieldType.LookUp) {
					fieldVO.setInFilterPanel(true);
				}

				item.add(new Label("name", fieldVO.getUiName()));
				item.add(new Label("type", fieldVO.getType()));

				item.add(new CheckBox("inFilterPanel", new PropertyModel<Boolean>(fieldVO, "inFilterPanel"))
					.setRequired(fieldVO.getRequiredSafely())
					.setLabel(new Model<>(getString("XDVField.inFilterPanel", null, "inFilterPanel") + " " + fieldVO.getName()))
					.setEnabled(enb && fieldVO.getType() != XDSFieldType.LookUp));

				List<XDSFieldResultType> resultTypeList = new ArrayList<>();
				if (!fieldVO.getIsKeyFieldSafely() && !fieldVO.getIsSelfRelPointerFieldSafely()) {
					resultTypeList.add(XDSFieldResultType.None);
				}
				resultTypeList.add(XDSFieldResultType.Hidden);
				resultTypeList.add(XDSFieldResultType.Shown);

				final WNumberInput columnWidth;
				item.add(columnWidth = new WNumberInput("columnWidth", new PropertyModel<Number>(fieldVO, "columnWidth"), Integer.class));
				columnWidth
					.setLabelVisible(false)
					.setLabel(new Model<>(getString("XDVField.columnWidth") + " " + fieldVO.getName()))
					.add(RangeValidator.range(50, 500))
					.setOutputMarkupId(true)
					.setOutputMarkupPlaceholderTag(true)
					.setVisible(fieldVO.getResultType() == null || fieldVO.getResultType() == XDSFieldResultType.Shown);

				WSelectionInput resultType;
				item.add(resultType = new WSelectionInput("resultType", new PropertyModel<String>(fieldVO, "resultType"), resultTypeList, false));
				resultType
					.setLabelVisible(false)
					.setRequired(true)
					.setLabel(new Model<>(getString("XDSField.resultType") + " " + fieldVO.getName()))
					.setEnabled(enb);
				resultType.addToChoices(new WSelectionInputAjaxUpdatingBehavior() {
					private static final long serialVersionUID = -6395621430140842441L;

					@Override
					protected void onUpdate(AjaxRequestTarget target) {
						XDSFieldResultType resultType = (XDSFieldResultType) getComponent().getDefaultModelObject();
						columnWidth.setVisible(resultType == XDSFieldResultType.Shown);
						target.add(columnWidth);
					}
				});

				boolean showFooter = !dataVO.isDataSourceEditable() &&
					fieldVO.getType().isNumerical() &&
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
