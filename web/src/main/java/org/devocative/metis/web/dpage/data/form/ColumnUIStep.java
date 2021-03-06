package org.devocative.metis.web.dpage.data.form;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
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
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.code.OCode;
import org.devocative.wickomp.form.code.OCodeMode;
import org.devocative.wickomp.form.code.WCodeInput;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class ColumnUIStep extends WWizardStepPanel {
	private static final long serialVersionUID = -1788789595236589905L;

	private DataVO dataVO;

	ColumnUIStep(DataVO dataVO) {
		this.dataVO = dataVO;
	}

	@Override
	protected void onInit() {

		add(new WCodeInput("selectionValidationJS", new PropertyModel<>(dataVO, "selectionValidationJS"), new OCode(OCodeMode.JAVA_SCRIPT)));

		add(new WCodeInput("rowStyler", new PropertyModel<>(dataVO, "rowStyler"), new OCode(OCodeMode.GROOVY)));

		add(new ListView<DataFieldVO>("fields", dataVO.getFields()) {
			private static final long serialVersionUID = 7157602000394481452L;

			@Override
			protected void populateItem(ListItem<DataFieldVO> item) {
				DataFieldVO fieldVO = item.getModelObject();
				boolean enb = !XDSFieldType.Unknown.equals(fieldVO.getType());

				item.add(new Label("name", fieldVO.getUiName()));
				item.add(new Label("type", fieldVO.getType()));

				List<XDSFieldResultType> resultTypeList = new ArrayList<>();
				if (!fieldVO.getIsKeyFieldSafely() && !fieldVO.getIsSelfRelPointerFieldSafely()) {
					resultTypeList.add(XDSFieldResultType.None);
				}
				resultTypeList.add(XDSFieldResultType.Hidden);
				resultTypeList.add(XDSFieldResultType.Shown);

				final WNumberInput columnWidth;
				item.add(columnWidth = new WNumberInput("columnWidth", new PropertyModel<>(fieldVO, "columnWidth"), Integer.class));
				columnWidth
					.setLabelVisible(false)
					.setLabel(new Model<>(getString("XDVField.columnWidth") + " " + fieldVO.getName()))
					.add(RangeValidator.range(50, 500))
					.setOutputMarkupId(true)
					.setOutputMarkupPlaceholderTag(true)
					.setVisible(fieldVO.getResultType() == null || fieldVO.getResultType() == XDSFieldResultType.Shown);

				WTextInput format = new WTextInput("format", new PropertyModel<>(fieldVO, "format"));
				format
					.setOutputMarkupId(true)
					.setOutputMarkupPlaceholderTag(true)
					.setVisible(!fieldVO.getIsKeyFieldSafely() &&
						fieldVO.getType().isFormatted() &&
						(fieldVO.getResultType() != null && fieldVO.getResultType() == XDSFieldResultType.Shown));
				item.add(format);


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
						format.setVisible(resultType == XDSFieldResultType.Shown);
						target.add(columnWidth);
						target.add(format);
					}
				});

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
