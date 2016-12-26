package org.devocative.metis.web.dpage.data.form;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.metis.entity.data.config.XDSFieldFilterType;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WSelectionInputAjaxUpdatingBehavior;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class ParamStep extends WWizardStepPanel {
	private static final long serialVersionUID = -5889919676603233247L;

	private DataVO dataVO;

	private Label messageWhenNoParam;
	private WebMarkupContainer table;

	@Inject
	private IDataService dataService;

	public ParamStep(DataVO dataVO) {
		this.dataVO = dataVO;
	}

	@Override
	public boolean onStepSubmit(AjaxRequestTarget target) {
		if (dataVO.isDataSourceEditable()) {
			dataService.updateFieldsByQuery(dataVO);
		}

		return true;
	}

	@Override
	protected void onInit() {
		setEnabled(dataVO.isDataSourceEditable());

		table = new WebMarkupContainer("table");
		add(table);

		table.add(new ListView<DataParameterVO>("params", dataVO.getParams()) {
			private static final long serialVersionUID = -3179712784909396096L;

			@Override
			protected void populateItem(ListItem<DataParameterVO> item) {
				DataParameterVO parameterVO = item.getModelObject();

				List<XDSFieldFilterType> filterTypes = new ArrayList<>();
				if (parameterVO.getType() != null) {
					filterTypes.addAll(Arrays.asList(parameterVO.getType().getParamProperFilterTypes()));
				}

				final WSelectionInput type, filterType;

				item.add(new Label("name", parameterVO.getName()));
				item.add(new WTextInput("title", new PropertyModel<String>(parameterVO, "title"))
					.setLabelVisible(false));
				item.add(type = new WSelectionInput("type", new PropertyModel<String>(parameterVO, "type"),
					XDSFieldType.getParameterProperTypes(), false));
				item.add(filterType = new WSelectionInput("filterType", new PropertyModel<String>(parameterVO, "filterType"),
					filterTypes, false));
				item.add(new CheckBox("required", new PropertyModel<Boolean>(parameterVO, "required")));
				item.add(new WTextInput("sampleData", new PropertyModel<String>(parameterVO, "sampleData"))
					.setLabelVisible(false));

				type.addToChoices(new WSelectionInputAjaxUpdatingBehavior() {
					private static final long serialVersionUID = -4526092343940413152L;

					@Override
					protected void onUpdate(AjaxRequestTarget target) {
						XDSFieldType type = (XDSFieldType) getComponent().getDefaultModelObject();
						filterType.updateChoices(target, Arrays.asList(type.getParamProperFilterTypes()));
					}
				});

				type
					.setLabelVisible(false)
					.setRequired(true)
					.setLabel(new Model<>(getString("XDSField.type") + " " + parameterVO.getName()));

				filterType
					.setLabelVisible(false)
					.setLabel(new Model<>(getString("XDSField.filterType") + " " + parameterVO.getName()))
					.setRequired(true)
					.setOutputMarkupId(true);
			}
		});

		add(messageWhenNoParam = new Label("messageWhenNoParam", new ResourceModel("DataSource.alert.noParameter")));
	}

	@Override
	protected void onBeforeRender() {
		table.setVisible(dataVO.getParams().size() > 0);
		messageWhenNoParam.setVisible(dataVO.getParams().size() == 0);

		super.onBeforeRender();
	}
}
