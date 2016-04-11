package org.devocative.metis.web.dPage.data.form;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataParameterVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;

import javax.inject.Inject;
import java.util.Arrays;

class ParamStep extends WWizardStepPanel {
	private DataVO dataVO;

	private Label messageWhenNoParam;
	private WebMarkupContainer table;

	@Inject
	private IDataService dataService;

	public ParamStep(DataVO dataVO) {
		this.dataVO = dataVO;
	}

	@Override
	public void onStepSubmit() {
		if (dataVO.isDataSourceEnabled()) {
			dataService.updateFieldsByQuery(dataVO);
		}
	}

	@Override
	protected void onInit() {
		setEnabled(dataVO.isDataSourceEnabled());

		table = new WebMarkupContainer("table");
		add(table);

		table.add(new ListView<DataParameterVO>("params", dataVO.getParams()) {
			@Override
			protected void populateItem(ListItem<DataParameterVO> item) {
				DataParameterVO parameterVO = item.getModelObject();

				final WSelectionInput type;
				item.add(new Label("name", parameterVO.getName()));
				item.add(new WTextInput("title", new PropertyModel<String>(parameterVO, "title"))
					.setLabelVisible(false));
				item.add(type = new WSelectionInput("type", new PropertyModel<String>(parameterVO, "type"),
					Arrays.asList(XDSFieldType.values()), false));
				item.add(new CheckBox("required", new PropertyModel<Boolean>(parameterVO, "required")));
				item.add(new WTextInput("sampleData", new PropertyModel<String>(parameterVO, "sampleData"))
					.setLabelVisible(false));

				type
					.setLabelVisible(false)
					.setRequired(true)
					.setLabel(new Model<>(getString("XDSField.type") + " " + parameterVO.getName()));

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