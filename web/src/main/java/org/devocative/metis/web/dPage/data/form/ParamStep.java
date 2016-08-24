package org.devocative.metis.web.dPage.data.form;

import org.apache.wicket.ajax.AjaxRequestTarget;
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
import java.util.ArrayList;
import java.util.Collections;
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

				List<XDSFieldType> xdsFieldTypes = new ArrayList<>();
				Collections.addAll(xdsFieldTypes, XDSFieldType.values());
				xdsFieldTypes.remove(XDSFieldType.LookUp);
				xdsFieldTypes.remove(XDSFieldType.Unknown);

				final WSelectionInput type;
				item.add(new Label("name", parameterVO.getName()));
				item.add(new WTextInput("title", new PropertyModel<String>(parameterVO, "title"))
					.setLabelVisible(false));
				item.add(type = new WSelectionInput("type", new PropertyModel<String>(parameterVO, "type"),
					xdsFieldTypes, false));
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
