package org.devocative.metis.web.dpagee.data.form;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.data.IDataSourceService;
import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.WBooleanInput;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.List;

class DefineLookupStep extends WWizardStepPanel {
	private static final long serialVersionUID = -8238129378929524572L;

	private DataVO dataVO;

	private List<DataAbstractFieldVO> lookupFields = new ArrayList<>();
	private List<DataSource> dataSourceList = new ArrayList<>();

	private WebMarkupContainer table;
	private Label messageWhenNoLookUp;

	@Inject
	private IDataService dataService;

	@Inject
	private IDataSourceService dataSourceService;

	public DefineLookupStep(DataVO dataVO) {
		this.dataVO = dataVO;
	}

	@Override
	protected void onInit() {

		table = new WebMarkupContainer("table");
		add(table);

		table.add(new ListView<DataAbstractFieldVO>("fields", lookupFields) {
			private static final long serialVersionUID = 6753517836822734711L;

			@Override
			protected void populateItem(ListItem<DataAbstractFieldVO> item) {
				DataAbstractFieldVO fieldVO = item.getModelObject();

				item.add(new Label("name", fieldVO.getUiName()));
				item.add(new WSelectionInput("targetDS", new PropertyModel(fieldVO, "targetDS"),
						dataSourceList, false)
						.setLabelVisible(false)
						.setRequired(true)
						.setLabel(new Model<>(getString("DataSource") + " " + fieldVO.getName()))
						.setEnabled(dataVO.isDataSourceEditable())
				);

				item.add(new WBooleanInput("targetDSMultipleSelection", new PropertyModel<Boolean>(fieldVO, "targetDSMultipleSelection"))
					.setLabelVisible(false)
					.setRequired(true)
					.setLabel(new Model<>("Multiple Selection " + fieldVO.getName()))); //TODO

				item.add(new WTextInput("targetDSFilter", new PropertyModel<String>(fieldVO, "targetDSFilter"))
						.setLabelVisible(false)
						.add(new AttributeModifier("size", "50"))
				);
			}
		});

		add(messageWhenNoLookUp = new Label("messageWhenNoLookUp", new ResourceModel("DataSource.alert.noLookUp")));
	}

	@Override
	protected void onBeforeRender() {
		lookupFields.clear();
		lookupFields.addAll(dataService.findLookUpFields(dataVO));


		table.setVisible(lookupFields.size() > 0);
		messageWhenNoLookUp.setVisible(lookupFields.size() == 0);

		dataSourceList.clear();
		dataSourceList.addAll(dataSourceService.getAllDataSourcesAsLookup());

		super.onBeforeRender();
	}
}
