package org.devocative.metis.web.dPage.data.form;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.List;

class DefineLookupStep extends WWizardStepPanel {
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
		setEnabled(dataVO.isDataSourceEditable());

		table = new WebMarkupContainer("table");
		add(table);

		table.add(new ListView<DataAbstractFieldVO>("fields", lookupFields) {
			@Override
			protected void populateItem(ListItem<DataAbstractFieldVO> item) {
				DataAbstractFieldVO fieldVO = item.getModelObject();

				item.add(new Label("name", fieldVO.getName()));
				item.add(new WSelectionInput("targetDS", new PropertyModel(fieldVO, "targetDS"),
					dataSourceList, false)
					.setLabelVisible(false)
					.setRequired(true)
					.setLabel(new Model<>(fieldVO.getName())));
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
		dataSourceList.addAll(dataSourceService.getListForLookup());

		super.onBeforeRender();
	}
}
