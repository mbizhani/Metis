package org.devocative.metis.web.dpage.data.form;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.model.PropertyModel;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.WOrderedListInput;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.List;

class FilterUIStep extends WWizardStepPanel {
	private static final long serialVersionUID = 275497861004109200L;

	private DataVO dataVO;
	private List<DataAbstractFieldVO> selectedFilters = new ArrayList<>();

	@Inject
	private IDataService dataService;

	public FilterUIStep(DataVO dataVO) {
		this.dataVO = dataVO;
	}

	@Override
	public boolean onStepSubmit(AjaxRequestTarget target) {
		for (DataAbstractFieldVO abstractFieldVO : dataVO.getAllFields()) {
			abstractFieldVO.setInFilterPanel(false);
			abstractFieldVO.setFilterPanelOrder(null);
		}

		for (int i = 0; i < selectedFilters.size(); i++) {
			DataAbstractFieldVO abstractFieldVO = selectedFilters.get(i);
			abstractFieldVO.setInFilterPanel(true);
			abstractFieldVO.setFilterPanelOrder(i);
		}

		return true;
	}

	@Override
	protected void onInit() {
		selectedFilters.addAll(dataService.findFilteringFields(dataVO.getAllFields()));
		add(new WOrderedListInput<>("filterSelection", new PropertyModel<List<DataAbstractFieldVO>>(this, "selectedFilters"), dataVO.getAllFields()));
	}
}
