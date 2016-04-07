package org.devocative.metis.web.dPage.data.form;

import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;

public class DetailDefStep extends WWizardStepPanel {
	private DataVO dataVO;

	public DetailDefStep(DataVO dataVO) {
		this.dataVO = dataVO;
	}

	@Override
	protected void onInit() {

	}
}
