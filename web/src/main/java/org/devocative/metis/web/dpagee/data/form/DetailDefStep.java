package org.devocative.metis.web.dpagee.data.form;

import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;

public class DetailDefStep extends WWizardStepPanel {
	private static final long serialVersionUID = 7536509579068694394L;

	private DataVO dataVO;

	public DetailDefStep(DataVO dataVO) {
		this.dataVO = dataVO;
	}

	@Override
	protected void onInit() {

	}
}
