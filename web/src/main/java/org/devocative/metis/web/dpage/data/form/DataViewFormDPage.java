package org.devocative.metis.web.dpage.data.form;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.Model;
import org.devocative.adroit.ObjectUtil;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.web.dpage.data.DataViewExecutorDPage;
import org.devocative.wickomp.form.wizard.OWizard;
import org.devocative.wickomp.form.wizard.WWizardPanel;
import org.devocative.wickomp.html.WMessager;
import org.devocative.wickomp.wrcs.EasyUIBehavior;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.io.Serializable;
import java.util.List;

public class DataViewFormDPage extends DPage {
	private static final long serialVersionUID = -2793411368253405276L;
	private static Logger logger = LoggerFactory.getLogger(DataViewFormDPage.class);

	private DataVO dataVO;

	@Inject
	private IDataService dataService;

	// ------------------------------

	public DataViewFormDPage(String id, List<String> params) {
		super(id, params);

		if (params.size() > 0) {
			dataVO = dataService.loadDataVOByName(params.get(0));
		} else {
			String dsName = getWebRequest().getRequestParameters().getParameterValue("dsName").toOptionalString();
			if (dsName != null) {
				dataVO = dataService.createAnotherDataView(dsName);
			}
		}

		if (dataVO == null) {
			dataVO = new DataVO();
		}

		Form form = new Form("form");
		add(form);

		OWizard oWizard = new OWizard()
			.addStep("init", new InitStep(dataVO))
			.addStep("query", new QueryStep(dataVO))
			.addStep("param", new ParamStep(dataVO))
			.addStep("columnDef", new ColumnDefStep(dataVO))
			.addStep("lookup", new DefineLookupStep(dataVO))
			.addStep("filterUI", new FilterUIStep(dataVO))
			.addStep("columnUI", new ColumnUIStep(dataVO))
			.addStep("linksToDVS", new LinksToDataViewStep(dataVO));

		form.add(new WWizardPanel("wizard", oWizard, WWizardPanel.ButtonBarPlace.TOP) {
				private static final long serialVersionUID = -4534169573929180621L;

				@Override
				protected void onNext(AjaxRequestTarget target, String stepId) {
					if (logger.isDebugEnabled()) {
						logger.debug("Step={}, DataVO = {}", stepId, ObjectUtil.toString(dataVO));
					}

					if ("init".equals(stepId)) {
						setTitle(dataVO.getName());
					}
				}

				@Override
				protected void onFinish(AjaxRequestTarget target, String stepId) {
					dataService.saveOrUpdate(dataVO);
					UrlUtil.redirectTo(DataViewExecutorDPage.class, dataVO.getName());
				}

				@Override
				protected void onError(AjaxRequestTarget target, String stepId, List<Serializable> errors) {
					WMessager.show(getString("label.error"), errors, target);
				}

				@Override
				protected void onCancel(AjaxRequestTarget target, String stepId) {
					UrlUtil.redirectTo(DataViewExecutorDPage.class, dataVO.getName());
				}
			}.setTitle(dataVO.getName())
				.setFinishEnabled(!dataVO.isNewInstance())
				.setCancelButtonVisible(!dataVO.isNewInstance())
				.setCancelConfirmationMessage(new Model<>("Cancel?")) //TODO
				.setFinishConfirmationMessage(new Model<>("Finish?")) //TODO
		);

		add(new EasyUIBehavior());
	}
}
