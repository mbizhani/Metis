package org.devocative.metis.web.dPage.data.form;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.devocative.adroit.ObjectUtil;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.web.dPage.DataSourceExecutor;
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
	private static Logger logger = LoggerFactory.getLogger(DataViewFormDPage.class);
	private DataVO dataVO;

	@Inject
	private IDataService dataService;

	public DataViewFormDPage(String id, List<String> params) {
		super(id, params);

		if (params.size() > 0) {
			dataVO = dataService.loadDataVO(params.get(0));
		} else {
			dataVO = new DataVO();
			String dsName = getWebRequest().getRequestParameters().getParameterValue("dsName").toOptionalString();
			if (dsName != null) {
				dataService.updateDataVOByDataSource(dataVO, dsName);
			}
		}

		Form form = new Form("form");
		add(form);

		OWizard oWizard = new OWizard()
			.addStep("init", new InitStep(dataVO))
			.addStep("query", new QueryStep(dataVO))
			.addStep("param", new ParamStep(dataVO))
			.addStep("columnDef", new ColumnDefStep(dataVO))
			.addStep("lookup", new DefineLookupStep(dataVO))
			.addStep("columnUI", new ColumnUIStep(dataVO));
		//TODO a review step

		form.add(new WWizardPanel("wizard", oWizard, WWizardPanel.ButtonBarPlace.TOP) {
				@Override
				protected void onNext(AjaxRequestTarget target, String stepId) {
					if (logger.isDebugEnabled()) {
						logger.debug("Step={}, DataVO = {}", stepId, ObjectUtil.toString(dataVO));
					}

					setTitle(dataVO.getTitle());
				}

				@Override
				protected void onFinish(AjaxRequestTarget target, String stepId) {
					//dataSourceService.saveOrUpdate(dataSource, xdsQuery, xdsFields, xdsParams);
					dataService.saveOrUpdate(dataVO);
					UrlUtil.redirectTo(DataSourceExecutor.class, dataVO.getDataSourceName());
				}

				@Override
				protected void onError(AjaxRequestTarget target, String stepId, List<Serializable> errors) {
					WMessager.show(getString("label.error"), errors, target);
				}

			}.setTitle(dataVO.getName())
		);

		add(new EasyUIBehavior());
	}
}
