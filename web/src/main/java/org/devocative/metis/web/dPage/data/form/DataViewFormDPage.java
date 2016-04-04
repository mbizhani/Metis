package org.devocative.metis.web.dPage.data.form;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.devocative.demeter.web.DPage;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.vo.DataVO;
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

	@Inject
	private IDataService dataService;

	@Inject
	private IDataSourceService dataSourceService;

	public DataViewFormDPage(String id, List<String> params) {
		super(id, params);

		DataVO dataVO;
		boolean newDataSource = false;

		if (params.size() > 0) {
			dataVO = dataService.loadDataVO(params.get(0));
		} else {
			dataVO = new DataVO();
			String dsName = getWebRequest().getRequestParameters().getParameterValue("dsName").toOptionalString();
			if (dsName != null) {
				dataService.updateDataVOByDataSource(dataVO, dsName);
			} else {
				newDataSource = true;
			}
		}

		Form form = new Form("form");
		add(form);

		OWizard oWizard = new OWizard()
			.addStep("init", new InitStep(dataVO, newDataSource))
			.addStep("query", new QueryStep(dataVO, newDataSource))
			.addStep("params", new ParamStep(dataVO, newDataSource))
			.addStep("columns", new ColumnDefStep(dataVO, newDataSource))
			.addStep("lookup", new DefineLookupStep(dataVO, newDataSource));
		//TODO a review step

		form.add(new WWizardPanel("wizard", oWizard, WWizardPanel.ButtonBarPlace.TOP) {
				@Override
				protected void onNext(AjaxRequestTarget target, String stepId) {
					/*if ("query".equals(stepId)) {
						setTitle(dataVO.getName());

						List<XDSParameter> list = dataSourceService.createParams(dataVO.getQuery().getText(), xdsParams);
						xdsParams.clear();
						xdsParams.addAll(list);

					} else if ("params".equals(stepId)) {
						List<XDSField> list = dataSourceService.createFields(
							xdsFields,
							xdsQuery,
							dataSource.getConnection().getId(),
							xdsParams
						);
						xdsFields.clear();
						xdsFields.addAll(list);
					}*/
				}

				@Override
				protected void onFinish(AjaxRequestTarget target, String stepId) {
					/*dataSourceService.saveOrUpdate(dataSource, xdsQuery, xdsFields, xdsParams);

					UrlUtil.redirectTo(DataSourceExecutor.class, dataSource.getName());*/
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
