package org.devocative.metis.web.dpage.data.form;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.data.EConnectionSelection;
import org.devocative.metis.entity.data.config.XDSQueryMode;
import org.devocative.metis.entity.data.config.XDVGridSelectionMode;
import org.devocative.metis.iservice.connection.IDBConnectionService;
import org.devocative.metis.iservice.data.IDataGroupService;
import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WSelectionInputAjaxUpdatingBehavior;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.validator.WAsciiIdentifierValidator;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;

import javax.inject.Inject;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

class InitStep extends WWizardStepPanel {
	private static final long serialVersionUID = -1186489219839480432L;

	private DataVO dataVO;

	@Inject
	private IDBConnectionService connectionService;

	@Inject
	private IDataGroupService dataGroupService;

	public InitStep(DataVO dataVO) {
		this.dataVO = dataVO;
	}

	@Override
	protected void onInit() {
		final WSelectionInput connection, queryMode;

		WebMarkupContainer dataSourceLabelRow = new WebMarkupContainer("dataSourceLabelRow");
		dataSourceLabelRow.setVisible(!dataVO.isDataSourceEditable());
		dataSourceLabelRow.add(new Label("dataSourceLabel", dataVO.getDataSourceName()));
		add(dataSourceLabelRow);

		add(new WTextInput("name", new PropertyModel<>(dataVO, "name"))
				.setLabelVisible(false)
				.add(new WAsciiIdentifierValidator())
				.setRequired(true)
				.setLabel(new ResourceModel("DataSource.name"))
		);

		add(new WTextInput("title", new PropertyModel<>(dataVO, "title"))
			.setLabelVisible(false)
			.setRequired(true)
			.setLabel(new ResourceModel("DataSource.title")));

		add(connection = new WSelectionInput("connection",
			new PropertyModel<>(dataVO, "connection"),
			connectionService.list(),
			false
		));
		connection
			.setLabelVisible(false)
			.setRequired(true)
			.setLabel(new ResourceModel("DataSource.connection"))
			.setEnabled(dataVO.isDataSourceEditable());

		add(new WSelectionInput("connectionSelection",
				new PropertyModel<>(dataVO, "connectionSelection"),
				EConnectionSelection.list(),
				false)
				.setLabelVisible(false)
				.setRequired(true)
				.setLabel(new ResourceModel("DataSource.connectionSelection"))
				.setEnabled(dataVO.isDataSourceEditable())
		);

		List<XDSQueryMode> modes = dataVO.getConnectionHasMapping() ?
			Arrays.asList(XDSQueryMode.values()) :
			Collections.singletonList(XDSQueryMode.Sql);

		add(queryMode = new WSelectionInput("queryMode", new PropertyModel<>(dataVO, "query.mode"), modes, false));
		queryMode
			.setLabelVisible(false)
			.setRequired(true)
			.setLabel(new ResourceModel("DataSource.query.mode"))
			.setEnabled(dataVO.isDataSourceEditable());

		if (dataVO.getCaseSensitiveFilter() == null) {
			dataVO.setCaseSensitiveFilter(false);
		}

		/*add(new WBooleanInput("caseSensitiveFilter")
				.setLabelVisible(false)
				.setRequired(true)
				.setLabel(new ResourceModel("DataSource.caseSensitiveFilter"))
				.setEnabled(dataVO.isDataSourceEditable())
		);*/

		/*add(new WSelectionInput("groups", new PropertyModel<>(dataVO, "groups"), dataGroupService.list(), true)
			.setLabelVisible(false)
			.setLabel(new ResourceModel("DataView.groups")));*/

		add(new WSelectionInput("groups", new PropertyModel<>(dataVO, "group"), dataGroupService.list(), false)
			.setLabelVisible(false)
			.setRequired(true)
			.setLabel(new ResourceModel("DataView.group")));

		add(new WSelectionInput("selectionMode", new PropertyModel<>(dataVO, "selectionMode"),
			Arrays.asList(XDVGridSelectionMode.values()), false)
			.setLabelVisible(false)
			.setRequired(true)
			.setLabel(new ResourceModel("DataView.selectionMode")));

		/*add(new WSelectionInput("gridHeight", new PropertyModel<>(dataVO, "gridHeight"),
			Arrays.asList(XDVGridHeight.values()), false)
			.setLabelVisible(false)
			.setRequired(true).setLabel(new ResourceModel("DataView.gridHeight")));*/

		if (dataVO.isDataSourceEditable()) {
			connection.addToChoices(new WSelectionInputAjaxUpdatingBehavior() {
				private static final long serialVersionUID = -7082781026542918009L;

				@Override
				protected void onUpdate(AjaxRequestTarget target) {
					DBConnection dbConnection = (DBConnection) getComponent().getDefaultModelObject();
					if (dbConnection.getSafeConfigId() != null) {
						queryMode.updateChoices(target, Arrays.asList(XDSQueryMode.values()));
					} else {
						queryMode.updateChoices(target, Collections.singletonList(XDSQueryMode.Sql));
					}
				}
			});
		}
	}
}
