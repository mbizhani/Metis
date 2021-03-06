package org.devocative.metis.web.dpage.data.menu;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.adroit.ConfigUtil;
import org.devocative.demeter.web.DPage;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.MetisPrivilegeKey;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.iservice.data.IReportService;
import org.devocative.metis.web.MetisIcon;
import org.devocative.metis.web.panel.ExportImportPanel;
import org.devocative.wickomp.WPanel;
import org.devocative.wickomp.html.WAjaxLink;
import org.devocative.wickomp.html.WMessager;
import org.devocative.wickomp.html.tab.OTab;
import org.devocative.wickomp.html.tab.OTabbedPanel;
import org.devocative.wickomp.html.tab.WTabbedPanel;
import org.devocative.wickomp.html.window.WModalWindow;
import org.devocative.wickomp.opt.OSize;
import org.devocative.wickomp.wrcs.CommonBehavior;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ReportMenuDPage extends DPage {
	private static final long serialVersionUID = -3354039946096764805L;
	private static final Logger logger = LoggerFactory.getLogger(ReportMenuDPage.class);

	private WTabbedPanel tabPanel;
	private String sentDBConnection;

	public ReportMenuDPage(String id, List<String> params) {
		super(id, params);

		OTabbedPanel oTabbedPanel = new OTabbedPanel();
		oTabbedPanel
			.setGlobalHotkeyEnabled(true);
		tabPanel = new WTabbedPanel("tabPanel", oTabbedPanel);
		tabPanel.addTab(new ReportMenuPanel(tabPanel.getTabContentId()),
			new ResourceModel("Report.menu.tab", "Reports List"));

		add(tabPanel);

		// TODO: the common.css must be after d_dmt.css (at first it is not in the page, and later added by ajax)
		add(new CommonBehavior());
	}

	// ------------------------------

	private class ReportMenuPanel extends WPanel {
		private static final long serialVersionUID = -9012565863392514416L;

		@Inject
		private IReportService reportService;

		public ReportMenuPanel(String id) {
			super(id);
		}

		@Override
		protected void onInitialize() {
			super.onInitialize();

			if (ConfigUtil.hasKey(MetisConfigKey.DBConnParamName)) {
				sentDBConnection = getWebRequest()
					.getRequestParameters()
					.getParameterValue(ConfigUtil.getString(MetisConfigKey.DBConnParamName))
					.toOptionalString();
			}

			final WModalWindow modalWindow = new WModalWindow("modalWindow");
			add(modalWindow);

			add(new WAjaxLink("exportImport", MetisIcon.EXPORT_IMPORT) {
				private static final long serialVersionUID = -1334810219991176112L;

				@Override
				public void onClick(AjaxRequestTarget target) {
					modalWindow.getOptions()
						.setWidth(OSize.fixed(400))
						.setHeight(OSize.fixed(400));
					modalWindow.setContent(
						new ExportImportPanel(modalWindow.getContentId())
							.setSentDBConnection(sentDBConnection)
					);
					modalWindow.show(target);
				}
			}.setVisible(
				hasPermission(MetisPrivilegeKey.ReportImport)
					|| hasPermission(MetisPrivilegeKey.ReportExport)
			));

			final Map<DataGroup, List<Report>> listPerGroup = reportService.listPerGroup(sentDBConnection);

			add(new ListView<DataGroup>("group", new ArrayList<>(listPerGroup.keySet())) {
				private static final long serialVersionUID = -4234474135351171102L;

				@Override
				protected void populateItem(ListItem<DataGroup> groupItem) {
					DataGroup group = groupItem.getModelObject();

					//item.add(new Label("groupTitle", group.getName()));
					groupItem.add(new AttributeModifier("title", group.getName()));

					groupItem.add(new ListView<Report>("report", listPerGroup.get(group)) {
						private static final long serialVersionUID = 7979832690560075180L;

						@Override
						protected void populateItem(ListItem<Report> item) {
							final Report report = item.getModelObject();

							WAjaxLink run = new WAjaxLink("run", new Model<>(report.getTitle())) {
								private static final long serialVersionUID = -2507993467733702720L;

								@Override
								public void onClick(AjaxRequestTarget target) {
									try {
										reportService.assertReportAuthorization(report, sentDBConnection);

										tabPanel.addTab(target, new ReportExecutorPanel(
												tabPanel.getTabContentId(),
												report.getId(),
												sentDBConnection),
											new OTab(new Model<>(report.getTitle()), true));
									} catch (Exception e) {
										logger.error("Showing Report: ", e);
										WMessager.show(e, target);
									}
								}
							};
							run.add(new AttributeModifier("gidx", groupItem.getIndex()));
							item.add(run);
						}
					});
				}
			});
		}
	}
}
