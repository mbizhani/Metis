package org.devocative.metis.web.dpage.data.menu;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.Report;
import org.devocative.metis.iservice.data.IReportService;
import org.devocative.wickomp.WPanel;
import org.devocative.wickomp.async.AsyncBehavior;
import org.devocative.wickomp.html.WAjaxLink;
import org.devocative.wickomp.html.tab.OTab;
import org.devocative.wickomp.html.tab.WTabbedPanel;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ReportMenuDPage extends DPage {
	private static final long serialVersionUID = -3354039946096764805L;

	private WTabbedPanel tabPanel;

	public ReportMenuDPage(String id, List<String> params) {
		super(id, params);

		tabPanel = new WTabbedPanel("tabPanel");
		tabPanel.addTab(new ReportMenuPanel(tabPanel.getTabContentId()),
			new ResourceModel("Report.menu.tab", "Reports List"));

		add(tabPanel);

		// TODO
		add(new AsyncBehavior());
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

			final Map<DataGroup, List<Report>> listPerGroup = reportService.listPerGroup();

			add(new ListView<DataGroup>("group", new ArrayList<>(listPerGroup.keySet())) {
				private static final long serialVersionUID = -4234474135351171102L;

				@Override
				protected void populateItem(ListItem<DataGroup> item) {
					DataGroup group = item.getModelObject();

					//item.add(new Label("groupTitle", group.getName()));
					item.add(new AttributeModifier("title", group.getName()));

					item.add(new ListView<Report>("report", listPerGroup.get(group)) {
						private static final long serialVersionUID = 7979832690560075180L;

						@Override
						protected void populateItem(ListItem<Report> item) {
							final Report report = item.getModelObject();

							WAjaxLink run = new WAjaxLink("run", new Model<>(report.getTitle())) {
								private static final long serialVersionUID = -2507993467733702720L;

								@Override
								public void onClick(AjaxRequestTarget target) {
									tabPanel.addTab(target, new ReportExecutorPanel(
											tabPanel.getTabContentId(),
											report.getId()),
										new OTab(new Model<>(report.getTitle()), true));
								}
							};
							//run.add(new AttributeModifier("title", report.getTitle()));
							item.add(run);
						}
					});
				}
			});
		}
	}
}