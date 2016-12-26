package org.devocative.metis.web.dPage.data.form;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.devocative.metis.entity.data.config.XDVLink;
import org.devocative.metis.iservice.data.IDataViewService;
import org.devocative.metis.vo.DataVO;
import org.devocative.wickomp.form.WAjaxButton;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.form.wizard.WWizardStepPanel;
import org.devocative.wickomp.html.WAjaxLink;

import javax.inject.Inject;
import java.util.ArrayList;

public class LinksToDataViewStep extends WWizardStepPanel {
	private static final long serialVersionUID = 6693655081502853685L;

	@Inject
	private IDataViewService dataViewService;

	private DataVO dataVO;

	private WebMarkupContainer rows;

	public LinksToDataViewStep(DataVO dataVO) {
		this.dataVO = dataVO;
	}

	@Override
	protected void onInit() {
		if (dataVO.getLinksToDV() == null) {
			dataVO.setLinksToDV(new ArrayList<XDVLink>());
		}

		add(rows = new WebMarkupContainer("rows"));
		rows.setOutputMarkupId(true);

		rows.add(new ListView<XDVLink>("row", dataVO.getLinksToDV()) {
			private static final long serialVersionUID = -5558187704889220863L;

			@Override
			protected void populateItem(ListItem<XDVLink> item) {
				final XDVLink xdvLink = item.getModelObject();

				item.add(
					new WTextInput("title", new PropertyModel<String>(xdvLink, "title"))
						.setLabelVisible(false)
				);

				item.add(
					new WSelectionInput("targetDV", new PropertyModel(xdvLink, "targetDV"), dataViewService.list(), false)
						.setLabelVisible(false)
						.setLabel(new Model<>("targetDV"))
						.setRequired(true)
				);

				item.add(
					new TextArea<>("sentData", new PropertyModel<String>(xdvLink, "sentData"))
				);

				item.add(
					new WAjaxLink("remove") {
						private static final long serialVersionUID = -4788782781874860300L;

						@Override
						public void onClick(AjaxRequestTarget target) {
							dataVO.getLinksToDV().remove(xdvLink);
							target.add(rows);
						}
					}
				);
			}
		});

		add(new WAjaxButton("add") {
			private static final long serialVersionUID = -4640376692879387720L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				dataVO.getLinksToDV().add(new XDVLink());
				target.add(rows);
			}
		});
	}
}
