//overwrite
package org.devocative.metis.web.dpage.data;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.iservice.data.IDataGroupService;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.html.window.WModalWindow;

import javax.inject.Inject;
import java.util.Collections;
import java.util.List;

public class DataGroupFormDPage extends DPage {
	private static final long serialVersionUID = -1240514247L;

	@Inject
	private IDataGroupService dataGroupService;

	private DataGroup entity;

	// ------------------------------

	public DataGroupFormDPage(String id) {
		this(id, new DataGroup());
	}

	// Main Constructor - For Ajax Call
	public DataGroupFormDPage(String id, DataGroup entity) {
		super(id, Collections.<String>emptyList());

		this.entity = entity;
	}

	// ---------------

	// Main Constructor - For REST Call
	public DataGroupFormDPage(String id, List<String> params) {
		super(id, params);

		this.entity = params != null && !params.isEmpty() ?
			dataGroupService.load(params.get(0)) :
			new DataGroup();
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		WFloatTable floatTable = new WFloatTable("floatTable");
		//floatTable.setEqualWidth(true);
		floatTable.add(new WTextInput("name")
			.setLabel(new ResourceModel("DataGroup.name")));

		Form<DataGroup> form = new Form<>("form", new CompoundPropertyModel<>(entity));
		form.add(floatTable);
		form.add(new DAjaxButton("save", new ResourceModel("label.save")) {
			private static final long serialVersionUID = 1888350113L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				dataGroupService.saveOrUpdate(entity);

				if (!WModalWindow.closeParentWindow(DataGroupFormDPage.this, target)) {
					UrlUtil.redirectTo(DataGroupListDPage.class);
				}
			}
		});
		add(form);
	}
}