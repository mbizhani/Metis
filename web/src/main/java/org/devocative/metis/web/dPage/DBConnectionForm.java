package org.devocative.metis.web.dPage;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.devocative.demeter.web.DPage;
import org.devocative.metis.entity.DBConnectionInfo;
import org.devocative.metis.iservice.IDBConnectionService;
import org.devocative.wickomp.form.WTextInput;

import javax.inject.Inject;
import java.util.List;

public class DBConnectionForm extends DPage {

	@Inject
	private IDBConnectionService connectionService;

	public DBConnectionForm(String id, List<String> params) {
		super(id, params);

		//TODO edit

		final Form<DBConnectionInfo> form = new Form<>("form", new CompoundPropertyModel<>(new DBConnectionInfo()));
		form.add(new WTextInput("name"));
		form.add(new WTextInput("driver"));
		form.add(new WTextInput("url"));
		form.add(new WTextInput("username"));
		form.add(new WTextInput("password"));
		form.add(new WTextInput("schema"));
		form.add(new AjaxButton("save", form) {
			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> f) {
				connectionService.saveOrUpdate(form.getModelObject());
			}
		});
		add(form);
	}
}
