//overwrite
package org.devocative.metis.web.dpage.connection;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DPage;
import org.devocative.demeter.web.UrlUtil;
import org.devocative.demeter.web.component.DAjaxButton;
import org.devocative.metis.entity.connection.DBConnectionAlias;
import org.devocative.metis.entity.connection.EAliasMode;
import org.devocative.metis.iservice.connection.IDBConnectionAliasService;
import org.devocative.metis.web.MetisIcon;
import org.devocative.wickomp.form.WSelectionInput;
import org.devocative.wickomp.form.WTextInput;
import org.devocative.wickomp.html.WFloatTable;
import org.devocative.wickomp.html.window.WModalWindow;

import javax.inject.Inject;
import java.util.Collections;
import java.util.List;

public class DBConnectionAliasFormDPage extends DPage {
	private static final long serialVersionUID = 1373542470L;

	@Inject
	private IDBConnectionAliasService dBConnectionAliasService;

	private DBConnectionAlias entity;

	// ------------------------------

	public DBConnectionAliasFormDPage(String id) {
		this(id, new DBConnectionAlias());
	}

	// Main Constructor - For Ajax Call
	public DBConnectionAliasFormDPage(String id, DBConnectionAlias entity) {
		super(id, Collections.<String>emptyList());

		this.entity = entity;
	}

	// ---------------

	// Main Constructor - For REST Call
	public DBConnectionAliasFormDPage(String id, List<String> params) {
		super(id, params);

		this.entity = params != null && !params.isEmpty() ?
			dBConnectionAliasService.load(Long.valueOf(params.get(0))) :
			new DBConnectionAlias();
	}

	// ------------------------------

	@Override
	protected void onInitialize() {
		super.onInitialize();

		WFloatTable floatTable = new WFloatTable("floatTable");
		floatTable.add(new WTextInput("name")
			.setRequired(true)
			.setLabel(new ResourceModel("DBConnectionAlias.name", "name")));
		floatTable.add(new WSelectionInput("mode", EAliasMode.list(), false)
			.setLabel(new ResourceModel("DBConnectionAlias.mode", "mode")));
		floatTable.add(new WSelectionInput("connection", dBConnectionAliasService.getConnectionList(), false)
			.setRequired(true)
			.setLabel(new ResourceModel("DBConnectionAlias.connection", "connection")));

		Form<DBConnectionAlias> form = new Form<>("form", new CompoundPropertyModel<>(entity));
		form.add(floatTable);

		form.add(new DAjaxButton("save", new ResourceModel("label.save"), MetisIcon.SAVE) {
			private static final long serialVersionUID = 1908022062L;

			@Override
			protected void onSubmit(AjaxRequestTarget target) {
				dBConnectionAliasService.saveOrUpdate(entity);

				if (!WModalWindow.closeParentWindow(DBConnectionAliasFormDPage.this, target)) {
					UrlUtil.redirectTo(DBConnectionAliasListDPage.class);
				}
			}
		});
		add(form);
	}
}