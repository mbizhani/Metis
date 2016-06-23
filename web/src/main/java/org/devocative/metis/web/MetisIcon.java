package org.devocative.metis.web;

import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DemeterIcon;
import org.devocative.wickomp.html.HTMLBase;
import org.devocative.wickomp.html.icon.FontAwesome;

public class MetisIcon extends DemeterIcon {
	public static final HTMLBase CHECK_CONNECTION = new FontAwesome("plug", new ResourceModel("DBConnection.testQuery"));
	public static final HTMLBase DEFAULT_CONNECTION = new FontAwesome("dot-circle-o", new ResourceModel("DBConnection.user.default"));
}
