package org.devocative.metis.web;

import org.apache.wicket.model.ResourceModel;
import org.devocative.demeter.web.DemeterIcon;
import org.devocative.wickomp.html.icon.FontAwesome;
import org.devocative.wickomp.html.icon.IconFont;

public class MetisIcon extends DemeterIcon {
	public static final IconFont CHECK_CONNECTION = new FontAwesome("plug", new ResourceModel("DBConnection.testQuery"));
	public static final IconFont DEFAULT_CONNECTION = new FontAwesome("dot-circle-o", new ResourceModel("DBConnection.user.default"));
}
