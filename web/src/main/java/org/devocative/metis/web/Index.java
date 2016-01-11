package org.devocative.metis.web;

import org.apache.wicket.markup.html.WebPage;
import org.devocative.metis.web.panel.DataSourceViewerPanel;

public class Index extends WebPage {
	public Index() {
		add(new DataSourceViewerPanel("dataSource"));
	}
}
