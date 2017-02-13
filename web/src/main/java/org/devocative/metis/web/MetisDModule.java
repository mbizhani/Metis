package org.devocative.metis.web;

import org.devocative.demeter.iservice.task.ITaskService;
import org.devocative.demeter.web.WebDModule;
import org.devocative.wickomp.async.AsyncToken;
import org.devocative.wickomp.async.IAsyncRequestHandler;

import javax.inject.Inject;

public class MetisDModule extends WebDModule {
	public static final String EXEC_DATA_VIEW = "EXEC_DATA_VIEW";
	public static final String EXEC_DATA_VIEW_CHILDREN = "EXEC_DATA_VIEW_CHILDREN";

	@Inject
	private ITaskService taskService;

	@Override
	public void init() {
		registerAsyncHandler(EXEC_DATA_VIEW, new IAsyncRequestHandler() {
			@Override
			public void onRequest(AsyncToken asyncToken, Object requestPayLoad) {
				taskService.start("mtsExecuteDataSourceDTask", asyncToken, requestPayLoad, MetisDModule.this);
			}
		});
		registerAsyncHandler(EXEC_DATA_VIEW_CHILDREN, new IAsyncRequestHandler() {
			@Override
			public void onRequest(AsyncToken asyncToken, Object requestPayLoad) {
				taskService.start("mtsExecuteDataSourceDTask", asyncToken, requestPayLoad, MetisDModule.this);
			}
		});
	}
}
