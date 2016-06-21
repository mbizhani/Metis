package org.devocative.metis.web;

import org.devocative.demeter.iservice.task.ITaskService;
import org.devocative.demeter.web.WebDModule;
import org.devocative.wickomp.async.AsyncToken;
import org.devocative.wickomp.async.IAsyncRequestHandler;

import javax.inject.Inject;
import java.io.Serializable;

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
				storeAsyncToken(asyncToken);
				taskService.start("mtsExecuteDataSourceDTask", asyncToken.getId(), requestPayLoad, MetisDModule.this);
			}
		});
		registerAsyncHandler(EXEC_DATA_VIEW_CHILDREN, new IAsyncRequestHandler() {
			@Override
			public void onRequest(AsyncToken asyncToken, Object requestPayLoad) {
				storeAsyncToken(asyncToken);
				taskService.start("mtsExecuteDataSourceDTask", asyncToken.getId(), requestPayLoad, MetisDModule.this);
			}
		});
	}

	@Override
	public void onTaskResult(String id, Object result) {
		pushResponseToPage(getAndRemove(id), (Serializable) result);
	}

	@Override
	public void onTaskError(String id, Exception e) {
		pushErrorToPage(getAndRemove(id), e);
	}
}
