package org.devocative.metis.service.task;

import org.devocative.demeter.iservice.task.DTask;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.async.DataViewQVO;
import org.devocative.metis.vo.async.DataViewRVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Scope("prototype")
@Component("mtsExecuteDataSourceDTask")
public class ExecuteDataViewDTask extends DTask<DataViewRVO> {
	private DataViewQVO data;

	@Autowired
	private IDataService dataService;

	@Override
	public void init() {
		data = (DataViewQVO) getInputData();

		if (data != null) {
			String action = data.isDoExport() ? "EXPT" : data.getParentId() == null ? "EXEC" : "XPRT";
			setDetail(action + ": " + data.getName());
		}
	}

	@Override
	public boolean canStart() {
		return data != null;
	}

	@Override
	public void execute() {
		DataViewRVO result;
		if (data.getParentId() == null) {
			if (data.isDoExport()) {
				result = dataService.exportDataView(data);
			} else {
				result = dataService.executeDataView(data);
			}
		} else {
			result = dataService.executeDataViewForParent(data);
		}

		sendResult(result);
	}

	@Override
	public void cancel() {
		throw new RuntimeException("Not Implemented!");
	}
}
