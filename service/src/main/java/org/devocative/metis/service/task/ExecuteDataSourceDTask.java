package org.devocative.metis.service.task;

import org.devocative.demeter.iservice.task.DTask;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.iservice.IDataSourceService;
import org.devocative.metis.vo.async.DataSourceRVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component("mtsExecuteDataSourceDTask")
@Scope("prototype")
public class ExecuteDataSourceDTask extends DTask {
	private DataSource dataSource;
	private Map<String, Object> data;

	@Autowired
	private IDataSourceService dataSourceService;

	@Override
	public void init() {
		data = (Map<String, Object>) inputData;
		dataSource = dataSourceService.loadByName((String) data.get("name"));
	}

	@Override
	public boolean canStart() {
		return dataSource != null;
	}

	@Override
	public void execute() {
		List<Map<String, Object>> list = dataSourceService.executeDataSource(dataSource.getName(),
			(Map<String, Object>) data.get("filter"),
			(Map<String, String>) data.get("sortFieldList"),
			(Long) data.get("pageIndex"),
			(Long) data.get("pageSize"));

		long count = dataSourceService.executeCountForDataSource(
			dataSource.getName(),
			(Map<String, Object>) data.get("filter"));

		setResult(new DataSourceRVO(list, count));
	}
}
