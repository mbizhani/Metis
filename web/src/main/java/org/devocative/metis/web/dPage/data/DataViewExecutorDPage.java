package org.devocative.metis.web.dPage.data;

import org.devocative.demeter.web.DPage;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.vo.DataVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.util.List;

public class DataViewExecutorDPage extends DPage {
	private static final Logger logger = LoggerFactory.getLogger(DataViewExecutorDPage.class);

	@Inject
	private IDataService dataService;

	public DataViewExecutorDPage(String id, List<String> params) {
		super(id, params);

		DataVO dataVO = dataService.loadDataVO(params.get(0));


	}
}
