package org.devocative.metis.iservice;

import org.devocative.metis.vo.DataVO;

public interface IDataEventHandler {
	void handleDataVoSaved(DataVO dataVO);
}
