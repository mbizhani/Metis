package org.devocative.metis;

import org.devocative.demeter.imodule.DModuleException;
import org.devocative.metis.vo.query.QueryExecInfoRVO;

import java.util.Collections;
import java.util.List;

public class MetisException extends DModuleException {
	private static final long serialVersionUID = -3778789679497632312L;

	private List<QueryExecInfoRVO> execInfoList;

	// ------------------------------

	public MetisException(MetisErrorCode errorCode) {
		this(errorCode, null, null);
	}

	public MetisException(MetisErrorCode errorCode, String errorParameter) {
		this(errorCode, errorParameter, null);
	}

	public MetisException(MetisErrorCode errorCode, Throwable cause) {
		this(errorCode, null, cause);
	}

	// Main Constructor
	public MetisException(MetisErrorCode errorCode, String errorParameter, Throwable cause) {
		super("mts", errorCode, errorParameter, cause);
	}

	// ------------------------------

	public List<QueryExecInfoRVO> getExecInfoList() {
		return execInfoList;
	}

	public MetisException setExecInfoList(List<QueryExecInfoRVO> execInfoList) {
		this.execInfoList = execInfoList;
		return this;
	}

	// ------------------------------

	public MetisException setExecInfoList(QueryExecInfoRVO execInfo) {
		this.execInfoList = Collections.singletonList(execInfo);
		return this;
	}
}
