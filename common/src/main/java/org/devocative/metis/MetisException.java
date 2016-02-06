package org.devocative.metis;

import org.devocative.demeter.imodule.DModuleException;

public class MetisException extends DModuleException {
	public MetisException(MetisErrorCode errorCode) {
		this(errorCode, null, null);
	}

	public MetisException(MetisErrorCode errorCode, String errorParameter) {
		this(errorCode, errorParameter, null);
	}

	// Main Constructor
	public MetisException(MetisErrorCode errorCode, String errorParameter, Throwable cause) {
		super("MTS", errorCode, errorParameter, cause);
	}
}
