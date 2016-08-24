package org.devocative.metis;

import org.devocative.demeter.imodule.DModuleException;

public class MetisException extends DModuleException {
	private static final long serialVersionUID = -3778789679497632312L;

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
}
