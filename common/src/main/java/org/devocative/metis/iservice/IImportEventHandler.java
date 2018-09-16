package org.devocative.metis.iservice;

import java.util.Collection;

public interface IImportEventHandler {
	void handleDataViewImport(Collection<Object> ids);
}
