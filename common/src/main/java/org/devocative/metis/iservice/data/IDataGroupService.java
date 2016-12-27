//overwrite
package org.devocative.metis.iservice.data;

import org.devocative.demeter.entity.User;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.vo.filter.data.DataGroupFVO;

import java.util.List;

public interface IDataGroupService {
	void saveOrUpdate(DataGroup entity);

	DataGroup load(Long id);

	DataGroup loadByName(String name);

	List<DataGroup> list();

	List<DataGroup> search(DataGroupFVO filter, long pageIndex, long pageSize);

	long count(DataGroupFVO filter);

	List<User> getCreatorUserList();

	List<User> getModifierUserList();

	// ==============================
}