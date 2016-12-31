//overwrite
package org.devocative.metis.vo.filter.data;

import org.devocative.adroit.vo.RangeVO;
import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.persistor.Filterer;
import org.devocative.metis.entity.data.DataGroup;
import org.devocative.metis.entity.data.DataView;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

@Filterer
public class ReportFVO implements Serializable {
	private static final long serialVersionUID = -1093444616L;

	private String title;
	private List<DataView> dataView;
	private List<DataGroup> groups;
	private RangeVO<Date> creationDate;
	private List<User> creatorUser;
	private RangeVO<Date> modificationDate;
	private List<User> modifierUser;

	// ------------------------------

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public List<DataView> getDataView() {
		return dataView;
	}

	public void setDataView(List<DataView> dataView) {
		this.dataView = dataView;
	}

	public List<DataGroup> getGroups() {
		return groups;
	}

	public void setGroups(List<DataGroup> groups) {
		this.groups = groups;
	}

	public RangeVO<Date> getCreationDate() {
		return creationDate;
	}

	public void setCreationDate(RangeVO<Date> creationDate) {
		this.creationDate = creationDate;
	}

	public List<User> getCreatorUser() {
		return creatorUser;
	}

	public void setCreatorUser(List<User> creatorUser) {
		this.creatorUser = creatorUser;
	}

	public RangeVO<Date> getModificationDate() {
		return modificationDate;
	}

	public void setModificationDate(RangeVO<Date> modificationDate) {
		this.modificationDate = modificationDate;
	}

	public List<User> getModifierUser() {
		return modifierUser;
	}

	public void setModifierUser(List<User> modifierUser) {
		this.modifierUser = modifierUser;
	}

}