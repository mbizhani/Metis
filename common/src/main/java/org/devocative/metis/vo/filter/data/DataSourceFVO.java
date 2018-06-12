//overwrite
package org.devocative.metis.vo.filter.data;

import org.devocative.adroit.vo.RangeVO;
import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.persistor.Filterer;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.data.EConnectionSelection;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

@Filterer
public class DataSourceFVO implements Serializable {
	private static final long serialVersionUID = -426222425L;

	private String name;
	private String title;
	private String keyField;
	private String titleField;
	private String selfRelPointerField;
	private List<EConnectionSelection> connectionSelection;
	private List<DBConnection> connection;
	private RangeVO<Date> creationDate;
	private List<User> creatorUser;
	private RangeVO<Date> modificationDate;
	private List<User> modifierUser;

	// ------------------------------

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getKeyField() {
		return keyField;
	}

	public void setKeyField(String keyField) {
		this.keyField = keyField;
	}

	public String getTitleField() {
		return titleField;
	}

	public void setTitleField(String titleField) {
		this.titleField = titleField;
	}

	public String getSelfRelPointerField() {
		return selfRelPointerField;
	}

	public void setSelfRelPointerField(String selfRelPointerField) {
		this.selfRelPointerField = selfRelPointerField;
	}

	public List<EConnectionSelection> getConnectionSelection() {
		return connectionSelection;
	}

	public void setConnectionSelection(List<EConnectionSelection> connectionSelection) {
		this.connectionSelection = connectionSelection;
	}

	public List<DBConnection> getConnection() {
		return connection;
	}

	public void setConnection(List<DBConnection> connection) {
		this.connection = connection;
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
