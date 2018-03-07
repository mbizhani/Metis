//overwrite
package org.devocative.metis.vo.filter.connection;

import org.devocative.adroit.vo.RangeVO;
import org.devocative.demeter.entity.User;
import org.devocative.demeter.iservice.persistor.Filterer;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.connection.EAliasMode;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

@Filterer
public class DBConnectionAliasFVO implements Serializable {
	private static final long serialVersionUID = -30326038L;

	private String name;
	private List<EAliasMode> mode;
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

	public List<EAliasMode> getMode() {
		return mode;
	}

	public void setMode(List<EAliasMode> mode) {
		this.mode = mode;
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