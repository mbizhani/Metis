package org.devocative.metis.entity;

import org.devocative.demeter.entity.ICreationDate;
import org.devocative.demeter.entity.IModificationDate;
import org.devocative.demeter.entity.User;
import org.devocative.metis.entity.connection.DBConnection;

import javax.persistence.*;
import java.util.Date;

@Entity
@Table(name = "t_mts_usr_prf")
public class MetisUserProfile implements ICreationDate, IModificationDate {
	private static final long serialVersionUID = 4805397319500384541L;

	@Id
	private Long id;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "id", insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "mtsusrprf2user"))
	private User user;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_dflt_conn", foreignKey = @ForeignKey(name = "mtsusrprf2dbconn"))
	private DBConnection defaultConnection;

	@Column(name = "f_dflt_conn", insertable = false, updatable = false)
	private Long defaultConnectionId;

	// ------------------------------ CREATE / MODIFY

	@Column(name = "d_creation", nullable = false, columnDefinition = "date")
	private Date creationDate;

	@Column(name = "d_modification", columnDefinition = "date")
	private Date modificationDate;

	@Version
	@Column(name = "n_version", nullable = false)
	private Integer version = 0;

	// ------------------------------ CONSTRUCTORS

	public MetisUserProfile() {
	}

	public MetisUserProfile(Long id) {
		this.id = id;
	}

	// ------------------------------ ACCESSORS

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public User getUser() {
		return user;
	}

	public DBConnection getDefaultConnection() {
		return defaultConnection;
	}

	public void setDefaultConnection(DBConnection defaultConnection) {
		this.defaultConnection = defaultConnection;
	}

	public Long getDefaultConnectionId() {
		return defaultConnectionId;
	}

	@Override
	public Date getCreationDate() {
		return creationDate;
	}

	@Override
	public void setCreationDate(Date creationDate) {
		this.creationDate = creationDate;
	}

	@Override
	public Date getModificationDate() {
		return modificationDate;
	}

	@Override
	public void setModificationDate(Date modificationDate) {
		this.modificationDate = modificationDate;
	}

	@Override
	public Integer getVersion() {
		return version;
	}

	@Override
	public void setVersion(Integer version) {
		this.version = version;
	}
}
