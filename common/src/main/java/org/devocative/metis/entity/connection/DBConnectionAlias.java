package org.devocative.metis.entity.connection;

import org.devocative.demeter.entity.*;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import javax.persistence.*;
import java.util.Date;

@Audited
@Entity
@Table(name = "t_mts_db_conn_alias", uniqueConstraints = {
	@UniqueConstraint(name = DBConnectionAlias.UQ_CONST, columnNames = {"c_name", "e_mode"})
})
public class DBConnectionAlias implements ICreationDate, ICreatorUser, IModificationDate, IModifierUser {
	public static final String UQ_CONST = "uk_mts_dbConnAlias_all";

	private static final long serialVersionUID = 6632811275596865233L;

	@Id
	@GeneratedValue(generator = "mts_db_conn_alias")
	@org.hibernate.annotations.GenericGenerator(name = "mts_db_conn_alias", strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
		parameters = {
			//@org.hibernate.annotations.Parameter(name = "optimizer", value = "pooled"),
			@org.hibernate.annotations.Parameter(name = "initial_value", value = "1"),
			@org.hibernate.annotations.Parameter(name = "increment_size", value = "1"),
			@org.hibernate.annotations.Parameter(name = "sequence_name", value = "mts_db_conn_alias")
		})
	private Long id;

	@Column(name = "c_name", nullable = false)
	private String name;

	@Column(name = "e_mode", nullable = false)
	@Convert(converter = EAliasMode.Converter.class)
	private EAliasMode mode = EAliasMode.NORMAL;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_connection", nullable = false, foreignKey = @ForeignKey(name = "alias2connection"))
	private DBConnection connection;

	@Column(name = "f_connection", nullable = false, insertable = false, updatable = false)
	private Long connectionId;

	// ---------------

	@NotAudited
	@Column(name = "d_creation", nullable = false, columnDefinition = "date")
	private Date creationDate;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_creator_user", nullable = false, insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "dbconnalias_crtrusr2user"))
	private User creatorUser;

	@NotAudited
	@Column(name = "f_creator_user", nullable = false)
	private Long creatorUserId;

	@NotAudited
	@Column(name = "d_modification", columnDefinition = "date")
	private Date modificationDate;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_modifier_user", insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "dbconnalias_mdfrusr2user"))
	private User modifierUser;

	@Column(name = "f_modifier_user")
	private Long modifierUserId;

	@Version
	@Column(name = "n_version", nullable = false)
	private Integer version = 0;

	// ------------------------------

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public EAliasMode getMode() {
		return mode;
	}

	public void setMode(EAliasMode mode) {
		this.mode = mode;
	}

	public DBConnection getConnection() {
		return connection;
	}

	public void setConnection(DBConnection connection) {
		this.connection = connection;
	}

	public Long getConnectionId() {
		return connectionId;
	}

	// ---------------

	@Override
	public Date getCreationDate() {
		return creationDate;
	}

	@Override
	public void setCreationDate(Date creationDate) {
		this.creationDate = creationDate;
	}

	public User getCreatorUser() {
		return creatorUser;
	}

	public void setCreatorUser(User creatorUser) {
		this.creatorUser = creatorUser;
	}

	@Override
	public Long getCreatorUserId() {
		return creatorUserId;
	}

	@Override
	public void setCreatorUserId(Long creatorUserId) {
		this.creatorUserId = creatorUserId;
	}

	@Override
	public Date getModificationDate() {
		return modificationDate;
	}

	@Override
	public void setModificationDate(Date modificationDate) {
		this.modificationDate = modificationDate;
	}

	public User getModifierUser() {
		return modifierUser;
	}

	public void setModifierUser(User modifierUser) {
		this.modifierUser = modifierUser;
	}

	@Override
	public Long getModifierUserId() {
		return modifierUserId;
	}

	@Override
	public void setModifierUserId(Long modifierUserId) {
		this.modifierUserId = modifierUserId;
	}

	@Override
	public Integer getVersion() {
		return version;
	}

	@Override
	public void setVersion(Integer version) {
		this.version = version;
	}

	// ---------------

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof DBConnectionAlias)) return false;

		DBConnectionAlias that = (DBConnectionAlias) o;

		return !(getId() != null ? !getId().equals(that.getId()) : that.getId() != null);

	}

	@Override
	public int hashCode() {
		return getId() != null ? getId().hashCode() : 0;
	}

	@Override
	public String toString() {
		return getName();
	}
}
