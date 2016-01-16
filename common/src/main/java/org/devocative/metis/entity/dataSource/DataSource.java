package org.devocative.metis.entity.dataSource;

import org.devocative.demeter.entity.*;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.DBConnection;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import javax.persistence.*;
import java.util.Date;

@Audited
@Entity
@Table(name = "t_mts_data_src_info")
public class DataSource implements ICreationDate, ICreatorUser, IModificationDate, IModifierUser {
	@Id
	@GeneratedValue(generator = "mts_data_src_info")
	@org.hibernate.annotations.GenericGenerator(name = "mts_data_src_info", strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
		parameters = {
			//@org.hibernate.annotations.Parameter(name = "optimizer", value = "pooled"),
			@org.hibernate.annotations.Parameter(name = "initial_value", value = "1"),
			@org.hibernate.annotations.Parameter(name = "increment_size", value = "1"),
			@org.hibernate.annotations.Parameter(name = "sequence_name", value = "mts_data_src_info")
		})
	private Long id;

	@Column(name = "c_name", nullable = false, unique = true)
	private String name;

	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_config", foreignKey = @ForeignKey(name = "datasrc2cfglob"))
	private ConfigLob config;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_conn_info", foreignKey = @ForeignKey(name = "datasrc2dbconn"))
	private DBConnection connectionInfo;

	@Column(name = "f_conn_info", insertable = false, updatable = false)
	private Long connectionInfoId;

	//----------------------------- CREATE / MODIFY

	@NotAudited
	@Column(name = "d_creation", nullable = false, columnDefinition = "date")
	private Date creationDate;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_creator_user", foreignKey = @ForeignKey(name = "prsn_crtrusr2user"), insertable = false, updatable = false)
	private User creatorUser;

	@NotAudited
	@Column(name = "f_creator_user")
	private Long creatorUserId;

	@NotAudited
	@Column(name = "d_modification", columnDefinition = "date")
	private Date modificationDate;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_modifier_user", foreignKey = @ForeignKey(name = "prsn_mdfrusr2user"), insertable = false, updatable = false)
	private User modifierUser;

	@Column(name = "f_modifier_user")
	private Long modifierUserId;

	@Version
	@Column(name = "n_version", nullable = false)
	private Integer version = 0;

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

	public ConfigLob getConfig() {
		return config;
	}

	public void setConfig(ConfigLob config) {
		this.config = config;
	}

	public DBConnection getConnectionInfo() {
		return connectionInfo;
	}

	public void setConnectionInfo(DBConnection connectionInfo) {
		this.connectionInfo = connectionInfo;
	}

	public Long getConnectionInfoId() {
		return connectionInfoId;
	}

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

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof DataSource)) return false;

		DataSource that = (DataSource) o;

		return !(getId() != null ? !getId().equals(that.getId()) : that.getId() != null);

	}

	@Override
	public int hashCode() {
		return getId() != null ? getId().hashCode() : 0;
	}

	@Override
	public String toString() {
		return getName() != null ? getName() : "[?]";
	}
}
