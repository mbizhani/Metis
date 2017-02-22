package org.devocative.metis.entity.data;

import org.devocative.demeter.entity.*;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.data.config.XDataSource;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import javax.persistence.*;
import java.util.Date;

@Audited
@Entity
@Table(name = "t_mts_data_src", uniqueConstraints = {
	@UniqueConstraint(name = "uk_mts_datasrc_name", columnNames = {"c_name"})
})
public class DataSource implements ICreationDate, ICreatorUser, IModificationDate, IModifierUser {
	private static final long serialVersionUID = -1352772492330821846L;

	@Id
	@GeneratedValue(generator = "mts_data_src")
	@org.hibernate.annotations.GenericGenerator(name = "mts_data_src", strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
		parameters = {
			//@org.hibernate.annotations.Parameter(name = "optimizer", value = "pooled"),
			@org.hibernate.annotations.Parameter(name = "initial_value", value = "1"),
			@org.hibernate.annotations.Parameter(name = "increment_size", value = "1"),
			@org.hibernate.annotations.Parameter(name = "sequence_name", value = "mts_data_src")
		})
	private Long id;

	@Column(name = "c_name", nullable = false)
	private String name;

	@Column(name = "c_title", nullable = false)
	private String title;

	@Column(name = "c_key_field")
	private String keyField;

	@Column(name = "c_title_field")
	private String titleField;

	@Column(name = "c_self_rel_pointer_field")
	private String selfRelPointerField;

	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_config", foreignKey = @ForeignKey(name = "datasrc2cfglob"))
	private ConfigLob config;

	@Column(name = "f_config", insertable = false, updatable = false)
	private Long configId;

	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "f_connection", foreignKey = @ForeignKey(name = "datasrc2dbconn"))
	private DBConnection connection;

	@Column(name = "f_connection", insertable = false, updatable = false)
	private Long connectionId;

	// --------------- CREATE / MODIFY

	@NotAudited
	@Column(name = "d_creation", nullable = false, columnDefinition = "date")
	private Date creationDate;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_creator_user", insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "datasrc_crtrusr2user"))
	private User creatorUser;

	@NotAudited
	@Column(name = "f_creator_user")
	private Long creatorUserId;

	@NotAudited
	@Column(name = "d_modification", columnDefinition = "date")
	private Date modificationDate;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_modifier_user", insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "datasrc_mdfrusr2user"))
	private User modifierUser;

	@Column(name = "f_modifier_user")
	private Long modifierUserId;

	@Version
	@Column(name = "n_version", nullable = false)
	private Integer version = 0;

	// ---------------

	@Transient
	private XDataSource xDataSource;

	// ----------------------------- CONSTRUCTORS

	public DataSource() {
	}

	public DataSource(Long id) {
		this.id = id;
	}

	public DataSource(String name) {
		this.name = name;
	}

	// ----------------------------- ACCESSORS

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

	public ConfigLob getConfig() {
		return config;
	}

	public void setConfig(ConfigLob config) {
		this.config = config;
	}

	public Long getConfigId() {
		return configId;
	}

	public DBConnection getConnection() {
		return connection;
	}

	public void setConnection(DBConnection connection) {
		this.connection = connection;
		if (connection != null) {
			this.connectionId = connection.getId();
		}
	}

	public Long getConnectionId() {
		return connectionId;
	}

	// --------------- CREATE / MODIFY ACCESSORS

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

	public XDataSource getXDataSource() {
		return xDataSource;
	}

	public void setXDataSource(XDataSource xDataSource) {
		this.xDataSource = xDataSource;
	}

	// --------------- OBJECT METHODS

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof DataSource)) return false;

		DataSource that = (DataSource) o;

		return
			!(getId() != null ? !getId().equals(that.getId()) : that.getId() != null) ||
				!(getName() != null ? !getName().equals(that.getName()) : that.getName() != null);

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
