package org.devocative.metis.entity.data;

import org.devocative.demeter.entity.*;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.connection.DBConnection;
import org.devocative.metis.entity.data.config.XDataSource;
import org.hibernate.annotations.GenericGenerator;
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
	@GeneratedValue(generator = "uuid")
	@GenericGenerator(name = "uuid", strategy = "uuid2")
	private String id;

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

	@Column(name = "e_conn_selection", nullable = false)
	@Convert(converter = EConnectionSelection.Converter.class)
	private EConnectionSelection connectionSelection = EConnectionSelection.THREE_STEPS;

	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_config", nullable = false, foreignKey = @ForeignKey(name = "datasrc2cfglob"))
	private ConfigLob config;

	@Column(name = "f_config", nullable = false, insertable = false, updatable = false)
	private String configId;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_connection", nullable = false, foreignKey = @ForeignKey(name = "datasrc2dbconn"))
	private DBConnection connection;

	@Column(name = "f_connection", nullable = false, insertable = false, updatable = false)
	private Long connectionId;

	// --------------- CREATE / MODIFY

	@NotAudited
	@Column(name = "d_creation", nullable = false, columnDefinition = "date")
	private Date creationDate;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_creator_user", nullable = false, insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "datasrc_crtrusr2user"))
	private User creatorUser;

	@NotAudited
	@Column(name = "f_creator_user", nullable = false)
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

	// ------------------------------ ACCESSORS

	public String getId() {
		return id;
	}

	public DataSource setId(String id) {
		this.id = id;
		return this;
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

	public EConnectionSelection getConnectionSelection() {
		return connectionSelection;
	}

	public void setConnectionSelection(EConnectionSelection connectionSelection) {
		this.connectionSelection = connectionSelection;
	}

	public ConfigLob getConfig() {
		return config;
	}

	public void setConfig(ConfigLob config) {
		this.config = config;
	}

	public String getConfigId() {
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
