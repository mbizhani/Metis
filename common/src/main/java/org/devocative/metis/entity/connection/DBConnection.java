package org.devocative.metis.entity.connection;

import org.devocative.demeter.entity.*;
import org.devocative.metis.entity.ConfigLob;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import javax.persistence.*;
import java.util.Date;

@Audited
@Entity
@Table(name = "t_mts_db_conn", uniqueConstraints = {
	@UniqueConstraint(name = DBConnection.UQ_CONST, columnNames = {"c_name"})
})
public class DBConnection implements ICreationDate, ICreatorUser, IModificationDate, IModifierUser {
	public static final String UQ_CONST = "uk_mts_dbConn_name";

	private static final long serialVersionUID = 3191943462582109397L;

	@Id
	@GeneratedValue(generator = "mts_db_conn")
	@org.hibernate.annotations.GenericGenerator(name = "mts_db_conn", strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
		parameters = {
			//@org.hibernate.annotations.Parameter(name = "optimizer", value = "pooled"),
			@org.hibernate.annotations.Parameter(name = "initial_value", value = "1"),
			@org.hibernate.annotations.Parameter(name = "increment_size", value = "1"),
			@org.hibernate.annotations.Parameter(name = "sequence_name", value = "mts_db_conn")
		})
	private Long id;

	@Column(name = "c_name", nullable = false)
	private String name;

	@Column(name = "c_driver")
	private String driver;

	@Column(name = "c_url")
	private String url;

	@Column(name = "c_username", nullable = false)
	private String username;

	@Column(name = "c_password")
	private String password;

	@Column(name = "c_schema")
	private String schema;

	@Column(name = "c_test_query")
	private String testQuery;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_group", nullable = false, foreignKey = @ForeignKey(name = "dbconn2group"))
	private DBConnectionGroup group;

	@Column(name = "f_group", insertable = false, updatable = false)
	private String groupId;

	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_config", foreignKey = @ForeignKey(name = "dbconn2cfglob"))
	private ConfigLob config;

	@Column(name = "f_config", insertable = false, updatable = false)
	private String configId;

	@Column(name = "c_custom_param_1")
	private String customParam1;

	// ---------------

	@NotAudited
	@Column(name = "d_creation", nullable = false, columnDefinition = "date")
	private Date creationDate;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_creator_user", insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "dbconn_crtrusr2user"))
	private User creatorUser;

	@NotAudited
	@Column(name = "f_creator_user")
	private Long creatorUserId;

	@NotAudited
	@Column(name = "d_modification", columnDefinition = "date")
	private Date modificationDate;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_modifier_user", insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "dbconn_mdfrusr2user"))
	private User modifierUser;

	@Column(name = "f_modifier_user")
	private Long modifierUserId;

	@Version
	@Column(name = "n_version", nullable = false)
	private Integer version = 0;

	// ------------------------------

	public DBConnection() {
	}

	public DBConnection(Long id) {
		this.id = id;
	}

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

	public String getDriver() {
		return driver;
	}

	public void setDriver(String driver) {
		this.driver = driver;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getSchema() {
		return schema;
	}

	public void setSchema(String schema) {
		this.schema = schema;
	}

	public String getTestQuery() {
		return testQuery;
	}

	public void setTestQuery(String testQuery) {
		this.testQuery = testQuery;
	}

	public DBConnectionGroup getGroup() {
		return group;
	}

	public void setGroup(DBConnectionGroup group) {
		this.group = group;
		if (group != null) {
			this.groupId = group.getId();
		}
	}

	public String getGroupId() {
		return groupId;
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

	public String getCustomParam1() {
		return customParam1;
	}

	public void setCustomParam1(String customParam1) {
		this.customParam1 = customParam1;
	}

	// ---------------

	public DBConnectionGroup getSafeGroup() {
		return getGroup() != null ? getGroup() : new DBConnectionGroup();
	}

	public String getSafeDriver() {
		return getDriver() != null ? getDriver() : getSafeGroup().getDriver();
	}

	public String getSafeUrl() {
		return getUrl() != null ? getUrl() : getSafeGroup().getUrl();
	}

	public String getSafeTestQuery() {
		return getTestQuery() != null ? getTestQuery() : getSafeGroup().getTestQuery();
	}

	public ConfigLob getSafeConfig() {
		return getConfig() != null ? getConfig() : getSafeGroup().getConfig();
	}

	public String getSafeConfigId() {
		return getConfigId() != null ? getConfigId() : getSafeGroup().getConfigId();
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

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof DBConnection)) return false;

		DBConnection that = (DBConnection) o;

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
