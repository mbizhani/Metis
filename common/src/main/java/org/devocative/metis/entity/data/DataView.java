package org.devocative.metis.entity.data;

import org.devocative.demeter.entity.*;
import org.devocative.metis.entity.ConfigLob;
import org.devocative.metis.entity.data.config.XDataView;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import javax.persistence.*;
import java.util.Date;
import java.util.List;

@Audited
@Entity
@Table(name = "t_mts_data_view", uniqueConstraints = {
	@UniqueConstraint(name = "uk_mts_dataview_name", columnNames = {"c_name"})
})
public class DataView implements ICreationDate, ICreatorUser, IModificationDate, IModifierUser {
	private static final long serialVersionUID = 1406191496517488967L;

	@Id
	@GeneratedValue(generator = "uuid")
	@GenericGenerator(name = "uuid", strategy = "uuid2")
	private String id;

	@Column(name = "c_name", nullable = false)
	private String name;

	@Column(name = "c_title", nullable = false)
	private String title;

	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_config", nullable = false, foreignKey = @ForeignKey(name = "dataview2cfglob"))
	private ConfigLob config;

	@Column(name = "f_config", nullable = false, insertable = false, updatable = false)
	private String configId;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_data_src", nullable = false, foreignKey = @ForeignKey(name = "dataview2datasrc"))
	private DataSource dataSource;

	@Column(name = "f_data_src", nullable = false, insertable = false, updatable = false)
	private String dataSourceId;

	//TODO relation to itself for LookUp & Details

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(name = "mt_mts_dataview_group",
		joinColumns = {@JoinColumn(name = "f_data_view")},
		inverseJoinColumns = {@JoinColumn(name = "f_group")},
		foreignKey = @ForeignKey(name = "dataview_group2dataview"),
		inverseForeignKey = @ForeignKey(name = "dataview_group2group"))
	private List<DataGroup> groups;

	// ---------------

	@NotAudited
	@Column(name = "d_creation", nullable = false, columnDefinition = "date")
	private Date creationDate;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_creator_user", insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "dataview_crtrusr2user"))
	private User creatorUser;

	@NotAudited
	@Column(name = "f_creator_user")
	private Long creatorUserId;

	@NotAudited
	@Column(name = "d_modification", columnDefinition = "date")
	private Date modificationDate;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_modifier_user", insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "dataview_mdfrusr2user"))
	private User modifierUser;

	@Column(name = "f_modifier_user")
	private Long modifierUserId;

	@Version
	@Column(name = "n_version", nullable = false)
	private Integer version = 0;

	// ---------------

	@Transient
	private XDataView xDataView;

	// ------------------------------

	public String getId() {
		return id;
	}

	public DataView setId(String id) {
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

	public ConfigLob getConfig() {
		return config;
	}

	public void setConfig(ConfigLob config) {
		this.config = config;
	}

	public String getConfigId() {
		return configId;
	}

	public DataSource getDataSource() {
		return dataSource;
	}

	public void setDataSource(DataSource dataSource) {
		this.dataSource = dataSource;
	}

	public String getDataSourceId() {
		return dataSourceId;
	}

	public List<DataGroup> getGroups() {
		return groups;
	}

	public void setGroups(List<DataGroup> groups) {
		this.groups = groups;
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

	public XDataView getXDataView() {
		return xDataView;
	}

	public void setXDataView(XDataView xDataView) {
		this.xDataView = xDataView;
	}

	// ---------------

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof DataView)) return false;

		DataView dataView = (DataView) o;

		return
			!(getId() != null ? !getId().equals(dataView.getId()) : dataView.getId() != null) ||
				!(getName() != null ? !getName().equals(dataView.getName()) : dataView.getName() != null);

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
