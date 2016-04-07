package org.devocative.metis.entity.data;

import org.devocative.demeter.entity.*;
import org.devocative.metis.entity.ConfigLob;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import javax.persistence.*;
import java.util.Date;
import java.util.List;

@Audited
@Entity
@Table(name = "t_mts_data_view")
public class DataView implements ICreationDate, ICreatorUser, IModificationDate, IModifierUser {
	@Id
	@GeneratedValue(generator = "mts_data_view")
	@org.hibernate.annotations.GenericGenerator(name = "mts_data_view", strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
		parameters = {
			//@org.hibernate.annotations.Parameter(name = "optimizer", value = "pooled"),
			@org.hibernate.annotations.Parameter(name = "initial_value", value = "1"),
			@org.hibernate.annotations.Parameter(name = "increment_size", value = "1"),
			@org.hibernate.annotations.Parameter(name = "sequence_name", value = "mts_data_view")
		})
	private Long id;

	@Column(name = "c_name", nullable = false, unique = true)
	private String name;

	@Column(name = "c_title", nullable = false)
	private String title;

	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_config", foreignKey = @ForeignKey(name = "dataview2cfglob"))
	private ConfigLob config;

	@Column(name = "f_config", insertable = false, updatable = false)
	private Long configId;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(
		name = "mt_mts_data_view2src",
		joinColumns = {@JoinColumn(name = "f_data_view")},
		foreignKey = @ForeignKey(name = "mtdatasrcviw2view"),
		inverseJoinColumns = {@JoinColumn(name = "f_data_src")},
		inverseForeignKey = @ForeignKey(name = "mtdatasrcviw2src"))
	private List<DataSource> dataSources;

	// ----------------------------- CREATE / MODIFY

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

	// ----------------------------- CONSTRUCTORS


	public DataView() {
	}

	public DataView(String name) {
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

	public ConfigLob getConfig() {
		return config;
	}

	public void setConfig(ConfigLob config) {
		this.config = config;
	}

	public Long getConfigId() {
		return configId;
	}

	public List<DataSource> getDataSources() {
		return dataSources;
	}

	public void setDataSources(List<DataSource> dataSources) {
		this.dataSources = dataSources;
	}

	// ----------------------------- CREATE / MODIFY ACCESSORS

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

	// ----------------------------- OBJECT METHODS

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
