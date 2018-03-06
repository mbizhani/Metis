package org.devocative.metis.entity.data;

import org.devocative.demeter.entity.*;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import javax.persistence.*;
import java.util.Date;

@Audited
@Entity
@Table(name = "t_mts_data_group", uniqueConstraints = {
	@UniqueConstraint(name = "uk_mts_datagrp_name", columnNames = {"c_name"})
})
public class DataGroup implements ICreationDate, ICreatorUser, IModificationDate, IModifierUser, Comparable<DataGroup> {
	private static final long serialVersionUID = 6455487013206016555L;

	@Id
	@GeneratedValue(generator = "uuid")
	@GenericGenerator(name = "uuid", strategy = "uuid2")
	private String id;

	@Column(name = "c_name", nullable = false)
	private String name;

	// --------------- CREATE / MODIFY

	@NotAudited
	@Column(name = "d_creation", nullable = false, columnDefinition = "date")
	private Date creationDate;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_creator_user", insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "datagrp_crtrusr2user"))
	private User creatorUser;

	@NotAudited
	@Column(name = "f_creator_user")
	private Long creatorUserId;

	@NotAudited
	@Column(name = "d_modification", columnDefinition = "date")
	private Date modificationDate;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_modifier_user", insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "datagrp_mdfrusr2user"))
	private User modifierUser;

	@Column(name = "f_modifier_user")
	private Long modifierUserId;

	@Version
	@Column(name = "n_version", nullable = false)
	private Integer version = 0;

	// ------------------------------ ACCESSORS

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
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
	public String toString() {
		return getName() != null ? getName() : "[?]";
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof DataGroup)) return false;

		DataGroup dataGroup = (DataGroup) o;

		return !(getId() != null ? !getId().equals(dataGroup.getId()) : dataGroup.getId() != null);

	}

	@Override
	public int hashCode() {
		return getId() != null ? getId().hashCode() : 0;
	}

	// ---------------

	@Override
	public int compareTo(DataGroup o) {
		if (getName() != null && o != null && o.getName() != null) {
			return getName().compareTo(o.getName());
		}

		return 0;
	}
}
