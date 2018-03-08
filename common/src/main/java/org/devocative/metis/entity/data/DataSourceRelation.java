package org.devocative.metis.entity.data;

import org.devocative.demeter.entity.*;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import javax.persistence.*;
import java.util.Date;

@Audited
@Entity
@Table(name = "t_mts_data_src_rel",
	uniqueConstraints = {
		@UniqueConstraint(name = "uk_datasrcrel_main", columnNames = {"f_tgt_datasrc", "f_src_datasrc", "c_src_ptr_field"})
	})
public class DataSourceRelation implements ICreationDate, ICreatorUser, IModificationDate, IModifierUser {
	private static final long serialVersionUID = -8316259666720647816L;

	@Id
	@GeneratedValue(generator = "uuid")
	@GenericGenerator(name = "uuid", strategy = "uuid2")
	private String id;

	@Column(name = "c_src_ptr_field", nullable = false)
	private String sourcePointerField;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_src_datasrc", nullable = false, foreignKey = @ForeignKey(name = "datasrcrel_src2datasrc"))
	private DataSource source;

	@Column(name = "f_src_datasrc", nullable = false, insertable = false, updatable = false)
	private String sourceId;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_tgt_datasrc", nullable = false, foreignKey = @ForeignKey(name = "datasrcrel_tgt2datasrc"))
	private DataSource target;

	@Column(name = "f_tgt_datasrc", nullable = false, insertable = false, updatable = false)
	private String targetId;

	// ---------------

	@NotAudited
	@Column(name = "d_creation", nullable = false, columnDefinition = "date")
	private Date creationDate;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_creator_user", insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "datasrcrel_crtrusr2user"))
	private User creatorUser;

	@NotAudited
	@Column(name = "f_creator_user", insertable = false)
	private Long creatorUserId;

	@NotAudited
	@Column(name = "d_modification", columnDefinition = "date")
	private Date modificationDate;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_modifier_user", insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "datasrcrel_mdfrusr2user"))
	private User modifierUser;

	@Column(name = "f_modifier_user")
	private Long modifierUserId;

	@Version
	@Column(name = "n_version", nullable = false)
	private Integer version = 0;

	// ------------------------------

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getSourcePointerField() {
		return sourcePointerField;
	}

	public void setSourcePointerField(String sourcePointerField) {
		this.sourcePointerField = sourcePointerField;
	}

	public DataSource getSource() {
		return source;
	}

	public void setSource(DataSource source) {
		this.source = source;
	}

	public String getSourceId() {
		return sourceId;
	}

	public DataSource getTarget() {
		return target;
	}

	public void setTarget(DataSource target) {
		this.target = target;
	}

	public String getTargetId() {
		return targetId;
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
}
