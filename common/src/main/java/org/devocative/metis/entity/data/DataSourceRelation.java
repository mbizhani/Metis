package org.devocative.metis.entity.data;

import org.devocative.demeter.entity.*;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import javax.persistence.*;
import java.util.Date;

@Audited
@Entity
@Table(name = "t_mts_data_src_rel",
	uniqueConstraints = {
		@UniqueConstraint(name = "datasrc_main", columnNames = {"c_src_ptr_field", "f_src_datasrc"})
	})
public class DataSourceRelation implements ICreationDate, ICreatorUser, IModificationDate, IModifierUser {
	private static final long serialVersionUID = -8316259666720647816L;

	@Id
	@GeneratedValue(generator = "mts_data_src_rel")
	@org.hibernate.annotations.GenericGenerator(name = "mts_data_src_rel", strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
		parameters = {
			//@org.hibernate.annotations.Parameter(name = "optimizer", value = "pooled"),
			@org.hibernate.annotations.Parameter(name = "initial_value", value = "1"),
			@org.hibernate.annotations.Parameter(name = "increment_size", value = "1"),
			@org.hibernate.annotations.Parameter(name = "sequence_name", value = "mts_data_src_rel")
		})
	private Long id;

	@Column(name = "c_src_ptr_field", nullable = false)
	private String sourcePointerField;

	@Column(name = "c_tgt_key_field")
	private String targetKeyField;

	@Column(name = "c_tgt_title_field")
	private String targetTitleField;

	@Column(name = "b_deleted", nullable = false)
	private Boolean deleted = false;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_src_datasrc", nullable = false, foreignKey = @ForeignKey(name = "datasrcrel_src2datasrc"))
	private DataSource source;

	@Column(name = "f_src_datasrc", insertable = false, updatable = false)
	private Long sourceId;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_tgt_datasrc", nullable = false, foreignKey = @ForeignKey(name = "datasrcrel_tgt2datasrc"))
	private DataSource target;

	@Column(name = "f_tgt_datasrc", nullable = false, insertable = false, updatable = false)
	private Long targetId;

	//----------------------------- CREATE / MODIFY

	@NotAudited
	@Column(name = "d_creation", nullable = false, columnDefinition = "date")
	private Date creationDate;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_creator_user", insertable = false, updatable = false,
		foreignKey = @ForeignKey(name = "datasrcrel_crtrusr2user"))
	private User creatorUser;

	@NotAudited
	@Column(name = "f_creator_user")
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


	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getSourcePointerField() {
		return sourcePointerField;
	}

	public void setSourcePointerField(String sourcePointerField) {
		this.sourcePointerField = sourcePointerField;
	}

	public String getTargetKeyField() {
		return targetKeyField;
	}

	public void setTargetKeyField(String targetKeyField) {
		this.targetKeyField = targetKeyField;
	}

	public String getTargetTitleField() {
		return targetTitleField;
	}

	public void setTargetTitleField(String targetTitleField) {
		this.targetTitleField = targetTitleField;
	}

	public Boolean getDeleted() {
		return deleted;
	}

	public void setDeleted(Boolean deleted) {
		this.deleted = deleted;
	}

	public DataSource getSource() {
		return source;
	}

	public void setSource(DataSource source) {
		this.source = source;
	}

	public Long getSourceId() {
		return sourceId;
	}

	public void setSourceId(Long sourceId) {
		this.sourceId = sourceId;
	}

	public DataSource getTarget() {
		return target;
	}

	public void setTarget(DataSource target) {
		this.target = target;
	}

	public Long getTargetId() {
		return targetId;
	}

	public void setTargetId(Long targetId) {
		this.targetId = targetId;
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
