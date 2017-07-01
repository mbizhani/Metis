-----------------------
-- CREATE AUDIT TABLES
-----------------------

CREATE TABLE a_mt_mts_dataview_group (
	r_num       NUMBER(10, 0) NOT NULL,
	f_data_view NUMBER(19, 0) NOT NULL,
	f_group     NUMBER(19, 0) NOT NULL,
	r_type      NUMBER(3, 0),
	PRIMARY KEY (r_num, f_data_view, f_group)
);

CREATE TABLE a_mt_mts_report_group (
	r_num    NUMBER(10, 0) NOT NULL,
	f_report NUMBER(19, 0) NOT NULL,
	f_group  NUMBER(19, 0) NOT NULL,
	r_type   NUMBER(3, 0),
	PRIMARY KEY (r_num, f_report, f_group)
);

CREATE TABLE a_t_mts_cfg_lob (
	id              NUMBER(19, 0) NOT NULL,
	r_num           NUMBER(10, 0) NOT NULL,
	r_type          NUMBER(3, 0),
	f_modifier_user NUMBER(19, 0),
	c_value         CLOB,
	PRIMARY KEY (id, r_num)
);

CREATE TABLE a_t_mts_data_group (
	id              NUMBER(19, 0) NOT NULL,
	r_num           NUMBER(10, 0) NOT NULL,
	r_type          NUMBER(3, 0),
	f_modifier_user NUMBER(19, 0),
	c_name          VARCHAR2(255 CHAR),
	PRIMARY KEY (id, r_num)
);

CREATE TABLE a_t_mts_data_src (
	id                       NUMBER(19, 0) NOT NULL,
	r_num                    NUMBER(10, 0) NOT NULL,
	r_type                   NUMBER(3, 0),
	f_config                 NUMBER(19, 0),
	f_connection             NUMBER(19, 0),
	c_key_field              VARCHAR2(255 CHAR),
	f_modifier_user          NUMBER(19, 0),
	c_name                   VARCHAR2(255 CHAR),
	c_self_rel_pointer_field VARCHAR2(255 CHAR),
	c_title                  VARCHAR2(255 CHAR),
	c_title_field            VARCHAR2(255 CHAR),
	PRIMARY KEY (id, r_num)
);

CREATE TABLE a_t_mts_data_src_rel (
	id                NUMBER(19, 0) NOT NULL,
	r_num             NUMBER(10, 0) NOT NULL,
	r_type            NUMBER(3, 0),
	b_deleted         NUMBER(1, 0),
	f_modifier_user   NUMBER(19, 0),
	f_src_datasrc     NUMBER(19, 0),
	c_src_ptr_field   VARCHAR2(255 CHAR),
	f_tgt_datasrc     NUMBER(19, 0),
	c_tgt_key_field   VARCHAR2(255 CHAR),
	c_tgt_title_field VARCHAR2(255 CHAR),
	PRIMARY KEY (id, r_num)
);

CREATE TABLE a_t_mts_data_view (
	id              NUMBER(19, 0) NOT NULL,
	r_num           NUMBER(10, 0) NOT NULL,
	r_type          NUMBER(3, 0),
	f_config        NUMBER(19, 0),
	f_data_src      NUMBER(19, 0),
	f_modifier_user NUMBER(19, 0),
	c_name          VARCHAR2(255 CHAR),
	c_title         VARCHAR2(255 CHAR),
	PRIMARY KEY (id, r_num)
);

CREATE TABLE a_t_mts_db_conn (
	id              NUMBER(19, 0) NOT NULL,
	r_num           NUMBER(10, 0) NOT NULL,
	r_type          NUMBER(3, 0),
	f_config        NUMBER(19, 0),
	c_driver        VARCHAR2(255 CHAR),
	f_group         NUMBER(19, 0),
	f_modifier_user NUMBER(19, 0),
	c_name          VARCHAR2(255 CHAR),
	c_password      VARCHAR2(255 CHAR),
	c_schema        VARCHAR2(255 CHAR),
	c_test_query    VARCHAR2(255 CHAR),
	c_url           VARCHAR2(255 CHAR),
	c_username      VARCHAR2(255 CHAR),
	PRIMARY KEY (id, r_num)
);

CREATE TABLE a_t_mts_db_conn_grp (
	id              NUMBER(19, 0) NOT NULL,
	r_num           NUMBER(10, 0) NOT NULL,
	r_type          NUMBER(3, 0),
	f_config        NUMBER(19, 0),
	c_driver        VARCHAR2(255 CHAR),
	f_modifier_user NUMBER(19, 0),
	c_name          VARCHAR2(255 CHAR),
	c_test_query    VARCHAR2(255 CHAR),
	c_url           VARCHAR2(255 CHAR),
	PRIMARY KEY (id, r_num)
);

CREATE TABLE a_t_mts_report (
	id              NUMBER(19, 0) NOT NULL,
	r_num           NUMBER(10, 0) NOT NULL,
	r_type          NUMBER(3, 0),
	c_config        VARCHAR2(1000 CHAR),
	f_data_view     NUMBER(19, 0),
	f_modifier_user NUMBER(19, 0),
	c_title         VARCHAR2(255 CHAR),
	PRIMARY KEY (id, r_num)
);

------------------------
-- CREATE MIDDLE TABLES
------------------------

CREATE TABLE mt_mts_dataview_group (
	f_data_view NUMBER(19, 0) NOT NULL,
	f_group     NUMBER(19, 0) NOT NULL
);

CREATE TABLE mt_mts_report_group (
	f_report NUMBER(19, 0) NOT NULL,
	f_group  NUMBER(19, 0) NOT NULL
);

----------------------
-- CREATE MAIN TABLES
----------------------

CREATE TABLE t_mts_cfg_lob (
	id              NUMBER(19, 0) NOT NULL,
	d_creation      DATE          NOT NULL,
	f_creator_user  NUMBER(19, 0),
	d_modification  DATE,
	f_modifier_user NUMBER(19, 0),
	c_value         CLOB          NOT NULL,
	n_version       NUMBER(10, 0) NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE t_mts_data_group (
	id              NUMBER(19, 0)      NOT NULL,
	d_creation      DATE               NOT NULL,
	f_creator_user  NUMBER(19, 0),
	d_modification  DATE,
	f_modifier_user NUMBER(19, 0),
	c_name          VARCHAR2(255 CHAR) NOT NULL,
	n_version       NUMBER(10, 0)      NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE t_mts_data_src (
	id                       NUMBER(19, 0)      NOT NULL,
	f_config                 NUMBER(19, 0),
	f_connection             NUMBER(19, 0),
	d_creation               DATE               NOT NULL,
	f_creator_user           NUMBER(19, 0),
	c_key_field              VARCHAR2(255 CHAR),
	d_modification           DATE,
	f_modifier_user          NUMBER(19, 0),
	c_name                   VARCHAR2(255 CHAR) NOT NULL,
	c_self_rel_pointer_field VARCHAR2(255 CHAR),
	c_title                  VARCHAR2(255 CHAR) NOT NULL,
	c_title_field            VARCHAR2(255 CHAR),
	n_version                NUMBER(10, 0)      NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE t_mts_data_src_rel (
	id                NUMBER(19, 0)      NOT NULL,
	d_creation        DATE               NOT NULL,
	f_creator_user    NUMBER(19, 0),
	b_deleted         NUMBER(1, 0)       NOT NULL,
	d_modification    DATE,
	f_modifier_user   NUMBER(19, 0),
	f_src_datasrc     NUMBER(19, 0),
	c_src_ptr_field   VARCHAR2(255 CHAR) NOT NULL,
	f_tgt_datasrc     NUMBER(19, 0),
	c_tgt_key_field   VARCHAR2(255 CHAR),
	c_tgt_title_field VARCHAR2(255 CHAR),
	n_version         NUMBER(10, 0)      NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE t_mts_data_view (
	id              NUMBER(19, 0)      NOT NULL,
	f_config        NUMBER(19, 0),
	d_creation      DATE               NOT NULL,
	f_creator_user  NUMBER(19, 0),
	f_data_src      NUMBER(19, 0),
	d_modification  DATE,
	f_modifier_user NUMBER(19, 0),
	c_name          VARCHAR2(255 CHAR) NOT NULL,
	c_title         VARCHAR2(255 CHAR) NOT NULL,
	n_version       NUMBER(10, 0)      NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE t_mts_db_conn (
	id              NUMBER(19, 0)      NOT NULL,
	f_config        NUMBER(19, 0),
	d_creation      DATE               NOT NULL,
	f_creator_user  NUMBER(19, 0),
	c_driver        VARCHAR2(255 CHAR),
	f_group         NUMBER(19, 0),
	d_modification  DATE,
	f_modifier_user NUMBER(19, 0),
	c_name          VARCHAR2(255 CHAR) NOT NULL,
	c_password      VARCHAR2(255 CHAR),
	c_schema        VARCHAR2(255 CHAR),
	c_test_query    VARCHAR2(255 CHAR),
	c_url           VARCHAR2(255 CHAR),
	c_username      VARCHAR2(255 CHAR) NOT NULL,
	n_version       NUMBER(10, 0)      NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE t_mts_db_conn_grp (
	id              NUMBER(19, 0)      NOT NULL,
	f_config        NUMBER(19, 0),
	d_creation      DATE               NOT NULL,
	f_creator_user  NUMBER(19, 0),
	c_driver        VARCHAR2(255 CHAR),
	d_modification  DATE,
	f_modifier_user NUMBER(19, 0),
	c_name          VARCHAR2(255 CHAR) NOT NULL,
	c_test_query    VARCHAR2(255 CHAR),
	c_url           VARCHAR2(255 CHAR),
	n_version       NUMBER(10, 0)      NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE t_mts_report (
	id              NUMBER(19, 0)      NOT NULL,
	c_config        VARCHAR2(1000 CHAR),
	d_creation      DATE               NOT NULL,
	f_creator_user  NUMBER(19, 0),
	f_data_view     NUMBER(19, 0),
	d_modification  DATE,
	f_modifier_user NUMBER(19, 0),
	c_title         VARCHAR2(255 CHAR) NOT NULL,
	n_version       NUMBER(10, 0)      NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE t_mts_usr_prf (
	id             NUMBER(19, 0) NOT NULL,
	d_creation     DATE          NOT NULL,
	f_dflt_conn    NUMBER(19, 0),
	d_modification DATE,
	n_version      NUMBER(10, 0) NOT NULL,
	PRIMARY KEY (id)
);

-----------------------------
-- CREATE UNIQUE CONSTRAINTS
-----------------------------

ALTER TABLE t_mts_data_group
ADD CONSTRAINT uk_mts_datagrp_name UNIQUE (c_name);

ALTER TABLE t_mts_data_src
ADD CONSTRAINT uk_mts_datasrc_name UNIQUE (c_name);

ALTER TABLE t_mts_data_src_rel
ADD CONSTRAINT datasrc_main UNIQUE (c_src_ptr_field, f_src_datasrc);

ALTER TABLE t_mts_data_view
ADD CONSTRAINT uk_mts_dataview_name UNIQUE (c_name);

ALTER TABLE t_mts_db_conn
ADD CONSTRAINT uk_mts_dbconn_name UNIQUE (c_name);

ALTER TABLE t_mts_db_conn_grp
ADD CONSTRAINT uk_mts_dbconngrp_name UNIQUE (c_name);

----------------------------------
-- CREATE REFERENTIAL CONSTRAINTS
----------------------------------

ALTER TABLE a_mt_mts_dataview_group
ADD CONSTRAINT FK_as3vnbmsb0rsim7oi9hb7puw4
FOREIGN KEY (r_num)
REFERENCES REVINFO;

ALTER TABLE a_mt_mts_report_group
ADD CONSTRAINT FK_rqrfamcem36iwh2wg7hp9r7uv
FOREIGN KEY (r_num)
REFERENCES REVINFO;

ALTER TABLE a_t_mts_cfg_lob
ADD CONSTRAINT FK_j1p53q2e99grrafqc9r3af0o7
FOREIGN KEY (r_num)
REFERENCES REVINFO;

ALTER TABLE a_t_mts_data_group
ADD CONSTRAINT FK_aabbixvys9m8d94idoeokdoa6
FOREIGN KEY (r_num)
REFERENCES REVINFO;

ALTER TABLE a_t_mts_data_src
ADD CONSTRAINT FK_t4al3camda5pkk3dna274f6y7
FOREIGN KEY (r_num)
REFERENCES REVINFO;

ALTER TABLE a_t_mts_data_src_rel
ADD CONSTRAINT FK_aigk7fpogx3gfaoi05y8d3d6k
FOREIGN KEY (r_num)
REFERENCES REVINFO;

ALTER TABLE a_t_mts_data_view
ADD CONSTRAINT FK_4u871kwsno6fncxsqyvw49o4l
FOREIGN KEY (r_num)
REFERENCES REVINFO;

ALTER TABLE a_t_mts_db_conn
ADD CONSTRAINT FK_hx7nxcl41sklyncalbblegrmo
FOREIGN KEY (r_num)
REFERENCES REVINFO;

ALTER TABLE a_t_mts_db_conn_grp
ADD CONSTRAINT FK_16v70l9o27g9djqmbsuyqmiws
FOREIGN KEY (r_num)
REFERENCES REVINFO;

ALTER TABLE a_t_mts_report
ADD CONSTRAINT FK_18m3x0g7bavy9yv3ykmudfiic
FOREIGN KEY (r_num)
REFERENCES REVINFO;

------------------------------

ALTER TABLE mt_mts_dataview_group
ADD CONSTRAINT dataview_group2group
FOREIGN KEY (f_group)
REFERENCES t_mts_data_group;

ALTER TABLE mt_mts_dataview_group
ADD CONSTRAINT dataview_group2dataview
FOREIGN KEY (f_data_view)
REFERENCES t_mts_data_view;

ALTER TABLE mt_mts_report_group
ADD CONSTRAINT report_group2group
FOREIGN KEY (f_group)
REFERENCES t_mts_data_group;

ALTER TABLE mt_mts_report_group
ADD CONSTRAINT report_group2report
FOREIGN KEY (f_report)
REFERENCES t_mts_report;

ALTER TABLE t_mts_cfg_lob
ADD CONSTRAINT mtscfglob_crtrusr2user
FOREIGN KEY (f_creator_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_cfg_lob
ADD CONSTRAINT mtscfglob_mdfrusr2user
FOREIGN KEY (f_modifier_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_data_group
ADD CONSTRAINT datagrp_crtrusr2user
FOREIGN KEY (f_creator_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_data_group
ADD CONSTRAINT datagrp_mdfrusr2user
FOREIGN KEY (f_modifier_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_data_src
ADD CONSTRAINT datasrc2cfglob
FOREIGN KEY (f_config)
REFERENCES t_mts_cfg_lob;

ALTER TABLE t_mts_data_src
ADD CONSTRAINT datasrc2dbconn
FOREIGN KEY (f_connection)
REFERENCES t_mts_db_conn;

ALTER TABLE t_mts_data_src
ADD CONSTRAINT datasrc_crtrusr2user
FOREIGN KEY (f_creator_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_data_src
ADD CONSTRAINT datasrc_mdfrusr2user
FOREIGN KEY (f_modifier_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_data_src_rel
ADD CONSTRAINT datasrcrel_crtrusr2user
FOREIGN KEY (f_creator_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_data_src_rel
ADD CONSTRAINT datasrcrel_mdfrusr2user
FOREIGN KEY (f_modifier_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_data_src_rel
ADD CONSTRAINT datasrcrel_src2datasrc
FOREIGN KEY (f_src_datasrc)
REFERENCES t_mts_data_src;

ALTER TABLE t_mts_data_src_rel
ADD CONSTRAINT datasrcrel_tgt2datasrc
FOREIGN KEY (f_tgt_datasrc)
REFERENCES t_mts_data_src;

ALTER TABLE t_mts_data_view
ADD CONSTRAINT dataview2cfglob
FOREIGN KEY (f_config)
REFERENCES t_mts_cfg_lob;

ALTER TABLE t_mts_data_view
ADD CONSTRAINT dataview_crtrusr2user
FOREIGN KEY (f_creator_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_data_view
ADD CONSTRAINT dataview2datasrc
FOREIGN KEY (f_data_src)
REFERENCES t_mts_data_src;

ALTER TABLE t_mts_data_view
ADD CONSTRAINT dataview_mdfrusr2user
FOREIGN KEY (f_modifier_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_db_conn
ADD CONSTRAINT dbconn2cfglob
FOREIGN KEY (f_config)
REFERENCES t_mts_cfg_lob;

ALTER TABLE t_mts_db_conn
ADD CONSTRAINT dbconn_crtrusr2user
FOREIGN KEY (f_creator_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_db_conn
ADD CONSTRAINT dbconn2group
FOREIGN KEY (f_group)
REFERENCES t_mts_db_conn_grp;

ALTER TABLE t_mts_db_conn
ADD CONSTRAINT dbconn_mdfrusr2user
FOREIGN KEY (f_modifier_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_db_conn_grp
ADD CONSTRAINT dbconngrp2cfglob
FOREIGN KEY (f_config)
REFERENCES t_mts_cfg_lob;

ALTER TABLE t_mts_db_conn_grp
ADD CONSTRAINT dbconngrp_crtrusr2user
FOREIGN KEY (f_creator_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_db_conn_grp
ADD CONSTRAINT dbconngrp_mdfrusr2user
FOREIGN KEY (f_modifier_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_report
ADD CONSTRAINT report_crtrusr2user
FOREIGN KEY (f_creator_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_report
ADD CONSTRAINT report2dataview
FOREIGN KEY (f_data_view)
REFERENCES t_mts_data_view;

ALTER TABLE t_mts_report
ADD CONSTRAINT report_mdfrusr2user
FOREIGN KEY (f_modifier_user)
REFERENCES t_dmt_user;

ALTER TABLE t_mts_usr_prf
ADD CONSTRAINT mtsusrprf2dbconn
FOREIGN KEY (f_dflt_conn)
REFERENCES t_mts_db_conn;

ALTER TABLE t_mts_usr_prf
ADD CONSTRAINT mtsusrprf2user
FOREIGN KEY (id)
REFERENCES t_dmt_user;

--------------------
-- CREATE SEQUENCES
--------------------

CREATE SEQUENCE mts_cfg_lob START WITH 1 INCREMENT BY 1;

CREATE SEQUENCE mts_data_group START WITH 1 INCREMENT BY 1;

CREATE SEQUENCE mts_data_src START WITH 1 INCREMENT BY 1;

CREATE SEQUENCE mts_data_src_rel START WITH 1 INCREMENT BY 1;

CREATE SEQUENCE mts_data_view START WITH 1 INCREMENT BY 1;

CREATE SEQUENCE mts_db_conn START WITH 1 INCREMENT BY 1;

CREATE SEQUENCE mts_db_conn_grp START WITH 1 INCREMENT BY 1;

CREATE SEQUENCE mts_report START WITH 1 INCREMENT BY 1;


