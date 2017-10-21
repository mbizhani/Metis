----------------
-- alter tables
----------------

ALTER TABLE a_t_mts_db_conn ADD c_custom_param_1 VARCHAR2(255 CHAR);
ALTER TABLE a_t_mts_data_src ADD e_conn_selection NUMBER(10, 0);

-----------------

ALTER TABLE t_mts_db_conn ADD c_custom_param_1 VARCHAR2(255 CHAR);

ALTER TABLE t_mts_data_src ADD e_conn_selection NUMBER(10, 0);
UPDATE t_mts_data_src
SET e_conn_selection = 1;
ALTER TABLE t_mts_data_src MODIFY e_conn_selection NUMBER(10, 0) NOT NULL;