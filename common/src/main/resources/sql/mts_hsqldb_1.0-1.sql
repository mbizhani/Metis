----------------
-- ALTER TABLES
----------------

ALTER TABLE a_t_mts_db_conn ADD c_custom_param_1 VARCHAR(255);
ALTER TABLE t_mts_db_conn ADD c_custom_param_1 VARCHAR(255);

ALTER TABLE t_mts_data_src ADD e_conn_selection INTEGER NOT NULL;
ALTER TABLE a_t_mts_data_src ADD e_conn_selection INTEGER;