CREATE TABLE t_topic (
  id       NUMBER(10, 0),
  c_name   VARCHAR2(255 CHAR),
  n_level  NUMBER(10, 0),
  f_parent NUMBER(10, 0),

  CONSTRAINT t_topic_pk PRIMARY KEY (id),
  CONSTRAINT child2parent FOREIGN KEY (f_parent) REFERENCES t_topic (id)
);

INSERT INTO t_topic (id, c_name, n_level, f_parent) VALUES (1, 'L01', 1, NULL);
INSERT INTO t_topic (id, c_name, n_level, f_parent) VALUES (2, 'L02', 1, NULL);
INSERT INTO t_topic (id, c_name, n_level, f_parent) VALUES (3, 'L03', 1, NULL);
INSERT INTO t_topic (id, c_name, n_level, f_parent) VALUES (4, 'L04', 1, NULL);

INSERT INTO t_topic (id, c_name, n_level, f_parent) VALUES (5, 'L01.01', 2, 1);
INSERT INTO t_topic (id, c_name, n_level, f_parent) VALUES (6, 'L01.02', 2, 1);
INSERT INTO t_topic (id, c_name, n_level, f_parent) VALUES (7, 'L01.03', 2, 1);

INSERT INTO t_topic (id, c_name, n_level, f_parent) VALUES (8, 'L02.01', 2, 2);
INSERT INTO t_topic (id, c_name, n_level, f_parent) VALUES (9, 'L02.02', 2, 2);

INSERT INTO t_topic (id, c_name, n_level, f_parent) VALUES (10, 'L01.02.01', 3, 6);
INSERT INTO t_topic (id, c_name, n_level, f_parent) VALUES (11, 'L01.02.02', 3, 6);
INSERT INTO t_topic (id, c_name, n_level, f_parent) VALUES (12, 'L01.02.03', 3, 6);



CREATE TABLE t_person (
  id     NUMBER(10, 0),
  c_name VARCHAR2(255 CHAR),

  CONSTRAINT t_person_pk PRIMARY KEY (id)
);

CREATE TABLE t_state (
  id     NUMBER(10, 0),
  c_name VARCHAR2(255 CHAR),

  CONSTRAINT t_state_pk PRIMARY KEY (id)
);

CREATE TABLE t_city (
  id      NUMBER(10, 0),
  c_name  VARCHAR2(255 CHAR),
  f_state NUMBER(10, 0),

  CONSTRAINT t_city_pk PRIMARY KEY (id),
  CONSTRAINT city2state FOREIGN KEY (f_state) REFERENCES t_state (id)
);

CREATE TABLE t_trx (
  id       NUMBER(10, 0),
  n_value  NUMBER(38, 4),
  d_date   DATE,
  f_place  NUMBER(10, 0),
  f_person NUMBER(10, 0),

  CONSTRAINT t_trx_pk PRIMARY KEY (id),
  CONSTRAINT trx2city FOREIGN KEY (f_place) REFERENCES t_city (id),
  CONSTRAINT trx2person FOREIGN KEY (f_person) REFERENCES t_person (id)
);


INSERT INTO t_person (id, c_name) VALUES (1, 'Jack');
INSERT INTO t_person (id, c_name) VALUES (2, 'Joe');
INSERT INTO t_person (id, c_name) VALUES (3, 'John');
INSERT INTO t_person (id, c_name) VALUES (4, 'James');

INSERT INTO t_state (id, c_name) VALUES (1, 'S01');
INSERT INTO t_state (id, c_name) VALUES (2, 'S02');
INSERT INTO t_state (id, c_name) VALUES (3, 'S03');
INSERT INTO t_state (id, c_name) VALUES (4, 'S04');
INSERT INTO t_state (id, c_name) VALUES (5, 'S05');
INSERT INTO t_state (id, c_name) VALUES (6, 'S06');
INSERT INTO t_state (id, c_name) VALUES (7, 'S07');
INSERT INTO t_state (id, c_name) VALUES (8, 'S08');
INSERT INTO t_state (id, c_name) VALUES (9, 'S09');
INSERT INTO t_state (id, c_name) VALUES (10, 'S10');
INSERT INTO t_state (id, c_name) VALUES (11, 'S11');
INSERT INTO t_state (id, c_name) VALUES (12, 'S12');
INSERT INTO t_state (id, c_name) VALUES (13, 'S13');
INSERT INTO t_state (id, c_name) VALUES (14, 'S14');
INSERT INTO t_state (id, c_name) VALUES (15, 'S15');
INSERT INTO t_state (id, c_name) VALUES (16, 'S16');
INSERT INTO t_state (id, c_name) VALUES (17, 'S17');
INSERT INTO t_state (id, c_name) VALUES (18, 'S18');
INSERT INTO t_state (id, c_name) VALUES (19, 'S19');
INSERT INTO t_state (id, c_name) VALUES (20, 'S20');


DECLARE
  cnt_st  NUMBER;
  ct_id   NUMBER;
  st_name VARCHAR2(255 CHAR);
BEGIN
  ct_id := 1;

  SELECT count(1)
  INTO cnt_st
  FROM t_state;

  FOR st IN 1..cnt_st
  LOOP
    SELECT c_name
    INTO st_name
    FROM t_state
    WHERE id = st;

    FOR ct IN 1..dbms_random.value(1, 9)
    LOOP
      INSERT INTO t_city (id, c_name, f_state) VALUES (ct_id, st_name || '.C' || ct, st);
      ct_id := ct_id + 1;
    END LOOP;

    COMMIT;
  END LOOP;
END;

DECLARE
  cnt_ct NUMBER;
  cnt_pr NUMBER;
  trx_id NUMBER;
BEGIN
  trx_id := 1;

  SELECT count(1)
  INTO cnt_pr
  FROM t_person;

  SELECT count(1)
  INTO cnt_ct
  FROM t_city;

  FOR pr IN 1..cnt_pr
  LOOP
    FOR ct IN 1..cnt_ct
    LOOP
      FOR tx IN 1..dbms_random.value(10, 1000)
      LOOP
        INSERT INTO t_trx (id, n_value, d_date, f_place, f_person)
        VALUES (trx_id, dbms_random.value(10000, 100000000), sysdate - dbms_random.value(10, 10000), ct, pr);
        trx_id := trx_id + 1;
      END LOOP;
    END LOOP;

    COMMIT;
  END LOOP;
END;


