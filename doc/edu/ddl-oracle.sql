CREATE TABLE "T_BOOK_SIZE" (
	"ID"     NUMBER(19, 0),
	"C_NAME" VARCHAR2(255 CHAR),

	PRIMARY KEY ("ID")
);

CREATE TABLE "T_LOCATION_TYPE" (
	"ID"     NUMBER(19, 0),
	"C_NAME" VARCHAR2(255 CHAR),

	PRIMARY KEY ("ID")
);

CREATE TABLE "T_CATEGORY" (
	"ID"     NUMBER(19, 0),
	"C_CODE" VARCHAR2(255 CHAR),
	"C_NAME" VARCHAR2(255 CHAR),

	PRIMARY KEY ("ID")
);

CREATE TABLE "T_CATEGORY_ELEMENT" (
	"ID"         NUMBER(19, 0),
	"C_CODE"     VARCHAR2(255 CHAR),
	"C_NAME"     VARCHAR2(255 CHAR),
	"F_CATEGORY" NUMBER(19, 0),

	PRIMARY KEY ("ID"),
	CONSTRAINT "FK_CTGE2CT" FOREIGN KEY ("F_CATEGORY") REFERENCES "T_CATEGORY" ("ID")
);

-- ---------------

CREATE TABLE "T_PERSON" (
	"ID"             NUMBER(19, 0),
	"C_NAME"         VARCHAR2(255 CHAR),
	"D_BIRTH_DATE"   DATE,
	"D_REGISTRATION" DATE,
	"B_ALIVE"        NUMBER(1, 0),

	PRIMARY KEY ("ID")
);

CREATE TABLE "T_BOOK" (
	"ID"             NUMBER(19, 0),
	"C_TITLE"        VARCHAR2(255 CHAR),
	"N_PUBLISH_YEAR" NUMBER(10, 0),
	"N_PRICE"        NUMBER(38, 4),
	"F_SIZE"         NUMBER(19, 0),

	PRIMARY KEY ("ID"),
	CONSTRAINT "FK_BOOK2CTGE" FOREIGN KEY ("F_SIZE") REFERENCES "T_CATEGORY_ELEMENT" ("ID")
);

CREATE TABLE "MT_BOOK_AUTHOR" (
	"F_BOOK"   NUMBER(19, 0),
	"F_AUTHOR" NUMBER(19, 0),

	PRIMARY KEY ("F_BOOK", "F_AUTHOR"),
	CONSTRAINT "FK_BOOKAUTHOR2BOOK" FOREIGN KEY ("F_BOOK") REFERENCES "T_BOOK" ("ID"),
	CONSTRAINT "FK_BOOKAUTHOR2AUTHOR" FOREIGN KEY ("F_AUTHOR") REFERENCES "T_PERSON" ("ID")
);

CREATE TABLE "T_LOCATION" (
	"ID"       NUMBER(19, 0),
	"C_NAME"   VARCHAR2(255 CHAR),
	"F_TYPE"   NUMBER(19, 0),
	"F_PARENT" NUMBER(19, 0),

	PRIMARY KEY ("ID"),
	CONSTRAINT "FK_LOCATION2CTGE" FOREIGN KEY ("F_TYPE") REFERENCES "T_CATEGORY_ELEMENT" ("ID"),
	CONSTRAINT "FK_LOCATION2PARENT" FOREIGN KEY ("F_PARENT") REFERENCES "T_LOCATION" ("ID")
);

CREATE TABLE "T_STORE" (
	"ID"         NUMBER(19, 0),
	"C_NAME"     VARCHAR2(255 CHAR),
	"F_LOCATION" NUMBER(19, 0),

	PRIMARY KEY ("ID"),
	CONSTRAINT "FK_STORE2LOCATION" FOREIGN KEY ("F_LOCATION") REFERENCES "T_LOCATION" ("ID")
);

CREATE TABLE "T_INVENTORY" (
	"ID"      NUMBER(19, 0),
	"N_COUNT" NUMBER(19, 0),
	"F_BOOK"  NUMBER(19, 0),
	"F_STORE" NUMBER(19, 0),

	PRIMARY KEY ("ID"),
	CONSTRAINT "FK_INVENTORY2BOOK" FOREIGN KEY ("F_BOOK") REFERENCES "T_BOOK" ("ID"),
	CONSTRAINT "FK_INVENTORY2STORE" FOREIGN KEY ("F_STORE") REFERENCES "T_STORE" ("ID")
);

-- ------
--  DATA
-- ------

insert into T_BOOK_SIZE (ID, C_NAME) values (11, 'Large');
insert into T_BOOK_SIZE (ID, C_NAME) values (12, 'Normal');
insert into T_BOOK_SIZE (ID, C_NAME) values (13, 'Small');
insert into T_BOOK_SIZE (ID, C_NAME) values (14, 'Mini');

insert into T_LOCATION_TYPE (ID, C_NAME) values (1, 'Province');
insert into T_LOCATION_TYPE (ID, C_NAME) values (2, 'City');
insert into T_LOCATION_TYPE (ID, C_NAME) values (3, 'Section');

-- ---------------

insert into T_CATEGORY (ID, C_CODE, C_NAME) values (1, 'BOOK_SIZE', 'Book Size');
insert into T_CATEGORY (ID, C_CODE, C_NAME) values (2, 'LOCATION_TYPE', 'Location Type');

insert into T_CATEGORY_ELEMENT (ID, C_CODE, C_NAME, F_CATEGORY) values (11, 'LARGE', 'Large', 1);
insert into T_CATEGORY_ELEMENT (ID, C_CODE, C_NAME, F_CATEGORY) values (12, 'NORMAL', 'Normal', 1);
insert into T_CATEGORY_ELEMENT (ID, C_CODE, C_NAME, F_CATEGORY) values (13, 'SMALL', 'Small', 1);
insert into T_CATEGORY_ELEMENT (ID, C_CODE, C_NAME, F_CATEGORY) values (14, 'MINI', 'Mini', 1);
insert into T_CATEGORY_ELEMENT (ID, C_CODE, C_NAME, F_CATEGORY) values (1, 'PROVINCE', 'Province', 2);
insert into T_CATEGORY_ELEMENT (ID, C_CODE, C_NAME, F_CATEGORY) values (2, 'CITY', 'City', 2);
insert into T_CATEGORY_ELEMENT (ID, C_CODE, C_NAME, F_CATEGORY) values (3, 'SECTION', 'Section', 2);

-- ---------------

insert into T_PERSON (ID, C_NAME, D_BIRTH_DATE, B_ALIVE, D_REGISTRATION)
values (1, 'Steve Jobs', to_date('1945-02-04 12:00:00', 'yyyy-mm-dd hh24:mi:ss'), 0,
				to_date('2010-12-14 10:30:00', 'yyyy-mm-dd hh24:mi:ss'));
insert into T_PERSON (ID, C_NAME, D_BIRTH_DATE, B_ALIVE, D_REGISTRATION)
values (2, 'Bill Gates', to_date('1945-02-05 12:00:00', 'yyyy-mm-dd hh24:mi:ss'), 1,
				to_date('2009-11-21 00:00:00', 'yyyy-mm-dd hh24:mi:ss'));
insert into T_PERSON (ID, C_NAME, D_BIRTH_DATE, B_ALIVE, D_REGISTRATION)
values (3, 'Enrique Castro-Leon', to_date('1980-11-23 12:00:00', 'yyyy-mm-dd hh24:mi:ss'), 1,
				to_date('2011-01-01 22:56:00', 'yyyy-mm-dd hh24:mi:ss'));
insert into T_PERSON (ID, C_NAME, D_BIRTH_DATE, B_ALIVE, D_REGISTRATION)
values (4, 'Robert Harmon', to_date('1981-08-11 12:00:00', 'yyyy-mm-dd hh24:mi:ss'), 1,
				to_date('2008-06-02 23:59:59', 'yyyy-mm-dd hh24:mi:ss'));

insert into T_BOOK (ID, C_TITLE, N_PUBLISH_YEAR, N_PRICE, F_SIZE) values (1, 'Kill Bill', 1999, 42.5, 11);
insert into T_BOOK (ID, C_TITLE, N_PUBLISH_YEAR, N_PRICE, F_SIZE) values (2, 'Programing with QBasic', 1982, 22.5, 13);
insert into T_BOOK (ID, C_TITLE, N_PUBLISH_YEAR, N_PRICE, F_SIZE) values (3, 'Cloud as a Service', 2016, 52.9, 12);

insert into MT_BOOK_AUTHOR (F_BOOK, F_AUTHOR) values (1, 1);
insert into MT_BOOK_AUTHOR (F_BOOK, F_AUTHOR) values (2, 2);
insert into MT_BOOK_AUTHOR (F_BOOK, F_AUTHOR) values (3, 3);
insert into MT_BOOK_AUTHOR (F_BOOK, F_AUTHOR) values (3, 4);

insert into T_LOCATION (ID, C_NAME, F_TYPE, F_PARENT) values (1, 'Tehran', 1, null);
insert into T_LOCATION (ID, C_NAME, F_TYPE, F_PARENT) values (11, 'Tehran', 2, 1);
insert into T_LOCATION (ID, C_NAME, F_TYPE, F_PARENT) values (111, 'Sec01', 3, 11);
insert into T_LOCATION (ID, C_NAME, F_TYPE, F_PARENT) values (112, 'Sec02', 3, 11);
insert into T_LOCATION (ID, C_NAME, F_TYPE, F_PARENT) values (113, 'Sec03', 3, 11);
insert into T_LOCATION (ID, C_NAME, F_TYPE, F_PARENT) values (13, 'Pardis', 2, 1);
insert into T_LOCATION (ID, C_NAME, F_TYPE, F_PARENT) values (2, 'Kerman', 1, null);
insert into T_LOCATION (ID, C_NAME, F_TYPE, F_PARENT) values (21, 'Kerman', 2, 2);

insert into T_STORE (ID, C_NAME, F_LOCATION) values (1, 'Noble', 111);
insert into T_STORE (ID, C_NAME, F_LOCATION) values (2, 'CityBook B1', 113);
insert into T_STORE (ID, C_NAME, F_LOCATION) values (3, 'CityBook B2', 21);
insert into T_STORE (ID, C_NAME, F_LOCATION) values (4, 'ParadiseBook', 13);

insert into T_INVENTORY (ID, N_COUNT, F_BOOK, F_STORE) values (10, 2, 1, 1);
insert into T_INVENTORY (ID, N_COUNT, F_BOOK, F_STORE) values (11, 3, 1, 2);
insert into T_INVENTORY (ID, N_COUNT, F_BOOK, F_STORE) values (12, 0, 1, 3);
insert into T_INVENTORY (ID, N_COUNT, F_BOOK, F_STORE) values (13, 4, 1, 4);
insert into T_INVENTORY (ID, N_COUNT, F_BOOK, F_STORE) values (14, 1, 2, 1);
insert into T_INVENTORY (ID, N_COUNT, F_BOOK, F_STORE) values (15, 7, 2, 2);
insert into T_INVENTORY (ID, N_COUNT, F_BOOK, F_STORE) values (16, 9, 2, 3);
insert into T_INVENTORY (ID, N_COUNT, F_BOOK, F_STORE) values (17, 2, 2, 4);
insert into T_INVENTORY (ID, N_COUNT, F_BOOK, F_STORE) values (18, 5, 3, 1);
insert into T_INVENTORY (ID, N_COUNT, F_BOOK, F_STORE) values (19, 8, 3, 2);
insert into T_INVENTORY (ID, N_COUNT, F_BOOK, F_STORE) values (20, 0, 3, 3);
insert into T_INVENTORY (ID, N_COUNT, F_BOOK, F_STORE) values (21, 6, 3, 4);

