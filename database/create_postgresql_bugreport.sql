/*==============================================================*/
/* Database name:  PostgreSql                                   */
/* DBMS name:      PostgreSQL 7                                 */
/* Created on:     04.02.2003 19:59:06                          */
/*==============================================================*/


--SET CLIENT_ENCODING TO WIN1251 -- should not be necessary anymore - everything should be ASCII now;

/*==============================================================*/
/* Tables for Bug#739514                                        */
/*==============================================================*/
CREATE TABLE test739514 (
  id  INT4 NOT NULL,
  fld VARCHAR(35),
  PRIMARY KEY (id)
);

INSERT INTO test739514 VALUES (1, E'\u0410\u0431\u0440\u0430\u043a\u0430\u0434\u0430\u0431\u0440\u0430');  /* the second value is Abrakadabra in kyryllic letters, unicode encoded */

/*==============================================================*/
/* Tables for Bug#739519                                        */
/*==============================================================*/
CREATE TABLE test739519 (
  id  INT4 NOT NULL,
  fld VARCHAR(35),
  fld1 VARCHAR(35),
  PRIMARY KEY (id)
);

/*==============================================================*/
/* Tables for Bug#766053                                        */
/*==============================================================*/
CREATE TABLE test766053a (
  id  INT8 NOT NULL,
  fld VARCHAR(50)
);

INSERT INTO test766053a VALUES (93521, E'\u0410\u0431\u0440\u0430\u043a\u0430\u0434\u0430\u0431\u0440\u0430');  /* the second value is Abrakadabra in kyryllic letters */
INSERT INTO test766053a VALUES (93522, 'Open this door');

CREATE TABLE test766053b (
  id  INT8 NOT NULL,
  fld VARCHAR(50)
);

INSERT INTO test766053b VALUES (93521, E'\u0410\u0431\u0440\u0430\u043a\u0430\u0434\u0430\u0431\u0440\u0430');  /* the second value is Abrakadabra in kyryllic letters */
INSERT INTO test766053b VALUES (93522, 'Open this door');

/*==============================================================*/
/* Tables for Bug#816846                                       */
/*==============================================================*/
CREATE TABLE test816846 (
  fld1 TEXT,
  fld2 TEXT
);

INSERT INTO test816846 VALUES ('a', 'b');
INSERT INTO test816846 VALUES ('c', 'd');

/*==============================================================*/
/* Tables for Bug#831559                                       */
/*==============================================================*/

create table "insert" (
  "user" varchar(100),
  "number" integer
); 

/*==============================================================*/
/* Tables for Bug#894367                                       */
/*==============================================================*/

CREATE TABLE test894367a (
  f1 VARCHAR(20) PRIMARY KEY,
  f2 BOOLEAN DEFAULT FALSE,
  f3 BOOLEAN DEFAULT FALSE,
  f4 VARCHAR(20)
) WITHOUT OIDS;

CREATE TABLE test894367b (
  CONSTRAINT fkd65f1 FOREIGN KEY (f1) REFERENCES test894367a (f1)
  ON UPDATE CASCADE ON DELETE CASCADE,
  PRIMARY KEY (f1,f2),
  f1 VARCHAR(20),
  f2 INTEGER,
  f3 VARCHAR(20),
  f4 VARCHAR(20)
) WITHOUT OIDS;


/*==============================================================*/
/* Tables for Mantis-Bug#0000240                                */
/*==============================================================*/

CREATE TABLE ntax_bejovo_konyvelesi_tipusok
(
  kod serial NOT NULL,
  nev character varying(100) NOT NULL,
  tartozik character varying(30),
  kovetel character varying(30),
  CONSTRAINT ntax_bejovo_konyvelesi_tipusok_pkey PRIMARY KEY (kod)
);

/*==============================================================*/
/* Tables for Mantis-Bug#0000229                                */
/*==============================================================*/

CREATE TABLE Mantis229
(
  TEXTFIELD VARCHAR
);

INSERT INTO Mantis229 VALUES ('Mantis229');

/*==============================================================*/
/* Tables for Ticket#51                                         */
/*==============================================================*/

CREATE TABLE Ticket51_A
(
    "ID" integer NOT NULL,
    order_id integer,
    CONSTRAINT table1_pkey PRIMARY KEY ("ID")
);

CREATE TABLE Ticket51_B
(
    "ID" oid NOT NULL,
    t1_id oid,
    "Name" character varying(80),
    CONSTRAINT table2_pkey PRIMARY KEY ("ID")
);

INSERT INTO Ticket51_A values(1,2);
INSERT INTO Ticket51_A values(2,3);
INSERT INTO Ticket51_B VALUES (1,1,'MyName');