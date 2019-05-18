alter session set NLS_DATE_FORMAT = 'YYYY-MM-DD';
alter session set NLS_TIMESTAMP_FORMAT = 'YYYY-MM-DD HH24:MI:SS';

/*==============================================================*/
/* Table : blob_values                                          */
/*==============================================================*/
create table blob_values (
b_id                 INTEGER not null,
b_long               LONG,
b_nclob              NCLOB,
b_clob               CLOB,
b_blob               BLOB,
b_bfile              BFILE,
primary key (b_id)
);

/*==============================================================*/
/* Table : binary_values                                        */
/*==============================================================*/
create table binary_values (
n_id                 INTEGER not null,
n_raw                RAW(255),
n_longraw            LONG RAW,
n_blob               BLOB,
primary key (n_id)
);

/*==============================================================*/
/* Table : cargo                                                */
/*==============================================================*/
create table cargo (
c_id                 INTEGER                        not null,
c_dep_id             SMALLINT,
c_name               CHAR(10),
c_seal               SMALLINT,
c_date_came          TIMESTAMP,
c_date_out           TIMESTAMP,
c_weight             FLOAT,
c_width              INTEGER,
c_height             INTEGER,
c_cost               NUMERIC(12,4),
c_attributes         CLOB,
primary key (c_id)
);

/*==============================================================*/
/* Table : date_values                                          */
/*==============================================================*/
create table date_values (
d_id                 INTEGER                        not null,
d_date               DATE,
d_time               TIMESTAMP,
d_datetime           TIMESTAMP,
d_timestamp          TIMESTAMP,
primary key (d_id)
);

/*==============================================================*/
/* Table : department                                           */
/*==============================================================*/
create table department (
dep_id               SMALLINT                       not null,
dep_name             VARCHAR(20),
dep_shname           CHAR(5),
dep_address          VARCHAR(255),
primary key (dep_id)
);

/*==============================================================*/
/* View : department                                            */
/*==============================================================*/
create view dep_view as select * from department;

/*==============================================================*/
/* Table : equipment                                            */
/*==============================================================*/
create table equipment (
eq_id                INTEGER                        not null,
eq_name              VARCHAR(30),
eq_type              SMALLINT,
eq_cost              NUMERIC(9,4),
eq_date              DATE,
woff_date            DATE,
primary key (eq_id)
);

/*==============================================================*/
/* Table : equipment2                                           */
/*==============================================================*/
create table equipment2 (
dep_id               SMALLINT                       not null,
eq_id                INTEGER                        not null,
primary key (dep_id, eq_id)
);

/*==============================================================*/
/* Table : number_values                                        */
/*==============================================================*/
create table number_values (
n_id                 INTEGER                        not null,
n_tint               SMALLINT,
n_sint               SMALLINT,
n_int                INTEGER,
n_bdecimal           DECIMAL(20),
n_numeric            NUMERIC(9,4),
n_float              FLOAT,
n_real               FLOAT,
n_dprecission        DOUBLE PRECISION,
n_money              NUMERIC(19,4),
primary key (n_id)
);

/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table people (
p_id                 SMALLINT                       not null,
p_dep_id             SMALLINT,
p_name               VARCHAR(40),
p_begin_work         TIMESTAMP,
p_end_work           TIMESTAMP,
p_picture            BLOB,
p_resume             CLOB,
p_redundant          SMALLINT,
primary key (p_id)
);

/*==============================================================*/
/* Table : string_values                                        */
/*==============================================================*/
create table string_values
(
   s_id                           INTEGER not null,
   s_char                         CHAR(255),
   s_varchar                      VARCHAR(255),
   s_nchar                        CHAR(255),
   s_nvarchar                     NVARCHAR2(255),
   s_bit                          CLOB,
   s_varbit                       BLOB,
   primary key (s_id)
);

/*==============================================================*/
/* Table : not_null_values                                      */
/*==============================================================*/
create table not_null_values
(
   n_id                           int              	not null,
   n_varchar                      VARCHAR(255)		not null,
   primary key (n_id)
);


/*==============================================================*/
/* Table : Case_Sensitive                                       */
/*==============================================================*/
create table "Case_Sensitive" (
cs_id                 INTEGER                        not null,
"Cs_Data1"            INTEGER,
"cs_data1"            INTEGER,
"cs data1"            INTEGER,
primary key (cs_id)
);

/*==============================================================*/
/* Table : Spaced Names                                         */
/*==============================================================*/
create table "Spaced Names" (
cs_id                 INTEGER                        not null,
"Cs Data1"            INTEGER,
"cs data2"            INTEGER,
"cS data3"            INTEGER,
primary key (cs_id)
);

/*==============================================================*/
/* Table : case_sensitive                                       */
/*==============================================================*/
create table case_sensitive (
cs_id                 INTEGER                        not null,
"CS_DATA1"            INTEGER,
"CS_Data2"            INTEGER,
"Cs_Data3"            INTEGER,
primary key (cs_id)
);

/*==============================================================*/
/* Table : high_load                                            */
/*==============================================================*/
create table high_load (
hl_id		      INTEGER NOT NULL,
stBoolean             CHAR(1),
stByte                SMALLINT,
stShort               SMALLINT,
stInteger             INTEGER,
stLong                DECIMAL(19),
stFloat               FLOAT,
stDouble              DOUBLE PRECISION,
stBigDecimal          NUMERIC(18,4),
stString              VARCHAR(100),
stUnicodeString       NVARCHAR2(255),
stBytes               RAW(100),
stDate                DATE,
stTime                TIMESTAMP,
stTimestamp           TIMESTAMP,
stGUID                CHAR(38),
stAsciiStream         CLOB,
stUnicodeStream       NCLOB,
stBinaryStream        BLOB,
primary key (hl_id)
);

/*==============================================================*/
/* Table : default_values                                       */
/*==============================================================*/
create table default_values
(
   d_id                           INTEGER NOT NULL,
   d_fld1                         INTEGER DEFAULT 123456,
   d_fld2                         FLOAT DEFAULT 123.456,
   d_fld3                     	  VARCHAR(10) DEFAULT 'xyz',
   d_fld4                     	  DATE default '2003-12-11',
   d_fld6                     	  TIMESTAMP default '2003-12-11 23:12:11',
   primary key (d_id)
);

/*==============================================================*/
/* Table : default_values2                                       */
/*==============================================================*/
create table default_values2
(
   d_id                           INTEGER NOT NULL,
   d_fld1                         FLOAT DEFAULT 123.456,
   d_fld2                         INTEGER DEFAULT 123456,
   d_fld3                     	  DATE default '2003-12-11',
   d_fld4                     	  VARCHAR(10) DEFAULT 'xyz',
   d_fld6                     	  TIMESTAMP default '2003-12-11 23:12:11',
   primary key (d_id)
);

alter table cargo
   add foreign key (c_dep_id) references department (dep_id);

alter table equipment2
   add foreign key (dep_id) references department (dep_id);

alter table equipment2
   add foreign key (eq_id) references equipment (eq_id);

alter table people
   add foreign key (p_dep_id) references department (dep_id);

/*==============================================================*/
/* Table : bcd_values                                           */
/*==============================================================*/
create table bcd_values
(
   id                             INTEGER NOT NULL,
   curr18_4                       DECIMAL(18,4),
   curr15_2                       DECIMAL(15,2),
   curr10_4                       DECIMAL(10,4),
   curr4_4                        DECIMAL(4,4),
   bigd18_1                       DECIMAL(18,1),
   bigd18_5                       DECIMAL(18,5),
   bigd12_10                      DECIMAL(12,10),
   bigd18_18                      DECIMAL(18,18),
   primary key (id)
);

SET TERM ^ ;

/*==============================================================*/
/* Stored procedure: abtest                                     */
/*==============================================================*/
Create or replace procedure ABTEST (P1 NUMBER, P2 NUMBER, P3 VARCHAR,
  P4 OUT NUMBER, P5 OUT VARCHAR) IS
BEGIN
  p4 := p1 * 10 + p2;
  p5 := p3 || p3;
END; ^

/*==============================================================*/
/* Stored procedure: myfuncInOutReturn                          */
/*==============================================================*/
CREATE OR REPLACE FUNCTION "myfuncInOutReturn"(x IN OUT VARCHAR) RETURN VARCHAR IS
BEGIN
  x := x||'outvalue';
  RETURN 'returned string';
END; ^

/*==============================================================*/
/* Stored procedure: simple_func                                */
/*==============================================================*/
CREATE OR REPLACE FUNCTION simple_func RETURN INTEGER IS
BEGIN
  RETURN 1111;
END; ^

/*==============================================================*/
/* Stored procedure: simplefunc                                */
/*==============================================================*/
CREATE OR REPLACE FUNCTION simplefunc RETURN INTEGER IS
BEGIN
  RETURN 2222;
END; ^

/*==============================================================*/
/* Stored procedure: IS_ACCOUNT_SERVE                           */
/*==============================================================*/
CREATE OR REPLACE FUNCTION IS_ACCOUNT_SERVE(p_MIFARE_ID varchar2, p_msg OUT varchar2)
  RETURN NUMBER
IS
  l_ret NUMBER;
BEGIN
  l_ret := 1;
  p_msg := 'OK';
  RETURN l_ret;
END; ^

CREATE OR REPLACE
PACKAGE MYPACKAGE AS 
  procedure ABTEST (P1 NUMBER, P2 NUMBER, P3 VARCHAR2, P4 OUT NUMBER, P5 OUT VARCHAR2);
  FUNCTION "myfuncInOutReturn"(x IN OUT VARCHAR) RETURN VARCHAR2;
  FUNCTION simple_func RETURN INTEGER;
  FUNCTION simplefunc RETURN INTEGER;
END MYPACKAGE; ^

CREATE OR REPLACE
PACKAGE BODY MYPACKAGE AS

  procedure ABTEST (P1 NUMBER, P2 NUMBER, P3 VARCHAR2, P4 OUT NUMBER, P5 OUT VARCHAR2) AS
  BEGIN
    p4 := p1 * 10 + p2;
    p5 := p3 || p3;
  END ABTEST;

  FUNCTION "myfuncInOutReturn"(x IN OUT VARCHAR) RETURN VARCHAR2 AS
  BEGIN
    x := x||'outvalue';
    RETURN 'returned string';
  END "myfuncInOutReturn";

  FUNCTION simple_func RETURN INTEGER AS
  BEGIN
    RETURN 1111;
  END simple_func;

  FUNCTION simplefunc RETURN INTEGER AS
  BEGIN
     RETURN 2222;
  END simplefunc;

END MYPACKAGE; ^

SET TERM ; ^

CREATE TYPE address_t AS OBJECT (
   street  VARCHAR2(30),
   city    VARCHAR2(20),
   state   CHAR(2),
   zip     CHAR(5) );

CREATE TYPE address_tab IS TABLE OF address_t;

CREATE TABLE customers (
   custid  NUMBER,
   address address_tab )
NESTED TABLE address STORE AS customer_addresses;

/*==============================================================*/
/* Grant privileges to columns                                  */
/*==============================================================*/
grant update(p_resume, p_redundant) on people to sys;

/*==============================================================*/
/* Grant privileges to table                                    */
/*==============================================================*/
grant select on people to sys;
