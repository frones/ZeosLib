/*==============================================================*/
/* Database name:  PostgreSql                                   */
/* DBMS name:      PostgreSQL 7                                 */
/* Created on:     04.02.2003 19:59:06                          */
/*==============================================================*/

/*==============================================================*/
/* Table : blob_values                                          */
/*==============================================================*/
create table blob_values (
  b_id                 INT4                 not null,
  b_text               TEXT                 null,
  b_image              OID                  null,
  primary key (b_id)
);

/*==============================================================*/
/* Table : date_values                                          */
/*==============================================================*/
create table date_values (
  d_id                 INT4                 not null,
  d_date               DATE                 null,
  d_time               TIME without time zone null,
  d_datetime           TIMESTAMP without time zone null,
  d_timestamp          TIMESTAMP without time zone null,
  primary key (d_id)
);

/*==============================================================*/
/* Table : default_values                                          */
/*==============================================================*/
create table default_values
(
   d_id                           SERIAL not null,
   d_fld1                         INT4 default 123456,
   d_fld2                         FLOAT4 default 123.456,
   d_fld3                     	  VARCHAR(10) default 'xyz',
   d_fld4                     	  DATE default '2003-12-11',
   d_fld5                     	  TIME default '23:12:11',
   d_fld6                     	  TIMESTAMP default '2003-12-11 23:12:11',
   primary key (d_id)
);

/*==============================================================*/
/* Table : default_values2                                          */
/*==============================================================*/
create table default_values2
(
   d_id                           SERIAL not null,
   d_fld1                        FLOAT4 default 123.456,
   d_fld2                         INT4 default 123456,
   d_fld3                     	  DATE default '2003-12-11',
   d_fld4                     	  VARCHAR(10) default 'xyz',
   d_fld5                     	  TIMESTAMP default '2003-12-11 23:12:11',
   d_fld6                     	  TIME default '23:12:11',
   primary key (d_id)
);

/*==============================================================*/
/* Table : department                                           */
/*==============================================================*/
create table department (
dep_id               SERIAL               not null,
dep_name             VARCHAR(20)          null,
dep_shname           CHAR(5)              null,
dep_address          VARCHAR(255)         null,
primary key (dep_id)
);

/*==============================================================*/
/* Table : equipment                                            */
/*==============================================================*/
create table equipment (
eq_id                SERIAL               not null,
eq_name              VARCHAR(30)          null,
eq_type              INT2                 null,
eq_cost              NUMERIC(9,4)         null,
eq_date              DATE                 null,
woff_date            DATE                 null,
primary key (eq_id)
);

/*==============================================================*/
/* Table : equipment2                                           */
/*==============================================================*/
create table equipment2 (
dep_id               INT4                 not null,
eq_id                INT4                 not null,
primary key (dep_id, eq_id),
foreign key (dep_id) references department (dep_id)
   on delete restrict on update restrict,
foreign key (eq_id) references equipment (eq_id)
   on delete restrict on update restrict
);

/*==============================================================*/
/* Table : number_values                                        */
/*==============================================================*/
create table number_values (
n_id                 INT4                 not null,
n_tint               INT2                 null,
n_sint               INT2                 null,
n_int                INT4                 null,
n_bdecimal           INT8                 null,
n_numeric            NUMERIC(9,4)         null,
n_float              FLOAT8               null,
n_real               FLOAT4               null,
n_dprecission        FLOAT8               null,
n_money              MONEY                null,
primary key (n_id)
);

/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table people (
p_id                 SERIAL               not null,
p_dep_id             INT2                 null,
p_name               VARCHAR(40)          null,
p_begin_work         TIME without time zone null,
p_end_work           TIME without time zone null,
p_picture            BYTEA	          null,
p_resume             TEXT                 null,
p_redundant          INT2                 null,
primary key (p_id),
foreign key (p_dep_id) references department (dep_id)
   on delete restrict on update restrict
);

/*==============================================================*/
/* Table : string_values                                        */
/*==============================================================*/
create table string_values (
s_id                 INT4                 not null,
s_char               CHAR(255)            null,
s_varchar            VARCHAR(255)         null,
s_nchar              CHAR(255)            null,
s_nvarchar           VARCHAR(255)         null,
s_bit                CHAR(255)            null,
s_varbit             CHAR(1024)           null,
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
/* Table : cargo                                                */
/*==============================================================*/
create table cargo (
c_id                 SERIAL               not null,
c_dep_id             INT2                 null,
c_name               CHAR(10)             null,
c_seal               INT2                 null,
c_date_came          TIMESTAMP without time zone null,
c_date_out           TIMESTAMP without time zone null,
c_weight             FLOAT8               null,
c_width              INT4                 null,
c_height             INT4                 null,
c_cost               FLOAT4               null,
c_attributes         CHAR(10)             null,
primary key (c_id),
foreign key (c_dep_id) references department (dep_id)
   on delete restrict on update restrict
);

/*==============================================================*/
/* Table : Case_Sensitive                                       */
/*==============================================================*/
create table "Case_Sensitive" (
cs_id                 INT4                        not null,
"Cs_Data1"            INT4,
"cs_data1"            INT4,
"cs data1"            INT4,
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
cs_id                 INT4                        not null,
"CS_DATA1"            INT4,
"CS_Data2"            INT4,
"Cs_Data3"            INT4,
primary key (cs_id)
);

/*==============================================================*/
/* Table : high_load                                            */
/*==============================================================*/
create table high_load (
hl_id		      SERIAL,
stBoolean             BOOLEAN,
stByte                INT2,
stShort               INT2,
stInteger             INTEGER,
stLong                INT8,
stFloat               FLOAT4,
stDouble              FLOAT8,
stBigDecimal          NUMERIC(18,4),
stString              VARCHAR(100),
stUnicodeString       VARCHAR(255),
stBytes               BYTEA,
stDate                DATE,
stTime                TIME without time zone,
stTimestamp           TIMESTAMP without time zone,
stGUID                CHAR(38),
stAsciiStream         TEXT,
stUnicodeStream       TEXT,
stBinaryStream        BYTEA,
primary key (hl_id)
);

/*==============================================================*/
/* Table : bcd_values                                           */
/*==============================================================*/
create table bcd_values
(
   id                             INTEGER NOT NULL,
   curr18_4                       NUMERIC(18,4),
   curr15_2                       NUMERIC(15,2),
   curr10_4                       NUMERIC(10,4),
   curr4_4                        NUMERIC(4,4),
   bigd18_1                       NUMERIC(18,1),
   bigd18_5                       NUMERIC(18,5),
   bigd12_10                      NUMERIC(12,10),
   bigd18_18                      NUMERIC(18,18),
   primary key (id)
);

DELIMITER //
/*==============================================================*/
/* Stored procedure: procedure1                                 */
/*==============================================================*/

CREATE FUNCTION procedure1(INT4) RETURNS integer
    AS 'SELECT 1 AS RESULT;'
    LANGUAGE SQL;
//

/*==============================================================*/
/* Stored procedure: procedure2                                 */
/*==============================================================*/

CREATE FUNCTION procedure2() RETURNS refcursor AS '
DECLARE
        ref refcursor;
BEGIN
        OPEN ref FOR SELECT eq_name FROM equipment;
        RETURN ref;
END;
' LANGUAGE 'plpgsql';
//

/*==============================================================*/
/* Stored procedure: ABTEST                                     */
/*==============================================================*/

CREATE FUNCTION "ABTEST" (
  p1 integer,
  p2 integer,
  p3 varchar,
  out p4 integer,
  out p5 varchar
)
RETURNS record AS
$body$
BEGIN
	p4 = p1 * 10 + p2;
	p5 = p3 || p3;
END;
$body$
LANGUAGE 'plpgsql';
//

/*==============================================================*/
/* Stored procedure: proc_nonames                               */
/*==============================================================*/

CREATE FUNCTION proc_nonames (
  integer,
  integer,
  out integer
)
RETURNS integer AS
'SELECT $1 + $2'
LANGUAGE 'sql';
//

/*==============================================================*/
/* Stored procedure: proc_onename                               */
/*==============================================================*/

CREATE FUNCTION proc_onename (
  p1 integer,
  integer,
  out integer
)
RETURNS integer AS
'SELECT $1 + $2'
LANGUAGE 'sql';
//

/*==============================================================*/
/* Stored procedure: proc_noout                                 */
/*==============================================================*/

CREATE FUNCTION proc_noout (
  p1 integer,
  integer
)
RETURNS integer AS
'SELECT $1 + $2'
LANGUAGE 'sql';
//

/*==============================================================*/
/* Stored procedure: proc_mixedorder                             */
/*==============================================================*/

CREATE FUNCTION proc_mixedorder (
  OUT p1 integer,
  INOUT p2 integer,
  p3 integer
) AS
'SELECT $1 + $2, $1 * $2'
LANGUAGE 'sql';
//

/*==============================================================*/
/* Stored procedure: proc_composite                             */
/*==============================================================*/

CREATE TYPE compositetype AS (f1 integer, f2 integer);
//

CREATE FUNCTION proc_composite (
  p1 integer,
  p2 integer
)
RETURNS compositetype AS
'SELECT cast(($1, $2) as compositetype)'
LANGUAGE 'sql';
//

/*==============================================================*/
/* Stored procedure: proc_set                                   */
/*==============================================================*/

CREATE FUNCTION proc_set ()
RETURNS SETOF VARCHAR AS
'SELECT eq_name FROM equipment'
LANGUAGE 'sql';
//

DELIMITER ;

/*==============================================================*/
/* Grant privileges to columns                                  */
/*==============================================================*/
--grant update(p_resume, p_redundant) on people to postgres;

/*==============================================================*/
/* Grant privileges to table                                    */
/*==============================================================*/
--grant select on people to postgres;