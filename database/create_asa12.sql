/*==============================================================*/
/* Table : blob_values                                          */
/*==============================================================*/
create table blob_values (
b_id                 INTEGER                        not null	default autoincrement,
b_text               long varchar					null,
b_image              long binary					null,
primary key (b_id)
);

/*==============================================================*/
/* Table : cargo                                                */
/*==============================================================*/
create table cargo (
c_id                 INTEGER                        not null	default autoincrement,
c_dep_id             SMALLINT						null,
c_name               CHAR(10)						null,
c_seal               SMALLINT						null,
c_date_came          TIMESTAMP						null,
c_date_out           TIMESTAMP						null,
c_weight             FLOAT							null,
c_width              INTEGER						null,
c_height             INTEGER						null,
c_cost               NUMERIC(16,4)					null,
c_attributes         long varchar					null,
primary key (c_id)
);

/*==============================================================*/
/* Table : date_values                                          */
/*==============================================================*/
create table date_values (
d_id                 INTEGER                        not null	default autoincrement,
d_date               DATE							null,
d_time               TIME							null,
d_datetime           TIMESTAMP						null,
d_timestamp          TIMESTAMP						null,
primary key (d_id)
);

/*==============================================================*/
/* Table : department                                           */
/*==============================================================*/
create table department (
dep_id               SMALLINT                       not null	default autoincrement,
dep_name             VARCHAR(20)					null,
dep_shname           CHAR(5)						null,
dep_address          VARCHAR(255)					null,
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
eq_id                INTEGER                        not null	default autoincrement,
eq_name              VARCHAR(30)					null,
eq_type              SMALLINT						null,
eq_cost              NUMERIC(13,4)					null,
eq_date              DATE							null,
woff_date            DATE							null,
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
n_id                 INTEGER                        not null	default autoincrement,
n_tint               SMALLINT						null,
n_sint               SMALLINT						null,
n_int                INTEGER						null,
n_bdecimal           DECIMAL(18)					null,
n_numeric            NUMERIC(13,4)					null,
n_float              FLOAT							null,
n_real               FLOAT							null,
n_dprecission        DOUBLE							null,
n_money              NUMERIC(10,2)					null,
primary key (n_id)
);

/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table people (
p_id                 SMALLINT                       not null	default autoincrement,
p_dep_id             SMALLINT						null,
p_name               VARCHAR(40)					null,
p_begin_work         TIME							null,
p_end_work           TIME							null,
p_picture            long binary					null,
p_resume             long varchar					null,
p_redundant          SMALLINT						null,
primary key (p_id)
);

create index people_FK on dba.people (p_dep_id);

/*==============================================================*/
/* Table : string_values                                        */
/*==============================================================*/
create table string_values
(
   s_id                           int              	not null	default autoincrement,
   s_char                         CHAR(255)			null,
   s_varchar                      VARCHAR(255)		null,
   s_nchar                        CHAR(255)			null,
   s_nvarchar                     NVARCHAR(255)		null,
   s_bit                          long varchar		null,
   s_varbit                       long binary		null,
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
/*create table "Case_Sensitive" (
cs_id                 INTEGER                        not null	default autoincrement,
"Cs_Data1"            INTEGER,
"cs_data1"            INTEGER,
"cs data1"            INTEGER,
primary key (cs_id)
);*/

/*==============================================================*/
/* Table : case_sensitive                                       */
/*==============================================================*/
/*create table case_sensitive (
cs_id                 INTEGER                        not null	default autoincrement,
"CS_DATA1"            INTEGER,
"CS_Data2"            INTEGER,
"Cs_Data3"            INTEGER,
primary key (cs_id)
);*/

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
/* Table : high_load                                            */
/*==============================================================*/
create table high_load (
hl_id		      INTEGER NOT NULL default autoincrement,
stBoolean             BIT,
stByte                TINYINT,
stShort               SMALLINT,
stInteger             INTEGER,
stLong                BIGINT,
stFloat               FLOAT,
stDouble              DOUBLE,
stBigDecimal          DECIMAL(18,4),
stString              VARCHAR(100),
stUnicodeString       NVARCHAR(255),
stBytes               VARBINARY(100),
stDate                DATE,
stTime                TIME,
stTimestamp           TIMESTAMP,
stGUID                UNIQUEIDENTIFIER,
stAsciiStream         LONG VARCHAR,
stUnicodeStream       LONG NVARCHAR,
stBinaryStream        LONG BINARY,
primary key (hl_id)
);

/*==============================================================*/
/* Table : default_values                                       */
/*==============================================================*/
create table default_values
(
   d_id                           INTEGER NOT NULL		default autoincrement,
   d_fld1                         INTEGER DEFAULT 123456,
   d_fld2                         FLOAT DEFAULT 123.456,
   d_fld3                     	  VARCHAR(10) DEFAULT 'xyz',
   d_fld4                     	  DATE default '2003-12-11',
   d_fld5                     	  TIME default '23:12:11',
   d_fld6                     	  TIMESTAMP default '2003-12-11 23:12:11',
   primary key (d_id)
);

/*==============================================================*/
/* Table : default_values2                                      */
/*==============================================================*/
create table default_values2
(
   d_id                           INTEGER NOT NULL		default autoincrement,
   d_fld1                         FLOAT DEFAULT 123.456,
   d_fld2                         INTEGER DEFAULT 123456,
   d_fld3                     	  DATE default '2003-12-11',
   d_fld4                     	  VARCHAR(10) DEFAULT 'xyz',
   d_fld5                     	  TIMESTAMP default '2003-12-11 23:12:11',
   d_fld6                     	  TIME default '23:12:11',
   primary key (d_id)
);

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

/*==============================================================*/
/* Table : domain_values                                        */
/*==============================================================*/

create DOMAIN tinteger INTEGER default 123456;
create DOMAIN tfloat FLOAT default 123.456;
create DOMAIN tstring VARCHAR(10) default 'xyz';

create table domain_values
(
   d_id                       INTEGER not null	default autoincrement,
   d_fld1                     tinteger null,
   d_fld2                     tfloat null,
   d_fld3                     tstring null,
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
/* Stored procedure: procedure1                                 */
/*==============================================================*/

CREATE PROCEDURE PROCEDURE1(IN P1 INTEGER,OUT R1 INTEGER)
BEGIN
  SET R1=P1 + 1
END;

/*==============================================================*/
/* Stored procedure: procedure2                                 */
/*==============================================================*/

CREATE PROCEDURE PROCEDURE2()
   RESULT(R1 VARCHAR(30))
BEGIN
  SELECT eq_name as R1 FROM equipment ORDER BY eq_name
END;

/*==============================================================*/
/* Grant privileges to columns                                  */
/*==============================================================*/
grant update(p_resume, p_redundant) on people to DBA;

/*==============================================================*/
/* Grant privileges to table                                    */
/*==============================================================*/
grant select on people to DBA;
