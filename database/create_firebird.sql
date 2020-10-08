/*==============================================================*/
/* Table : blob_values                                          */
/*==============================================================*/
create table blob_values (
b_id                 INTEGER                        not null,
b_text               BLOB SUB_TYPE TEXT,
b_image              BLOB,
primary key (b_id)
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
c_attributes         BLOB SUB_TYPE TEXT,
primary key (c_id)
);

/*==============================================================*/
/* Table : date_values                                          */
/*==============================================================*/
create table date_values (
d_id                 INTEGER                        not null,
d_date               DATE,
d_time               TIME,
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
n_bdecimal           DECIMAL(18),
n_numeric            NUMERIC(9,4),
n_float              FLOAT,
n_real               FLOAT,
n_dprecission        DOUBLE PRECISION,
n_money              NUMERIC(8,2),
primary key (n_id)
);

/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table people (
p_id                 SMALLINT                       not null,
p_dep_id             SMALLINT,
p_name               VARCHAR(40),
p_begin_work         TIME,
p_end_work           TIME,
p_picture            BLOB,
p_resume             BLOB SUB_TYPE TEXT,
p_redundant          SMALLINT,
primary key (p_id)
);

/*==============================================================*/
/* Table : string_values                                        */
/*==============================================================*/
create table string_values
(
   s_id                           int              not null,
   s_char                         CHAR(255),
   s_varchar                      VARCHAR(255),
   s_nchar                        CHAR(255),
   s_nvarchar                     VARCHAR(255),
   s_bit                          BLOB SUB_TYPE TEXT,
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
hl_id		      INTEGER NOT NULL,
stBoolean             CHAR(1),
stByte                SMALLINT,
stShort               SMALLINT,
stInteger             INTEGER,
stLong                BIGINT,
stFloat               FLOAT,
stDouble              DOUBLE PRECISION,
stBigDecimal          DECIMAL(18,4),
stString              CHAR(100),
stUnicodeString       VARCHAR(255),
stBytes               CHAR(100) CHARACTER SET OCTETS,
stDate                DATE,
stTime                TIME,
stTimestamp           TIMESTAMP,
stGUID                CHAR(38),
stAsciiStream         BLOB SUB_TYPE TEXT,
stUnicodeStream       BLOB SUB_TYPE TEXT,
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
   d_fld5                     	  TIME default '23:12:11',
   d_fld6                     	  TIMESTAMP default '2003-12-11 23:12:11',
   primary key (d_id)
);

create table default_values2
(
   d_id                           INTEGER NOT NULL,
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

create domain tinteger INT default 123456;
create domain tfloat FLOAT default 123.456;
create domain tstring VARCHAR(10) default 'xyz';

create table domain_values
(
   d_id                       INTEGER not null,
   d_fld1                     tinteger,
   d_fld2                     tfloat,
   d_fld3                     tstring,
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
/* Table : Guids                                                */
/*==============================================================*/

CREATE DOMAIN DOM_GUID CHAR(16) CHARACTER SET OCTETS;

CREATE TABLE Guids (
    ID               INTEGER NOT NULL,
    GUID_DOM_FIELD   DOM_GUID,
    GUID_TYPE_FIELD  CHAR(16) CHARACTER SET OCTETS
);

/*==============================================================*/
/* Table : insert_returning                                     */
/*==============================================================*/

create table insert_returning
(
   id                INTEGER not null,
   fld               VARCHAR(10),
   primary key (id)
);

/*==============================================================*/
/* Generator : GEN_ID                                           */
/*==============================================================*/

CREATE GENERATOR GEN_ID;

/*==============================================================*/
/* Start PSQL section                                           */
/*==============================================================*/

SET TERM ^ ;

/*==============================================================*/
/* Trigger : insert_returning_bi                                */
/*==============================================================*/

create trigger insert_returning_bi FOR insert_returning
ACTIVE BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.ID IS NULL OR NEW.ID = 0) THEN
    select Coalesce(Max(ID), 0)+1 from insert_returning
      into NEW.ID;
  NEW.FLD = 'ID' || NEW.ID;
END
^

/*==============================================================*/
/* Stored procedure: procedure1                                 */
/*==============================================================*/

CREATE PROCEDURE PROCEDURE1(P1 INTEGER)
   RETURNS(R1 INTEGER)
AS
BEGIN
  R1 = P1 + 1;
SUSPEND;
END
^

/*==============================================================*/
/* Stored procedure: procedure2                                 */
/*==============================================================*/

CREATE PROCEDURE PROCEDURE2
   RETURNS(R1 VARCHAR(30))
AS
BEGIN
  FOR SELECT eq_name FROM equipment ORDER BY eq_name INTO :R1
  DO
  SUSPEND;
END
^

/*==============================================================*/
/* Stored procedure: procedure_upd_people_A                     */
/*==============================================================*/

CREATE PROCEDURE procedure_upd_people_A
   RETURNS(R1 Int)
AS
BEGIN
  FOR SELECT p_id FROM people ORDER BY p_id INTO :R1
  DO begin
    update people set p_id = p_id where p_id = :R1;
    SUSPEND;
  end
END
^

/*==============================================================*/
/* Stored procedure: procedure_upd_people_B                     */
/*==============================================================*/

CREATE PROCEDURE procedure_upd_people_B
   RETURNS(R1 Int)
AS
BEGIN
  FOR SELECT p_id FROM people ORDER BY p_id INTO :R1
  DO SUSPEND;
  update people set p_id = p_id;
END
^

/*==============================================================*/
/* Stored procedure: procedure_upd_people_C                     */
/*==============================================================*/

CREATE PROCEDURE procedure_upd_people_C
   RETURNS(R1 Int)
AS
BEGIN
  update people set p_id = p_id;
  FOR SELECT p_id FROM people ORDER BY p_id INTO :R1
  DO SUSPEND;
END
^

/*==============================================================*/
/* Stored procedure: ABTEST                                     */
/*==============================================================*/

create or alter procedure abtest (
    P1 integer,
    P2 integer,
    P3 varchar(10))
returns (
    P4 integer,
    P5 varchar(20))
as
begin
  P4 = P1 * 10 + P2;
  P5 = P3 || P3;
end
^ 

/*==============================================================*/
/* Stored procedure: ABTEST_BIGINT                              */
/*==============================================================*/

create or alter procedure abtest_bigint (
    P1 bigint,
    P2 bigint,
    P3 varchar(10))
returns (
    P4 bigint,
    P5 varchar(30))
as
begin
  P4 = P1 * 10 + P2;
  P5 = P3 || P3;
end
^ 

/*==============================================================*/
/* Stored procedure: GUIDTEST                                   */
/*==============================================================*/
CREATE OR ALTER PROCEDURE GUIDTEST (
    G_IN DOM_GUID)
RETURNS (
    G_OUT DOM_GUID)
AS
begin
  G_OUT = :G_IN;
  suspend;
end
^

/*==============================================================*/
/* Finish PSQL section                                          */
/*==============================================================*/

SET TERM ; ^

/*==============================================================*/
/* Grant privileges to columns                                  */
/*==============================================================*/
grant update(p_resume, p_redundant) on people to SYSDBA;

/*==============================================================*/
/* Grant privileges to table                                    */
/*==============================================================*/
grant select on people to SYSDBA;
