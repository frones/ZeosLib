/*==============================================================*/
/* Database name:  Sybase                                       */
/* DBMS name:      Sybase Adaptive Server Enterprise 12.5       */
/* Created on:     17.02.2003 20:22:43                          */
/*==============================================================*/

set quoted_identifier on
go

if exists (select 1
            from  sysindexes
           where  id    = object_id('cargo')
            and   name  = 'cargo_FK'
            and   indid > 0
            and   indid < 255)
   drop index cargo.cargo_FK
go


if exists (select 1
            from  sysindexes
           where  id    = object_id('equipment2')
            and   name  = 'equipment2_FK'
            and   indid > 0
            and   indid < 255)
   drop index equipment2.equipment2_FK
go


if exists (select 1
            from  sysindexes
           where  id    = object_id('equipment2')
            and   name  = 'equipment_FK'
            and   indid > 0
            and   indid < 255)
   drop index equipment2.equipment_FK
go


if exists (select 1
            from  sysindexes
           where  id    = object_id('people')
            and   name  = 'people_FK'
            and   indid > 0
            and   indid < 255)
   drop index people.people_FK
go


if exists (select 1
            from  sysobjects
           where  id = object_id('blob_values')
            and   type = 'U')
   drop table blob_values
go


if exists (select 1
            from  sysobjects
           where  id = object_id('cargo')
            and   type = 'U')
   drop table cargo


if exists (select 1
            from  sysobjects
           where  id = object_id('date_values')
            and   type = 'U')
   drop table date_values
go

if exists (select 1
            from  sysobjects
           where  id = object_id('default_values')
            and   type = 'U')
   drop table default_values
go

if exists (select 1
            from  sysobjects
           where  id = object_id('default_values2')
            and   type = 'U')
   drop table default_values2
go

if exists (select 1
            from  sysobjects
           where  id = object_id('department')
            and   type = 'U')
   drop table department
go


if exists (select 1
            from  sysobjects
           where  id = object_id('equipment')
            and   type = 'U')
   drop table equipment
go


if exists (select 1
            from  sysobjects
           where  id = object_id('equipment2')
            and   type = 'U')
   drop table equipment2
go



if exists (select 1
            from  sysobjects
           where  id = object_id('number_values')
            and   type = 'U')
   drop table number_values
go


if exists (select 1
            from  sysobjects
           where  id = object_id('people')
            and   type = 'U')
   drop table people
go


if exists (select 1
            from  sysobjects
           where  id = object_id('string_values')
            and   type = 'U')
   drop table string_values
go

if exists (select 1
            from  sysobjects
           where  id = object_id('not_null_values')
            and   type = 'U')
   drop table string_values
go

if exists (select 1
            from  sysobjects
           where  id = object_id('"Case_Sensitive"')
            and   type = 'U')
   drop table string_values
go

if exists (select 1
            from  sysobjects
           where  id = object_id('"Spaced Names"')
            and   type = 'U')
   drop table string_values
go

if exists (select 1
            from  sysobjects
           where  id = object_id('case_sensitive')
            and   type = 'U')
   drop table string_values
go

if exists (select 1
            from  sysobjects
           where  id = object_id('high_load')
            and   type = 'U')
   drop table string_values
go

/*==============================================================*/
/* Table : blob_values                                          */
/*==============================================================*/
create table blob_values (
b_id                 int                  not null,
b_text               text                 null,
b_image              image                null,
primary key  (b_id)
)
go


/*==============================================================*/
/* Table : cargo                                                */
/*==============================================================*/
create table cargo (
c_id                 int                  not null,
c_dep_id             smallint             null,
c_name               char(10)             null,
c_seal               tinyint              null,
c_date_came          datetime             null,
c_date_out           datetime             null,
c_weight             float                null,
c_width              int                  null,
c_height             int                  null,
c_cost               money                null,
c_attributes         binary(10)           null,
primary key  (c_id)
)
go


/*==============================================================*/
/* Index: cargo_FK                                              */
/*==============================================================*/
create   index cargo_FK on cargo (
c_dep_id
)
go


/*==============================================================*/
/* Table : date_values                                          */
/*==============================================================*/
create table date_values (
d_id                 int                  not null,
d_date               datetime             null,
d_time               datetime             null,
d_datetime           datetime             null,
d_timestamp          datetime             null,
constraint PK_DATE_VALUES primary key  (d_id)
)
go

/*==============================================================*/
/* Table : default_values                                       */
/*==============================================================*/
create table default_values(
   d_id                           int 	      not null,
   d_fld1                         int 	      default 123456,
   d_fld2                         float       default 123.456,
   d_fld3                     	  varchar(10) default 'xyz',
   d_fld4                     	  datetime    default '2003-12-11',
   d_fld5                     	  datetime    default '23:12:11',
   d_fld6                     	  datetime    default '2003-12-11 23:12:11',
   constraint PK_DEFAULT_VALUES primary key (d_id)
)
go

/*==============================================================*/
/* Table : default_values2                                       */
/*==============================================================*/
create table default_values2(
   d_id                           int 	      not null,
   d_fld1                         float       default 123.456,
   d_fld2                         int 	      default 123456,
   d_fld3                     	  datetime    default '2003-12-11',
   d_fld4                     	  varchar(10) default 'xyz',
   d_fld5                     	  datetime    default '2003-12-11 23:12:11',
   d_fld6                     	  datetime    default '23:12:11',
   constraint PK_DEFAULT_VALUES2 primary key (d_id)
)
go

/*==============================================================*/
/* Table : department                                           */
/*==============================================================*/
create table department (
dep_id               smallint             not null,
dep_name             varchar(20)          null,
dep_shname           char(5)              null,
dep_address          varchar(255)         null,
primary key  (dep_id)
)
go


/*==============================================================*/
/* Table : equipment                                            */
/*==============================================================*/
create table equipment (
eq_id                int                  not null,
eq_name              varchar(30)          null,
eq_type              smallint             null,
eq_cost              numeric(9,4)         null,
eq_date              datetime             null,
woff_date            datetime             null,
primary key  (eq_id)
)
go


/*==============================================================*/
/* Table : equipment2                                           */
/*==============================================================*/
create table equipment2 (
dep_id               smallint             not null,
eq_id                int                  not null,
primary key  (dep_id, eq_id)
)
go


/*==============================================================*/
/* Index: equipment_FK                                          */
/*==============================================================*/
create   index equipment_FK on equipment2 (
dep_id
)
go


/*==============================================================*/
/* Index: equipment2_FK                                         */
/*==============================================================*/
create   index equipment2_FK on equipment2 (
eq_id
)
go


/*==============================================================*/
/* Table : number_values                                        */
/*==============================================================*/
create table number_values (
n_id                 int                  not null,
n_tint               tinyint              null,
n_sint               smallint             null,
n_int                int                  null,
n_bdecimal           int                  null,
n_numeric            numeric(9,4)         null,
n_float              float                null,
n_real               real                 null,
n_dprecission        double precision     null,
n_money              money                null,
primary key  (n_id)
)
go


/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table people (
p_id                 smallint             not null,
p_dep_id             smallint             null,
p_name               varchar(40)          null,
p_begin_work         datetime             null,
p_end_work           datetime             null,
p_picture            image                null,
p_resume             text                 null,
p_redundant          tinyint           	  null,
primary key  (p_id)
)
go


/*==============================================================*/
/* Index: people_FK                                             */
/*==============================================================*/
create   index people_FK on people (
p_dep_id
)
go


/*==============================================================*/
/* Table : string_values                                        */
/*==============================================================*/
create table string_values (
s_id                 int                  not null,
s_char               char(255)            null,
s_varchar            varchar(255)         null,
s_nchar              unichar(255)            null,
s_nvarchar           unichar(255)         null,
s_bit                binary(255)          null,
s_varbit             varbinary(1024)      null,
primary key  (s_id)
)
go

/*==============================================================*/
/* Table : not_null_values                                      */
/*==============================================================*/
create table not_null_values
(
   n_id                           int              	not null,
   n_varchar                      VARCHAR(255)		not null,
   primary key (n_id)
)
go

/*==============================================================*/
/* Table : Case_Sensitive                                       */
/*==============================================================*/
create table "Case_Sensitive" (
cs_id                 INTEGER                        not null,
"Cs_Data1"            INTEGER		null,
"cs_data1"            INTEGER		null,
"cs data1"            INTEGER		null,
primary key (cs_id)
)
go

/*==============================================================*/
/* Table : Spaced Names                                         */
/*==============================================================*/
create table "Spaced Names" (
cs_id                 INTEGER                        not null,
"Cs Data1"            INTEGER,
"cs data2"            INTEGER,
"cS data3"            INTEGER,
primary key (cs_id)
)
go

/*==============================================================*/
/* Table : case_sensitive                                       */
/*==============================================================*/
create table case_sensitive (
cs_id                 INTEGER                        not null,
"CS_DATA1"            INTEGER		null,
"CS_Data2"            INTEGER		null,
"Cs_Data3"            INTEGER		null,
primary key (cs_id)
)
go

/*==============================================================*/
/* Table : high_load                                            */
/*==============================================================*/
create table high_load (
hl_id		      INTEGER NOT NULL,
stBoolean             BIT,
stByte                TINYINT null,
stShort               SMALLINT null,
stInteger             INTEGER null,
stLong                BIGINT null,
stFloat               FLOAT null,
stDouble              double precision null,
stBigDecimal          money null,
stString              VARCHAR(100) null,
stUnicodeString       NVARCHAR(255) null,
stBytes               VARBINARY(100) null,
stDate                datetime null,
stTime                datetime null,
stTimestamp           datetime null,
stGUID                varbinary(16) null /*UNIQUEIDENTIFIER*/,
stAsciiStream         text null,
stUnicodeStream       text null,
stBinaryStream        IMAGE null,
primary key (hl_id)
)
go

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
)
go

alter table cargo
   add foreign key (c_dep_id) references department (dep_id)
go


alter table equipment2
   add foreign key (dep_id) references department (dep_id)
go


alter table equipment2
   add foreign key (eq_id) references equipment (eq_id)
go


alter table people
   add foreign key (p_dep_id) references department (dep_id)
go


/*
SET ANSI_NULLS ON 
go

CREATE PROCEDURE [ABTEST]
@p1 int,
@p2 int,
@p3 varchar(10),
@p4 int output,
@p5 varchar(20) output
AS
select @p4 = @p1 * 10 + @p2;
select @p5 = @p3 + @p3; 
go
*/

