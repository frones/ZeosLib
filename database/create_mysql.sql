/*==============================================================*/
/* Database name:  MySql                                        */
/* DBMS name:      MySQL 3.23                                   */
/* Created on:     04.02.2003 19:48:39                          */
/*==============================================================*/

/*==============================================================*/
/* Table : blob_values                                          */
/*==============================================================*/
create table blob_values
(
   b_id                           int not null,
   b_text                         longtext,
   b_image                        longblob   
);

/*INSERT INTO blob_values VALUES (1, LOAD_FILE('text/lgpl.txt'), LOAD_FILE('images/dogs.jpg'));*/

/*==============================================================*/
/* Table : date_values                                          */
/*==============================================================*/
create table date_values
(
   d_id                           int                    not null,
   d_date                         date,
   d_time                         time,
   d_datetime                     datetime,
   d_timestamp                    timestamp NULL DEFAULT NULL, /*skip the mysql automation of CURRENT_TIMESTAMP*/
   primary key (d_id)
);

/*==============================================================*/
/* Table : default_values                                          */
/*==============================================================*/
create table default_values
(
   d_id                           int not null auto_increment,
   d_fld1                         int default 123456,
   d_fld2                         float default 123.456,
   d_fld3                     	  varchar(10) default 'xyz',
   d_fld4                     	  date default '2003-12-11',
   d_fld5                     	  time default '23:12:11',
   d_fld6                     	  datetime default '2003-12-11 23:12:11',
   primary key (d_id)
);

/*==============================================================*/
/* Table : default_values                                          */
/*==============================================================*/
create table default_values2
(
   d_id                           int not null auto_increment,
   d_fld1                         float default 123.456,
   d_fld2                         int default 123456,
   d_fld3                     	  date default '2003-12-11',
   d_fld4                     	  varchar(10) default 'xyz',
   d_fld5                     	  datetime default '2003-12-11 23:12:11',
   d_fld6                     	  time default '23:12:11',
   primary key (d_id)
);

/*==============================================================*/
/* Table : department                                           */
/*==============================================================*/
create table department
(
   dep_id                         smallint not null auto_increment,
   dep_name                       varchar(20),
   dep_shname                     char(5),
   dep_address                    varchar(255),
   primary key (dep_id)
);

/*==============================================================*/
/* Table : equipment                                            */
/*==============================================================*/
create table equipment
(
   eq_id                          int not null auto_increment,
   eq_name                        varchar(30),
   eq_type                        smallint,
   eq_cost                        numeric(9,4),
   eq_date                        date,
   woff_date                      date,
   primary key (eq_id)
);

/*==============================================================*/
/* Table : equipment2                                           */
/*==============================================================*/
create table equipment2
(
   dep_id                         smallint not null,
   eq_id                          int not null,
   primary key (dep_id, eq_id)   
);

/*==============================================================*/
/* Index: equipment_FK                                          */
/*==============================================================*/
create index equipment_FK on equipment2 (dep_id);

/*==============================================================*/
/* Index: equipment2_FK                                         */
/*==============================================================*/
create index equipment2_FK on equipment2 (eq_id);

/*==============================================================*/
/* Table : extension                                            */
/*==============================================================*/
create table extension
(
   ext_id                         CHAR(10),
   ext_set1                       SET('Y', 'N'),
   ext_set2                       SET('White', 'Black', 'Yellow'),
   ext_enum                       ENUM('Car', 'House', 'Work', 'Dog', 'Wife', 'Child')
);

/*==============================================================*/
/* Table : number_values                                        */
/*==============================================================*/
create table number_values
(
   n_id                           int not null,
   n_tint                         tinyint,
   n_sint                         smallint,
   n_int                          int,
   n_bdecimal                     bigint,
   n_numeric                      numeric(9,4),
   n_float                        float,
   n_real                         real,
   n_dprecission                  double,
   n_money                        float(8,2),
   primary key (n_id)
);

/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table people
(
   p_id                           smallint  not null auto_increment,
   p_dep_id                       smallint,
   p_name                         varchar(40),
   p_begin_work                   time,
   p_end_work                     time,
   p_picture                      longblob,
   p_resume                       text,
   p_redundant                    tinyint(1),
   primary key (p_id)   
);

/*==============================================================*/
/* Index: people_FK                                             */
/*==============================================================*/
create index people_FK on people (p_dep_id);

/*==============================================================*/
/* Table : string_values                                        */
/*==============================================================*/
create table string_values
(
   s_id                           int                            not null,
   s_char                         char(255),
   s_varchar                      varchar(255),
   s_nchar                        char(255),
   s_nvarchar                     varchar(255),
   s_bit                          blob,
   s_varbit                       longblob,
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
create table cargo
(
   c_id                           bigint not null auto_increment,
   c_dep_id                       smallint,
   c_name                         CHAR(10),
   c_seal                         tinyint(1),
   c_date_came                    datetime,
   c_date_out                     datetime,
   c_weight                       float,
   c_width                        int,
   c_height                       int,
   c_cost                         float(12,4),
   c_attributes                   blob,
   primary key (c_id)   
);

/*==============================================================*/
/* Table : Spaced Names                                         */
/*==============================================================*/
create table `Spaced Names` (
cs_id                 INTEGER                        not null,
`Cs Data1`            INTEGER,
`cs data2`            INTEGER,
`cS data3`            INTEGER,
primary key (cs_id)
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
/* Table : high_load                                            */
/*==============================================================*/
create table high_load (
hl_id		      INTEGER NOT NULL,
stBoolean             BIT(1),
stByte                TINYINT UNSIGNED,
stShort               SMALLINT,
stInteger             INTEGER,
stLong                BIGINT(20),
stFloat               FLOAT,
stDouble              DOUBLE,
stBigDecimal          DECIMAL(18,4),
stString              VARCHAR(100),
stUnicodeString       VARCHAR(255),
stBytes               VARBINARY(100),
stDate                DATE,
stTime                TIME,
stTimestamp           TIMESTAMP,
stGUID                CHAR(38),
stAsciiStream         TEXT,
stUnicodeStream       TEXT,
stBinaryStream        BLOB,
primary key (hl_id)
);

/*==============================================================*/
/* Tables for testing Bit_Fields                                */
/*==============================================================*/
CREATE TABLE TEST_BIT_FIELDS (
ID int(11) NOT NULL,
B1 BIT(1),
B2 BIT(2),
B3 BIT(3),
B4 BIT(4),
B5 BIT(5),
B6 BIT(6),
B7 BIT(7),
B8 BIT(8),
B9 BIT(9),
B10 BIT(10),
B11 BIT(11),
B12 BIT(12),
B13 BIT(13),
B14 BIT(14),
B15 BIT(15),
B16 BIT(16),
B17 BIT(17),
B18 BIT(18),
B19 BIT(19),
B20 BIT(20),
B21 BIT(21),
B22 BIT(22),
B23 BIT(23),
B24 BIT(24),
B25 BIT(25),
B26 BIT(26),
B27 BIT(27),
B28 BIT(28),
B29 BIT(29),
B30 BIT(30),
B31 BIT(31),
B32 BIT(32),
B33 BIT(33),
B34 BIT(34),
B35 BIT(35),
B36 BIT(36),
B37 BIT(37),
B38 BIT(38),
B39 BIT(39),
B40 BIT(40),
B41 BIT(41),
B42 BIT(42),
B43 BIT(43),
B44 BIT(44),
B45 BIT(45),
B46 BIT(46),
B47 BIT(47),
B48 BIT(48),
B49 BIT(49),
B50 BIT(50),
B51 BIT(51),
B52 BIT(52),
B53 BIT(53),
B54 BIT(54),
B55 BIT(55),
B56 BIT(56),
B57 BIT(57),
B58 BIT(58),
B59 BIT(59),
B60 BIT(60),
B61 BIT(61),
B62 BIT(62),
B63 BIT(63),
B64 BIT(64),
PRIMARY KEY (ID)
);

/*==============================================================*/
/* Tables for TEST_MYSQL_BOOLEANS                               */
/*==============================================================*/

CREATE TABLE TEST_MYSQL_BOOLEANS
(
  id INT NOT NULL,
  fld1 ENUM('Y','N'),
  fld2 enum('n','y'),
  fld3 BIT(1),
  fld4 ENUM('Y','N') default 'Y',
  fld5 enum('n','y') default 'n',
  fld6 BIT(1) default 0,
  fld7 ENUM('Y','N') default 'N',
  fld8 enum('n','y') default 'Y',
  fld9 BIT(1) default b'1',
  PRIMARY KEY (id)
);

/*==============================================================*/
/* Index: cargo_FK                                              */
/*==============================================================*/
create index cargo_FK on cargo (c_dep_id);

SET GLOBAL max_allowed_packet=16*1024*1024;

DELIMITER //
/*==============================================================*/
/* Stored Procedure: ABTEST                                     */
/*==============================================================*/
CREATE PROCEDURE `ABTEST`(
        IN `P1` INTEGER,
        IN `P2` INTEGER,
        IN `P3` VARCHAR(10),
        OUT `P4` INTEGER,
        OUT `P5` VARCHAR(20)
    )
    DETERMINISTIC
    NO SQL
    SQL SECURITY DEFINER
    COMMENT ''
BEGIN
  set `P4` = `P1` * 10 + `P2`;
  set `P5` = concat(`P3`, `P3`);
END; //

CREATE PROCEDURE `TEST_All_TYPES`(
        INOUT `P1` TINYINT(4),
        INOUT `P2` TINYINT(1),
        INOUT `P3` SMALLINT(6),
        INOUT `P4` MEDIUMINT(9),
        INOUT `P5` INTEGER(11),
        INOUT `P6` BIGINT(20),
        INOUT `P7` DOUBLE,
        INOUT `P8` FLOAT,
        INOUT `P9` DECIMAL(19,4),
        INOUT `P10` DECIMAL(10,0),
        INOUT `P11` VARCHAR(100),
        INOUT `P12` DATE,
        INOUT `P13` TIME,
        INOUT `P14` YEAR(4),
        INOUT `P15` TIMESTAMP,
        INOUT `P16` DATETIME,
        INOUT `P17` TINYBLOB,
        INOUT `P18` BLOB,
        INOUT `P19` MEDIUMBLOB,
        INOUT `P20` LONGBLOB,
        INOUT `P21` TINYTEXT,
        INOUT `P22` TEXT,
        INOUT `P23` MEDIUMTEXT,
        INOUT `P24` LONGTEXT,
        INOUT `P25` VARBINARY(100),
        INOUT `P26` SET('a','b','c','d'),
        INOUT `P27` INT,
        INOUT `P28` INTEGER
    )
    DETERMINISTIC
    NO SQL
    SQL SECURITY DEFINER
    COMMENT ''
BEGIN
  DECLARE `FP1` TINYINT(4);
  DECLARE `FP2` TINYINT(1);
  DECLARE `FP3` SMALLINT(6);
  DECLARE `FP4` MEDIUMINT(9);
  DECLARE `FP5` INTEGER(11);
  DECLARE `FP6` BIGINT(20);
  DECLARE `FP7` DOUBLE;
  DECLARE `FP8` FLOAT;
  DECLARE `FP9` DECIMAL(19,4);
  DECLARE `FP10` DECIMAL(10,0);
  DECLARE `FP11` VARCHAR(100);
  DECLARE `FP12` DATE;
  DECLARE `FP13` TIME;
  DECLARE `FP14` YEAR(4);
  DECLARE `FP15` TIMESTAMP;
  DECLARE `FP16` DATETIME;
  DECLARE `FP17` TINYBLOB;
  DECLARE `FP18` BLOB;
  DECLARE `FP19` MEDIUMBLOB;
  DECLARE `FP20` LONGBLOB;
  DECLARE `FP21` TINYTEXT;
  DECLARE `FP22` TEXT;
  DECLARE `FP23` MEDIUMTEXT;
  DECLARE `FP24` LONGTEXT;
  DECLARE `FP25` VARBINARY(100);
  DECLARE `FP26` SET('a','b','c','d');
  DECLARE `FP27` INT;
  DECLARE `FP28` INTEGER;

  SET FP1 = `P1`;
  SET FP2 = `P2`;
  SET FP3 = `P3`;
  SET FP4 = `P4`;
  SET FP5 = `P5`;
  SET FP6 = `P6`;
  SET FP7 = `P7`;
  SET FP8 = `P8`;
  SET FP9 = `P9`;
  SET FP10 = `P10`;
  SET FP11 = `P11`;
  SET FP12 = `P12`;
  SET FP13 = `P13`;
  SET FP14 = `P14`;
  SET FP15 = `P15`;
  SET FP16 = `P16`;
  SET FP17 = `P17`;
  SET FP18 = `P18`;
  SET FP19 = `P19`;
  SET FP20 = `P20`;
  SET FP21 = `P21`;
  SET FP22 = `P22`;
  SET FP23 = `P23`;
  SET FP24 = `P24`;
  SET FP25 = `P25`;
  SET FP26 = `P26`;
  SET FP27 = `P27`;
  SET FP28 = `P28`;

  SET `P1` = FP1;
  SET `P2` = FP2;
  SET `P3` = FP3;
  SET `P4` = FP4;
  SET `P5` = FP5;
  SET `P6` = FP6;
  SET `P7` = FP7;
  SET `P8` = FP8;
  SET `P9` = FP9;
  SET `P10` = FP10;
  SET `P11` = FP11;
  SET `P12` = FP12;
  SET `P13` = FP13;
  SET `P14` = FP14;
  SET `P15` = FP15;
  SET `P16` = FP16;
  SET `P17` = FP17;
  SET `P18` = FP18;
  SET `P19` = FP19;
  SET `P20` = FP20;
  SET `P21` = FP21;
  SET `P22` = FP22;
  SET `P23` = FP23;
  SET `P24` = FP24;
  SET `P25` = FP25;
  SET `P26` = FP26;
  SET `P27` = FP27;
  SET `P28` = FP28;
END; //

CREATE FUNCTION FuncReturnInteger(`p_in` INT) RETURNS INTEGER
    DETERMINISTIC
	NO SQL
BEGIN
  DECLARE `Temp` INTEGER;
  set Temp = `p_in` + 10;
  RETURN IFNULL(Temp, 0);
END;//

CREATE PROCEDURE `MultipleVaryingResultSets`(
  IN p_in INT,
  OUT p_out INT,
  INOUT p_inout INT
)
	READS SQL DATA
BEGIN
  SELECT p_in, p_inout, p_out;
  SET p_in = 100, p_out = 200, p_inout = 300;
  SELECT p_inout, p_in, p_out;
  SELECT p_in, p_inout;
  SELECT 10;
END;//

CREATE PROCEDURE SingleResultSet()
	READS SQL DATA
BEGIN
  SELECT * FROM people;
END;//

CREATE PROCEDURE ALL_PARAMS_IN(
  IN p_id INT,
  IN p_name varchar(40)
)
	READS SQL DATA
BEGIN
  SELECT * from people where people.p_id=p_id or people.p_name=p_name; 
END;//

CREATE FUNCTION SIMPLE_FUNC() RETURNS INTEGER
	READS SQL DATA
BEGIN
  RETURN 10;
END;//

CREATE PROCEDURE TwoResultSets()
	READS SQL DATA
BEGIN
  SELECT * FROM people;
  SELECT * FROM string_values;
END;//

CREATE PROCEDURE ThreeResultSets()
	READS SQL DATA
BEGIN
  SELECT * FROM people;
  SELECT * FROM string_values;
  SELECT * FROM equipment;
END;//

DELIMITER ;

/*==============================================================*/
/* Grant privileges to columns                                  */
/*==============================================================*/

/* grant update(p_resume, p_redundant) on zeoslib.people to root@localhost;
*/

/*==============================================================*/
/* Grant privileges to table                                    */
/*==============================================================*/

/*grant select on zeoslib.people to root@localhost;
*/

create table time_fraction_values (
  id integer not null,
  timestamp_none timestamp,
  timestamp0 timestamp(0),
  timestamp1 timestamp(1),
  timestamp2 timestamp(2),
  timestamp3 timestamp(3),
  timestamp4 timestamp(4),
  timestamp5 timestamp(5),
  timestamp6 timestamp(6),
  time_none time,
  time0 time(0),
  time1 time(1),
  time2 time(2),
  time3 time(3),
  time4 time(4),
  time5 time(5),
  time6 time(6)
);
insert into time_fraction_values Values
(0, '2012-12-12 12:12:12', '2012-12-12 12:12:12', '2012-12-12 12:12:12.1', '2012-12-12 12:12:12.22', '2012-12-12 12:12:12.333', '2012-12-12 12:12:12.4444', '2012-12-12 12:12:12.55555', '2012-12-12 12:12:12.666666', '12:12:12', '12:12:12', '12:12:12.1', '12:12:12.22', '12:12:12.333', '12:12:12.4444', '12:12:12.55555', '12:12:12.666666'),
(1, '2012-12-12 12:12:12', '2012-12-12 12:12:12', '2012-12-12 12:12:12.1', '2012-12-12 12:12:12.22', '2012-12-12 12:12:12.333', '2012-12-12 12:12:12.4444', '2012-12-12 12:12:12.55555', '2012-12-12 12:12:12.666666', '12:12:12', '12:12:12', '12:12:12.1', '12:12:12.22', '12:12:12.333', '12:12:12.4444', '12:12:12.55555', '12:12:12.666666'),
(2, '2012-12-12 12:12:12', '2012-12-12 12:12:12', '2012-12-12 12:12:12.1', '2012-12-12 12:12:12.22', '2012-12-12 12:12:12.333', '2012-12-12 12:12:12.4444', '2012-12-12 12:12:12.55555', '2012-12-12 12:12:12.666666', '12:12:12', '12:12:12', '12:12:12.1', '12:12:12.22', '12:12:12.333', '12:12:12.4444', '12:12:12.55555', '12:12:12.666666'),
(3, '2012-12-12 12:12:12', '2012-12-12 12:12:12', '2012-12-12 12:12:12.1', '2012-12-12 12:12:12.22', '2012-12-12 12:12:12.333', '2012-12-12 12:12:12.4444', '2012-12-12 12:12:12.55555', '2012-12-12 12:12:12.666666', '12:12:12', '12:12:12', '12:12:12.1', '12:12:12.22', '12:12:12.333', '12:12:12.4444', '12:12:12.55555', '12:12:12.666666'),
(4, '2012-12-12 12:12:12', '2012-12-12 12:12:12', '2012-12-12 12:12:12.1', '2012-12-12 12:12:12.22', '2012-12-12 12:12:12.333', '2012-12-12 12:12:12.4444', '2012-12-12 12:12:12.55555', '2012-12-12 12:12:12.666666', '12:12:12', '12:12:12', '12:12:12.1', '12:12:12.22', '12:12:12.333', '12:12:12.4444', '12:12:12.55555', '12:12:12.666666'),
(5, '2012-12-12 12:12:12', '2012-12-12 12:12:12', '2012-12-12 12:12:12.1', '2012-12-12 12:12:12.22', '2012-12-12 12:12:12.333', '2012-12-12 12:12:12.4444', '2012-12-12 12:12:12.55555', '2012-12-12 12:12:12.666666', '12:12:12', '12:12:12', '12:12:12.1', '12:12:12.22', '12:12:12.333', '12:12:12.4444', '12:12:12.55555', '12:12:12.666666'),
(6, '2012-12-12 12:12:12', '2012-12-12 12:12:12', '2012-12-12 12:12:12.1', '2012-12-12 12:12:12.22', '2012-12-12 12:12:12.333', '2012-12-12 12:12:12.4444', '2012-12-12 12:12:12.55555', '2012-12-12 12:12:12.666666', '12:12:12', '12:12:12', '12:12:12.1', '12:12:12.22', '12:12:12.333', '12:12:12.4444', '12:12:12.55555', '12:12:12.666666');

