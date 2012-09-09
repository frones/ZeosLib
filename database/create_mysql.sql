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
   b_id                           int			 not null,
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
   d_timestamp                    timestamp,
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
   n_id                           int   		 not null,
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
/* Table : high_load                                            */
/*==============================================================*/
create table high_load
(
  hl_id		      INTEGER NOT NULL,
  data1		      FLOAT,
  data2		      CHAR(10),
  primary key (hl_id)
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

CREATE PROCEDURE `TripleResultVarying`(
  IN p_in INT,
  OUT p_out INT,
  INOUT p_inout INT)
BEGIN
  SELECT p_in, p_out, p_inout;
  SET p_in = 100, p_out = 200, p_inout = 300;
  SELECT p_in, p_out, p_inout;
END;//

DELIMITER ;

/*==============================================================*/
/* Grant privileges to columns                                  */
/*==============================================================*/

/* grant update(p_resume, p_redundant) on zeoslib.people to root@localhost; */

/*==============================================================*/
/* Grant privileges to table                                    */
/*==============================================================*/

/*grant select on zeoslib.people to root@localhost;
*/