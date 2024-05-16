/*==============================================================*/
/* Database name:  MySql                                        */
/* DBMS name:      MySQL 3.23                                   */
/* Created on:     04.02.2003 19:48:39                          */
/*==============================================================*/

/*==============================================================*/
/* Tables for Bug#735226                                        */
/*==============================================================*/

CREATE TABLE table735226a
(
  id INT NOT NULL,
  referenceid INT NOT NULL
);

CREATE TABLE table735226b
(
  id INT NOT NULL
);

/*==============================================================*/
/* Tables for Bug#726788                                        */
/*==============================================================*/

CREATE TABLE Table726788
(
  FieldName INT NOT NULL
);

/*==============================================================*/
/* Tables for Bug#735299                                        */
/*==============================================================*/

CREATE TABLE table735299
(
  id INT NOT NULL,
  fld1 ENUM('Y','N'),
  fld2 enum('n','y'),
  PRIMARY KEY (id)
);

/*==============================================================*/
/* Tables for Bug#735299                                        */
/* used if MYSQL_FIELTYPE_BIT_1_ISBOOLEN is set                 */
/*==============================================================*/

CREATE TABLE table735299_bit
(
  id INT NOT NULL,
  fld1 bit(1),
  fld2 bit(1),
  PRIMARY KEY (id)
);

/*==============================================================*/
/* Tables for Bug#740899                                        */
/*==============================================================*/

CREATE TABLE table740899
(
  id INT NOT NULL,
  fld VARCHAR(35)
);

/*==============================================================*/
/* Tables for Bug #724542                                       */
/*==============================================================*/

CREATE TABLE table724542
(
  fld1 int(11),
  fld2 char(30),
  fld3 char(10)
);

/*==============================================================*/
/* Tables for Bug #739448                                       */
/*==============================================================*/

CREATE TABLE table739448a
(
  fld1 int(11),
  fld2 char(30),
  fld3 char(10)
);

insert into table739448a values (1, 'abcdefghi', '123456789');

CREATE TABLE table739448b
(
  fld1 int(11),
  fld2 char(30),
  fld3 char(10)
);

insert into table739448b values(1, 'abcdefghi', '123456789');

/*==============================================================*/
/* Tables for Bug #733236                                       */
/*==============================================================*/

CREATE TABLE table733236
(
  date DATE,
  time TIME,
  datetime DATETIME,
  timestamp TIMESTAMP
);

INSERT INTO table733236 VALUES("1998-12-31", "23:59:59", "1998-12-31 23:59:59",19981231235959);
INSERT INTO table733236 VALUES("1999-01-01", "00:00:00", "1999-01-01 00:00:00",19990101000000);
INSERT INTO table733236 VALUES("2000-01-01", "00:00:00", "2000-01-01 00:00:00",20000101000000);
INSERT INTO table733236 VALUES("2001-01-01", "00:00:00", "2001-01-01 00:00:00",20010101000000);
INSERT INTO table733236 VALUES("2004-12-31", "23:59:59", "2004-12-31 23:59:59",20041231235959);

/*==============================================================*/
/* Tables for Bug #768163                                       */
/*==============================================================*/

create table table768163
(
  fld1 int unsigned,
  fld2 bigint
);

/*==============================================================*/
/* Tables for Bug #799863                                       */
/*==============================================================*/

create table table799863
(
  fld1 year
);

/*insert into table799863 values (1024);*/
insert into table799863 values (1940);
insert into table799863 values (2003);

/*==============================================================*/
/* Tables for Bug #000001                                       */
/*==============================================================*/

create table table000001
(
  fld1 longtext,
  fld2 longblob
);

/*==============================================================*/
/* Tables for Bug #817607                                       */
/*==============================================================*/

create table table817607
(
  id int,
  `fruit name` varchar(10)
);

insert into table817607 values (1, 'apple');
insert into table817607 values (2, 'cherry');
insert into table817607 values (3, 'mango');

/*==============================================================*/
/* Tables for Bug #816925                                       */
/*==============================================================*/

create table table816925
(
  fld1 decimal(5,0),
  fld2 decimal(9,2),
  fld3 numeric(11,0),
  fld4 numeric(8,3)
);

/*==============================================================*/
/* Tables for Bug #828147                                       */
/*==============================================================*/

create table table828147
(
  id int4,
  txt text
);

insert into table828147 values (1, 'abc');

/*==============================================================*/
/* Tables for Bug #840608                                       */
/*==============================================================*/

CREATE TABLE table840608 (
`icode` varchar(7) default NULL,
`name` varchar(100) default NULL,
`strength` varchar(15) default NULL,
`units` varchar(50) default NULL,
`unitprice` double(15,3) default NULL,
`dosageform` varchar(100) default NULL,
`criticalpriority` int(11) default NULL,
`drugaccount` char(1) default NULL,
`drugcategory` varchar(150) default NULL,
`drugnote` varchar(150) default NULL,
`hintcode` char(2) default NULL,
`istatus` char(1) default NULL,
`lastupdatestdprice` datetime default NULL,
`lockprice` char(1) default NULL,
`lockprint` char(1) default NULL,
`maxlevel` int(11) default NULL,
`minlevel` int(11) default NULL,
`maxunitperdose` int(11) default NULL,
`packqty` int(11) default NULL,
`reorderqty` int(11) default NULL,
`stdprice` double(15,3) default NULL,
`stdtaken` varchar(30) default NULL,
`therapeutic` varchar(150) default NULL,
`therapeuticgroup` varchar(150) default NULL,
`default_qty` int(11) default NULL,
UNIQUE KEY `icode_unique` (`icode`),
UNIQUE KEY `units_icode_name_unique` 
(`units`,`icode`,`name`,`packqty`,`strength`),
KEY `dosageform` (`dosageform`),
KEY `drugaccount` (`drugaccount`),
KEY `drugcategory` (`drugcategory`),
KEY `drugnote` (`drugnote`),
KEY `hintcode` (`hintcode`),
KEY `icode` (`icode`),
KEY `istatus` (`istatus`),
KEY `name` (`name`),
KEY `packqty` (`packqty`),
KEY `strength` (`strength`),
KEY `therapeutic` (`therapeutic`),
KEY `therapeuticgroup` (`therapeuticgroup`),
KEY `units` (`units`)
);

INSERT INTO table840608
(icode,name,strength,units,unitprice,dosageform,criticalpriority,
drugaccount,drugcategory,drugnote,hintcode,istatus,
lastupdatestdprice,lockprice,lockprint,maxlevel,minlevel,
maxunitperdose,packqty,reorderqty,stdprice,stdtaken,therapeutic,
therapeuticgroup,default_qty) VALUES 
('1450012','PARACETAMOL (GPO)','500 mg.','TAB',0.5,'Tablets',0,'',
NULL,'p5','58','N',NULL,'N','N',1,1,0,1000,1,NULL,'0153',
'ANTI-PYRETIC','',NULL);


/*==============================================================*/
/* Tables for Bug #849723                                       */
/*==============================================================*/

CREATE TABLE table849723 (
  fld1 time not null,
  fld2 varchar(10),
  primary key (fld1)
);

INSERT INTO table849723 values ('00:00:00', 'abc');

/*==============================================================*/
/* Tables for Bug #869609                                       */
/*==============================================================*/

CREATE TABLE table869609 (
  id int8 not null auto_increment,
  primary key (id)
);

/*==============================================================*/
/* Tables for Bug #865564                                       */
/*==============================================================*/

CREATE TABLE table865564 (
  fld1 float(30,2),
  fld2 decimal(10,4)
);

INSERT INTO table865564 VALUES (123.45, 123.4567);

/*==============================================================*/
/* Tables for Bug #881634                                       */
/*==============================================================*/

CREATE TABLE table881634a (
  idt1 int4 not null,
  ft1 varchar(20),
  PRIMARY KEY (idt1)
);

CREATE TABLE table881634b (
  idt2 int4 not null,
  ft2 varchar(20),
  ft1 integer,
  PRIMARY KEY (idt2)
);

/*==============================================================*/
/* Tables for Bug #884135                                       */
/*==============================================================*/

CREATE TABLE table884135a (
  id int4 unsigned auto_increment not null,
  fld varchar(20),
  PRIMARY KEY (id)
);

INSERT INTO table884135a VALUES (1, 'aaa');
INSERT INTO table884135a VALUES (2, 'bbb');
INSERT INTO table884135a VALUES (3, 'ccc');

CREATE TABLE table884135b (
  id int4 unsigned auto_increment not null,
  mid int4 unsigned,
  fld varchar(20),
  PRIMARY KEY (id)
);

INSERT INTO table884135b VALUES (1, 1, 'aaaa');
INSERT INTO table884135b VALUES (2, 1, 'aabb');
INSERT INTO table884135b VALUES (3, 1, 'aacc');
INSERT INTO table884135b VALUES (4, 2, 'bbaa');
INSERT INTO table884135b VALUES (5, 2, 'bbbb');
INSERT INTO table884135b VALUES (6, 2, 'bbcc');
INSERT INTO table884135b VALUES (7, 3, 'ccaa');
INSERT INTO table884135b VALUES (8, 3, 'ccbb');
INSERT INTO table884135b VALUES (9, 3, 'cccc');

/*==============================================================*/
/* Tables for Bug #886841                                       */
/*==============================================================*/

CREATE TABLE table886841 (
  fld enum('y','n') default 'y'  
);

/*==============================================================*/
/* Tables for Bug #894367                                       */
/*==============================================================*/

CREATE TABLE table894367a (
  fld1 varchar(30),
  fld2 enum('y','n')
);

CREATE TABLE table894367b (
  fld1 int4,
  fld2 float
);

CREATE TABLE table894367c (
  fld1 tinyblob,
  fld2 int8
); 

/*==============================================================*/
/* Tables for Bug #886841                                       */
/*==============================================================*/
/*
CREATE TABLE table914436 (
  fld1 CHAR(15) CHARACTER SET LATIN1,
  fld2 VARCHAR(15) CHARACTER SET LATIN1,
  fld3 TEXT(15) CHARACTER SET LATIN1
);
/**/

/*==============================================================*/
/* Tables for Bug #938705                                       */
/*==============================================================*/

CREATE TABLE `Table 938705` (
  `Field First` int4 not null auto_increment,
  `Field Second` varchar(10),
  primary key (`Field First`)
);

/*==============================================================*/
/* Tables for Bug #957126                                       */
/*==============================================================*/
CREATE TABLE table957126 (
  fld1 VARCHAR(15) DEFAULT '',
  fld2 VARCHAR(15)
);

/*==============================================================*/
/* Tables for Bug #987022                                       */
/*==============================================================*/
CREATE TABLE table987022 (
  fld1 BIGINT,
  fld2 VARCHAR(15)
);

/*==============================================================*/
/* Tables for Bug #989474                                       */
/*==============================================================*/
CREATE TABLE table989474 (
  CustID INT4,
  CreateDate DATE,
  PRIMARY KEY (CustID)
);

/*==============================================================*/
/* Tables for Bug #1045286                                      */
/*==============================================================*/
CREATE TABLE table1045286 (
  fld text not null
);

INSERT INTO table1045286 VALUES ('');


/*==============================================================*/
/* Tables for Ticket#52                                         */
/*==============================================================*/
CREATE TABLE TableTicket52 (
  id_test int(11) UNSIGNED NOT NULL,
  filter_test varchar(255) DEFAULT NULL,
  PRIMARY KEY (id_test)
);

INSERT INTO TableTicket52(id_test, filter_test) VALUES (1, 'String01');
INSERT INTO TableTicket52(id_test, filter_test) VALUES (2, '0');
INSERT INTO TableTicket52(id_test, filter_test) VALUES (3, '1');
INSERT INTO TableTicket52(id_test, filter_test) VALUES (4, '2');
INSERT INTO TableTicket52(id_test, filter_test) VALUES (5, 'Other');
INSERT INTO TableTicket52(id_test, filter_test) VALUES (6, 'String02');

/*==============================================================*/
/* Tables for TableMS56OBER9357                                 */
/*==============================================================*/

SET sql_mode = ''; -- To make possible assigning of '0000-00-00' value

CREATE TABLE `TableMS56OBER9357` (
`keyfield` int(11) NOT NULL default '0',
`dtfield` date NOT NULL default '0000-00-00',
`infofield` varchar(10) default NULL,
PRIMARY KEY (`keyfield`,`dtfield`)
);

/*==============================================================*/
/* Tables for Ticket#240                                        */
/*==============================================================*/
CREATE TABLE TableTicket240 (
  id_test BIGINT(20) UNSIGNED NOT NULL AUTO_INCREMENT,
  field_1 varchar(255) DEFAULT NULL,
  PRIMARY KEY (id_test)
);

/*==============================================================*/
/* Table for Ticket#304                                         */
/*==============================================================*/
CREATE TABLE IF NOT EXISTS TableTicket304 (
  id_test BIGINT NOT NULL PRIMARY KEY,
  LATITUDE double precision,
  LONGITUDE double precision,
  point_field point DEFAULT NULL,
  geometry_field geometry DEFAULT NULL
);

delimiter //
/*==============================================================*/
/* trigger for Ticket#304                                       */
/*==============================================================*/
CREATE TRIGGER TR_UPD_TableTicket304 BEFORE UPDATE ON TableTicket304 FOR EACH ROW
BEGIN
  DECLARE dummy INT;
  if not NEW.point_field <=> old.point_field then
    SELECT point_field_changed INTO dummy FROM TableTicket304 WHERE 1=0;
  END IF;
  if not NEW.geometry_field <=> old.geometry_field then
    SELECT geometry_field_changed INTO dummy FROM TableTicket304 WHERE 1=0;
  END IF;	
  if NEW.point_field IS NULL AND NEW.LATITUDE IS NOT NULL AND NEW.LONGITUDE IS NOT NULL then
    SET NEW.point_field = POINT(NEW.LONGITUDE, NEW.LATITUDE); 
  END IF;
  if NEW.geometry_field IS NULL AND NEW.LATITUDE IS NOT NULL AND NEW.LONGITUDE IS NOT NULL then
    SET NEW.geometry_field = POINT(NEW.LONGITUDE, NEW.LATITUDE); 
  END IF;
END; //
delimiter ;


/*==============================================================*/
/* Tables for Ticket#389                                        */
/*==============================================================*/
CREATE TABLE TableTicked389 (
  id_test BIGINT(20) UNSIGNED NOT NULL AUTO_INCREMENT,
  int_field BIGINT DEFAULT NULL,
  PRIMARY KEY (id_test)
);

/*================================================================*/
/* A table for an error concerning bigints. See                   */
/* See https://zeoslib.sourceforge.io/viewtopic.php?f=50&t=118430 */
/*================================================================*/
CREATE TABLE biginterror (
  f bigint(20) unsigned DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf16;

CREATE TABLE table_ticket_442 (
  `objectid` int(11) NOT NULL, 
  `label` varchar(255) NOT NULL, 
  PRIMARY KEY (`objectid`) 
  );

CREATE TABLE table_p156227 (
  `id`int not null,
  `SaleDateTime` date NOT NULL, 
  `VatLow` numeric(10,2) NOT NULL, 
  PRIMARY KEY (`id`) 
  );

delimiter //

create procedure forum199899(in somevalue varchar(50)) 
begin
  select length(somevalue);
end; //



delimiter ;

create table TblForumT199899_1 (
  id int auto_increment primary key,
  appliname varchar(100) not null default '',
  pc_name varchar(15) not null default '',
  contents1 varchar(200) not null default '',
  update_stamp datetime(3) not null default current_timestamp(3) on update current_timestamp(3)
);

/*
create table TblForumT199899_2 (
  id int auto_increment primary key,
  customercode varchar(6) not null default '',
  orderflg varchar(1) not null default '0',
  customername varchar(100) not null default '',
  update_stamp datetime(3) not null default current_timestamp(3) on update current_timestamp(3)
);
*/

delimiter //

create function FncForumT199899_1 (
  in_tablename varchar(100)
)
returns text deterministic
begin
  return concat('select * from ', in_tablename);
end //

create function FncForumT199899_2 (
  in_flg int,
  in_table_headername varchar(100)
)
returns text deterministic
begin
  declare val_sqltext_select text default "";
  declare val_open_tablename varchar(100) default "";

  set val_open_tablename = concat(in_table_headername, 'zeos');
  if in_flg=0 then
    set val_sqltext_select = concat('select * from ', val_open_tablename);
  else
    set val_sqltext_select = concat('select customercode, orderflg, customername, ',
	                                'case when orderflg="0" then "not ordered" else "ordered" as orderflg_name ',
									'from ', val_open_tablename);
  end if;
  return val_sqltext_select;
end //

create procedure ProcForumT199899_3 (
  in in_tablename varchar(100),
  out out_pcname varchar(100)
)
begin
  set @s = concat('select pc_name into @pc_name from ', in_tablename, ' where appliname = "aaa.exe"');
  prepare stmt1 from @s;
  execute stmt1;
  deallocate prepare stmt1;
  
  set out_pcname=@pc_name;
end //

delimiter ;

