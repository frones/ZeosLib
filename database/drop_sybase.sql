/*==============================================================*/
/* Database name:  Sybase                                       */
/* DBMS name:      Sybase ASE 12.5                              */
/* Created on:     04.02.2003 20:01:43                          */
/*==============================================================*/

set quoted_identifier on
go

drop table blob_values
go

drop table cargo
go

drop table date_values
go

drop table equipment2
go

drop table equipment
go

drop table number_values
go

drop table people
go

drop table department
go

drop table string_values
go

drop table not_null_values
go

drop table "Case_Sensitive"
go

drop table case_sensitive
go

drop table high_load
go

drop table "Spaced Names"
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
           where  id = object_id('bcd_values')
            and   type = 'U')
   drop table bcd_values
go
/*
drop procedure abtest
go */
