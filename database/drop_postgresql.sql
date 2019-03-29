/*==============================================================*/
/* Database name:  PostgreSql                                   */
/* DBMS name:      PostgreSQL 7                                 */
/* Created on:     04.02.2003 19:59:06                          */
/*==============================================================*/

drop table if exists blob_values;
drop table if exists date_values;
drop table if exists default_values;
drop table if exists default_values2;
drop table if exists string_values;
drop table if exists not_null_values;
drop table if exists number_values;

drop table if exists cargo;
drop table if exists people;
drop table if exists equipment2;
drop table if exists equipment;
drop table if exists department;

drop table if exists "Case_Sensitive";
drop table if exists "Spaced Names";
drop table if exists case_sensitive;
drop table if exists high_load;
drop table if exists bcd_values;

drop function if exists procedure1(INT4);
drop function if exists procedure2();
drop function if exists "ABTEST"(integer, integer, character varying);
drop function if exists proc_nonames (integer, integer);
drop function if exists proc_onename (integer, integer);
drop function if exists proc_noout (integer, integer);
drop function if exists proc_composite (integer, integer);
drop function if exists proc_mixedorder (integer, integer);
drop function if exists proc_set ();

drop type if exists compositetype;

/*
drop SEQUENCE cargo_c_id_seq;
drop SEQUENCE default_values_d_id_seq;
drop SEQUENCE department_dep_id_seq;
drop SEQUENCE equipment_eq_id_seq;
drop SEQUENCE people_p_id_seq;
*/