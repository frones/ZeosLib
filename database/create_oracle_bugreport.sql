alter session set NLS_DATE_FORMAT = 'YYYY-MM-DD';
alter session set NLS_TIMESTAMP_FORMAT = 'YYYY-MM-DD HH24:MI:SS';

/*==============================================================*/
/* Table for check Numbers wo Precission/Scale                  */
/*==============================================================*/
create table TABLE_NUM1 (
id                 NUMBER(9, 0) not null,
Num		   NUMBER
);

INSERT INTO Table_Num1 VALUES(1, 54321.0123456789);

create table table_ticket437(
  id int not null,
  text varchar2(4000),
  clobek clob,
  longek long
);

insert into table_ticket437(id, text)
select 1, 'ASD' from dual
union all
select 2, 'ASDF' from dual
union all
select 3, 'QWERT' from dual;

update table_ticket437 set clobek = text;

update table_ticket437 set longek = clobek;

SET TERM ^ ;
CREATE OR REPLACE PACKAGE P_TICKET96 AS
  PROCEDURE Update_Doc(id NUMBER, qty NUMBER, price NUMBER);
  PROCEDURE Update_Doc(row NUMBER);
END P_TICKET96; ^

CREATE OR REPLACE PACKAGE BODY P_TICKET96 AS

    PROCEDURE  Update_Doc(id NUMBER, qty NUMBER, price NUMBER) IS
    BEGIN
      NULL;  
    END Update_Doc;

    PROCEDURE  Update_Doc(row NUMBER) IS
    BEGIN
      NULL;
    END Update_Doc;

END P_TICKET96; ^

set term ; ^