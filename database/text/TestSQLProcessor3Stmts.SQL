create table domain_values
(
   d_id                       INTEGER not null,
   d_fld1                     tinteger,
   d_fld2                     tfloat,
   d_fld3                     tstring,
   primary key (d_id)
);


SET TERM ^ ;

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
/* Grant privileges to columns                                  */
/*==============================================================*/
SET TERM ; ^

