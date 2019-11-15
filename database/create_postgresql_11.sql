/*==============================================================*/
/* Database name:  PostgreSql                                   */
/* DBMS name:      PostgreSQL 11 and up                         */
/* Created on:     31.10.2019 04:15                             */
/*==============================================================*/

/*==============================================================*/
/* PROCEDURE : PROC_ABTEST                                      */
/*==============================================================*/
CREATE PROCEDURE "PROC_ABTEST"(
  p1 integer,
  p2 integer,
  p3 varchar,
  inout p4 integer,
  inout p5 varchar
)
LANGUAGE 'plpgsql' AS $$
BEGIN
	p4 = p1 * 10 + p2;
	p5 = p3 || p3;
END;
$$;
