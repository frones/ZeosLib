SET TERM ^ ;
BEGIN
  FOR rec in (select OBJECT_NAME 
                from user_objects
               where object_type = 'TABLE'
                 and upper(OBJECT_NAME) in ('TABLE_NUM1','TABLE_TICKET437')) LOOP
    execute immediate('DROP TABLE "'||rec.object_name||'"');
  END LOOP;

  FOR rec in (select OBJECT_NAME 
                from user_objects
               where object_type = 'PACKAGE'
                 and upper(OBJECT_NAME) in ('P_TICKET96')) LOOP
    execute immediate('DROP PACKAGE "'||rec.object_name||'"');
  END LOOP;
  
END; ^

SET TERM ; ^