SET TERM ^ ;
BEGIN
  FOR rec in (select OBJECT_NAME 
                from user_objects
               where object_type = 'TABLE'
                 and upper(OBJECT_NAME) in ('BLOB_VALUES',
                                            'BINARY_VALUES',
                                            'CARGO',
                                            'DATE_VALUES',
                                            'DEPARTMENT',
                                            'EQUIPMENT',
                                            'EQUIPMENT2',
                                            'NUMBER_VALUES',
                                            'PEOPLE',
                                            'STRING_VALUES',
                                            'NOT_NULL_VALUES',
                                            'CASE_SENSITIVE',
                                            'HIGH_LOAD',
                                            'DEFAULT_VALUES',
                                            'DEFAULT_VALUES2',
                                            'TABLE_NUM1',
                                            'CUSTOMERS',
                                            'SPACED NAMES',
                                            'BCD_VALUES')
               order by decode(upper(object_name),'DEPARTMENT',2,'EQUIPMENT',2,1)) LOOP
    execute immediate('DROP TABLE "'||rec.object_name||'"');
  END LOOP;

  FOR rec in (select OBJECT_NAME 
                from user_objects
               where object_type = 'TYPE'
                 and upper(OBJECT_NAME) in ('ADDRESS_TAB', 'ADDRESS_T')
               order by object_name desc) LOOP
    execute immediate('DROP TYPE '||rec.object_name);
  END LOOP;

  FOR rec in (select OBJECT_NAME 
                from user_objects
               where object_type = 'VIEW'
                 and upper(OBJECT_NAME) in ('DEP_VIEW')) LOOP
    execute immediate('DROP VIEW "'||rec.object_name||'"');
  END LOOP;

  FOR rec in (select OBJECT_NAME 
                from user_objects
               where object_type = 'PROCEDURE'
                 and upper(OBJECT_NAME) in ('ABTEST')) LOOP
    execute immediate('DROP PROCEDURE "'||rec.object_name||'"');
  END LOOP;

  FOR rec in (select OBJECT_NAME 
                from user_objects
               where object_type = 'FUNCTION'
                 and upper(OBJECT_NAME) in ('SIMPLEFUNC',
                                            'SIMPLE_FUNC',
                                            'MYFUNCINOUTRETURN',
                                            'IS_ACCOUNT_SERVE')) LOOP
    execute immediate('DROP FUNCTION "'||rec.object_name||'"');
  END LOOP;

  FOR rec in (select OBJECT_NAME 
                from user_objects
               where object_type = 'PACKAGE'
                 and upper(OBJECT_NAME) in ('MYPACKAGE')) LOOP
    execute immediate('DROP PACKAGE "'||rec.object_name||'"');
  END LOOP;
  
END; ^

SET TERM ; ^