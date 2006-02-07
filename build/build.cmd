@echo off
@cls
@echo Building Zeos DBO Project...
call clean
call updatecvs
call compileall
call test

