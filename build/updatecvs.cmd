@echo off
Set CVSROOT=:ext:sf_username@cvs.sourceforge.net:/cvsroot/zeoslib
call ant -q -buildfile updatecvs.xml
Set CVSROOT=

