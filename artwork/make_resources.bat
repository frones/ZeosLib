@@echo off
echo ----------------------
echo BUILD Delphi resources
echo ----------------------
rc.exe -r -v -fo../src/component/ZComponent.dcr ZComponent.rc
echo -----------------------
echo BUILD Lazarus resources
echo -----------------------
bmptoxpm TZConnection.bmp
bmptoxpm TZTransaction.bmp
bmptoxpm TZReadOnlyQuery.bmp
bmptoxpm TZQuery.bmp
bmptoxpm TZUpdateSql.bmp
bmptoxpm TZSqlProcessor.bmp
bmptoxpm TZTable.bmp
bmptoxpm TZStoredProc.bmp
bmptoxpm TZSqlMonitor.bmp
bmptoxpm TZSqlMetaData.bmp
bmptoxpm TZSequence.bmp
bmptoxpm TZIBEventAlerter.bmp
bmptoxpm TZConnectionGroup.bmp
bmptoxpm TZGroupedConnection.bmp
bmptoxpm TZPgEventAlerter.bmp
lazres ../src/component/ZComponentReg.lrs TZConnection.xpm TZTransaction.xpm TZReadOnlyQuery.xpm TZConnectionGroup.xpm TZPgEventAlerter.xpm TZGroupedConnection.xpm TZQuery.xpm TZUpdateSql.xpm TZSqlProcessor.xpm TZTable.xpm TZStoredProc.xpm TZSqlMonitor.xpm TZSqlMetaData.xpm TZSequence.xpm TZIBEventAlerter.xpm
echo ----------------------
echo Remove temporary files
echo ----------------------
del *.xpm
pause