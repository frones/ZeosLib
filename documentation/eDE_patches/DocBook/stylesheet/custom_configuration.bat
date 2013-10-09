@echo off
echo\

if '%ZEOS_DIR%'=='' goto nozeosdir
set docbook_document_repository=%ZEOS_DIR%\documentation\articles
set docbook_document_output=%ZEOS_DIR%\documentation\articles_generated\output
set docbook_document_deploy=%ZEOS_DIR%\documentation\articles_generated\deploy
set docbook_editor="C:\Program Files\XML Copy Editor\xmlcopyeditor.exe"
set docbook_html_viewer="%ProgramFiles%\Mozilla Firefox\firefox.exe"

goto end

:nozeosdir
echo no ZEOS_DIR environment variable

:end
echo document dirs set to %ZEOS_DIR%