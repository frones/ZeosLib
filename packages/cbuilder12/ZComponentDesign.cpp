//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORMNS("..\..\src\component\ZPropertiesEditor.pas", Zpropertieseditor, frmPropertyEditor);
USEFORMNS("..\..\src\component\ZUpdateSqlEditor.pas", Zupdatesqleditor, ZUpdateSQLEditForm);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
