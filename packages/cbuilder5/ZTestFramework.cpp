//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ZTestFramework.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\..\test\external\TextTestRunner.pas");
USEUNIT("..\..\test\external\TestFrameWork.pas");
USEUNIT("..\..\test\external\MemCheck.pas");
USEUNIT("..\..\test\framework\ZBugReport.pas");
USEUNIT("..\..\test\framework\ZPerformanceTestCase.pas");
USEUNIT("..\..\test\framework\ZSqlTestCase.pas");
USEUNIT("..\..\test\framework\ZTestConsts.pas");
USEUNIT("..\..\test\framework\ZTestConfig.pas");
USEUNIT("..\..\test\framework\ZTestDefinitions.pas");
USEUNIT("..\..\test\framework\ZTestCase.pas");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("ZComponent.bpi");
USEPACKAGE("ZCore.bpi");
USEPACKAGE("ZDbc.bpi");
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
