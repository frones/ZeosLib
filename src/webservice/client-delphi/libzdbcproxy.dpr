library libzdbcproxy;

{ Wichtiger Hinweis zur DLL-Speicherverwaltung: ShareMem muss die erste
  Unit in der USES-Klausel Ihrer Bibliothek UND in der USES-Klausel Ihres Projekts
  sein (w�hlen Sie "Projekt, Quelltext anzeigen"), wenn Ihre DLL Prozeduren oder Funktionen
  exportiert, die Strings als Parameter oder Funktionsergebnisse �bergeben. Dies
  gilt f�r alle Strings, die an oder von Ihrer DLL �bergeben werden, auch f�r solche,
  die in Records und Klassen verschachtelt sind. ShareMem ist die Interface-Unit f�r
  den gemeinsamen BORLNDMM.DLL-Speichermanager, der zusammen mit Ihrer DLL
  weitergegeben werden muss. �bergeben Sie String-Informationen mit PChar- oder ShortString-
  Parametern, um die Verwendung von BORLNDMM.DLL zu vermeiden.

  Wichtiger Hinweis zur Verwendung der VCL: Wenn diese DLL implizit geladen wird
  und die in einem Unit-Initialisierungsabschnitt erstellte TWicImage/TImageCollection-Komponente
  verwendet, dann muss Vcl.WicImageInit in die USES-Klausel
  der Bibliothek aufgenommen werden. }

uses
  System.SysUtils,
  System.Classes,
  ZPlainProxyDriverInternalProxy,
  ZPlainProxyDriverIntf;

{$R *.res}

function GetLastErrorStr: WideString; stdcall;
begin
  Result := ZPlainProxyDriverInternalProxy.GetLastErrorStr;
end;

function GetInterface: IZDbcProxy; stdcall;
begin
  Result := ZPlainProxyDriverInternalProxy.GetInterface;
end;

exports GetInterface, GetLastErrorStr;

end.

