{ Dit bestand is automatisch aangemaakt door Lazarus. Niet wijzigen!
Deze broncode is alleen gebruikt voor compilatie en installatie.
 }

unit zcomponent; 

interface

uses
  ZComponentReg, ZConnection, ZDatasetUtils, ZUpdateSqlEditor, 
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('ZComponentReg', @ZComponentReg.Register); 
end; 

initialization
  RegisterPackage('zcomponent', @Register); 
end.
