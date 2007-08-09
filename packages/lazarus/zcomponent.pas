{ Dit bestand is automatisch aangemaakt door Lazarus. Niet wijzigen!
Deze broncode is alleen gebruikt voor compilatie en installatie.
 }

unit zcomponent; 

interface

uses
  ZAbstractDataset, ZAbstractRODataset, ZAbstractTable, ZComponentReg, 
    ZConnection, ZDataset, ZDatasetUtils, ZPropertyEditor, ZSqlMetadata, 
    ZSqlMonitor, ZSqlProcessor, ZSqlStrings, ZSqlUpdate, ZStoredProcedure, 
    ZStreamBlob, ZUpdateSqlEditor, ZSequence, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('ZComponentReg', @ZComponentReg.Register); 
end; 

initialization
  RegisterPackage('zcomponent', @Register); 
end.
