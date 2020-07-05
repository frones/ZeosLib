{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                   Properties editor                     }
{                                                         }
{             Originally written by EgonHugeist           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPropertiesEditor;

interface

{$I ZComponent.inc}
uses
  {$IFNDEF FPC}Windows, Messages, {$ENDIF}SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  ZClasses, ZDbcProperties, ZDbcIntfs,
  {$IFNDEF FPC}DesignIntf, DesignEditors{$ELSE}
  PropEdits, LCLIntf, LResources, ComponentEditors{$ENDIF};

type

  { TfrmPropertyEditor }

  TfrmPropertyEditor = class(TForm)
    pnlProps: TPanel;
    bgPropsUsed: TGroupBox;
    lbUsed: TListBox;
    btnAdd: TButton;
    spltProps: TSplitter;
    gbAvailable: TGroupBox;
    pnlBottom: TPanel;
    btnRemove: TButton;
    Splitter1: TSplitter;
    pnlValDesc: TPanel;
    gbVal: TGroupBox;
    Splitter2: TSplitter;
    gbDescription: TGroupBox;
    mmDescrption: TMemo;
    lbAvailable: TListBox;
    cbEnum: TComboBox;
    cbBoolean: TCheckBox;
    edString: TEdit;
    lblProtocol: TLabel;
    lblServerProvider: TLabel;
    lblHostversion: TLabel;
    lblClientVersion: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure lbUsedClick(Sender: TObject);
    procedure cbBooleanClick(Sender: TObject);
    procedure lbAvailableClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
  private
    { Private declarations }
    FZPropertyLevelTypes: TZPropertyLevelTypes;
    FProtocol: String;
    FServerProvider: TZServerProvider;
    FPropsUsed, FPropsUnUsed: TZSortedList;
    function compareProps(Item1, Item2: Pointer): Integer;
  public
    { Public declarations }
  end;

  {** Implements the basic methods of the property editor. }
  TZProperitesEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

uses ZSysUtils, TypInfo,
  ZAbstractRODataset, ZAbstractDataset, ZAbstractConnection;

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

procedure TfrmPropertyEditor.FormShow(Sender: TObject);
var I, j: Integer;
  UpperUsed, UpperCurrent: String;
  Found: Boolean;
  Current: PZProperty;
  ZPropertiesArray: PZPropertyRefDynArray;
begin
  cbBoolean.Top := edString.Top;
  cbEnum.Top := edString.Top;
  ZPropertiesArray := GetZProperties;
  for i := 0 to High(ZPropertiesArray^) do
    if ZPropertiesArray^[i] <> nil then begin
      Current := ZPropertiesArray^[i];
      if ((pltConnection in FZPropertyLevelTypes) and (pltConnection in Current.LevelTypes)) or
         ((pltTransaction in FZPropertyLevelTypes) and (pltTransaction in Current.LevelTypes)) or
         ((pltStatement in FZPropertyLevelTypes) and (pltStatement in Current.LevelTypes) and not
           ((pltConnection in FZPropertyLevelTypes) and not (pltConnection in Current.LevelTypes))) then begin
        if (FProtocol <> '') and (Current.Protocols.Count > 0) and (Current.Protocols.Items <> nil) then begin
          lblProtocol.Caption := 'Protocol: '+fProtocol;
          Found := False;
          UpperCurrent := UpperCase(FProtocol);
          for J := 0 to Current.Protocols.Count -1 do begin
            UpperUsed := UpperCase(Current.Protocols.Items[j]);
            Found := StartsWith(UpperCurrent, UpperUsed);
            if Found then
              Break;
          end;
          if not Found then
            continue;
        end;
        if (FServerProvider <> spUnknown) and (Current.Providers.Count > 0) and
           (Current.Providers.Items <> nil) then begin
          lblServerProvider.Caption := 'ServerProvider: '+GetEnumName(TypeInfo(TZServerProvider), Ord(FServerProvider));
          Found := False;
          for J := 0 to Current.Protocols.Count -1 do
            if FServerProvider = Current.Providers.Items[j].Provider then begin
              //todo add possible version check
              Found := True;
              Break;
            end;
          if not Found then
            continue;
        end;
        Found := False;
        UpperCurrent := UpperCase(Current.Name);
        for J := 0 to lbUsed.Items.Count -1 do begin
          UpperUsed := UpperCase(lbUsed.Items[j]);
          Found := StartsWith(UpperUsed, UpperCurrent);
          if Found then begin
            FPropsUsed.Add(Current);
            lbUsed.Items.Objects[j] := TObject(Current);
            Break;
          end;
        end;
        if not Found then
          FPropsUnUsed.Add(Current);
      end;
    end;
  FPropsUnUsed.Sort(compareProps);
  for i := 0 to FPropsUnUsed.Count -1 do begin
    Current := FPropsUnUsed[i];
    lbAvailable.Items.AddObject(Current.Name, TObject(Current));
  end;
  if lbUsed.Items.Count > 0 then begin
    lbUsed.ItemIndex := 0;
    lbUsedClick(Sender);
  end else if lbAvailable.Items.Count > 0 then begin
    lbAvailable.ItemIndex := 0;
    lbAvailableClick(Sender);
  end else begin
    btnAdd.Enabled := False;
    btnRemove.Enabled := False;
  end;
end;

procedure TfrmPropertyEditor.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  Close;
end;

procedure TfrmPropertyEditor.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
  Close;
end;

{ TZProperitesEditor }

procedure TZProperitesEditor.Edit;
var
  Component: TComponent;
  Lines, SortedLines: TStrings;
label jmpProtocol;
begin
  Component := TComponent(GetComponent(0));
  SortedLines := TStringList.Create;
  with TfrmPropertyEditor.Create(Application) do
  try
    Lines := TStrings(GetOrdValue);
    if Component.InheritsFrom(TZAbstractConnection) then begin
      FZPropertyLevelTypes := [pltConnection, pltTransaction, pltStatement];
jmpProtocol:
      FProtocol := TZAbstractConnection(Component).Protocol;
      if TZAbstractConnection(Component).Connected then
        FServerProvider := TZAbstractConnection(Component).DbcConnection.GetServerProvider;
    end else if Component.InheritsFrom(TZAbstractTransaction) then begin
      FZPropertyLevelTypes := [pltTransaction];
      if TZAbstractTransaction(Component).Connection <> nil then begin
        Component := TZAbstractTransaction(Component).Connection;
        goto jmpProtocol;
      end;
    end else if Component.InheritsFrom(TZAbstractDataset) then begin
      FZPropertyLevelTypes := [pltStatement, pltResolver];
      if TZAbstractRODataSet(Component).Connection <> nil then begin
        Component := TZAbstractRODataSet(Component).Connection;
        goto jmpProtocol;
      end;
    end else if Component.InheritsFrom(TZAbstractRODataSet) then begin
      FZPropertyLevelTypes := [pltStatement];
      if TZAbstractRODataSet(Component).Connection <> nil then begin
        Component := TZAbstractRODataSet(Component).Connection;
        goto jmpProtocol;
      end;
    end else Exit;
    SortedLines.AddStrings(Lines);
    TStringList(SortedLines).Sort;
    lbUsed.Items.AddStrings(SortedLines);
    ActiveControl := lbUsed;
    case ShowModal of
      mrOk: begin
              Lines.Clear;
              Lines.AddStrings(lbUsed.Items);
              SetOrdValue(NativeInt(Lines));
              ShowMessage(Lines.Text);
            end;
    end;
  finally
    Free;
    SortedLines.Free;
  end;
end;

function TZProperitesEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

procedure TfrmPropertyEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPropsUsed);
  FreeAndNil(FPropsUnUsed);
end;

procedure TfrmPropertyEditor.FormCreate(Sender: TObject);
begin
  FPropsUsed := TZSortedList.Create;
  FPropsUnUsed := TZSortedList.Create;
end;

procedure TfrmPropertyEditor.btnRemoveClick(Sender: TObject);
var I, idx: Integer;
  Current: PZProperty;
begin
  idx := lbUsed.ItemIndex;
  if lbUsed.ItemIndex <> -1 then begin
    Current := PZProperty(lbUsed.Items.Objects[Idx]);
    if Current <> nil then begin
      FPropsUnUsed.Add(Current);
      FPropsUnUsed.Sort(compareProps);
      lbAvailable.Enabled := False;
      lbAvailable.Clear;
      for i := 0 to FPropsUnUsed.Count -1 do
        lbAvailable.Items.AddObject(PZProperty(FPropsUnUsed[i]).Name, FPropsUnUsed[i]);
      lbAvailable.Enabled := True;
      i := FPropsUsed.IndexOf(Current);
      FPropsUsed.Delete(I);
    end;
    lbUsed.Items.Delete(Idx);
  end;
  if Idx = FPropsUsed.Count then
    Dec(IDX);
  lbUsed.ItemIndex := idx;
  lbUsedClick(Sender);
end;

function TfrmPropertyEditor.compareProps(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareStr(PZProperty(Item1).Name, PZProperty(Item2).Name);
end;

procedure TfrmPropertyEditor.lbUsedClick(Sender: TObject);
var Current: PZProperty;
begin
  btnRemove.Enabled := True;
  btnAdd.Enabled := False;
  if (lbUsed.ItemIndex <> -1) and lbUsed.Enabled then begin
    Current := PZProperty(lbUsed.Items.Objects[lbUsed.ItemIndex]);
    edString.Visible := False;
    cbBoolean.Visible := False;
    cbEnum.Visible := False;
    if Current = nil then Exit;
    mmDescrption.Text := Current.Purpose;
    gbVal.Caption := Current.Name;
    case Current.ValueType of
      pvtBool: begin
                 cbBoolean.Visible := True;
                 cbBoolean.Checked := StrToBoolEx(lbUsed.Items.Values[Current.Name]);
               end;
      pvtEnum: begin
                 cbEnum.Visible := True;
                 cbEnum.Text := lbUsed.Items.Values[Current.Name];
               end;
      pvtNumber,
      pvtString,
      pvtBoolOrString: begin
                 edString.Visible := True;
                 edString.Text := lbUsed.Items.Values[Current.Name];
              end;
    end;
  end;
end;

procedure TfrmPropertyEditor.cbBooleanClick(Sender: TObject);
var Current: PZProperty;
begin
  if (lbUsed.ItemIndex <> -1) and lbUsed.Enabled then begin
    Current := PZProperty(lbUsed.Items.Objects[lbUsed.ItemIndex]);
    if cbBoolean.Checked
    then lbUsed.Items.Values[Current.Name] := 'true'
    else lbUsed.Items.Values[Current.Name] := 'false'
  end;
end;

procedure TfrmPropertyEditor.lbAvailableClick(Sender: TObject);
var Current: PZProperty;
  List: TStrings;
begin
  btnRemove.Enabled := False;
  btnAdd.Enabled := True;
  if (lbAvailable.ItemIndex <> -1) and lbAvailable.Enabled then begin
    Current := PZProperty(lbAvailable.Items.Objects[lbAvailable.ItemIndex]);
    edString.Visible := False;
    cbBoolean.Visible := False;
    cbEnum.Visible := False;
    if Current = nil then Exit;
    mmDescrption.Text := Current.Purpose;
    gbVal.Caption := Current.Name;
      case Current.ValueType of
        pvtBool: begin
                   cbBoolean.Visible := True;
                   cbBoolean.Checked := StrToBoolEx(Current.Default);
                 end;
        pvtEnum: begin
                  cbEnum.Visible := True;
                  List := ZSysUtils.SplitString(Current.Values, '|;,');
                  try
                    cbEnum.Items.Assign(List);
                    if Current.Default <> ''
                    then cbEnum.ItemIndex := cbEnum.Items.IndexOf(Current.Default)
                    else cbEnum.ItemIndex := 0;
                  finally
                    List.Free;
                  end;
                 end;
        pvtNumber: begin
                   edString.Visible := True;
                   edString.Text := Current.Default;
                 end;
        pvtString,
        pvtBoolOrString: begin
                   edString.Visible := True;
                   edString.Text := Current.Default;
                end;
      end;
  end;
end;

procedure TfrmPropertyEditor.btnAddClick(Sender: TObject);
var Current: PZProperty;
  Value: String;
  SL: TStrings;
begin
  if lbAvailable.ItemIndex <> -1 then begin
    Current := PZProperty(lbAvailable.Items.Objects[lbAvailable.ItemIndex]);
    if Current <> nil then begin
      case Current.ValueType of
        pvtBool:  if cbBoolean.Checked
                  then Value := 'True'
                  else Value := 'False';
        pvtEnum:  Value := cbEnum.Text;
        pvtNumber,
        pvtString,
        pvtBoolOrString: Value := edString.Text;
        else Value := '';
      end;
      if (Value = '') then
        if (Current.ValueType <> pvtEmpty) then begin
          ShowMessage('Invalid value');
          Exit;
        end else
          Value := Current.Name
      else Value := Current.Name+'='+Value;
      FPropsUsed.Add(Current);
      FPropsUsed.Sort(compareProps);
      SL := TStringList.Create;
      SL.Assign(lbUsed.Items);
      SL.AddObject(Value, TObject(Current));
      TStringList(SL).Sort;
      lbUsed.Items.Assign(SL);
      SL.Free;
      FPropsUnUsed.Delete(FPropsUnUsed.IndexOf(Current));
    end;
    lbAvailable.Items.Delete(lbAvailable.ItemIndex);
  end;
  lbAvailableClick(Sender);
end;

{$IFDEF FPC}
initialization
{$i ZPropertiesEditor.lrs}
{$ENDIF}
end.
