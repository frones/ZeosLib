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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
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
  ZClasses, ZDbcProperties, ZDbcIntfs, ComCtrls,
  {$IFNDEF FPC}DesignIntf, DesignEditors{$ELSE}
  PropEdits, LCLIntf, {LResources, }ComponentEditors{$ENDIF};

type
  TZPropertyLevelTypes = set of (pltConnection, pltTransaction, pltStatement,
    pltResolver);

  { TfrmPropertyEditor }

  TfrmPropertyEditor = class(TForm)
    pcEditValues: TPageControl;
    tcDiffList: TTabSheet;
    pnlProps: TPanel;
    spltProps: TSplitter;
    bgPropsUsed: TGroupBox;
    lbUsed: TListBox;
    btnAdd: TButton;
    btnRemove: TButton;
    gbAvailable: TGroupBox;
    lbAvailable: TListBox;
    Splitter1: TSplitter;
    btnOK: TButton;
    btnCancel: TButton;
    pnlBottom: TPanel;
    lblProtocol: TLabel;
    lblServerProvider: TLabel;
    lblHostversion: TLabel;
    lblClientVersion: TLabel;
    cbProtocol: TComboBox;
    pnlValDesc: TPanel;
    Splitter2: TSplitter;
    gbVal: TGroupBox;
    cbEnum: TComboBox;
    edString: TEdit;
    gbDescription: TGroupBox;
    mmDescrption: TMemo;
    tsStringList: TTabSheet;
    cbProvider: TComboBox;
    mmStringList: TMemo;
    lblProtocols: TLabel;
    lblProviders: TLabel;
    cbHideAlias: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure lbUsedClick(Sender: TObject);
    procedure lbAvailableClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure pcEditValuesChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure cbProtocolChange(Sender: TObject);
    procedure lbUsedMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbAvailableMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure cbHideAliasClick(Sender: TObject);
  private
    { Private declarations }
    FZPropertyLevelTypes: TZPropertyLevelTypes;
    FServerProvider: TZServerProvider;
    FPropsUsed, FPropsUnused: TZSortedList;
    FSortedLines: TStrings;
    function compareProps(Item1, Item2: Pointer): Integer;
    procedure LoadProperties;
    procedure SetItemHint(ListBox: TListBox; ItemIndex: Integer);
    procedure SetUnusedItems;
  public
    { Public declarations }
    Lines: TStrings;
  end;

  {** Implements the basic methods of the property editor. }
  TZProperitesEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

Type
  TZPropertyValueType = (
    pvtEmpty,
    pvtEnum,
    pvtNumber,
    pvtString);

  PZPropertyProvider = ^TZPropertyProvider;
  TZPropertyProvider = record
    Provider: TZServerProvider;
    MinimumServerVersion: Integer;
    MinimumClientVersion: Integer;
    MinimumProtocolVersion: Integer;
  end;
  PZPropertyProviderArray = ^TZPropertyProviderArray;
  TZPropertyProviderArray = array[Byte] of TZPropertyProvider;

  TZPropertyProviders = record
    Count: Cardinal;
    Items: PZPropertyProviderArray;
  end;

  PProtocolArray = ^TProtocolArray;
  TProtocolArray = array[Byte] of String;

  TProtocols = record
    Count: Cardinal;
    Items: PProtocolArray;
  end;

  TZPropertyProtocols = record
    Count: Cardinal;
    Items: PZPropertyProviderArray;
  end;

  PZProperty = ^TZProperty;
  TZProperty = Record
    Name: String;
    Purpose: String;
    ValueType: TZPropertyValueType;
    LevelTypes: TZPropertyLevelTypes;
    Values, Default, Alias: String;
    Providers: TZPropertyProviders;
    Protocols: TProtocols;
  End;

  PZPropertyRefDynArray = ^TZPropertyRefDynArray;
  TZPropertyRefDynArray = array of PZProperty;

  PZPropertyArray = ^TZPropertyArray;
  TZPropertyArray = array[Byte] of PZProperty;

function GetZProperties: PZPropertyRefDynArray;
procedure RegisterZProperty(Value: PZProperty);
procedure RegisterZProperties(const Values: Array of PZProperty);
procedure RegisterZPropertiesArray(const Count: Cardinal; Values: PZPropertyArray);

implementation

uses TypInfo, Types,
  ZSysUtils, ZCompatibility,
  ZAbstractRODataset, ZAbstractDataset, ZAbstractConnection
  {$IFDEF ENABLE_MYSQL},ZPlainMySqlDriver{$ENDIF};

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

var
  prEditValuesPageIndex: Integer;
  frmPropertyHeight: Integer;
  frmPropertyWidth: Integer;
  frmPropertyTop: Integer;
  frmPropertyLeft: Integer;
  pnlBottomHeight: Integer;
  gbValWidth: Integer;
  bgPropsUsedWidth: Integer;
  HideEquals: Boolean;

{ EH: represent each constant in an record for a better control/description}
//wish of Jan@EH: instead of a constant array, use a dynamic array,
//so others can easily add it's own properties
var ZPropertyArray: TZPropertyRefDynArray;

function GetZProperties: PZPropertyRefDynArray;
begin
  Result := @ZPropertyArray;
end;

procedure RegisterZProperty(Value: PZProperty);
begin
  if Value <> nil then begin
    SetLength(ZPropertyArray, Length(ZPropertyArray)+1);
    ZPropertyArray[High(ZPropertyArray)] := Value;
  end;
end;

procedure RegisterZProperties(const Values: Array of PZProperty);
var I, Cnt, Idx: Integer;
begin
  Cnt := 0;
  for i := 0 to High(Values) do
    if Values[i] <> nil then
      Inc(Cnt);
  Idx := Length(ZPropertyArray);
  SetLength(ZPropertyArray, Idx+Cnt);
  for i := 0 to High(Values) do
    if Values[i] <> nil then begin
      ZPropertyArray[Idx] := Values[i];
      Inc(Idx);
    end;
end;

procedure RegisterZPropertiesArray(const Count: Cardinal; Values: PZPropertyArray);
var I, Cnt, Idx: Integer;
begin
  Cnt := 0;
  {$R-}
  for i := 0 to Count do
    if Values^[i] <> nil then
      Inc(Cnt);
  Idx := Length(ZPropertyArray);
  SetLength(ZPropertyArray, Idx+Cnt);
  for i := 0 to Count -1 do
    if Values^[i] <> nil then begin
      ZPropertyArray[Idx] := Values^[i];
      Inc(Idx);
    end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

procedure TfrmPropertyEditor.FormShow(Sender: TObject);
begin
  cbEnum.Top := edString.Top;
  if frmPropertyHeight <> -1 then
    Height := frmPropertyHeight;
  if frmPropertyWidth <> -1 then
    Width := frmPropertyWidth;
  if frmPropertyTop <> -1 then
    Top := frmPropertyTop;
  if frmPropertyLeft <> -1 then
    Left := frmPropertyLeft;
  if pnlBottomHeight <> -1 then
    pnlBottom.Height := pnlBottomHeight;
  if gbValWidth <> -1 then
    gbVal.Width := gbValWidth;
  if bgPropsUsedWidth <> -1 then
    bgPropsUsed.Width := bgPropsUsedWidth;
  cbHideAlias.Checked := HideEquals;
  LoadProperties;
end;

procedure TfrmPropertyEditor.pcEditValuesChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange := True;
  if pcEditValues.ActivePageIndex = 0 then begin
    mmStringList.Lines.Clear;
    mmStringList.Lines.AddStrings(lbUsed.Items);
  end else begin
    FSortedLines.Clear;
    FSortedLines.AddStrings(mmStringList.Lines);
    TStringList(FSortedLines).Sort;
    lbUsed.Items.Clear;
    lbUsed.Items.AddStrings(FSortedLines);
    LoadProperties;
  end;
end;

{ TZProperitesEditor }

procedure TZProperitesEditor.Edit;
var
  Component: TComponent;
label jmpProtocol;
begin
  Component := TComponent(GetComponent(0));
  with TfrmPropertyEditor.Create(Application) do
  try
    Lines := TStrings(GetOrdValue);
    if Component.InheritsFrom(TZAbstractConnection) then begin
      FZPropertyLevelTypes := [pltConnection, pltTransaction, pltStatement];
jmpProtocol:
      cbProtocol.ItemIndex := cbProtocol.Items.IndexOf(TZAbstractConnection(Component).Protocol);
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
    FSortedLines.AddStrings(Lines);
    TStringList(FSortedLines).Sort;
    lbUsed.Items.AddStrings(FSortedLines);
    pcEditValues.ActivePageIndex := prEditValuesPageIndex;
    if ShowModal = mrOk then
      if pcEditValues.ActivePageIndex = 0
      then SetOrdValue(NativeInt(lbUsed.Items))
      else SetOrdValue(NativeInt(mmStringList.Lines));
  finally
    Free;
  end;
end;

function TZProperitesEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

procedure TfrmPropertyEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPropsUsed);
  FreeAndNil(FPropsUnused);
  FreeAndNil(FSortedLines);
  frmPropertyHeight := Height;
  frmPropertyWidth  := Width;
  frmPropertyTop    := top;
  frmPropertyLeft   := left;
  pnlBottomHeight   := pnlBottom.Height;
  gbValWidth        := gbVal.Width;
  bgPropsUsedWidth  := bgPropsUsedWidth;
  prEditValuesPageIndex := pcEditValues.ActivePageIndex;
end;

procedure TfrmPropertyEditor.FormCreate(Sender: TObject);
var
  I, J: Integer;
  Drivers: IZCollection;
  Protocols: TStringDynArray;
  ServerProvider: TZServerProvider;
  TInfo: PTypeInfo;
begin
  FPropsUsed := TZSortedList.Create;
  FPropsUnUsed := TZSortedList.Create;
  FSortedLines := TStringList.Create;
  Drivers := DriverManager.GetDrivers;
  Protocols := nil;
  FSortedLines.Add('');
  for I := 0 to Drivers.Count - 1 do begin
    Protocols := (Drivers[I] as IZDriver).GetSupportedProtocols;
    for J := Low(Protocols) to High(Protocols) do
      if StartsWith(LowerCase(Protocols[J]), 'asa') then
        FSortedLines.AddObject(Protocols[J], TObject(Ord(spASA)))
      else if StartsWith(Protocols[J], 'firebird') then
        FSortedLines.AddObject(Protocols[J], TObject(Ord(spIB_FB)))
      else if StartsWith(Protocols[J], 'mssql') then
        FSortedLines.AddObject(Protocols[J], TObject(Ord(spMSSQL)))
      else if StartsWith(Protocols[J], 'mysql') then
        FSortedLines.AddObject(Protocols[J], TObject(Ord(spMySQL)))
      else if StartsWith(Protocols[J], 'oracle') then
        FSortedLines.AddObject(Protocols[J], TObject(Ord(spOracle)))
      else if StartsWith(Protocols[J], 'postgres') then
        FSortedLines.AddObject(Protocols[J], TObject(Ord(spPostgreSQL)))
      else if StartsWith(Protocols[J], 'sqlite') then
        FSortedLines.AddObject(Protocols[J], TObject(Ord(spSQLite)))
      else if StartsWith(Protocols[J], 'sybase') then
        FSortedLines.AddObject(Protocols[J], TObject(Ord(spASE)))
      else FSortedLines.AddObject(Protocols[J], nil);
  end;
  TStringList(FSortedLines).Sort;
  cbProtocol.Items.Assign(FSortedLines);
  FSortedLines.Clear;
  TInfo := TypeInfo(TZServerProvider);
  for ServerProvider := Low(TZServerProvider) to High(TZServerProvider) do
    cbProvider.Items.AddObject(GetEnumName(TInfo, Ord(ServerProvider)), TObject(Ord(ServerProvider)));
end;

procedure TfrmPropertyEditor.btnRemoveClick(Sender: TObject);
var I, idx: Integer;
  Current: PZProperty;
begin
  idx := lbUsed.ItemIndex;
  if lbUsed.ItemIndex <> -1 then begin
    Current := PZProperty(lbUsed.Items.Objects[Idx]);
    if Current <> nil then begin
      FPropsUnused.Add(Current);
      i := FPropsUsed.IndexOf(Current);
      FPropsUsed.Delete(I);
    end;
    lbUsed.Items.Delete(Idx);
    SetUnusedItems;
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
    I: Integer;
    TInfo: PTypeInfo;
begin
  btnRemove.Enabled := True;
  btnAdd.Enabled := False;
  if (lbUsed.ItemIndex <> -1) and lbUsed.Enabled then begin
    Current := PZProperty(lbUsed.Items.Objects[lbUsed.ItemIndex]);
    edString.Visible := False;
    cbEnum.Visible := False;
    if Current = nil then begin
      lblProtocols.Caption := 'Protocol(s): <ALL>';
      lblProviders.Caption := 'Provider(s): spUnknown';
      Exit;
    end;
    if Current.Protocols.Count = 0 then
      lblProtocols.Caption := 'Protocol(s): <ALL>'
    else begin
      lblProtocols.Caption := 'Protocols(s): '+Current.Protocols.Items^[0];
      for I := 1 to Current.Protocols.Count -1 do
        lblProtocols.Caption := lblProtocols.Caption+', '+Current.Protocols.Items^[I];
    end;
    if Current.Providers.Count = 0 then
      lblProviders.Caption := 'Provider(s): spUnknown'
    else begin
      TInfo := TypeInfo(TZServerProvider);
      lblProviders.Caption := 'Provider(s): '+GetEnumName(TInfo, Ord(Current.Providers.Items[0].Provider));
      for I := 1 to Current.Providers.Count -1 do
        lblProviders.Caption := lblProviders.Caption+', '+GetEnumName(TInfo, Ord(Current.Providers.Items[I].Provider));
    end;
    case Current.ValueType of
      pvtEnum: begin
                 cbEnum.Visible := True;
                 cbEnum.Text := lbUsed.Items.Values[Current.Name];
               end;
      pvtNumber,
      pvtString: begin
                 edString.Visible := True;
                 edString.Text := lbUsed.Items.Values[Current.Name];
              end;
      {$IFDEF WITH_CASE_WARNING}else;{$ENDIF}
    end;
  end;
end;

procedure TfrmPropertyEditor.lbAvailableClick(Sender: TObject);
var Current: PZProperty;
    List: TStrings;
    I: Integer;
    TInfo: PTypeInfo;
begin
  btnRemove.Enabled := False;
  btnAdd.Enabled := True;
  if (lbAvailable.ItemIndex <> -1) and lbAvailable.Enabled then begin
    Current := PZProperty(lbAvailable.Items.Objects[lbAvailable.ItemIndex]);
    edString.Visible := False;
    cbEnum.Visible := False;
    if Current = nil then begin
      lblProtocols.Caption := 'Protocol(s): <ALL>';
      lblProviders.Caption := 'Provider(s): spUnknown';
      Exit;
    end;
    if Current.Protocols.Count = 0 then
      lblProtocols.Caption := 'Protocol(s): <ALL>'
    else begin
      lblProtocols.Caption := 'Protocol(s): '+Current.Protocols.Items^[0];
      for I := 1 to Current.Protocols.Count -1 do
        lblProtocols.Caption := lblProtocols.Caption+', '+Current.Protocols.Items^[I];
    end;
    if Current.Providers.Count = 0 then
      lblProviders.Caption := 'Provider(s): spUnknown'
    else begin
      TInfo := TypeInfo(TZServerProvider);
      lblProviders.Caption := 'Providers: '+GetEnumName(TInfo, Ord(Current.Providers.Items[0].Provider));
      for I := 1 to Current.Providers.Count -1 do
        lblProviders.Caption := lblProviders.Caption+', '+GetEnumName(TInfo, Ord(Current.Providers.Items[I].Provider));
    end;
    mmDescrption.Text := Current.Purpose;
    gbVal.Caption := Current.Name;
      case Current.ValueType of
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
        pvtString: begin
                   edString.Visible := True;
                   edString.Text := Current.Default;
                end;
        {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
      end;
  end;
end;

procedure TfrmPropertyEditor.btnAddClick(Sender: TObject);
var Current: PZProperty;
  Value: String;
begin
  if lbAvailable.ItemIndex <> -1 then begin
    Current := PZProperty(lbAvailable.Items.Objects[lbAvailable.ItemIndex]);
    if Current <> nil then begin
      case Current.ValueType of
        pvtEnum:  Value := cbEnum.Text;
        pvtNumber,
        pvtString: Value := edString.Text;
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
      FSortedLines.Assign(lbUsed.Items);
      FSortedLines.AddObject(Value, TObject(Current));
      TStringList(FSortedLines).Sort;
      lbUsed.Items.Assign(FSortedLines);
      FSortedLines.Clear;
      FPropsUnused.Delete(FPropsUnused.IndexOf(Current));
    end;
    SetUnusedItems;
  end;
  lbAvailableClick(Sender);
end;

procedure TfrmPropertyEditor.LoadProperties;
var I, j: Integer;
  UpperUsed, UpperCurrent: String;
  Found: Boolean;
  Current: PZProperty;
begin
  FPropsUnused.Clear;
  FPropsUsed.Clear;
  for i := 0 to High(ZPropertyArray) do
    if ZPropertyArray[i] <> nil then begin
      Current := ZPropertyArray[i];
      if ((pltConnection in FZPropertyLevelTypes) and (pltConnection in Current.LevelTypes)) or
         ((pltTransaction in FZPropertyLevelTypes) and (pltTransaction in Current.LevelTypes)) or
         ((pltResolver in FZPropertyLevelTypes) and (pltResolver in Current.LevelTypes)) or
         ((pltStatement in FZPropertyLevelTypes) and (pltStatement in Current.LevelTypes) and not
           ((pltConnection in FZPropertyLevelTypes) and not (pltConnection in Current.LevelTypes))) then begin
        if (cbProtocol.Text <> '') and (Current.Protocols.Count > 0) and (Current.Protocols.Items <> nil) then begin
          Found := False;
          UpperCurrent := UpperCase(cbProtocol.Text);
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
          FPropsUnused.Add(Current);
      end;
    end;
  SetUnusedItems;
  if lbUsed.Items.Count > 0 then begin
    lbUsed.ItemIndex := 0;
    lbUsedClick(nil);
  end else if lbAvailable.Items.Count > 0 then begin
    lbAvailable.ItemIndex := 0;
    lbAvailableClick(nil);
  end else begin
    btnAdd.Enabled := False;
    btnRemove.Enabled := False;
  end;
end;

procedure TfrmPropertyEditor.cbProtocolChange(Sender: TObject);
begin
  LoadProperties;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Shift" not used} {$ENDIF}
procedure TfrmPropertyEditor.lbUsedMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  SetItemHint(lbUsed, lbUsed.ItemAtPos(Point(X, Y), true));
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Shift" not used} {$ENDIF}
procedure TfrmPropertyEditor.lbAvailableMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  SetItemHint(lbAvailable, lbAvailable.ItemAtPos(Point(X, Y), true));
end;
{$IFDEF FPC} {$POP} {$ENDIF}

var
  LastListBox: TListBox;
  LastItemIndex: Integer;

procedure TfrmPropertyEditor.SetItemHint(ListBox: TListBox; ItemIndex: Integer);
var Current: PZProperty;
begin
  if (LastListBox = ListBox) and (LastItemIndex = ItemIndex) then
    Exit;
  ListBox.ShowHint := False;
  Application.CancelHint;
  if (ItemIndex <> -1) and (ListBox.Items.Objects[ItemIndex] <> nil) then begin
    Current := PZProperty(ListBox.Items.Objects[ItemIndex]);
    ListBox.Hint := Current.Purpose;
    ListBox.ShowHint := True;
  end;
  LastItemIndex := ItemIndex;
  LastListBox := ListBox;
end;

procedure TfrmPropertyEditor.cbHideAliasClick(Sender: TObject);
begin
  LoadProperties;
end;

procedure TfrmPropertyEditor.SetUnusedItems;
var I, J, N: Integer;
  Current: PZProperty;
  Found: Boolean;
  UpperCurrent, UpperUsed: string;
begin
  FPropsUnused.Sort(compareProps);
  lbAvailable.Items.Clear;
  if cbHideAlias.Checked then begin
    for i := 0 to FPropsUnused.Count -1 do begin
      Current := FPropsUnused[i];
      if (Current <> nil) and (Current.Alias <> '') then begin
        ZSysUtils.PutSplitString(FSortedLines, Current.Alias, ',;');
        Found := False;
        for N := 0 to FSortedLines.Count -1 do begin
          UpperCurrent := UpperCase(FSortedLines[N]);
          for J := 0 to FPropsUsed.Count -1 do begin
            UpperUsed := UpperCase(PZProperty(FPropsUsed[j])^.Name);
            if UpperUsed = UpperCurrent then begin
              Found := True;
              Break;
            end;
          end;
          if Found then
            Break;
        end;
        FSortedLines.Clear;
        if not Found then
          lbAvailable.Items.AddObject(Current.Name, TObject(Current));
      end else
        lbAvailable.Items.AddObject(Current.Name, TObject(Current));
    end;
  end else for i := 0 to FPropsUnused.Count -1 do begin
    Current := FPropsUnused[i];
    lbAvailable.Items.AddObject(Current.Name, TObject(Current));
  end;
end;

const
  cBoolEnum = 'False|true';
  cBoolTrue = 'True';
  cBoolFalse = 'False';
  {cOff_On_Enum = 'Off|On';
  cOn = 'On';
  cOff = 'Off';}
  cNo_Yes_Enum = 'NO|YES';
  cYes = 'YES';
  cNo = 'NO';
  ZProp_UID: TZProperty = (
    Name: ConnProps_UID; Purpose: 'the login username (same as username)';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Username;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_Username: TZProperty = (
    Name: ConnProps_Username; Purpose: 'the login username (same as UID)';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_UID;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_PWD: TZProperty = (
    Name: ConnProps_PWD; Purpose: 'the login password';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Password;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_Password : TZProperty = (
    Name: ConnProps_Password; Purpose: 'the login password';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_PWD;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_LibLocation : TZProperty = (
    Name: ConnProps_LibLocation;
    Purpose: 'the client lib name with full path(optional)';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_CodePage : TZProperty = (
    Name: ConnProps_CodePage;
    Purpose: 'Codepage to interact with driver'+LineEnding+
      'for odbc_a/ole_db it''s implemented as:'+LineEnding+
      'set a custom codepage to notify zeos about conversion routines note: cp must be equal for all fields else use the W driver.'+LineEnding+
      'first place in a name, second use '':'' for the codepage, third use ''/'' for the maximum amount of bytes per character equal to database defined charset'+LineEnding+
      'example: codepage=latin1:1252/1 or characterset=utf8:65001/4';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  {ZProp_Transliterate: TZProperty = (
    Name: ConnProps_Transliterate;
    Purpose: 'transliterate between client-characterset and RawStringEncoding'+LineEnding+
      'this option might be interesting for !Ansi!-Compilers and drivers like SQLite'+LineEnding+
      'the more you can work with ut8 encoding and the database encoding is ansi or vice versa';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: 'ConnProps_Transliterate';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );}
  ZProp_ControlsCP : TZProperty = (
    Name: ConnProps_ControlsCP;
    Purpose: //'deprecated use RawStringEncoding instead'+LineEnding+
             'determine the stringField-Types of the dataset and/or '+LineEnding+
             'identify the raw string codepage for W-drivers on non unicode compilers for:'+LineEnding+
             'GetString()/SetString()/GetRawByteString()/SetRawByteString()'+LineEnding+
             'it''s also used for non A-Drivers if String-Translitation is enabled';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'CP_UTF8|GET_ACP'; Default: {$IFDEF LCL}'CP_UTF8'{$ELSE}'GET_ACP'{$ENDIF}; Alias: ConnProps_RawStringEncoding;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  {ZProp_RawStringEncoding : TZProperty = (
    Name: ConnProps_RawStringEncoding;
    Purpose: 'deprecated use RawStringEncoding instead'+LineEnding+
             'determine the stringField-Types of the dataset and/or '+LineEnding+
             'identify the raw string codepage for W-drivers on non unicode compilers for:'+LineEnding+
             'GetString()/SetString()/GetRawByteString()/SetRawByteString()'+LineEnding+
             'it''s also used for non A-Drivers if String-Translitation is enabled';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'CP_UTF8|GET_ACP'; Default: cBoolFalse; Alias: ConnProps_ControlsCP;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );}
  ZProp_Timeout : TZProperty = (
    Name: ConnProps_Timeout;
    Purpose: 'The login timeout to use in seconds.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: '10'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_DateReadFormat : TZProperty = (
    Name: ConnProps_DateReadFormat;
    Purpose: 'Format to read a date, like YYYY-MM-DD. Just simple Formats are supported.'+LineEnding+
             'Neither centuries nor weekdays and so on.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'YYYY-MM-DD'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_DateWriteFormat : TZProperty = (
    Name: ConnProps_DateWriteFormat;
    Purpose: 'Format to write a date, like YYYY-MM-DD. Just simple formats are supported.'+LineEnding+
             'Neither centuries nor weekdays and so on.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'YYYY-MM-DD'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_TimeReadFormat : TZProperty = (
    Name: ConnProps_TimeReadFormat;
    Purpose: 'Format to read time, like HH:MM:SS. Just simple formats are supported.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'HH:MM:SS.F'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_TimeWriteFormat : TZProperty = (
    Name: ConnProps_TimeWriteFormat;
    Purpose: 'Format to write time, like HH:MM:SS. Just simple formats are supported.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'HH:MM:SS.F'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_DateTimeReadFormat : TZProperty = (
    Name: ConnProps_DateTimeReadFormat;
    Purpose: 'Format to read date & time, like YYYY-MM-DD HH:NN:SS.F'+LineEnding+
       'Just simple formats are supported. ISO 8601 is prefered.'+LineEnding+
       'If the driver(f.e.SQLite) supports the ''T''delimiter do not hasitate to use!';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'YYYY-MM-DD HH:NN:SS.F'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_DateTimeWriteFormat : TZProperty = (
    Name: ConnProps_DateTimeWriteFormat;
    Purpose: 'Format to read date & time, like YYYY-MM-DD HH:NN:SS.F'+LineEnding+
       'Just simple formats are supported. ISO 8601 is prefered.'+LineEnding+
       'If the driver(f.e.SQLite) supports the ''T'' delimiter do not hasitate to use!';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'YYYY-MM-DD HH:NN:SS.F'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_IdentifierQuotes : TZProperty = (
    Name: ConnProps_IdentifierQuotes;
    Purpose: 'Overwrites TZAbstractDatabaseInfo.IdentifierQuotes property, used for Identifier quoting. '+LineEnding+
             'i.e. ADO, OleDB, ODBC, SQLite or Postgres dollar quotes...'+LineEnding+
             'skip it if you don''t use such driver''s'+LineEnding+
             'SQL standard 2003: "" ... wondering about ODBC-Syntax....';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: '""'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
{$IF declared(DSProps_PreferPrepared)}
  ZProp_PreferPrepared : TZProperty = (
    Name: DSProps_PreferPrepared;
    Purpose: 'Use prepared statements? We recommend you to use this.'+LineEnding+
      'The performance is much better then. Same as TZDatasetOptions.doPreferPrepared in Dataset.Options property'+LineEnding+
      'Drivers like Oracle, SQLite, ASE, Firebird, and Interbase always do prepare the stmt -> this property is ignored.'+LineEnding+
      'For drivers like ODBC, OleDb the property is used as "DEFERPREPARE" see manuals..'+LineEnding+
      'Some servers might fail to prepare the statments(MS-products are master of fails including unknown exceptions) -> turn it off on DataSet/Statement level if you run into that issue';
    ValueType: pvtEnum; LevelTypes: [pltStatement];
    Values: cBoolEnum; Default: cBoolTrue; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
{$IFEND}
  ZProp_KeyFields : TZProperty = (
    Name: DSProps_KeyFields;
    Purpose: 'like Field1[, Field2, ...] (valid separators are: "," or ";")'+LineEnding+
       'List of fields; if defined, they are used for locating and, if WhereMode = KeyOnly,'+LineEnding+
       'for constructing a WHERE clause';
    ValueType: pvtString; LevelTypes: [pltResolver];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_AffectedRows : TZProperty = (
    Name: DSProps_ValidateUpdateCount;
    Purpose: 'Check number of rows affected after executing a statement.'+LineEnding+
      'If the value is different to one an error is raised. Reason is we just update !one! record,'+LineEnding+
      ' and we do not expect to change many or zero rows the stmt did affect! Use a valid primary key!';
    ValueType: pvtEnum; LevelTypes: [pltResolver];
    Values: cBoolEnum; Default: cBoolTrue; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );

{$IF declared(DSProps_InternalBufSize)}
  ZProp_InternalBufSize : TZProperty = (
    Name: DSProps_InternalBufSize;
    Purpose: 'Drivers like ODBC, OleDB, Oracle, ASE(SACAPI) do allow block-fetches to reduce roundtrips.'+LineEnding+
      'Define memory in bytes for the block buffer. Default is 128Kb'+LineEnding+
      'Zeos will do a minimum allocation for one row.';
    ValueType: pvtNumber; LevelTypes: [pltStatement];
    Values: ''; Default: '131072'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
{$IFEND}
{$IF declared(DSProps_CachedLobs)}
  const All_Oracle_IB_FB_Postgre: array[0..3] of String = ('oracle', 'firebird', 'interbase', 'postrgres');
  ZProp_CachedLobs : TZProperty = (
    Name: DSProps_CachedLobs;
    Purpose: 'Cache the Lob-Streams? Used for Oracle-Lobs, All IB/FB-lob''s, '+
      'Postgre-OID-lob''s only. All other providers do not support a good '+
      'locator API. Servers like MySQL(real prepared), ASE do support late-fetching methods '+
      'but we need to refetch the whole row first if the cursor postion changes';
    ValueType: pvtEnum; LevelTypes: [pltConnection, pltStatement];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 4; Items: @All_Oracle_IB_FB_Postgre);
  );
{$IFEND}
{$IF declared(DSProps_UndefVarcharAsStringLength)}
  const All_Postgres_SQLite: array[0..1] of String = ('postrgres', 'sqlite');
  ZProp_UndefVarcharAsStringLength : TZProperty = (
    Name: DSProps_UndefVarcharAsStringLength;
    Purpose: 'Treat varchar fields without a length limit as if they had a '+
      'length limit of <maxlength> thus making these fields usable with '+
      'TDBEdit components.';
    ValueType: pvtNumber; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: '255'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @All_Postgres_SQLite);
  );
{$IFEND}

{$IF declared(ConnProps_Provider)}
  const AllOleDBAndADO: array[0..1] of String =
    ('OleDB','ADO');
  ZProp_OleDBProvider : TZProperty = (
    Name: ConnProps_Provider;
    Purpose: 'The OleDB-Provider if not spezified in the DataBase-String.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllOleDBAndADO);
  );
{$IFEND}
{$IF declared(ConnProps_TrustedConnection)}
  const AllODBC_OleDB_ADO: array[0..2] of String =
    ('odbc','OleDB','ADO');
  ZProp_TrustedConnection : TZProperty = (
    Name: ConnProps_TrustedConnection;
    Purpose: 'Use trusted connection?';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 3; Items: @AllODBC_OleDB_ADO);
  );
{$IFEND}
{$IF declared(DSProps_StatementTimeOut)}
  const AllODBC_OleDB_Firebird_Interbase: array[0..3] of String =
    ('odbc','OleDB','firebird','interbase');
  ZProp_StatementTimeOut : TZProperty = (
    Name: DSProps_StatementTimeOut;
    Purpose: 'Execution timeout of a statement.'+LineEnding+
      'Seconds for OleDB and ODBC, Milliseconds for Firebird and Interbase';
    ValueType: pvtNumber; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: '0'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 4; Items: @AllODBC_OleDB_Firebird_Interbase);
  );
{$IFEND}

{$IF declared(DSProps_DeferPrepare)}
  const AllODBC_OleDB: array[0..1] of String =
    ('odbc','OleDB');
  ZProp_DeferPrepare : TZProperty = (
    Name: DSProps_DeferPrepare;
    Purpose: 'Defer prepare? If not set we''ll try to prepere the [update|delete'+
      '|insert|select] statements immediately.'+LineEnding+
      'The more we try determine the parameter types, alloc the param-buffer '+
      'once and do not use parameter late-bindings. Thus it''s faster if NO '+
      'defer prepare is used'+LineEnding+
      'Some servers might fail to prepare the statments(MS-products are master '+
      'of fails including unknown exceptions) -> turn it off on DataSet/Statement '+
      'level if you run into that issue';
    ValueType: pvtEnum; LevelTypes: [pltStatement];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllODBC_OleDB);
  );
{$IFEND}

{$IF defined (ENABLE_MYSQL) or defined (ENABLE_POSTGRESQL)}
  const AllMySQL_MariaDB_Postgre: array[0..2] of String =
    ('mysql','mariadb','postgres');
  ZProp_MinExecCntBeforePrepare : TZProperty = (
    Name: DSProps_MinExecCntBeforePrepare;
    Purpose: 'How many executions must be done to realy prepare the statement '+
      'on the Server? JDBC does prepare after 4 executions. A negative '+
      'value means never prepare. A zero value means prepare immediately. '+LineEnding+
      'Actually default is 2 executions before prepare the stmt on the server';
    ValueType: pvtNumber; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: '2'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 3; Items: @AllMySQL_MariaDB_Postgre);
  );
  ZProp_EmulatePrepares : TZProperty = (
    Name: DSProps_EmulatePrepares;
    Purpose: 'Old postgres before protocol V3 can''t bind paramters. '+
      'MySQL may have known issuse(resolved inbetween? unkown for MariaDB) like: '+
      'https://dev.mysql.com/doc/refman/5.7/en/c-api-prepared-statement-problems.html '+
      'If enabled turn of parameter bindings and send composed strings instead.'+
      'That''s definitelly killing the performance so have a good reason like:'+LineEnding+
      'http://zeoslib.sourceforge.net/viewtopic.php?f=20&t=10695&p=30151#p30151';
    ValueType: pvtEnum; LevelTypes: [pltConnection, pltStatement];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 3; Items: @AllMySQL_MariaDB_Postgre);
  );
{$IFEND}

{$IFDEF ENABLE_DBLIB}
  const AllSybaseMSSQL: array[0..1] of String = ('sybase','mssql');
  ZProp_TDSVersion : TZProperty = (
    Name: ConnProps_TDSVersion;
    Purpose: '(DBLIB) If set, the TDS version will be set on connect or dbinit '+
      '(sybase-lib only) see ZPlainDbLibDriver.pas "TDSDBVERSION_?"s also see:'+ LineEnding+
      'https://www.freetds.org/userguide/ChoosingTdsProtocol.html'+LineEnding+
      'By default we set the latest protocol version';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSProtocolVersion : TZProperty = (
    Name: ConnProps_TDSProtocolVersion;
    Purpose: '(DBLIB) It''s the documented TDS Protocol Version like ''7.2''. '+
      'Purpose is equal to param '+ConnProps_TDSVersion+'. If set, the TDS '+
      'version will be set on connect or dbinit (sybase-lib only) see:'+ LineEnding+
      'https://www.freetds.org/userguide/ChoosingTdsProtocol.html'+LineEnding+
      'By default we set the latest protocol version';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_AnsiPadding : TZProperty = (
    Name: ConnProps_AnsiPadding;
    Purpose: 'Turn Ansi-Padding on/off. See Server-documentation.'+
      'If set, executes ''SET ANSI_PADDING ON'' on connect';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'OFF|ON'; Default: 'ON'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_AppName : TZProperty = (
    Name: ConnProps_AppName;
    Purpose: 'The application name to send to the server on connect.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  {$IFDEF ENABLE_ASA}
    const All_ASA_Sybase_MSSQL: array[0..2] of String = ('ASA','sybase','mssql');
  {$ENDIF}

  cLanguagePurpose = 'Specifies the language of the connection.'+LineEnding+
      '{ Language | LANG }=language-code'+LineEnding+
      'The two-letter combination representing a language. For example, '+
      'specifying Language=DE sets the default language to German.'+LineEnding+
      'This connection parameter establishes the language for the connection. '+
      'Any errors or warnings from the server are delivered in the specified '+
      'language, assuming that the server supports the language. If no language '+
      'is specified, the default language is used. This connection parameter '+
      'only affects the connection.';
  ZProp_Language : TZProperty = (
    Name: ConnProps_Language;
    Purpose: cLanguagePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    {$IFDEF ENABLE_ASA}
    Protocols: (Count: 3; Items: @All_ASA_Sybase_MSSQL);
    {$ELSE}
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
    {$ENDIF}
  );
  ZProp_Workstation : TZProperty = (
    Name: ConnProps_Workstation;
    Purpose: 'The workstation name to send to the server';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSLog : TZProperty = (
    Name: ConnProps_Log;
    Purpose: 'Write a TDS log file';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: ConnProps_Logging+','+ConnProps_TDSDump;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSLogging : TZProperty = (
    Name: ConnProps_Logging;
    Purpose: 'Write a TDS log file';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: ConnProps_Log+','+ConnProps_TDSDump;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSDump : TZProperty = (
    Name: ConnProps_TDSDump;
    Purpose: 'Write a TDS log file';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: ConnProps_Log+','+ConnProps_Logging;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSLogFile : TZProperty = (
    Name: ConnProps_LogFile;
    Purpose: 'Path to log file. If not set and log/dump is active, '+
      'the <AppPath>\<AppName>.tdslog will be used.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Log_File+','+ConnProps_TDSDumpFile;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSLog_File : TZProperty = (
    Name: ConnProps_Log_File;
    Purpose: 'Path to log file. If not set and log/dump is active, '+
      'the <AppPath>\<AppName>.tdslog will be used.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_LogFile+','+ConnProps_TDSDumpFile;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSDumpFile : TZProperty = (
    Name: ConnProps_TDSDumpFile;
    Purpose: 'Path to log file. If not set and log/dump is active, '+
      'the <AppPath>\<AppName>.tdslog will be used.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Log_File+','+ConnProps_Log_File;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSNTAuth : TZProperty = (
    Name: ConnProps_NTAuth;
    Purpose: 'Use Windows auth when connecting to server';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: ConnProps_Secure+','+ConnProps_Trusted;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSSecure : TZProperty = (
    Name: ConnProps_Secure;
    Purpose: 'Use Windows auth when connecting to server';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: ConnProps_NTAuth+','+ConnProps_Trusted;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSTrusted : TZProperty = (
    Name: ConnProps_Trusted;
    Purpose: 'Use Windows auth when connecting to server';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: ConnProps_NTAuth+','+ConnProps_Secure;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
{$ENDIF}
{$IFDEF ENABLE_MYSQL}
  const
    AllMySQL: array[0..1] of String = ('mariadb','mysql');
  const cMySQLProvider: TZPropertyProvider = (
    Provider: spMySQL; MinimumServerVersion: 10;
    MinimumClientVersion: 0; MinimumProtocolVersion: 0;);
  ZProp_MYSQLSSL : TZProperty = (
    Name: ConnProps_MYSQLSSL;
    Purpose: 'Enable SSL certificate loading.';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
  );
  ZProp_MYSQLCompress : TZProperty = (
    Name: ConnProps_Compress;
    Purpose: 'same as MYSQL_OPT_COMPRESS, refer to MySql manual for details';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
  );
  ZProp_MYSQLdbless : TZProperty = (
    Name: ConnProps_DBLess;
    Purpose: 'Same as CLIENT_CONNECT_WITH_DB, refer to MySql manual for details';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
  );
  ZProp_MySQL_FieldType_Bit_1_IsBoolean : TZProperty = (
    Name: ConnProps_MySQL_FieldType_Bit_1_IsBoolean;
    Purpose: 'Treat fieldtype BIT(1) as Boolean instead of ENUM(''Y'',''N'')'+LineEnding+
      'Default since 7.3';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolTrue; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
  );
  ZProp_MySQLDatadir : TZProperty = (
    Name: ConnProps_Datadir;
    Purpose: 'Refer to MySql manual for details. Used for mysql_init';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
  );
  ZProp_MySQLLibrary : TZProperty = (
    Name: ConnProps_Library;
    Purpose: 'Refer to MySql manual for details. Used for mysql_init';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
  );
  ZProp_MySQL_UseResult : TZProperty = (
    Name: DSProps_UseResult;
    Purpose: 'Fetching rows one by one using mysql_use_result(not prepared) '+
      'instead of  mysql_stmt_store_result(prepared)/mysql_store_result(not prepared) '+
      'this reduces the memory-consumtion of libmysql. Rows are fetched one by one.'+
      'Side effects: Each call is a roundtrip, i guess. Mysql is tabular '+
      'streamed. -> So you can''t use it within using metainformations or '+
      'multiple active resultsets.';
    ValueType: pvtEnum; LevelTypes: [pltStatement];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
  );
  ZProp_MySQL_STMT_ATTR_PREFETCH_ROWS : TZProperty = (
    Name: DSProps_PrefetchRows;
    Purpose: 'For prepared statements only. Sets STMT_ATTR_PREFETCH_ROWS '+
      'option, refer to MySql manual for details';
    ValueType: pvtNumber; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
  );
  ZProp_MySQL_chunk_size : TZProperty = (
    Name: DSProps_ChunkSize;
    Purpose: 'size of chunks for sending long data, depends to your network speed';
    ValueType: pvtNumber; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
  );
  ZProp_MYSQL_OPT_CONNECT_TIMEOUT : TZProperty = (
    Name: ConnProps_MYSQL_OPT_CONNECT_TIMEOUT;
    Purpose: 'The connect timeout in seconds.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Timeout;
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_COMPRESS : TZProperty = (
    Name: ConnProps_MYSQL_OPT_COMPRESS;
    Purpose: 'Compress all information sent between the client and the server '+
      'if possible.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Compress;
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_NAMED_PIPE : TZProperty = (
    Name: ConnProps_MYSQL_OPT_NAMED_PIPE;
    Purpose: 'Use a named pipe to connect to the MySQL server on Windows, if '+
      'the server permits named-pipe connections.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_INIT_COMMAND : TZProperty = (
    Name: ConnProps_MYSQL_INIT_COMMAND;
    Purpose: 'SQL statement to execute when connecting to the MySQL server. '+
      'Automatically re-executed if reconnection occurs.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_READ_DEFAULT_FILE : TZProperty = (
    Name: ConnProps_MYSQL_READ_DEFAULT_FILE;
    Purpose: 'Read options from the named option file instead of from my.cnf.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_READ_DEFAULT_GROUP : TZProperty = (
    Name: ConnProps_MYSQL_READ_DEFAULT_GROUP;
    Purpose: 'Read options from the named group from my.cnf or the file '+
      'specified with MYSQL_READ_DEFAULT_FILE.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_SET_CHARSET_DIR : TZProperty = (
    Name: ConnProps_MYSQL_SET_CHARSET_DIR;
    Purpose: 'The path name of the directory that contains character set '+
      'definition files.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_SET_CHARSET_NAME : TZProperty = (
    Name: ConnProps_MYSQL_SET_CHARSET_NAME;
    Purpose: 'The name of the character set to use as the default character '+
      'set. The argument can be MYSQL_AUTODETECT_CHARSET_NAME to cause the '+
      'character set to be autodetected based on the operating system setting.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_CodePage;
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_LOCAL_INFILE : TZProperty = (
    Name: ConnProps_MYSQL_OPT_LOCAL_INFILE;
    Purpose: 'This option affects client-side LOCAL capability for LOAD DATA '+
      'operations. By default, LOCAL capability is determined by the default '+
      'compiled into the MySQL client library. Refer MySQL manual.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_PROTOCOL : TZProperty = (
    Name: ConnProps_MYSQL_OPT_PROTOCOL;
    Purpose: 'Transport protocol to use for connection. Specify one of the '+
      'enum values of mysql_protocol_type defined in mysql.h.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_SHARED_MEMORY_BASE_NAME : TZProperty = (
    Name: ConnProps_MYSQL_SHARED_MEMORY_BASE_NAME;
    Purpose: 'The name of the shared-memory object for communication to the '+
      'server on Windows, if the server supports shared-memory connections. '+
      'Specify the same value as used for the shared_memory_base_name system '+
      'variable. of the mysqld server you want to connect to.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_READ_TIMEOUT : TZProperty = (
    Name: ConnProps_MYSQL_OPT_READ_TIMEOUT;
    Purpose: 'The timeout in seconds for each attempt to read from the server. '+
      'There are retries if necessary, so the total effective timeout value is '+
      'three times the option value. You can set the value so that a lost '+
      'connection can be detected earlier than the TCP/IP Close_Wait_Timeout '+
      'value of 10 minutes.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_WRITE_TIMEOUT : TZProperty = (
    Name: ConnProps_MYSQL_OPT_WRITE_TIMEOUT;
    Purpose: 'The timeout in seconds for each attempt to write to the server. '+
      'There is a retry if necessary, so the total effective timeout value is '+
      'two times the option value.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_USE_RESULT : TZProperty = (
    Name: ConnProps_MYSQL_OPT_USE_RESULT;
    Purpose: 'This option is unused.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_USE_REMOTE_CONNECTION : TZProperty = (
    Name: ConnProps_MYSQL_OPT_USE_REMOTE_CONNECTION;
    Purpose: 'For an application linked against the libmysqld embedded server '+
      'library, this forces the use of a remote server for the connection. '+
      'This option is ignored for applications linked against the libmysqlclient '+
      'client library.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_USE_EMBEDDED_CONNECTION : TZProperty = (
    Name: ConnProps_MYSQL_OPT_USE_EMBEDDED_CONNECTION;
    Purpose: 'For an application linked against the libmysqld embedded server '+
      'library, this forces the use of the embedded server for the connection. '+
      'This option is ignored for applications linked against the libmysqlclient '+
      'client library.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_GUESS_CONNECTION : TZProperty = (
    Name: ConnProps_MYSQL_OPT_GUESS_CONNECTION;
    Purpose: 'For an application linked against the libmysqld embedded server '+
      'library, this enables the library to guess whether to use the embedded '+
      'server or a remote server. “Guess” means that if the host name is set '+
      'and is not localhost, it uses a remote server. This behavior is the '+
      'default. MYSQL_OPT_USE_EMBEDDED_CONNECTION and '+
      'MYSQL_OPT_USE_REMOTE_CONNECTION can be used to override it. This option '+
      'is ignored for applications linked against the libmysqlclient client '+
      'library.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_SET_CLIENT_IP : TZProperty = (
    Name: ConnProps_MYSQL_SET_CLIENT_IP;
    Purpose: 'For an application linked against the libmysqld embedded server '+
      'library (when libmysqld is compiled with authentication support), this '+
      'means that the user is considered to have connected from the specified '+
      'IP address (specified as a string) for authentication purposes. This '+
      'option is ignored for applications linked against the libmysqlclient '+
      'client library.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_SECURE_AUTH : TZProperty = (
    Name: ConnProps_MYSQL_SECURE_AUTH;
    Purpose: 'Whether to connect to a server that does not support the password '+
      'hashing used in MySQL 4.1.1 and later.';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_REPORT_DATA_TRUNCATION : TZProperty = (
    Name: ConnProps_MYSQL_REPORT_DATA_TRUNCATION;
    Purpose: 'Enable or disable reporting of data truncation errors for '+
      'prepared statements using the error member of MYSQL_BIND structures. '+
      '(Default: enabled.) Added in 5.0.3.';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolTrue; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_RECONNECT : TZProperty = (
    Name: ConnProps_MYSQL_OPT_RECONNECT;
    Purpose: 'Enable or disable automatic reconnection to the server if the '+
      'connection is found to have been lost. Reconnect has been off by default '+
      'since MySQL 5.0.3; this option is new in 5.0.13 and provides a way to '+
      'set reconnection behavior explicitly.';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolTrue; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_SSL_VERIFY_SERVER_CERT : TZProperty = (
    Name: ConnProps_MYSQL_OPT_SSL_VERIFY_SERVER_CERT;
    Purpose: 'Enable or disable verification of the server''s Common Name value '+
      'in its certificate against the host name used when connecting to the '+
      'server. The connection is rejected if there is a mismatch. For encrypted '+
      'connections, this feature can be used to prevent man-in-the-middle '+
      'attacks. Verification is disabled by default. Added in MySQL 5.0.23';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_PLUGIN_DIR : TZProperty = (
    Name: ConnProps_MYSQL_PLUGIN_DIR;
    Purpose: 'Specify the location of client plugins. The plugin directory can '+
      'also be specified with the MARIADB_PLUGIN_DIR environment variable.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_DEFAULT_AUTH : TZProperty = (
    Name: ConnProps_MYSQL_DEFAULT_AUTH;
    Purpose: 'Default authentication client-side plugin to use.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_BIND : TZProperty = (
    Name: ConnProps_MYSQL_OPT_BIND;
    Purpose: 'Specify the network interface from which to connect to MariaDB '+
      'Server.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_SSL_KEY : TZProperty = (
    Name: ConnProps_MYSQL_OPT_SSL_KEY;
    Purpose: 'TLS option. Defines a path to a private key file to use for TLS. '+
      'This option requires that you use the absolute path, not a relative path. '+
      'If the key is protected with a passphrase, the passphrase needs to be '+
      'specified with MARIADB_OPT_TLS_PASSPHRASE option.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_SSL_CERT : TZProperty = (
    Name: ConnProps_MYSQL_OPT_SSL_CERT;
    Purpose: 'Defines a path to the X509 certificate file to use for TLS. This '+
      'option requires that you use the absolute path, not a relative path.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_SSL_CA : TZProperty = (
    Name: ConnProps_MYSQL_OPT_SSL_CA;
    Purpose: 'Defines a path to a PEM file that should contain one or more '+
      'X509 certificates for trusted Certificate Authorities (CAs) to use for '+
      'TLS. This option requires that you use the absolute path, not a '+
      'relative path.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_SSL_CAPATH : TZProperty = (
    Name: ConnProps_MYSQL_OPT_SSL_CAPATH;
    Purpose: 'Defines a path to a directory that contains one or more PEM '+
      'files that should each contain one X509 certificate for a trusted '+
      'Certificate Authority (CA) to use for TLS. This option requires that '+
      'you use the absolute path, not a relative path. The directory specified '+
      'by this option needs to be run through the openssl rehash command.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_SSL_CIPHER : TZProperty = (
    Name: ConnProps_MYSQL_OPT_SSL_CIPHER;
    Purpose: 'Defines a list of permitted ciphers or cipher suites to use for TLS.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_SSL_CRL : TZProperty = (
    Name: ConnProps_MYSQL_OPT_SSL_CRL;
    Purpose: 'Defines a path to a PEM file that should contain one or more '+
      'revoked X509 certificates to use for TLS. This option requires that you '+
      'use the absolute path, not a relative path.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_SSL_CRLPATH : TZProperty = (
    Name: ConnProps_MYSQL_OPT_SSL_CRLPATH;
    Purpose: 'Defines a path to a directory that contains one or more PEM '+
      'files that should each contain one revoked X509 certificate to use for '+
      'TLS. This option requires that you use the absolute path, not a '+
      'relative path.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_CONNECT_ATTR_RESET : TZProperty = (
    Name: ConnProps_MYSQL_OPT_CONNECT_ATTR_RESET;
    Purpose: 'Clears the current list of connection attributes.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_CONNECT_ATTR_ADD : TZProperty = (
    Name: ConnProps_MYSQL_OPT_CONNECT_ATTR_ADD;
    Purpose: 'Adds a key/value pair to connection attributes.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_CONNECT_ATTR_DELETE : TZProperty = (
    Name: ConnProps_MYSQL_OPT_CONNECT_ATTR_DELETE;
    Purpose: 'Deletes a connection attribute for the given key.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_SERVER_PUBLIC_KEY : TZProperty = (
    Name: ConnProps_MYSQL_SERVER_PUBLIC_KEY;
    Purpose: 'Specifies the name of the file which contains the RSA public '+
      'key of the database server. The format of this file must be in PEM '+
      'format. This option is used by the caching_sha2_password client '+
      'authentication plugin. It was introduced in Connector/C 3.1.0.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_ENABLE_CLEARTEXT_PLUGIN : TZProperty = (
    Name: ConnProps_MYSQL_ENABLE_CLEARTEXT_PLUGIN;
    Purpose: 'This option is supported to be compatible with MySQL client '+
      'libraries. MySQL client libraries use this option to determine whether '+
      'the mysql_clear_password authentication plugin can be used. However, '+
      'MariaDB clients and client libraries do not need to set any options in '+
      'order to use this authentication plugin. Therefore, this option does '+
      'not actually do anything in MariaDB Connector/C.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS : TZProperty = (
    Name: ConnProps_MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS;
    Purpose: 'If this option is set, the client indicates that it will be able '+
      'to handle expired passwords by setting the '+
      'CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS capability flag. If the password '+
      'has expired and CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS is set, the server '+
      'will not return an error when connecting, but put the connection in '+
      'sandbox mode, where all commands will return error 1820 '+
      '(ER_MUST_CHANGE_PASSWORD) unless a new password was set. This option '+
      'was added in MariaDB Connector/C 3.0.4';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_SSL_ENFORCE : TZProperty = (
    Name: ConnProps_MYSQL_OPT_SSL_ENFORCE;
    Purpose: 'Whether to force TLS. This enables TLS with the default system '+
      'settings. It does not prevent the connection from being created if the '+
      'server does not support TLS';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_MAX_ALLOWED_PACKET : TZProperty = (
    Name: ConnProps_MYSQL_OPT_MAX_ALLOWED_PACKET;
    Purpose: 'The maximum packet length to send to or receive from server. '+
      'The default is 16MB, the maximum 1GB.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_NET_BUFFER_LENGTH : TZProperty = (
    Name: ConnProps_MYSQL_OPT_NET_BUFFER_LENGTH;
    Purpose: 'The buffer size for TCP/IP and socket communication. Default is 16KB.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_TLS_VERSION : TZProperty = (
    Name: ConnProps_MYSQL_OPT_TLS_VERSION;
    Purpose: 'Which protocols the client permits for encrypted connections. '+
      'The value is a list of one or more comma-separated protocol versions. '+
      'The protocols that can be named for this option depend on the SSL '+
      'library used to compile MySQL.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZProp_MYSQL_OPT_SSL_MODE : TZProperty = (
    Name: ConnProps_MYSQL_OPT_SSL_MODE;
    Purpose: 'The security state to use for the connection to the server: '+
      'SSL_MODE_DISABLED, SSL_MODE_PREFERRED, SSL_MODE_REQUIRED, '+
      'SSL_MODE_VERIFY_CA, SSL_MODE_VERIFY_IDENTITY. If this option is not '+
      'specified, the default is SSL_MODE_PREFERRED. These modes are the '+
      'permitted values of the mysql_ssl_mode enumeration defined in mysql.h.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 {MySQL 8:}
 ZProp_MYSQL_OPT_GET_SERVER_PUBLIC_KEY : TZProperty = (
    Name: ConnProps_MYSQL_OPT_GET_SERVER_PUBLIC_KEY;
    Purpose: 'Enables the client to request from the server the public key '+
      'required for RSA key pair-based password exchange. This option applies '+
      'to clients that authenticate with the caching_sha2_password '+
      'authentication plugin. For that plugin, the server does not send the '+
      'public key unless requested. This option is ignored for accounts that '+
      'do not authenticate with that plugin. It is also ignored if RSA-based '+
      'password exchange is not used, as is the case when the client connects '+
      'to the server using a secure connection.';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MYSQL_OPT_RETRY_COUNT : TZProperty = (
    Name: ConnProps_MYSQL_OPT_RETRY_COUNT;
    Purpose: 'The retry count for I/O-related system calls that are '+
      'interrupted while connecting to the server or communicating with it. '+
      'If this option is not specified, the default value is 1 (1 retry if '+
      'the initial call is interrupted for 2 tries total).';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MYSQL_OPT_OPTIONAL_RESULTSET_METADATA : TZProperty = (
    Name: ConnProps_MYSQL_OPT_OPTIONAL_RESULTSET_METADATA;
    Purpose: 'This flag makes result set metadata optional. It is an '+
      'alternative way of setting the CLIENT_OPTIONAL_RESULTSET_METADATA '+
      'connection flag for the mysql_real_connect() function.';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MYSQL_OPT_SSL_FIPS_MODE : TZProperty = (
    Name: ConnProps_MYSQL_OPT_SSL_FIPS_MODE;
    Purpose: 'Controls whether to enable FIPS mode on the client side. '+
      'The MYSQL_OPT_SSL_FIPS_MODE option differs from other MYSQL_OPT_SSL_xxx '+
      'options in that it is not used to establish encrypted connections, '+
      'but rather to affect which cryptographic operations to permit.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MYSQL_OPT_TLS_CIPHERSUITES : TZProperty = (
    Name: ConnProps_MYSQL_OPT_TLS_CIPHERSUITES;
    Purpose: 'Which ciphersuites the client permits for encrypted connections '+
      'that use TLSv1.3. The value is a list of one or more colon-separated '+
      'ciphersuite names. The ciphersuites that can be named for this option '+
      'depend on the SSL library used to compile MySQL.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MYSQL_OPT_COMPRESSION_ALGORITHMS : TZProperty = (
    Name: ConnProps_MYSQL_OPT_COMPRESSION_ALGORITHMS;
    Purpose: 'The permitted compression algorithms for connections to the '+
      'server. The available algorithms are the same as for the '+
      'protocol_compression_algorithms system variable. If this option is not '+
      'specified, the default value is uncompressed.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MYSQL_OPT_ZSTD_COMPRESSION_LEVEL : TZProperty = (
    Name: ConnProps_MYSQL_OPT_ZSTD_COMPRESSION_LEVEL;
    Purpose: 'The compression level to use for connections to the server that '+
      'use the zstd compression algorithm. The permitted levels are from 1 to '+
      '22, with larger values indicating increasing levels of compression. '+
      'If this option is not specified, the default zstd compression level is '+
      '3. The compression level setting has no effect on connections that do '+
      'not use zstd compression.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 2; Items: @AllMySQL);
 );
 ZMySqlOptionProperties: Array[TMySqlOption] of PZProperty = (
    @ZProp_MYSQL_OPT_CONNECT_TIMEOUT, @ZProp_MYSQL_OPT_COMPRESS,
    @ZProp_MYSQL_OPT_NAMED_PIPE, @ZProp_MYSQL_INIT_COMMAND,
    @ZProp_MYSQL_READ_DEFAULT_FILE, @ZProp_MYSQL_READ_DEFAULT_GROUP,
    @ZProp_MYSQL_SET_CHARSET_DIR, @ZProp_MYSQL_SET_CHARSET_NAME,
    @ZProp_MYSQL_OPT_LOCAL_INFILE, @ZProp_MYSQL_OPT_PROTOCOL,
    @ZProp_MYSQL_SHARED_MEMORY_BASE_NAME, @ZProp_MYSQL_OPT_READ_TIMEOUT,
    @ZProp_MYSQL_OPT_WRITE_TIMEOUT, @ZProp_MYSQL_OPT_USE_RESULT,
    @ZProp_MYSQL_OPT_USE_REMOTE_CONNECTION, @ZProp_MYSQL_OPT_USE_EMBEDDED_CONNECTION,
    @ZProp_MYSQL_OPT_GUESS_CONNECTION, @ZProp_MYSQL_SET_CLIENT_IP,
    @ZProp_MYSQL_SECURE_AUTH, @ZProp_MYSQL_REPORT_DATA_TRUNCATION,
    @ZProp_MYSQL_OPT_RECONNECT, @ZProp_MYSQL_OPT_SSL_VERIFY_SERVER_CERT,
    @ZProp_MYSQL_PLUGIN_DIR, @ZProp_MYSQL_DEFAULT_AUTH,
    @ZProp_MYSQL_OPT_BIND, @ZProp_MYSQL_OPT_SSL_KEY, @ZProp_MYSQL_OPT_SSL_CERT,
    @ZProp_MYSQL_OPT_SSL_CA, @ZProp_MYSQL_OPT_SSL_CAPATH,
    @ZProp_MYSQL_OPT_SSL_CIPHER, @ZProp_MYSQL_OPT_SSL_CRL,
    @ZProp_MYSQL_OPT_SSL_CRLPATH, @ZProp_MYSQL_OPT_CONNECT_ATTR_RESET,
    @ZProp_MYSQL_OPT_CONNECT_ATTR_ADD, @ZProp_MYSQL_OPT_CONNECT_ATTR_DELETE,
    @ZProp_MYSQL_SERVER_PUBLIC_KEY, @ZProp_MYSQL_ENABLE_CLEARTEXT_PLUGIN,
    @ZProp_MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS, @ZProp_MYSQL_OPT_SSL_ENFORCE,
    @ZProp_MYSQL_OPT_MAX_ALLOWED_PACKET, @ZProp_MYSQL_OPT_NET_BUFFER_LENGTH,
    @ZProp_MYSQL_OPT_TLS_VERSION, @ZProp_MYSQL_OPT_SSL_MODE,
    {MySQL 8:}@ZProp_MYSQL_OPT_GET_SERVER_PUBLIC_KEY, @ZProp_MYSQL_OPT_RETRY_COUNT,
    @ZProp_MYSQL_OPT_OPTIONAL_RESULTSET_METADATA, @ZProp_MYSQL_OPT_SSL_FIPS_MODE,
    @ZProp_MYSQL_OPT_TLS_CIPHERSUITES, @ZProp_MYSQL_OPT_COMPRESSION_ALGORITHMS,
    @ZProp_MYSQL_OPT_ZSTD_COMPRESSION_LEVEL);

 ZProp_MYSQL_DATABASE_DRIVER : TZProperty = (
    Name: ConnProps_MYSQL_DATABASE_DRIVER;
    Purpose: 'Unknown';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_SSL_FP : TZProperty = (
    Name: ConnProps_MARIADB_OPT_SSL_FP;
    Purpose: 'Specify the SHA1 fingerprint of a server certificate for '+
      'validation during the TLS handshake. This is deprecated. Use '+
      'MARIADB_OPT_TLS_PEER_FP instead.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_SSL_FP_LIST : TZProperty = (
    Name: ConnProps_MARIADB_OPT_SSL_FP_LIST;
    Purpose: 'Specify a file which contains one or more SHA1 fingerprints of '+
      'server certificates for validation during the TLS handshake. This is '+
      'deprecated. Use MARIADB_OPT_TLS_PEER_FP_LIST instead.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_TLS_PASSPHRASE : TZProperty = (
    Name: ConnProps_MARIADB_OPT_TLS_PASSPHRASE;
    Purpose: 'Specify a passphrase for a passphrase-protected private key, '+
      'as configured by the MYSQL_OPT_SSL_KEY option. This option is only '+
      'supported if the connector was built with OpenSSL or GnuTLS. If the '+
      'connector was built with Schannel, then this option is not supported. '+
      'See TLS and Cryptography Libraries Used by MariaDB for more information '+
      'about which libraries are used on which platforms.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_TLS_CIPHER_STRENGTH : TZProperty = (
    Name: ConnProps_MARIADB_OPT_TLS_CIPHER_STRENGTH;
    Purpose: 'Cipher strength. This value will be passed as an unsigned int parameter.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_TLS_VERSION : TZProperty = (
    Name: ConnProps_MARIADB_OPT_TLS_VERSION;
    Purpose: 'Defines which TLS protocol versions are allowed. This should be a '+
      'comma-separated list of TLS protocol versions to allow. Valid TLS '+
      'protocol versions are TLSv1.0, TLSv1.1, TLSv1.2, and TLSv1.3. Both the '+
      'client and server should support the allowed TLS protocol versions. See '+
      'Secure Connections Overview: TLS Protocol Version Support for information '+
      'on which TLS libraries support which TLS protocol versions. See TLS and '+
      'Cryptography Libraries Used by MariaDB for more information about which '+
      'TLS libraries are used on which platforms.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_TLS_PEER_FP : TZProperty = (
    Name: ConnProps_MARIADB_OPT_TLS_PEER_FP;
    Purpose: 'Specify the SHA1 fingerprint of a server certificate for '+
      'validation during the TLS handshake.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_TLS_PEER_FP_LIST : TZProperty = (
    Name: ConnProps_MARIADB_OPT_TLS_PEER_FP_LIST;
    Purpose: 'Specify a file which contains one or more SHA1 fingerprints of '+
      'server certificates for validation during the TLS handshake.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_CONNECTION_READ_ONLY : TZProperty = (
    Name: ConnProps_MARIADB_OPT_CONNECTION_READ_ONLY;
    Purpose: 'This option is used by connection handler plugins and indicates '+
      'that the current connection will be used for read operations only.';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: '0|1'; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MYSQL_OPT_CONNECT_ATTRS : TZProperty = (
    Name: ConnProps_MYSQL_OPT_CONNECT_ATTRS;
    Purpose: 'for mysql_get_optionv.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_USERDATA : TZProperty = (
    Name: ConnProps_MARIADB_OPT_USERDATA;
    Purpose: 'Bundle user data to the current connection, e.g. for use in '+
      'connection handler plugins. This option requires 4 parameters: '+
      'connection, option, key and value.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_CONNECTION_HANDLER : TZProperty = (
    Name: ConnProps_MARIADB_OPT_CONNECTION_HANDLER;
    Purpose: 'Specify the name of a connection handler plugin.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_PORT : TZProperty = (
    Name: ConnProps_MARIADB_OPT_PORT;
    Purpose: 'Port number to use for connection.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: '3307'; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_UNIXSOCKET : TZProperty = (
    Name: ConnProps_MARIADB_OPT_UNIXSOCKET;
    Purpose: 'For connections to localhost, the Unix socket file to use, or, '+
      'on Windows, the name of the named pipe to use.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_PASSWORD : TZProperty = (
    Name: ConnProps_MARIADB_OPT_PASSWORD;
    Purpose: 'Password of the user to login to the server.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Username;
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_HOST : TZProperty = (
    Name: ConnProps_MARIADB_OPT_HOST;
    Purpose: 'Hostname or IP address of the server to connect to.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_USER : TZProperty = (
    Name: ConnProps_MARIADB_OPT_USER;
    Purpose: 'User to login to the server.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Username;
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_SCHEMA : TZProperty = (
    Name: ConnProps_MARIADB_OPT_SCHEMA;
    Purpose: 'Database to use.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_DEBUG : TZProperty = (
    Name: ConnProps_MARIADB_OPT_DEBUG;
    Purpose: 'Unknown.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_FOUND_ROWS : TZProperty = (
    Name: ConnProps_MARIADB_OPT_FOUND_ROWS;
    Purpose: 'Return the number of matched rows instead of number of changed rows.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: '0|1'; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_MULTI_RESULTS : TZProperty = (
    Name: ConnProps_MARIADB_OPT_MULTI_RESULTS;
    Purpose: 'Indicates that the client is able to handle multiple result sets '+
      'from stored procedures or multi statements. This option will be '+
      'automatically set if MARIADB_OPT_MULTI_STATEMENTS is set.';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: '1'; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_MULTI_STATEMENTS : TZProperty = (
    Name: ConnProps_MARIADB_OPT_MULTI_STATEMENTS;
    Purpose: 'Allows the client to send multiple statements in one command. '+
      'Statements will be divided by a semicolon.';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: '1'; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_INTERACTIVE : TZProperty = (
    Name: ConnProps_MARIADB_OPT_INTERACTIVE;
    Purpose: 'not documented.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_PROXY_HEADER : TZProperty = (
    Name: ConnProps_MARIADB_OPT_PROXY_HEADER;
    Purpose: 'not documented.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZProp_MARIADB_OPT_IO_WAIT : TZProperty = (
    Name: ConnProps_MARIADB_OPT_IO_WAIT;
    Purpose: 'not documented.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cMySQLProvider);
    Protocols: (Count: 1; Items: @AllMySQL[1]);
 );
 ZMariaDbConnectorOptionProperties: Array[Low(TMariaDBConnectorOption)..High(TMariaDBConnectorOption)] of PZProperty = (
    @ZProp_MYSQL_DATABASE_DRIVER,
    @ZProp_MARIADB_OPT_SSL_FP,             // deprecated, use MARIADB_OPT_TLS_PEER_FP instead
    @ZProp_MARIADB_OPT_SSL_FP_LIST,        // deprecated, use MARIADB_OPT_TLS_PEER_FP_LIST instead
    @ZProp_MARIADB_OPT_TLS_PASSPHRASE,     // passphrase for encrypted certificates
    @ZProp_MARIADB_OPT_TLS_CIPHER_STRENGTH,
    @ZProp_MARIADB_OPT_TLS_VERSION,
    @ZProp_MARIADB_OPT_TLS_PEER_FP,            // single finger print for server certificate verification
    @ZProp_MARIADB_OPT_TLS_PEER_FP_LIST,       // finger print white list for server certificate verification
    @ZProp_MARIADB_OPT_CONNECTION_READ_ONLY,
    @ZProp_MYSQL_OPT_CONNECT_ATTRS,        // for mysql_get_optionv
    @ZProp_MARIADB_OPT_USERDATA,
    @ZProp_MARIADB_OPT_CONNECTION_HANDLER,
    @ZProp_MARIADB_OPT_PORT,
    @ZProp_MARIADB_OPT_UNIXSOCKET,
    @ZProp_MARIADB_OPT_PASSWORD,
    @ZProp_MARIADB_OPT_HOST,
    @ZProp_MARIADB_OPT_USER,
    @ZProp_MARIADB_OPT_SCHEMA,
    @ZProp_MARIADB_OPT_DEBUG,
    @ZProp_MARIADB_OPT_FOUND_ROWS,
    @ZProp_MARIADB_OPT_MULTI_RESULTS,
    @ZProp_MARIADB_OPT_MULTI_STATEMENTS,
    @ZProp_MARIADB_OPT_INTERACTIVE,
    @ZProp_MARIADB_OPT_PROXY_HEADER,
    @ZProp_MARIADB_OPT_IO_WAIT
);
{$ENDIF}

{$IF defined(ENABLE_INTERBASE) OR DEFINED(ENABLE_FIREBIRD)}
  const AllInterbaseAndFirebirdProtocols: array[0..1] of String =
    ('firebird','interbase');
  const cInterbaseAndFirebirdProvider: TZPropertyProvider = (
    Provider: spIB_FB; MinimumServerVersion: 0;
    MinimumClientVersion: 0; MinimumProtocolVersion: 0;);
  ZProp_IB_FB_Dialect : TZProperty = (
    Name: ConnProps_Dialect;
    Purpose: 'Dialect of API communication';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: '1|3'; Default: '3'; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_IB_FB_Rolename : TZProperty = (
    Name: ConnProps_Rolename;
    Purpose: 'Name of the role the user connects with';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  const cFirebird3upProvider: TZPropertyProvider = (
    Provider: spIB_FB; MinimumServerVersion: 3000000;
    MinimumClientVersion: 0; MinimumProtocolVersion: 0;);
  ZProp_IB_FB_WireCompression : TZProperty = (
    Name: ConnProps_WireCompression;
    Purpose: 'Enable the wire compression in Firebird 3.0 and above. '+
      'This option generates isc_dpb_config string';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cFirebird3upProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols[0]);
  );
  ZProp_FBProtocol : TZProperty = (
    Name: ConnProps_FBProtocol;
    Purpose: 'can be used to define the firebird protocol to be used '+
      'for FB 3.0 this will enable the construction of url style connection '+
      'strings. see firebird 3.0 release notes';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'INET|WNET|XNET|LOCAL'; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cFirebird3upProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_CreateNewDatabase : TZProperty = (
    Name: ConnProps_CreateNewDatabase;
    Purpose: 'Create new DB on the given path on connect. You can use '+
      'the sql-statment or since FB2.5+ just use the value "true"';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_SetGUIDByType : TZProperty = (
    Name: ConnProps_SetGUIDByType;
    Purpose: 'Set a type of **all** CHAR(16) CHAR SET OCTETS fields to GUID.';
    ValueType: pvtString; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_GUIDDomains : TZProperty = (
    Name: ConnProps_GUIDDomains;
    Purpose: 'like Domain1[, Domain2, ...] (separators: "," or ";") '+
      'List of domains; if defined, fields of that domains will get GUID type';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_GUIDFields : TZProperty = (
    Name: DSProps_GUIDFields;
    Purpose: 'like Domain1[, Domain2, ...] (separators: "," or ";") '+
      'List of fields; if defined, fields with these names will get GUID type';
    ValueType: pvtString; LevelTypes: [pltStatement];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_Charset_NONE_Alias : TZProperty = (
    Name: ConnProps_Charset_NONE_Alias;
    Purpose: 'Identify the CodePage of characterset "NONE" by setting an '+
      'character-set alias. Relevant only if the default characterset of your '+
       'database is "NONE" and you have fields without an explicit collation. '+
       'If not set we''ll use the attachment characterset, and this might be wrong';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_InsertReturningFields : TZProperty = (
    Name: DSProps_InsertReturningFields;
    Purpose: 'Field1[, Field2, ...] (allowed separators: "," or ";") '+
      'It''s a list of fields which will get their values on INSERT '+
      '(by INSERT...RETURNING/OUTPUT). Results are set by IZCachedResolver to '+
      'the inserted row '+LineEnding+'Yet implemented for Firebird only.';
    ValueType: pvtString; LevelTypes: [pltResolver];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  //database parameter block properties:
  ZProp_isc_dpb_page_size : TZProperty = (
    Name: ConnProps_isc_dpb_page_size;
    Purpose: 'This is for setting the page size of a newly-created DB.'+
      'The allowed values are 1024, 2048, 4096, 8192, 16384.';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: '1024|2048|4096|8192|16384'; Default: '4096'; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_num_buffers : TZProperty = (
    Name: ConnProps_isc_dpb_num_buffers;
    Purpose: 'Environmental control. Number of cache buffers';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: '1024|2048|4096|8192|16384'; Default: '4096'; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_debug : TZProperty = (
    Name: ConnProps_isc_dpb_debug;
    Purpose: 'Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_garbage_collect : TZProperty = (
    Name: ConnProps_isc_dpb_garbage_collect;
    Purpose: 'Refer Firebird/Interbase manual.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_verify : TZProperty = (
    Name: ConnProps_isc_dpb_verify;
    Purpose: 'System management. Perform consistency checking of internal '+
      'structures; See ZPlainFirebirdInterbaseDriver.pas, section '+
      '"isc_dpb_verify specific flags"';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_sweep : TZProperty = (
    Name: ConnProps_isc_dpb_sweep;
    Purpose: 'sweep the database upon attachment';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_enable_journal : TZProperty = (
    Name: ConnProps_isc_dpb_enable_journal;
    Purpose: 'Refer Firebird/Interbase manual.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_disable_journal : TZProperty = (
    Name: ConnProps_isc_dpb_disable_journal;
    Purpose: 'Refer Firebird/Interbase manual.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_dbkey_scope : TZProperty = (
    Name: ConnProps_isc_dpb_dbkey_scope;
    Purpose: 'Environmental control. Scope of dbkey context. 0 limits scope '+
      'to the current transaction, 1 extends scope to the database session. ';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: '0|1'; Default: '0'; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_trace : TZProperty = (
    Name: ConnProps_isc_dpb_trace;
    Purpose: 'Refer Firebird/Interbase manual. Produce call trace?';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_no_garbage_collect : TZProperty = (
    Name: ConnProps_isc_dpb_no_garbage_collect;
    Purpose: 'disable sweeping/Turns off garbage collection by this attachment. '+
      'Used primarily by -g switch of gbak';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_damaged : TZProperty = (
    Name: ConnProps_isc_dpb_damaged;
    Purpose: 'Environmental control. Specifies whether or not the database '+
      'should be marked as damaged. 1 = mark as damaged 0 = do not mark as damaged';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: '0|1'; Default: '0'; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_license : TZProperty = (
    Name: ConnProps_isc_dpb_license;
    Purpose: 'User validation parameter. Authorization key for a software license.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_sys_user_name : TZProperty = (
    Name: ConnProps_isc_dpb_sys_user_name;
    Purpose: 'User validation parameter. System database administrator’s user '+
      'name, up to 255 characters.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_encrypt_key : TZProperty = (
    Name: ConnProps_isc_dpb_encrypt_key;
    Purpose: 'User validation parameter. String encryption key, up to 255 '+
      'characters';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_activate_shadow : TZProperty = (
    Name: ConnProps_isc_dpb_activate_shadow;
    Purpose: 'Shadow control. Activates the database shadow, an optional, '+
      'duplicate, in-sync copy of the database.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_sweep_interval : TZProperty = (
    Name: ConnProps_isc_dpb_sweep_interval;
    Purpose: 'Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: '20000'; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_delete_shadow : TZProperty = (
    Name: ConnProps_isc_dpb_delete_shadow;
    Purpose: 'Shadow control. Delete the database shadow.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_force_write : TZProperty = (
    Name: ConnProps_isc_dpb_force_write;
    Purpose: 'Specifies whether database writes are synchronous or asynchronous. '+
      '0 = asynchronous; 1 = synchronous';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: '0|1'; Default: '0'; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_begin_log : TZProperty = (
    Name: ConnProps_isc_dpb_begin_log;
    Purpose: 'Replay logging system control. Activate a replay logging system '+
      'to keep track of all database calls';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_quit_log : TZProperty = (
    Name: ConnProps_isc_dpb_quit_log;
    Purpose: 'Replay logging system control. Deactivate the replay logging '+
      'system';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_no_reserve : TZProperty = (
    Name: ConnProps_isc_dpb_no_reserve;
    Purpose: 'Specifies whether or not a small amount of space on each database '+
      'page is reserved for holding backup versions of records when '+
      'modifications are made; keep backup versions on the same page as the '+
      'primary record to optimize update activity'+LineEnding+
      '0 (default) = reserve space'+LineEnding+
      '1= do not reserve space';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: '0|1'; Default: '0'; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_user_name : TZProperty = (
    Name: ConnProps_isc_dpb_user_name;
    Purpose: 'String user name, up to 255 characters';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_UID+','+ConnProps_Username;
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_password : TZProperty = (
    Name: ConnProps_isc_dpb_password;
    Purpose: 'String password, up to 255 characters';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_PWD+','+ConnProps_Password;
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_password_enc : TZProperty = (
    Name: ConnProps_isc_dpb_password_enc;
    Purpose: 'String encrypted password, up to 255 characters';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_sys_user_name_enc : TZProperty = (
    Name: ConnProps_isc_dpb_sys_user_name_enc;
    Purpose: 'String sysdba''s encrypted username, up to 255 characters';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_interp : TZProperty = (
    Name: ConnProps_isc_dpb_interp;
    Purpose: 'Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_online_dump : TZProperty = (
    Name: ConnProps_isc_dpb_online_dump;
    Purpose: 'Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_old_file_size : TZProperty = (
    Name: ConnProps_isc_dpb_old_file_size;
    Purpose: '(Removed) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_old_num_files : TZProperty = (
    Name: ConnProps_isc_dpb_old_num_files;
    Purpose: '(Removed) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_old_file : TZProperty = (
    Name: ConnProps_isc_dpb_old_file;
    Purpose: '(Removed) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_old_start_page : TZProperty = (
    Name: ConnProps_isc_dpb_old_start_page;
    Purpose: '(Removed) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_old_start_seqno : TZProperty = (
    Name: ConnProps_isc_dpb_old_start_seqno;
    Purpose: '(Removed) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_old_start_file : TZProperty = (
    Name: ConnProps_isc_dpb_old_start_file;
    Purpose: '(Removed) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_drop_walfile : TZProperty = (
    Name: ConnProps_isc_dpb_drop_walfile;
    Purpose: '(Removed) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_old_dump_id : TZProperty = (
    Name: ConnProps_isc_dpb_old_dump_id;
    Purpose: '(Removed) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_wal_backup_dir : TZProperty = (
    Name: ConnProps_isc_dpb_wal_backup_dir;
    Purpose: '(Removed) Refer Firebird/Interbase manual.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_wal_chkptlen : TZProperty = (
    Name: ConnProps_isc_dpb_wal_chkptlen;
    Purpose: '(Removed) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_wal_numbufs : TZProperty = (
    Name: ConnProps_isc_dpb_wal_numbufs;
    Purpose: '(Removed) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_wal_bufsize : TZProperty = (
    Name: ConnProps_isc_dpb_wal_bufsize;
    Purpose: '(Removed) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_wal_grp_cmt_wait : TZProperty = (
    Name: ConnProps_isc_dpb_wal_grp_cmt_wait;
    Purpose: '(Removed) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_lc_messages : TZProperty = (
    Name: ConnProps_isc_dpb_lc_messages;
    Purpose: 'String specifying a language-specific message file';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_lc_ctype : TZProperty = (
    Name: ConnProps_isc_dpb_lc_ctype;
    Purpose: 'String specifying the character set to be utilized';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_CodePage;
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_shutdown : TZProperty = (
    Name: ConnProps_isc_dpb_shutdown;
    Purpose: '(Removed since FB2.1) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_online : TZProperty = (
    Name: ConnProps_isc_dpb_online;
    Purpose: '(Removed since FB2.1) Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_shutdown_delay : TZProperty = (
    Name: ConnProps_isc_dpb_shutdown_delay;
    Purpose: 'Refer Firebird/Interbase manual.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_reserved : TZProperty = (
    Name: ConnProps_isc_dpb_reserved;
    Purpose: 'sets the database to single user if super user. Refer Firebird/Interbase manual.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_overwrite : TZProperty = (
    Name: ConnProps_isc_dpb_overwrite;
    Purpose: 'On create, allow overwriting existing file. Refer Firebird/Interbase manual.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_sec_attach : TZProperty = (
    Name: ConnProps_isc_dpb_sec_attach;
    Purpose: 'Special attach for security database (obsolete). Refer Firebird/Interbase manual.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  //ConnProps_isc_dpb_disable_wal
  ZProp_isc_dpb_connect_timeout : TZProperty = (
    Name: ConnProps_isc_dpb_connect_timeout;
    Purpose: 'Terminate connection if no traffic for n seconds';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: '180'; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_dummy_packet_interval : TZProperty = (
    Name: ConnProps_isc_dpb_dummy_packet_interval;
    Purpose: 'Interval in seconds between keep-alive packets';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: '60'; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_gbak_attach : TZProperty = (
    Name: ConnProps_isc_dpb_gbak_attach;
    Purpose: 'Special rights for gbak';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_sql_role_name : TZProperty = (
    Name: ConnProps_isc_dpb_sql_role_name;
    Purpose: 'The role name';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Rolename;
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_set_page_buffers : TZProperty = (
    Name: ConnProps_isc_dpb_set_page_buffers;
    Purpose: 'Set buffer count in database header';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_working_directory : TZProperty = (
    Name: ConnProps_isc_dpb_working_directory;
    Purpose: 'Set working directory for this session (for classic we suppose, deprecated?)';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_sql_dialect : TZProperty = (
    Name: ConnProps_isc_dpb_sql_dialect;
    Purpose: 'Dialect to specify on create new database/connect to database';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: '1|2|3'; Default: '3'; Alias: ConnProps_Dialect;
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_set_db_readonly : TZProperty = (
    Name: ConnProps_isc_dpb_set_db_readonly;
    Purpose: 'Set database as read only in database header';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_set_db_sql_dialect : TZProperty = (
    Name: ConnProps_isc_dpb_set_db_sql_dialect;
    Purpose: 'Number indicating the SQL dialect used by the client; used by '+
      'the gfix utility to set the dialect in the database header page Dialect '+
      '3 gives access to features introduced in InterBase XE. '+
      'This option is omited since FireBird 2.0.5 onwards';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: '1|2|3'; Default: '3'; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_gfix_attach : TZProperty = (
    Name: ConnProps_isc_dpb_gfix_attach;
    Purpose: 'Special rights for gfix. This option is omited since FireBird '+
      '2.0.5 onwards';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_gstat_attach : TZProperty = (
    Name: ConnProps_isc_dpb_gstat_attach;
    Purpose: 'Special rights for gstat. This option is omited since FireBird '+
      '2.0.5 onwards';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_set_db_charset : TZProperty = (
    Name: ConnProps_isc_dpb_set_db_charset;
    Purpose: 'Set default db charset in database header.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_gsec_attach : TZProperty = (
    Name: ConnProps_isc_dpb_gsec_attach;
    Purpose: 'Special rights for gfix.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_address_path : TZProperty = (
    Name: ConnProps_isc_dpb_address_path;
    Purpose: 'Protocol dependent network address of the remote client including '+
      'intermediate hosts in the case of redirection.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_process_id : TZProperty = (
    Name: ConnProps_isc_dpb_process_id;
    Purpose: 'The PID value (shown in MON$ATTACHMENTS.MON$REMOTE_PID) using '+
      'isc_dpb_process_id';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_no_db_triggers : TZProperty = (
    Name: ConnProps_isc_dpb_no_db_triggers;
    Purpose: 'Disable database (eg. ON CONNECT) triggers for this session. This '+
      'option is omited since FireBird 2.0.5 onwards';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_trusted_auth : TZProperty = (
    Name: ConnProps_isc_dpb_trusted_auth;
    Purpose: 'Force trusted authentication; fb 2.1+. See: '+LineEnding+
      'https://firebirdsql.org/rlsnotesh/rnfb210-wintrusted.html';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_process_name : TZProperty = (
    Name: ConnProps_isc_dpb_process_name;
    Purpose: 'Set a process name.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_trusted_role : TZProperty = (
    Name: ConnProps_isc_dpb_trusted_role;
    Purpose: 'Name of the trusted role for this session.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_org_filename : TZProperty = (
    Name: ConnProps_isc_dpb_org_filename;
    Purpose: 'Database name passed by the client (possibly an alias)';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_utf8_filename : TZProperty = (
    Name: ConnProps_isc_dpb_utf8_filename;
    Purpose: 'Inform Firebird that dbname is in UTF8 as opposed to OS charset';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_ext_call_depth : TZProperty = (
    Name: ConnProps_isc_dpb_ext_call_depth;
    Purpose: 'Set level of nested external database connections.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_auth_block : TZProperty = (
    Name: ConnProps_isc_dpb_auth_block;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_client_version : TZProperty = (
    Name: ConnProps_isc_dpb_client_version;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_remote_protocol : TZProperty = (
    Name: ConnProps_isc_dpb_remote_protocol;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_host_name : TZProperty = (
    Name: ConnProps_isc_dpb_host_name;
    Purpose: 'The host name.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_os_user : TZProperty = (
    Name: ConnProps_isc_dpb_os_user;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_specific_auth_data : TZProperty = (
    Name: ConnProps_isc_dpb_specific_auth_data;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_auth_plugin_list : TZProperty = (
    Name: ConnProps_isc_dpb_auth_plugin_list;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_auth_plugin_name : TZProperty = (
    Name: ConnProps_isc_dpb_auth_plugin_name;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_config : TZProperty = (
    Name: ConnProps_isc_dpb_config;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_nolinger : TZProperty = (
    Name: ConnProps_isc_dpb_nolinger;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_reset_icu : TZProperty = (
    Name: ConnProps_isc_dpb_reset_icu;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_map_attach : TZProperty = (
    Name: ConnProps_isc_dpb_map_attach;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_session_time_zone : TZProperty = (
    Name: ConnProps_isc_dpb_session_time_zone;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_set_db_replica : TZProperty = (
    Name: ConnProps_isc_dpb_set_db_replica;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_set_bind : TZProperty = (
    Name: ConnProps_isc_dpb_set_bind;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_decfloat_round : TZProperty = (
    Name: ConnProps_isc_dpb_decfloat_round;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_dpb_decfloat_traps : TZProperty = (
    Name: ConnProps_isc_dpb_decfloat_traps;
    Purpose: 'Refer Firebird manuals.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_HardCommit: TZProperty = (
    Name: ConnProps_HardCommit;
    Purpose: 'Don''t use isc_commit_retaining or isc_rollback_retaining call.'+
      'If enabled all record-streams of the txn are closed immediatelly.'+LineEnding+
      'Note since 7.2.6 ZeosLib uses short-transactions but keeps the retaining '+
      'design by default. If a retained commit/rollback is done the transaction '+
      'is removed from the transaction-manager, and is alive until: no more row '+
      'of a opened IZResultset can be fetched and if there are no more Lob''s '+
      'to read. ZeosLib automatically will try to perform a fetchall and loads '+
      'all data, if possible. You can use cached lob''s to guarantiee all '+
      'lob''s can be read. Then the transaction will end up with a committed '+
      'or rollback as requested. However a new request will create a new '+
      'transaction.';
    ValueType: pvtEnum; LevelTypes: [pltConnection, pltTransaction];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  //transaction parameter block properties:
  ZProp_isc_tpb_consistency: TZProperty = (
    Name: TxnProps_isc_tpb_consistency;
    Purpose: 'Table-locking transaction model'+LineEnding+
      'This parameter is used for tiReadCommitted and tiSeriaizable. '+
      'You manually can use this param only if isolation level tiNone is specified.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_concurrency: TZProperty = (
    Name: TxnProps_isc_tpb_concurrency;
    Purpose:  'High throughput, high concurrency transaction with acceptable '+
      'consistency; use of this parameter takes full advantage of the InterBase or Firebird '+
      'multi-generational transaction model [Default]'+LineEnding+
      'This parameter is used for isolationlevel tiRepeatableRead and default for tiNone.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_shared: TZProperty = (
    Name: TXnProps_isc_tpb_shared;
    Purpose: 'Concurrent, shared access of a specified table among all transactions; use '+
      'in conjunction with isc_tpb_lock_read and isc_tpb_lock_write to '+
      'establish the lock option [Default]';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_protected: TZProperty = (
    Name: TxnProps_isc_tpb_protected;
    Purpose: 'Concurrent, restricted access of a specified table; use in conjunction with '+
      'isc_tpb_lock_read and isc_tpb_lock_write to establish the lock option';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_exclusive: TZProperty = (
    Name: TxnProps_isc_tpb_exclusive;
    Purpose: 'Used to specify exclusive table access when calling '+
      'isc_start_transaction() at the API level.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_wait: TZProperty = (
    Name: TxnProps_isc_tpb_wait;
    Purpose: 'Lock resolution specifies that the transaction is to wait until locked '+
      'resources are released before retrying an operation [Default]'+LineEnding+
      'This parameter is default using isolation level tiNone.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_nowait: TZProperty = (
    Name: TxnProps_isc_tpb_nowait;
    Purpose: 'Lock resolution specifies that the transaction is not to wait for '+
      'locks to be released, but instead, a lock conflict error should be '+
      'returned immediately.'+LineEnding+
      'This parameter is used for isolationlevel tiReadCommitted and tiRepeatableRead.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_read: TZProperty = (
    Name: TxnProps_isc_tpb_read;
    Purpose: 'Read-only access mode that allows a transaction only to select '+
      'data from tables'+LineEnding+
      'This parameter is set if your transaction is ReadOnly';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_write: TZProperty = (
    Name: TxnProps_isc_tpb_write;
    Purpose: 'Read-write access mode of that allows a transaction to select, insert, '+
      'update, and delete table data [Default]'+LineEnding+
      'This parameter is set if your transaction is not ReadOnly';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_lock_read: TZProperty = (
    Name: TxnProps_isc_tpb_lock_read;
    Purpose: 'Read-only access of a specified table. Use in conjunction with '+
      'isc_tpb_shared, isc_tpb_protected, and isc_tpb_exclusive to establish the '+
      'lock option.';
    ValueType: pvtString; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_lock_write: TZProperty = (
    Name: TxnProps_isc_tpb_lock_write;
    Purpose: 'Read-write access of a specified table. Use in conjunction with '+LineEnding+
      'isc_tpb_shared, isc_tpb_protected, and isc_tpb_exclusive to establish the '+LineEnding+
      'lock option [Default]';
    ValueType: pvtString; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  cFirebird2upProvider: TZPropertyProvider = (
    Provider: spIB_FB; MinimumServerVersion: 2000000;
    MinimumClientVersion: 2000000; MinimumProtocolVersion: 0;);
  ZProp_isc_ignore_limbo: TZProperty = (
    Name: TxnProps_isc_tpb_ignore_limbo;
    Purpose: 'With this option, records created by limbo transactions are '+
      'ignored. Transactions are in limbo if the second stage of a two-phase '+
      'commit fails.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cFirebird2upProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_read_committed: TZProperty = (
    Name: TxnProps_isc_tpb_read_committed;
    Purpose: 'With this option, records created by limbo transactions are '+
      'ignored. Transactions are in limbo if the second stage of a two-phase '+
      'commit fails.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_autocommit: TZProperty = (
    Name: TxnProps_isc_tpb_autocommit;
    Purpose: 'This parameter is set if your transaction is set to AutoCommit.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_rec_version: TZProperty = (
    Name: TxnProps_isc_tpb_rec_version;
    Purpose: 'Enables an isc_tpb_read_committed transaction to read the most '+
      'recently committed version of a record even if other, uncommitted '+
      'versions are pending.'+LineEnding+
      'This parameter is set if your isolation level is tiReadCommitted.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_no_rec_version: TZProperty = (
    Name: TxnProps_isc_tpb_no_rec_version;
    Purpose: 'Enables an isc_tpb_read_committed transaction to read only the '+
      'latest committed version of a record. If an uncommitted version of a '+
      'record is pending and isc_tpb_wait is also specified, then the '+
      'transaction waits for the pending record to be committed or rolled back '+
      'before proceeding.'+LineEnding+
      'Otherwise, a lock conflict error is reported at once.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_restart_requests: TZProperty = (
    Name: TxnProps_isc_tpb_restart_requests;
    Purpose: '<undocumented>';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFirebirdProtocols);
  );
  ZProp_isc_tpb_no_auto_undo: TZProperty = (
    Name: TxnProps_isc_tpb_no_auto_undo;
    Purpose: 'With NO AUTO UNDO, the transaction refrains from keeping the log '+
      'that is normally used to undo changes in the event of a rollback. '+
      'Should the transaction be rolled back after all, other transactions '+
      'will pick up the garbage (eventually). This option can be useful for '+
      'massive insertions that don''t need to be rolled back. For transactions '+
      'that don''t perform any mutations, NO AUTO UNDO makes no difference at all';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  cInterbase7_5upProvider: TZPropertyProvider = (
    Provider: spIB_FB; MinimumServerVersion: 7005000;
    MinimumClientVersion: 0; MinimumProtocolVersion: 0;);
  ZProp_isc_no_savepoint: TZProperty = (
    Name: TxnProps_isc_tpb_no_savepoint;
    Purpose: 'With NO AUTO UNDO, the transaction refrains from keeping the log '+
      'that is normally used to undo changes in the event of a rollback. '+
      'Should the transaction be rolled back after all, other transactions '+
      'will pick up the garbage (eventually). This option can be useful for '+
      'massive insertions that don''t need to be rolled back. For transactions '+
      'that don''t perform any mutations, NO AUTO UNDO makes no difference at all';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbase7_5upProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols[1]);
  );
  ZProp_isc_tpb_lock_timeout: TZProperty = (
    Name: TxnProps_isc_tpb_lock_timeout;
    Purpose: 'With NO AUTO UNDO, the transaction refrains from keeping the log '+
      'that is normally used to undo changes in the event of a rollback. '+
      'Should the transaction be rolled back after all, other transactions '+
      'will pick up the garbage (eventually). This option can be useful for '+
      'massive insertions that don''t need to be rolled back. For transactions '+
      'that don''t perform any mutations, NO AUTO UNDO makes no difference at all';
    ValueType: pvtNumber; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cFirebird2upProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
  cFirebird4upProvider: TZPropertyProvider = (
    Provider: spIB_FB; MinimumServerVersion: 4000000;
    MinimumClientVersion: 0; MinimumProtocolVersion: 0;);
  ZProp_isc_tpb_read_consistency: TZProperty = (
    Name: TxnProps_isc_tpb_read_consistency;
    Purpose: 'Firebird 4.0 release notes pages 26 ("Read Consistency for '+
      'Statements in Read-Committed Transactions") and 28 ("New API Constant '+
      'in the TPB")'+LineEnding+
      'This new isolation level should be the default isolation level for read '+
      'committed transactions on Firebird 4.0';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cFirebird4upProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols);
  );
{$IFEND}

{$IFDEF ENABLE_FIREBIRD}
  ZProp_SessionIdleTimeOut: TZProperty = (
    Name: ConnProps_SessionIdleTimeOut;
    Purpose: 'Set the session idle timeout in milliseconds. '+
      'Minimum value is 16. Supported since Firebird 4.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cFirebird4upProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols[0]);
  );
  ZProp_FirebirdAPI: TZProperty = (
    Name: ConnProps_FirebirdAPI;
    Purpose: 'defines the Firebird API which is used used for. The default for '+
      'firebird 3+ is object API. If "legacy" is set the old firebird lagacy '+
      'API is used. If the library version is lower then 3.0 the parameter is '+
      'ignored and the legacy API is always used.';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'legacy|interface'; Default: 'interface'; Alias: '';
    Providers: (Count: 1; Items: @cFirebird3upProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFirebirdProtocols[0]);
  );
{$ENDIF ENABLE_FIREBIRD}

{$IFDEF ENABLE_SQLITE}
  cSqlite3upProvider: TZPropertyProvider = (
    Provider: spSQLite; MinimumServerVersion: 0;
    MinimumClientVersion: 0; MinimumProtocolVersion: 0;);
  cSQLiteProtocol: String = 'sqlite';

  ZProp_Encrypted: TZProperty = (
    Name: ConnProps_Encrypted;
    Purpose: 'Use connection encryption?';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cSqlite3upProvider);
    Protocols: (Count: 1; Items: @cSQLiteProtocol);
  );
  ZProp_BusyTimeout: TZProperty = (
    Name: ConnProps_BusyTimeout;
    Purpose: 'calls sqlite3_busy_timeout. This routine sets a busy handler '+
      'that sleeps for a specified amount of time when a table is locked. The '+
      'handler will sleep multiple times until at least "ms" milliseconds of '+
      'sleeping have accumulated. After at least "ms" milliseconds of sleeping, '+
      'the handler returns 0 which causes sqlite3_step() to return SQLITE_BUSY.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: '0'; Alias: '';
    Providers: (Count: 1; Items: @cSqlite3upProvider);
    Protocols: (Count: 1; Items: @cSQLiteProtocol);
  );
  ZProp_CacheSize: TZProperty = (
    Name: ConnProps_CacheSize;
    Purpose: 'calls PRAGMA cache_size = value. See:'+LineEnding+
      'https://www.sqlite.org/pragma.html#pragma_cache_size';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: '0'; Alias: '';
    Providers: (Count: 1; Items: @cSqlite3upProvider);
    Protocols: (Count: 1; Items: @cSQLiteProtocol);
  );
  ZProp_Synchronous: TZProperty = (
    Name: ConnProps_Synchronous;
    Purpose: 'calls PRAGMA synchronous = value. See:'+LineEnding+
      'https://www.sqlite.org/pragma.html#pragma_synchronous'+LineEnding+
      '"OFF" brings the best performance';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: '0|OFF|1|NORMAL|2|FULL|3|EXTRA'; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cSqlite3upProvider);
    Protocols: (Count: 1; Items: @cSQLiteProtocol);
  );
  ZProp_LockingMode: TZProperty = (
    Name: ConnProps_LockingMode;
    Purpose: 'calls PRAGMA locking_mode = value. See:'+LineEnding+
      'https://www.sqlite.org/pragma.html#pragma_locking_mode'+LineEnding+
      '"EXCLUSIVE" brings the best performance';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'NORMAL|EXCLUSIVE'; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cSqlite3upProvider);
    Protocols: (Count: 1; Items: @cSQLiteProtocol);
  );
  ZProp_ForeignKeys: TZProperty = (
    Name: ConnProps_ForeignKeys;
    Purpose: 'calls PRAGMA foreign_keys = value. See:'+LineEnding+
      'https://www.sqlite.org/pragma.html#pragma_foreign_keys';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolTrue; Alias: '';
    Providers: (Count: 1; Items: @cSqlite3upProvider);
    Protocols: (Count: 1; Items: @cSQLiteProtocol);
  );
  ZProp_journal_mode: TZProperty = (
    Name: ConnProps_journal_mode;
    Purpose: 'calls PRAGMA journal_mode = value. See:'+LineEnding+
      'https://www.sqlite.org/pragma.html#pragma_journal_mode';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'DELETE|TRUNCATE|PERSIST|MEMORY|WAL|OFF'; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cSqlite3upProvider);
    Protocols: (Count: 1; Items: @cSQLiteProtocol);
  );
  ZProp_BindDoubleDateTimeValues: TZProperty = (
    Name: DSProps_BindDoubleDateTimeValues;
    Purpose: 'If set, directly bind the double value of date/time/datetime '+
      'fields. Otherwise, use intermediate string';
    ValueType: pvtEnum; LevelTypes: [pltConnection,pltStatement];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cSqlite3upProvider);
    Protocols: (Count: 1; Items: @cSQLiteProtocol);
  );
  ZProp_BindOrdinalBoolValues: TZProperty = (
    Name: DSProps_BindOrdinalBoolValues;
    Purpose: 'If set, directly bind the ordinal of boolean fields. Otherwise, '+
      'use intermediate alltime ''Y''/''N'' string.';
    ValueType: pvtEnum; LevelTypes: [pltConnection,pltStatement];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cSqlite3upProvider);
    Protocols: (Count: 1; Items: @cSQLiteProtocol);
  );
  ZProp_SQLiteTransactionBehaviour: TZProperty = (
    Name: TxnProps_TransactionBehaviour;
    Purpose: 'Sets the transaction behavior on starting a transaction.'+
      'See Section 2.2 of https://www.sqlite.org/lang_transaction.html';
    ValueType: pvtEnum; LevelTypes: [pltConnection,pltTransaction];
    Values: 'DEFERRED|IMMEDIATE|EXCLUSIVE'; Default: 'DEFERRED'; Alias: '';
    Providers: (Count: 1; Items: @cSqlite3upProvider);
    Protocols: (Count: 1; Items: @cSQLiteProtocol);
  );
  ZProp_SQLiteIntAffinity : TZProperty = (
    Name: DSProps_SQLiteIntAffinity;
    Purpose: 'Treat "INT" fields in any kind as Int64, means ignore all subtypes '+
      'like smallint';
    ValueType: pvtEnum; LevelTypes: [pltConnection, pltStatement];
    Values: cBoolEnum; Default: cBoolFalse; Alias: '';
    Providers: (Count: 1; Items: @cSqlite3upProvider);
    Protocols: (Count: 1; Items: @cSQLiteProtocol);
  );
  ZProp_SQLiteOpen_Flags : TZProperty = (
    Name: ConnProps_SQLiteOpen_Flags;
    Purpose: 'Support sqlite_open_v2'+LineEnding+
      'see: https://www.sqlite.org/c3ref/open.html'+LineEnding+
      'all values are defined in ZPlainSqLiteDriver.pas'+LineEnding+
      'i.e. thread-safety';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cSqlite3upProvider);
    Protocols: (Count: 1; Items: @cSQLiteProtocol);
  );
  ZProp_SQLiteOpen_zVfs : TZProperty = (
    Name: ConnProps_SQLiteOpen_zVfs;
    Purpose: 'Support sqlite_open_v2'+LineEnding+
      'see: https://www.sqlite.org/c3ref/open.html';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cSqlite3upProvider);
    Protocols: (Count: 1; Items: @cSQLiteProtocol);
  );
{$ENDIF}

{$IFDEF ENABLE_ORACLE}
  cOracleProvider: TZPropertyProvider = (
    Provider: spOracle; MinimumServerVersion: 0;
    MinimumClientVersion: 0; MinimumProtocolVersion: 0;);
  cOracleProtocol: String = 'oracle';
  ZProp_ServerCachedStmts : TZProperty = (
    Name: ConnProps_ServerCachedStmts;
    Purpose: 'If enabled or not specified, sets StatementMode to OCI_STMT_CACHE (refer to Oracle manual for details)';
    ValueType: pvtEnum; LevelTypes: [pltConnection, pltStatement];
    Values: cBoolEnum; Default: cBoolTrue; Alias: '';
    Providers: (Count: 1; Items: @cOracleProvider);
    Protocols: (Count: 1; Items: @cOracleProtocol);
  );
  ZProp_BlobPrefetchSize : TZProperty = (
    Name: ConnProps_BlobPrefetchSize;
    Purpose: 'Sets value for OCI_ATTR_DEFAULT_LOBPREFETCH_SIZE option, refer to Oracle manual for details';
    ValueType: pvtNumber; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cOracleProvider);
    Protocols: (Count: 1; Items: @cOracleProtocol);
  );
  ZProp_StatementCache : TZProperty = (
    Name: ConnProps_StatementCache;
    Purpose: 'Sets value for OCI_ATTR_STMTCACHESIZE option, refer to Oracle manual for details';
    ValueType: pvtNumber; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cOracleProvider);
    Protocols: (Count: 1; Items: @cOracleProtocol);
  );
  ZProp_row_prefetch_size : TZProperty = (
    Name: DSProps_RowPrefetchSize;
    Purpose: 'Sets value for OCI_ATTR_PREFETCH_MEMORY option, refer to Oracle manual for details';
    ValueType: pvtNumber; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cOracleProvider);
    Protocols: (Count: 1; Items: @cOracleProtocol);
  );
  ZProp_OCIAuthenticateMode : TZProperty = (
    Name: ConnProps_OCIAuthenticateMode;
    Purpose: 'Specifies the various modes of operation. '+ LineEnding+
      'The constants are defined in ZPlainOracleDriver.pas'+LineEnding+
      'Valid modes are: '+LineEnding+
      'OCI_DEFAULT - in this mode, the user session context returned may only ever be set with the same server context specified in svchp. For encoding, the server handle uses the setting in the environment handle.'+LineEnding+
      'OCI_MIGRATE - in this mode, the new user session context may be set in a '+
      'service handle with a different server handle. This mode establishes the user session context. '+
      'To create a migratable session, the service handle must already be set '+
      'with a non-migratable user session, which becomes the "creator" session '+
      'of the migratable session. That is, a migratable session must have a non-migratable parent session.'+LineEnding+
      'OCI_SYSDBA - in this mode, the user is authenticated for SYSDBA access.'+LineEnding+
      'OCI_SYSOPER - in this mode, the user is authenticated for SYSOPER access.'+LineEnding+
      'OCI_PRELIM_AUTH - this mode may only be used with OCI_SYSDBA or OCI_SYSOPER to authenticate for certain administration tasks.';
    ValueType: pvtNumber; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: '0'; Alias: '';
    Providers: (Count: 1; Items: @cOracleProvider);
    Protocols: (Count: 1; Items: @cOracleProtocol);
  );
  ZProp_MultiThreaded : TZProperty = (
    Name: ConnProps_OCIMultiThreaded;
    Purpose: 'If enabled, OCI_THREADED will also be used for initializing the connection environment (refer to Oracle manual for details)';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolTrue; Alias: '';
    Providers: (Count: 1; Items: @cOracleProvider);
    Protocols: (Count: 1; Items: @cOracleProtocol);
  );
{$ENDIF}

{$IFDEF ENABLE_ASA}
  cASAProvider: TZPropertyProvider = (
    Provider: spASA; MinimumServerVersion: 0;
    MinimumClientVersion: 0; MinimumProtocolVersion: 0;);
  cASAProtocols: array[0..1] of String = ('asa','asa_capi');
  cASA_AppInfoPurpose = 'Assists administrators in identifying the origin of particular '+
             'client connections from a database server. Clients can specify '+
             'their own string that is appended to the generated string. '+
             'The AppInfo property string is a sequence of semicolon-delimited '+
             'key=value pairs. Refer to ASA manual for types and acceptable '+
             'values of these parameter.';
  ZProp_AppInfo : TZProperty = (
    Name: ConnProps_AppInfo;
    Purpose: cASA_AppInfoPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_APP;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_APP : TZProperty = (
    Name: ConnProps_APP;
    Purpose: cASA_AppInfoPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_AppInfo;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_AutoStartPurpose =
    'Controls whether a local database server is started if no server can be '+
    'connected to. Refer to ASA manual';
  ZProp_AutoStart : TZProperty = (
    Name: ConnProps_AutoStart;
    Purpose: cASA_AutoStartPurpose;
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolTrue; Alias: ConnProps_ASTART;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_ASTART : TZProperty = (
    Name: ConnProps_ASTART;
    Purpose: cASA_AutoStartPurpose;
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cBoolEnum; Default: cBoolTrue; Alias: ConnProps_AutoStart;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_CharSetPurpose =
    'Specifies the character set to be used on this connection. '+
    'Syntax: { CharSet | CS }={ NONE | character-set }'+LineEnding+
    'NONE   Specifying CharSet=NONE requests that the connection use the '+
    'database CHAR character set.';
  ZProp_CharSet : TZProperty = (
    Name: ConnProps_CharSet;
    Purpose: cASA_CharSetPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_CS+','+ConnProps_CodePage;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_CS : TZProperty = (
    Name: ConnProps_CS;
    Purpose: cASA_CharSetPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_CharSet+','+ConnProps_CodePage;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_CommBufferSizePurpose =
    'Sets the maximum size of communication packets. '+LineEnding+
    '{ CommBufferSize | CBSIZE }=size[ k ]'+LineEnding+
    ' This integer specifies the maximum size of communication packets. The '+
    'default value is in bytes, but you can use k to specify units of '+
    'kilobytes. The minimum value of CommBufferSize is 500 bytes, and the '+
    'maximum is 16000 bytes.. If no CommBufferSize value is set, the '+
    'CommBufferSize is controlled by the setting on the server, which defaults '+
    'to 7300 bytes on all operating systems except Windows Mobile. On '+
    'Windows Mobile, the default is 1460 bytes.';
  ZProp_CommBufferSize : TZProperty = (
    Name: ConnProps_CommBufferSize;
    Purpose: cASA_CommBufferSizePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: '7300'; Alias: ConnProps_CBSIZE;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_CBSIZE : TZProperty = (
    Name: ConnProps_CBSIZE;
    Purpose: cASA_CommBufferSizePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_CommBufferSize;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_CommLinksPurpose =
    'Specifies client-side network protocol options.'+LineEnding+
    'Syntax: { CommLinks | LINKS }={ [ SharedMemory | ShMem ] | ALL | [ TCPIP | TCP ] } [, ... ] string'+LineEnding+
    'Refer Sybase manuals';
  ZProp_CommLinks : TZProperty = (
    Name: ConnProps_CommLinks;
    Purpose: cASA_CommLinksPurpose;
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'SharedMemory|ShMem|ALL|TCPIP|TCP'; Default: 'SharedMemory'; Alias: ConnProps_LINKS;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_LINKS : TZProperty = (
    Name: ConnProps_LINKS;
    Purpose: cASA_CommLinksPurpose;
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'SharedMemory|ShMem|ALL|TCPIP|TCP'; Default: 'SharedMemory'; Alias: ConnProps_CommLinks;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_CompressPurpose =
    '(ASA) Turns compression on or off for a connection.'+LineEnding+
    'Syntax: { Compress | COMP }={ YES | NO }'+LineEnding+
    'Anywhere except with TDS connections. TDS connections (including jConnect) '+
    'do not support SQL Anywhere communication compression.';
  ZProp_Compress : TZProperty = (
    Name: ConnProps_Compress;
    Purpose: cASA_CompressPurpose;
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cNo_Yes_Enum; Default: cNo; Alias: ConnProps_COMP;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_COMP : TZProperty = (
    Name: ConnProps_COMP;
    Purpose: cASA_CompressPurpose;
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cNo_Yes_Enum; Default: cNo; Alias: ConnProps_Compress;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_CompressionThresholdPurpose =
    'Increases or decreases the size limit at which packets are compressed.'+LineEnding+
    'Syntax: { CompressionThreshold | COMPTH }=size[ k ]';
  ZProp_CompressionThreshold : TZProperty = (
    Name: ConnProps_CompressionThreshold;
    Purpose: cASA_CompressionThresholdPurpose;
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: '120'; Alias: ConnProps_COMPTH;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_COMPTH : TZProperty = (
    Name: ConnProps_COMPTH;
    Purpose: cASA_CompressionThresholdPurpose;
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: '120'; Alias: ConnProps_CompressionThreshold;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_ConnectionNamePurpose =
    'Names a connection, to make switching to it easier in multi-connection applications.'+LineEnding+
    '{ ConnectionName | CON }=connection-name';
  ZProp_ConnectionName : TZProperty = (
    Name: ConnProps_ConnectionName;
    Purpose: cASA_ConnectionNamePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_CON;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_CON : TZProperty = (
    Name: ConnProps_CON;
    Purpose: cASA_ConnectionNamePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_ConnectionName;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_ConnectionPoolPurpose =
    'Controls the behavior of client connection pooling.'+LineEnding+
    'ConnectionPool={ NO | YES [ ( [ Timeout=timeout-sec; ] [ MaxCached=max-cached-conn ] ) ] }'+LineEnding+
    'All platforms except Windows Mobile and non-threaded Unix clients.'+LineEnding+
    'Connection pooling may improve the performance of applications that make '+
    'multiple, brief connections to the database server. When a connection is '+
    'disconnected it is automatically cached and may be reused when the '+
    'application reconnects. For a connection to be pooled, the connection '+
    'name can be different, but all other connection parameters must be identical.';
  ZProp_ConnectionPool : TZProperty = (
    Name: ConnProps_ConnectionPool;
    Purpose: cASA_ConnectionPoolPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: cNo_Yes_Enum; Default: cYes; Alias: ConnProps_CON;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_CPOOL : TZProperty = (
    Name: ConnProps_CPOOL;
    Purpose: cASA_ConnectionPoolPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: cNo_Yes_Enum; Default: cYes; Alias: ConnProps_ConnectionName;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_DatabaseFilePurpose =
    'Indicates which database file you want to load and connect to when starting a database that is not running.'+LineEnding+
    '{ DatabaseFile | DBF }=filename'+LineEnding+
    'Embedded databases.';
  ZProp_DatabaseFile : TZProperty = (
    Name: ConnProps_DatabaseFile;
    Purpose: cASA_DatabaseFilePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_DBF;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_DBF : TZProperty = (
    Name: ConnProps_DBF;
    Purpose: cASA_DatabaseFilePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_DatabaseFile;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_DatabaseKeyPurpose =
    'Starts an encrypted database with a connect request.'+LineEnding+
    '{ DatabaseKey | DBKEY }=key'+LineEnding+
    'The encryption key is a string, including mixed cases, numbers, letters, '+
    'and special characters. Database keys cannot include leading spaces, '+
    'trailing spaces, or semicolons.';
  ZProp_DatabaseKey : TZProperty = (
    Name: ConnProps_DatabaseKey;
    Purpose: cASA_DatabaseKeyPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_DBKEY;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_DBKEY : TZProperty = (
    Name: ConnProps_DBKEY;
    Purpose: cASA_DatabaseKeyPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_DatabaseKey;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_DatabaseNamePurpose =
    'Identifies a loaded database to which a connection needs to be made when '+
    'connecting to a database that is already running.'+LineEnding+
    '{ DatabaseName | DBN }=database-name'+LineEnding+
    'Running local databases or network database servers.';
  ZProp_DatabaseName : TZProperty = (
    Name: ConnProps_DatabaseName;
    Purpose: cASA_DatabaseNamePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_DBN;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_DBN : TZProperty = (
    Name: ConnProps_DBN;
    Purpose: cASA_DatabaseNamePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_DatabaseName;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_DatabaseSwitchesPurpose =
    'Provides database-specific options when starting a database.'+LineEnding+
    '{ DatabaseSwitches | DBS }=database-options'+LineEnding+
    'Connecting to a database server when the database is not started. This '+
    'connection parameter starts a database server automatically with the '+
    'specified database and options if a database server is not running.';
  ZProp_DatabaseSwitches : TZProperty = (
    Name: ConnProps_DatabaseSwitches;
    Purpose: cASA_DatabaseSwitchesPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_DBS;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_DBS : TZProperty = (
    Name: ConnProps_DBS;
    Purpose: cASA_DatabaseSwitchesPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_DatabaseSwitches;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_DatabaseSourceNamePurpose =
    'Tells the ODBC driver manager or embedded SQL library where to look in the '+
    'registry or the system information file (named .odbc.ini by default) to '+
    'find ODBC data source information. '+LineEnding+
    '{ DataSourceName | DSN }=data-source-name'+LineEnding+
    'This string specifies the name of the ODBC data source that contains '+
    'connection in formation for your database. ';
  ZProp_DatabaseSourceName : TZProperty = (
    Name: ConnProps_DataSourceName;
    Purpose: cASA_DatabaseSourceNamePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_DSN;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_DSN : TZProperty = (
    Name: ConnProps_DSN;
    Purpose: cASA_DatabaseSourceNamePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_DataSourceName;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_DisableMultiRowFetchPurpose =
    'Turns off multi-row fetches across the network.'+LineEnding+
    '{ DisableMultiRowFetch | DMRF }={ YES | NO }'+LineEnding+
    'By default, when the database server gets a simple fetch request, the '+
    'application asks for extra rows. You can disable this behavior by setting '+
    'this parameter to YES.';
  ZProp_DisableMultiRowFetch : TZProperty = (
    Name: ConnProps_DisableMultiRowFetch;
    Purpose: cASA_DisableMultiRowFetchPurpose;
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cNo_Yes_Enum; Default: cNo; Alias: ConnProps_DMRF;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_DMRF : TZProperty = (
    Name: ConnProps_DMRF;
    Purpose: cASA_DisableMultiRowFetchPurpose;
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cNo_Yes_Enum; Default: cNo; Alias: ConnProps_DisableMultiRowFetch;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_Elevate : TZProperty = (
    Name: ConnProps_Elevate;
    Purpose: 'Elevates automatically started database server executables on '+LineEnding+
      'Windows Vista.'+LineEnding+'Elevate={ YES | NO }'+LineEnding+
      'You can specify ELEVATE=YES in your connection string so that '+
      'automatically started database server executables are elevated. This '+
      'allows non-elevated client processes to start elevated servers '+
      'automatically, which is necessary on Windows Vista because non-elevated '+
      'servers cannot use AWE memory. This parameter is ignored if the database '+
      'server is not started automatically. You must specify the -cw option '+
      'when starting the database server command to use an AWE cache.';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: cNo_Yes_Enum; Default: cNo; Alias: '';
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_EncryptedPasswordPurpose =
    'Provides a password, stored in an encrypted format in a data source.'+LineEnding+
    '{ EncryptedPassword | ENP }=password'+LineEnding+
    'Every user of a database has a password. The password must be supplied for '+
    'the user to connect to the database. The EncryptedPassword (ENP) connection '+
    'parameter is used to specify an encrypted password. An application may '+
    'include the encrypted password in the connection string. If both the '+
    'Password (PWD) connection parameter and the EncryptedPassword (ENP) '+
    'connection parameter are specified, the Password (PWD) connection '+
    'parameter takes precedence.';
  ZProp_EncryptedPassword : TZProperty = (
    Name: ConnProps_EncryptedPassword;
    Purpose: cASA_EncryptedPasswordPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_ENP;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_ENP : TZProperty = (
    Name: ConnProps_ENP;
    Purpose: cASA_EncryptedPasswordPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_EncryptedPassword;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_EncryptionPurpose =
    'Encrypts packets sent between the client application and the database '+
    'server using transport-layer security or simple encryption.'+LineEnding+
    '{ Encryption | ENC }={ NONE | SIMPLE | TLS( TLS_TYPE=algorithm;'+LineEnding+
    '  [ FIPS={ Y | N }; ]'+LineEnding+
    ' TRUSTED_CERTIFICATE=public-certificate;'+LineEnding+
    '  [ CERTIFICATE_COMPANY=organization; ]'+LineEnding+
    '  [ CERTIFICATE_NAME=common-name; ]'+LineEnding+
    '  [ CERTIFICATE_UNIT=organization-unit ] )'+LineEnding+
    'Every user of a database has a password. The password must be supplied for '+
    'the user to connect to the database. The Encryption (ENP) connection '+
    'parameter is used to specify an encrypted password. An application may '+
    'include the encrypted password in the connection string. If both the '+
    'Password (PWD) connection parameter and the Encryption (ENP) '+
    'connection parameter are specified, the Password (PWD) connection '+
    'parameter takes precedence. ';
  ZProp_Encryption : TZProperty = (
    Name: ConnProps_Encryption;
    Purpose: cASA_EncryptionPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_ENC;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_ENC : TZProperty = (
    Name: ConnProps_ENC;
    Purpose: cASA_EncryptionPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Encryption;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_FileDataSourceNamePurpose =
    'Tells the client library there is an ODBC file data source holding information about the database to which you want to connect.'+LineEnding+
    '{ FileDataSourceName | FILEDSN }=file-data-source-name'+LineEnding+
    'File Data Sources hold the same information as ODBC data sources stored '+LineEnding+
    'in the registry. File Data Sources can be easily distributed to end users '+LineEnding+
    'so that connection information does not have to be reconstructed on each '+LineEnding+
    'computer.'+LineEnding+
    'Both ODBC and embedded SQL applications can use file data sources.';
  ZProp_FileDataSourceName : TZProperty = (
    Name: ConnProps_FileDataSourceName;
    Purpose: cASA_FileDataSourceNamePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_FILEDSN;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_FILEDSN : TZProperty = (
    Name: ConnProps_FILEDSN;
    Purpose: cASA_FileDataSourceNamePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_FileDataSourceName;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_ForceStartPurpose =
    'Starts a database server without checking if any server is running.'+LineEnding+
    '{ ForceStart | FORCE }={ YES | NO }'+LineEnding+
    'Only with the db_start_engine function. '+LineEnding+
    'By setting ForceStart=YES, the db_start_engine function does not check if '+
    'any server is running before starting the server.';
  ZProp_ForceStart : TZProperty = (
    Name: ConnProps_ForceStart;
    Purpose: cASA_ForceStartPurpose;
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'YES|NO'; Default: 'NO'; Alias: ConnProps_FORCE;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_FORCE : TZProperty = (
    Name: ConnProps_FORCE;
    Purpose: cASA_ForceStartPurpose;
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'YES|NO'; Default: 'NO'; Alias: ConnProps_ForceStart;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_Host : TZProperty = (
    Name: ConnProps_Host;
    Purpose: 'Accepts a host name or IP address and optional port number that '+
      'tells the client where to find the database server.'+LineEnding+
      'Host={ hostname | ip-address }[ :port-number ]'+LineEnding+
      'The Host connection parameter is recommended for connections to a '+
      'network server, and indicates the use of TCP/IP.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Host;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_Idle : TZProperty = (
    Name: ConnProps_Host;
    Purpose: 'Specifies a connection''s idle timeout period.'+LineEnding+
      'Idle=timeout-value'+LineEnding+
      'The connection''s idle timeout period, in minutes. The minimum value '+
      'for the Idle connection parameter is 1 minute, and the maximum supported '+
      'value is 32767 minutes. If you specify 0, idle timeout checking is '+
      'turned off for the connection.'+LineEnding+
      'Default: 240 minutes (TCP/IP)|0 (shared memory)';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Idle;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_IntegratedPurpose =
    'Specifies whether an integrated login can be attempted.'+LineEnding+
    '{ Integrated | INT }={ YES | NO }'+LineEnding+
    'For a client application to use an integrated login, the server must be '+
    'running with the login_mode database option set to a value that includes '+
    'Integrated.';
  ZProp_Integrated : TZProperty = (
    Name: ConnProps_Integrated;
    Purpose: cASA_IntegratedPurpose;
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'YES|NO'; Default: 'NO'; Alias: ConnProps_INT;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_INT : TZProperty = (
    Name: ConnProps_INT;
    Purpose: cASA_IntegratedPurpose;
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'YES|NO'; Default: 'NO'; Alias: ConnProps_Integrated;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_KerberosPurpose =
    'Specifies whether Kerberos authentication can be used when connecting to the database server.'+LineEnding+
    '{ Kerberos | KRB }= { YES | NO | SSPI | GSS-API-library-file }'+LineEnding+
    'The UserID and Password connection parameters are ignored when using a '+
    'Kerberos authenticated login.'+LineEnding+
    'To use Kerberos authentication, a Kerberos client must already be '+
    'installed and configured (nothing needs to be done for SSPI), the user '+
    'must have already logged in to Kerberos (have a valid ticket-granting '+
    'ticket), and the database server must have enabled and configured Kerberos '+
    'authenticated logins.';
  ZProp_Kerberos : TZProperty = (
    Name: ConnProps_Kerberos;
    Purpose: cASA_KerberosPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'NO'; Alias: ConnProps_KRB;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_KRB : TZProperty = (
    Name: ConnProps_KRB;
    Purpose: cASA_KerberosPurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'NO'; Alias: ConnProps_Kerberos;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_LANG : TZProperty = (
    Name: ConnProps_LANG;
    Purpose: cLanguagePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Language;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  cASA_LazyClosePurpose =
    'Controls whether cursor requests are queued until the next request or '+
    'performed immediately. Queuing close cursor requests saves a round trip '+
    'and improves performance.'+LineEnding+
    '{ LazyClose | LCLOSE }={ YES | NO | AUTO }'+LineEnding+
    'When this connection parameter is set to YES or AUTO, cursors are not '+
    'closed until the next database request. Enabling this option can improve '+
    'performance, if your network exhibits poor latency or your application '+
    'sends many cursor open and close requests.';
  ZProp_LazyClose : TZProperty = (
    Name: cASA_LazyClosePurpose;
    Purpose: cASA_LazyClosePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: 'YES|NO|AUTO'; Default: 'AUTO'; Alias: ConnProps_LCLOSE;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
  ZProp_LCLOSE : TZProperty = (
    Name: ConnProps_KRB;
    Purpose: cASA_LazyClosePurpose;
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: 'YES|NO|AUTO'; Default: 'AUTO'; Alias: ConnProps_LazyClose;
    Providers: (Count: 1; Items: @cASAProvider);
    Protocols: (Count: 2; Items: @cASAProtocols);
  );
 (*
  ConnProps_LivenessTimeout = 'LivenessTimeout';
  ConnProps_LTO = 'LTO';
  {$IFNDEF ENABLE_DBLIB}
  ConnProps_LogFile = 'LogFile';
  ConnProps_LOG = 'LOG';
  {$ENDIF ENABLE_DBLIB}
  ConnProps_NewPassword = 'NewPassword';
  ConnProps_NEWPWD = 'NEWPWD';
  ConnProps_MatView = 'MatView';
  ConnProps_NodeType = 'NodeType';
  ConnProps_NODE = 'NODE';
  //ConnProps_Password  = 'Password';
  //ConnProps_PWD = 'PWD';
  ConnProps_PrefetchBuffer = 'PrefetchBuffer';
  ConnProps_PBUF = 'PBUF';
  ConnProps_PrefetchOnOpen = 'PrefetchOnOpen';
  ConnProps_PrefetchRows = 'PrefetchRows';
  ConnProps_PROWS = 'PROWS';
  ConnProps_RetryConnectionTimeout = 'RetryConnectionTimeout';
  ConnProps_RetryConnTO = 'RetryConnTO';
  ConnProps_ServerName = 'ServerName';
  ConnProps_StartLine = 'StartLine';
  ConnProps_START = 'START';
  ConnProps_Unconditional = 'Unconditional';
  ConnProps_UNC = 'UNC';
    *)
{$ENDIF}

{$IFDEF ENABLE_ODBC}
  cODBCProtocols: array[0..1] of String = ('odbc_a','odbc_w');
  ZProp_Server : TZProperty = (
    Name: ConnProps_Server;
    Purpose: 'Specifies the name of a running database server to which you want to connect.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @cODBCProtocols);
  );
  ZProp_CharacterSet : TZProperty = (
    Name: ConnProps_CharacterSet;
    Purpose: 'Specifies the character set to be used on this connection. '+
      'Syntax: characterset={ NONE | character-set }'+LineEnding+
      'NONE   Specifying CharSet=NONE requests that the connection use the '+
      'database CHAR character set.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_CS+','+ConnProps_CodePage;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @cODBCProtocols);
  );
  ZProp_DRIVER : TZProperty = (
    Name: ConnProps_DRIVER;
    Purpose: 'Specifies the ODBC driver to be used on this connection.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @cODBCProtocols);
  );
{$ENDIF}

initialization
  prEditValuesPageIndex := 0;
  frmPropertyHeight := -1;
  frmPropertyWidth := -1;
  frmPropertyHeight := -1;
  frmPropertyWidth := -1;
  frmPropertyTop := -1;
  frmPropertyLeft := -1;
  pnlBottomHeight := -1;
  gbValWidth := -1;
  bgPropsUsedWidth := -1;
  LastListBox := nil;
  LastItemIndex := -1;
  HideEquals := True;

  RegisterZProperties([@ZProp_UID, @ZProp_Username, @ZProp_PWD, @ZProp_Password,
    @ZProp_LibLocation, @ZProp_CodePage, //@ZProp_Transliterate,
    @ZProp_ControlsCP, @ZProp_Timeout,
    @ZProp_DateReadFormat, @ZProp_DateWriteFormat, @ZProp_TimeReadFormat,
    @ZProp_TimeWriteFormat, @ZProp_DateTimeReadFormat, @ZProp_DateTimeWriteFormat,
    @ZProp_IdentifierQuotes, @ZProp_KeyFields, @ZProp_AffectedRows]);
{$IF declared(DSProps_PreferPrepared)}
  RegisterZProperty(@ZProp_PreferPrepared);
{$IFEND}
{$IF declared(ZProp_InternalBufSize)}
  RegisterZProperty(@ZProp_InternalBufSize);
{$IFEND}
{$IFDEF ENABLE_SQLITE}
  RegisterZProperties([@ZProp_Encrypted, @ZProp_BusyTimeout, @ZProp_CacheSize,
    @ZProp_Synchronous, @ZProp_LockingMode, @ZProp_ForeignKeys,
    @ZProp_journal_mode, @ZProp_BindDoubleDateTimeValues,
    @ZProp_BindOrdinalBoolValues, @ZProp_SQLiteTransactionBehaviour,
    @ZProp_SQLiteIntAffinity, @ZProp_SQLiteOpen_Flags, @ZProp_SQLiteOpen_zVfs]);
{$ENDIF}
{$IFDEF ENABLE_ORACLE}
  RegisterZProperties([@ZProp_ServerCachedStmts,@ZProp_BlobPrefetchSize,
    @ZProp_StatementCache,@ZProp_row_prefetch_size,@ZProp_OCIAuthenticateMode,@ZProp_MultiThreaded]);
{$ENDIF}
{$IF declared(ZProp_CachedLobs)}
  RegisterZProperty(@ZProp_CachedLobs);
{$IFEND}
{$IF declared(ZProp_UndefVarcharAsStringLength)}
  RegisterZProperty(@ZProp_UndefVarcharAsStringLength);
{$IFEND}
{$IF declared(ZProp_OleDBProvider)}
  RegisterZProperty(@ZProp_OleDBProvider);
{$IFEND}
{$IF declared(ZProp_StatementTimeOut)}
  RegisterZProperty(@ZProp_StatementTimeOut);
{$IFEND}
{$IF declared(ZProp_TrustedConnection)}
  RegisterZProperty(@ZProp_TrustedConnection);
{$IFEND}
{$IF declared(ZProp_MinExecCntBeforePrepare)}
  RegisterZProperty(@ZProp_MinExecCntBeforePrepare);
{$IFEND}
{$IF declared(ZProp_EmulatePrepares)}
  RegisterZProperty(@ZProp_EmulatePrepares);
{$IFEND}
{$IF declared(ZProp_DeferPrepare)}
  RegisterZProperty(@ZProp_DeferPrepare);
{$IFEND}
{$IFDEF ENABLE_DBLIB}
  RegisterZProperties([@ZProp_TDSVersion, @ZProp_TDSProtocolVersion,
    @ZProp_AnsiPadding, @ZProp_AppName, @ZProp_Language, @ZProp_Workstation,
    @ZProp_TDSLog, @ZProp_TDSLogging, @ZProp_TDSDump, @ZProp_TDSLogFile,
    @ZProp_TDSLog_File, @ZProp_TDSDumpFile, @ZProp_TDSNTAuth, @ZProp_TDSSecure,
    @ZProp_TDSTrusted]);
{$ENDIF}
{$IFDEF ENABLE_MYSQL}
  RegisterZProperties([@ZProp_MYSQLSSL, @ZProp_MYSQLCompress, @ZProp_MYSQLdbless,
    @ZProp_MySQL_FieldType_Bit_1_IsBoolean, @ZProp_MySQLDatadir,
    @ZProp_MySQLLibrary, @ZProp_MySQL_UseResult,
    @ZProp_MySQL_STMT_ATTR_PREFETCH_ROWS, @ZProp_MySQL_chunk_size]);
  RegisterZPropertiesArray(Ord(High(TMySqlOption))+1, @ZMySqlOptionProperties);
  RegisterZPropertiesArray(Ord(High(TMariaDBConnectorOption))+1-Ord(Low(TMariaDBConnectorOption)), @ZMariaDbConnectorOptionProperties);
{$ENDIF}
{$IF declared(ZProp_SessionIdleTimeOut)}
  RegisterZProperties([@ZProp_SessionIdleTimeOut, @ZProp_FirebirdAPI]);
{$IFEND}
{$IF declared(ZProp_SessionIdleTimeOut)}
  RegisterZProperty(@ZProp_FBProtocol);
{$IFEND}
{$IF defined(ENABLE_INTERBASE) OR DEFINED(ENABLE_FIREBIRD)}
  RegisterZProperties([@ZProp_IB_FB_Dialect, @ZProp_IB_FB_Rolename,
    @ZProp_IB_FB_WireCompression, @ZProp_CreateNewDatabase, @ZProp_SetGUIDByType,
    @ZProp_GUIDDomains, @ZProp_GUIDFields, @ZProp_Charset_NONE_Alias,
    @ZProp_InsertReturningFields,
    //database parameter block properties:
    @ZProp_isc_dpb_page_size, @ZProp_isc_dpb_num_buffers, @ZProp_isc_dpb_debug,
    @ZProp_isc_dpb_garbage_collect, @ZProp_isc_dpb_verify, @ZProp_isc_dpb_sweep,
    @ZProp_isc_dpb_enable_journal, @ZProp_isc_dpb_disable_journal,
    @ZProp_isc_dpb_dbkey_scope, @ZProp_isc_dpb_trace, @ZProp_isc_dpb_no_garbage_collect,
    @ZProp_isc_dpb_damaged, @ZProp_isc_dpb_license, @ZProp_isc_dpb_sys_user_name,
    @ZProp_isc_dpb_encrypt_key, @ZProp_isc_dpb_activate_shadow, @ZProp_isc_dpb_sweep_interval,
    @ZProp_isc_dpb_delete_shadow, @ZProp_isc_dpb_force_write, @ZProp_isc_dpb_begin_log,
    @ZProp_isc_dpb_quit_log, @ZProp_isc_dpb_no_reserve, @ZProp_isc_dpb_user_name,
    @ZProp_isc_dpb_password, @ZProp_isc_dpb_password_enc, @ZProp_isc_dpb_sys_user_name_enc,
    @ZProp_isc_dpb_interp, @ZProp_isc_dpb_online_dump, @ZProp_isc_dpb_old_file_size,
    @ZProp_isc_dpb_old_num_files, @ZProp_isc_dpb_old_file, @ZProp_isc_dpb_old_start_page,
    @ZProp_isc_dpb_old_start_seqno, @ZProp_isc_dpb_old_start_file, @ZProp_isc_dpb_drop_walfile,
    @ZProp_isc_dpb_old_dump_id, @ZProp_isc_dpb_wal_backup_dir, @ZProp_isc_dpb_wal_chkptlen,
    @ZProp_isc_dpb_wal_numbufs, @ZProp_isc_dpb_wal_bufsize, @ZProp_isc_dpb_wal_grp_cmt_wait,
    @ZProp_isc_dpb_lc_messages, @ZProp_isc_dpb_lc_ctype, @ZProp_isc_dpb_shutdown,
    @ZProp_isc_dpb_online, @ZProp_isc_dpb_shutdown_delay, @ZProp_isc_dpb_reserved,
    @ZProp_isc_dpb_overwrite, @ZProp_isc_dpb_sec_attach, @ZProp_isc_dpb_connect_timeout,
    @ZProp_isc_dpb_dummy_packet_interval, @ZProp_isc_dpb_gbak_attach, @ZProp_isc_dpb_sql_role_name,
    @ZProp_isc_dpb_set_page_buffers, @ZProp_isc_dpb_working_directory,
    @ZProp_isc_dpb_sql_dialect, @ZProp_isc_dpb_set_db_readonly, @ZProp_isc_dpb_set_db_sql_dialect,
    @ZProp_isc_dpb_gfix_attach, @ZProp_isc_dpb_gstat_attach, @ZProp_isc_dpb_set_db_charset,
    @ZProp_isc_dpb_gsec_attach, @ZProp_isc_dpb_address_path, @ZProp_isc_dpb_process_id,
    @ZProp_isc_dpb_no_db_triggers, @ZProp_isc_dpb_trusted_auth, @ZProp_isc_dpb_process_name,
    @ZProp_isc_dpb_trusted_role, @ZProp_isc_dpb_org_filename, @ZProp_isc_dpb_utf8_filename,
    @ZProp_isc_dpb_ext_call_depth, @ZProp_isc_dpb_auth_block, @ZProp_isc_dpb_client_version,
    @ZProp_isc_dpb_remote_protocol, @ZProp_isc_dpb_host_name, @ZProp_isc_dpb_os_user,
    @ZProp_isc_dpb_specific_auth_data, @ZProp_isc_dpb_auth_plugin_list,
    @ZProp_isc_dpb_auth_plugin_name, @ZProp_isc_dpb_config, @ZProp_isc_dpb_nolinger,
    @ZProp_isc_dpb_reset_icu, @ZProp_isc_dpb_map_attach, @ZProp_isc_dpb_session_time_zone,
    @ZProp_isc_dpb_set_db_replica, @ZProp_isc_dpb_set_bind, @ZProp_isc_dpb_decfloat_round,
    @ZProp_isc_dpb_decfloat_traps, @ZProp_HardCommit,
    //transaction parameter block properties:
    @ZProp_isc_tpb_consistency, @ZProp_isc_tpb_concurrency, @ZProp_isc_tpb_shared,
    @ZProp_isc_tpb_protected, @ZProp_isc_tpb_exclusive, @ZProp_isc_tpb_wait,
    @ZProp_isc_tpb_nowait, @ZProp_isc_tpb_read, @ZProp_isc_tpb_write,
    @ZProp_isc_tpb_lock_read, @ZProp_isc_tpb_lock_write, @ZProp_isc_ignore_limbo,
    @ZProp_isc_tpb_read_committed, @ZProp_isc_tpb_autocommit, @ZProp_isc_tpb_rec_version,
    @ZProp_isc_tpb_no_rec_version, @ZProp_isc_tpb_restart_requests, @ZProp_isc_tpb_no_auto_undo,
    @ZProp_isc_no_savepoint, @ZProp_isc_tpb_lock_timeout, @ZProp_isc_tpb_read_consistency]);
{$IFEND}
{$IFDEF ENABLE_ASA}
  RegisterZProperties([@ZProp_AppInfo, @ZProp_APP, @ZProp_AutoStart,
    @ZProp_ASTART, @ZProp_CharSet, @ZProp_CS, @ZProp_CommBufferSize,
    @ZProp_CBSIZE, @ZProp_CommLinks, @ZProp_LINKS, @ZProp_Compress, @ZProp_COMP,
    @ZProp_CompressionThreshold, @ZProp_COMPTH, @ZProp_ConnectionName, @ZProp_CON,
    @ZProp_ConnectionPool,@ZProp_CPOOL,@ZProp_DatabaseFile,@ZProp_DBF,
    @ZProp_DatabaseKey, @ZProp_DBKEY, @ZProp_DatabaseName, @ZProp_DBN,
    @ZProp_DatabaseSwitches, @ZProp_DBS, @ZProp_DatabaseSourceName, @ZProp_DSN,
    @ZProp_DisableMultiRowFetch, @ZProp_DMRF, @ZProp_Elevate,
    @ZProp_EncryptedPassword, @ZProp_ENP, @ZProp_Encryption, @ZProp_ENC,
    @ZProp_FileDataSourceName, @ZProp_FILEDSN,@ZProp_ForceStart,@ZProp_FORCE,
    @ZProp_Host, @ZProp_Idle, @ZProp_Integrated, @ZProp_INT,@ZProp_Kerberos,
    @ZProp_KRB, @ZProp_LANG, @ZProp_LazyClose, @ZProp_LCLOSE]);
{$ENDIF}
{$IFDEF ENABLE_ODBC}
  RegisterZProperties([@ZProp_Server, @ZProp_CharacterSet, @ZProp_DRIVER]);
{$ENDIF}

end.
