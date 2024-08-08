program FirebirdPasToZPlainFirebirdDriverPas;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  ZSysUtils, ZClasses, ZCompatibility;
type
  PObjectInfoDesc = ^TObjectInfoDesc;
  TObjectInfoDesc = record
    ObjectName, ObjectNameNew, InheritesFrom, InheritesFromNew: String;
    HelperList: TStringList;
  end;

  TObjectInfoDescList = Class(TZCustomUniqueElementBinarySearchList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create;
  End;



procedure PatchForwardDeclaration(Lines: TStringList; LineIndex: Integer; var Line: String);
var PC, PEnd, FWStart, FWEnd: PChar;
    tmp: String;
begin
  PC := Pointer(Line);
  PEnd := PC+Length(Line);
  FWStart := nil;
  FWEnd := nil;
  while (PC < PEnd) do begin
    if (FWStart = nil) and (Ord(PC^) = (Ord('I'))) then
      FWStart := PC+1;
    if (FWStart <> nil) and (Ord(PC^) = Ord(' ')) then begin
      FWEnd := PC;
      Break;
    end;
    Inc(PC);
  end;
  tmp := '';
  SetLength(tmp, FWEnd - FWStart);
  Move(FWStart^, Pointer(tmp)^, (FWEnd - FWStart) * SizeOf(Char));
  Line := '  I'+Tmp+' = {$IFDEF WITH_RECORD_METHODS}^T'+Tmp+'{$ELSE}class{$ENDIF};';
  Lines[LineIndex] := Line;
end;

procedure PatchObjectDeclaration(ObjList: TObjectInfoDescList; Lines, ConstList, IntfNameList: TStringList; var lIdx: Cardinal; ClassPos: Cardinal);
var PC, PEnd, FWStart, FWEnd: PChar;
    ObjectInfoDesc, FinderObjectInfoDesc: TObjectInfoDesc;
    Line, OrgLine, sTmp: String;
    tmpIdx: Cardinal;
    aIdx, aCnt: NativeInt;
    ObjectInfoDescRef, ObjectInfoDescRef2: PObjectInfoDesc;
begin
  FillChar(ObjectInfoDesc, SizeOf(TObjectInfoDesc), 0);
  FillChar(FinderObjectInfoDesc, SizeOf(TObjectInfoDesc), 0);
  Line := Lines[lIdx];
  OrgLine := Line;
  PC := Pointer(Line);
  PEnd := PC+ClassPos;
  FWStart := nil;
  FWEnd := nil;
  while (PC < PEnd) do begin
    if (FWStart = nil) and (Ord(PC^) >= (Ord('0'))) then
      FWStart := PC;
    if (FWStart <> nil) and (Ord(PC^) = Ord(' ')) then begin
      FWEnd := PC;
      Break;
    end;
    Inc(PC);
  end;
  SetLength(ObjectInfoDesc.ObjectName, FWEnd - FWStart);
  Move(FWStart^, Pointer(ObjectInfoDesc.ObjectName)^, (FWEnd - FWStart) * SizeOf(Char));
  if EndsWith(ObjectInfoDesc.ObjectName, 'VTable') then begin
    ObjectInfoDesc.ObjectNameNew := 'T'+ObjectInfoDesc.ObjectName;
  end;

  PC := Pointer(Line);
  PEnd := PC+Length(Line);
  Inc(PC, ClassPos+Cardinal(Length('= class')));
  FWStart := nil;
  FWEnd := nil;
  while (PC < PEnd) do begin
    if (FWStart = nil) and (Ord(PC^) >= (Ord('('))) then
      FWStart := PC;
    if (FWStart <> nil) and (Ord(PC^) = Ord(')')) then begin
      FWEnd := PC;
      Break;
    end;
    Inc(PC);
  end;
  if FWEnd <> nil then begin
    SetLength(ObjectInfoDesc.InheritesFrom, FWEnd - FWStart);
    Move(FWStart^, Pointer(ObjectInfoDesc.InheritesFrom)^, (FWEnd - FWStart) * SizeOf(Char));
    if ObjectInfoDesc.InheritesFrom = 'UserFieldVTable' then
    FinderObjectInfoDesc.ObjectName := ObjectInfoDesc.InheritesFrom else
    FinderObjectInfoDesc.ObjectName := ObjectInfoDesc.InheritesFrom;
    if ObjList.Find(@FinderObjectInfoDesc, aIdx) then begin
      ObjectInfoDescRef := ObjList.Get(aIdx);
      if ObjectInfoDescRef.ObjectNameNew <> '' then
        ObjectInfoDesc.InheritesFromNew := ObjectInfoDescRef.ObjectNameNew
      else
        ObjectInfoDesc.InheritesFromNew := 'T'+ObjectInfoDesc.InheritesFrom
    end;

  end;
  ObjList.Find(@ObjectInfoDesc, aIdx);
  ObjectInfoDescRef := ObjList.Insert(aIdx);
  ObjectInfoDescRef^.ObjectName := ObjectInfoDesc.ObjectName;
  ObjectInfoDescRef^.ObjectNameNew := ObjectInfoDesc.ObjectNameNew;
  ObjectInfoDescRef^.InheritesFrom := ObjectInfoDesc.InheritesFrom;
  ObjectInfoDescRef^.InheritesFromNew := ObjectInfoDesc.InheritesFromNew;
  Lines[lIdx] := '  {$IFDEF WITH_RECORD_METHODS}';
  Inc(lIdx);
  if EndsWith(ObjectInfoDesc.ObjectName, 'VTable') then begin
    ObjectInfoDesc.ObjectNameNew := 'T'+ObjectInfoDesc.ObjectName;
    Line := '  P'+ObjectInfoDesc.ObjectName+' = ^'+ObjectInfoDesc.ObjectNameNew+';';
    Lines.Insert(lIdx, Line);
    Inc(lIdx);
    Line := '  '+ObjectInfoDesc.ObjectNameNew+' = record';
    Lines.Insert(lIdx, Line);
    Inc(lIdx);
    if ObjectInfoDesc.ObjectName = 'VersionedVTable' then begin
      Lines.Insert(lIdx, '    SelfOffsetPtr: pointer;');
      Inc(lIdx);
    end;
    if ObjectInfoDesc.InheritesFrom <> '' then begin
      Line := '    '+ObjectInfoDesc.InheritesFrom+': T'+ObjectInfoDesc.InheritesFrom+';';
      Lines.Insert(lIdx, Line);
      Inc(lIdx);
    end;
    PC := Pointer(OrgLine);
    aCnt := 1;
    while (Ord(PC^) < Ord('0')) and (PC^ <> #0) do begin
      Inc(PC);
      Inc(aCnt);
    end;
    Insert('T', OrgLine, aCnt);
    PC := Pointer(OrgLine);
    aCnt := 1;
    while (Ord(PC^) <> Ord('(')) and (PC^ <> #0) do begin
      Inc(PC);
      Inc(aCnt);
    end;
    if PC^ <> #0 then
      Insert('T', OrgLine, aCnt+1);
    ObjectInfoDescRef^.HelperList := TStringList.Create;
    tmpIdx := lIdx;
    while tmpIdx <= Cardinal(Lines.Count) -1 do begin
      Line := Lines[tmpIdx];
      if EndsWith(Line, 'end;') then
        Break;
      aCnt := Pos('version: NativeInt', Line);
      if ((aCnt = 0) or (ObjectInfoDesc.ObjectName = 'VersionedVTable')) and (Line <> '') then
        ObjectInfoDescRef^.HelperList.Add(Trim(Line));
      Inc(tmpIdx);
    end;
  end else if EndsWith(ObjectInfoDesc.ObjectName, 'Impl') then begin
    if ObjectInfoDesc.ObjectName = 'IVersionedImpl' then begin
      Lines.Insert(lIdx, '  IVersionedImpl = class');
      Inc(lIdx);
      Lines.Insert(lIdx, '  protected');
      Inc(lIdx);
      Lines.Insert(lIdx, '    SelfOffsetPtr: pointer;');
      Inc(lIdx);
      Lines.Insert(lIdx, '    vTable: pointer;');
      Inc(lIdx);
      Lines.Insert(lIdx, '    SelfObject: Pointer;');
      Inc(lIdx);
      Lines.Insert(lIdx, '  public');
      Inc(lIdx);
    end else begin
      FinderObjectInfoDesc.ObjectName := ObjectInfoDescRef.InheritesFrom;
      if ObjList.Find(@FinderObjectInfoDesc, aIdx) then begin
        ObjectInfoDescRef2 := ObjList.Get(aIdx);
        if ObjectInfoDescRef2.InheritesFrom <> '' then
          ObjectInfoDescRef.InheritesFromNew := ObjectInfoDescRef2.InheritesFrom+'Impl';
        Line := '  '+ObjectInfoDescRef.ObjectName+' = class('+ObjectInfoDescRef.InheritesFromNew+')';
        Lines.Insert(lIdx, line);
        Inc(lIdx);
        Lines.Insert(lIdx, '  public');
        Inc(lIdx);
        if (ObjectInfoDesc.ObjectName <> 'IDisposableImpl') and (ObjectInfoDesc.ObjectName <> 'IReferenceCountedImpl') then begin
          sTmp := Copy(ObjectInfoDescRef.ObjectName, 1, Length(ObjectInfoDescRef.ObjectName) -4);
          Lines.Insert(lIdx, '    function As'+sTmp+': '+sTmp+'; {$IFDEF WITH_INLINE}inline;{$ENDIF}');
          Inc(lIdx);
        end;
      end;
      ObjectInfoDescRef^.HelperList := TStringList.Create;
      Line := Lines[lIdx];
      if Pos('constructor', Line) > 0 then begin
        tmpIdx := lIdx +1;
        while tmpIdx <= Cardinal(Lines.Count) -1 do begin
          Line := Lines[tmpIdx];
          aCnt := Pos('virtual; abstract;', Line);
          if (Pos('toString', Line) > 0) and (aCnt > 0) then begin
            insert('reintroduce; ', Line, aCnt);
            Lines[tmpIdx] := Line;
          end;
          if EndsWith(Line, 'end;') then begin
            Lines.Insert(tmpIdx, '    constructor create;');
            Lines.Insert(tmpIdx, '  public');
            Break;
          end else if Line = '' then begin
            Lines.Delete(tmpIdx);
            Continue;
          end else
            ObjectInfoDescRef^.HelperList.Add(Line);
          Inc(tmpIdx);
        end;
        Lines.Delete(lIdx);
      end;
    end;
  end else begin //the interfaces
    Line := Copy(ObjectInfoDescRef.ObjectName, 2, Length(ObjectInfoDescRef.ObjectName)-1);
    sTmp := 's'+ObjectInfoDescRef.ObjectName+': RawByteString = '''+ObjectInfoDescRef.ObjectName+''';';
    IntfNameList.Add(sTmp);
    ObjectInfoDescRef.ObjectNameNew := 'T'+Line;
    Lines.Insert(lIdx, '  '+ObjectInfoDescRef.ObjectNameNew+' = record');
    Inc(lIdx);
    if ObjectInfoDescRef.ObjectName = 'IVersioned' then begin
      Lines.Insert(lIdx, '    SelfOffsetPtr: pointer;');
      Inc(lIdx);
      Lines.Insert(lIdx, '    vTable: PVersionedVTable;');
      Inc(lIdx);
      Lines.Insert(lIdx, '    SelfObject: Pointer;');
      Inc(lIdx);
    end;
    if ObjectInfoDescRef.InheritesFrom <> '' then begin
      Line := Copy(ObjectInfoDescRef.InheritesFrom, 2, Length(ObjectInfoDescRef.InheritesFrom)-1);
      Line := '    '+Line+': T'+Line+';';
      Lines.Insert(lIdx, Line);
      Inc(lIdx);
    end;
  end;
  Lines.Insert(lIdx, '  {$ELSE !WITH_RECORD_METHODS}');
  Inc(lIdx);
  Lines.Insert(lIdx, OrgLine);
  Inc(lIdx);
  if (ObjectInfoDescRef <> nil) {and (EndsWith(ObjectInfoDescRef.ObjectName, 'Impl') or EndsWith(ObjectInfoDescRef.ObjectName, 'VTable'))}
    and (not EndsWith(Lines[lIdx+1], 'end;') or ((ObjectInfoDescRef.HelperList <> nil) and (ObjectInfoDescRef.HelperList.Count > 0))) then begin
    Lines.Insert(lIdx, '  public');
    Inc(lIdx);
  end;
  aCnt := 0;
  if (ObjectInfoDescRef <> nil) and EndsWith(ObjectInfoDescRef.ObjectName, 'Impl') and (ObjectInfoDescRef.HelperList <> nil) then begin
    FinderObjectInfoDesc.ObjectName := ObjectInfoDescRef.InheritesFromNew;
    if ObjList.Find(@FinderObjectInfoDesc, aIdx) then begin
      ObjectInfoDescRef2 := ObjList.Get(aIdx);
      if ObjectInfoDescRef2.HelperList <> nil then begin
        for aIdx := 0 to ObjectInfoDescRef2.HelperList.Count -1 do begin
          Line := ObjectInfoDescRef2.HelperList[aIdx];
          if ObjectInfoDescRef.HelperList.IndexOf(Line) >= 0 then begin
            Lines.Insert(lIdx, Line);
            Inc(lIdx);
            Inc(aCnt);
          end;
        end;
      end;
    end;
  end;
  {if ObjectInfoDesc.ObjectName = 'IVersionedImpl' then begin
    Lines.Insert(lIdx, '  public');
    Inc(lIdx);
  end;}

  Lines.Insert(lIdx, '  {$ENDIF !WITH_RECORD_METHODS}');
  Inc(lIdx);
  if ObjectInfoDesc.ObjectName = 'IVersioned' then begin
    Lines.Insert(lIdx-1, '    vTable: TVersionedVTable');
    Inc(lIdx);
    Line := Lines[lIdx];
    Lines.Delete(lIdx); //delete old vTable line
  end;
  for aIdx := 0 to aCnt -1 do
    Lines.Delete(lIdx);
  if StartsWith(ObjectInfoDescRef.ObjectName, 'I') and StartsWith(ObjectInfoDescRef.ObjectNameNew, 'T') then begin
    while lIdx < Cardinal(Lines.Count) - 1 do begin
      Line := Lines[lIdx];
      if EndsWith(Line, 'end;') then
        Break;
      if (Pos('function', Line) > 0) or (Pos('procedure', Line) > 0) then begin
        aCnt := Pos(' reintroduce;', Line);
        if aCnt > 0 then begin
          Delete(Line, aCnt, Length(' reintroduce;'));
          Insert('{$IFNDEF WITH_RECORD_METHODS} reintroduce; {$ENDIF WITH_RECORD_METHODS}', Line, aCnt);
          Lines[lIdx] := Line;
        end;
      end;
      if Line = '' then begin
        Lines.Delete(lIdx);
        Continue;
      end;
      aIdx := Pos('const', Line);
      if aIdx > 0 then begin
        Line := Copy(Line, aIdx+6, Length(Line)-6);
        sTmp :='c'+ObjectInfoDescRef.ObjectName+'_';
        Line := StringReplace(Line, ObjectInfoDescRef.ObjectName+'.', sTmp, [rfReplaceAll]);
        Line := sTmp+Trim(Line);
        ConstList.Add(Line);
        Lines.Delete(lIdx);
        Continue;
      end;
      Inc(lIdx);
    end;
  end;
end;

function ConcatVTable(ModelList: TObjectInfoDescList; const AObjName: String): String;
var
  aObjToFind: TObjectInfoDesc;
  ObjectInfoDescRef: PObjectInfoDesc;
  findIdx: NativeInt;
  vTableField: String;
begin
  FillChar(aObjToFind, SizeOf(TObjectInfoDesc), 0);
  aObjToFind.ObjectName := AObjName;
  Result := '';
  if ModelList.Find(@aObjToFind, findIdx) then begin
    ObjectInfoDescRef := ModelList.Get(findIdx);
    if ObjectInfoDescRef.InheritesFrom <> '' then begin
      Result := ConcatVTable(ModelList, ObjectInfoDescRef.InheritesFrom);
      vTableField := StringReplace(ObjectInfoDescRef.InheritesFrom, 'VTable', '', []);;
      vTableField := Copy(vTableField, 2, Length(vTableField) -1);
      if Result = '' then
        Result := vTableField
      else begin
        Result := vTableField+'.'+Result;
      end;
    end;
  end;
end;

function ConcatImplVar(ModelList: TObjectInfoDescList; const AObjName, CallerVersion, Spaces: String): String;
var
  aObjToFind: TObjectInfoDesc;
  ObjectInfoDescRef: PObjectInfoDesc;
  findIdx: NativeInt;
  aMethodIdx: Integer;
  aName, ImplName, tmp: String;
begin
  FillChar(aObjToFind, SizeOf(TObjectInfoDesc), 0);
  aObjToFind.ObjectName := AObjName;
  Result := '';
  if ModelList.Find(@aObjToFind, findIdx) then begin
    ObjectInfoDescRef := ModelList.Get(findIdx);
    if ObjectInfoDescRef.InheritesFrom <> '' then begin
      Result := ConcatImplVar(ModelList, ObjectInfoDescRef.InheritesFrom, CallerVersion, Spaces+'  ');
      Result := ObjectInfoDescRef.InheritesFrom+': ('+LineEnding+'  '+Spaces+Result;
      if (ObjectInfoDescRef.HelperList <> nil) and (ObjectInfoDescRef.HelperList.Count > 0) then begin
        aName := StringReplace(ObjectInfoDescRef.ObjectName, 'VTable', '', []);
        ImplName := aName+'Impl';
        for aMethodIdx := 0 to ObjectInfoDescRef.HelperList.Count -2 do begin
          tmp := StringReplace(ObjectInfoDescRef.HelperList[aMethodIdx], aName, ImplName,[]);
          Result := Result+LineEnding+Spaces +StringReplace(tmp, 'Ptr;', 'Dispatcher;',[]);
        end;
        tmp := StringReplace(ObjectInfoDescRef.HelperList[ObjectInfoDescRef.HelperList.Count -1], aName, ImplName,[]);
        Result := Result+LineEnding+Spaces +StringReplace(tmp, 'Ptr;', 'Dispatcher',[]);
      end;
      Result := Result +');';
    end else if AObjName = 'VersionedVTable' then begin
      Result := 'SelfOffsetPtr: nil; version: '+CallerVersion+');';
    end;
  end;
end;

procedure FirebirdPasToZPlainFirebird(const SrcFileName: String);
var Lines: TStringList;
  lIdx, tmpIdx, SaveIdx, TypeLineIdx: Cardinal;
  lPos, lPos2: Integer;
  aLine, sTmp, sTmp2, aVersionConstName, vTableNew: String;
  findIdx: NativeInt;
  B: Boolean;
  ModelList: TObjectInfoDescList;
  aObjToFind: TObjectInfoDesc;
  ObjectInfoDescRef: PObjectInfoDesc;
  AConstList, aBodyList, aIntfNameList: TStringList;
  PC: PChar;
  y,m,d: Word;
begin
  TypeLineIdx := 0;
  FillChar(aObjToFind, SizeOf(TObjectInfoDesc), 0);
  if FileExists(SrcFileName) then begin
    Lines := TStringList.Create;
    lIdx := 0;
    ModelList := TObjectInfoDescList.Create;
    AConstList := TStringList.Create;
    aBodyList := TStringList.Create;
    aIntfNameList := TStringList.Create;
    try
      Lines.LoadFromFile(SrcFileName, TEncoding.UTF8);
      { Patch forward declarations}
      B := False;
      while lIdx < Cardinal(Lines.Count -1) do begin
        aLine := Lines[lIdx];
        if lIdx = 1 then begin
          aLine := 'unit ZPlainFirebird;';
          Lines[lIdx] := aLine;
          inc(lIdx);
          Lines.Insert(lIdx, '');
          inc(lIdx);
          Lines.Insert(lIdx, 'interface');
          inc(lIdx);
          Lines.Insert(lIdx, '');
          inc(lIdx);
          Lines.Insert(lIdx, '{$I ZPlain.inc}');
          inc(lIdx);
          Lines.Insert(lIdx, '');
        end;
        if StartsWith(aLine, '{$MODE') then begin
          Lines.Delete(lIdx);
          Continue;
        end else if StartsWith(aLine, '{$OBJECTCHECKS') then begin
          Lines.Insert(lIdx, '{$IFNDEF WITH_RECORD_METHODS}{$INTERFACES CORBA}{$ENDIF}');
          Break;
        end;
        Inc(lIdx);
      end;
      while lIdx < Cardinal(Lines.Count -1) do begin
        aLine := Lines[lIdx];
        if StartsWith(aLine, 'interface') or StartsWith(aLine, 'unit') then begin
          Lines.Delete(lIdx);
          Continue;
        end else if StartsWith(aLine, 'uses') then begin
          Lines.Insert(lIdx, '{$IFNDEF ZEOS_DISABLE_FIREBIRD}  //if set we''ve a empty unit');
          Inc(lIdx);
          Lines.Insert(lIdx, '');
          Inc(lIdx);
          aLine := 'uses ZCompatibility, ZPlainFirebirdInterbaseDriver;';
          Lines[lIdx] := aLine;
          Break;
        end;
        Inc(lIdx);
      end;
      while lIdx < Cardinal(Lines.Count -1) do begin
        aLine := Lines[lIdx];
        if StartsWith(aLine, 'type') then begin
          TypeLineIdx := lIdx;
          //delete the QWord implementation
          Inc(lIdx);
          Lines.Delete(lIdx);
          Lines.Delete(lIdx);
          Lines.Delete(lIdx);
          Lines.Delete(lIdx);
          Continue;
        end
        else if EndsWith(aLine, '= class;') then begin
          PatchForwardDeclaration(Lines, lIdx, aLine);
          B := True;
        end else if B then begin
          Inc(lIdx);
          Break;
        end;
        Inc(lIdx);
      end;
      { now skip all lines until the method declaration start}
      if B then begin
        while lIdx < Cardinal(Lines.Count -1) do begin
          aLine := Lines[lIdx];
          if Pos('Exception', aLine) > 0 then begin
            Lines.Delete(lIdx);
            while True do begin
              aLine := Lines[lIdx];
              Lines.Delete(lIdx);
              if EndsWith(aLine, 'end;') then
                break;
            end;
          end;
          if EndsWith(aLine, 'cdecl;') then
            Break
          else if ((aLine <> '') or (Lines[lIdx-1] = '')) and
              (Pos('BooleanPtr = ^Boolean;', aLine) = 0) and
              (Pos('IKeyHolderPluginPtr', aLine) = 0) then begin
            Lines.Delete(lIdx);
            Continue;
          end;
          inc(lIdx)
        end;
      end;

      while lIdx < Cardinal(Lines.Count -1) do begin
        aLine := Lines[lIdx];
        lPos := Pos('= class', aLine);
        if Pos('= class', aLine) > 0 then begin
          PatchObjectDeclaration(ModelList, Lines, AConstList, aIntfNameList, lIdx, lPos);
        end else if aLine = 'implementation' then begin
          Lines.Insert(lIdx,    '{$ENDIF ZEOS_DISABLE_FIREBIRD}');
          Lines.Insert(lIdx +2, '{$IFNDEF ZEOS_DISABLE_FIREBIRD}');
          Lines.Insert(lIdx +3,'');
          Lines.Insert(lIdx +4, 'procedure setVersionError(status: IStatus; interfaceName: PAnsiChar;');
          Lines.Insert(lIdx +5, '  currentVersion, expectedVersion: NativeInt);');
          Lines.Insert(lIdx +6, 'var statusVector: array[0..8] of NativeIntPtr;');
          Lines.Insert(lIdx +7, 'begin');
          Lines.Insert(lIdx +8, '  statusVector[0] := NativeIntPtr(isc_arg_gds);');
          Lines.Insert(lIdx +9, '  statusVector[1] := NativeIntPtr(isc_interface_version_too_old);');
          Lines.Insert(lIdx +10,'  statusVector[2] := NativeIntPtr(isc_arg_number);');
          Lines.Insert(lIdx +11,'  statusVector[3] := NativeIntPtr(expectedVersion);');
          Lines.Insert(lIdx +12,'  statusVector[4] := NativeIntPtr(isc_arg_number);');
          Lines.Insert(lIdx +13,'  statusVector[5] := NativeIntPtr(currentVersion);');
          Lines.Insert(lIdx +14,'  statusVector[6] := NativeIntPtr(isc_arg_string);');
          Lines.Insert(lIdx +15,'  statusVector[7] := NativeIntPtr(interfaceName);');
          Lines.Insert(lIdx +16,'  statusVector[8] := NativeIntPtr(isc_arg_end);');
          Lines.Insert(lIdx +17,'  status.setErrors(@statusVector);');
          Lines.Insert(lIdx +18,'end;');
          Lines.Insert(lIdx +19,'');
          Lines.Insert(lIdx +20,'const');
          Inc(lIdx, 21);
          for lPos := 0 to aIntfNameList.Count -1 do begin
            Lines.Insert(lIdx, '  '+aIntfNameList[lPos]);
            Inc(lIdx);
          end;
          Break;
        end;
        Inc(lIdx);
      end;
      while lIdx < Cardinal(Lines.Count -1) do begin
        aLine := Lines[lIdx];
        if StartsWith(aLine, 'procedure') or StartsWith(aLine, 'function') and
           (Pos('FbException.', aLine) = 0)  then begin
          if EndsWith(aLine, 'cdecl;') then begin //Dispatcher -> remove the Try except block first
            tmpIdx := lIdx +1;
            while tmpIdx < Cardinal(Lines.Count -1) do begin
              aLine := Lines[tmpIdx];
              if aLine = 'end;' then
                Break;
              if aLine <> 'begin' then begin
                if (Pos('checkException', aLine) > 0) then
                  Lines.Delete(tmpIdx);
                if EndsWith(aLine, 'try') or EndsWith(aLine, 'except') or
                   EndsWith(aLine, 'Result := 0;') or EndsWith(aLine, 'end') or
                   (Pos('on e: Exception', aLine) > 0) or EndsWith(aLine, 'Result := nil;') or
                   EndsWith(aLine, 'Result := false;') then begin
                  Lines.Delete(tmpIdx);
                  Continue;
                end else begin
                  aLine := Trim(aLine);
                  sTmp := '';
                  lPos := Pos('(this)', aLine);
                  ObjectInfoDescRef := nil;
                  if lPos > 0 then begin
                    lPos2 := Pos(' := ', aLine);
                    if lPos2 > 0 then
                      Inc(lPos2, Length(' := '))
                    else
                      lPos2 := 1;
                    sTmp := Copy(aLine, lPos2, lPos-lPos2);
                    aObjToFind.ObjectName := sTmp;
                    if ModelList.Find(@aObjToFind, findIdx) then
                      ObjectInfoDescRef := ModelList.Get(findIdx);
                  end;
                  if aLine <> '' then begin
                    aLine := '  '+aLine;
                    if (sTmp <> '') then begin
                      if sTmp = 'IReferenceCountedImpl' then
                      Lines.Insert(tmpIdx, '{$IFDEF WITH_RECORD_METHODS}') else
                      Lines.Insert(tmpIdx, '{$IFDEF WITH_RECORD_METHODS}');
                      Inc(tmpIdx);
                      if (ObjectInfoDescRef <> nil) and (ObjectInfoDescRef.InheritesFrom <> '') then
                        stmp2 := ConcatVTable(ModelList, ObjectInfoDescRef.InheritesFrom)
                      else
                        stmp2 := '';
                      if sTmp2 <> '' then
                        sTmp := '(this.'+sTmp2 +'.SelfObject)'
                      else
                        sTmp := '(this.SelfObject)';
                      sTmp2 := StringReplace(aLine, '(this)', sTmp, []);
                      Lines.Insert(tmpIdx, sTmp2);
                      Inc(tmpIdx);
                      Lines.Insert(tmpIdx, '{$ELSE !WITH_RECORD_METHODS}');
                      Inc(tmpIdx);
                    end;
                    Lines[tmpIdx] := aLine;
                    if sTmp <> '' then begin
                      Inc(tmpIdx);
                      Lines.Insert(tmpIdx, '{$ENDIF !WITH_RECORD_METHODS}');
                      lIdx := tmpIdx;
                    end;
                  end;
                end;
              end;
              Inc(tmpIdx);
            end;
          end else begin
            lPos := Pos(' ', aLine);
            lPos2 := Pos('.', aLine);
            sTmp := Copy(aLine, lPos+1, lPos2-lPos-1);
            aObjToFind.ObjectName := sTmp;
            aBodyList.Clear;
            if ModelList.Find(@aObjToFind, findIdx) then begin
              ObjectInfoDescRef := ModelList.Get(findIdx);
              vTableNew := ConcatVTable(ModelList, aObjToFind.ObjectName)+'.vTable^';
              aVersionConstName := 'c'+aObjToFind.ObjectName+'_VERSION';
              Lines.Insert(lIdx, '{$IFDEF WITH_RECORD_METHODS}');
              Inc(lIdx);
              SaveIdx := lIdx;
              aLine := StringReplace(aLine, sTmp, ObjectInfoDescRef.ObjectNameNew, []);
              tmpIdx := lIdx;
              while tmpIdx < Cardinal(Lines.Count -1) do begin
                if aLine = 'end;' then begin
                  Lines.Insert(tmpIdx, '{$ENDIF !WITH_RECORD_METHODS}');
                  Break;
                end;
                if (Pos('checkException', aLine) > 0) then begin
                  Lines.Delete(tmpIdx);
                  aLine := Lines[tmpIdx];
                  Continue;
                end;
                lPos := Pos('(vTable.version < ', ALine);
                if lPos > 0 then begin
                  PC := Pointer(aLine);
                  lPos := lPos + Length('(vTable.version < ');
                  Inc(PC, lPos-1);
                  lPos2 := lPos;
                  while (PC^ <> ')') and (PC^ <> #0) do begin
                    Inc(PC);
                    Inc(lPos2);
                  end;
                  Delete(aLine, lPos, lpos2-lpos);
                  Insert(aVersionConstName, aLine, lPos);
                  Lines[tmpIdx] := aLine;
                  aLine := StringReplace(aLine, 'vTable', vTableNew, [rfReplaceAll]);
                end;
                lPos := Pos('FbException.', ALine);
                if lPos > 0 then begin
                  Delete(aLine, lPos, Length('FbException.'));
                  lPos := Pos('status, '#39, aLine);
                  if lPos > 0 then begin
                    PC := Pointer(ALine);
                    Inc(lPos, Length('status, '#39)-1);
                    lPos2 := lPos+1;
                    Inc(Pc, lPos2);
                    while (PC^ <> #39) and (PC^ <> #0) do begin
                      Inc(PC);
                      Inc(lPos2);
                    end;
                    if PC^ <> #0 then begin
                      sTmp := Copy(aLine, lPos+1, lPos2 - lPos);
                      Delete(aLine, lPos, lPos2 - lPos +2);
                      Insert('Pointer(s'+sTmp+')', aLine, lPos)
                    end;
                  end;
                  lPos := Pos('vTable.version, ', aLine);
                  if lPos > 0 then begin
                    PC := Pointer(ALine);
                    Inc(lPos, Length('vTable.version, '));
                    lPos2 := lPos;
                    Inc(Pc, lPos2-1);
                    while (PC^ <> ')') and (PC^ <> #0) do begin
                      Inc(PC);
                      Inc(lPos2);
                    end;
                    if PC^ <> #0 then begin
                      Delete(aLine, lPos, lPos2 - lPos);
                      Insert(aVersionConstName, aLine, lPos)
                    end;
                  end;
                  Lines[tmpIdx] := aLine;
                  aLine := StringReplace(aLine, 'vTable', vTableNew, [rfReplaceAll]);
                end;
                lPos := Pos('(vTable)', ALine);
                lPos2 := Pos(' := ', aLine);
                if lPos2 > 0 then
                  Inc(lPos2, 4)
                else lPos2 := 1;
                if (lPos > 0) then begin
                  sTmp := Copy(aLine, lPos2, lPos - lPos2);
                  sTmp := Trim(sTmp);
                  Lines[tmpIdx] := StringReplace(aLine, sTmp, 'T'+Stmp, []);
                  aLine := StringReplace(aLine, sTmp, 'P'+Stmp, []);
                  sTmp := ConcatVTable(ModelList, aObjToFind.ObjectName);
                  aLine := StringReplace(aLine, '(vTable)', '('+Stmp+'.vTable)^', []);
                  aLine := StringReplace(aLine, 'Self', '@'+Stmp+'.SelfOffsetPtr', []);
                end;
                aBodyList.Add(aLine);
                Inc(tmpIdx);
                aLine := Lines[tmpIdx];
              end;
              for lPos := 0 to aBodyList.Count-1 do begin
                aLine := aBodyList[lPos];
                Lines.Insert(SaveIdx, aLine);
                Inc(SaveIdx);
              end;
              Lines.Insert(SaveIdx, '{$ELSE !WITH_RECORD_METHODS}');
              aBodyList.Clear;
              lIdx := SaveIdx;
            end;
            while lIdx < Cardinal(Lines.Count -1) do begin
              aLine := Lines[lIdx];
              if aLine = 'end;' then
                Break;
              if (Pos('checkException', aLine) > 0) then begin
                Lines.Delete(lIdx);
                Continue;
              end;
              Inc(lIdx);
            end;
          end;
        end else if StartsWith(aLine, 'constructor') and
          (Pos('FbException.', aLine) = 0) then begin
          lPos := Pos('.', aLine);
          sTmp := Copy(aLine, 13, lPos -13);
          aObjToFind.ObjectName := sTmp;
          if (sTmp <> 'IVersionedImpl') and (sTmp <> 'IDisposableImpl') and (sTmp <> 'IReferenceCountedImpl') then begin
            sTmp2 := Copy(sTmp, 1, Length(sTmp)-4);
            Lines.Insert(lIdx, '{$IFDEF WITH_RECORD_METHODS}');
            Inc(lIdx);
            aLine := 'function '+sTmp+'.As'+sTmp2+': '+sTmp2+';';
            Lines.Insert(lIdx, aLine);
            Inc(lIdx);
            Lines.Insert(lIdx, 'begin');
            Inc(lIdx);
            Lines.Insert(lIdx, '  Result := '+sTmp2+'(@SelfOffsetPtr)');
            Inc(lIdx);
            Lines.Insert(lIdx, 'end;');
            Inc(lIdx);
            Lines.Insert(lIdx, '{$ENDIF !WITH_RECORD_METHODS}');
            Inc(lIdx);
            Lines.Insert(lIdx, '');
            Inc(lIdx);
          end;
          Inc(lIdx, 2); //skip begin
          aLine := Lines[lIdx];
          Lines.Delete(lIdx);
          if Pos('vTable', aLine) > 0  then begin
            sTmp2 := ConcatVTable(ModelList, aObjToFind.ObjectName);
            sTmp2 := StringReplace(aLine, 'vTable := ', 'vTable := @', []);
            Lines.Insert(lIdx, '{$IFDEF WITH_RECORD_METHODS}');
            Inc(lIdx);
            Lines.Insert(lIdx, '  SelfObject := Self;');
            Inc(lIdx);
            Lines.Insert(lIdx, sTmp2);
            Inc(lIdx);
            Lines.Insert(lIdx, '{$ELSE !WITH_RECORD_METHODS}');
            Inc(lIdx);
            Lines.Insert(lIdx, aLine);
            Inc(lIdx);
            Lines.Insert(lIdx, '{$ENDIF !WITH_RECORD_METHODS}');
            Inc(lIdx);
          end;
        end else if StartsWith(aLine, 'initialization') then begin
          Lines.Insert(lIdx, '{$IFNDEF WITH_RECORD_METHODS}');
          Inc(lIdx);
          Inc(lIdx);
          while lIdx < Cardinal(Lines.Count -1) do begin
            aLine := Lines[lIdx];
            lPos := Pos('.version := ', aLine);
            if (lPos > 0) then begin
              aLine := Copy(aLine, 1, lPos -1 + Length('.version := '));
              lPos2 := Pos('Impl_vTable', aLine);
              stmp := Copy(aLine, 1, lPos2-1);
              sTmp := Trim(sTmp);
              sTmp2 := 'c'+Copy(sTmp, 1, lPos2-1)+'_VERSION;';
              aLine := aLine+sTmp2;
              Lines[lIdx] := aLine;
            end else begin
              lPos := Pos('Impl_vTable := ', aLine);
              if (lPos > 0) then begin
                Insert('T', aLine, lPos+Length('Impl_vTable := '));
                Lines[lIdx] := aLine;
              end;
            end;
            Inc(lIdx);
          end;
        end else if StartsWith(aLine, 'var') then begin
          Inc(lIdx);
          Lines.Insert(lIdx, '{$IFDEF WITH_RECORD_METHODS}');
          Inc(lIdx);
          aLine := Lines[lIdx];
          lPos := Pos(':', aLine);
          lPos2 := Pos(';', aLine);
          sTmp := Copy(aLine, lPos+2, lPos2 - Lpos -2);
          sTmp := Trim(sTmp);
          lPos2 := Pos('VTable', sTmp);
          aLine := StringReplace(aLine, sTmp, 'T'+sTmp, []);
          Lines[lIdx] := aLine;
          sTmp2 := 'cI'+Copy(sTmp, 1, lPos2-1)+'_VERSION';
          sTmp := ' = ('+LineEnding+'    '+ConcatImplVar(ModelList, sTmp, sTmp2, '    ');
          sTmp2 := StringReplace(aLine, ';', sTmp, []);
          Lines.Insert(lIdx, sTmp2);
          Inc(lIdx);
          Lines.Insert(lIdx, '{$ELSE !WITH_RECORD_METHODS}');
          Inc(lIdx);
          Inc(lIdx);
          Lines.Insert(lIdx, '{$ENDIF !WITH_RECORD_METHODS}');
        end;
        if (Pos('class procedure FbException.', aLine) > 0) or
          (Pos('constructor FbException.', aLine) > 0) or
          (Pos('destructor FbException.', aLine) > 0) or
          (Pos('function FbException.', aLine) > 0)  then begin
          repeat
            aLine := Lines[lIdx];
            Lines.Delete(lIdx);
          until aLine = 'end;';
          Continue;
        end;
        Inc(lIdx);
      end;
      Lines.Insert(lIdx-1, '{$ENDIF !WITH_RECORD_METHODS}');
      Lines.Insert(TypeLineIdx, 'const');
      lIdx := TypeLineIdx+1;
      Lines.Capacity := Lines.Count+AConstList.Count;
      for lPos := 0 to AConstList.Count -1 do begin
        Lines.Insert(lIdx, '  '+AConstList[lPos]);
        Inc(lIdx);
      end;
      Lines.Insert(Lines.Count -1, '{$ENDIF ZEOS_DISABLE_FIREBIRD}');
      lIdx := 0;
      while lIdx < Cardinal(Lines.Count) -1 do begin
        aLine := Lines[lIdx];
        if (aLine = '') and (lIdx > 0) and (Lines[lidx-1] = '') then begin
          Lines.Delete(lIdx-1);
          Continue;
        end;

        if Pos(#9, aLine) > 0 then
          aLine := StringReplace(aLine, #9, '  ', [rfReplaceAll]);
        if Pos('BooleanPtr', aLine) > 0 then
          aLine := StringReplace(aLine, 'BooleanPtr', 'PBoolean', [rfReplaceAll]);
        if Pos('BytePtr', aLine) > 0 then
          aLine := StringReplace(aLine, 'BytePtr', 'PByte', [rfReplaceAll]);
        if Pos('CardinalPtr', aLine) > 0 then
          aLine := StringReplace(aLine, 'CardinalPtr', 'PCardinal', [rfReplaceAll]);
        if Pos('FB_DEC16Ptr', aLine) > 0 then
          aLine := StringReplace(aLine, 'FB_DEC16Ptr', 'PFB_DEC16', [rfReplaceAll]);
        if Pos('FB_DEC34Ptr', aLine) > 0 then
          aLine := StringReplace(aLine, 'FB_DEC34Ptr', 'PFB_DEC34', [rfReplaceAll]);
        if Pos('FB_I128Ptr', aLine) > 0 then
          aLine := StringReplace(aLine, 'FB_I128Ptr', 'PFB_I128', [rfReplaceAll]);
        if Pos('IKeyHolderPluginPtr', aLine) > 0 then
          aLine := StringReplace(aLine, 'IKeyHolderPluginPtr', 'PIKeyHolderPlugin', [rfReplaceAll]);
        if Pos('ISC_QUADPtr', aLine) > 0 then
          aLine := StringReplace(aLine, 'ISC_QUADPtr', 'PISC_QUAD', [rfReplaceAll]);
        if Pos('ISC_TIMESTAMP_TZPtr', aLine) > 0 then
          aLine := StringReplace(aLine, 'ISC_TIMESTAMP_TZPtr', 'PISC_TIMESTAMP_TZ', [rfReplaceAll]);
        if Pos('ISC_TIMESTAMP_TZ_EXPtr', aLine) > 0 then
          aLine := StringReplace(aLine, 'ISC_TIMESTAMP_TZ_EXPtr', 'PISC_TIMESTAMP_TZ_EX', [rfReplaceAll]);
        if Pos('ISC_TIME_TZPtr', aLine) > 0 then
          aLine := StringReplace(aLine, 'ISC_TIME_TZPtr', 'PISC_TIME_TZ', [rfReplaceAll]);
        if Pos('ISC_TIME_TZ_EXPtr', aLine) > 0 then
          aLine := StringReplace(aLine, 'ISC_TIME_TZ_EXPtr', 'PISC_TIME_TZ_EX', [rfReplaceAll]);
        if Pos('Int64Ptr', aLine) > 0 then
          aLine := StringReplace(aLine, 'Int64Ptr', 'PInt64', [rfReplaceAll]);
        if Pos('IntegerPtr', aLine) > 0 then
          aLine := StringReplace(aLine, 'IntegerPtr', 'PInteger', [rfReplaceAll]);
        if Pos('NativeIntPtr', aLine) > 0 then
          aLine := StringReplace(aLine, 'NativeIntPtr', 'PNativeInt', [rfReplaceAll]);
        if Pos('PerformanceInfoPtr', aLine) > 0 then
          aLine := StringReplace(aLine, 'PerformanceInfoPtr', 'PPerformanceInfo', [rfReplaceAll]);
        if Pos(' ISC_DATE;', aLine) > 0 then
          aLine := StringReplace(aLine, ' ISC_DATE;', ' TISC_DATE;', [rfReplaceAll]);
        if Pos(' ISC_TIME;', aLine) > 0 then
          aLine := StringReplace(aLine, ' ISC_TIME;', ' TISC_TIME;', [rfReplaceAll]);
        if Pos(' ISC_TIMESTAMP_TZ;', aLine) > 0 then
          aLine := StringReplace(aLine, ' ISC_TIMESTAMP_TZ;', ' TISC_TIMESTAMP_TZ;', [rfReplaceAll]);
        if Pos(' ISC_TIMESTAMP_TZ)', aLine) > 0 then
          aLine := StringReplace(aLine, ' ISC_TIMESTAMP_TZ)', ' TISC_TIMESTAMP_TZ)', [rfReplaceAll]);
        if Pos('dscPtr', aLine) > 0 then
          aLine := StringReplace(aLine, 'dscPtr', 'Pdsc', [rfReplaceAll]);
        if Pos('QWord', aLine) > 0 then
          aLine := StringReplace(aLine, 'QWord', 'UInt64', [rfReplaceAll]);
        Lines[lIdx] := aLine;
        Inc(lIdx);
      end;
      DecodeDate(now, y, m, d);
      Lines.Insert(0,  '{*********************************************************}');
      Lines.Insert(1,  '{                                                         }');
      Lines.Insert(2,  '{                 Zeos Database Objects                   }');
      Lines.Insert(3,  '{            Interfaces for Native Plain Drivers          }');
      Lines.Insert(4,  '{                                                         }');
      Lines.Insert(5,  '{          Originally distributed by firebird.org         }');
      Lines.Insert(6,  '{                  ported by EgonHugeist                  }');
      Lines.Insert(7,  '{                                                         }');
      Lines.Insert(8,  '{*********************************************************}');
      Lines.Insert(9,  '');
      Lines.Insert(10, '{@********************************************************}');
      Lines.Insert(11, '{    Copyright (c) 1999-'+IntToStr(y)+' Zeos Development Group       }');
      Lines.Insert(12, '{                                                         }');
      Lines.Insert(13, '{ License Agreement:                                      }');
      Lines.Insert(14, '{                                                         }');
      Lines.Insert(15, '{ This library is distributed in the hope that it will be }');
      Lines.Insert(16, '{ useful, but WITHOUT ANY WARRANTY; without even the      }');
      Lines.Insert(17, '{ implied warranty of MERCHANTABILITY or FITNESS FOR      }');
      Lines.Insert(18, '{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }');
      Lines.Insert(19, '{ Public License for more details.                        }');
      Lines.Insert(20, '{                                                         }');
      Lines.Insert(21, '{ The source code of the ZEOS Libraries and packages are  }');
      Lines.Insert(22, '{ distributed under the Library GNU General Public        }');
      Lines.Insert(23, '{ License (see the file COPYING / COPYING.ZEOS)           }');
      Lines.Insert(24, '{ with the following  modification:                       }');
      Lines.Insert(25, '{ As a special exception, the copyright holders of this   }');
      Lines.Insert(26, '{ library give you permission to link this library with   }');
      Lines.Insert(27, '{ independent modules to produce an executable,           }');
      Lines.Insert(28, '{ regardless of the license terms of these independent    }');
      Lines.Insert(29, '{ modules, and to copy and distribute the resulting       }');
      Lines.Insert(30, '{ executable under terms of your choice, provided that    }');
      Lines.Insert(31, '{ you also meet, for each linked independent module,      }');
      Lines.Insert(32, '{ the terms and conditions of the license of that module. }');
      Lines.Insert(33, '{ An independent module is a module which is not derived  }');
      Lines.Insert(34, '{ from or based on this library. If you modify this       }');
      Lines.Insert(35, '{ library, you may extend this exception to your version  }');
      Lines.Insert(36, '{ of the library, but you are not obligated to do so.     }');
      Lines.Insert(37, '{ If you do not wish to do so, delete this exception      }');
      Lines.Insert(38, '{ statement from your version.                            }');
      Lines.Insert(39, '{                                                         }');
      Lines.Insert(40, '{                                                         }');
      Lines.Insert(41, '{ The project web site is located on:                     }');
      Lines.Insert(42, '{   https://zeoslib.sourceforge.io/ (FORUM)               }');
      Lines.Insert(43, '{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}');
      Lines.Insert(44, '{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }');
      Lines.Insert(45, '{                                                         }');
      Lines.Insert(46, '{   http://www.sourceforge.net/projects/zeoslib.          }');
      Lines.Insert(47, '{                                                         }');
      Lines.Insert(48, '{                                                         }');
      Lines.Insert(49, '{                                 Zeos Development Group. }');
      Lines.Insert(50, '{********************************************************@}');
      Lines.Insert(51, '');
      Lines.Insert(52, '{ the original file shipped by Firebird 3.0 and up');
      Lines.Insert(53, 'the file firebird.pas as been  formattet by program "FirebirdPasToZPlainFirebirdDriverPas"');
      Lines.Insert(54, '1. to get it running with FPC <= v3: add the "INTERFACES CORBA" macro');
      Lines.Insert(55, 'and add "OBJECTCHECKS OFF"');
      Lines.Insert(56, '2. to get it running with D7..D2007, FPC2:');
      Lines.Insert(57, 'comment all error checks original like FbException.checkException(status);');
      Lines.Insert(58, 'and stricly follow step 3');
      Lines.Insert(59, 'Those compiler (including FPC < v3) do not support class consts so we add');
      Lines.Insert(60, 'replacements.');
      Lines.Insert(61, '3. genaral: comment all try .. except block they leave ugly warnings,');
      Lines.Insert(62, 'slowing down the code executable code and finally D7 crash an each call');
      Lines.Insert(63, 'with multiple parameters.');
      Lines.Insert(64, 'So we do the error checks @all');
      Lines.Insert(65, '4. comment the FBException. The error details are not interpreted..');
      Lines.Insert(66, '  Error handling happens higher up!');
      Lines.Insert(67, '5. to avoid overdozed rtti, add the "public" and "protected" keywords to all');
      Lines.Insert(68, '  VTable and Impl(dispatcher) objects');
      Lines.Insert(69, '6. Make a string replace : QWord -> UInt64 etc');
      Lines.Insert(70, '7. Since the fpc did decide to add private fields to the TObject, we had to rework');
      Lines.Insert(71, '  everything again: use records insteaed of TObjects.');
      Lines.Insert(72, '  See FPC-BugTracker: https://gitlab.com/freepascal.org/fpc/source/-/issues/40810');
      Lines.Insert(73, '  and Zeos-BugTracker: https://sourceforge.net/p/zeoslib/tickets/603/');
      Lines.Insert(74, '  This also works since D2010.');
      Lines.Insert(75, '}');
      Lines.SaveToFile('ZPlainFirebird_new.pas');
    finally
      FreeAndNil(Lines);
      FreeAndNil(ModelList);
      FreeAndNil(AConstList);
      FreeAndNil(aBodyList);
      FreeAndNil(aIntfNameList);
    end;
  end;

end;

{ TObjectInfoDescList }

function KeyCompare(P1, P2: Pointer): Integer;
begin
  Result := StrComp(PChar(PObjectInfoDesc(P1).ObjectName), PChar(PObjectInfoDesc(P2).ObjectName));
end;

constructor TObjectInfoDescList.Create;
begin
  inherited Create(@KeyCompare, SizeOf(TObjectInfoDesc), True);
end;

procedure TObjectInfoDescList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    PObjectInfoDesc(Ptr)^.ObjectName := '';
    PObjectInfoDesc(Ptr)^.ObjectNameNew := '';
    PObjectInfoDesc(Ptr)^.InheritesFrom := '';
    PObjectInfoDesc(Ptr)^.InheritesFromNew := '';
    if PObjectInfoDesc(Ptr)^.HelperList <> nil then
      FreeAndNil(PObjectInfoDesc(Ptr)^.HelperList);
  end;
end;

begin
  try
    { TODO -oUser -cConsole Main : Code hier einfügen }
    FirebirdPasToZPlainFirebird('firebird.pas')
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
