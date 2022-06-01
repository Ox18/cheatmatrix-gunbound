{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

unit lvkDataSetToExcel;

// $Author: Lasse V. Karlsen $
// $Revision: 10 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkDataSetToExcel.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
{$IFDEF DELPHI7UP}
  Variants, ExcelXP,
{$ELSE}
  {$IFDEF DELPHI6}
    Variants, Excel2000,
  {$ELSE}
    Excel97,
  {$ENDIF}
{$ENDIF}
  OleServer, ComObj, ActiveX,
  Dialogs, SysUtils, Classes, DB, TypInfo, lvkComponents;

type
{$IFDEF DELPHI7UP}
  Range = ExcelRange;
{$ENDIF}
  TTemplate = record
    Name  : string;
    Line  : Range;
  end;
  TTemplateList = array of TTemplate;

  TlvkCustomDataSetToExcel = class;
  TProgressEvent = procedure(const Sender: TlvkCustomDataSetToExcel; const RecordNumber: Integer) of object;
  TExportType = (etSimple, etTemplateBased);

  TlvkCustomDataSetToExcel = class(TlvkComponent)
  private
    FDataSet          : TDataSet;
    FTemplateFilename : string;
    FTemplateNames    : TStrings;
    FStartLine        : string;
    FEndLine          : string;
    FOnProgress       : TProgressEvent;
    FExportType       : TExportType;

    FExcelApplication : TExcelApplication;
    FSheet            : ExcelWorksheet;

    procedure SetTemplateNames(const Value: TStrings);
    procedure LocateTemplates(var StartRange, EndRange: Range; var Templates: TTemplateList);
    procedure CopyValuesFromDataSet(const TemplateName: string;
      const TemplateRange, DataRange: Range);
    procedure DoProgress(const RecordNumber: Integer);
    procedure ConnectToExcel;
    procedure DisconnectFromExcel;
    procedure ExportTemplateBased;
    procedure ExportSimple;
    function ExpandTemplateFilename: string;
    function FieldToValue(const Field: TField): OleVariant;

  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    property DataSet: TDataSet read FDataSet write FDataSet;
    property TemplateFilename: string read FTemplateFilename
      write FTemplateFilename;
    property StartLine: string read FStartLine write FStartLine;
    property EndLine: string read FEndLine write FEndLine;
    property TemplateNames: TStrings read FTemplateNames
      write SetTemplateNames;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property ExportType: TExportType read FExportType write FExportType
      default etSimple;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Execute;
  end;

  TlvkDataSetToExcel = class(TlvkCustomDataSetToExcel)
  published
    property DataSet;
    property TemplateFilename;
    property TemplateNames;
    property StartLine;
    property EndLine;
    property OnProgress;
    property ExportType;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

implementation

const
  xlUp      = -4162;
  xlDown    = -4121;
  xlValues  = -4163;
  xlWhole   = 1;
  xlByRows  = 1;
  xlNext    = 1;

{$IFNDEF DELPHI6UP}
function VarTypeIsStr(const AVarType: Word): Boolean;
begin
  Result := (AVarType = varOleStr) or (AVarType = varString);
end;

function VarIsStr(const V: Variant): Boolean;
begin
  Result := VarTypeIsStr(TVarData(V).VType);
end;
{$ENDIF}

{ TlvkCustomDataSetToExcel }

procedure TlvkCustomDataSetToExcel.ConnectToExcel;
begin
  Assert(not Assigned(FExcelApplication));

  case FExportType of
    etSimple:
      begin
        // Connect to excel, open new spreadsheet

        FExcelApplication := TExcelApplication.Create(Self);
        FExcelApplication.ConnectKind := ckNewInstance;
        FExcelApplication.Connect;
        FExcelApplication.Interactive[0] := True;
        FExcelApplication.ScreenUpdating[0] := False;
        FExcelApplication.Workbooks.Add(EmptyParam, 0);
      end;

    etTemplateBased:
      if FTemplateFilename <> '' then
      begin
        // Connect to excel, open template
        FExcelApplication := TExcelApplication.Create(Self);
        FExcelApplication.ConnectKind := ckNewInstance;
        FExcelApplication.Connect;
        FExcelApplication.Interactive[0] := True;
        FExcelApplication.ScreenUpdating[0] := False;

        {$IFDEF DELPHI7UP}
        FExcelApplication.Workbooks.Open(ExpandTemplateFilename, EmptyParam,
          EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
          EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
          EmptyParam, 0);
        {$ELSE}
        FExcelApplication.Workbooks.Open(ExpandTemplateFilename, EmptyParam,
          EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
          EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, 0);
        {$ENDIF}
      end else begin
        // Connect to excel, use active worksheet
        FExcelApplication := TExcelApplication.Create(Self);
        FExcelApplication.ConnectKind := ckRunningInstance;
        FExcelApplication.Connect;
        FExcelApplication.ScreenUpdating[0] := False;
      end;
  end;

  FSheet := FExcelApplication.ActiveSheet as ExcelWorksheet;
  FExcelApplication.AutoQuit := False;
end;

procedure TlvkCustomDataSetToExcel.CopyValuesFromDataSet(
  const TemplateName: string; const TemplateRange, DataRange: Range);
var
  FieldName : OleVariant;
  Field     : TField;
  Column    : Integer;
begin
  Column := 1;

  repeat
    FieldName := TemplateRange.Cells.Item[1, Column].Value;

    if VarIsStr(FieldName) then
    begin
      if CompareText(FieldName, TemplateName) = 0 then
        Break;

      Field := FDataSet.Fields.FindField(FieldName);
      if Assigned(Field) then
        DataRange.Cells.Item[1, Column].Value := Field.Value;
    end else
      Break;

    Inc(Column);
  until False;
end;

constructor TlvkCustomDataSetToExcel.Create(AOwner: TComponent);
begin
  inherited;

  FExportType := etSimple;
  FTemplateNames := TStringList.Create;
  FTemplateNames.Add('$TEMPLATE');
  FTemplateNames.Add('$TEMPLATE1');
  FTemplateNames.Add('$TEMPLATE2');
  FTemplateNames.Add('$TEMPLATE3');
  FTemplateNames.Add('$TEMPLATE4');
  FStartLine := '$START';
  FEndLine := '$END';
end;

destructor TlvkCustomDataSetToExcel.Destroy;
begin
  FTemplateNames.Free;

  inherited;
end;

procedure TlvkCustomDataSetToExcel.DisconnectFromExcel;
begin
  Assert(Assigned(FExcelApplication));

  try
    FSheet := nil;
    FExcelApplication.Visible[0] := True;
    FExcelApplication.ScreenUpdating[0] := True;
    FExcelApplication.Disconnect;
  finally
    FreeAndNil(FExcelApplication);
  end;
end;

procedure TlvkCustomDataSetToExcel.DoProgress(const RecordNumber: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, RecordNumber);
end;

procedure TlvkCustomDataSetToExcel.Execute;
begin
  Assert(Assigned(FDataSet), 'No dataset available');
  Assert(FDataSet.Active, 'Dataset is not active');
  Assert(FTemplateNames.Count > 0, 'No template names defined');
  Assert(FStartLine <> '', 'No start line defined');
  Assert(FEndLine <> '', 'No end line defined');

  ConnectToExcel;
  try
    case FExportType of
      etSimple:
        ExportSimple;

      etTemplateBased:
        ExportTemplateBased;

    else
      raise Exception.Create('Internal error');
    end;
  finally
    DisconnectFromExcel;
  end;
end;

function TlvkCustomDataSetToExcel.ExpandTemplateFilename: string;
begin
  Result := FTemplateFilename;
  Result := StringReplace(Result, '$APPDIR', ExcludeTrailingBackslash(ExtractFilePath(ParamStr(0))), [rfReplaceAll]);
end;

procedure TlvkCustomDataSetToExcel.ExportSimple;
var
  Index   : Integer;
  Row     : Integer;
  Column  : Integer;
begin
  FDataSet.First;

  Column := 1;
  for Index := 0 to FDataSet.Fields.Count-1 do
    if FDataSet.Fields[Index].Visible then
    begin
      if FDataSet.Fields[Index].DisplayLabel <> '' then
        FSheet.Cells.Item[1, Column].Value := FDataSet.Fields[Index].DisplayLabel
      else
        FSheet.Cells.Item[1, Column].Value := FDataSet.Fields[Index].FieldName;
      Inc(Column);
    end;

  Row := 2;
  while not FDataSet.Eof do
  begin
    Column := 1;
    for Index := 0 to FDataSet.Fields.Count-1 do
      if FDataSet.Fields[Index].Visible then
      begin
        FSheet.Cells.Item[Row, Column].Value := FieldToValue(FDataSet.Fields[Index]);
        Inc(Column);
      end;

    DoProgress(Row-1);
    FDataSet.Next;
    Inc(Row);
  end;
end;

procedure TlvkCustomDataSetToExcel.ExportTemplateBased;
var
  Templates     : TTemplateList;
  StartRange    : Range;
  EndRange      : Range;
  NewLine       : Range;
  TemplateNo    : Integer;
  Index         : Integer;
  RecordNumber  : Integer;
begin
  LocateTemplates(StartRange, EndRange, Templates);
  Assert(Length(Templates)>0, 'No templates found in spreadsheet');
  Assert(Assigned(StartRange), 'No start line found in spreadsheet');
  Assert(Assigned(EndRange), 'No end line found in spreadsheet');

  FDataSet.First;
  TemplateNo := 0;
  RecordNumber := 0;
  while not FDataSet.Eof do
  begin
    {$IFDEF DELPHI7UP}
    EndRange.EntireRow.Insert(xlDown, EmptyParam);
    {$ELSE}
    EndRange.EntireRow.Insert(xlDown);
    {$ENDIF}
    NewLine := EndRange.Offset[-1, 0].EntireRow;
    Templates[TemplateNo].Line.Offset[-1, 0].EntireRow.Copy(NewLine);

    CopyValuesFromDataSet(Templates[TemplateNo].Name,
      Templates[TemplateNo].Line.EntireRow,
      NewLine);

    Inc(RecordNumber);
    DoProgress(RecordNumber);
    FDataSet.Next;
    TemplateNo := (TemplateNo + 1) mod Length(Templates);
  end;

  StartRange.EntireRow.Delete(xlUp);
  EndRange.EntireRow.Delete(xlUp);

  for Index := Low(Templates) to High(Templates) do
  begin
    Templates[Index].Line.Offset[-1, 0].EntireRow.Delete(xlUp);
    Templates[Index].Line.EntireRow.Delete(xlUp);
  end;
end;

function TlvkCustomDataSetToExcel.FieldToValue(
  const Field: TField): OleVariant;
begin
  Result := Unassigned;
  case Field.DataType of
    ftADT,
    ftArray,
    ftBlob,
    ftBytes,
    ftCursor,
    ftDataSet,
    ftDBaseOle,
{$IFDEF DELPHI6UP}
    ftFMTBcd,
{$ENDIF}
    ftGraphic,
    ftIDispatch,
    ftInterface,
    ftOraBlob,
    ftOraClob,
    ftParadoxOle,
    ftReference,
{$IFDEF DELPHI6UP}
    ftTimeStamp,
{$ENDIF}
    ftTypedBinary,
    ftUnknown,
    ftVarBytes:
      Result := Format('(%s)', [GetEnumName(TypeInfo(TFieldType), Ord(Field.DataType))]);

    ftFixedChar,
    ftFmtMemo,
    ftGuid,
    ftMemo,
    ftString,
    ftWideString:
      Result := Field.AsString;

    ftAutoInc,
    ftInteger,
    ftLargeint,
    ftWord,
    ftSmallint:
      Result := Field.AsInteger;

    ftBoolean:
      Result := Field.AsBoolean;

    ftFloat, ftCurrency, ftBCD:
      Result := Field.AsFloat;

    ftDate, ftTime, ftDateTime:
      Result := Field.AsDateTime;

    ftVariant:
      Result := Field.AsVariant;
  end;
end;

procedure TlvkCustomDataSetToExcel.LocateTemplates(
  var StartRange, EndRange: Range; var Templates: TTemplateList);
var
  Index : Integer;
  Found : Range;

  function Find(const Name: string): Range;
  var
    Temp  : OleVariant;
    Cells : OleVariant;
  begin
    // Workaround to get around problem with different parameter lists for
    // office 97 and 2000
    Cells := FSheet.Cells;
    Temp := Cells.Find(Name, EmptyParam, xlValues, xlWhole, xlByRows, xlNext); // , EmptyParam, EmptyParam{$IFNDEF DELPHI6UP}, EmptyParam, EmptyParam, EmptyParam, EmptyParam{$ENDIF});
    Assert(TVarData(Temp).VType = varDispatch);
    Result := IDispatch(TVarData(Temp).VDispatch) as Range;
  end;

begin
  Assert(Assigned(FExcelApplication));

  SetLength(Templates, 0);

  StartRange := Find(FStartLine);
  if Assigned(StartRange) then
    StartRange := StartRange.EntireRow.Cells;
  EndRange := Find(FEndLine);
  if Assigned(EndRange) then
    EndRange := EndRange.EntireRow.Cells;

  for Index := 0 to FTemplateNames.Count-1 do
  begin
    Found := Find(FTemplateNames[Index]);
    if Assigned(Found) then
    begin
      SetLength(Templates, Length(Templates)+1);
      Templates[High(Templates)].Name := FTemplateNames[Index];
      Templates[High(Templates)].Line := Found;
    end;
  end;
end;

procedure TlvkCustomDataSetToExcel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FDataSet then
      FDataSet := nil;
  end;
end;

procedure TlvkCustomDataSetToExcel.SetTemplateNames(const Value: TStrings);
begin
  if Assigned(Value) then
    FTemplateNames.Assign(Value)
  else
    FTemplateNames.Clear;
end;

end.
