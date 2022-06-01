{ TODO 2 -oLVK -cDocumentation : Document this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains ADO-derived components for simplified use of ADO.
}
unit lvkADO;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 17.04.03 16:07 $
// $Archive: /Components/LVK/Source/lvkADO.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  ADODB_TLB, SysUtils, Classes,
  lvkRTTIDispatch,
  lvkVersion, lvkComponents;

type
  TCommandType = (ctUnspecified, ctUnknown, ctText, ctTable, ctStoredProc,
    ctFile, ctTableDirect);

  TFieldNameList = array of string;
  TFieldValueList = array of OleVariant;

  IlvkADORecordset = interface;

  IlvkADOConnection = interface
    ['{50DE22A8-EB7C-43A1-8E05-A2612E304420}']

    function GetConnection: ADODB_TLB.Connection;
    property Connection: ADODB_TLB.Connection read GetConnection;

    function BeginTransaction: Integer;
    procedure CommitTransaction;
    procedure RollbackTransaction;

    procedure ExecSQL(const CommandText: string; const CommandType: TCommandType=ctText); overload;
    procedure ExecSQL(const CommandText: string; const Parameters: array of const; const CommandType: TCommandType=ctText); overload;
    procedure ExecSQL(const CommandText: string; const Parameters: array of const; const ParameterNames: array of string; const CommandType: TCommandType=ctText); overload;

    procedure ExecSQL2(const CommandText: string; const Parameters: array of OleVariant; const CommandType: TCommandType=ctText); overload;
    procedure ExecSQL2(const CommandText: string; const Parameters: array of OleVariant; const ParameterNames: array of string; const CommandType: TCommandType=ctText); overload;

    function GetRecordset(const CommandText: string; const CommandType: TCommandType=ctText): IlvkADORecordset; overload;
    function GetRecordset(const CommandText: string; const Parameters: array of const; const CommandType: TCommandType=ctText): IlvkADORecordset; overload;
    function GetRecordset(const CommandText: string; const Parameters: array of const; const ParameterNames: array of string; const CommandType: TCommandType=ctText): IlvkADORecordset; overload;

    function GetRecordset2(const CommandText: string; const Parameters: array of OleVariant; const CommandType: TCommandType=ctText): IlvkADORecordset; overload;
    function GetRecordset2(const CommandText: string; const Parameters: array of OleVariant; const ParameterNames: array of string; const CommandType: TCommandType=ctText): IlvkADORecordset; overload;

    function GetEmptyRecordset(const TableName: string): IlvkADORecordset;

    procedure Save(const Recordset: IlvkADORecordset;
      const AutoHandleConflicts: Boolean=False); overload;
    procedure Save(const Recordsets: array of IlvkADORecordset;
      const AutoHandleConflicts: Boolean=False); overload;

    procedure Requery(const Recordset: IlvkADORecordset); overload;
    procedure Requery(const Recordsets: array of IlvkADORecordset); overload;

    procedure Resync(const Recordset: IlvkADORecordset); overload;
    procedure Resync(const Recordsets: array of IlvkADORecordset); overload;
  end;

  TRecordsetSaveFormat = (rsfADTG, rsfXML);
  TFindSearchDirection = (fsdForward, fsdBackward);
  TFilterType = (ftCriteria, ftAffectedRecords, ftConflictingRecords,
    ftFetchedRecords, ftPendingRecords);

  IlvkADORecordset = interface
    ['{3C5D43B7-3F2F-4834-9CA7-3C2F1CFA9191}']

    function GetRecordset: ADODB_TLB._Recordset;
    property Recordset: ADODB_TLB._Recordset read GetRecordset;

    procedure Save(const Connection: IlvkADOConnection;
      const AutoHandleConflicts: Boolean=False); overload;
    procedure Requery(const Connection: IlvkADOConnection); overload;
    procedure Resync(const Connection: IlvkADOConnection); overload;

    procedure Append(const FieldNames: string; const Values: OleVariant); overload;
    procedure Append(const FieldNames: array of string;
      const Values: array of OleVariant); overload;
    procedure Append; overload;

    procedure Update(const FieldNames: string; const Values: OleVariant); overload;
    procedure Update(const FieldNames: array of string;
      const Values: array of OleVariant); overload;
    procedure Update; overload;

    procedure Delete;
    procedure Clear;

    function GetBOF: Boolean;
    property BOF: Boolean read GetBOF;
    function GetEOF: Boolean;
    property EOF: Boolean read GetEOF;

    function GetBookmark: OleVariant;
    procedure SetBookmark(const Value: OleVariant);
    property Bookmark: OleVariant read GetBookmark write SetBookmark;

    function GetRecordCount: Integer;
    property RecordCount: Integer read GetRecordCount;

    function GetFilter: WideString;
    procedure SetFilter(const Value: WideString);
    property Filter: WideString read GetFilter write SetFilter;

    function GetFilterEnabled: Boolean;
    procedure SetFilterEnabled(const Value: Boolean);
    property FilterEnabled: Boolean read GetFilterEnabled write SetFilterEnabled;
    function GetFilterType: TFilterType;
    procedure SetFilterType(const Value: TFilterType);
    property FilterType: TFilterType read GetFilterType write SetFilterType;

    function GetSortOrder: WideString;
    procedure SetSortOrder(const Value: WideString);
    property SortOrder: WideString read GetSortOrder write SetSortOrder;

    procedure CancelBatch;
    procedure CancelUpdate;

    procedure First;
    procedure Last;
    procedure Next;
    procedure Previous;

    function Clone(const ReadonlyLock: Boolean=False;
      const CloneFilter: Boolean=True): IlvkADORecordset;

    function FindFrom(const Criteria: string; const StartBookmark: OleVariant;
      const SearchDirection: TFindSearchDirection): Boolean; overload;
    function Find(const Criteria: string='';
      const SearchDirection: TFindSearchDirection=fsdForward): Boolean; overload;

    function FindFirst(const Criteria: string=''): Boolean;
    function FindNext(const Criteria: string=''): Boolean;
    function FindPrevious(const Criteria: string=''): Boolean;
    function FindLast(const Criteria: string=''): Boolean;

    function GetIsEmpty: Boolean;
    property IsEmpty: Boolean read GetIsEmpty;

    procedure SaveToFile(const FileName: string; const SaveFormat: TRecordsetSaveFormat=rsfXML);
    procedure SaveToStream(const Stream: TStream; const SaveFormat: TRecordsetSaveFormat=rsfXML);

    function GetFieldValues(const FieldName: string): OleVariant;
    procedure SetFieldValues(const FieldName: string; const Value: OleVariant);
    property FieldValues[const FieldName: string]: OleVariant
      read GetFieldValues write SetFieldValues; default;

    function GetValues: OleVariant;
    property Values: OleVariant read GetValues;

    function GetOriginalValues: OleVariant;
    property OriginalValues: OleVariant read GetOriginalValues;

    function GetUnderlyingValues: OleVariant;
    property UnderlyingValues: OleVariant read GetUnderlyingValues;

    function GetChildRecordset(const ChildName: string): IlvkADORecordset;
  end;

  ElvkADO = class(Exception);
  ElvkADOConnection = class(ElvkADO);
  ElvkADORecordset = class(ElvkADO);
  ElvkADORecordsetConflict = class(ElvkADORecordset);
  ElvkADORecordsetNoChild = class(ElvkADORecordset);

function NewConnection(const ConnectionString: string): IlvkADOConnection; overload;
function NewConnection(const ConnectionString: string;
  const Username, Password: WideString): IlvkADOConnection; overload;
function NewConnection(const IniFileName, SectionName: string): IlvkADOConnection; overload;

function NewRecordset(const RecordSet: ADODB_TLB._Recordset): IlvkADORecordset; overload;
function NewRecordset(const FileName: string): IlvkADORecordset; overload;
function NewRecordset(const Stream: TStream): IlvkADORecordset; overload;

function NewGUID: TGUID;
function NewGUIDAsString: string;

implementation

uses
{$IFDEF DELPHI6UP}
  Variants,
{$ENDIF}
  ComObj, Windows, IniFiles, ActiveX;

const
  PersistFormats  : array[TRecordsetSaveFormat] of TOleEnum = (
    adPersistADTG,
    adPersistXML
  );

  FIELDNAME_BASE  = 1000;
  FIELDNAME_END   = FIELDNAME_BASE + 999;

  E_ROW_UPDATE_CONFLICT = Integer($80040E38);

type
  TlvkADOConnection = class(TInterfacedObject, IlvkADOConnection)
  private
    FConnection : ADODB_TLB.Connection;

    function InternalExecute(const CommandText: string;
      const Parameters: array of const;
      const ParameterNames: array of string;
      const CommandType: TCommandType;
      const ResultSet: Boolean;
      out RecordsAffected: OleVariant): IlvkADORecordset; overload;
    function InternalExecute(const CommandText: string;
      const Parameters: array of OleVariant;
      const ParameterNames: array of string;
      const CommandType: TCommandType;
      const ResultSet: Boolean;
      out RecordsAffected: OleVariant): IlvkADORecordset; overload;

  protected
    // IlvkADOConnection interface
    function GetConnection: ADODB_TLB.Connection;
    function BeginTransaction: Integer;
    procedure CommitTransaction;
    procedure RollbackTransaction;

    procedure ExecSQL(const CommandText: string; const CommandType: TCommandType=ctText); overload;
    procedure ExecSQL(const CommandText: string; const Parameters: array of const; const CommandType: TCommandType=ctText); overload;
    procedure ExecSQL(const CommandText: string; const Parameters: array of const; const ParameterNames: array of string; const CommandType: TCommandType=ctText); overload;

    procedure ExecSQL2(const CommandText: string; const Parameters: array of OleVariant; const CommandType: TCommandType=ctText); overload;
    procedure ExecSQL2(const CommandText: string; const Parameters: array of OleVariant; const ParameterNames: array of string; const CommandType: TCommandType=ctText); overload;

    function GetRecordset(const CommandText: string; const CommandType: TCommandType=ctText): IlvkADORecordset; overload;
    function GetRecordset(const CommandText: string; const Parameters: array of const; const CommandType: TCommandType=ctText): IlvkADORecordset; overload;
    function GetRecordset(const CommandText: string; const Parameters: array of const; const ParameterNames: array of string; const CommandType: TCommandType=ctText): IlvkADORecordset; overload;

    function GetRecordset2(const CommandText: string; const Parameters: array of OleVariant; const CommandType: TCommandType=ctText): IlvkADORecordset; overload;
    function GetRecordset2(const CommandText: string; const Parameters: array of OleVariant; const ParameterNames: array of string; const CommandType: TCommandType=ctText): IlvkADORecordset; overload;

    function GetEmptyRecordset(const TableName: string): IlvkADORecordset;

    procedure Save(const Recordset: IlvkADORecordset;
      const AutoHandleConflicts: Boolean); overload;
    procedure Save(const Recordsets: array of IlvkADORecordset;
      const AutoHandleConflicts: Boolean); overload;

    procedure Requery(const Recordset: IlvkADORecordset); overload;
    procedure Requery(const Recordsets: array of IlvkADORecordset); overload;

    procedure Resync(const Recordset: IlvkADORecordset); overload;
    procedure Resync(const Recordsets: array of IlvkADORecordset); overload;

  public
    constructor Create(const ConnectionString: string); overload;
    constructor Create(const ConnectionString: string;
      const Username, Password: WideString); overload;
    constructor Create(const IniFileName, SectionName: string); overload;
  end;

  TlvkADORecordset = class;
  TWhichValue = (wvValue, wvOriginalValue, wvUnderlyingValue);

  TValueObject = class(TInterfacedObject, IDispatch)
  private
    FRecordset    : TlvkADORecordset;
    FWhichValue   : TWhichValue;

  protected
    // IDispatch interface
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;

  public
    constructor Create(const Recordset: TlvkADORecordset;
      const WhichValue: TWhichValue);
  end;

  TlvkADORecordset = class(TInterfacedObject, IlvkADORecordset)
  private
    FRecordset        : ADODB_TLB._Recordset;
    FFilter           : string;
    FFilterEnabled    : Boolean;
    FLastCriteria     : string;
    FFieldNames       : TStringList;
    FChapterNames     : TStringList;
    FValues           : OleVariant;
    FUnderlyingValues : OleVariant;
    FOriginalValues   : OleVariant;
    FFilterType       : TFilterType;

    function InternalFind(const Criteria: string;
      const StartBookmark: OleVariant;
      const SearchDirection: TFindSearchDirection): Boolean;
    procedure AdjustFilter;

  protected
    // IlvkADORecordset interface
    function GetRecordset: ADODB_TLB._Recordset;
    procedure Save(const Connection: IlvkADOConnection;
      const AutoHandleConflicts: Boolean=False); overload;
    procedure Requery(const Connection: IlvkADOConnection); overload;
    procedure Resync(const Connection: IlvkADOConnection); overload;

    procedure Append(const FieldNames: string; const Values: OleVariant); overload;
    procedure Append(const FieldNames: array of string;
      const Values: array of OleVariant); overload;
    procedure Append; overload;

    procedure Update(const FieldNames: string; const Values: OleVariant); overload;
    procedure Update(const FieldNames: array of string;
      const Values: array of OleVariant); overload;
    procedure Update; overload;

    procedure Delete;
    procedure Clear;
    function GetBOF: Boolean;
    function GetEOF: Boolean;
    function GetBookmark: OleVariant;
    procedure SetBookmark(const Value: OleVariant);
    function GetRecordCount: Integer;
    function GetFilter: WideString;
    procedure SetFilter(const Value: WideString);
    function GetFilterEnabled: Boolean;
    procedure SetFilterEnabled(const Value: Boolean);
    function GetFilterType: TFilterType;
    procedure SetFilterType(const Value: TFilterType);
    procedure CancelBatch;
    procedure CancelUpdate;
    procedure First;
    procedure Last;
    procedure Next;
    procedure Previous;
    function Clone(const ReadonlyLock, CloneFilter: Boolean): IlvkADORecordset;
    function GetIsEmpty: Boolean;
    procedure SaveToFile(const FileName: string; const SaveFormat: TRecordsetSaveFormat);
    procedure SaveToStream(const Stream: TStream; const SaveFormat: TRecordsetSaveFormat);
    function GetFieldValues(const FieldName: string): OleVariant;
    procedure SetFieldValues(const FieldName: string; const Value: OleVariant);
    function FindFrom(const Criteria: string; const StartBookmark: OleVariant;
      const SearchDirection: TFindSearchDirection): Boolean; overload;
    function Find(const Criteria: string;
      const SearchDirection: TFindSearchDirection=fsdForward): Boolean; overload;
    function FindFirst(const Criteria: string=''): Boolean;
    function FindNext(const Criteria: string=''): Boolean;
    function FindPrevious(const Criteria: string=''): Boolean;
    function FindLast(const Criteria: string=''): Boolean;
    function GetSortOrder: WideString;
    procedure SetSortOrder(const Value: WideString);
    function GetValues: OleVariant;
    function GetUnderlyingValues: OleVariant;
    function GetOriginalValues: OleVariant;
    function GetChildRecordset(const ChildName: string): IlvkADORecordset;

  public
    constructor Create(const Recordset: ADODB_TLB._Recordset); overload;
    constructor Create(const FileName: string); overload;
    constructor Create(const Stream: TStream); overload;
    destructor Destroy; override;
  end;

function NewGUID: TGUID;
begin
  CoCreateGUID(Result);
end;

function NewGUIDAsString: string;
begin
  Result := GUIDToString(NewGUID);
end;

function NewConnection(const ConnectionString: string): IlvkADOConnection;
begin
  Result := TlvkADOConnection.Create(ConnectionString);
end;

function NewConnection(const ConnectionString: string;
  const Username, Password: WideString): IlvkADOConnection;
begin
  Result := TlvkADOConnection.Create(ConnectionString, Username, Password);
end;

function NewConnection(const IniFileName, SectionName: string): IlvkADOConnection;
begin
  Result := TlvkADOConnection.Create(IniFileName, SectionName);
end;

function NewRecordset(const Recordset: ADODB_TLB._Recordset): IlvkADORecordset;
begin
  Result := TlvkADORecordset.Create(Recordset);
end;

function NewRecordset(const FileName: string): IlvkADORecordset;
begin
  Result := TlvkADORecordset.Create(FileName);
end;

function NewRecordset(const Stream: TStream): IlvkADORecordset;
begin
  Result := TlvkADORecordset.Create(Stream);
end;

function Disconnected: IDispatch;
begin
  Result := nil;
end;

{ TlvkADOConnection }

function TlvkADOConnection.GetRecordset(const CommandText: string;
  const CommandType: TCommandType): IlvkADORecordset;
var
  Parameters  : array of OleVariant;
begin
  Parameters := nil;
  Result := GetRecordset2(CommandText, Parameters, CommandType);
end;

function TlvkADOConnection.GetRecordset(const CommandText: string;
  const Parameters: array of const;
  const CommandType: TCommandType): IlvkADORecordset;
begin
  Result := GetRecordset(CommandText, Parameters, [], CommandType);
end;

function TlvkADOConnection.GetRecordset(const CommandText: string;
  const Parameters: array of const; const ParameterNames: array of string;
  const CommandType: TCommandType): IlvkADORecordset;
var
  ra  : OleVariant;
begin
  Result := InternalExecute(CommandText, Parameters, ParameterNames,
    CommandType, True, ra);
end;

procedure TlvkADOConnection.Save(const Recordset: IlvkADORecordset;
  const AutoHandleConflicts: Boolean);
begin
  Recordset.Save(Self);
end;

procedure TlvkADOConnection.Requery(const Recordset: IlvkADORecordset);
begin
  Recordset.Requery(Self);
end;

procedure TlvkADOConnection.Requery(const Recordsets: array of IlvkADORecordset);
var
  Index : Integer;
begin
  for Index := Low(Recordsets) to High(Recordsets) do
    Recordsets[Index].Requery(Self);
end;

procedure TlvkADOConnection.Save(const Recordsets: array of IlvkADORecordset;
  const AutoHandleConflicts: Boolean);
var
  Index : Integer;
begin
  for Index := Low(Recordsets) to High(Recordsets) do
    Recordsets[Index].Save(Self, AutoHandleConflicts);
end;

procedure TlvkADOConnection.ExecSQL(const CommandText: string;
  const Parameters: array of const; const CommandType: TCommandType);
begin
  ExecSQL(CommandText, Parameters, [], CommandType);
end;

procedure TlvkADOConnection.ExecSQL(const CommandText: string;
  const CommandType: TCommandType);
var
  Parameters  : array of OleVariant;
begin
  Parameters := nil;
  ExecSQL2(CommandText, Parameters, [], CommandType);
end;

procedure TlvkADOConnection.ExecSQL(const CommandText: string;
  const Parameters: array of const; const ParameterNames: array of string;
  const CommandType: TCommandType);
var
  RecordsAffected : OleVariant;
begin
  InternalExecute(CommandText, Parameters, ParameterNames,
    CommandType, False, RecordsAffected);
end;

function TlvkADOConnection.InternalExecute(const CommandText: string;
  const Parameters: array of const; const ParameterNames: array of string;
  const CommandType: TCommandType; const ResultSet: Boolean;
  out RecordsAffected: OleVariant): IlvkADORecordset;
var
  VariantParameters : TFieldValueList;
  Index             : Integer;
begin
  VariantParameters := nil;

  if Length(Parameters) > 0 then
  begin
    SetLength(VariantParameters, Length(Parameters));
    for Index := Low(Parameters) to High(Parameters) do
    begin
      case TVarRec(Parameters[Index]).VType of
        vtInteger:
          VariantParameters[Index] := TVarRec(Parameters[Index]).VInteger;

        vtBoolean:
          VariantParameters[Index] := TVarRec(Parameters[Index]).VBoolean;

        vtChar:
          VariantParameters[Index] := TVarRec(Parameters[Index]).VChar;

        vtExtended:
          VariantParameters[Index] := TVarRec(Parameters[Index]).VExtended^;

        vtString:
          VariantParameters[Index] := TVarRec(Parameters[Index]).VString^;

        vtPointer, vtObject, vtClass, vtInterface:
          raise ElvkADOConnection.CreateFmt('Invalid datatype for parameter #%d', [Index]);

        vtPChar:
          VariantParameters[Index] := string(TVarRec(Parameters[Index]).VPChar);

        vtWideChar:
          VariantParameters[Index] := WideString(TVarRec(Parameters[Index]).VWideChar);

        vtPWideChar:
          VariantParameters[Index] := WideString(TVarRec(Parameters[Index]).VPWideChar);

        vtAnsiString:
          VariantParameters[Index] := string(TVarRec(Parameters[Index]).VAnsiString);

        vtCurrency:
          VariantParameters[Index] := TVarRec(Parameters[Index]).VCurrency^;

        vtVariant:
          VariantParameters[Index] := TVarRec(Parameters[Index]).VVariant^;

        vtWideString:
          VariantParameters[Index] := WideString(TVarRec(Parameters[Index]).VWideString^);

        {$IFDEF DELPHI6UP}
        vtInt64:
          VariantParameters[Index] := TVarRec(Parameters[Index]).VInt64^;
        {$ENDIF}

      else
        raise ElvkADOConnection.Create('INTERNAL ERROR IN INTERNALEXECUTE');
      end;
    end;
  end;

  Result := InternalExecute(CommandText, VariantParameters, ParameterNames,
    CommandType, ResultSet, RecordsAffected);
end;

procedure TlvkADOConnection.ExecSQL2(const CommandText: string;
  const Parameters: array of OleVariant; const CommandType: TCommandType);
begin
  ExecSQL2(CommandText, Parameters, [], CommandType);
end;

procedure TlvkADOConnection.ExecSQL2(const CommandText: string;
  const Parameters: array of OleVariant;
  const ParameterNames: array of string; const CommandType: TCommandType);
var
  RecordsAffected : OleVariant;
begin
  InternalExecute(CommandText, Parameters, ParameterNames, CommandType, False,
    RecordsAffected);
end;

function TlvkADOConnection.InternalExecute(const CommandText: string;
  const Parameters: array of OleVariant;
  const ParameterNames: array of string; const CommandType: TCommandType;
  const ResultSet: Boolean;
  out RecordsAffected: OleVariant): IlvkADORecordset;
var
  cmd       : ADODB_TLB.Command;
  Parameter : ADODB_TLB.Parameter;
  rs        : ADODB_TLB._Recordset;
  Index     : Integer;
const
  CommandTypes  : array[TCommandType] of LongWord = (
    adCmdUnspecified,
    adCmdUnknown,
    adCmdText,
    adCmdTable,
    adCmdStoredProc,
    adCmdFile,
    adCmdTableDirect
  );
begin
  cmd := CoCommand.Create;
  cmd.Set_ActiveConnection(FConnection);
  cmd.CommandText := CommandText;
  cmd.CommandType := CommandTypes[CommandType];

  if Length(Parameters) > 0 then
  begin
    for Index := Low(Parameters) to High(Parameters) do
    begin
      Parameter := CoParameter.Create;
      if Index < Length(ParameterNames) then
        Parameter.Name := ParameterNames[Index];

      case TVarData(Parameters[Index]).VType of
        {$IFDEF DELPHI6UP}
        varShortInt, varWord, varLongWord,
        {$ENDIF}
        varSmallInt, varInteger, varByte:
          begin
            Parameter.Type_ := adInteger;
            Parameter.Value := Integer(Parameters[Index]);
          end;

        {$IFDEF DELPHI6UP}
        varInt64:
          begin
            Parameter.Type_ := adBigInt;
            Parameter.Value := Parameters[Index];
          end;
        {$ENDIF}

        varSingle, varDouble:
          begin
            Parameter.Type_ := adDouble;
            Parameter.Value := Double(Parameters[Index]);
          end;

        varCurrency:
          begin
            Parameter.Type_ := adCurrency;
            Parameter.Value := Currency(Parameters[Index]);
          end;

        varDate:
          begin
            Parameter.Type_ := adDate;
            Parameter.Value := TDateTime(Parameters[Index]);
          end;

        varOleStr:
          begin
            Parameter.Type_ := adVarWChar;
            Parameter.Size := Length(WideString(Parameters[Index]));
            Parameter.Value := WideString(Parameters[Index]);
          end;

        varDispatch, varError, varUnknown, varAny, varArray, varByRef:
          raise ElvkADOConnection.CreateFmt('Invalid datatype for parameter #%d', [Index]);

        varBoolean:
          begin
            Parameter.Type_ := adBoolean;
            Parameter.Value := Boolean(Parameters[Index]);
          end;

        varString:
          begin
            Parameter.Type_ := adVarChar;
            Parameter.Size := Length(Parameters[Index]);
            Parameter.Value := string(Parameters[Index]);
          end;

      else
        raise ElvkADOConnection.Create('INTERNAL ERROR IN INTERNALEXECUTE');
      end;
      cmd.Parameters.Append(Parameter);
    end;
  end;

  if ResultSet then
  begin
    rs := CoRecordset.Create as ADODB_TLB._Recordset;
    rs.CursorLocation := adUseClient;
    rs.Open(cmd, EmptyParam, adOpenStatic, adLockBatchOptimistic, 0);
    rs.Set_ActiveConnection(Disconnected);
    Result := NewRecordset(rs);
  end else begin
    cmd.Execute(RecordsAffected, EmptyParam, adExecuteNoRecords);
    Result := nil;
  end;
end;

function TlvkADOConnection.GetRecordset2(const CommandText: string;
  const Parameters: array of OleVariant;
  const CommandType: TCommandType): IlvkADORecordset;
begin
  Result := GetRecordset2(CommandText, Parameters, [], CommandType);
end;

function TlvkADOConnection.GetRecordset2(const CommandText: string;
  const Parameters: array of OleVariant;
  const ParameterNames: array of string;
  const CommandType: TCommandType): IlvkADORecordset;
var
  RecordsAffected : OleVariant;
begin
  Result := InternalExecute(CommandText, Parameters, ParameterNames,
    CommandType, True, RecordsAffected);
end;

procedure TlvkADOConnection.Resync(const Recordset: IlvkADORecordset);
begin
  Recordset.Resync(Self);
end;

procedure TlvkADOConnection.Resync(const Recordsets: array of IlvkADORecordset);
var
  Index : Integer;
begin
  for Index := Low(Recordsets) to High(Recordsets) do
    Recordsets[Index].Resync(Self);
end;

function TlvkADOConnection.BeginTransaction: Integer;
begin
  Result := FConnection.BeginTrans;
end;

procedure TlvkADOConnection.CommitTransaction;
begin
  FConnection.CommitTrans;
end;

procedure TlvkADOConnection.RollbackTransaction;
begin
  FConnection.RollbackTrans;
end;

constructor TlvkADOConnection.Create(const ConnectionString: string;
  const Username, Password: WideString);
begin
  inherited Create;

  FConnection := CoConnection.Create;
  FConnection.Open(ConnectionString, Username, Password, 0);
end;

constructor TlvkADOConnection.Create(const IniFileName,
  SectionName: string);
var
  IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(IniFileName);
  try
    Create(IniFile.ReadString(
      SectionName, 'ConnectionString', ''),
      IniFile.ReadString(SectionName, 'Username', ''),
      IniFile.ReadString(SectionName, 'Password', ''));
  finally
    IniFile.Free;
  end;
end;

constructor TlvkADOConnection.Create(const ConnectionString: string);
begin
  Create(ConnectionString, '', '');
end;

function TlvkADOConnection.GetConnection: ADODB_TLB.Connection;
begin
  Result := FConnection;
end;

function TlvkADOConnection.GetEmptyRecordset(
  const TableName: string): IlvkADORecordset;
begin
  Assert(TableName <> '', 'No tablename given to GetEmptyRecordset');

  Result := GetRecordset('SELECT * FROM [' + TableName + '] WHERE 1 = 0');
end;

{ TlvkADORecordset }

procedure TlvkADORecordset.Append(const FieldNames: string;
  const Values: OleVariant);
begin
  FRecordset.AddNew(FieldNames, Values);
end;

procedure TlvkADORecordset.Append(const FieldNames: array of string;
  const Values: array of OleVariant);
var
  FieldNameList : OleVariant;
  ValueList     : OleVariant;
  Index         : Integer;
begin
  Assert(Length(FieldNames) > 0, 'No field names given to Append');
  Assert(Length(FieldNames) = Length(Values), 'Field names list and value list must be of same length');

  FieldNameList := VarArrayCreate([0, Length(FieldNames)-1], varVariant);
  for Index := 0 to Length(FieldNames)-1 do
    FieldNameList[Index] := FieldNames[Index];

  ValueList := VarArrayCreate([0, Length(Values)-1], varVariant);
  for Index := 0 to Length(Values)-1 do
    ValueList[Index] := Values[Index];

  FRecordset.AddNew(FieldNameList, ValueList);
end;

procedure TlvkADORecordset.CancelBatch;
begin
  FRecordset.CancelBatch(adAffectAll);
end;

procedure TlvkADORecordset.CancelUpdate;
begin
  FRecordset.CancelUpdate;
end;

constructor TlvkADORecordset.Create(const Recordset: _Recordset);
var
  Index : Integer;
begin
  inherited Create;

  Assert(Assigned(Recordset), 'No recordset given to TlvkADORecordset.Create');

  FRecordset := Recordset;
  FFilter := '';
  FFilterEnabled := False;

  FFieldNames := TStringList.Create;
  for Index := 0 to FRecordset.Fields.Count-1 do
    if FRecordset.Fields.Item[Index].Type_ <> adChapter then
      FFieldNames.Add(UpperCase(FRecordset.Fields.Item[Index].Name));
  FFieldNames.Sorted := True;

  FChapterNames := TStringList.Create;
  for Index := 0 to FRecordset.Fields.Count-1 do
    if FRecordset.Fields.Item[Index].Type_ = adChapter then
      FChapterNames.Add(UpperCase(FRecordset.Fields.Item[Index].Name));
  FChapterNames.Sorted := True;
end;

constructor TlvkADORecordset.Create(const FileName: string);
var
  rs  : ADODB_TLB._Recordset;
begin
  rs := CoRecordset.Create as ADODB_TLB._Recordset;
  rs.Open(FileName, EmptyParam, adOpenStatic, adLockBatchOptimistic, adCmdFile);
  Create(rs);
end;

constructor TlvkADORecordset.Create(const Stream: TStream);
var
  rs            : ADODB_TLB._Recordset;
  StreamAdapter : IStream;
begin
  rs := CoRecordset.Create as ADODB_TLB._Recordset;
  StreamAdapter := TStreamAdapter.Create(Stream);
  try
    rs.Open(StreamAdapter, EmptyParam, adOpenStatic, adLockBatchOptimistic, adCmdFile);
  finally
    StreamAdapter := nil;
  end;

  Create(rs);
end;

destructor TlvkADORecordset.Destroy;
begin
  FreeAndNil(FFieldNames);
  FreeAndNil(FChapterNames);

  inherited;
end;

function TlvkADORecordset.Find(const Criteria: string;
  const SearchDirection: TFindSearchDirection): Boolean;
begin
  if GetIsEmpty then
    Result := False
  else if (SearchDirection = fsdForward) and FRecordset.EOF then
    Result := False
  else if (SearchDirection = fsdBackward) and FRecordset.BOF then
    Result := False
  else
    Result := InternalFind(Criteria, FRecordset.Bookmark, SearchDirection);
end;

function TlvkADORecordset.FindFirst(const Criteria: string): Boolean;
begin
  if GetIsEmpty then
    Result := False
  else begin
    First;
    Result := InternalFind(Criteria, FRecordset.Bookmark, fsdForward);
  end;
end;

function TlvkADORecordset.FindFrom(const Criteria: string;
  const StartBookmark: OleVariant;
  const SearchDirection: TFindSearchDirection): Boolean;
begin
  if GetIsEmpty then
    Result := False
  else
    Result := InternalFind(Criteria, StartBookmark, SearchDirection);
end;

function TlvkADORecordset.FindLast(const Criteria: string): Boolean;
begin
  if GetIsEmpty then
    Result := False
  else begin
    Last;
    Result := InternalFind(Criteria, FRecordset.Bookmark, fsdBackward);
  end;
end;

function TlvkADORecordset.FindNext(const Criteria: string): Boolean;
begin
  if GetIsEmpty then
    Result := False
  else if FRecordset.EOF then
    Result := False
  else
    Result := InternalFind(Criteria, EmptyParam, fsdForward);
end;

function TlvkADORecordset.FindPrevious(const Criteria: string): Boolean;
begin
  if GetIsEmpty then
    Result := False
  else if FRecordset.BOF then
    Result := False
  else
    Result := InternalFind(Criteria, FRecordset.Bookmark, fsdBackward);
end;

procedure TlvkADORecordset.First;
begin
  FRecordset.MoveFirst;
end;

function TlvkADORecordset.GetBOF: Boolean;
begin
  Result := FRecordset.BOF;
end;

function TlvkADORecordset.GetBookmark: OleVariant;
begin
  Result := FRecordset.Bookmark;
end;

function TlvkADORecordset.GetEOF: Boolean;
begin
  Result := FRecordset.EOF;
end;

function TlvkADORecordset.GetFieldValues(
  const FieldName: string): OleVariant;
begin
  Result := FRecordset.Fields.Item[FieldName].Value;
end;

function TlvkADORecordset.GetFilter: WideString;
begin
  Result := FFilter;
end;

function TlvkADORecordset.GetFilterEnabled: Boolean;
begin
  Result := FFilterEnabled;
end;

function TlvkADORecordset.GetIsEmpty: Boolean;
begin
  Result := FRecordset.EOF and FRecordset.BOF;
end;

function TlvkADORecordset.GetRecordCount: Integer;
begin
  Result := FRecordset.RecordCount;
end;

function TlvkADORecordset.GetRecordset: ADODB_TLB._Recordset;
begin
  Result := FRecordset;
end;

function TlvkADORecordset.InternalFind(const Criteria: string;
  const StartBookmark: OleVariant;
  const SearchDirection: TFindSearchDirection): Boolean;
const
  Directions  : array[TFindSearchDirection] of TOleEnum = (
    TOleEnum(adSearchForward),
    TOleEnum(adSearchBackward)
  );
begin
  if (Criteria = '') and (FLastCriteria = '') then
    raise ElvkADORecordset.Create('No criteria for find to use');
  if Criteria = '' then
    FRecordset.Find(FLastCriteria, 0, Directions[SearchDirection], StartBookmark)
  else begin
    FRecordset.Find(Criteria, 0, Directions[SearchDirection], StartBookmark);
    FLastCriteria := Criteria;
  end;
  
  if FRecordset.BOF or FRecordset.EOF then
    Result := False
  else
    Result := True;
end;

procedure TlvkADORecordset.Last;
begin
  FRecordset.MoveLast;
end;

procedure TlvkADORecordset.Next;
begin
  FRecordset.MoveNext;
end;

procedure TlvkADORecordset.Previous;
begin
  FRecordset.MovePrevious;
end;

procedure TlvkADORecordset.Requery(const Connection: IlvkADOConnection);
begin
  FRecordset.Set_ActiveConnection(Connection.Connection);
  try
    FRecordset.Requery(0);
  finally
    FRecordset.Set_ActiveConnection(Disconnected);
  end;
end;

procedure TlvkADORecordset.Resync(const Connection: IlvkADOConnection);
begin
  FRecordset.Set_ActiveConnection(Connection.Connection);
  try
    FRecordset.Resync(adAffectAll, adResyncAllValues);
  finally
    FRecordset.Set_ActiveConnection(Disconnected);
  end;
end;

procedure TlvkADORecordset.Save(const Connection: IlvkADOConnection;
  const AutoHandleConflicts: Boolean);
var
  Index : Integer;
begin
  FRecordset.Set_ActiveConnection(Connection.Connection);
  try
    try
      FRecordset.UpdateBatch(adAffectAllChapters);
      if (not GetIsEmpty) and (FChapterNames.Count > 0) then
        for Index := 0 to FChapterNames.Count-1 do
          GetChildRecordset(FChapterNames[Index]).Save(Connection,
            AutoHandleConflicts);
    except
      on E: EOleException do
      begin
        if E.ErrorCode = E_ROW_UPDATE_CONFLICT then
        begin
          if AutoHandleConflicts then
          begin
            FRecordset.Filter := adFilterConflictingRecords;
            FRecordset.Resync(adAffectGroup, adResyncUnderlyingValues);
            FFilterType := ftConflictingRecords;
            FFilterEnabled := True;
          end;

          raise ElvkADORecordsetConflict.Create('Conflicting values in database');
        end else
          raise;
      end;
    end;
  finally
    FRecordset.Set_ActiveConnection(Disconnected);
  end;
end;

procedure TlvkADORecordset.SaveToFile(const FileName: string;
  const SaveFormat: TRecordsetSaveFormat);
begin
  if FileExists(FileName) then
    Win32Check(DeleteFile(PChar(FileName)));
  FRecordset.Save(FileName, PersistFormats[SaveFormat]);
end;

procedure TlvkADORecordset.SaveToStream(const Stream: TStream;
  const SaveFormat: TRecordsetSaveFormat);
var
  StreamAdapter : IStream;
begin
  StreamAdapter := TStreamAdapter.Create(Stream);
  try
    FRecordset.Save(StreamAdapter, PersistFormats[SaveFormat]);
  finally
    StreamAdapter := nil;
  end;
end;

procedure TlvkADORecordset.SetBookmark(const Value: OleVariant);
begin
  FRecordset.Bookmark := Value;
end;

procedure TlvkADORecordset.SetFieldValues(const FieldName: string;
  const Value: OleVariant);
begin
  FRecordset.Fields.Item[FieldName].Value := Value;
end;

procedure TlvkADORecordset.SetFilter(const Value: WideString);
begin
  if Value <> FFilter then
  begin
    FFilter := Value;
    AdjustFilter;
  end;
end;

procedure TlvkADORecordset.SetFilterEnabled(const Value: Boolean);
begin
  if Value <> FFilterEnabled then
  begin
    FFilterEnabled := Value;
    try
      AdjustFilter;
    except
      FFilterEnabled := not Value;
      raise;
    end;
  end;
end;

function TlvkADORecordset.Clone(const ReadonlyLock, CloneFilter: Boolean): IlvkADORecordset;
var
  ClonedRS  : ADODB_TLB._Recordset;
begin
  if ReadonlyLock then
    ClonedRS := FRecordset.Clone(adLockReadonly) as ADODB_TLB._Recordset
  else
    ClonedRS := FRecordset.Clone(TOleEnum(adLockUnspecified)) as ADODB_TLB._Recordset;

  // Make a cloned lvk ado recordset
  Result := TlvkADORecordset.Create(ClonedRS);

  // Clone filter
  if CloneFilter then
  begin
    Result.Filter := FFilter;
    Result.FilterEnabled := FFilterEnabled;
  end;

  // Go to same record
  Result.Bookmark := GetBookmark;
end;

procedure TlvkADORecordset.Delete;
begin
  FRecordset.Delete(adAffectCurrent);
end;

procedure TlvkADORecordset.Clear;
begin
  FRecordset.Delete(adAffectAll);
end;

procedure TlvkADORecordset.Append;
begin
  FRecordset.AddNew(EmptyParam, EmptyParam);
end;

procedure TlvkADORecordset.Update(const FieldNames: string;
  const Values: OleVariant);
begin
  FRecordset.Update(FieldNames, Values);
end;

procedure TlvkADORecordset.Update(const FieldNames: array of string;
  const Values: array of OleVariant);
var
  FieldNameList : OleVariant;
  ValueList     : OleVariant;
  Index         : Integer;
begin
  Assert(Length(FieldNames) > 0, 'No field names given to Update');
  Assert(Length(FieldNames) = Length(Values), 'Field names list and value list must be of same length');

  FieldNameList := VarArrayCreate([0, Length(FieldNames)-1], varVariant);
  for Index := 0 to Length(FieldNames)-1 do
    FieldNameList[Index] := FieldNames[Index];

  ValueList := VarArrayCreate([0, Length(Values)-1], varVariant);
  for Index := 0 to Length(Values)-1 do
    ValueList[Index] := Values[Index];

  FRecordset.Update(FieldNameList, ValueList);
end;

procedure TlvkADORecordset.Update;
begin
  FRecordset.Update(EmptyParam, EmptyParam);
end;

function TlvkADORecordset.GetFilterType: TFilterType;
begin
  Result := FFilterType;
end;

procedure TlvkADORecordset.SetFilterType(const Value: TFilterType);
begin
  if Value <> FFilterType then
  begin
    FFilterType := Value;
    AdjustFilter;
  end;
end;

procedure TlvkADORecordset.AdjustFilter;
begin
  if FFilterEnabled then
  begin
    case FFilterType of
      ftCriteria:
        if FFilter = '' then
          FRecordset.Filter := adFilterNone
        else
          FRecordset.Filter := FFilter;

      ftAffectedRecords:
        FRecordset.Filter := adFilterAffectedRecords;

      ftConflictingRecords:
        FRecordset.Filter := adFilterConflictingRecords;

      ftFetchedRecords:
        FRecordset.Filter := adFilterFetchedRecords;

      ftPendingRecords:
        FRecordset.Filter := adFilterPendingRecords;
    end;
  end else
    FRecordset.Filter := adFilterNone;
end;

function TlvkADORecordset.GetSortOrder: WideString;
begin
  Result := FRecordset.Sort;
end;

procedure TlvkADORecordset.SetSortOrder(const Value: WideString);
begin
  FRecordset.Sort := Value;
end;

function TlvkADORecordset.GetOriginalValues: OleVariant;
begin
  if VarIsEmpty(FOriginalValues) then
    FOriginalValues := TValueObject.Create(Self, wvOriginalValue) as IDispatch;
  Result := FOriginalValues;
end;

function TlvkADORecordset.GetUnderlyingValues: OleVariant;
begin
  if VarIsEmpty(FUnderlyingValues) then
    FUnderlyingValues := TValueObject.Create(Self, wvUnderlyingValue) as IDispatch;
  Result := FUnderlyingValues;
end;

function TlvkADORecordset.GetValues: OleVariant;
begin
  if VarIsEmpty(FValues) then
    FValues := TValueObject.Create(Self, wvValue) as IDispatch;
  Result := FValues;
end;

function TlvkADORecordset.GetChildRecordset(
  const ChildName: string): IlvkADORecordset;
var
  disp  : IDispatch;
  rs    : ADODB_TLB._Recordset;
begin
  if FChapterNames.IndexOf(UpperCase(ChildName)) >= 0 then
  begin
    disp := IDispatch(FRecordset.Fields.Item[ChildName].Value);
    rs := disp as ADODB_TLB._Recordset;
    Result := TlvkADORecordset.Create(rs);
  end else
    raise ElvkADORecordsetNoChild.CreateFmt('No child recordset with the name %s', [ChildName]);
end;

{ TValueObject }

constructor TValueObject.Create(const Recordset: TlvkADORecordset;
  const WhichValue: TWhichValue);
begin
  inherited Create;

  FRecordset := Recordset;
  FWhichValue := WhichValue;
end;

function TValueObject.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
type
  PPChar  = ^PWideChar;
var
  i, j    : Integer;
  pName   : PPChar;
  pDispID : ^Integer;
  ID      : Integer;
begin
  pName := Names;
  pDispID := DispIDs;
  Result := S_OK;
  for i := 1 to NameCount do
  begin
    ID := DISPID_UNKNOWN;
    j := FRecordset.FFieldNames.IndexOf(UpperCase(pName^));
    if j >= 0 then
      ID := FIELDNAME_BASE + j;

    if ID = DISPID_UNKNOWN then
      Result := DISP_E_UNKNOWNNAME;

    pDispID^ := ID;

    Inc(pDispID, 4);
    Inc(pName, 4);
  end;
end;

function TValueObject.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TValueObject.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := S_OK;
end;

function TValueObject.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
var
  NewParams : array of Variant;
  Res       : Variant;
  ParamsPtr : PDispParams;
  i         : Integer;
  Handled   : Boolean;
  FieldName : string;

  procedure AdjustByRef;
  var
    i : Integer;
  begin
    for i := 0 to ParamsPtr^.cArgs - 1 do
      UpdateVariant(OleVariant(ParamsPtr^.rgvarg[i]),
        NewParams[High(NewParams) - i]);
  end;

begin
  ParamsPtr := @Params;
  SetLength(NewParams, ParamsPtr^.cArgs);
  for i := 0 to ParamsPtr^.cArgs - 1 do
    NewParams[High(NewParams) - i] := VariantOf(ParamsPtr^.rgvarg[i]);

  Handled := False;
  Result := DISP_E_MEMBERNOTFOUND;

  if (Flags and DISPATCH_PROPERTYGET) <> 0 then
  begin
    if (DispID >= FIELDNAME_BASE) and (DispID <= FIELDNAME_END) then
    begin
      FieldName := FRecordset.FFieldNames[DispID - FIELDNAME_BASE];
      case FWhichValue of
        wvValue:
          Res := FRecordset.FRecordset.Fields.Item[FieldName].Value;

        wvOriginalValue:
          Res := FRecordset.FRecordset.Fields.Item[FieldName].OriginalValue;

        wvUnderlyingValue:
          Res := FRecordset.FRecordset.Fields.Item[FieldName].UnderlyingValue;
      end;
      Handled := True;
      Result := S_OK;
    end;
  end;

  if not Handled then
  begin
    if (Flags and DISPATCH_PROPERTYPUT) <> 0 then
    begin
      if (DispID >= FIELDNAME_BASE) and (DispID < FIELDNAME_END) then
      begin
        FieldName := FRecordset.FFieldNames[DispID - FIELDNAME_BASE];
        Result := S_OK;
        case FWhichValue of
          wvValue:
            FRecordset.FRecordset.Fields.Item[FieldName].Value := NewParams[0];

          wvOriginalValue:
            Result := E_ACCESSDENIED;

          wvUnderlyingValue:
            Result := E_ACCESSDENIED;
        end;
        Handled := True;
        Res := 0;
      end;
    end;
  end;

  if not Handled then
  begin
    if (Flags and DISPATCH_METHOD) <> 0 then
    begin
      // handle method calls here, if any
    end;
  end;

  if not Handled then
    Result := DISP_E_MEMBERNOTFOUND;

  if Assigned(VarResult) and (Result = S_OK) then
    OleVariant(VarResult^) := Res;
end;

end.
