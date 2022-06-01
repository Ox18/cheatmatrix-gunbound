{ TODO  -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the global SQL object that is used with the SQL
    script preprocessor.
}
unit lvkSQLGlobal;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 17.04.03 15:00 $
// $Archive: /Components/LVK/Source/lvkSQLGlobal.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Windows, ADOInt, lvkRTTIDispatch;

type
  TSQLConnection = record
    Name        : WideString;
    Connection  : _Connection;
  end;

  { Description:
      This object is an internal object used by the active scripting classes,
      and by the SQL script preprocessor class.

      See the TSQLScriptPreProcessor class for more information.
    See also:
      TSQLScriptPreProcessor
  }
  TlvkSQLGlobal = class(TlvkRTTIBaseDispatch)
  private
    FConnections        : array of TSQLConnection;
    FDefaultConnection  : Integer;

    function IndexOf(const Name: string): Integer;
    function RunSQL(var Parameters: array of Variant; out FunctionResult: Variant;
      const ReturnData: Boolean; out ResultData: Variant): HRESULT;

  published
    function Connect(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT; virtual;
    function Disconnect(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT; virtual;
    function StartForEach(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT; virtual;
    function BeginTransaction(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT; virtual;
    function CommitTransaction(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT; virtual;
    function AbortTransaction(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT; virtual;
    function ExecuteSQL(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT; virtual;
    function CallSQL(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT; virtual;
    function RegExpMatch(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT; virtual;
  end;

  ESQLGlobal = class(Exception);

implementation

uses
{$IFDEF DELPHI6UP}
  Variants,
{$ENDIF}
  lvkRegExp;

{ TlvkSQLGlobal }

function TlvkSQLGlobal.AbortTransaction(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
var
  Name  : WideString;
  Index : Integer;
begin
  if Length(Parameters) = 1 then
  begin
    Name := Parameters[0];

    if Name = '*' then
    begin
      for Index := 0 to Length(FConnections)-1 do
        FConnections[Index].Connection.RollbackTrans;
    end else begin
      Index := IndexOf(Name);

      if Index < 0 then
        raise ESQLGlobal.Create('DSN Name "' + Name + '" does not exist, cannot abort transaction');

      FConnections[Index].Connection.RollbackTrans;
    end;

    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

function TlvkSQLGlobal.BeginTransaction(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
var
  Name  : WideString;
  Index : Integer;
begin
  if Length(Parameters) = 1 then
  begin
    Name := Parameters[0];

    if Name = '*' then
    begin
      for Index := 0 to Length(FConnections)-1 do
        FConnections[Index].Connection.BeginTrans;
    end else begin
      Index := IndexOf(Name);

      if Index < 0 then
        raise ESQLGlobal.Create('DSN Name "' + Name + '" does not exist, cannot begin transaction');

      FConnections[Index].Connection.BeginTrans;
    end;

    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

function TlvkSQLGlobal.CallSQL(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
var
  Disp      : IDispatch;
  RecordSet : _RecordSet;
begin
  Result := RunSQL(Parameters, FunctionResult, True, FunctionResult);

  if (TVarData(FunctionResult).VType = varDispatch) and
    (Assigned(TVarData(FunctionResult).VDispatch)) then
  begin
    Disp := FunctionResult;
    if Disp.QueryInterface(_RecordSet, RecordSet) = S_OK then
    begin
      if (RecordSet.RecordCount = 1) and (RecordSet.Fields.Count = 1) then
      begin
        if (RecordSet.Fields.Item[0].Name = '') or lvkRegExp.RegExpMatch('^Expr[0-9]+$', RecordSet.Fields.Item[0].Name) then
          FunctionResult := RecordSet.Fields.Item[0].Value;
      end;
    end;
  end;
end;

function TlvkSQLGlobal.CommitTransaction(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
var
  Name  : WideString;
  Index : Integer;
begin
  if Length(Parameters) = 1 then
  begin
    Name := Parameters[0];

    if Name = '*' then
    begin
      for Index := 0 to Length(FConnections)-1 do
        FConnections[Index].Connection.CommitTrans;
    end else begin
      Index := IndexOf(Name);

      if Index < 0 then
        raise ESQLGlobal.Create('DSN Name "' + Name + '" does not exist, cannot commit transaction');

      FConnections[Index].Connection.CommitTrans;
    end;

    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

function TlvkSQLGlobal.Connect(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
var
  Name              : WideString;
  Index             : Integer;
  ConnectionString  : WideString;
  Connection        : _Connection;
begin
  if Length(Parameters) = 2 then
  begin
    Name := Parameters[0];
    ConnectionString := Parameters[1];

    Index := IndexOf(Name);
    if Index >= 0 then
      raise ESQLGlobal.Create('DSN Name "' + Name + '" already exists, cannot connection again');

    Connection := CoConnection.Create;
    Connection.Open(ConnectionString, '', '', 0);

    SetLength(FConnections, Length(FConnections)+1);
    FConnections[High(FConnections)].Name := Name;
    FConnections[High(FConnections)].Connection := Connection;
    if FDefaultConnection = -1 then
      FDefaultConnection := High(FConnections);

    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

function TlvkSQLGlobal.Disconnect(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
var
  Name  : string;
  Index : Integer;
begin
  if Length(Parameters) = 1 then
  begin
    Name := Parameters[0];
    if Name = '*' then
      SetLength(FConnections, 0)
    else begin
      Index := IndexOf(Name);
      if Index < 0 then
        raise ESQLGlobal.Create('DSN Name "' + Name + '" does not exist, cannot disconnect');

      if FDefaultConnection = Index then
        FDefaultConnection := -1;

      while Index < High(FConnections) do
        FConnections[Index] := FConnections[Index+1];
      SetLength(FConnections, Length(FConnections)-1);
    end;

    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

function TlvkSQLGlobal.ExecuteSQL(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
var
  Dummy : Variant;
begin
  Result := RunSQL(Parameters, FunctionResult, False, Dummy);
end;

function TlvkSQLGlobal.IndexOf(const Name: string): Integer;
var
  Index : Integer;
begin
  Result := -1;

  for Index := 0 to Length(FConnections)-1 do
    if CompareText(FConnections[Index].Name, Name) = 0 then
    begin
      Result := Index;
      Break;
    end;
end;

function TlvkSQLGlobal.RegExpMatch(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
begin
  if Length(Parameters) = 2 then
  begin
    FunctionResult := lvkRegExp.RegExpMatch(string(Parameters[0]), string(Parameters[1]));
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

function TlvkSQLGlobal.RunSQL(var Parameters: array of Variant;
  out FunctionResult: Variant; const ReturnData: Boolean;
  out ResultData: Variant): HRESULT;
var
  Index           : Integer;
  SQL             : WideString;
  Command         : _Command;
  RecordSet       : _RecordSet;
  RecordsAffected : OleVariant;
  ParamIndex      : Integer;
begin
  if Length(Parameters) >= 2 then
  begin
    if Parameters[0] = '' then
    begin
      Index := FDefaultConnection;

      if Index < 0 then
        raise ESQLGlobal.Create('no default connection, cannot execute sql');
    end else begin
      Index := IndexOf(Parameters[0]);
      if Index < 0 then
        raise ESQLGlobal.Create('DSN Name "' + Parameters[0] +
          '" does not exist, cannot execute sql');
    end;

    SQL := Parameters[1];
    Command := CoCommand.Create;
    Command.Set_ActiveConnection(FConnections[Index].Connection);
    Command.CommandText := SQL;

    if Length(Parameters) > 2 then
    begin
      for ParamIndex := 2 to Length(Parameters)-1 do
        Command.Parameters[ParamIndex-2].Value := Parameters[ParamIndex];
    end;

    if ReturnData then
    begin
      RecordSet := CoRecordset.Create;
      RecordSet.CursorLocation := adUseClient;
      RecordSet.Open(Command, EmptyParam, adOpenKeySet, adLockOptimistic, 0);
      ResultData := RecordSet;
    end else
      Command.Execute(RecordsAffected, EmptyParam, adExecuteNoRecords);

    if Length(Parameters) > 2 then
    begin
      for ParamIndex := 2 to Length(Parameters)-1 do
        Parameters[ParamIndex] := Command.Parameters[ParamIndex-2].Value;
    end;

    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

function TlvkSQLGlobal.StartForEach(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
var
  Command     : _Command;
  RecordSet   : _Recordset;
  Name        : WideString;
  SQL         : WideString;
  Index       : Integer;
  ParamIndex  : Integer;
begin
  if Length(Parameters) >= 2 then
  begin
    Name := Parameters[0];
    SQL := Parameters[1];

    if Name <> '' then
      Index := IndexOf(Name)
    else
      Index := FDefaultConnection;

    if Index < 0 then
      raise ESQLGlobal.Create('DSN Name "' + Name + '" does not exist, cannot retrieve records');

    Command := CoCommand.Create;
    Command.Set_ActiveConnection(FConnections[Index].Connection);
    Command.CommandText := SQL;

    if Length(Parameters) > 2 then
    begin
      for ParamIndex := 2 to Length(Parameters)-1 do
        Command.Parameters[ParamIndex-2].Value := Parameters[ParamIndex];
    end;

    RecordSet := CoRecordset.Create;
    RecordSet.CursorType := adOpenForwardOnly;
    RecordSet.Open(Command, EmptyParam, adUseClient, adLockOptimistic, 0);

    if Length(Parameters) > 2 then
    begin
      for ParamIndex := 2 to Length(Parameters)-1 do
        Parameters[ParamIndex] := Command.Parameters[ParamIndex-2].Value;
    end;

    FunctionResult := RecordSet;
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

end.
