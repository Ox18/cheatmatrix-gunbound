{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains components for quering a LDAP server for information.
}
unit lvkLDAP;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkLDAP.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, SysUtils, WinLdap;

const
  iLogonDN        = 0;
  iLogonPassword  = 1;
  iLogonCred      = 2;

  iQuery          = 0;
  iFields         = 1;
  iBaseDN         = 2;

type
{ Support classes }
  ElvkLDAP        = class(Exception);
  PPCharArray     = ^TPCharArray;
  TPCharArray     = array[0..32767] of PChar;

{ TlvkLDAPConnection class }
  TlvkLDAPBindType            = (btSimple, btSendCred);
  TlvkLDAPConnectorNotifyType = (ntAdd, ntRemove);

const
  DEFAULT_PORT            = LDAP_PORT;
  DEFAULT_CONNECTED       = False;
  DEFAULT_BINDTYPE        = btSimple;
  DEFAULT_AUTO_CONNECT    = True;
  DEFAULT_AUTO_DISCONNECT = True;

type
  TlvkLDAPConnection          = class(TComponent)
  private
    // Private members
    FHostName           : string;
    FPort               : LongWord;

    FConnected          : Boolean;

    FLDAP               : PLDAP;
    FLogonPassword      : string;
    FLogonDN            : string;
    FLogonCred          : string;
    FBindType           : TlvkLDAPBindType;
    FAutoConnect        : Boolean;
    FAutoDisconnect     : Boolean;

    FConnectors         : TList;

    FSetConnected       : Boolean;
    FSetConnectedValue  : Boolean;

    FOnConnect          : TNotifyEvent;
    FOnDisconnect       : TNotifyEvent;

    // Private methods
    procedure SetHostName(const Value: string);
    procedure SetPort(const Value: LongWord);
    procedure SetConnected(const Value: Boolean);
    function GetLogonString(const Index: Integer): string;
    procedure SetLogonString(const Index: Integer; const Value: string);
    procedure SetBindType(const Value: TlvkLDAPBindType);

    procedure ConnectorNotify(const Connector: TObject;
      const Notification: TlvkLDAPConnectorNotifyType);

  protected
    // Protected methods
    procedure DoConnect;
    procedure DoDisconnect;
    procedure Loaded; override;

  public
    // Public constructor and destructor
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Public methods
    procedure Connect;
    procedure Disconnect;

    // Public properties
    property pld: PLDAP read FLDAP;

  published
    // Published properties
    property HostName: string read FHostName write SetHostName;
    property Port: LongWord read FPort write SetPort default DEFAULT_PORT;

    property Connected: Boolean read FConnected write SetConnected
      default DEFAULT_CONNECTED;
    property LogonDN: string index iLogonDN read GetLogonString
      write SetLogonString;
    property LogonPassword: string index iLogonPassword read GetLogonString
      write SetLogonString;
    property LogonCred: string index iLogonCred read GetLogonString
      write SetLogonString;
    property BindType: TlvkLDAPBindType read FBindType write SetBindType
      default DEFAULT_BINDTYPE;
    property AutoConnect: Boolean read FAutoConnect write FAutoConnect
      default DEFAULT_AUTO_CONNECT;
    property AutoDisconnect: Boolean read FAutoDisconnect write FAutoDisconnect
      default DEFAULT_AUTO_DISCONNECT;

    // Published events
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
  end; // class TlvkLDAPConnection

{ TlvkLDAPConnector class }
  TlvkLDAPConnector = class(TComponent)
  private
    // Private members
    FConnection : TlvkLDAPConnection;

    // Private methods
    procedure SetConnection(const Value: TlvkLDAPConnection);
    function FindConnection(const AOwner: TComponent): TlvkLDAPConnection;

  protected
    // Protected methods
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

  public
    // Public constructor and destructor
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    // Published properties
    property Connection: TlvkLDAPConnection read FConnection
      write SetConnection;
  end; // class TlvkLDAPConnector

{ TlvkLDAPItemAttribute class }
  TlvkLDAPItemAttribute   = class(TCollectionItem)
  private
    // Private members
    FName       : string;
    FValues     : TStrings;

  public
    // Public constructor and destructor
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    // Public properties
    property Name: string
      read FName write FName;
    property Values: TStrings
      read FValues;
  end; // class TlvkLDAPItemAttribute

{ TlvkLDAPItemAttributes class }
  TlvkLDAPItemAttributes    = class(TCollection)
  private
    // Private methods
    function GetItem(const Index: Integer): TlvkLDAPItemAttribute;
    procedure SetItem(const Index: Integer;
      const Value: TlvkLDAPItemAttribute);
    function AttributeByName(const Name: string): TlvkLDAPItemAttribute;

  public
    // Public constructor
    constructor Create;

    // Public methods
    function Add: TlvkLDAPItemAttribute;
    function FindItemID(ID: Integer): TlvkLDAPItemAttribute;
    function Insert(Index: Integer): TlvkLDAPItemAttribute;

    // Public properties
    property Attributes[const Name: string]: TlvkLDAPItemAttribute
      read AttributeByName; default;
    property Items[const Index: Integer]: TlvkLDAPItemAttribute read GetItem
      write SetItem;
  end; // class TlvkLDAPItemAttributes

{ TlvkLDAPQueryResultItem class }
  TlvkLDAPQueryResultItem = class(TCollectionItem)
  private
    // Private members
    FAttributes : TlvkLDAPItemAttributes;

  public
    // Public constructor and destructor
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    // Public properties
    property Attributes: TlvkLDAPItemAttributes
      read FAttributes;
  end; // class TlvkLDAPQueryResultItem

{ TlvkLDAPQueryResult class }
  TlvkLDAPQueryResult     = class(TCollection)
  private
    function GetItem(const Index: Integer): TlvkLDAPQueryResultItem;
    procedure SetItem(const Index: Integer;
      const Value: TlvkLDAPQueryResultItem);
  public
    // Public constructor
    constructor Create;

    // Public methods
    function Add: TlvkLDAPQueryResultItem;
    function FindItemID(ID: Integer): TlvkLDAPQueryResultItem;
    function Insert(Index: Integer): TlvkLDAPQueryResultItem;

    // Public properties
    property Items[const Index: Integer]: TlvkLDAPQueryResultItem
      read GetItem write SetItem;
  end; // class TlvkLDAPQueryResult

{ TlvkLDAPParameter class }
  TlvkLDAPParameter = class(TCollectionItem)
  private
    // Private members
    FName   : string;
    FValue  : string;
    procedure SetValue(const Value: string);
    procedure SetName(const Value: string);

  protected
    // Protected methods
    function GetDisplayName: string; override;

  public
    // Public constructor and destructor
    constructor Create(Collection: TCollection); override;

  published
    // Published properties
    property Name: string
      read FName write SetName;
    property Value: string
      read FValue write SetValue;
  end; // class TlvkLDAPParameter

{ TlvkLDAPParameters class }
  TlvkLDAPParameters  = class(TOwnedCollection)
  private
    // Private methods
    function GetItem(const Index: Integer): TlvkLDAPParameter;
    procedure SetItem(const Index: Integer;
      const Value: TlvkLDAPParameter);
    function GetIndexedParameter(const Name: string): string;
    procedure SetIndexedParameter(const Name, Value: string);

  public
    // Public constructor
    constructor Create(AOwner: TPersistent);

    // Public methods
    function Add: TlvkLDAPParameter;
    function FindItemID(ID: Integer): TlvkLDAPParameter;
    function Insert(Index: Integer): TlvkLDAPParameter;
    function ParamByName(const Name: string): TlvkLDAPParameter;

    // Public properties
    property Parameters[const Name: string]: string
      read GetIndexedParameter write SetIndexedParameter; default;
    property Items[const Index: Integer]: TlvkLDAPParameter
      read GetItem write SetItem;
  end; // class TlvkLDAPParameters

{ TlvkLDAPQuery class }
  TlvkLDAPQueryScope  = (qsBase, qsOneLevel, qsSubTree);
  TlvkLDAPQueryFormat = (qfLDAP, qfBoolean);
  TlvkLDAPGetParam    = function(const Name: string): string of object;

const
  DEFAULT_QUERY_FORMAT    = qfLDAP;
  DEFAULT_SCOPE           = qsSubTree;

type
  TlvkLDAPQuery       = class(TlvkLDAPConnector)
  private
    // Private members
    FQuery        : string;
    FBaseDN       : string;
    FFields       : string;
    FScope        : TlvkLDAPQueryScope;
    FResult       : TlvkLDAPQueryResult;
    FParameters   : TlvkLDAPParameters;
    FQueryFormat  : TlvkLDAPQueryFormat;

    // Private methods
    function GetQueryString(const Index: Integer): string;
    procedure SetQueryString(const Index: Integer; const Value: string);
    function RegisterParam(const ParamName: string): string;
    function GetParam(const ParamName: string): string;
    function ParseQuery(const Query: string; const GetParam: TlvkLDAPGetParam): string;
    procedure SetQueryFormat(const Value: TlvkLDAPQueryFormat);
    procedure SetScope(const Value: TlvkLDAPQueryScope);
    function Boolean2LDAP(sExpression: string): string;
    function GetRealQuery: string;

  public
    // Public constructor and destructor
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Public methods
    procedure Execute;
    procedure Invalidate;

    // Public properties
    property Result: TlvkLDAPQueryResult
      read FResult;
    property RealQuery: string
      read GetRealQuery;

  published
    // Published properties
    property Query: string
      index iQuery read GetQueryString write SetQueryString;
    property Fields: string
      index iFields read GetQueryString write SetQueryString;
    property BaseDN: string
      index iBaseDN read GetQueryString write SetQueryString;
    property Parameters: TlvkLDAPParameters
      read FParameters;
    property QueryFormat: TlvkLDAPQueryFormat
      read FQueryFormat write SetQueryFormat default DEFAULT_QUERY_FORMAT;
    property Scope: TlvkLDAPQueryScope
      read FScope write SetScope default DEFAULT_SCOPE;
  end; // class TlvkLDAPQuery

procedure LDAPCheck(const Result: LongWord);

resourcestring
  SInvalidIndex         = 'Invalid index passed to a indexed property accessor method';
  SUnableToConnect      = 'Unable to connect to ldap server';
  SNotOpen              = 'Cannot execute ldap commands over a closed connection';
  SErrOrMissingOperand  = 'Missing operand for OR operator';
  SErrAndMissingOperand = 'Missing operand for AND operator';
  SErrNotMissingOperand = 'Missing operand for NOT operator';
  SErrExpressionParse   = 'Invalid boolean expression';

function PCharOf(const s: string): PChar;

implementation

function PCharOf(const s: string): PChar;
begin
  if s = '' then
    Result := nil
  else
    Result := PChar(s);
end;

procedure LDAPCheck(const Result: LongWord);
begin
  if (Result <> LDAP_SUCCESS) then
    raise ElvkLDAP.Create(ldap_err2string(Result));
end; // procedure LDAPCheck

{ TlvkLDAPConnection }

procedure TlvkLDAPConnection.Connect;
begin
  Connected := True;
end; // procedure TlvkLDAPConnection.Connect

procedure TlvkLDAPConnection.ConnectorNotify(const Connector: TObject;
  const Notification: TlvkLDAPConnectorNotifyType);
begin
  case Notification of
    ntAdd     : FConnectors.Add(Connector);
    ntRemove  : FConnectors.Remove(Connector);
  end; // case type of notification
end; // procedure TlvkLDAPConnection.ConnectorNotify

constructor TlvkLDAPConnection.Create(AOwner: TComponent);
begin
  // First create inherited part
  inherited Create(AOwner);

  // Then initialize private members
  FConnectors := TList.Create;
  FConnected := DEFAULT_CONNECTED;
  FLDAP := nil;
  FHostName := '';
  FPort := LDAP_PORT;
  FLogonDN := '';
  FLogonPassword := '';
  FLogonCred := '';
  FBindType := DEFAULT_BINDTYPE;
  FAutoConnect := DEFAULT_AUTO_CONNECT;
  FAutoDisconnect := DEFAULT_AUTO_DISCONNECT;
  FSetConnected := False;
end; // constructor TlvkLDAPConnection.Create

destructor TlvkLDAPConnection.Destroy;
begin
  // First destroy private members
  while (FConnectors.Count > 0) do
    TlvkLDAPConnector(FConnectors[0]).Connection := nil;
  FreeAndNil(FConnectors);
  Disconnect;

  // Then destroy inherited part
  inherited;
end; // destructor TlvkLDAPConnection.Destroy

procedure TlvkLDAPConnection.Disconnect;
begin
  Connected := False;
end; // procedure TlvkLDAPConnection.Disconnect

procedure TlvkLDAPConnection.DoConnect;
begin
  try
    FLDAP := ldap_openA(PChar(FHostName), FPort);
    if (not Assigned(FLDAP)) then
      raise ElvkLDAP.Create(SUnableToConnect);
    case FBindType of
      btSimple    : LDAPCheck(ldap_simple_bind_sA(FLDAP, PCharOf(FLogonDN), PCharOf(FLogonPassword)));
      btSendCred  : LDAPCheck(ldap_bind_sA(FLDAP, PCharOf(FLogonDN), PCharOf(FLogonCred), 0));
    end; // case type of binding
    if (Assigned(FOnConnect)) then
      FOnConnect(Self);
  except
    ldap_unbind(FLDAP);
    FLDAP := nil;
    raise;
  end; // try except - close connection on bind failure
end; // procedure TlvkLDAPConnection.DoConnect

procedure TlvkLDAPConnection.DoDisconnect;
begin
  if (Assigned(FOnDisconnect)) then
    FOnDisconnect(Self);
  LDAPCheck(ldap_unbind(FLDAP));
  FLDAP := nil;
end; // procedure TlvkLDAPConnection.DoDisconnect

function TlvkLDAPConnection.GetLogonString(const Index: Integer): string;
begin
  case Index of
    iLogonDN        : Result := FLogonDN;
    iLogonPassword  : Result := FLogonPassword;
    iLogonCred      : Result := FLogonCred;
  else
    raise ElvkLDAP.Create(SInvalidIndex);
  end; // case type of string to return
end; // function TlvkLDAPConnection.GetLogonString

procedure TlvkLDAPConnection.Loaded;
begin
  // First call inherited method
  inherited;

  // Then initialize private members
  if (FSetConnected) then
    Connected := FSetConnectedValue;
end; // procedure TlvkLDAPConnection.Loaded

procedure TlvkLDAPConnection.SetBindType(const Value: TlvkLDAPBindType);
begin
  if (Value <> FBindType) then
  begin
    if (not (csDesigning in ComponentState)) then
      Disconnect;
    FBindType := Value;
  end; // if new setting
end; // procedure TlvkLDAPConnection.SetBindType

procedure TlvkLDAPConnection.SetConnected(const Value: Boolean);
begin
  if (csLoading in ComponentState) then
  begin
    if (Value) then
    begin
      FSetConnected := True;
      FSetConnectedValue := Value;
    end; // if connected after loading
  end // if loading component off of stream
  else begin
    if (Value <> FConnected) then
    begin
      if (not (csDesigning in ComponentState)) then
      begin
        if (Value) then
          DoConnect
        else
          DoDisconnect;
      end; // if not design-time
      FConnected := Value;
    end; // if new setting
  end; // if not loading component off of stream
end; // procedure TlvkLDAPConnection.SetConnected

procedure TlvkLDAPConnection.SetHostName(const Value: string);
begin
  if (Value <> FHostName) then
  begin
    if (not (csDesigning in ComponentState)) then
      Disconnect;
    FHostName := Value;
  end; // if new value
end; // procedure TlvkLDAPConnection.SetHostName

procedure TlvkLDAPConnection.SetLogonString(const Index: Integer;
  const Value: string);
begin
  if (Value <> GetLogonString(Index)) then
  begin
    if (not (csDesigning in ComponentState)) then
      Disconnect;
    case Index of
      iLogonDN        : FLogonDN := Value;
      iLogonPassword  : FLogonPassword := Value;
      iLogonCred      : FLogonCred := Value;
    else
      raise ElvkLDAP.Create(SInvalidIndex);
    end; // case type of string to set
  end; // if new value
end; // procedure TlvkLDAPConnection.SetLogonString

procedure TlvkLDAPConnection.SetPort(const Value: LongWord);
begin
  if (Value <> FPort) then
  begin
    if (not (csDesigning in ComponentState)) then
      Disconnect;
    FPort := Value;
  end; // if new value
end; // procedure TlvkLDAPConnection.SetPort

{ TlvkLDAPConnector }

constructor TlvkLDAPConnector.Create(AOwner: TComponent);
begin
  // First create inherited part
  inherited Create(AOwner);

  // Then initialize private members
  FConnection := FindConnection(AOwner);
end; // constructor TlvkLDAPConnector.Create

destructor TlvkLDAPConnector.Destroy;
begin
  // First destroy private members
  Connection := nil;

  // Then destroy inherited part
  inherited;
end; // destructor TlvkLDAPConnector.Destroy

function TlvkLDAPConnector.FindConnection(
  const AOwner: TComponent): TlvkLDAPConnection;
var
  Index : Integer;
  Found : Integer;
  Count : Integer;
begin
  if (not Assigned(AOwner)) or (not (csDesigning in ComponentState)) then
    Result := nil
  else begin
    Index := 0;
    Count := 0;
    Found := 0;
    while (Index < AOwner.ComponentCount) do
    begin
      if (AOwner.Components[Index] is TlvkLDAPConnection) then
      begin
        Inc(Count);
        Found := Index;
      end; // if found a connection
      Inc(Index);
    end; // while more components to check

    if (Count = 1) then
      Result := AOwner.Components[Found] as TlvkLDAPConnection
    else
      Result := nil;
  end; // if got a owner
end; // function TlvkLDAPConnector.FindConnection

procedure TlvkLDAPConnector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Connection) then
    FConnection := nil;
  inherited;
end; // procedure TlvkLDAPConnector.Notification

procedure TlvkLDAPConnector.SetConnection(const Value: TlvkLDAPConnection);
begin
  if (Value <> FConnection) then
  begin
    if (Assigned(FConnection)) then
      FConnection.ConnectorNotify(Self, ntRemove);
    FConnection := Value;
    if (Assigned(FConnection)) then
      FConnection.ConnectorNotify(Self, ntAdd);
  end; // if new setting
end; // procedure TlvkLDAPConnector.SetConnection

{ TlvkLDAPQuery }

function TlvkLDAPQuery.Boolean2LDAP(sExpression: string): string;
var
  clParts : TStringList;

  procedure CollapseExpression;
  var
    i : Integer;
  begin
    sExpression := StringReplace(sExpression, #9, #32, [rfReplaceAll]);
    i := 1;
    while (i <= Length(sExpression)) do
    begin
      if (sExpression[i] in ['=', '<', '>', '~' ]) then begin
        while (i > 1) and (sExpression[i-1] = #32) do
        begin
          Delete(sExpression, i-1, 1);
          Dec(i);
        end; // while more spaces in front of symbol
        while (i < Length(sExpression)) and (sExpression[i+1] = #32) do
          Delete(sExpression, i+1, 1);
      end; // if character is a symbol

      // Next
      Inc(i);
    end; // while more to scan
  end; // procedure TlvkLDAPQuery.Boolean2LDAP.CollapseExpression

  procedure SplitExpression;
  var
    i           : Integer;
    CurrentWord : string;
    InString    : Char;

    procedure EndOfWord;
    var
      CurrentUpcaseWord : string;
    begin
      if (CurrentWord <> '') then
      begin
        CurrentUpcaseWord := AnsiUpperCase(CurrentWord);
        if (CurrentUpcaseWord = 'AND') then
          CurrentWord := '&'
        else if (CurrentUpcaseWord = 'OR') then
          CurrentWord := '|'
        else if (CurrentUpcaseWord = 'NOT') then
          CurrentWord := '!';
        clParts.Add(CurrentWord);
        CurrentWord := '';
      end; // if we have just left a word
    end; // procedure TBaseIndex.Query.SplitExpression.EndOfWord

  begin
    i := 1;
    InString := #0;
    repeat
      if (InString <> #0) then
      begin
        CurrentWord := CurrentWord + sExpression[i];
        Inc(i);
        if (sExpression[i-1] = InString) then
          InString := #0;
      end // if inside string
      else begin
        case sExpression[i] of
          #32, #9 : begin
            EndOfWord;
            Inc(i);
          end; // case character is space
          '"', '''' : begin
            InString := sExpression[i];
            CurrentWord := CurrentWord + sExpression[i];
            Inc(i);
          end;
          '(', ')', '|', '&', '!', '+', '^' : begin
            EndOfWord;
            case sExpression[i] of
              '+' : clParts.Add('|');
              '*' : clParts.Add('&');
            else
              clParts.Add(sExpression[i]);
            end;
            Inc(i);
          end; // case character is single token
        else
          CurrentWord := CurrentWord + sExpression[i];
          Inc(i);
        end; // case type of character
      end; // if not inside string
    until (i > Length(sExpression));
    EndOfWord;
  end; // procedure TlvkLDAPQuery.Boolean2LDAP.SplitExpression

  function Evaluate(i1, i2: Integer): string;
  type
    TPriority = record
      Sym : string;
      Pri : Integer;
    end;
  const
    Priority  : array[1..3] of TPriority = (
      (Sym: '|';  Pri: 2),
      (Sym: '&';  Pri: 3),
      (Sym: '^';  Pri: 3)
    );
  var
    Pri, PriX : Integer;
    i,j       : Integer;
    FoundPri  : Integer;
    Level     : Integer;

    function HandleOr(i1, i2, ib: Integer): string;
    var
      d1,d2 : string;
    begin
      if (i1 = ib) or (i2 = ib) then
        raise Exception.Create(SErrOrMissingOperand);

      d1 := Evaluate(i1, ib-1);
      d2 := Evaluate(ib+1, i2);
      Result := '(|' + d1 + d2 + ')';
    end; // function TlvkLDAPQuery.Boolean2LDAP.Evaluate.HandleOr

    function HandleAnd(i1, i2, ib: Integer): string;
    var
      d1,d2 : string;
    begin
      if (i1 = ib) or (i2 = ib) then
        raise Exception.Create(SErrAndMissingOperand);

      d1 := Evaluate(i1, ib-1);
      d2 := Evaluate(ib+1, i2);
      Result := '(&' + d1 + d2 + ')';
    end; // function TlvkLDAPQuery.Boolean2LDAP.Evaluate.HandleAnd

    function HandleNot(i1, i2, ib: Integer): string;
    var
      d2  : string;
    begin
      if (i1 <> ib) or (i2 = ib) then
        raise Exception.Create(SErrNotMissingOperand);

      d2 := Evaluate(ib+1, i2);
      Result := '(!' + d2 + ')';
    end; // function TlvkLDAPQuery.Boolean2LDAP.Evaluate.HandleNot

  begin
    Pri := 99;
    PriX := -1;
    i := i1;
    Level := 0;

    // Find split point
    while (i <= i2) do
    begin
      if (clParts[i] = '(') then
        Inc(Level)
      else if (clParts[i] = ')') then
        Dec(Level)
      else if (Level = 0) then
      begin
        FoundPri := 0;
        for j := Low(Priority) to High(Priority) do
          if (clParts[i] = Priority[j].Sym) then
          begin
            FoundPri := Priority[j].Pri;
            Break;
          end; // if we found the symbol

        if (FoundPri > 0) and (FoundPri < Pri) then
        begin
          Pri := FoundPri;
          PriX := i;
        end; // if better priority
      end; // if at base level

      Inc(i);
    end; // while more symbols to check

    // Decide what to do
    if (Pri < 99) then
    begin
      if (clParts[PriX] = '|') then
        Result := HandleOr(i1, i2, PriX)
      else
      if (clParts[PriX] = '&') then
        Result := HandleAnd(i1, i2, PriX)
      else if (clParts[PriX] = '!') then
        Result := HandleNot(i1, i2, PriX);
    end // if expression
    else begin
      if (i2 > i1) then
      begin
        if (clParts[i1] = '!') then
          Result := HandleNot(i1, i2, i1)
        else if (clParts[i1] = '(') and (clParts[i2] = ')') then
          Result := Evaluate(i1+1, i2-1)
        else
          raise Exception.Create(SErrExpressionParse);
      end // if must be sub-expression
      else
        Result := '(' + StringReplace(clParts[i1], '"', '', [rfReplaceAll]) + ')';
    end; // if not a expression
  end; // function TlvkLDAPQuery.Boolean2LDAP.Evaluate

begin
  Result := '';

  clParts := TStringList.Create;
  try
    if (Trim(sExpression) <> '') then
    begin
      CollapseExpression;
      SplitExpression;

      if (clParts.Count > 0) then
        Result := Evaluate(0, clParts.Count - 1);
    end; // if an expression was given
  finally
    clParts.Free;
  end; // try finally - destroy clParts object
end; // function TlvkLDAPQuery.Boolean2LDAP

constructor TlvkLDAPQuery.Create(AOwner: TComponent);
begin
  // First create inherited part
  inherited Create(AOwner);

  // Then initialize private members
  FResult := TlvkLDAPQueryResult.Create;
  FParameters := TlvkLDAPParameters.Create(Self);
  FFields := '';
  FQuery := '(objectClass=*)';
  FQueryFormat := qfLDAP;
  FScope := qsSubTree;
end; // constructor TlvkLDAPQuery.Create

destructor TlvkLDAPQuery.Destroy;
begin
  // First destroy private members
  Invalidate;
  FreeAndNil(FParameters);
  FreeAndNil(FResult);

  // Then destroy inherited part
  inherited;
end; // destructor TlvkLDAPQuery.Destroy

procedure TlvkLDAPQuery.Execute;
var
  TempQuery     : string;
  List          : TStrings;
  Index, rc     : Integer;
  FieldList     : array of PChar;
  FieldListPtr  : Pointer;
  Field         : string;
  Count         : Integer;
  QueryResult   : PLDAPMessage;
  Entry         : PLDAPMessage;
  Attribute     : PChar;
  Values        : PPCharArray;
  Element       : PBerElement;

  Item          : TlvkLDAPQueryResultItem;
  ItemAttribute : TlvkLDAPItemAttribute;
const
  ScopeValue    : array[TlvkLDAPQueryScope] of LongWord = (
    LDAP_SCOPE_BASE,
    LDAP_SCOPE_ONELEVEL,
    LDAP_SCOPE_SUBTREE
  );
begin
  // Assert data
  Assert(Assigned(Connection));
  Assert(Query <> '');

  // Do the search
  if (not Connection.Connected) then
  begin
    if (Connection.AutoConnect) then
      Connection.Connect
    else
      raise ElvkLDAP.Create(SNotOpen);
  end; // if connection not opened

  try
    Invalidate;
    SetLength(FieldList, 0);
    try
      if (Trim(FFields) <> '') then
      begin
        List := TStringList.Create;
        try
          List.Text := StringReplace(FFields, ';', #13#10, [rfReplaceAll]);
          Index := 0;
          SetLength(FieldList, List.Count + 1);
          Count := 0;
          while (Index < List.Count) do
          begin
            Field := Trim(List[Index]);
            if (Field <> '') then
            begin
              FieldList[Count] := StrNew(PChar(Field));
              Inc(Count);
            end; // if field set

            // Next field
            Inc(Index);
          end; // while more fields to copy

          // Set correct length of array
          FieldList[Count] := nil;
          SetLength(FieldList, Count + 1);
          FieldListPtr := @FieldList[0];
        finally
          FreeAndNil(List);
        end; // try finally - destroy list object
      end // if got list of fields
      else begin
        FieldList := nil;
        FieldListPtr := nil;
      end; // if no list of fields

      // Do the search
      TempQuery := ParseQuery(FQuery, GetParam);
      rc := ldap_search_s(Connection.pld, PCharOf(FBaseDN), ScopeValue[FScope],
        PCharOf(TempQuery), FieldListPtr, 0, @QueryResult);
      if (rc <> LDAP_NO_SUCH_OBJECT) then
      try
        LDAPCheck(rc);
        try
          Entry := ldap_first_entry(Connection.pld, QueryResult);
          while (Assigned(Entry)) do
          begin
            // Add new item to result collection
            Item := FResult.Add;

            // Get all attributes for this item
            Attribute := ldap_first_attribute(Connection.pld, Entry, Element);
            while (Assigned(Attribute)) do
            begin
              Values := PPCharArray(ldap_get_values(Connection.pld, Entry, Attribute));
              ItemAttribute := Item.Attributes.Add;
              ItemAttribute.Name := Attribute;
              if (Assigned(Values)) then
              try
                Index := 0;
                while (Assigned(Values[Index])) do
                begin
                  ItemAttribute.Values.Add(Values[Index]);
                  Inc(Index);
                end; // while more values for this attribute
              finally
                LDAPCheck(ldap_value_free(PPChar(Values)));
              end; // try finally - destroy values memory block

              // Next attribute
              Attribute := ldap_next_attribute(Connection.pld, Entry, Element);
            end; // while more attributes

            // Next entry
            Entry := ldap_next_entry(Connection.pld, Entry);
          end; // while more entries
        except
          Invalidate;
          raise;
        end; // try except - destroy result on failure
      finally
        LDAPCheck(ldap_msgfree(QueryResult));
      end; // try finally - clean up after search
    finally
      Index := 0;
      while (Index < Length(FieldList)) do
      begin
        StrDispose(FieldList[Index]);
        Inc(Index);
      end; // while more fields to dispose of
      SetLength(FieldList, 0);
    end; // try finally - clean up after search
  finally
    if (Connection.AutoDisconnect) then
      Connection.Disconnect;
  end; // try finally - make sure it's disconnected if that option is set
end; // procedure TlvkLDAPQuery.Execute

function TlvkLDAPQuery.GetParam(const ParamName: string): string;
begin
  Result := Parameters.ParamByName(ParamName).Value;
end; // function TlvkLDAPQuery.GetParam

function TlvkLDAPQuery.GetQueryString(const Index: Integer): string;
begin
  case Index of
    iQuery  : Result := FQuery;
    iFields : Result := FFields;
    iBaseDN : Result := FBaseDN;
  else
    raise ElvkLDAP.Create(SInvalidIndex);
  end; // case type of string to return
end; // function TlvkLDAPQuery.GetQueryString

function TlvkLDAPQuery.GetRealQuery: string;
begin
  Result := ParseQuery(FQuery, GetParam);
end; // function TlvkLDAPQuery.GetRealQuery

procedure TlvkLDAPQuery.Invalidate;
begin
  FResult.Clear;
end; // procedure TlvkLDAPQuery.Invalidate

function TlvkLDAPQuery.ParseQuery(const Query: string;
  const GetParam: TlvkLDAPGetParam): string;
var
  TempQuery : string;
  x         : Integer;
begin
  Result := '';
  if (QueryFormat = qfBoolean) then
    TempQuery := Boolean2LDAP(Query)
  else
    TempQuery := Query;
  while (TempQuery <> '') do
  begin
    x := Pos(':', TempQuery);
    if (x = 0) then
    begin
      Result := Result + TempQuery;
      TempQuery := '';
    end // if no more parameters
    else begin
      // First copy part before parameter
      Result := Result + Copy(TempQuery, 1, x-1);
      Delete(TempQuery, 1, x-1);

      // Check for double colon (::)
      if (Copy(TempQuery, 1, 2) = '::') then
      begin
        Result := Result + ':';
        Delete(TempQuery, 1, 2);
      end // if double colon
      else begin
        Delete(TempQuery, 1, 1);
        x := 1;
        while (x <= Length(TempQuery)) and (TempQuery[x] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
          Inc(x);
        Dec(x);
        Result := Result + GetParam(Copy(TempQuery, 1, x));
        Delete(TempQuery, 1, x);
      end; // if not double colon
    end; // if more parameters
  end; // while more to parse
end; // function TlvkLDAPQuery.ParseQuery

function TlvkLDAPQuery.RegisterParam(const ParamName: string): string;
begin
  with Parameters.Add do
  begin
    FName := ParamName;
    Value := '*';
  end; // with new parameter do
end; // function TlvkLDAPQuery.RegisterParam

procedure TlvkLDAPQuery.SetQueryFormat(const Value: TlvkLDAPQueryFormat);
begin
  if (Value <> FQueryFormat) then
  begin
    Invalidate;
    FQueryFormat := Value;
  end; // if new value
end; // procedure TlvkLDAPQuery.SetQueryFormat

procedure TlvkLDAPQuery.SetQueryString(const Index: Integer;
  const Value: string);
begin
  if (Value <> GetQueryString(Index)) then
  begin
    Invalidate;
    case Index of
      iQuery  : begin
        FParameters.Clear;
        FQuery := Value;
        ParseQuery(Value, RegisterParam);
      end; // if new query
      iFields : FFields := Value;
      iBaseDN : FBaseDN := Value;
    else
      raise ElvkLDAP.Create(SInvalidIndex);
    end; // case type of string to return
  end; // if new value
end; // procedure TlvkLDAPQuery.SetQueryString

procedure TlvkLDAPQuery.SetScope(const Value: TlvkLDAPQueryScope);
begin
  if (Value <> FScope) then
  begin
    Invalidate;
    FScope := Value;
  end; // if new scope
end; // procedure TlvkLDAPQuery.SetScope

{ TlvkLDAPQueryResult }

function TlvkLDAPQueryResult.Add: TlvkLDAPQueryResultItem;
begin
  Result := (inherited Add) as TlvkLDAPQueryResultItem;
end; // function TlvkLDAPQueryResult.Add

constructor TlvkLDAPQueryResult.Create;
begin
  // First create inherited part
  inherited Create(TlvkLDAPQueryResultItem);
end; // constructor TlvkLDAPQueryResult.Create

function TlvkLDAPQueryResult.FindItemID(ID: Integer): TlvkLDAPQueryResultItem;
begin
  Result := (inherited FindItemID(ID)) as TlvkLDAPQueryResultItem;
end; // function TlvkLDAPQueryResult.FindItemID

function TlvkLDAPQueryResult.GetItem(
  const Index: Integer): TlvkLDAPQueryResultItem;
begin
  Result := (inherited GetItem(Index)) as TlvkLDAPQueryResultItem;
end; // function TlvkLDAPQueryResult.GetItem

function TlvkLDAPQueryResult.Insert(Index: Integer): TlvkLDAPQueryResultItem;
begin
  Result := (inherited Insert(Index)) as TlvkLDAPQueryResultItem;
end; // function TlvkLDAPQueryResult.Insert

procedure TlvkLDAPQueryResult.SetItem(const Index: Integer;
  const Value: TlvkLDAPQueryResultItem);
begin
  inherited Items[Index] := Value;
end; // procedure TlvkLDAPQueryResult.SetItem

{ TlvkLDAPQueryResultItem }

constructor TlvkLDAPQueryResultItem.Create(Collection: TCollection);
begin
  // First create inherited part
  inherited;

  // Then initialize private members
  FAttributes := TlvkLDAPItemAttributes.Create;
end; // constructor TlvkLDAPQueryResultItem.Create

destructor TlvkLDAPQueryResultItem.Destroy;
begin
  // First destroy private members
  FreeAndNil(FAttributes);

  // Then destroy inherited part
  inherited;
end; // destructor TlvkLDAPQueryResultItem.Destroy

{ TlvkLDAPItemAttributes }

function TlvkLDAPItemAttributes.Add: TlvkLDAPItemAttribute;
begin
  Result := (inherited Add) as TlvkLDAPItemAttribute;
end; // function TlvkLDAPItemAttributes.Add

function TlvkLDAPItemAttributes.AttributeByName(
  const Name: string): TlvkLDAPItemAttribute;
var
  Index : Integer;
begin
  // Default result is "not found"
  Result := nil;

  // Find the attribute
  Index := 0;
  while (Index < Count) do
  begin
    if (CompareText(Items[Index].Name, Name) = 0) then
    begin
      Result := Items[Index];
      Break;
    end; // if item found
    Inc(Index);
  end; // while more attributes to check
end; // function TlvkLDAPItemAttributes.AttributeByName

constructor TlvkLDAPItemAttributes.Create;
begin
  // First create inherited part
  inherited Create(TlvkLDAPItemAttribute);
end; // constructor TlvkLDAPItemAttributes.Create

function TlvkLDAPItemAttributes.FindItemID(ID: Integer): TlvkLDAPItemAttribute;
begin
  Result := (inherited FindItemID(ID)) as TlvkLDAPItemAttribute;
end; // function TlvkLDAPItemAttributes.FindItemID

function TlvkLDAPItemAttributes.GetItem(
  const Index: Integer): TlvkLDAPItemAttribute;
begin
  Result := (inherited GetItem(Index)) as TlvkLDAPItemAttribute;
end; // function TlvkLDAPItemAttributes.GetItem

function TlvkLDAPItemAttributes.Insert(Index: Integer): TlvkLDAPItemAttribute;
begin
  Result := (inherited Insert(Index)) as TlvkLDAPItemAttribute;
end; // function TlvkLDAPItemAttributes.Insert

procedure TlvkLDAPItemAttributes.SetItem(const Index: Integer;
  const Value: TlvkLDAPItemAttribute);
begin
  inherited Items[Index] := Value;
end; // procedure TlvkLDAPItemAttributes.SetItem

{ TlvkLDAPItemAttribute }

constructor TlvkLDAPItemAttribute.Create(Collection: TCollection);
begin
  // First create inherited part
  inherited;

  // Then initialize private members
  FName := '';
  FValues := TStringList.Create;
end; // constructor TlvkLDAPItemAttribute.Create

destructor TlvkLDAPItemAttribute.Destroy;
begin
  // First destroy private members
  FreeAndNil(FValues);

  // Then destroy inherited part
  inherited;
end; // destructor TlvkLDAPItemAttribute.Destroy

{ TlvkLDAPParameters }

function TlvkLDAPParameters.Add: TlvkLDAPParameter;
begin
  Result := (inherited Add) as TlvkLDAPParameter;
end; // function TlvkLDAPParameters.Add

constructor TlvkLDAPParameters.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TlvkLDAPParameter);
end; // constructor TlvkLDAPParameters.Create

function TlvkLDAPParameters.FindItemID(ID: Integer): TlvkLDAPParameter;
begin
  Result := (inherited FindItemID(ID)) as TlvkLDAPParameter;
end; // function TlvkLDAPParameters.FindItemID

function TlvkLDAPParameters.GetIndexedParameter(const Name: string): string;
begin
  Result := ParamByName(Name).Value;
end; // function TlvkLDAPParameters.GetIndexedParameter

function TlvkLDAPParameters.GetItem(
  const Index: Integer): TlvkLDAPParameter;
begin
  Result := (inherited GetItem(Index)) as TlvkLDAPParameter;
end; // function TlvkLDAPParameters.GetItem

function TlvkLDAPParameters.Insert(Index: Integer): TlvkLDAPParameter;
begin
  Result := (inherited Insert(Index)) as TlvkLDAPParameter;
end; // function TlvkLDAPParameters.Insert

function TlvkLDAPParameters.ParamByName(const Name: string): TlvkLDAPParameter;
var
  Index : Integer;
begin
  // Default result is "not found"
  Result := nil;

  // Find the attribute
  Index := 0;
  while (Index < Count) do
  begin
    if (CompareText(Items[Index].Name, Name) = 0) then
    begin
      Result := Items[Index];
      Break;
    end; // if item found
    Inc(Index);
  end; // while more parameters to check
end; // function TlvkLDAPParameters.ParamByName

procedure TlvkLDAPParameters.SetIndexedParameter(const Name, Value: string);
begin
  ParamByName(Name).Value := Value;
end; // procedure TlvkLDAPParameters.SetIndexedParameter

procedure TlvkLDAPParameters.SetItem(const Index: Integer;
  const Value: TlvkLDAPParameter);
begin
  inherited Items[Index] := Value;
end; // procedure TlvkLDAPParameters.SetItem

{ TlvkLDAPParameter }

constructor TlvkLDAPParameter.Create(Collection: TCollection);
begin
  // First create inherited part
  inherited;

  // Then initialize private members
  FName := '';
  FValue := '*';
end; // constructor TlvkLDAPParameter.Create

function TlvkLDAPParameter.GetDisplayName: string;
begin
  Result := FName;
end; // function TlvkLDAPParameter.GetDisplayName

procedure TlvkLDAPParameter.SetName(const Value: string);
begin
  // Do nothing, not allowed to change the name but we need this method
end; // procedure TlvkLDAPParameter.SetName

procedure TlvkLDAPParameter.SetValue(const Value: string);
begin
  if (FValue <> Value) then
  begin
    FValue := Value;
    // Inform ldap query component of change
  end; // if new value
end; // procedure TlvkLDAPParameter.SetValue

end. // unit lvkLDAP
//
// $History: lvkLDAP.pas $
// 
// *****************  Version 8  *****************
// User: Lasse V. Karlsen Date: 16.04.03   Time: 10:50
// Updated in $/Components/LVK/source
// Added DIRECTIVES.INC file to all source code
// 
// *****************  Version 7  *****************
// User: Lasse V. Karlsen Date: 2.11.02    Time: 22:27
// Updated in $/Components/LVK/source
// 
// *****************  Version 6  *****************
// User: Lasse V. Karlsen Date: 2.11.02    Time: 21:50
// Updated in $/Components/LVK/source
// - Updated documentation
// - Fixed some minor bugs
// 
// *****************  Version 5  *****************
// User: Lasse V. Karlsen Date: 27.10.02   Time: 22:24
// Updated in $/Components/LVK/source
// - Updated documentation
// 
// *****************  Version 4  *****************
// User: Lasse V. Karlsen Date: 24.10.02   Time: 7:36
// Updated in $/Components/LVK/source
// - Added todos about updating documentation
// 
// *****************  Version 3  *****************
// User: Lasse V. Karlsen Date: 8.03.02    Time: 16:59
// Updated in $/Components/LVK/Source
// 
// *****************  Version 1  *****************
// User: Lasse V. Karlsen Date: 2.03.02    Time: 20:18
// Created in $/Components/LVK/Source
// LVK Components for Delphi 5 and 6
// 
// *****************  Version 2  *****************
// User: Lasse V. Karlsen Date: 9.02.02    Time: 17:42
// Updated in $/Components/lvkComponents/Source
// 
// *****************  Version 1  *****************
// User: Lasse V. Karlsen Date: 27.01.02   Time: 22:01
// Created in $/Components/lvkComponents/Source
// 
// *****************  Version 1  *****************
// User: Lasse        Date: 19.08.00   Time: 10:35
// Created in $/lvk_d5/Source
//

