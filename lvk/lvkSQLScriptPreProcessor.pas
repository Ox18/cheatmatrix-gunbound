{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the SQL script preprocessor class.
}
unit lvkSQLScriptPreProcessor;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 17.04.03 15:00 $
// $Archive: /Components/LVK/Source/lvkSQLScriptPreProcessor.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, lvkActiveScriptPreProcessor;

type
  { Description:
      This script preprocessor handles VBScript and J(ava)Script code, and
      adds support for "inline sql" to the language.

      The preprocessors will look for the given patterns in the script code:

        # connect...
        # disconnect...
        # use...
        # begin...
        # commit...
        # abort...
        # for each row
        # next for
        # &lt;sql&gt;
        $(...)

      The preprocessor will replace patterns found with calls to a global object
      that handles the sql code through ado. Typical use is to use the
      # connect statement to connect to a database, use one or more of the
      other statements to extract or modify data in the database, and finally
      use the # disconnect statement to disconnect from the database.

      The correct syntax, and meaning, for each one is given below. Parts in
      &lt;...&gt; means that instead of &lt;...&gt; you write a specific name
      or expression, which is explained in the text. Parts inside [...] are
      optional. Parts separated by a bar | are mutually exclusive, you choose
      one.

      In all sql statements, you can use parameters to specify values. You
      can use two different formats for this:

        :name
        :(expression)

      The first form will be replaced by the value of the script variable with
      the given name. The other form will be replaced by the value of the
      script expression.

      Examples:

        select * from persons where person_id = :id

      In this select statement, the parameter :id will be replaced with the
      given id stored in the script variable with the name 'id'.

      In this sql statement:

        select * from persons where person_id = :(id+1)

      You will get the person with the next higher id, provided such a person
      exists in the database. Note that this is full parameter support, and
      not string concatenation, so this will work without modification:

        select * from logfiles where timestamp >= :(Now-1/24)

      This will fetch all records from the logfiles table where the timestamp
      is less than one hour in the past. A string lookup will look like this:

        select * from persons where FirstName = :name

      As you can see, you don't need to quote the :name parameter in any way
      to make sure you provide legal sql.

      Here's the documentation for the statements provided by this preprocessor.
      All the # statements are given on a single line by themselves.

      # connect &lt;id&gt; = "&lt;connectionstring here&gt;"
      # connect &lt;id&gt; = '&lt;connectionstring here&gt;'
      # connect &lt;id&gt; = &lt;script expression&gt;
      # connect &lt;id&gt;

        The connect statement opens up a new connection to a database. With
        the first two forms, you can provide a ado connectionstring directly
        in the statement. The third form allows you to write any script
        expression that will give a oonnectionstring (like a functioncall,
        the name of a variable or constant, etc.). The fourth form will use
        the id of the connection as a variable name as well and read the
        connectionstring from that variable. The id is used internally as you
        can have more than one connection open at any given time.

        Examples:

          # connect dbContacts = GetContactsConnectionString
          # connect dbPersons = 'Provider=MSSQL.1;Database=Test'
          # connect dbSystems

      # disconnect &lt;id&gt;
      # disconnect *

        The disconnect statement will disconnect one or all of the open
        connections. The first form will disconnect only the given connection,
        the second form will disconnect all open connections. No error is
        given if you try to disconnect all with the * and no open connections
        exists.

        Examples:

          # disconnect dbContacts
          # disconnect *

      # begin [trans[action]] [in] [&lt;id&gt;|*]

        The begin statement will start a new transaction on the given
        connection. The word transaction (which can be shortened to trans)
        is optional, and the keyword in is also optional. If you specify a
        connection id, a transaction on that connection will be started.
        If you don't specify a connection id, or specify *, all connections
        will start a new transaction.

        Examples:

          # begin transaction in dbContacts
          # begin trans in dbContacts
          # begin trans dbContacts
          # begin dbContacts
          # begin *
          # begin

      # commit [trans[action]] [in] [&lt;id&gt;|*]

        The syntax for this is the same as for #begin, only this statement will
        commit the transaction(s) in the given connection(s).

        Examples:

          # commit transaction in dbContacts
          # commit trans in dbContacts
          # commit trans dbContacts
          # commit dbContacts
          # commit *
          # commit

      # abort|rollback [trans[action]] [in] [&lt;id&gt;|*]

        The syntax for this is the same as for #begin, only this statement will
        abort/rollback the transaction(s) in the given connection(s). You can
        choose if you want to use the keyword abort or rollback for this
        statement.

          # abort transaction in dbContacts
          # abort trans in dbContacts
          # abort trans dbContacts
          # abort dbContacts
          # rollback *
          # rollback

      # use &lt;id&gt;

        Whenever you issue a sql statement without qualifying it with a
        connection id, the default connection is used. If you create a new
        connection without having a default connection, the newly created
        connection will be the default. When you disconnect a connection and
        that connection was the default, you will have no default connection
        until you tell the system to use a different one, or create a new
        connection again.

        You can override which connection to use in each of the sql statements,
        but unqualified statemens will use the default. The # use statement will
        change what the default connection is to the one with the given id.

        Examples:

          # use dbContacts

      # [in &lt;id&gt;] for each row @&lt;name&gt; in (&lt;sql&gt;)
      # next row

        This provides a loop that runs through all the records returned by the
        sql code given. The sql code must thus return a set of records, and
        is typically a select statement or a call to a stored procedure which
        returns a recordset.

        The @&lt;name&gt; will be used as a variable referring to the current record
        inside the loop, and you can use the dot syntax to get access to
        column values in the current row. The recordset is fully updateable,
        if possible, meaning you can change the values of the columns, and the
        changes will be persisted to the database unless you issue a # abort
        statement on the connection.

        The [in &lt;id&gt;] part is optional. If left out, the default connection is
        used, and if provided controls which specific connection to issue
        the sql code to.

        Note: You can nest these loops, but you must make sure you don't
          re-use the name of the @&lt;name&gt; variable when you nest. You can re-use
          when you write a whole new loop which won't execute at the same time
          though.

        Note: The @&lt;name&gt; variable is actually a reference to the ado recordset
          object that is used to loop through the records. As such, if you
          pass this on to a procedure through its parameters, use the methods
          and abilities of a ADO RecordSet object to use it fully.

        Examples:

          # for each row @person in (select * from persons)
            ProcessPerson @person.FirstName, @person.LastName
          # next row

          # in dbContacts for each row @contact in (select * from Contacts)
            ProcessContact @contact
          # next row

      # for each row @&lt;name&gt; in $([&lt;id&gt;,]&lt;sql&gt;)
      # next row

        This is an alternate way of writing the for each statement. Instead
        of using the [in &lt;id&gt;] part, you use the $(...) syntax which will be
        described below. In that syntax, you can write the id of the connection
        as part of the sql statement. Here are the above examples in this
        alternate syntax:

          # for each row @person in $(select * from persons)
            ProcessPerson @person.FirstName, @person.LastName
          # next row

          # for each row @contact in $(dbContacts, select * from Contacts)
            ProcessContact @contact
          # next row

      # &lt;sql&gt;

        This statement allows you to write any sql statement you wish, and
        it will be passed on to the database withour question. Examples on
        this is delete and update statements.

        # update persons set Address = :Address where Person_ID = :id
        # delete from Contacts where Contact_ID = :id

      $([<id>,]<sql>)

        This syntax is more like a "sql function" than a "sql statement". You
        can use this inline in expressions to do a call to the sql database
        and get back some results.

        The result is either a recordset or a single value. The rule for what
        is returns is as follows:

          - If the result has more than one row, the result is returned as a
            recordset
          - If the result has more than one column, the result is returned as
            a recordset
          - If the result has named columns, the result is returned as a
            recordset
          - If the result has one column, one row, and the column is not named,
            the value in that first row is returned as a single value

        The result, if returned as a recordset, is returned as a ADBO RecordSet
        object, so you can use all the methods and properties found on that
        object to manipulate it.

        Examples:

          If $(dbContacts, select count(*) from Contacts) > 0 Then
            MsgBox "There is still contacts in the table"
          End If

          If $(select FirstName + '' from Contacts where id = :id) = "Lasse" Then
            MsgBox "First person named Lasse"
          End If

          Set Everyone = $(select * from persons)
          Everyone.MoveFirst
          While Not Everyone.EOF
            MsgBox "Hey " & Everyone("Firstname")
            Everyone.MoveNext
          Wend
  }
  TSQLScriptPreProcessor = class(TScriptPreProcessorHandler)
  private
    FLoops      : TStrings;
    FVariables  : TStrings;

    function PreProcessSQLLine(const Language, SQL: string): string;
    function ChangeRowVariable(const Language, SQL: string): string;
    function PreProcessInlineSQL(const Language, Line: string): string;

  public
    constructor Create; override;
    destructor Destroy; override;

    property Loops: TStrings read FLoops;
    property Variables: TStrings read FVariables;

    procedure Execute(const Language: String; const Code: TStrings); override;
    class function HandlesLanguage(const Language: String): Boolean; override;
  end;

  ESQLScriptPreProcessor = class(Exception);

implementation

uses
  lvkRegExp;

const
  GlobalSQLObject = 'lvkSQLGlobal';

type
  TParameterList = array of string;
  TSQLFunction = function(const PreProcessor: TSQLScriptPreProcessor;
    const Language, SQL: string): string;

function MakeFunctionCall(
  const Language, MethodName: string; const Parameters: array of string;
  const IsFunction: Boolean=False): string;
var
  JavaScript  : Boolean;
  Index       : Integer;
begin
  Result := Format('%s.%s', [GlobalSQLObject, MethodName]);

  if RegExpMatch('^J(ava)?Script$', Language) then
  begin
    JavaScript := True;
    Result := Result + '(';
  end else begin
    JavaScript := False;
    if IsFunction then
      Result := Result + '('
    else
      Result := Result + ' ';
  end;

  for Index := Low(Parameters) to High(Parameters) do
  begin
    Result := Result + Parameters[Index];
    if Index < High(Parameters) then
      Result := Result + ', ';
  end;

  if JavaScript then
  begin
    if IsFunction then
      Result := Result + ')'
    else
      Result := Result + ');'
  end else if IsFunction then
    Result := Result + ')';
end;

function QuoteString(const Language, Value: string): string;
begin
  if RegExpMatch('^J(ava)?Script$', Language) then
    Result := '''' + RegExpStringReplace(Value, '''', '\''') + ''''
  else
    Result := '"' + RegExpStringReplace(Value, '"', '""') + '"';
end;

function FixupLoopVar(const s: string): string;
begin
  Assert(s[1] = '@');
  Result := 'lvkrow' + Copy(s, 2, Length(s)-1);
end;

function MakeSQLParameters(const Language: string; var SQL: string;
  const Parameters: array of string): TParameterList;
var
  Index : Integer;
  Start : Integer;
  Level : Integer;
  Quote : Char;

  procedure AddParameter;
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := Copy(SQL, Start, Index-Start);

    Delete(SQL, Start-1, Index-Start+1);
    Insert('?', SQL, Start-1);

    Index := Start-1;
    Start := 0;
  end;

begin
  SetLength(Result, Length(Parameters));
  for Index := 0 to Length(Parameters)-1 do
    Result[Index] := Parameters[Index];

  Start := 0;
  Index := 1;
  Level := 0;
  Quote := #0;
  while Index <= Length(SQL) do
  begin
    case SQL[Index] of
      '''', '"':
        if Quote = #0 then
          Quote := SQL[Index]
        else if Quote = SQL[Index] then
          Quote := #0;

      ':':
        if (Quote = #0) and (Start = 0) and (Level = 0) then
        begin
          Start := Index+1;
          Level := 0;
        end;

      #32, #9, #13, #10, ',':
        if (Start > 0) and (Quote = #0) and (Level = 0) then
          AddParameter;

      '(':
        if (Start > 0) and (Quote = #0) then
          Inc(Level);

      ')':
        if (Start > 0) and (Quote = #0) then
        begin
          if Level = 0 then
            AddParameter
          else
            Dec(Level);
        end;
    end;

    Inc(Index);
  end;
  if Start > 0 then
    AddParameter;
end;

function SQL_CONNECT(const PreProcessor: TSQLScriptPreProcessor;
  const Language, SQL: string): string;
var
  Name              : string;
  ConnectionString  : string;
begin
  Result := '';

  if RegExpScanf('^connect\s+([a-zA-Z0-9_]+)$', SQL,
    ['STRING', @Name]) then
    Result := MakefunctionCall(Language, 'Connect',
      [QuoteString(Language, Name), Name])
  else if RegExpScanf('^connect\s+([a-zA-Z0-9_]+)\s*=\s*(''|")(.+)\2$', SQL,
    ['STRING', @Name, '3:STRING', @ConnectionString]) then
    Result := MakefunctionCall(Language, 'Connect',
      [QuoteString(Language, Name), QuoteString(Language, ConnectionString)])
  else if RegExpScanf('^connect\s+([a-zA-Z0-9_]+)\s*=\s*(.+)$', SQL,
    ['STRING', @Name, 'STRING', @ConnectionString]) then
    Result := MakefunctionCall(Language, 'Connect',
      [QuoteString(Language, Name), ConnectionString]);
end;

function SQL_DISCONNECT(const PreProcessor: TSQLScriptPreProcessor;
  const Language, SQL: string): string;
var
  Name  : string;
begin
  Result := '';

  if RegExpScanf('^disconnect\s+([a-zA-Z0-9_]+)$', SQL,
    ['STRING', @Name]) then
    Result := MakefunctionCall(Language, 'Disconnect',
      [QuoteString(Language, Name)])
  else if RegExpScanf('^disconnect\s+(\*|all)$', SQL,
    ['STRING', @Name]) then
  begin
    if CompareText(Name, 'all') = 0 then
      Name := '*';
    Result := MakefunctionCall(Language, 'Disconnect',
      [QuoteString(Language, Name)])
  end else if RegExpMatch('^disconnect$', SQL) then
    Result := MakefunctionCall(Language, 'Disconnect',
      [QuoteString(Language, '*')])
end;

function SQL_FOREACH(const PreProcessor: TSQLScriptPreProcessor;
  const Language, SQL: string): string;
var
  VariableName  : string;
  DSNName       : string;
  SQLStatement  : string;
  ForEach       : Boolean;
  Parameters    : TParameterList;
begin
  Result := '';
  Parameters := nil;

  DSNName := '';

  if RegExpScanf('^for\s+each\s+row\s+(@[a-zA-Z_0-9]+)\s+in\s+\((.+)\)$', SQL,
    ['STRING', @VariableName, 'STRING', @SQLStatement]) then
    ForEach := True
  else if RegExpScanf('^for\s+each\s+row\s+(@[a-zA-Z_0-9]+)\s+in\s+\$\(([a-zA-Z0-9]+),\s*(.+)\)$', SQL,
    ['STRING', @VariableName, 'STRING', @DSNName, 'STRING', @SQLStatement]) then
    ForEach := True
  else if RegExpScanf('^for\s+each\s+row\s+(@[a-zA-Z_0-9]+)\s+in\s+\$\((.+)\)$', SQL,
    ['STRING', @VariableName, 'STRING', @SQLStatement]) then
    ForEach := True
  else if RegExpScanf('^in\s+([a-zA-Z_0-9]+)\s+for\s+each\s+row\s+(@[a-zA-Z_0-9]+)\s+in\s+\((.+)\)$', SQL,
    ['STRING', @DSNName, 'STRING', @VariableName, 'STRING', @SQLStatement]) then
    ForEach := True
  else
    ForEach := False;

  if ForEach then
  begin
    if PreProcessor.Loops.IndexOf(VariableName) >= 0 then
      raise ESQLScriptPreProcessor.Create('You must use different variable names when nesting # for each calls');

    SQLStatement := Trim(SQLStatement);

    if RegExpMatch('^J(ava)?Script$', Language) then
    begin
      Parameters := MakeSQLParameters(Language, SQLStatement,
        [QuoteString(Language, DSNName),
         QuoteString(Language, SQLStatement)]);
      Parameters[1] := QuoteString(Language, SQLStatement);

      PreProcessor.Variables.Add(FixupLoopVar(VariableName));
      Result := FixupLoopVar(VariableName) + ' = ' +
        MakeFunctionCall(Language, 'StartForEach', Parameters,
          True) + ';'#13#10'while (!' + FixupLoopVar(VariableName) + '.EOF) {';
    end else begin
      Parameters := MakeSQLParameters(Language, SQLStatement,
        [QuoteString(Language, DSNName),
         QuoteString(Language, SQLStatement)]);
      Parameters[1] := QuoteString(Language, SQLStatement);

      PreProcessor.Variables.Add(FixupLoopVar(VariableName));
      Result := 'Set ' + FixupLoopVar(VariableName) + ' = ' +
        MakeFunctionCall(Language, 'StartForEach', Parameters,
          True) + #13#10'Do While Not ' + FixupLoopVar(VariableName) + '.EOF';
    end;
    PreProcessor.Loops.Add(VariableName);
    PreProcessor.Variables.Add(FixupLoopVar(VariableName));
  end;
end;

function SQL_NEXT(const PreProcessor: TSQLScriptPreProcessor;
  const Language, SQL: string): string;
var
  LoopName  : string;
begin
  if RegExpMatch('^next\s+row$', SQL) then
  begin
    if PreProcessor.Loops.Count = 0 then
      raise ESQLScriptPreProcessor.Create('# next without # for each');
    LoopName := PreProcessor.Loops[PreProcessor.Loops.Count-1];
    PreProcessor.Loops.Delete(PreProcessor.Loops.Count-1);

    if RegExpMatch('^J(ava)?Script$', Language) then
      Result := FixupLoopVar(LoopName) + '.MoveNext();'#13#10'}'
    else
      Result := FixupLoopVar(LoopName) + '.MoveNext'#13#10'Loop';
  end;
end;

function SQL_BEGIN_TRANSACTION(const PreProcessor: TSQLScriptPreProcessor;
  const Language, SQL: string): string;
var
  Name  : string;
begin
  if RegExpMatch('^begin(\s+trans(action)?)?((\s+in)?\s+\*)?$', SQL) then
    Result := MakeFunctionCall(Language, 'BeginTransaction',
      [QuoteString(Language, '*')])
  else if RegExpScanf('^begin(\s+trans(action)?)?(\s+in)?\s+([a-zA-Z0-9]+)$', SQL,
    ['4:STRING', @Name]) then
    Result := MakeFunctionCall(Language, 'BeginTransaction',
      [QuoteString(Language, Name)])
  else
    Result := '';
end;

function SQL_COMMIT_TRANSACTION(const PreProcessor: TSQLScriptPreProcessor;
  const Language, SQL: string): string;
var
  Name  : string;
begin
  if RegExpMatch('^commit(\s+trans(action)?)?((\s+in)?\s+\*)?$', SQL) then
    Result := MakeFunctionCall(Language, 'CommitTransaction',
      [QuoteString(Language, '*')])
  else if RegExpScanf('^commit(\s+trans(action)?)?(\s+in)?\s+([a-zA-Z0-9]+)$', SQL,
    ['4:STRING', @Name]) then
    Result := MakeFunctionCall(Language, 'CommitTransaction',
      [QuoteString(Language, Name)])
  else
    Result := '';
end;

function SQL_ABORT_TRANSACTION(const PreProcessor: TSQLScriptPreProcessor;
  const Language, SQL: string): string;
var
  Name  : string;
begin
  if RegExpMatch('^(abort|rollback)(\s+trans(action)?)?((\s+in)?\s+\*)?$', SQL) then
    Result := MakeFunctionCall(Language, 'AbortTransaction',
      [QuoteString(Language, '*')])
  else if RegExpScanf('^(abort|rollback)(\s+trans(action)?)?(\s+in)?\s+([a-zA-Z0-9]+)$', SQL,
    ['5:STRING', @Name]) then
    Result := MakeFunctionCall(Language, 'AbortTransaction',
      [QuoteString(Language, Name)])
  else
    Result := '';
end;

function SQL_SQL(const PreProcessor: TSQLScriptPreProcessor;
  const Language, SQL: string): string;
var
  Name        : string;
  Statement   : string;
  Parameters  : TParameterList;
begin
  if RegExpScanf('^in\s+([a-zA-Z_0-9]+)\s+(.*)$', SQL,
    ['STRING', @Name, 'STRING', @Statement]) then
    Result := MakeFunctionCall(Language, 'ExecuteSQL',
      [QuoteString(Language, Name), QuoteString(Language, SQL)])
  else begin
    Name := '';
    Statement := SQL;
  end;

  Parameters := MakeSQLParameters(Language, Statement,
    [QuoteString(Language, ''), QuoteString(Language, Statement)]);

  Parameters[1] := QuoteString(Language, Statement);
  Result := MakeFunctionCall(Language, 'ExecuteSQL', Parameters);
end;

const
  SQLFunctions  : array[1..8] of TSQLFunction = (
    SQL_CONNECT,
    SQL_DISCONNECT,
    SQL_FOREACH,
    SQL_NEXT,
    SQL_BEGIN_TRANSACTION,
    SQL_COMMIT_TRANSACTION,
    SQL_ABORT_TRANSACTION,
    SQL_SQL
  );

{ TVBScriptPreProcessor }

function TSQLScriptPreProcessor.ChangeRowVariable(const Language,
  SQL: string): string;
begin
  Result := RegExpStringReplace(SQL, '@([a-zA-Z_0-9]+)\.([a-zA-Z0-9_]+)', 'lvkrow{1}("{2}")', True, True);
end;

constructor TSQLScriptPreProcessor.Create;
begin
  inherited;

  FLoops := TStringList.Create;
  FVariables := TStringList.Create;
  TStringList(FVariables).Duplicates := dupIgnore;
  TStringList(FVariables).Sorted := True;
end;

destructor TSQLScriptPreProcessor.Destroy;
begin
  FVariables.Free;
  FLoops.Free;

  inherited;
end;

procedure TSQLScriptPreProcessor.Execute(const Language: String;
  const Code: TStrings);
var
  Index   : Integer;
  Index2  : Integer;
  Line    : string;

  procedure UpdateLine;
  var
    Lines   : TStrings;
    Prefix  : string;
    Index2  : Integer;
  begin
    if Line = '' then
    begin
      Inc(Index);
      Exit;
    end;

    Lines := TStringList.Create;
    try
      Lines.Text := Line;
      if Lines.Count = 0 then
        Code.Delete(Index)
      else begin
        Code[Index] := Lines[0];
        if not RegExpScanf('^(\s+)', Lines[0], ['STRING', @Prefix]) then
          Prefix := '';
        Inc(Index);

        for Index2 := 1 to Lines.Count-1 do
          Code.Insert(Index-1+Index2, Prefix + Lines[Index2]);
      end;
    finally
      Lines.Free;
    end;
  end;

begin
  Index := 0;
  while Index < Code.Count do
  begin
    Line := Code[Index];
    if RegExpMatch('^\s*#', Line) then
    begin
      Line := PreProcessSQLLine(Language, Line);
      UpdateLine;
    end else if RegExpMatch('@[a-zA-Z_0-9]+\.', Line) then
    begin
      Line := ChangeRowVariable(Language, Line);
      UpdateLine;
    end else if RegExpMatch('\$\(', Line) then
    begin
      Line := PreProcessInlineSQL(Language, Line);
      UpdateLine;
    end else
      Inc(Index);
  end;

  if RegExpMatch('^J(ava)?Script$', Language) then
    for Index := 0 to FVariables.Count-1 do
      Code.Insert(0, 'var ' + FVariables[Index] + ';')
  else begin
    for Index := 0 to Code.Count-1 do
      if RegExpMatch('Option\s+Explicit', Code[Index]) then
      begin
        for Index2 := 0 to FVariables.Count-1 do
          Code.Insert(Index+1, 'Dim ' + FVariables[Index2]);
        Exit;
      end;

    for Index2 := 0 to FVariables.Count-1 do
      Code.Insert(0, 'Dim ' + FVariables[Index2]);
  end;
end;

class function TSQLScriptPreProcessor.HandlesLanguage(
  const Language: String): Boolean;
begin
  Result := RegExpMatch('^(VBSCRIPT|J(AVA)?SCRIPT)$', Language);
end;

function TSQLScriptPreProcessor.PreProcessInlineSQL(const Language,
  Line: string): string;
var
  Index : Integer;
  Quote : Char;
  c     : Char;
  Start : Integer;
  Level : Integer;

  procedure AddSQL;
  var
    InlineSQL   : string;
    Name        : string;
    SQL         : string;
    Parameters  : TParameterList;
  begin
    InlineSQL := Copy(Line, Start+2, Index-Start-2);

    if not RegExpScanf('^([a-zA-Z0-9_]+),\s+(.*)$', InlineSQL,
      ['STRING', @Name, 'STRING', @SQL]) then
    begin
      Name := '';
      SQL := InlineSQL;
    end;

    Parameters := MakeSQLParameters(Language, SQL,
      [QuoteString(Language, Name), QuoteString(Language, SQL)]);
    Parameters[1] := QuoteString(Language, SQL);

    Result := Result + MakeFunctionCall(Language, 'CallSQL', Parameters, True);
  end;

begin
  Quote := #0;
  Index := 1;
  Result := '';
  Level := 0;
  Start := 0;

  while Index <= Length(Line) do
  begin
    c := Line[Index];
    case c of
      '''', '"':
        begin
          if Start = 0 then
            Result := Result + c;
          if Quote = #0 then
            Quote := c
          else if Quote = c then
            Quote := #0;
        end;

      '$':
        if (Index < Length(Line)) and (Line[Index+1] = '(') then
          Start := Index;

      '(':
        begin
          if Start = 0 then
            Result := Result + c;

          if (Quote = #0) and (Start > 0) then
            Inc(Level);
        end;

      ')':
        begin
          if Start = 0 then
            Result := Result + c;

          if (Quote = #0) and (Start > 0) then
          begin
            Dec(Level);
            if (Level = 0) and (Start > 0) then
            begin
              AddSQL;
              Start := 0;
            end;
          end;
        end;
    else
      if Start = 0 then
        Result := Result + c;
    end;

    Inc(Index);
  end;
end;

function TSQLScriptPreProcessor.PreProcessSQLLine(const Language,
  SQL: string): string;
var
  Prefix    : string;
  Statement : string;
  Index     : Integer;
begin
  if not RegExpScanf('^(\s*)#\s*(.*)$', SQL, ['STRING', @Prefix, 'STRING', @Statement]) then
    Exit;

  Statement := Trim(Statement);
  for Index := Low(SQLFunctions) to High(SQLFunctions) do
  begin
    Result := SQLFunctions[Index](Self, Language, Statement);
    if Result <> '' then
    begin
      Result := Prefix + Result;
      Break;
    end;
  end;

  if Result = '' then
    Result := SQL; // this will trigger a script code error
end;

initialization
  RegisterPreProcessorHandler(TSQLScriptPreProcessor);
end.

