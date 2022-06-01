{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains base classes for form and component state saving
    components.
}
unit lvkState;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkState.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Controls, INIFiles, Forms, TypInfo;

type
  { Description:
      This is the base class for storages. This class does all the dirtywork
      of translating reading and writing of different data types into
      reading and writing of strings, to simplify creating descendant
      components.

      Note: Any class that descends from TlvkCustomStateStorage must implement
        at least WriteString, ReadString and DeleteValue, but will probably
        have to implement BeginJob@Boolean and EndJob as well.
  }
  TlvkCustomStateStorage = class(TComponent)
  protected
    function DoReadString(const Section, ID, Default: string): string;
      virtual; abstract;
    procedure DoWriteString(const Section, ID, Value: string);
      virtual; abstract;

  public
    { Description:
        When a state component starts restoring or saves information, it must
        first prepare the storage, by calling the BeginJob method. The
        storage in turn must open up physical storages in preparation for
        reading or writing data from/to them.

        Note: A class that descends from TlvkCustomStateStorage should
          implement this method.
      Parameters:
        WriteMode - This parameter will be True if the state component is going
          to save information, False if it's going to restore information.
      See also:
        EndJob
    }
    procedure BeginJob(const WriteMode: Boolean); virtual;

    { Description:
        This method is responsible for writing a single string value into the
        storage. Note that the value may contain special characters like
        linefeeds, delimiters, etc. and the method must be able to deal
        gracefully with that. Specifically, when the value is read back, all
        those special characters must be intact.

        Note: A class that descends from TlvkCustomStateStorage must
          implement this method.
      Parameters:
        SectionName - The sub-section of the storage to write the data to.
        ID - The identifier of this single string value.
        Value - The value to write.
      See also:
        ReadString
    }
    procedure WriteString(const SectionName, ID, Value: string);

    { Description:
        This function retrieves a single string value from the storage.

        Note: A class that descends from TlvkCustomStateStorage must
          implement this method.
      Parameters:
        SectionName - The sub-section of the storage to read the data from.
        ID - The identifier of the value to read.
        Default - If the value does not exist in the storage, this value
          will be returned instead.
      See also:
        WriteString
    }
    function ReadString(const SectionName, ID, Default: string): string;

    { Description:
        This method functions like the WriteString method, except it
        writes an integer value instead.
      See also:
        WriteString, ReadInteger
    }
    procedure WriteInteger(const SectionName, ID: string; const Value: Int64);

    { Description:
        This method functions like the ReadString method, except it
        reads an integer value instead.
      See also:
        ReadString, WriteInteger
    }
    function ReadInteger(const SectionName, ID: string;
      const Default: Int64=0): Integer;

    { Description:
        This method functions like the WriteString method, except it
        writes a floating ponit value instead.
      See also:
        WriteString, ReadFloat
    }
    procedure WriteFloat(const SectionName, ID: string; const Value: Extended);

    { Description:
        This method functions like the ReadString method, except it
        reads a floating point value instead.
      See also:
        ReadString, WriteFloat
    }
    function ReadFloat(const SectionName, ID: string;
      const Default: Extended=0.0): Extended;

    { Description:
        This method removes a named value from the storage, regardless of
        type. The value might not exist in the storage before this call, and
        the method must simply ignore the call then, and not raise an exception.

        Note: A class that descends from TlvkCustomStateStorage must
          implement this method.
      Parameters:
        SectionName - The sub-section of the storage to delete the value from.
        ID - The identifier of the value to delete.
    }
    procedure DeleteValue(const SectionName, ID: string); virtual; abstract;

    { Description:
        After the state component has finished saving or restoring the state,
        it will call EndJob to close the storage.

        Note: A class that descends from TlvkCustomStateStorage should
          implement this method.
      See also:
        BeginJob
    }
    procedure EndJob; virtual;
  end;

  TStateNotifyEvent = procedure(Sender: TObject;
    const Storage: TlvkCustomStateStorage) of object;

  { Description:
      This class implements the base framework for a state component that
      allows saving the state to a storage and later retrieving it. It will
      handle being placed on a form and calls the appropriate methods when
      the form is being loaded and closed in order to restore and save state.

      Note: Descendant classes must implement the DoSaveState and DoRestoreState
        methods.
  }
  TlvkCustomState = class(TComponent)
  private
    FHasSaved         : Boolean;
    FActive           : Boolean;
    FSectionName      : string;
    FStorage          : TlvkCustomStateStorage;
    FForm             : TCustomForm;
    FOnFormCreate     : TNotifyEvent;
    FOnFormDestroy    : TNotifyEvent;
    FOnFormCloseQuery : TCloseQueryEvent;
    FOnBeforeSave     : TStateNotifyEvent;
    FOnAfterSave      : TStateNotifyEvent;
    FOnBeforeRestore  : TStateNotifyEvent;
    FOnAfterRestore   : TStateNotifyEvent;

    procedure DoSaveState;
    procedure DoRestoreState;
    procedure SetSectionName(const Value: string);

    procedure FindForm;
    procedure EnableEvents;
    procedure DisableEvents;

    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    { Description:
        This method is called to save the state of whatever this component
        keeps track of.

        Use the storage and form properties to get access to the data. The
        storage has been opened before this method is called and is
        automatically closed afterwards, so this method should only save
        the values it needs to.

        Note: Descendant classes must implement this method.
      See also:
        RestoreState
    }
    procedure SaveState; virtual; abstract;

    { Description:
        This method is called to restore the state of whatever this component
        keeps track of.

        Use the storage and form properties to get access to the data. The
        storage has been opened before this method is called and is
        automatically closed afterwards, so this method should only save
        the values it needs to.

        Note: Descendant classes must implement this method.
      See also:
        SaveState
    }
    procedure RestoreState; virtual; abstract;

    { Description:
        These methods translates a call to write data into the storage into
        a call to the appropriate storage, with the section name specified for
        the component as the first parameter.
      Parameters:
        ID - The identifier of the value to save.
        Value - The value to write to the storage.
    }
    procedure WriteString(const ID, Value: string); virtual;

    // <COMBINE WriteString@string@string>
    procedure WriteInteger(const ID: string; const Value: Int64); virtual;
    // <COMBINE WriteString@string@string>
    procedure WriteFloat(const ID: string; const Value: Extended); virtual;
    // <COMBINE WriteString@string@string>
    procedure WriteEnumerated(const ID: string; const TypeInfo: PTypeInfo;
      const Value: Integer); virtual;

    { Description:
        This method translates a call to read data from the storage into
        a call to the appropriate storage, with the section name specified for
        the component as the first parameter.
      Parameters:
        ID - The identifier of the value to read.
        Default - The default value to return if the value does not exist in
          the storage.
    }
    function ReadString(const ID: string; const Default: string=''): string;
      virtual;
    // <COMBINE ReadString@string@string>
    function ReadInteger(const ID: string; const Default: Int64=0): Int64;
      virtual;
    // <COMBINE ReadString@string@string>
    function ReadFloat(const ID: string; const Default: Extended=0.0): Extended;
      virtual;
    // <COMBINE ReadString@string@string>
    function ReadEnumerated(const ID: string; const TypeInfo: PTypeInfo;
      const Default: Integer=0): Integer; virtual;

    { Description:
        This method deletes all the named identifiers from the storage, in
        the section specified by this component.

        Note: Internally this method translates to a series of calls to
          DeleteValue@string to delete each individual identifier.
      Parameters:
        IDs - A list of identifiers to delete in the current section.
      See also:
        DeleteValue
    }
    procedure DeleteValues(const IDs: array of string);

    { Description:
        This method deletes a single named identifier from the storage, in
        the section specified by this component.
      Parameters:
        ID - The identifier to delete from the storage.
      See also:
        DeleteValues
    }
    procedure DeleteValue(const ID: string);

    { Description:
        This property returns the form that this component has been dropped on.
    }
    property Form: TCustomForm read FForm;

    { Description:
        This property holds the storage responsible for keeping the values
        between sessions. Any component that inherits from
        TlvkCustomStateStorage will do.
    }
    property Storage: TlvkCustomStateStorage read FStorage write FStorage;

    { Description:
        This property specifies the sub-section of the storage to store the
        values in. Usually this translates to a name derived from the name of
        the underlying form in order to identify the values, but it can
        be overridden by the programmer if needed.
    }
    property SectionName: string read FSectionName write SetSectionName;

    { Description:
        This event handler is called once before the save process starts. The
        storage has been opened and is available specified by the parameter to
        the event, should any custom values need to be saved.
      See also:
        OnAfterSave, OnBeforeRestore, OnAfterRestore
    }
    property OnBeforeSave: TStateNotifyEvent read FOnBeforeSave write FOnBeforeSave;

    { Description:
        This event handler is called once after the save process has completed.
        The storage has been opened and is available specified by the
        parameter to the event, should any custom values need to be saved.
      See also:
        OnBeforeSave, OnBeforeRestore, OnAfterRestore
    }
    property OnAfterSave: TStateNotifyEvent read FOnAfterSave write FOnAfterSave;

    { Description:
        This event handler is called once before the restore process starts. The
        storage has been opened and is available specified by the parameter to
        the event, should any custom values need to be restored.
      See also:
        OnAfterRestore, OnBeforeSave, OnAfterSave
    }
    property OnBeforeRestore: TStateNotifyEvent read FOnBeforeRestore write FOnBeforeRestore;

    { Description:
        This event handler is called once after the restore process has
        completed. The storage has been opened and is available specified by
        the parameter to the event, should any custom values need to be
        restored.
      See also:
        OnBeforeRestore, OnBeforeSave, OnAfterSave
    }
    property OnAfterRestore: TStateNotifyEvent read FOnAfterRestore write FOnAfterRestore;

    { Description:
        Set this property to False to temporarily stop saving/restoring values
        to/from the storage. Note that even if this property is set to True,
        the component still needs a storage, and if any of these requirements
        are not filled, the component will fail silently.
    }
    property Active: Boolean read FActive write FActive default True;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  ElvkStateStorage = class(Exception);
  ElvkINIFileStateStorage = class(ElvkStateStorage);

  ElvkCustomState = class(Exception);

{ Description:
    These two functions does a halfway job of immitating how an url is encoded.
    Special characters are replaced by a % and their hexadecimal representation
    as the next two letters. A single % will be replaced by a double (%%) so as
    not to confuse the decode routine.
  Parameters:
    s - The data to encode or decode
  See also:
    SafeDecode
}
function SafeEncode(const s: string): string;
// <COMBINE SafeEncode@string>
function SafeDecode(const s: string): string;

const
  STATE_PROPERTY_KINDS = [tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkClass, tkWChar, tkLString, tkWString,
    tkInt64];

implementation

function SafeEncode(const s: string): string;
var
  Index : Integer;
begin
  Result := '';
  for Index := 1 to Length(s) do
    case s[Index] of
      '%':
        Result := Result + '%%';

      ' ', #13, #10, #9, #8, '=':
        Result := Result + '%' + IntToHex(Ord(s[Index]), 2);

    else
      Result := Result + s[Index];
    end;
end;

function SafeDecode(const s: string): string;
var
  Index : Integer;
  s2    : string;
begin
  Result := '';
  Index := 1;
  while Index <= Length(s) do
  begin
    case s[Index] of
      '%':
        begin
          Inc(Index);
          if Index <= Length(s) then
          begin
            case s[Index] of
              '%':
                begin
                  Result := Result + '%';
                  Inc(Index);
                end;

              'a'..'f', '0'..'9', 'A'..'F':
                begin
                  s2 := s[Index];
                  Inc(Index);
                  if s[Index] in ['a'..'f', '0'..'9', 'A'..'F'] then
                  begin
                    s2 := s2 + s[Index];
                    Inc(Index);
                    Result := Result + Chr(StrToInt('$' + s2));
                  end else
                    Break;
                end;
            else
              Break;
            end;
          end else
            Break;
        end;
    else
      begin
        Result := Result + s[Index];
        Inc(Index);
      end;
    end;
  end;
end;

{ TlvkCustomState }

constructor TlvkCustomState.Create(AOwner: TComponent);
begin
  inherited;

  FindForm;
  Assert(Assigned(FForm));

  FSectionName := FForm.Name + '.Components';
  FActive := True;
end;

procedure TlvkCustomState.DeleteValue(const ID: string);
begin
  FStorage.DeleteValue(FSectionName, ID);
end;

procedure TlvkCustomState.DeleteValues(const IDs: array of string);
var
  Index : Integer;
begin
  for Index := Low(IDs) to High(IDs) do
    DeleteValue(IDs[Index]);
end;

destructor TlvkCustomState.Destroy;
begin
  if not (csDesigning in ComponentState) then
    DisableEvents;

  inherited;
end;

procedure TlvkCustomState.DisableEvents;
var
  Form  : TForm;
begin
  Form := TForm(FForm);

  Form.OnDestroy := FOnFormDestroy;
  Form.OnCloseQuery := FOnFormCloseQuery;
  Form.OnCreate := FOnFormCreate;
end;

procedure TlvkCustomState.DoRestoreState;
begin
  if Assigned(FStorage) and Active then
  begin
    FStorage.BeginJob(False);
    try
      if Assigned(FOnBeforeRestore) then
        FOnBeforeRestore(Self, FStorage);

      RestoreState;

      if Assigned(FOnAfterRestore) then
        FOnAfterRestore(Self, FStorage);
    finally
      FStorage.EndJob;
    end;
  end;
end;

procedure TlvkCustomState.DoSaveState;
begin
  if Assigned(FStorage) and Active and (not FHasSaved) then
  begin
    FStorage.BeginJob(True);
    try
      if Assigned(FOnBeforeSave) then
        FOnBeforeSave(Self, FStorage);

      SaveState;

      if Assigned(FOnAfterSave) then
        FOnAfterSave(Self, FStorage);
    finally
      FStorage.EndJob;
    end;
    FHasSaved := True;
  end;
end;

procedure TlvkCustomState.EnableEvents;
var
  Form  : TForm;
begin
  Form := TForm(FForm);

  FOnFormDestroy := Form.OnDestroy;
  Form.OnDestroy := FormDestroy;

  FOnFormCloseQuery := Form.OnCloseQuery;
  Form.OnCloseQuery := FormCloseQuery;

  FOnFormCreate := Form.OnCreate;
  Form.OnCreate := FormCreate;
end;

procedure TlvkCustomState.FindForm;
var
  Component : TComponent;
begin
  Component := Owner;
  while Assigned(Component) and (not (Component is TCustomForm)) do
    Component := Component.Owner;

  if Assigned(Component) then
    FForm := Component as TCustomForm
  else
    FForm := nil;
end;

procedure TlvkCustomState.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if Assigned(FOnFormCloseQuery) then
    FOnFormCloseQuery(Sender, CanClose)
  else
    CanClose := True;

  if CanClose then
    DoSaveState;
end;

procedure TlvkCustomState.FormCreate(Sender: TObject);
begin
  if Assigned(FOnFormCreate) then
    FOnFormCreate(Sender);

  DoRestoreState;
end;

procedure TlvkCustomState.FormDestroy(Sender: TObject);
begin
  try
    DoSaveState;
  except
    Application.HandleException(Self);
  end;

  if Assigned(FOnFormDestroy) then
    FOnFormDestroy(Sender);
end;

procedure TlvkCustomState.Loaded;
var
  NeedEvents  : Boolean;
begin
  NeedEvents := (csLoading in ComponentState);

  inherited;

  if (not (csDesigning in ComponentState)) and NeedEvents then
    EnableEvents;
end;

procedure TlvkCustomState.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FStorage) then
    FStorage := nil;
end;

function TlvkCustomState.ReadEnumerated(const ID: string;
  const TypeInfo: PTypeInfo; const Default: Integer): Integer;
var
  s : string;
begin
  s := ReadString(ID, '');
  if s = '' then
    Result := 0
  else
    Result := GetEnumValue(TypeInfo, s);
end;

function TlvkCustomState.ReadFloat(const ID: string;
  const Default: Extended): Extended;
begin
  Result := FStorage.ReadFloat(FSectionName, ID, Default);
end;

function TlvkCustomState.ReadInteger(const ID: string;
  const Default: Int64): Int64;
begin
  Result := FStorage.ReadInteger(FSectionName, ID, Default);
end;

function TlvkCustomState.ReadString(const ID, Default: string): string;
begin
  Result := FStorage.ReadString(FSectionName, ID, Default);
end;

procedure TlvkCustomState.SetSectionName(const Value: string);
begin
  if Value <> FSectionName then
  begin
    if Trim(Value) = '' then
      raise ElvkCustomState.Create('SectionName property cannot be a blank string');

    FSectionName := Value;
  end;
end;

procedure TlvkCustomState.WriteEnumerated(const ID: string;
  const TypeInfo: PTypeInfo; const Value: Integer);
begin
  WriteString(ID, GetEnumName(TypeInfo, Value));
end;

procedure TlvkCustomState.WriteFloat(const ID: string;
  const Value: Extended);
begin
  FStorage.WriteFloat(FSectionName, ID, Value);
end;

procedure TlvkCustomState.WriteInteger(const ID: string;
  const Value: Int64);
begin
  FStorage.WriteInteger(FSectionName, ID, Value);
end;

procedure TlvkCustomState.WriteString(const ID, Value: string);
begin
  FStorage.WriteString(FSectionName, ID, Value);
end;

{ TlvkCustomStateStorage }

procedure TlvkCustomStateStorage.BeginJob(const WriteMode: Boolean);
begin
  // Do nothing by default
end;

procedure TlvkCustomStateStorage.EndJob;
begin
  // Do nothing by default
end;

function TlvkCustomStateStorage.ReadFloat(const SectionName, ID: string;
  const Default: Extended): Extended;
var
  s : string;
begin
  s := DoReadString(SectionName, ID, '');
  if s = '' then
    Result := Default
  else
    Result := StrToFloat(s);
end;

function TlvkCustomStateStorage.ReadInteger(const SectionName, ID: string;
  const Default: Int64): Integer;
var
  s : string;
begin
  s := ReadString(SectionName, ID, '');
  if s = '' then
    Result := Default
  else
    Result := StrToInt64(s);
end;

function TlvkCustomStateStorage.ReadString(const SectionName, ID,
  Default: string): string;
begin
  Result := DoReadString(SectionName, ID, Default);
end;

procedure TlvkCustomStateStorage.WriteFloat(const SectionName, ID: string;
  const Value: Extended);
begin
  WriteString(SectionName, ID, FloatToStr(Value));
end;

procedure TlvkCustomStateStorage.WriteInteger(const SectionName,
  ID: string; const Value: Int64);
begin
  DoWriteString(SectionName, ID, IntToStr(Value));
end;

procedure TlvkCustomStateStorage.WriteString(const SectionName, ID,
  Value: string);
begin
  DoWriteString(SectionName, ID, Value);
end;

end.
