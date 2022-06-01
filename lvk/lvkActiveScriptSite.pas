{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{* Portions contributed by Wim van der Vegt                                   *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit holds internal classes for the TlvkActiveScript class.
}
unit lvkActiveScriptSite;

// $Author: Lasse V. Karlsen $
// $Revision: 4 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkActiveScriptSite.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, ActiveX, ComObj, Windows,
  lvkActiveScriptInterfaces;

type
{ Description:
    This record holds error information for the TlvkActiveScriptSite class.
  See also:
    TlvkActiveScriptSite
}
  TlvkActiveScriptErrorRec = record
    // Line number of error
    lineNo        : Cardinal;
    // Character position of error (relative to start of line)
    charPos       : Integer;
    // More information about error
    Info          : TExcepInfo;
  end;

{ Description:
    This interface is used internally to get error data from the
    TlvkActiveScriptSite class.
  See also:
    TlvkActiveScriptSite
}
  IActiveScriptSiteError = interface
    ['{72A9AA15-9D68-4250-B65A-44130A14B9FD}']
    { Description:
        This method returns a copy of the internal record that holds
        information about the last error reported to the site object.
      See also:
        Error, TlvkActiveScriptSite.GetError
    }
    function GetError: TlvkActiveScriptErrorRec;

    { Description:
        This method returns a boolean flag telling if an error has occured or
        not. Specifically, this method will return True if the event handler
        TlvkActiveScriptSite.OnScriptError has been called.
      Return value:
        True if an error has occured, False if not.
      See also:
        ErrorOccured, ClearError
    }
    function GetErrorOccured: Boolean;

    { Description:
        This method clears the ErrorOccured flag.
      See also:
        ErrorOccured
    }
    procedure ClearError;

    { Description:
        This method returns a copy of the internal record that holds
        information about the last error reported to the site object.
      See also:
        GetError, TlvkActiveScriptSite.GetError
    }
    property Error: TlvkActiveScriptErrorRec read GetError;

    { Description:
        This property returns a boolean flag telling if an error has occured or
        not. Specifically, this property will return True if the event handler
        TlvkActiveScriptSite.OnScriptError has been called.
      Return value:
        True if an error has occured, False if not.
      See also:
        GetErrorOccured, ClearError
    }
    property ErrorOccured: Boolean read GetErrorOccured;
  end;

{ Description:
    This class implements the IActiveScriptSite interface and is necessary in
    order to use the Microsoft ActiveScript Engine.<P>

    You can read more about the IActiveScriptSite interface here:
    <EXTLINK http://msdn.microsoft.com/scripting/hosting/iactivescript/IActiveScriptSite.htm#IActiveScriptSite>MSDN Documentation for IActiveScriptSite</EXTLINK>
  See also:
    TlvkActiveScript
}
  TlvkActiveScriptSite = class(TInterfacedObject, IUnknown, IActiveScriptSite, IActiveScriptSiteWindow,
    IActiveScriptSiteError)
  private
    FOwner        : TObject;
    FError        : TlvkActiveScriptErrorRec;
    FHandle       : THandle;
    FErrorOccured : Boolean;

  protected
    // IUnknown interface
    { Description:
        This method will shortcircuit the reference count scheme in order to
        be sure that the object instance is destroyed.
    }
    function _AddRef: Integer; stdcall;
    { Description:
        This method will shortcircuit the reference count scheme in order to
        be sure that the object instance is destroyed.
    }
    function _Release: Integer; stdcall;

    // IActiveScriptSite interface
    { Description:
        This internal method returns the locale id to use when presenting text
        and/or error messages from the script engine.
      Parameters:
        wLCID - Output parameter to store the locale id in.
      Return value:
        S_OK to signal success.
    }
    function GetLCID(var wLCID: TLCID): HResult; stdcall;

    { Description:
        This internal method returns the information about a item that is
        available to the scripting engine.
      Parameters:
        StrName - The name associated with the item, as specified in the
          IActiveScript::AddNamedItem method.
        dwReturnMask - A bit mask specifying what information about the item
          should be returned.
        UnkItem - Output address of a variable that receives a pointer to the
          IUnknown interface associated with the given item.
        outTypeInfo - Address of a variable that receives a pointer to the
          ITypeInfo interface associated with the item.
      Return value:
        S_OK to signal success,<P>
        TYPE_E_ELEMENTNOTFOUND to signal that an element was not found,<P>
        E_INVALIDARG to signal that one or more parameters had invalid values.
    }
    function GetItemInfo(StrName: PWideChar; dwReturnMask: DWord;
      out UnkItem: IUnknown; out outTypeInfo: ITypeInfo): HResult; stdcall;

    { Description:
        This internal method retrieves a host-defined string that uniquely
        identifies the current document version from the host's point of view.
      Parameters:
        VersionString - Output parameter to store the version in.
      Return value:
        E_NOTIMPL to signal that this method is not implemented.
    }
    function GetDocVersionString(var VersionString: TBSTR): HResult; stdcall;

    { Description:
        This internal event handler is called when the script terminates.
      Parameters:
        VarResult - The result of executing the script.
        ExcepInfo - Information about any exceptions raised.
      Return value:
        S_OK to signal success.
    }
    function OnScriptTerminate(var VarResult: OleVariant; var ExcepInfo: TExcepInfo): HResult; stdcall;

    { Description:
        This internal event handler is called when the script engine changes
        state.
      Parameters:
        ScriptState - The new state of the engine.
      Return value:
        E_NOTIMPL to signal that this method is not implemented.
    }
    function OnStateChange(ScriptState: LongInt): HResult; stdcall;

    { Description:
        This internal event handler is called when the script engine raises
        an error exception. This method will copy information about the error
        and store it for future reference.
      Parameters:
        pAse - Reference to a IActiveScriptError interface.
      Return value:
        S_OK to signal success, or other error codes.
    }
    function OnScriptError(pAse: IActiveScriptError): HResult; stdcall;

    { Description:
        This internal event handler is called when the script engine starts
        executing the script.
      Return value:
        S_OK to signal success.
    }
    function OnEnterScript: HResult; stdcall;

    { Description:
        This internal event handler is called when the script engine has
        finished executing code.
      Return value:
        S_OK to signal success.
    }
    function OnLeaveScript: HResult; stdcall;

    // IActiveScriptSiteWindow interface
    { Description:
        This internal method retrieves the window handle that can act as the
        owner of a pop-up window that the scripting engine must display.
      Return value:
        S_OK to signal success,<P>
        E_NOTIMPL to signal that this method is not implemented.
    }
    function GetWindow(var Hwnd: THandle): HResult; stdcall;

    { Description:
        This internal method causes the host to enable or disable its main
        window as well as any modeless dialog boxes.
      Return value:
        E_NOTIMPL to signal that this method is not implemented.
    }
    function EnableModeless(FEnable: WordBool): HResult; stdcall;

    // IActiveScriptSiteError interface
    { Description:
        This internal method returns a copy of the internal record that holds
        information about the last error reported to the site object.
      Return value:
        Copy of record with error information.
      See also:
        IActiveScriptSiteError,
        IActiveScriptSiteError.Error,
        IActiveScriptSiteError.GetError
    }
    function GetError: TlvkActiveScriptErrorRec;

    { Description:
        This method returns a boolean flag telling if an error has occured or
        not. Specifically, this method will return True if the event handler
        OnScriptError has been called.
      Return value:
        True if an error has occured, False if not.
      See also:
        IActiveScriptSiteError.ErrorOccured, ClearError
    }
    function GetErrorOccured: Boolean;

    { Description:
        This method clears the ErrorOccured flag.
      See also:
        IActiveScriptSiteError.ErrorOccured
    }
    procedure ClearError;

  public
    { Description:
        This constructor creates a new instance of the TlvkActiveScriptSite
        class and prepares it for use.
      Parameters:
        AOwner - Reference to the owning TlvkActiveScript object.
        WindowHandle - Handle of window to use when showing message boxes and
          similar. Can be set to 0 to prevent the script code to display such
          messages.
      See also:
        Destroy
    }
    constructor Create(const AOwner: TObject; const WindowHandle: THandle=0);

    { Description:
        Destroys the instance of the TlvkActiveScriptSite object and cleans up
        after it.
      See also:
        Create
    }
    destructor Destroy; override;
  end;

implementation

uses
  lvkActiveScript, SysUtils;

{ TlvkActiveScriptSite }

procedure TlvkActiveScriptSite.ClearError;
begin
  FErrorOccured := False;
  FillChar(FError, SizeOf(FError), 0);
end;

constructor TlvkActiveScriptSite.Create(const AOwner: TObject; const WindowHandle: THandle=0);
begin
  inherited Create;
  FOwner := AOwner;
  FHandle := WindowHandle;
end;

destructor TlvkActiveScriptSite.Destroy;
begin
  FOwner := nil;
  inherited;
end;

function TlvkActiveScriptSite.EnableModeless(FEnable: WordBool): HResult;
begin
  Result := E_NOTIMPL;
end;

function TlvkActiveScriptSite.GetDocVersionString(
  var VersionString: TBSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TlvkActiveScriptSite.GetError: TlvkActiveScriptErrorRec;
begin
  Result := FError;
end;

function TlvkActiveScriptSite.GetErrorOccured: Boolean;
begin
  Result := FErrorOccured;
end;

function TlvkActiveScriptSite.GetItemInfo(StrName: PWideChar;
  dwReturnMask: DWord; out UnkItem: IUnknown;
  out outTypeInfo: ITypeInfo): HResult;
var
  Dispatch  : IDispatch;
  rc        : HRESULT;
begin
  if (dwReturnMask and SCRIPTINFO_ITYPEINFO)<>0 then
  begin
    Result := TYPE_E_ELEMENTNOTFOUND;
  end else if (dwReturnMask and SCRIPTINFO_IUNKNOWN)<>0 then
  begin
    UnkItem := TlvkActiveScript(FOwner).GetObject(StrName);
    if UnkItem<>nil then
      Result := S_OK
    else begin
      rc := TlvkActiveScript(FOwner).ActiveScript.GetScriptDispatch(StrName, Dispatch);
      if (rc = S_OK) and Assigned(Dispatch) then
      begin
        TlvkActiveScript(FOwner).LastModuleName := StrName;
        UnkItem := Dispatch;
        Result := S_OK;
      end else
        Result := TYPE_E_ELEMENTNOTFOUND;
    end;
  end else
    Result := E_INVALIDARG;
end;

function TlvkActiveScriptSite.GetLCID(var wLCID: TLCID): HResult;
begin
  wLCID := GetSystemDefaultLCID;
  Result := S_OK;
end;

function TlvkActiveScriptSite.GetWindow(var Hwnd: THandle): HResult;
begin
  if FHandle=0 then
    Result := E_FAIL
  else begin
    Hwnd := FHandle;
    Result := S_OK;
  end;
end;

function TlvkActiveScriptSite.OnEnterScript: HResult;
begin
  Result := S_OK;
end;

function TlvkActiveScriptSite.OnLeaveScript: HResult;
begin
  Result := S_OK;
end;

function TlvkActiveScriptSite.OnScriptError(
  pAse: IActiveScriptError): HResult;
var
  ContextCookie : Word;
begin
  FErrorOccured := True;
  
  Result := pAse.GetSourcePosition(ContextCookie, FError.lineNo, FError.charPos);
  if Result <> S_OK then
    Exit;

  Result := pAse.GetExceptionInfo(FError.Info);
  Inc(FError.charPos);
  if FError.lineNo = $FFFFFFFF then
    FError.lineNo := 0
  else
    Inc(FError.lineNo);
end;

function TlvkActiveScriptSite.OnScriptTerminate(var VarResult: OleVariant;
  var ExcepInfo: TExcepInfo): HResult;
begin
  Result := S_OK;
end;

function TlvkActiveScriptSite.OnStateChange(ScriptState: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TlvkActiveScriptSite._AddRef: Integer;
begin
  Result := -1;
end;

function TlvkActiveScriptSite._Release: Integer;
begin
  Result := -1;
end;

end.

