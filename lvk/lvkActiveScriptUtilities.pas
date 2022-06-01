{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains utility objects that the programmer can add to the
    scripting engines to add more functionality.
}
unit lvkActiveScriptUtilities;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkActiveScriptUtilities.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFDEF DELPHI6UP}
  Variants,
  {$ENDIF}
  lvkRTTIDispatch;

type
  { Description:
      This object can be added to a script engine to add the procedures
      MessageBox and DebugPrint to the script system. Check out the help page
      for each on how to use them.
    See also:
      TlvkActiveScriptDebugObject.DebugPrint@array of Variant@Variant,
      TlvkActiveScriptDebugObject.MessageBox@array of Variant@Variant
  }
  TlvkActiveScriptDebugObject = class(TlvkRTTIBaseDispatch)
  public
    constructor Create;

  published
    { Description:
        DebugPrint will output messages to the debug console. In Delphi you
        can view these messages by opening View->Debug Windows->Event Log.

        If you're running the program outside Delphi then you need a tool
        to show these messages. Check out www.sysinternals.com for
        DebugView, a tool that can do this.

        Note: The parameter list shown here is the parameter list for the
          script equivalent of the function, not the Delphi declaration.
      Parameters:
        Message - The message to print to the debug console.
    }
    function DebugPrint(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT;

    { Description:
        MessageBox will show a messagebox to the user. It works like
        TApplication.MessageBox in Delphi. Parameter nr. 2 and 3 are
        optional and can be omitted. The first parameter to
        <EXTLINK borland://TApplication.MessageBox>TApplication.MessageBox</EXTLINK>
        has been removed so skip this parameter.

        See
        <EXTLINK borland://TApplication.MessageBox>TApplication.MessageBox</EXTLINK>
        for more information about the parameters.

        All the constants for the third parameter of MessageBox have been
        made available to the script code as well.

        There is an example that shows the use of MessageBox, take a look
        if you feel unsure how this method works.
    }
    function MessageBox(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT;

    { Description:
        This method takes a single parameter and performs a suspended delay
        for the given amount of milliseconds. After the delay has passed,
        script execution resumes.

        Notice that the whole thread that executes the script will sleep, so
        any sleep done in the main thread of an application will make the
        user interface unresponsive.
      Parameters:
        Delay - Number of milliseconds to suspend the thread.
    }
    function Sleep(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT;
  end;

implementation

uses
  Windows, ComObj;

{ TlvkActiveScriptDebugObject }

constructor TlvkActiveScriptDebugObject.Create;
var
  Index : Integer;
type
  TConstant = record
    Name  : string;
    Value : Integer;
  end;
const
  Constants : array[1..34] of TConstant = (
    (Name:  'MB_ABORTRETRYIGNORE';      Value: MB_ABORTRETRYIGNORE),
    (Name:  'MB_OK';                    Value: MB_OK),
    (Name:  'MB_OKCANCEL';              Value: MB_OKCANCEL),
    (Name:  'MB_RETRYCANCEL';           Value: MB_RETRYCANCEL),
    (Name:  'MB_YESNO';                 Value: MB_YESNO),
    (Name:  'MB_YESNOCANCEL';           Value: MB_YESNOCANCEL),
    (Name:  'MB_ICONEXCLAMATION';       Value: MB_ICONEXCLAMATION),
    (Name:  'MB_ICONWARNING';           Value: MB_ICONWARNING),
    (Name:  'MB_ICONINFORMATION';       Value: MB_ICONINFORMATION),
    (Name:  'MB_ICONASTERISK';          Value: MB_ICONASTERISK),
    (Name:  'MB_ICONQUESTION';          Value: MB_ICONQUESTION),
    (Name:  'MB_ICONSTOP';              Value: MB_ICONSTOP),
    (Name:  'MB_ICONERROR';             Value: MB_ICONERROR),
    (Name:  'MB_ICONHAND';              Value: MB_ICONHAND),
    (Name:  'MB_DEFBUTTON1';            Value: MB_DEFBUTTON1),
    (Name:  'MB_DEFBUTTON2';            Value: MB_DEFBUTTON2),
    (Name:  'MB_DEFBUTTON3';            Value: MB_DEFBUTTON3),
    (Name:  'MB_DEFBUTTON4';            Value: MB_DEFBUTTON4),
    (Name:  'MB_SYSTEMMODAL';           Value: MB_SYSTEMMODAL),
    (Name:  'MB_TASKMODAL';             Value: MB_TASKMODAL),
    (Name:  'MB_DEFAULT_DESKTOP_ONLY';  Value: MB_DEFAULT_DESKTOP_ONLY),
    (Name:  'MB_HELP';                  Value: MB_HELP),
    (Name:  'MB_RIGHT';                 Value: MB_RIGHT),
    (Name:  'MB_RTLREADING';            Value: MB_RTLREADING),
    (Name:  'MB_SETFOREGROUND';         Value: MB_SETFOREGROUND),
    (Name:  'MB_TOPMOST';               Value: MB_TOPMOST),
    (Name:  'MB_SERVICE_NOTIFICATION';  Value: MB_SERVICE_NOTIFICATION),
    (Name:  'IDABORT';                  Value: IDABORT),
    (Name:  'IDCANCEL';                 Value: IDCANCEL),
    (Name:  'IDIGNORE';                 Value: IDIGNORE),
    (Name:  'IDNO';                     Value: IDNO),
    (Name:  'IDOK';                     Value: IDOK),
    (Name:  'IDRETRY';                  Value: IDRETRY),
    (Name:  'IDYES';                    Value: IDYES)
  );
begin
  inherited Create;

  for Index := Low(Constants) to High(Constants) do
    DefineConstant(Constants[Index].Name, Constants[Index].Value);
end;

function TlvkActiveScriptDebugObject.DebugPrint(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
var
  Message : string;
begin
  if Length(Parameters) = 1 then
  begin
    Message := Parameters[0];
    OutputDebugString(PChar(Message + #13#10));

    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

function TlvkActiveScriptDebugObject.MessageBox(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
var
  Message     : string;
  Caption     : string;
  MessageType : Integer;
begin
  Caption := 'Message';
  MessageType := MB_OK;

  if Length(Parameters) = 1 then
  begin
    Message := Parameters[0];

    Result := S_OK;
  end else if Length(Parameters) = 2 then
  begin
    Message := Parameters[0];
    Caption := Parameters[1];

    Result := S_OK;
  end else if Length(Parameters) = 3 then
  begin
    Message := Parameters[0];
    Caption := Parameters[1];
    MessageType := Parameters[2];

    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;

  if Result = S_OK then
    FunctionResult := Windows.MessageBox(0, PChar(Message), PChar(Caption),
      MessageType);
end;

function TlvkActiveScriptDebugObject.Sleep(
  var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
begin
  if Length(Parameters) = 1 then
  begin
    Windows.Sleep(Parameters[0]);
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

end.
