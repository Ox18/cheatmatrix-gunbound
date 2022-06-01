{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit defines the scripts comprising the Microsoft ActiveScript Engine.
}
unit lvkActiveScriptInterfaces;

// $Author: Lasse V. Karlsen $
// $Revision: 4 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkActiveScriptInterfaces.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  ComObj, ActiveX, Windows;

const
  // tagSCRIPTSTATE enum
  SCRIPTSTATE_UNINITIALIZED     = 0;
  SCRIPTSTATE_INITIALIZED       = 5;
  SCRIPTSTATE_STARTED           = 1;
  SCRIPTSTATE_CONNECTED         = 2;
  SCRIPTSTATE_DISCONNECTED      = 3;
  SCRIPTSTATE_CLOSED            = 4;

  // tagSCRIPTTHREADSTATE enum
  SCRIPTTHREADSTATE_NOTINSCRIPT = 0;
  SCRIPTTHREADSTATE_RUNNING     = 1;

  // Component Categories
  CATID_ActiveScript          : TGUID = '{F0B7A1A1-9847-11CF-8F20-00805F2CD064}';
  CATID_ActiveScriptParse     : TGUID = '{F0B7A1A2-9847-11CF-8F20-00805F2CD064}';
  CATID_ActiveScriptEncoding  : TGUID = '{F0B7A1A3-9847-11CF-8F20-00805F2CD064}';

  // Language Classes
  CLSID_XML                   : TGUID = '{989D1DC0-B162-11D1-B6EC-D27DDCF9A923}';
  CLSID_VBScript              : TGUID = '{B54F3741-5B07-11CF-A4B0-00AA004A55E8}';
  CLSID_VBScriptEncode        : TGUID = '{B54F3743-5B07-11CF-A4B0-00AA004A55E8}';
  CLSID_JScript               : TGUID = '{F414C260-6AC0-11CF-B6D1-00AA00BBBB58}';
  CLSID_JScriptEncode         : TGUID = '{F414C262-6AC0-11CF-B6D1-00AA00BBBB58}';
  CLSID_Python                : TGUID = '{DF630910-1C1D-11d0-AE36-8C0F5E000000}';

  // ParseScriptText Flags
  SCRIPTTEXT_ISVISIBLE        = $00000002;
  SCRIPTTEXT_ISEXPRESSION     = $00000020;
  SCRIPTTEXT_ISPERSISTENT     = $00000040;
  SCRIPTTEXT_ALL_FLAGS        = SCRIPTTEXT_ISVISIBLE or SCRIPTTEXT_ISEXPRESSION or SCRIPTTEXT_ISPERSISTENT;

  // AddNamedItem Flags
  SCRIPTITEM_ISVISIBLE        = $00000002;
  SCRIPTITEM_ISSOURCE         = $00000004;
  SCRIPTITEM_GLOBALMEMBERS    = $00000008;
  SCRIPTITEM_ISPERSISTENT     = $00000040;
  SCRIPTITEM_CODEONLY         = $00000200;
  SCRIPTITEM_NOCODE           = $00000400;

  // GetNamedItem ReturnMask Flags
  SCRIPTINFO_IUNKNOWN         = $00000001;
  SCRIPTINFO_ITYPEINFO        = $00000002;

// *** IDispatchEx constants ***
  // GetDispID flags
  fdexNameCaseSensitive       = $0001;
  fdexNameEnsure              = $0002;
  fdexNameImplicit            = $0004;
  fdexNameCaseInsensitive     = $0008;
  fdexNameCaseInternal        = $0010;
  fdexNameNoDynamicProperties = $0020;

  // GetMemberProperties flags
  fdexPropCanGet              = $0001;
  fdexPropCannotGet           = $0002;
  fdexPropCanPut              = $0004;
  fdexPropCannotPut           = $0008;
  fdexPropCanPutRef           = $0010;
  fdexPropCannotPutRef        = $0020;
  fdexPropNoSideEffects       = $0040;
  fdexPropDynamicType         = $0080;
  fdexPropCanCall             = $0100;
  fdexPropCannotCall          = $0200;
  fdexPropCanConstruct        = $0400;
  fdexPropCannotConstruct     = $0800;
  fdexPropCanSourceEvents     = $1000;
  fdexPropCannotSourceEvents  = $2000;

  // Flag-sets
  fdexPropCanAll              = fdexPropCanGet or
                                fdexPropCanPut or
                                fdexPropCanPutRef or
                                fdexPropCanCall or
                                fdexPropCanConstruct or
                                fdexPropCanSourceEvents;
  fdexPropCannotAll           = fdexPropCannotGet or
                                fdexPropCannotPut or
                                fdexPropCannotPutRef or
                                fdexPropCannotCall or
                                fdexPropCannotConstruct or
                                fdexPropCannotSourceEvents;
  fdexPropExtraAll            = fdexPropNoSideEffects or
                                fdexPropDynamicType;
  fdexPropAll                 = fdexPropCanAll or
                                fdexPropCannotAll or
                                fdexPropExtraAll;

  // GetNextDispID flags
  fdexEnumDefault             = $0001;
  fdexEnumAll                 = $0002;

  // Additional for Invoke
  DISPATCH_CONSTRUCT          = $4000;

  // Standard DispIDs
  DISPID_THIS                 = -613;
  DISPID_STARTENUM            = DISPID_UNKNOWN;

type
{ Description:
    This interface is used in the IActiveScriptSite.OnScriptError event handler
    to feed error information back to the class implementing the
    IActiveScriptSite interface.<P>

    You can read more about this interface at
    <EXTLINK http://msdn.microsoft.com/scripting/hosting/iactivescript/IActiveScriptError.htm>MSDN Documentation for IActiveScriptError</EXTLINK>
}
  IActiveScriptError = interface
    function GetExceptionInfo(var excepInfo: TExcepInfo):HResult; stdcall;
    function GetSourcePosition(out wContextCookie: Word; out lineNo: UINT; out charPos: Integer) : HResult; stdcall;
    function GetSourceLineText(wsSourceLine: WideString) : HResult; stdcall;
  end;

{ Description:
    This interface is when implementing an ActiveScript Site object.<P>

    You can read more about this interface at
    <EXTLINK http://msdn.microsoft.com/scripting/hosting/iactivescript/IActiveScriptSite.htm#IActiveScriptSite>MSDN Documentation for IActiveScriptSite</EXTLINK>
}
  IActiveScriptSite = interface
    ['{DB01A1E3-A42B-11cf-8F20-00805F2CD064}']
    function GetLCID(var wLCID: TLCID): HResult; stdcall;
    function GetItemInfo(StrName: PWideChar; dwReturnMask: DWord;
      out UnkItem: IUnknown; out outTypeInfo: ITypeInfo): HResult; stdcall;
    function GetDocVersionString(var VersionString: TBSTR): HResult; stdcall;
    function OnScriptTerminate(var VarResult: OleVariant; var ExcepInfo: TExcepInfo): HResult; stdcall;
    function OnStateChange(ScriptState: LongInt): HResult; stdcall;
    function OnScriptError(pAse: IActiveScriptError): HResult; stdcall;
    function OnEnterScript: HResult; stdcall;
    function OnLeaveScript: HResult; stdcall;
  end;

{ Description:
    This interface is when implementing an ActiveScript Parser object.<P>

    You can read more about this interface at
    <EXTLINK http://msdn.microsoft.com/scripting/hosting/iactivescript/IActiveScriptParse.htm#IActiveScriptParse>MSDN Documentation for IActiveScriptParse</EXTLINK>
}
  IActiveScriptParse  = interface
    ['{BB1A2AE2-A4F9-11CF-8F20-00805F2CD064}']
    function InitNew: HResult; stdcall;
    function AddScriptlet(DefaultName, ScriptCode, ItemName, SubItemName,
      EventName, Delimiter: PWideChar; wSrcContextCookie: Word;
      StartLine: Integer; wFlags: Word; StrName: PWideChar;
      var ExcepInfo: TExcepInfo): Integer; stdcall;
    function ParseScriptText(MainScript: PWideChar; ItemName: PWideChar; UnkContext: IUnknown;
      EndDelimiter: Pointer; dwSourceCookie: DWORD; StartLineNo: Integer;
      dwFlags: DWord; var VarOut: OleVariant; ExcepInfo: PExcepInfo): HResult; stdcall;
   end;

{ Description:
    This interface is when implementing an ActiveScript object.<P>

    You can read more about this interface at
    <EXTLINK http://msdn.microsoft.com/scripting/hosting/iactivescript/IActiveScript.htm#IActiveScript>MSDN Documentation for IActiveScript</EXTLINK>
}
  IActiveScript  = interface
    ['{BB1A2AE1-A4F9-11CF-8F20-00805F2CD064}']
    function SetScriptSite(ScriptSite: IActiveScriptSite):HResult; stdcall;
    function GetScriptSite(const iid: TIID; out vObj):HResult; stdcall;
    function SetScriptState(ScriptState: LongInt):HResult; stdcall;
    function GetScriptState(out ScriptState: LongInt): HResult; stdcall;
    function Close:HResult; stdcall;
    function AddNamedItem(ItemName: PWideChar; dwFlags: DWord): HResult; stdcall;
    function AddTypeLib(const GuidTypeLib: TGUID; wVerMajor, wVerMinor, wFlags: Word):HResult; stdcall;
    function GetScriptDispatch(StrItemName: Pointer; out ScriptDispatch: IDispatch):HResult; stdcall;
    function GetCurrentScriptThreadID(wScriptThreadID: Word):HResult; stdcall;
    function GetScriptThreadID(wWin32Thread, wScriptThreadID: Word):HResult; stdcall;
    function GetScriptThreadState(wScriptThreadID, wScriptState: Word):HResult; stdcall;
    function InterruptScriptThread(wScriptThreadID: DWORD; ExcepInfo: PExcepInfo; wFlags: DWord ) : HResult; stdcall;
    function Clone(Script: iActiveScript):HResult; stdcall;
  end;

{ Description:
    This interface is when implementing an ActiveScriptWindow object.<P>

    You can read more about this interface at
    <EXTLINK http://msdn.microsoft.com/scripting/hosting/iactivescript/IActiveScriptSiteWindow.htm#IActiveScriptSiteWindow>MSDN Documentation for IActiveScriptSiteWindow</EXTLINK>
}
  IActiveScriptSiteWindow = interface
    ['{D10F6761-83E9-11cF-8F20-00805F2CD064}']
    function GetWindow(var Hwnd: THandle): HResult; stdcall;
    function EnableModeless(FEnable: WordBool): HResult; stdcall;
  end;

  PServiceProvider = ^IServiceProvider;
  IServiceProvider = interface(IUnknown)
    ['{6d5140c1-7436-11ce-8034-00aa006009fa}']
    function QueryService(const rsid, iid: TGuid; out Obj): HResult; stdcall;
  end;

{ Description:
    This interface is missing in ActiveX.pas so I've defined it here.

    Note: If you look up this interface in the Delphi 6 version of ActiveX.pas
      you'll notice that the GUID of the D6 version is actually slightly
      wrong. In any case, this interface is missing in Delphi 5 so it's
      provided here, along with all the constants necessary for using it.
}
  IDispatchEx = interface(IDispatch)
    ['{A6EF9860-C720-11D0-9337-00A0C90DCAA9}']
    function GetDispID(const bstrName: TBSTR; const grfdex: DWORD;
      out id: TDispID): HResult; stdcall;
    function InvokeEx(const id: TDispID; const lcid: LCID; const wflags: WORD;
      const pdp: PDispParams; out varRes: OleVariant;
      out pei: TExcepInfo;
      const pspCaller: PServiceProvider): HResult; stdcall;
    function DeleteMemberByName(const bstr: TBSTR;
      const grfdex: DWORD): HResult; stdcall;
    function DeleteMemberByDispID(const id: TDispID): HResult; stdcall;
    function GetMemberProperties(const id: TDispID; const grfdexFetch:
       DWORD; out grfdex: DWORD): HResult; stdcall;
    function GetMemberName(const id: TDispID;
      out bstrName: TBSTR): HResult; stdcall;
    function GetNextDispID(const grfdex: DWORD; const id: TDispID;
       out nid: TDispID): HResult; stdcall;
    function GetNameSpaceParent(out unk: IUnknown): HResult; stdcall;
  end;

  IDispError = interface
    ['{A6EF9861-C720-11d0-9337-00A0C90DCAA9}']

    function QueryErrorInfo(guidErrorType: TGUID; var ppde: IDispError): HRESULT; stdcall;
    function GetNext(var ppde: IDispError): HRESULT; stdcall;
    function GetHresult(var phr: HRESULT): HRESULT; stdcall;
    function GetSource(out pbstrSource: PWideChar): HRESULT; stdcall;
    function GetHelpInfo(out pbstrFilename: PWideChar; out pdwContext: DWORD): HRESULT; stdcall;
    function GetDescription(out pbstrDescription: PWideChar): HRESULT; stdcall;
  end;

implementation

end.

