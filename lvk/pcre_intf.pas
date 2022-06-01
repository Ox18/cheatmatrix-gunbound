{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    For documentation for the PCRE regular expression library,
    please visit the official pcre webpage at:

    <EXTLINK http://www.pcre.org/>http://www.pcre.org/</EXTLINK>
}
unit pcre_intf;

// $Author: Lasse V. Karlsen $
// $Revision: 2 $
// $Date: 6.04.03 17:34 $
// $Archive: /Components/LVK/Source/pcre_intf.pas $

interface

{$I VERSIONS.INC}
{$IFDEF DELPHI6UP}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{ Options }

const
  PCRE_CASELESS                       = $0001;
  PCRE_MULTILINE                      = $0002;
  PCRE_DOTALL                         = $0004;
  PCRE_EXTENDED                       = $0008;
  PCRE_ANCHORED                       = $0010;
  PCRE_DOLLAR_ENDONLY                 = $0020;
  PCRE_EXTRA                          = $0040;
  PCRE_NOTBOL                         = $0080;
  PCRE_NOTEOL                         = $0100;
  PCRE_UNGREEDY                       = $0200;
  PCRE_NOTEMPTY                       = $0400;
  PCRE_UTF8                           = $0800;
  PCRE_NO_AUTO_CAPTURE                = $1000;

{ Exec-time and get/set-time error codes }

  PCRE_ERROR_NOMATCH                  = -1;
  PCRE_ERROR_NULL                     = -2;
  PCRE_ERROR_BADOPTION                = -3;
  PCRE_ERROR_BADMAGIC                 = -4;
  PCRE_ERROR_UNKNOWN_NODE             = -5;
  PCRE_ERROR_NOMEMORY                 = -6;
  PCRE_ERROR_NOSUBSTRING              = -7;
  PCRE_ERROR_MATCHLIMIT               = -8;
  PCRE_ERROR_CALLOUT                  = -9; // Never used by PCRE itself

  PCRE_ERROR_COMPILE                  = -10; // Unable to compile pattern
  PCRE_ERROR_STUDY                    = -11; // error when studying pattern
  PCRE_ERROR_BOUNDS                   = -12; // Parameter value out of bounds
  PCRE_ERROR_GENERIC                  = -13; // Generic error message

{ Request types for pcre_fullinfo() }

  PCRE_INFO_OPTIONS                   = 0;
  PCRE_INFO_SIZE                      = 1;
  PCRE_INFO_CAPTURECOUNT              = 2;
  PCRE_INFO_BACKREFMAX                = 3;
  PCRE_INFO_FIRSTBYTE                 = 4;
  PCRE_INFO_FIRSTCHAR                 = 4; // For backwards compatibility }
  PCRE_INFO_FIRSTTABLE                = 5;
  PCRE_INFO_LASTLITERAL               = 6;
  PCRE_INFO_NAMEENTRYSIZE             = 7;
  PCRE_INFO_NAMECOUNT                 = 8;
  PCRE_INFO_NAMETABLE                 = 9;
  PCRE_INFO_STUDYSIZE                 = 10;

{ Request types for pcre_config() }

  PCRE_CONFIG_UTF8                    = 0;
  PCRE_CONFIG_NEWLINE                 = 1;
  PCRE_CONFIG_LINK_SIZE               = 2;
  PCRE_CONFIG_POSIX_MALLOC_THRESHOLD  = 3;
  PCRE_CONFIG_MATCH_LIMIT             = 4;

{ Bit flags for the pcre_extra structure }

  PCRE_EXTRA_STUDY_DATA               = $0001;
  PCRE_EXTRA_MATCH_LIMIT              = $0002;
  PCRE_EXTRA_CALLOUT_DATA             = $0004;

type
  PPChar    = ^PChar;
  PPPChar   = ^PPChar;
  PInt      = ^Integer;
  {$IFNDEF DELPHI6UP}
  PByte     = ^Byte;
  {$ENDIF}

procedure pcre_dispose(re, rex: Pointer);

function ErrorText(const ErrorCode: Integer): string;

function pcre_compile(const pattern: PChar; options: integer; errorptr: PPChar;
  erroroffset: PInt; const tables: PChar): Pointer; pascal; 

function pcre_config(what: Integer; where: Pointer): Integer; pascal; 

function pcre_copy_named_substring(code: Pointer; subject: PChar;
  ovector: PInt; stringcount: Integer; stringname: PChar;
  buffer: POinter; size: Integer): Integer; pascal; 

function pcre_copy_substring(subject: PChar; ovector: Integer;
  stringcount, stringnumber: Integer;
  buffer: Pointer; size: Integer): Integer; pascal; 

function pcre_exec(const external_re: Pointer; const extra_data: Pointer;
  const subject: PChar; length, start_offset, options: integer; offsets: PInt;
  offsetcount: integer): integer; pascal; 

procedure pcre_free_substring(p: PChar); pascal; 
procedure pcre_free_substring_list(p: PPChar); pascal; 

function pcre_fullinfo(external_re, extra_data: Pointer; what: Integer;
  where: Pointer): Integer; pascal; 

function pcre_get_named_substring(code: Pointer; subject: PChar;
  ovector: PInt; stringcount: Integer; stringname: PChar;
  stringptr: PPChar): Integer; pascal; 

function pcre_get_stringnumber(code: Pointer;
  stringname: PChar): Integer; pascal; 

function pcre_get_substring(subject: PChar; ovector: PInt;
  stringcount, stringnumber: Integer;
  stringptr: PPChar): Integer; pascal; 
function pcre_get_substring_list(subject: PChar; ovector: PInt;
  stringcount: Integer;
  listptr: PPPChar): Integer; pascal; 

function pcre_info(external_re: Pointer; optptr: PInt;
  first_byte: PByte): Integer; pascal;

function pcre_study(external_re: Pointer; options: Integer;
  errorptr: PPChar): Pointer; pascal;

function pcre_version: PChar; pascal;

implementation

uses
  c_rtl;

{$L pcre-4.1\pcre.obj}
{$L pcre-4.1\study.obj}
{$L pcre-4.1\get.obj}

function pcre_compile(const pattern: PChar; options: integer; errorptr: PPChar;
  erroroffset: PInt; const tables: PChar): Pointer; pascal; external;

function pcre_config(what: Integer; where: Pointer): Integer; pascal; external;

function pcre_copy_named_substring(code: Pointer; subject: PChar;
  ovector: PInt; stringcount: Integer; stringname: PChar;
  buffer: POinter; size: Integer): Integer; pascal; external;

function pcre_copy_substring(subject: PChar; ovector: Integer;
  stringcount, stringnumber: Integer;
  buffer: Pointer; size: Integer): Integer; pascal; external;

function pcre_exec(const external_re: Pointer; const extra_data: Pointer;
  const subject: PChar; length, start_offset, options: integer; offsets: PInt;
  offsetcount: integer): integer; pascal; external;

procedure pcre_free_substring(p: PChar); pascal; external;
procedure pcre_free_substring_list(p: PPChar); pascal; external;

function pcre_fullinfo(external_re, extra_data: Pointer; what: Integer;
  where: Pointer): Integer; pascal; external;

function pcre_get_named_substring(code: Pointer; subject: PChar;
  ovector: PInt; stringcount: Integer; stringname: PChar;
  stringptr: PPChar): Integer; pascal; external;

function pcre_get_stringnumber(code: Pointer;
  stringname: PChar): Integer; pascal; external;

function pcre_get_substring(subject: PChar; ovector: PInt;
  stringcount, stringnumber: Integer;
  stringptr: PPChar): Integer; pascal; external;
function pcre_get_substring_list(subject: PChar; ovector: PInt;
  stringcount: Integer;
  listptr: PPPChar): Integer; pascal; external;

function pcre_info(external_re: Pointer; optptr: PInt;
  first_byte: PByte): Integer; pascal; external;

function pcre_study(external_re: Pointer; options: Integer;
  errorptr: PPChar): Pointer; pascal; external;

function pcre_version: PChar; pascal; external;

procedure pcre_dispose(re, rex: Pointer);
begin
  if Assigned(re) then
    free(re);
  if Assigned(rex) then
    free(rex);
end;

function ErrorText(const ErrorCode: Integer): string;
const
  ErrorTexts  : array[PCRE_ERROR_GENERIC..PCRE_ERROR_NOMATCH] of string = (
    'Generic error',
    'Parameter value out of bounds',
    'Unable to study pattern',
    'Unable to compile pattern',
    'No match found',
    'Null-pointer',
    'Bad option',
    'Bad magic value',
    'Unknown node found',
    'Out of memory',
    'No such named group',
    'Match reached limit',
    'Callout error'
  );
begin
  if ErrorCode < 0 then
  begin
    Result := ErrorTexts[ErrorCode];
  end else
    Result := '';
end;

end.

