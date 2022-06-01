{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ TODO 2 -oLVK -cSource : Reimplement this lost unit }
unit lvkTextToADORecordset;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:51 $
// $Revision: 2 $
// $Archive: /Components/LVK/source/lvkTextToADORecordset.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, lvkADO;

procedure LoadTextToRecordset(const Stream: TStream;
  out Recordset: IlvkADORecordset; const Fields: array of string); overload;
procedure LoadTextToRecordset(const Stream: TStream;
  out Recordset: IlvkADORecordset; const Fields: string); overload;
procedure LoadTextToRecordset(const Filename: string;
  out Recordset: IlvkADORecordset; const Fields: array of string); overload;
procedure LoadTextToRecordset(const Filename: string;
  out Recordset: IlvkADORecordset; const Fields: string); overload;

implementation

procedure LoadTextToRecordset(const Stream: TStream;
  out Recordset: IlvkADORecordset; const Fields: array of string);
begin
end;

procedure LoadTextToRecordset(const Stream: TStream;
  out Recordset: IlvkADORecordset; const Fields: string);
begin
end;

procedure LoadTextToRecordset(const Filename: string;
  out Recordset: IlvkADORecordset; const Fields: array of string);
begin
end;

procedure LoadTextToRecordset(const Filename: string;
  out Recordset: IlvkADORecordset; const Fields: string);
begin
end;

end.
