{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}
unit lvkUtils;

// $Author: $
// $Date: $
// $Revision: $
// $Archive: $

{ TODO 2 -oLVK -cDocumentation : Document lvkUtils.pas }
interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

function IsInteger(const s: string): Boolean;
function IsFloatingPoint(const s: string): Boolean;
function IsBoolean(const s: string): Boolean;

implementation

function IsInteger(const s: string): Boolean;
var
  Index : Boolean;
begin
  Result := False;

  if s = '' then
    Exit;

  if not (s[1] in ['-', '0'..'9']) then
    Exit;
  for Index := 2 to Length(s) do
    if not (s[Index] in ['0'..'9']) then
      Exit;

  Result := True;
end;

function IsFloatingPoint(const s: string): Boolean;
begin
  { TODO 2 -oLVK -cSource : Implement IsFloatingPoint }
end;

function IsBoolean(const s: string): Boolean;
begin
  { TODO 2 -oLVK -cSource : Implement IsBoolean }
end;

end.
 