{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains (for now) a single function for copying properties from
    one object instance to another, property by property.
  See also:
    MemberwiseCopy
}
unit lvkUtilityFunctions;

// $Author: Lasse V. Karlsen $
// $Revision: 4 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkUtilityFunctions.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

{ Description:
    This function copies the property values from the Source object
    instance to the Dest object instance, property by property.

    Only properties that both objects have will be copied, all other properties
    will be left alone.
  Parameters:
    Source  - The object instance to copy property values from.
    Dest    - The object instance to copy property values to.
  Returns:
    True if any properties were copied,<P>
    False if properties were copied.
}
function MemberwiseCopy(const Source, Dest: TObject): Boolean;

implementation

uses
  TypInfo;

function MemberwiseCopy(const Source, Dest: TObject): Boolean;
var
  PropList    : PPropList;
  Count       : Integer;
  Index       : Integer;

  SourceProp  : PPropInfo;
  DestProp    : PPropInfo;
begin
  Assert(Assigned(Source));
  Assert(Assigned(Dest));

  Result := False;

  New(PropList);
  try
    Count := GetPropList(Source.ClassInfo, tkAny, PropList);
    for Index := 0 to Count-1 do
    begin
      SourceProp := PropList^[Index];
      DestProp := GetPropInfo(Dest, SourceProp^.Name, tkAny);

      if Assigned(DestProp) then
      begin
        if SourceProp^.PropType = DestProp^.PropType then
        begin
          case SourceProp^.PropType^.Kind of
            tkUnknown, tkArray, tkRecord, tkDynArray:
              ;

            tkInteger, tkChar, tkClass, tkEnumeration:
              begin
                SetOrdProp(Dest, DestProp, GetOrdProp(Source, SourceProp));
                Result := True;
              end;

            tkFloat:
              begin
                SetFloatProp(Dest, DestProp, GetFloatProp(Source, SourceProp));
                Result := True;
              end;

            tkString, tkLString:
              begin
                SetStrProp(Dest, DestProp, GetStrProp(Source, SourceProp));
                Result := True;
              end;

            tkSet:
              begin
                SetSetProp(Dest, DestProp, GetSetProp(Source, SourceProp, True));
                Result := True;
              end;

            tkMethod:
              begin
                SetMethodProp(Dest, DestProp, GetMethodProp(Source, SourceProp));
                Result := True;
              end;

            {$IFDEF DELPHI6UP}
            tkWChar, tkWString:
              begin
                SetWideStrProp(Dest, DestProp, GetWideStrProp(Source, SourceProp));
                Result := True;
              end;

            tkInterface:
              begin
                SetInterfaceProp(Dest, DestProp,
                  GetInterfaceProp(Source, SourceProp));
                Result := True;
              end;
            {$ENDIF}

            tkVariant:
              begin
                SetVariantProp(Dest, DestProp, GetVariantProp(Source, SourceProp));
                Result := True;
              end;

            tkInt64:
              begin
                SetInt64Prop(Dest, DestProp, GetInt64Prop(Source, SourceProp));
                Result := True;
              end;

          else
            Assert(False);
          end;
        end;
      end;
    end;
  finally
    Dispose(PropList);
  end;
end;

end.
