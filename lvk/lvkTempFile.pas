{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code to register and keep a list of temporary files that
    the application creates and uses while it runs. Once the program ends,
    these files should be deleted.
  See also:
    lvkRegisterTempFile, lvkUnRegisterTempFile
}
unit lvkTempFile;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkTempFile.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

{ Description:
    This procedure adds the file with the specified name to the list of
    temporary files.

    All files in this list will be deleted when the program exits.

    Note: Calling this procedure twice with the same filename will only result
      in the filename being added to the list once, subsequent calls with the
      same filename will do nothing.
  Parameters:
    Filename - Path and name of file to add to list.
  See also:
    lvkUnRegisterTempFile
}
procedure lvkRegisterTempFile(const Filename: string);

{ Description:
    This procedure removes the file with the specified name from the list of
    temporary files.

    Note: If the filename is not on the list, nothing will happen.
  Parameters:
    Filename - Path and name of file to remove from list.
  See also:
    lvkRegisterTempFile
}
procedure lvkUnRegisterTempFile(const Filename: string);

implementation

uses
  SysUtils, Classes, SyncObjs;

var
  TempLock  : TCriticalSection;
  TempFiles : TStringList;

procedure lvkRegisterTempFile(const Filename: string);
begin
  if not Assigned(TempLock) then
    TempLock := TCriticalSection.Create;

  TempLock.Acquire;
  try
    if not Assigned(TempFiles) then
    begin
      TempFiles := TStringList.Create;
      TempFiles.Sorted := True;
    end;

    if TempFiles.IndexOf(Filename) < 0 then
      TempFiles.Add(Filename);
  finally
    TempLock.Release;
  end;
end;

procedure lvkUnRegisterTempFile(const Filename: string);
var
  Index : Integer;
begin
  TempLock.Acquire;
  try
    if Assigned(TempFiles) then
    begin
      Index := TempFiles.IndexOf(Filename);
      if Index >= 0 then
        TempFiles.Delete(Index);

      if TempFiles.Count = 0 then
        FreeAndNil(TempLock);
    end;
  finally
    TempLock.Release;
  end;
end;

procedure RemoveTempFiles;
var
  Index : Integer;
begin
  for Index := 0 to TempFiles.Count-1 do
    DeleteFile(TempFiles[Index]);
  FreeAndNil(TempFiles);
end;

initialization
finalization
  if Assigned(TempFiles) then
    RemoveTempFiles;
  FreeAndNil(TempLock);
end.
