{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a storage component that saves the information to an
    ini file.
}
unit lvkINIFileStateStorage;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkINIFileStateStorage.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, IniFiles, lvkState;

type
  { Description:
      This data type is used for the Directory property of the
      TlvkINIFileStateStorage component, specifying where to save the ini file.

      These are the available values:

        * sdUser - In the AppData directory of the currently logged on user.
        * sdWindows - In the windows directory.
        * sdApplication - In the same directory that the application executable
          is located.
  }
  TlvkINIFileStateStorageDirectory = (sdUser, sdWindows, sdApplication);

  { Description:
      This component handles saving state information to an ini file.
  }
  TlvkINIFileStateStorage = class(TlvkCustomStateStorage)
  private
    FDirectory  : TlvkINIFileStateStorageDirectory;
    FFileName   : string;
    FINIFile    : TINIFile;

    function UseFileName: string;

  protected
    function DoReadString(const SectionName, ID: String;
      const Default: String): String; override;
    procedure DoWriteString(const SectionName, ID: String; const Value: String);
      override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // <ALIAS TlvkCustomStateStorage.BeginJob@Boolean>
    procedure BeginJob(const WriteMode: Boolean); override;
    // <ALIAS TlvkCustomStateStorage.DeleteValue@string@string>
    procedure DeleteValue(const SectionName, ID: string); override;
    // <ALIAS TlvkCustomStateStorage.EndJob>
    procedure EndJob; override;

  published
    { Description:
        This property specifies the filename of the ini file to write or read
        from.
    }
    property FileName: string read FFileName write FFileName;

    { Description:
        This property specifies where to locate the ini file. See the
        TlvkINIFileStateStorageDirectory data type for a description of the
        available values.
    }
    property Directory: TlvkINIFileStateStorageDirectory read FDirectory
      write FDirectory default sdApplication;
  end;

implementation

uses
  Windows, Forms, ActiveX, ShlObj;

function GetSystemPath(Folder: Integer): string;
var
  PIDL    : PItemIDList;
  Path    : PChar;
  Malloc  : IMalloc;
begin
  Path := StrAlloc(MAX_PATH);
  try
    SHGetMalloc(Malloc);

    SHGetSpecialFolderLocation(Application.Handle, Folder, PIDL);
    try
      if SHGetPathFromIDList(PIDL, Path) then
        Result := Path;
    finally
      Malloc.Free(PIDL);
    end;
  finally
    StrDispose(Path);
  end;
end;

{ TlvkINIFileStateStorage }

procedure TlvkINIFileStateStorage.BeginJob(const WriteMode: Boolean);
begin
  EndJob;

  FINIFile := TINIFile.Create(UseFileName);
end;

constructor TlvkINIFileStateStorage.Create(AOwner: TComponent);
begin
  inherited;

  FDirectory := sdApplication;
end;

procedure TlvkINIFileStateStorage.DeleteValue(const SectionName,
  ID: string);
begin
  FINIFile.DeleteKey(SectionName, ID);
end;

destructor TlvkINIFileStateStorage.Destroy;
begin
  EndJob;

  inherited;
end;

function TlvkINIFileStateStorage.DoReadString(const SectionName, ID,
  Default: String): String;
begin
  Assert(Assigned(FINIFile));

  Result := SafeDecode(FINIFile.ReadString(SectionName, ID, Default));
end;

procedure TlvkINIFileStateStorage.DoWriteString(const SectionName, ID,
  Value: String);
begin
  Assert(Assigned(FINIFile));

  FINIFile.WriteString(SectionName, ID, SafeEncode(Value));
end;

procedure TlvkINIFileStateStorage.EndJob;
begin
  FreeAndNil(FINIFile);
end;

function TlvkINIFileStateStorage.UseFileName: string;
var
  Buffer    : array[0..MAX_PATH] of Char;
  Path      : string;
  FileName  : string;
begin
  case FDirectory of
    sdUser:
      Path := GetSystemPath(CSIDL_APPDATA);

    sdWindows:
      begin
        if GetWindowsDirectory(Buffer, SizeOf(Buffer)) > 0 then
          Path := Buffer
        else
          raise ElvkINIFileStateStorage.Create('Unable to locate windows directory');
      end;

    sdApplication:
      Path := ExtractFilePath(ParamStr(0));
  else
    raise ElvkINIFileStateStorage.Create('Internal error #1 in TlvkINIFileStateStorage.UseFileName');
  end;

  if FFileName <> '' then
    FileName := ExtractFileName(FFileName)
  else
    FileName := ExtractFileName(ChangeFileExt(ParamStr(0), '.ini'));

  Result := IncludeTrailingBackSlash(Path) + FileName;
end;

end.
