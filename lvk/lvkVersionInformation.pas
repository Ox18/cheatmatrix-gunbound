{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

unit lvkVersionInformation;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkVersionInformation.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Classes, SysUtils, lvkVersion;

type
  { Description:
      This component can be used to retrieve version information from any
      file that can contain it, normally .EXE, .DLL, .OCX and similar
      files.
  }
  TlvkVersionInformation = class(TComponent)
  private
    FFilename               : string;
    FDataRead               : Boolean;

    FComments               : string;
    FFileDescription        : string;
    FFileDate               : TDateTime;
    FFileFlags              : LongInt;
    FFileFlagsMask          : LongInt;
    FFileMajorVersion       : Word;
    FFileMinorVersion       : Word;
    FFileRelease            : Word;
    FFileBuild              : Word;
    FFileOS                 : LongInt;
    FFileType               : LongInt;
    FFileSubtype            : LongInt;
    FFileVersion            : string;
    FCompanyName            : string;
    FInternalName           : string;
    FLanguageName           : string;
    FLegalCopyright         : string;
    FLegalTrademarks        : string;
    FOriginalFilename       : string;
    FProductMajorVersion    : Word;
    FProductMinorVersion    : Word;
    FProductRelease         : Word;
    FProductBuild           : Word;
    FProductName            : string;
    FProductVersion         : string;
    FPrivateBuild           : string;
    FSpecialBuild           : string;

    procedure SetFilename(const NewFilename: string);
    procedure ReadData;
    function GetFileVersion: string;
    function GetLanguageName: string;
    function GetComments: string;
    function GetCompanyName: string;
    function GetFileDate: TDateTime;
    function GetFileDescription: string;
    function GetFileFlags: LongInt;
    function GetFileFlagsMask: LongInt;
    function GetFileMajorVersion: Word;
    function GetFileMinorVersion: Word;
    function GetFileBuild: Word;
    function GetFileRelease: Word;
    function GetFileOS: LongInt;
    function GetFileSubtype: LongInt;
    function GetFileType: LongInt;
    function GetInternalName: string;
    function GetLegalCopyright: string;
    function GetLegalTrademarks: string;
    function GetOriginalFilename: string;
    function GetProductMajorVersion: Word;
    function GetProductMinorVersion: Word;
    function GetProductBuild: Word;
    function GetProductRelease: Word;
    function GetProductName: string;
    function GetProductVersion: string;
    function GetIsDebugBuild: Boolean;
    function GetIsPreRelease: Boolean;
    function GetIsSpecialBuild: Boolean;
    function GetIsPrivateBuild: Boolean;
    function GetSpecialBuild: string;
    function GetPrivateBuild: string;
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    procedure SetPackageVersion(const Value: TPackageVersion);

  public
    constructor Create(AOwner: TComponent); override;

    { Description:
        This property retrieves the comments section from the version
        resource.
    }
    property Comments: string read GetComments;

    { Description:
        This property retrieves the file version of the file.
    }
    property FileVersion: string read GetFileVersion;

    { Description:
        This property retrieves the name of the language that the file
        was compiled for.
    }
    property LanguageName: string read GetLanguageName;

    { Description:
        This property retrieves the description added to the file at
        compile-time.
    }
    property FileDescription: string read GetFileDescription;

    { Description:
        This property retrieves the date-stamp added to the file at
        compile-time.
    }
    property FileDate: TDateTime read GetFileDate;

    { Description:
        This property retrieves the file flags of the file.
      See also:
        FileFlagsMask
    }
    property FileFlags: LongInt read GetFileFlags;

    { Description:
        This property retrieves the file flags mask for the file.
      See also:
        FileFlags
    }
    property FileFlagsMask: LongInt read GetFileFlagsMask;

    { Description:
        This property retrieves the major version number of the file. In a
        normal version number, like 1.2.3.4, this property will
        retrieve the 1.
      See also:
        FileMinorVersion, FileRelease, FileBuild
    }
    property FileMajorVersion: Word read GetFileMajorVersion;

    { Description:
        This property retrieves the minor version number of the file. In a
        normal version number, like 1.2.3.4, this property will
        retrieve the 2.
      See also:
        FileMajorVersion, FileRelease, FileBuild
    }
    property FileMinorVersion: Word read GetFileMinorVersion;

    { Description:
        This property retrieves the release number of the file. In a
        normal version number, like 1.2.3.4, this property will
        retrieve the 3.
      See also:
        FileMajorVersion, FileMinorVersion, FileBuild
    }
    property FileRelease: Word read GetFileRelease;

    { Description:
        This property retrieves the build number of the file. In a
        normal version number, like 1.2.3.4, this property will
        retrieve the 4.
      See also:
        FileMajorVersion, FileMinorVersion, FileRelease
    }
    property FileBuild: Word read GetFileBuild;

    { Description:
        This property retrieves the operating system information compiled
        into the file.
    }
    property FileOS: LongInt read GetFileOS;

    { Description:
        This property returns information about the file type.
      See also:
        FileSubtype
    }
    property FileType: LongInt read GetFileType;

    { Description:
        This property returns information about the file sub-type.
      See also:
        FileType
    }
    property FileSubtype: LongInt read GetFileSubtype;

    { Description:
        This property retrieves the company name compiled into the file.
      See also:
        LegalCopyright, LegalTrademarks
    }
    property CompanyName: string read GetCompanyName;

    { Description:
        This property retrieves the internal name of the file.
    }
    property InternalName: string read GetInternalName;

    { Description:
        This property retrieves the legal copyright added to the file
        at compile-time.
      See also:
        CompanyName, LegalTrademarks
    }
    property LegalCopyright: string read GetLegalCopyright;

    { Description:
        This property retrieves the legal trademarks added to the file
        at compile-time.
      See also:
        CompanyName, LegalCopyright
    }
    property LegalTrademarks: string read GetLegalTrademarks;

    { Description:
        This property retrieves the original file name the file had at
        compile-time.
    }
    property OriginalFilename: string read GetOriginalFilename;

    { Description:
        This property retrieves the major version number of the product that
        this file is a part of. In a normal version number, like 1.2.3.4, this
        property will retrieve the 1.
      See also:
        ProductMinorVersion, ProductRelease, ProductBuild
    }
    property ProductMajorVersion: Word read GetProductMajorVersion;

    { Description:
        This property retrieves the minor version number of the product that
        this file is a part of. In a normal version number, like 1.2.3.4, this
        property will retrieve the 2.
      See also:
        ProductMajorVersion, ProductRelease, ProductBuild
    }
    property ProductMinorVersion: Word read GetProductMinorVersion;

    { Description:
        This property retrieves the release number of the product that
        this file is a part of. In a normal version number, like 1.2.3.4, this
        property will retrieve the 3.
      See also:
        ProductMajorVersion, ProductMinorVersion, ProductBuild
    }
    property ProductRelease: Word read GetProductRelease;

    { Description:
        This property retrieves the build number of the product that
        this file is a part of. In a normal version number, like 1.2.3.4, this
        property will retrieve the 4.
      See also:
        ProductMajorVersion, ProductMinorVersion, ProductRelease
    }
    property ProductBuild: Word read GetProductBuild;

    { Description:
        This property will retrieve the name of the product that this file
        is a part of.
    }
    property ProductName: string read GetProductName;

    { Description:
        This property will retrieve the product version as a string.
    }
    property ProductVersion: string read GetProductVersion;

    { Description:
        This property checks if the file was marked as a debug build when it
        was compiled, and returns True if it was, False if not.
    }
    property IsDebugBuild: Boolean read GetIsDebugBuild;

    { Description:
        This property checks if the file was marked as a pre-release when it
        was compiled, and returns True if it was, False if not.
    }
    property IsPreRelease: Boolean read GetIsPreRelease;

    { Description:
        This property checks if the file was marked as a "special" build when it
        was compiled, and returns True if it was, False if not.
      See also:
        SpecialBuild
    }
    property IsSpecialBuild: Boolean read GetIsSpecialBuild;

    { Description:
        If the file was marked as a "special" build when it was compiled, this
        property will retrieve the reason/description for the special build.
      See also:
        IsSpecialBuild
    }
    property SpecialBuild: string read GetSpecialBuild;

    { Description:
        This property checks if the file was marked as a "private" build when it
        was compiled, and returns True if it was, False if not.
      See also:
        PrivateBuild
    }
    property IsPrivateBuild: Boolean read GetIsPrivateBuild;

    { Description:
        If the file was marked as a "private" build when it was compiled, this
        property will retrieve the reason/description for the private build.
      See also:
        IsPrivateBuild
    }
    property PrivateBuild: string read GetPrivateBuild;

  published
    { Description:
        Set this property to the path and filename of the file you wish to
        read the version information from. Set it to blank to use the
        currently executing application.
    }
    property Filename: string read FFilename write SetFilename;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion write SetPackageVersion stored False;
  end;

resourcestring
  ERR_VERINFO_UNABLE_TO_RETRIEVE  = 'Unable to retrieve version information from program file';
  ERR_VERINFO_NO_VERINFO          = 'No version information in program file';

implementation

uses
  Dialogs, TypInfo, Controls;
  
{ TlvkVersionInformation }

function TlvkVersionInformation.GetComments: string;
begin
  ReadData;
  Result := FComments;
end;

function TlvkVersionInformation.GetCompanyName: string;
begin
  ReadData;
  Result := FCompanyName;
end;

function TlvkVersionInformation.GetIsDebugBuild: Boolean;
begin
  Result := (GetFileFlags and 1) <> 0;
end;

function TlvkVersionInformation.GetFileBuild: Word;
begin
  ReadData;
  Result := FFileBuild;
end;

function TlvkVersionInformation.GetFileDate: TDateTime;
begin
  ReadData;
  Result := FFileDate;
end;

function TlvkVersionInformation.GetFileDescription: string;
begin
  ReadData;
  Result := FFileDescription;
end;

function TlvkVersionInformation.GetFileFlags: LongInt;
begin
  ReadData;
  Result := FFileFlags;
end;

function TlvkVersionInformation.GetFileFlagsMask: LongInt;
begin
  ReadData;
  Result := FFileFlagsMask;
end;

function TlvkVersionInformation.GetFileMajorVersion: Word;
begin
  ReadData;
  Result := FFileMajorVersion;
end;

function TlvkVersionInformation.GetFileMinorVersion: Word;
begin
  ReadData;
  Result := FFileMinorVersion;
end;

function TlvkVersionInformation.GetFileOS: LongInt;
begin
  ReadData;
  Result := FFileOS;
end;

function TlvkVersionInformation.GetFileRelease: Word;
begin
  ReadData;
  Result := FFileRelease;
end;

function TlvkVersionInformation.GetFileSubtype: LongInt;
begin
  ReadData;
  Result := FFileSubtype;
end;

function TlvkVersionInformation.GetFileType: LongInt;
begin
  ReadData;
  Result := FFileType;
end;

function TlvkVersionInformation.GetFileVersion: string;
begin
  ReadData;
  Result := FFileVersion;
end;

function TlvkVersionInformation.GetInternalName: string;
begin
  ReadData;
  Result := FInternalName;
end;

function TlvkVersionInformation.GetLanguageName: string;
begin
  ReadData;
  Result := FLanguageName;
end;

function TlvkVersionInformation.GetLegalCopyright: string;
begin
  ReadData;
  Result := FLegalCopyright;
end;

function TlvkVersionInformation.GetLegalTrademarks: string;
begin
  ReadData;
  Result := FLegalTrademarks;
end;

function TlvkVersionInformation.GetOriginalFilename: string;
begin
  ReadData;
  Result := FOriginalFilename;
end;

function TlvkVersionInformation.GetIsPreRelease: Boolean;
begin
  Result := (GetFileFlags and 2) <> 0;
end;

function TlvkVersionInformation.GetIsPrivateBuild: Boolean;
begin
  Result := (GetFileFlags and 8) <> 0;
end;

function TlvkVersionInformation.GetProductBuild: Word;
begin
  ReadData;
  Result := FProductBuild;
end;

function TlvkVersionInformation.GetProductMajorVersion: Word;
begin
  ReadData;
  Result := FProductMajorVersion;
end;

function TlvkVersionInformation.GetProductMinorVersion: Word;
begin
  ReadData;
  Result := FProductMinorVersion;
end;

function TlvkVersionInformation.GetProductName: string;
begin
  ReadData;
  Result := FProductName;
end;

function TlvkVersionInformation.GetProductRelease: Word;
begin
  ReadData;
  Result := FProductRelease;
end;

function TlvkVersionInformation.GetProductVersion: string;
begin
  ReadData;
  Result := FProductVersion;
end;

function TlvkVersionInformation.GetIsSpecialBuild: Boolean;
begin
  Result := (GetFileFlags and 32) <> 0;
end;

procedure TlvkVersionInformation.ReadData;
type
  PTranslation = ^TTranslation;
  TTranslation = record
    Language      : Word;
    CharacterSet  : Word;
  end;
var
  DummyHandle   : Cardinal;
  Size          : Integer;
  Buffer        : Pointer;
  Value         : Pointer;
  ValueLen      : Cardinal;

  Translation   : PTranslation;
  Temp          : array[0..511] of Char;
  Prefix        : string;
  PredefInfo    : TVSFixedFileInfo;
  rc            : Boolean;
  FilenameToUse : array[0..MAX_PATH+1] of Char;

  function GetString(const Name: string): string;
  var
    rc  : Boolean;
  begin
    rc := VerQueryValue(Buffer, PChar(Prefix + Name), Value, ValueLen);
    if rc and (ValueLen > 0) then
      Result := StrPas(PChar(Value))
    else
      Result := '';
  end;

  procedure Decipher(const VerString: string; out Major, Minor, Release, Build: Word);
  var
    Index : Integer;
    Part  : Integer;
  begin
    Major := 0;
    Minor := 0;
    Release := 0;
    Build := 0;

    Index := 1;
    Part := 1;
    while Index <= Length(VerString) do
    begin
      case VerString[Index] of
        '.':
          Inc(Part);

        '0'..'9':
          case Part of
            1:
              Major := Major*10 + Ord(VerString[Index])-48;

            2:
              Minor := Minor*10 + Ord(VerString[Index])-48;

            3:
              Release := Release*10 + Ord(VerString[Index])-48;

            4:
              Build := Build*10 + Ord(VerString[Index])-48;
          end;
      end;

      Inc(Index);
    end;
  end;

begin
  if FDataRead then
    Exit;

  if FFilename <> '' then
    StrPLCopy(FilenameToUse, FFilename, SizeOf(FilenameToUse)-1)
  else
    StrPLCopy(FilenameToUse, ParamStr(0), SizeOf(FilenameToUse)-1);
  Size := GetFileVersionInfoSize(FilenameToUse, DummyHandle);
  if Size = 0 then
    raise Exception.Create(ERR_VERINFO_NO_VERINFO);
  GetMem(Buffer, Size);
  try
    rc := GetFileVersionInfo(FilenameToUse, 0, Size, Buffer);

    if not rc then
      raise Exception.Create(ERR_VERINFO_UNABLE_TO_RETRIEVE);

    if VerQueryValue(Buffer, '\VarFileInfo\Translation', Value, ValueLen) then
    begin
      GetMem(Translation, ValueLen);
      try
        Move(Value^, Translation^, ValueLen);
        VerLanguageName(Translation^.Language, Temp, SizeOf(Temp));
        FLanguageName := Temp;

        Prefix := Format('StringFileInfo\%.4x%.4x\', [Translation^.Language, Translation^.CharacterSet]);

        FillChar(PredefInfo, SizeOf(PredefInfo), #0);
        VerQueryValue(Buffer, '\', Value, ValueLen);
        Move(Value^, PredefInfo, ValueLen);

        FFileFlagsMask := PredefInfo.dwFileFlagsMask;
        FFileFlags := PredefInfo.dwFileFlags;
        FFileDate := MakeLong(PredefInfo.dwFileDateMS, PredefInfo.dwFileDateLS);
        FFileOS := PredefInfo.dwFileOS;
        FFileType := PredefInfo.dwFileType;
        FFileSubtype := PredefInfo.dwFileSubtype;

        FComments := GetString('Comments');
        FCompanyName := GetString('CompanyName');
        FFileVersion := GetString('FileVersion');
        FInternalName := GetString('InternalName');
        FLegalCopyright := GetString('LegalCopyright');
        FLegalTrademarks := GetString('LegalTrademarks');
        FOriginalFilename := GetString('OriginalFilename');
        FProductName := GetString('ProductName');
        FProductVersion := GetString('ProductVersion');
        FFileDescription := GetString('FileDescription');
        FPrivateBuild := GetString('PrivateBuild');
        FSpecialBuild := GetString('SpecialBuild');

        Decipher(FFileVersion, FFileMajorVersion, FFileMinorVersion, FFileRelease, FFileBuild);
        Decipher(FProductVersion, FProductMajorVersion, FProductMinorVersion, FProductRelease, FProductBuild);
      finally
        FreeMem(Translation);
      end;
    end else
      raise Exception.Create(ERR_VERINFO_UNABLE_TO_RETRIEVE);

    FDataRead := True;
  finally
    FreeMem(Buffer);
  end;
end;

function TlvkVersionInformation.GetPrivateBuild: string;
begin
  ReadData;
  Result := FPrivateBuild;
end;

function TlvkVersionInformation.GetSpecialBuild: string;
begin
  ReadData;
  Result := FSpecialBuild;
end;

procedure TlvkVersionInformation.SetFilename(const NewFilename: string);
begin
  if FFilename <> NewFilename then
  begin
    FFilename := NewFilename;
    FDataRead := False;
  end;
end;

constructor TlvkVersionInformation.Create(AOwner: TComponent);
begin
  inherited;

  FFilename := '';
  FDataRead := False;
end;

function TlvkVersionInformation.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkVersionInformation.SetPackageVersion(
  const Value: TPackageVersion);
begin
  // Do nothing
end;

end.
