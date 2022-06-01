{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    For documentation for the zLib compression engine,
    please visit the official zLib webpage at:

    <EXTLINK http://www.gzip.org/zlib/>http://www.gzip.org/zlib/</EXTLINK>
}
unit lvkZLib;

// $Revision: 4 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkZLib.pas $

{ zlib.h -- interface of the 'zlib' general purpose compression library
  version 1.1.4, March 11th, 2002

	Copyright (C) 1995-1998 Jean-loup Gailly and Mark Adler

	This software is provided 'as-is', without any express or implied
	warranty.  In no event will the authors be held liable for any damages
	arising from the use of this software.

	Permission is granted to anyone to use this software for any purpose,
	including commercial applications, and to alter it and redistribute it
	freely, subject to the following restrictions:

	1. The origin of this software must not be misrepresented; you must not
		 claim that you wrote the original software. If you use this software
		 in a product, an acknowledgment in the product documentation would be
		 appreciated but is not required.
	2. Altered source versions must be plainly marked as such, and must not be
		 misrepresented as being the original software.
	3. This notice may not be removed or altered from any source distribution.

	Jean-loup Gailly        Mark Adler
	jloup@gzip.org          madler@alumni.caltech.edu


	The data format used by the zlib library is described by RFCs (Request for
	Comments) 1950 to 1952 in the files ftp://ds.internic.net/rfc/rfc1950.txt
	(zlib format), rfc1951.txt (deflate format) and rfc1952.txt (gzip format).

	zLib.pas -- Delphi 2.0-4.0 interface for the pre-built 'zlib' DLL available
	at http://www.winimage.com/zLibDll/.

	This code is free under the same terms and conditions as 'zlib.'

	Delphi version originally created by Don Hatlestad:

	Copyright (c) 1996 Don Hatlestad, Art Anderson Associates

	Version updated by Lasse Vågsæther Karlsen to conform to changes done to
	the c release of zLib, up to and including version 1.1.4. Also, all
	comments and unnecessary code, constants and structures have been
	removed from this source file, and it has been generally tidied up.

	If you wish to read the comments for each function, look them up in the
	zLib.h file which follows this zLib.pas file. zLib.h is the 1.1.3 header
	file from the original zlib package.

	zLib is available from these sources:
		Official Homepage:			http://www.cdrom.com/pub/infozip/zlib/zlib.html
		zLib.DLL homepage:			http://www.winimage.com/zLibDll/
		zLib.pas homepage:			http://www.cintra.no/lasse/streamfilter.html
}

// (C) Lasse Vågsæther Karlsen, 1999-2002
// lasse.karlsen@norcem.no

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
	SysUtils, lvkZLibTypes, lvkZLibConsts;

function zLibVersion: PAnsiChar;
function deflate(var strm: z_stream; flush: Integer): Integer; register;
function deflateEnd(var strm: z_stream): Integer; register;
function inflate(var strm: z_stream; flush: Integer): Integer; register;
function inflateEnd(var strm: z_stream): Integer; register;
function deflateSetDictionary(var strm: z_stream; dictionary: PAnsiChar; dictLength: uInt): Integer; register;
function deflateCopy(var dest: z_stream; const source: z_stream): Integer; register;
function deflateReset(var strm: z_stream): Integer; register;
function deflateParams(var strm: z_stream; level: Integer; strategy: Integer): Integer; register;
function inflateSetDictionary(var strm: z_stream; dictionary: PAnsiChar; dictLength: uInt): Integer; register;
function inflateSync(var strm: z_stream): Integer; register;
function inflateReset(var strm: z_stream): Integer; register;
function compress(dest: Pointer; var destLen: uLong; source: Pointer; sourceLen: ULONG): Integer; register;
function compress2(dest: Pointer; var destLen: uLong; source: Pointer; sourceLen: ULONG; level: Integer): Integer; register;
function uncompress(dest: Pointer; var destLen: uLong; source: Pointer; sourceLen: LongInt): Integer; register;
function adler32(adler: ULONG; buf: Pointer; len: uInt): ULONG; register;
function crc32(crc: ULONG; buf: Pointer; len: uInt): ULONG; register;
function deflateInit_(var strm: z_stream; level: Integer; version: PAnsiChar; stream_size: Integer): Integer; register;
function inflateInit_(var strm: z_stream; version: PAnsiChar; stream_size: Integer): Integer; register;
function deflateInit2_(var strm: z_stream; level: Integer; method: Integer; windowBits: Integer; memLevel: Integer; strategy: Integer; version: PAnsiChar; stream_size: Integer): Integer; register;
function inflateInit2_(var strm: z_stream; windowBits: Integer; version: PAnsiChar; stream_size: Integer): Integer; register;
function zError(err: Integer): PAnsiChar;
function inflateSyncPoint(var z: z_stream): Integer; register;
function get_crc_table: pcrc_table; register;

function deflateInit(var	strm: z_stream; level: Integer): Integer;
function inflateInit(var strm: z_stream): Integer;
function deflateInit2(var strm: z_stream; level: Integer; method: Integer;
	windowBits: Integer; memLevel: Integer; strategy: Integer): Integer;
function InflateInit2(var strm: z_stream; windowBits: Integer): Integer;

function z_check_version: Integer;

type
  EzLib = class(Exception);
  EzLibCompress = class(EzLib);
  EzLibDecompress = class(EzLib);

procedure zLibCheckCompress(const errorCode: Integer);
procedure zLibCheckDecompress(const errorCode: Integer);

const
	zVersionOK		= 0;	// DLL version is the same version as this unit supports
	zVersionOlder	= 1;	// The DLL is an older version than
	zVersionNewer	= 2;	// The DLL is newer than what this unit supports. The program should probably not attempt
											// to use the zLib.dll functions

procedure z_init_zstream(var strm: z_stream);
	{ Clears the z_stream variable and prepares it for use }

implementation

uses
	c_rtl, Windows;

{ Support functions by LVK }

function z_check_version: Integer;
begin
	Result := StrComp(ZLIB_VERSION, zLibVersion);
	if Result < 0 then
		Result := zVersionNewer
	else if Result > 0 then
		Result := zVersionOlder
	else
		Result := zVersionOK;
end;

procedure z_init_zstream(var strm: z_stream);
begin
	with strm do
	begin
		next_in		:= nil;
		avail_in	:= 0;
		total_in	:= 0;
		next_out	:= nil;
		avail_out	:= 0;
		total_out	:= 0;
		msg				:= nil;
		state			:= nil;
		zalloc		:= nil;
		zfree			:= nil;
		opaque		:= nil;
		data_type	:= 0;
		adler			:= 0;
		reserved	:= 0;
	end;
end;

{ Support functions }

procedure zLibCheckCompress(const errorCode: Integer);
begin
  if errorCode <> Z_OK then
    raise EzLibCompress.CreateFmt('zLib compression error %d: %s', [errorCode,
      AnsiString(zError(errorCode))]);
end;

procedure zLibCheckDecompress(const errorCode: Integer);
begin
  if errorCode <> Z_OK then
    raise EzLibDecompress.CreateFmt('zLib decompression error %d: %s', [
      errorCode, AnsiString(zError(errorCode))]);
end;

function deflateInit(var strm: z_stream; level : Integer): Integer;
begin
	Result := deflateInit_(strm, level, ZLIB_VERSION, SizeOf(z_stream));
end;

function inflateInit(var strm: z_stream): Integer;
begin
	Result := inflateInit_(strm, ZLIB_VERSION, SizeOf(z_stream));
end;

function deflateInit2(var strm: z_stream; level: Integer; method: Integer; windowBits: Integer;
	memLevel: Integer; strategy: Integer) : Integer;
begin
	Result := deflateInit2_(strm, level, method, windowBits, memLevel, strategy, ZLIB_VERSION, SizeOf(z_stream));
end;

function inflateInit2(var strm: z_stream; windowBits: Integer) : Integer;
begin
	Result := inflateInit2_(strm, windowBits, ZLIB_VERSION, SizeOf(z_stream));
end;

{function _memcpy(dest, src: Pointer; n: Integer): Pointer; cdecl;
begin
	Move(src^, dest^, n);
	Result := dest;
end;

function _memset(s: Pointer; c, n: Integer): Pointer; cdecl;
begin
	FillChar(s^, n, c);
	Result := s;
end; }

function zcalloc(opaque: Pointer; items, size: LongWord): Pointer; register;
begin
	GetMem(Result, items * Size);
end;

procedure zcfree(opaque, ptr: Pointer); register;
begin
	FreeMem(ptr);
end;

const
	z_errmsg	: array[0..9] of PAnsiChar = (
		'need dictionary',     // Z_NEED_DICT       2
		'stream end',          // Z_STREAM_END      1
		'',                    // Z_OK              0
		'file error',          // Z_ERRNO         (-1)
		'stream error',        // Z_STREAM_ERROR  (-2)
		'data error',          // Z_DATA_ERROR    (-3)
		'insufficient memory', // Z_MEM_ERROR     (-4)
		'buffer error',        // Z_BUF_ERROR     (-5)
		'incompatible version',// Z_VERSION_ERROR (-6)
		''
	);

{_$L zlib-1.1.4\zutil.obj}
{$L zlib-1.1.4\deflate.obj}
{$L zlib-1.1.4\adler32.obj}
{$L zlib-1.1.4\trees.obj}
{$L zlib-1.1.4\inflate.obj}
{$L zlib-1.1.4\infblock.obj}
{$L zlib-1.1.4\infcodes.obj}
{$L zlib-1.1.4\inftrees.obj}
{$L zlib-1.1.4\infutil.obj}
{$L zlib-1.1.4\inffast.obj}
{$L zlib-1.1.4\compress.obj}
{$L zlib-1.1.4\uncompr.obj}
{$L zlib-1.1.4\crc32.obj}
function deflate; register; external;
function deflateEnd; register; external;
function inflate; register; external;
function inflateEnd; register; external;
function deflateSetDictionary; register; external;
function deflateCopy; register; external;
function deflateReset; register; external;
function deflateParams; register; external;
function inflateSetDictionary; register; external;
function inflateSync; register; external;
function inflateReset; register; external;
function compress; register; external;
function compress2; register; external;
function uncompress; register; external;
function adler32; register; external;
function crc32; register; external;
function deflateInit_; register; external;
function inflateInit_; register; external;
function deflateInit2_; register; external;
function inflateInit2_; register; external;
function inflateSyncPoint; register; external;
function get_crc_table; register; external;

function zError(err: Integer): PAnsiChar;
begin
  Result := z_errmsg[2-err];
end;

function zLibVersion: PAnsiChar;
begin
  Result := '1.1.4';
end;

end.

