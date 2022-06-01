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
unit lvkZLibConsts;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkZLibConsts.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

const
	SEEK_SET							= 0;
	SEEK_CUR							= 1;
	ZLIB_VERSION					: PAnsiChar = '1.1.4';

	Z_NO_FLUSH						= 0;
	Z_PARTIAL_FLUSH				= 1;
	Z_SYNC_FLUSH					= 2;
	Z_FULL_FLUSH					= 3;
	Z_FINISH							= 4;

	Z_OK									= 0;
	Z_STREAM_END					= 1;
	Z_NEED_DICT						= 2;
	Z_ERRNO								= (-1);
	Z_STREAM_ERROR				= (-2);
	Z_DATA_ERROR					= (-3);
	Z_MEM_ERROR						= (-4);
	Z_BUF_ERROR						= (-5);
	Z_VERSION_ERROR				= (-6);

	Z_NO_COMPRESSION			= 0;
	Z_BEST_SPEED					= 1;
	Z_BEST_COMPRESSION		= 9;
	Z_DEFAULT_COMPRESSION	= (-1);

	Z_FILTERED						= 1;
	Z_HUFFMAN_ONLY				= 2;
	Z_DEFAULT_STRATEGY		= 0;

	Z_BINARY							= 0;
	Z_ASCII								= 1;
	Z_UNKNOWN							= 2;

	Z_DEFLATED						= 8;

	Z_NULL								= nil;

	ZLIBDLLNAME						= 'zLib.dll';

implementation

end.

