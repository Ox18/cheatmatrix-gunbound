{ Description:
    For documentation for the zLib compression engine,
    please visit the official zLib webpage at:

    <EXTLINK http://www.gzip.org/zlib/>http://www.gzip.org/zlib/</EXTLINK>
}
unit lvkZLibTypes;

// $Revision: 5 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkZLibTypes.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

type
	PInteger				= ^Integer;
	uInt						= LongWord;
	uLong						= LongWord;
	uLongf					= ULONG;
	puLongf					= ^ULONG;
	voidpf					= Pointer;
	voidp						= Pointer;
	z_off_t					= LongInt;
	crc_table				= array[ Byte ] of uLongf;
	pcrc_table			= ^crc_table;
	TGZFILE					= VOIDP;
	alloc_func			= function( opaque : voidpf; items : uInt; size : uInt ): Pointer; stdcall;
	free_func				= procedure( opaque : voidpf; address : voidpf ); stdcall;
	internal_state	= record end;
	pinternal_state	= ^internal_state;
	z_stream				= record
		next_in				: PAnsiChar;
		avail_in			: uInt;
		total_in			: uLong;
		next_out			: PAnsiChar;
		avail_out			: uInt;
		total_out			: uLong;
		msg						: PAnsiChar;
		state					: pinternal_state;
		zalloc				: alloc_func;
		zfree					: free_func;
		opaque				: Pointer;
		data_type			: Integer;
		adler					: uLong;
		reserved			: uLong;
	end;

implementation

end.

