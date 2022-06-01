{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains CRC16 and CRC32 code to calculate checksums of data.
}
unit lvkCRC;

// $Author: Lasse V. Karlsen $
// $Revision: 4 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkCRC.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  lvkTypes;

type
  { Description:
      This type is used for holding a CRC16 value, an unsigned 16-bit value.
    See also:
      CRC16Init, CRC16Process, CRC16Done, TCRC32
  }
  TCRC16  = UInt16;

  { Description:
      This type is used for holding a CRC32 value, an unsigned 32-bit value.
    See also:
      CRC32Init, CRC32Process, CRC32Done, TCRC16
  }
  TCRC32  = UInt32;

{ Description:
    This procedure initializes a TCRC16 variable with the value to use when
    starting a checksum generation process.
  Example:
<CODE>
<b>var</b>
  c16 : TCRC16;
  s   : <b>string</b>;
<b>begin</b>
  CRC16Init(c16);
  s := 'This is a test';
  CRC16Process(c16, PChar(s)^, Length(s));
  CRC16Done(c16);
  <i>// c16 now holds the CRC16 value of the string contents</i>
  ...
</CODE>
  Parameters:
    CRC16 - Output parameter that will hold the initial CRC16 value for the
      variable.
  See also:
    CRC16Process, CRC16Done@TCRC16, TCRC16
}
procedure CRC16Init(out CRC16: TCRC16);

{ Description:
    This procedure processes a block of bytes and incorporates them into the
    running checksum stored in the CRC16 variable.

    Note: See CRC16Init for an example of usage.
  Parameters:
    CRC16 - The running checksum variable.
    Data  - The data to process.
    Size  - How many bytes to process from Data.
  See also:
    CRC16Init@TCRC16, CRC16Done@TCRC16, TCRC16
}
procedure CRC16Process(var CRC16: TCRC16; const Data; const Size: UInt32);

{ Description:
    This procedure finalizes the value in the CRC16 variable so that it can
    be stored, compared to other values, etc.

    Note: See CRC16Init for an example of usage.
  Parameters:
    CRC16 - The variable to finalize.
  See also:
    CRC16Init@TCRC16, CRC16Process, TCRC16
}
procedure CRC16Done(var CRC16: TCRC16);

{ Description:
    This procedure initializes a TCRC32 variable with the value to use when
    starting a checksum generation process.
  Example:
<CODE>
<b>var</b>
  c32 : TCRC32;
  s   : <b>string</b>;
<b>begin</b>
  CRC32Init(c32);
  s := 'This is a test';
  CRC32Process(c32, PChar(s)^, Length(s));
  CRC32Done(c32);
  <i>// c32 now holds the CRC32 value of the string contents</i>
  ...
</CODE>
  Parameters:
    CRC32 - Output parameter that will hold the initial CRC32 value for the
      variable.
  See also:
    CRC32Process, CRC32Done@TCRC32, TCRC32
}
procedure CRC32Init(out CRC32: TCRC32);

{ Description:
    This procedure processes a block of bytes and incorporates them into the
    running checksum stored in the CRC32 variable.

    Note: See CRC32Init for an example of usage.
  Parameters:
    CRC32 - The running checksum variable.
    Data  - The data to process.
    Size  - How many bytes to process from Data.
  See also:
    CRC32Init@TCRC32, CRC32Done@TCRC32, TCRC32
}
procedure CRC32Process(var CRC32: TCRC32; const Data; const Size: UInt32);

{ Description:
    This procedure finalizes the value in the CRC32 variable so that it can
    be stored, compared to other values, etc.

    Note: See CRC32Init for an example of usage.
  Parameters:
    CRC32 - The variable to finalize.
  See also:
    CRC32Init@TCRC32, CRC32Process, TCRC32
}
procedure CRC32Done(var CRC32: TCRC32);

implementation

const
	CRC16Table	: array[Byte] of Word = (
		$0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241, $C601, $06C0, $0780,
		$C741, $0500, $C5C1, $C481, $0440, $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1,
		$CE81, $0E40, $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841, $D801,
		$18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40, $1E00, $DEC1, $DF81, $1F40,
		$DD01, $1DC0, $1C80, $DC41, $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680,
		$D641, $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040, $F001, $30C0,
		$3180, $F141, $3300, $F3C1, $F281, $3240, $3600, $F6C1, $F781, $3740, $F501,
		$35C0, $3480, $F441, $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
		$FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840, $2800, $E8C1, $E981,
		$2940, $EB01, $2BC0, $2A80, $EA41, $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1,
		$EC81, $2C40, $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640, $2200,
		$E2C1, $E381, $2340, $E101, $21C0, $2080, $E041, $A001, $60C0, $6180, $A141,
		$6300, $A3C1, $A281, $6240, $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480,
		$A441, $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41, $AA01, $6AC0,
		$6B80, $AB41, $6900, $A9C1, $A881, $6840, $7800, $B8C1, $B981, $7940, $BB01,
		$7BC0, $7A80, $BA41, $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
		$B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640, $7200, $B2C1, $B381,
		$7340, $B101, $71C0, $7080, $B041, $5000, $90C1, $9181, $5140, $9301, $53C0,
		$5280, $9241, $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440, $9C01,
		$5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40, $5A00, $9AC1, $9B81, $5B40,
		$9901, $59C0, $5880, $9841, $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81,
		$4A40, $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41, $4400, $84C1,
		$8581, $4540, $8701, $47C0, $4680, $8641, $8201, $42C0, $4380, $8341, $4100,
		$81C1, $8081, $4040
	 );

const
	CRC32Table	: array[Byte] of LongWord = (
		$000000000, $077073096, $0ee0e612c, $0990951ba, $0076dc419, $0706af48f,
		$0e963a535, $09e6495a3, $00edb8832, $079dcb8a4, $0e0d5e91e, $097d2d988,
		$009b64c2b, $07eb17cbd, $0e7b82d07, $090bf1d91, $01db71064, $06ab020f2,
		$0f3b97148, $084be41de, $01adad47d, $06ddde4eb, $0f4d4b551, $083d385c7,
		$0136c9856, $0646ba8c0, $0fd62f97a, $08a65c9ec, $014015c4f, $063066cd9,
		$0fa0f3d63, $08d080df5, $03b6e20c8, $04c69105e, $0d56041e4, $0a2677172,
		$03c03e4d1, $04b04d447, $0d20d85fd, $0a50ab56b, $035b5a8fa, $042b2986c,
		$0dbbbc9d6, $0acbcf940, $032d86ce3, $045df5c75, $0dcd60dcf, $0abd13d59,
		$026d930ac, $051de003a, $0c8d75180, $0bfd06116, $021b4f4b5, $056b3c423,
		$0cfba9599, $0b8bda50f, $02802b89e, $05f058808, $0c60cd9b2, $0b10be924,
		$02f6f7c87, $058684c11, $0c1611dab, $0b6662d3d, $076dc4190, $001db7106,
		$098d220bc, $0efd5102a, $071b18589, $006b6b51f, $09fbfe4a5, $0e8b8d433,
		$07807c9a2, $00f00f934, $09609a88e, $0e10e9818, $07f6a0dbb, $0086d3d2d,
		$091646c97, $0e6635c01, $06b6b51f4, $01c6c6162, $0856530d8, $0f262004e,
		$06c0695ed, $01b01a57b, $08208f4c1, $0f50fc457, $065b0d9c6, $012b7e950,
		$08bbeb8ea, $0fcb9887c, $062dd1ddf, $015da2d49, $08cd37cf3, $0fbd44c65,
		$04db26158, $03ab551ce, $0a3bc0074, $0d4bb30e2, $04adfa541, $03dd895d7,
		$0a4d1c46d, $0d3d6f4fb, $04369e96a, $0346ed9fc, $0ad678846, $0da60b8d0,
		$044042d73, $033031de5, $0aa0a4c5f, $0dd0d7cc9, $05005713c, $0270241aa,
		$0be0b1010, $0c90c2086, $05768b525, $0206f85b3, $0b966d409, $0ce61e49f,
		$05edef90e, $029d9c998, $0b0d09822, $0c7d7a8b4, $059b33d17, $02eb40d81,
		$0b7bd5c3b, $0c0ba6cad, $0edb88320, $09abfb3b6, $003b6e20c, $074b1d29a,
		$0ead54739, $09dd277af, $004db2615, $073dc1683, $0e3630b12, $094643b84,
		$00d6d6a3e, $07a6a5aa8, $0e40ecf0b, $09309ff9d, $00a00ae27, $07d079eb1,
		$0f00f9344, $08708a3d2, $01e01f268, $06906c2fe, $0f762575d, $0806567cb,
		$0196c3671, $06e6b06e7, $0fed41b76, $089d32be0, $010da7a5a, $067dd4acc,
		$0f9b9df6f, $08ebeeff9, $017b7be43, $060b08ed5, $0d6d6a3e8, $0a1d1937e,
		$038d8c2c4, $04fdff252, $0d1bb67f1, $0a6bc5767, $03fb506dd, $048b2364b,
		$0d80d2bda, $0af0a1b4c, $036034af6, $041047a60, $0df60efc3, $0a867df55,
		$0316e8eef, $04669be79, $0cb61b38c, $0bc66831a, $0256fd2a0, $05268e236,
		$0cc0c7795, $0bb0b4703, $0220216b9, $05505262f, $0c5ba3bbe, $0b2bd0b28,
		$02bb45a92, $05cb36a04, $0c2d7ffa7, $0b5d0cf31, $02cd99e8b, $05bdeae1d,
		$09b64c2b0, $0ec63f226, $0756aa39c, $0026d930a, $09c0906a9, $0eb0e363f,
		$072076785, $005005713, $095bf4a82, $0e2b87a14, $07bb12bae, $00cb61b38,
		$092d28e9b, $0e5d5be0d, $07cdcefb7, $00bdbdf21, $086d3d2d4, $0f1d4e242,
		$068ddb3f8, $01fda836e, $081be16cd, $0f6b9265b, $06fb077e1, $018b74777,
		$088085ae6, $0ff0f6a70, $066063bca, $011010b5c, $08f659eff, $0f862ae69,
		$0616bffd3, $0166ccf45, $0a00ae278, $0d70dd2ee, $04e048354, $03903b3c2,
		$0a7672661, $0d06016f7, $04969474d, $03e6e77db, $0aed16a4a, $0d9d65adc,
		$040df0b66, $037d83bf0, $0a9bcae53, $0debb9ec5, $047b2cf7f, $030b5ffe9,
		$0bdbdf21c, $0cabac28a, $053b39330, $024b4a3a6, $0bad03605, $0cdd70693,
		$054de5729, $023d967bf, $0b3667a2e, $0c4614ab8, $05d681b02, $02a6f2b94,
		$0b40bbe37, $0c30c8ea1, $05a05df1b, $02d02ef8d
	 );

procedure CRC16Init(out CRC16: TCRC16);
begin
  CRC16 := $0000;
end;

procedure CRC16Process(var CRC16: TCRC16; const Data; const Size: UInt32);
var
  DataPtr : PChar;
  Index   : Integer;
begin
  DataPtr := @Data;
  for Index := 0 to Size-1 do
    CRC16 := CRC16Table[(Ord(DataPtr[Index]) xor CRC16) and 255] xor (CRC16 shr 8);
end;

procedure CRC16Done(var CRC16: TCRC16);
begin
  // Nothing to do here
end;

procedure CRC32Init(out CRC32: TCRC32);
begin
  CRC32 := $FFFFFFFF;
end;

procedure CRC32Process(var CRC32: TCRC32; const Data; const Size: UInt32);
var
  DataPtr : PChar;
  Index   : Integer;
begin
  DataPtr := @Data;
  for Index := 0 to Size-1 do
    CRC32 := ((CRC32 shr 8) and $FFFFFF) xor CRC32Table[(CRC32 xor Ord(DataPtr[Index])) and 255];
end;

procedure CRC32Done(var CRC32: TCRC32);
begin
  CRC32 := CRC32 xor $FFFFFFFF;
end;

end.
