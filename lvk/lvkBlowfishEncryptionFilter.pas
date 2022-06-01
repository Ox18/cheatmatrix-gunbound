{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the code for implementing the Blowfish encryption
    algorithm.
}
unit lvkBlowfishEncryptionFilter;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkBlowfishEncryptionFilter.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, SysUtils, lvkStreamFilters, lvkBasicEncryptionFilters;

const
	Rounds	= 16;

type
  { Description:
      This type is used as the basis for keys passed to the Blowfish
      encryption algorithm.
  }
  TBlowfishKey  = array[0..(Rounds+2)*4-1] of Byte;

{ Description:
    This function creates a new stream filter that implements the
    Blowfish encryption algorithm.

    This filter is write-only.
  Parameters:
    Passphrase - The passphrase to use as a key for encrypting the data.
  See also:
    NewWriteableBlowfishEncryptionFilter@TBlowfishKey
}
function NewWriteableBlowfishEncryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Blowfish encryption algorithm.

    This filter is write-only.
  Parameters:
    Key - The key to use when encrypting the data.
  See also:
    NewWriteableBlowfishEncryptionFilter@string
}
function NewWriteableBlowfishEncryptionFilter(
  const Key: TBlowfishKey): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Blowfish decryption algorithm.

    This filter is write-only.
  Parameters:
    Passphrase - The passphrase to use as a key for decrypting the data.
  See also:
    NewWriteableBlowfishDecryptionFilter@TBlowfishKey
}
function NewWriteableBlowfishDecryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Blowfish decryption algorithm.

    This filter is write-only.
  Parameters:
    Key - The key to use when decrypting the data.
  See also:
    NewWriteableBlowfishDecryptionFilter@string
}
function NewWriteableBlowfishDecryptionFilter(
  const Key: TBlowfishKey): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Blowfish encryption algorithm.

    This filter is read-only.
  Parameters:
    Passphrase - The passphrase to use as a key for encrypting the data.
  See also:
    NewReadableBlowfishEncryptionFilter@TBlowfishKey
}
function NewReadableBlowfishEncryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Blowfish encryption algorithm.

    This filter is read-only.
  Parameters:
    Key - The key to use when encrypting the data.
  See also:
    NewReadableBlowfishEncryptionFilter@string
}
function NewReadableBlowfishEncryptionFilter(
  const Key: TBlowfishKey): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Blowfish decryption algorithm.

    This filter is read-only.
  Parameters:
    Passphrase - The passphrase to use as a key for decrypting the data.
  See also:
    NewReadableBlowfishDecryptionFilter@TBlowfishKey
}
function NewReadableBlowfishDecryptionFilter(
  const Passphrase: string): IStreamFilter; overload;

{ Description:
    This function creates a new stream filter that implements the
    Blowfish decryption algorithm.

    This filter is read-only.
  Parameters:
    Key - The key to use when decrypting the data.
  See also:
    NewReadableBlowfishDecryptionFilter@string
}
function NewReadableBlowfishDecryptionFilter(
  const Key: TBlowfishKey): IStreamFilter; overload;

implementation

type
	TPType	= array[0..Rounds+1] of LongWord;
	TSType	= array[0..3, Byte] of LongWord;

  TBlowfishBlock = array[0..1] of LongWord;

  TBlowfishEncryptionAlgorithm = class(TInterfacedObject, IEncryptionAlgorithm)
  private
		P	: TPType;
		S	: TSType;

    procedure CalculateSubKeys(const Key: TBlowfishKey);
		procedure Cipher(var Block: TBlowfishBlock; StartN, DeltaN: Integer);

  protected
    // IEncryptionAlgorithm interface
    function GetBlockSize: Integer;
    procedure EncryptBlock(var Block);
    procedure DecryptBlock(var Block);

  public
    constructor Create(const Key: TBlowfishKey); overload;
    constructor Create(const Passphrase: string); overload;
  end;

const
	InitP: TPType  =
	(
		$886A3F24, $D308A385, $2E8A1913, $44737003,
		$223809A4, $D0319F29, $98FA2E08, $896C4EEC,
		$E6212845, $7713D038, $CF6654BE, $6C0CE934,
		$B729ACC0, $DD507CC9, $B5D5843F, $170947B5,
		$D9D51692, $1BFB7989
	 ); // InitP[ ... ]

	InitS: TSType =
	(
		(
			$A60B31D1, $ACB5DF98, $DB72FD2F, $B7DF1AD0, $EDAFE1B8, $967E266A, $45907CBA, $997F2CF1,
			$4799A124, $F76C91B3, $E2F20108, $16FC8E85, $D8206963, $694E5771, $A3FE58A4, $7E3D93F4,
			$8F74950D, $58B68E72, $58CD8B71, $EE4A1582, $1DA4547B, $B5595AC2, $39D5309C, $1360F22A,
			$23B0D1C5, $F0856028, $187941CA, $EF38DBB8, $B0DC798E, $0E183A60, $8B0E9E6C, $3E8A1EB0,
			$C17715D7, $274B31BD, $DA2FAF78, $605C6055, $F32555E6, $94AB55AA, $62984857, $4014E863,
			$6A39CA55, $B610AB2A, $345CCCB4, $CEE84111, $AF8654A1, $93E9727C, $1114EEB3, $2ABC6F63,
			$5DC5A92B, $F6311874, $163E5CCE, $1E93879B, $33BAD6AF, $5CCF246C, $8153327A, $77869528,
			$98488F3B, $AFB94B6B, $1BE8BFC4, $93212866, $CC09D861, $91A921FB, $60AC7C48, $3280EC5D,
			$5D5D84EF, $B17585E9, $022326DC, $881B65EB, $813E8923, $C5AC96D3, $F36F6D0F, $3942F483, 
			$82440B2E, $042084A4, $4AF0C869, $5E9B1F9E, $4268C621, $9A6CE9F6, $619C0C67, $F088D3AB, 
			$D2A0516A, $682F54D8, $28A70F96, $A33351AB, $6C0BEF6E, $E43B7A13, $50F03BBA, $982AFB7E, 
			$1D65F1A1, $7601AF39, $3E59CA66, $880E4382, $1986EE8C, $B49F6F45, $C3A5847D, $BE5E8B3B, 
			$D8756FE0, $7320C185, $9F441A40, $A66AC156, $62AAD34E, $06773F36, $72DFFE1B, $3D029B42, 
			$24D7D037, $48120AD0, $D3EA0FDB, $9BC0F149, $C9725307, $7B1B9980, $D879D425, $F7DEE8F6, 
			$1A50FEE3, $3B4C79B6, $BDE06C97, $BA06C004, $B64FA9C1, $C4609F40, $C29E5C5E, $63246A19, 
			$AF6FFB68, $B5536C3E, $EBB23913, $6FEC523B, $1F51FC6D, $2C95309B, $444581CC, $09BD5EAF, 
			$04D0E3BE, $FD4A33DE, $07280F66, $B34B2E19, $57A8CBC0, $0F74C845, $395F0BD2, $DBFBD3B9, 
			$BDC07955, $0A32601A, $C600A1D6, $79722C40, $FE259F67, $CCA31FFB, $F8E9A58E, $F82232DB, 
			$DF16753C, $156B61FD, $C81E502F, $AB5205AD, $FAB53D32, $608723FD, $487B3153, $82DF003E, 
			$BB575C9E, $A08C6FCA, $2E56871A, $DB6917DF, $F6A842D5, $C3FF7E28, $C63267AC, $73554F8C,
			$B0275B69, $C858CABB, $5DA3FFE1, $A011F0B8, $983DFA10, $B88321FD, $6CB5FC4A, $5BD3D12D, 
			$79E4539A, $6545F8B6, $BC498ED2, $9097FB4B, $DAF2DDE1, $337ECBA4, $4113FB62, $E8C6E4CE, 
			$DACA20EF, $014C7736, $FE9E7ED0, $B41FF12B, $4DDADB95, $989190AE, $718EADEA, $A0D5936B, 
			$D0D18ED0, $E025C7AF, $2F5B3C8E, $B794758E, $FBE2F68F, $642B12F2, $12B88888, $1CF00D90, 
			$A05EAD4F, $1CC38F68, $91F1CFD1, $ADC1A8B3, $18222F2F, $77170EBE, $FE2D75EA, $A11F028B, 
			$0FCCA0E5, $E8746FB5, $D6F3AC18, $99E289CE, $E04FA8B4, $B7E013FD, $813BC47C, $D9A8ADD2, 
			$66A25F16, $05779580, $1473CC93, $77141A21, $6520ADE6, $86FAB577, $F54254C7, $CF359DFB, 
			$0CAFCDEB, $A0893E7B, $D31B41D6, $497E1EAE, $2D0E2500, $5EB37120, $BB006822, $AFE0B857, 
			$9B366424, $1EB909F0, $1D916355, $AAA6DF59, $8943C178, $7F535AD9, $A25B7D20, $C5B9E502, 
			$76032683, $A9CF9562, $6819C811, $414A734E, $CA2D47B3, $4AA9147B, $5200511B, $1529539A, 
			$3F570FD6, $E4C69BBC, $76A4602B, $0074E681, $B56FBA08, $1FE91B57, $6BEC96F2, $15D90D2A, 
			$216563B6, $B6F9B9E7, $2E0534FF, $645685C5, $5D2DB053, $A18F9FA9, $9947BA08, $6A07856E
		 ),  // InitS[ 0,  ... ]
		( 
			$E9707A4B, $4429B3B5, $2E0975DB, $232619C4, $B0A66EAD, $7DDFA749, $B860EE9C, $66B2ED8F, 
			$718CAAEC, $FF179A69, $6C526456, $E19EB1C2, $A5023619, $294C0975, $401359A0, $3E3A18E4, 
			$9A98543F, $659D425B, $D6E48F6B, $D63FF799, $079CD2A1, $F530E8EF, $E6382D4D, $C15D25F0, 
			$8620DD4C, $26EB7084, $C6E98263, $5ECC1E02, $3F6B6809, $C9EFBA3E, $1418973C, $A1706A6B, 
			$84357F68, $86E2A052, $05539CB7, $370750AA, $1C84073E, $5CAEDE7F, $EC447D8E, $B8F21657, 
			$37DA3AB0, $0D0C50F0, $041F1CF0, $FFB30002, $1AF50CAE, $B274B53C, $587A8325, $BD2109DC, 
			$F91391D1, $F62FA97C, $73473294, $0147F522, $81E5E53A, $DCDAC237, $3476B5C8, $A7DDF39A, 
			$466144A9, $0E03D00F, $3EC7C8EC, $411E75A4, $99CD38E2, $2F0EEA3B, $A1BB8032, $31B33E18, 
			$388B544E, $08B96D4F, $030D426F, $BF040AF6, $9012B82C, $797C9724, $72B07956, $AF89AFBC, 
			$1F779ADE, $100893D9, $12AE8BB3, $2E3FCFDC, $1F721255, $24716B2E, $E6DD1A50, $87CD849F, 
			$1847587A, $17DA0874, $BC9A9FBC, $8C7D4BE9, $3AEC7AEC, $FA1D85DB, $66430963, $D2C364C4, 
			$47181CEF, $08D91532, $373B43DD, $16BAC224, $434DA112, $51C4652A, $02009450, $DDE43A13,
			$9EF8DF71, $554E3110, $D677AC81, $9B19115F, $F1563504, $6BC7A3D7, $3B18113C, $09A52459, 
			$EDE68FF2, $FAFBF197, $2CBFBA9E, $6E3C151E, $7045E386, $B16FE9EA, $0A5E0E86, $B32A3E5A, 
			$1CE71F77, $FA063D4E, $B9DC6529, $0F1DE799, $D6893E80, $25C86652, $78C94C2E, $6AB3109C, 
			$BA0E15C6, $78EAE294, $533CFCA5, $F42D0A1E, $A74EF7F2, $3D2B1D36, $0F263919, $6079C219, 
			$08A72352, $B61213F7, $6EFEADEB, $661FC3EA, $9545BCE3, $83C87BA6, $D1377FB1, $28FF8C01, 
			$EFDD32C3, $A55A6CBE, $85215865, $0298AB68, $0FA5CEEE, $3B952FDB, $AD7DEF2A, $842F6E5B, 
			$28B62115, $70610729, $7547DDEC, $10159F61, $30A8CC13, $96BD61EB, $1EFE3403, $CF6303AA, 
			$905C73B5, $39A2704C, $0B9E9ED5, $14DEAACB, $BC86CCEE, $A72C6260, $AB5CAB9C, $6E84F3B2, 
			$AF1E8B64, $CAF0BD19, $B96923A0, $50BB5A65, $325A6840, $B3B42A3C, $D5E99E31, $F7B821C0, 
			$190B549B, $99A05F87, $7E99F795, $A87D3D62, $9A8837F8, $772DE397, $5F93ED11, $81126816, 
			$2988350E, $D61FE6C7, $A1DFDE96, $99BA5878, $A584F557, $6372221B, $FFC3839B, $9646C21A, 
			$EB0AB3CD, $54302E53, $E448D98F, $2831BC6D, $EFF2EB58, $EAFFC634, $61ED28FE, $733C7CEE, 
			$D9144A5D, $E3B764E8, $145D1042, $E0133E20, $B6E2EE45, $EAABAAA3, $154F6CDB, $D04FCBFA, 
			$42F442C7, $B5BB6AEF, $1D3B4F65, $0521CD41, $9E791ED8, $C74D8586, $6A474BE4, $5062813D, 
			$F2A162CF, $46268D5B, $A08388FC, $A3B6C7C1, $C324157F, $9274CB69, $0B8A8447, $85B29256, 
			$00BF5B09, $9D4819AD, $74B16214, $000E8223, $2A8D4258, $EAF5550C, $3EF4AD1D, $61703F23, 
			$92F07233, $417E938D, $F1EC5FD6, $DB3B226C, $5937DE7C, $6074EECB, $A7F28540, $6E3277CE,
			$848007A6, $9E50F819, $55D8EFE8, $3597D961, $AAA769A9, $C2060CC5, $FCAB045A, $DCCA0B80, 
			$2E7A449E, $843445C3, $0567D5FD, $C99E1E0E, $D3DB73DB, $CD885510, $79DA5F67, $404367E3, 
			$6534C4C5, $D8383E71, $9EF8283D, $20FF6DF1, $E7213E15, $4A3DB08F, $2B9FE3E6, $F7AD83DB
		 ),  // InitS[ 1,  ... ]
		( 
			$685A3DE9, $F7408194, $1C264CF6, $34296994, $F7201541, $F7D40276, $2E6BF4BC, $6800A2D4, 
			$712408D4, $6AF42033, $B7D4B743, $AF610050, $2EF6391E, $46452497, $744F2114, $40888BBF, 
			$1DFC954D, $AF91B596, $D3DDF470, $452FA066, $EC09BCBF, $8597BD03, $D06DAC7F, $0485CB31, 
			$B327EB96, $4139FD55, $E64725DA, $9A0ACAAB, $25785028, $F4290453, $DA862C0A, $FB6DB6E9, 
			$6214DC68, $006948D7, $A4C00E68, $EE8DA127, $A2FE3F4F, $8CAD87E8, $06E08CB5, $B6D6F47A, 
			$7C1ECEAA, $EC5F37D3, $99A378CE, $422A6B40, $359EFE20, $B985F3D9, $ABD739EE, $8B4E123B, 
			$F7FAC91D, $56186D4B, $3166A326, $B297E3EA, $74FA6E3A, $32435BDD, $F7E74168, $FB2078CA, 
			$4EF50AFB, $97B3FED8, $AC564045, $279548BA, $3A3A5355, $878D8320, $B7A96BFE, $4B9596D0, 
			$BC67A855, $589A15A1, $6329A9CC, $33DBE199, $564A2AA6, $F925313F, $1C7EF45E, $7C312990, 
			$02E8F8FD, $702F2704, $5C15BB80, $E32C2805, $4815C195, $226DC6E4, $3F13C148, $DC860FC7, 
			$EEC9F907, $0F1F0441, $A4794740, $176E885D, $EB515F32, $D1C09BD5, $8FC1BCF2, $64351141, 
			$34787B25, $609C2A60, $A3E8F8DF, $1B6C631F, $C2B4120E, $9E32E102, $D14F66AF, $1581D1CA, 
			$E095236B, $E1923E33, $620B243B, $22B9BEEE, $0EA2B285, $990DBAE6, $8C0C72DE, $28F7A22D, 
			$457812D0, $FD94B795, $62087D64, $F0F5CCE7, $6FA34954, $FA487D87, $27FD9DC3, $1E8D3EF3, 
			$4163470A, $74FF2E99, $AB6E6F3A, $37FDF8F4, $60DC12A8, $F8DDEBA1, $4CE11B99, $0D6B6EDB, 
			$10557BC6, $372C676D, $3BD46527, $04E8D0DC, $C70D29F1, $A3FF00CC, $920F39B5, $0BED0F69, 
			$FB9F7B66, $9C7DDBCE, $0BCF91A0, $A35E15D9, $882F13BB, $24AD5B51, $BF79947B, $EBD63B76, 
			$B32E3937, $795911CC, $97E22680, $2D312EF4, $A7AD4268, $3B2B6AC6, $CC4C7512, $1CF12E78, 
			$3742126A, $E75192B7, $E6BBA106, $5063FB4B, $18106B1A, $FAEDCA11, $D8BD253D, $C9C3E1E2, 
			$59164244, $8613120A, $6EEC0CD9, $2AEAABD5, $4E67AF64, $5FA886DA, $88E9BFBE, $FEC3E464, 
			$5780BC9D, $86C0F7F0, $F87B7860, $4D600360, $4683FDD1, $B01F38F6, $04AE4577, $CCFC36D7, 
			$336B4283, $71AB1EF0, $874180B0, $5F5E003C, $BE57A077, $24AEE8BD, $99424655, $612E58BF, 
			$8FF4584E, $A2FDDDF2, $38EF74F4, $C2BD8987, $C3F96653, $748EB3C8, $55F275B4, $B9D9FC46, 
			$6126EB7A, $84DF1D8B, $790E6A84, $E2955F91, $8E596E46, $7057B420, $9155D58C, $4CDE02C9,
			$E1AC0BB9, $D00582BB, $4862A811, $9EA97475, $B6197FB7, $09DCA9E0, $A1092D66, $334632C4, 
			$021F5AE8, $8CBEF009, $25A0994A, $10FE6E1D, $1D3DB91A, $DFA4A50B, $0FF286A1, $69F16828, 
			$83DAB7DC, $FE063957, $9BCEE2A1, $527FCD4F, $015E1150, $FA8306A7, $C4B502A0, $27D0E60D, 
			$278CF89A, $41863F77, $064C60C3, $B506A861, $287A17F0, $E086F5C0, $AA586000, $627DDC30, 
			$D79EE611, $63EA3823, $94DDC253, $3416C2C2, $56EECBBB, $DEB6BC90, $A17DFCEB, $761D59CE, 
			$09E4056F, $88017C4B, $3D0A7239, $247C927C, $5F72E386, $B99D4D72, $B45BC11A, $FCB89ED3, 
			$785554ED, $B5A5FC08, $D37C3DD8, $C40FAD4D, $5EEF501E, $F8E661B1, $D91485A2, $3C13516C, 
			$E7C7D56F, $C44EE156, $CEBF2A36, $37C8C6DD, $34329AD7, $12826392, $8EFA0E67, $E0006040
		 ),  // InitS[ 2,  ... ]
		( 
			$37CE393A, $CFF5FAD3, $3777C2AB, $1B2DC55A, $9E67B05C, $4237A34F, $402782D3, $BE9BBC99, 
			$9D8E11D5, $15730FBF, $7E1C2DD6, $7BC400C7, $6B1B8CB7, $4590A121, $BEB16EB2, $B46E366A, 
			$2FAB4857, $796E94BC, $D276A3C6, $C8C24965, $EEF80F53, $7DDE8D46, $1D0A73D5, $C64DD04C, 
			$DBBB3929, $5046BAA9, $E82695AC, $04E35EBE, $F0D5FAA1, $9A512D6A, $E28CEF63, $22EE869A,
			$B8C289C0, $F62E2443, $AA031EA5, $A4D0F29C, $BA61C083, $4D6AE99B, $5015E58F, $D65B64BA, 
			$F9A22628, $E13A3AA7, $8695A94B, $E96255EF, $D3EF2FC7, $DAF752F7, $696F043F, $590AFA77, 
			$15A9E480, $0186B087, $ADE6099B, $93E53E3B, $5AFD90E9, $97D7349E, $D9B7F02C, $518B2B02, 
			$3AACD596, $7DA67D01, $D63ECFD1, $282D7D7C, $CF259F1F, $9BB8F2AD, $72B4D65A, $4CF5885A, 
			$71AC29E0, $E6A519E0, $FDACB047, $9BFA93ED, $8DC4D3E8, $CC573B28, $2966D5F8, $282E1379, 
			$91015F78, $556075ED, $440E96F7, $8C5ED3E3, $D46D0515, $BA6DF488, $2561A103, $BDF06405, 
			$159EEBC3, $A257903C, $EC1A2797, $2A073AA9, $9B6D3F1B, $F521631E, $FB669CF5, $19F3DC26, 
			$28D93375, $F5FD55B1, $82345603, $BB3CBA8A, $11775128, $F8D90AC2, $6751CCAB, $5F92ADCC, 
			$5117E84D, $8EDC3038, $62589D37, $91F92093, $C2907AEA, $CE7B3EFB, $64CE2151, $32BE4F77, 
			$7EE3B6A8, $463D29C3, $6953DE48, $80E61364, $1008AEA2, $24B26DDD, $FD2D8569, $66210709, 
			$0A469AB3, $DDC04564, $CFDE6C58, $AEC8201C, $DDF7BE5B, $408D581B, $7F01D2CC, $BBE3B46B, 
			$7E6AA2DD, $45FF593A, $440A353E, $D5CDB4BC, $A8CEEA72, $BB8464FA, $AE12668D, $476F3CBF, 
			$63E49BD2, $9E5D2F54, $1B77C2AE, $70634EF6, $8D0D0E74, $57135BE7, $711672F8, $5D7D53AF, 
			$08CB4040, $CCE2B44E, $6A46D234, $84AF1501, $2804B0E1, $1D3A9895, $B49FB806, $48A06ECE, 
			$823B3F6F, $82AB2035, $4B1D1A01, $F8277227, $B1601561, $DC3F93E7, $2B793ABB, $BD254534, 
			$E13988A0, $4B79CE51, $B7C9322F, $C9BA1FA0, $7EC81CE0, $F6D1C7BC, $C31101CF, $C7AAE8A1, 
			$4987901A, $9ABD4FD4, $CBDEDAD0, $38DA0AD5, $2AC33903, $673691C6, $7C31F98D, $4F2BB1E0, 
			$B7599EF7, $3ABBF543, $FF19D5F2, $9C45D927, $2C2297BF, $2AFCE615, $71FC910F, $2515949B, 
			$6193E5FA, $EB9CB6CE, $5964A8C2, $D1A8BA12, $5E07C1B6, $0C6A05E3, $6550D210, $42A403CB, 
			$0E6EECE0, $3BDB9816, $BEA0984C, $64E97832, $32951F9F, $DF92D3E0, $2B34A0D3, $1EF27189, 
			$41740A1B, $8C34A34B, $2071BEC5, $D83276C3, $8D9F35DF, $2E2F999B, $476F0BE6, $1DF1E30F, 
			$54DA4CE5, $91D8DA1E, $CF7962CE, $6F7E3ECD, $66B11816, $051D2CFD, $C5D28F84, $9922FBF6, 
			$57F323F5, $237632A6, $3135A893, $02CDCC56, $6281F0AC, $B5EB755A, $9736166E, $CC73D288, 
			$926296DE, $D049B981, $1B90504C, $1456C671, $BDC7C6E6, $0A147A32, $06D0E145, $9A7BF2C3, 
			$FD53AAC9, $000FA862, $E2BF25BB, $F6D2BD35, $05691271, $220204B2, $7CCFCBB6, $2B9C76CD, 
			$C03E1153, $D3E34016, $60BDAB38, $F0AD4725, $9C2038BA, $76CE46F7, $C5A1AF77, $60607520, 
			$4EFECB85, $D88DE88A, $B0F9AA7A, $7EAAF94C, $5CC24819, $8C8AFB02, $E46AC301, $F9E1EBD6, 
			$69F8D490, $A0DE5CA6, $2D25093F, $9FE608C2, $32614EB7, $5BE277CE, $E3DF8F57, $E672C33A
		 ) // InitS[ 3, ... ]
	 ); // InitS[ ... ]


function NewWriteableBlowfishEncryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TBlowfishEncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, True);
end;

function NewWriteableBlowfishEncryptionFilter(const Key: TBlowfishKey): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TBlowfishEncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, True);
end;

function NewWriteableBlowfishDecryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TBlowfishEncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, False);
end;

function NewWriteableBlowfishDecryptionFilter(const Key: TBlowfishKey): IStreamFilter; overload;
begin
  Result := TlvkWriteableEncryptionFilter.Create(
    TBlowfishEncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, False);
end;

function NewReadableBlowfishEncryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TBlowfishEncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, True);
end;

function NewReadableBlowfishEncryptionFilter(const Key: TBlowfishKey): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TBlowfishEncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, True);
end;

function NewReadableBlowfishDecryptionFilter(const Passphrase: string): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TBlowfishEncryptionAlgorithm.Create(Passphrase) as IEncryptionAlgorithm, False);
end;

function NewReadableBlowfishDecryptionFilter(const Key: TBlowfishKey): IStreamFilter; overload;
begin
  Result := TlvkReadableEncryptionFilter.Create(
    TBlowfishEncryptionAlgorithm.Create(Key) as IEncryptionAlgorithm, False);
end;

{ TBlowfish }

procedure TBlowfishEncryptionAlgorithm.CalculateSubKeys(const Key: TBlowfishKey);
var
	i,j		: Integer;
	Temp	: LongWord;
	Block	: TBlowfishBlock;
begin
	P := InitP;
	S := InitS;

	for i := 0 to Rounds + 1 do
	begin
    Move(Key[i*4], Temp, 4);
		P[i] := P[i] xor Temp;
	end;

	Block[0] := 0;
	Block[1] := 0;

	i := 0;
	while i < Rounds + 1 do
	begin
		Cipher(Block, 0, 1);
		P[i] := Block[0];
		P[i+1] := Block[1];
		Inc(i, 2);
	end;

	for j := 0 to 3 do
	begin
		i := 0;
		while i < 256 do
		begin
			Cipher(Block, 0, 1);
			S[j, i] := Block[0];
			S[j, i+1] := Block[1];
			Inc(i, 2);
		end;
	end;
end;

procedure TBlowfishEncryptionAlgorithm.Cipher(var Block: TBlowfishBlock; StartN,
  DeltaN: Integer);
var
	I,N		: Integer;
	Xl,Xr	: LongWord;
	Temp	: LongWord;
	Bytes	: record
		d,c,b,a	: Byte;
	end absolute Xl;
	F			: LongWord;
begin
	// Get the data to encipher
	Xl := Block[0];
	Xr := Block[1];

	N := StartN;
	for i := 0 to Rounds - 1 do
	begin
		Xl := Xl xor P[N];
		Inc(N, DeltaN);
		F := ((S[0, Bytes.a]+(S[1, Bytes.b] mod 32)) xor S[2, Bytes.c]) + (S[3, Bytes.d] mod 32);
		Xr := Xr xor F;

		Temp := Xl;
		Xl := Xr;
		Xr := Temp;
	end;

	Temp := Xl;
	Xl := Xr;
	Xr := Temp;

	Xr := Xr xor P[N];
	Xl := Xl xor P[N + DeltaN];

	Block[0] := Xl;
	Block[1] := Xr;
end;

constructor TBlowfishEncryptionAlgorithm.Create(const Key: TBlowfishKey);
begin
  inherited Create;

  CalculateSubKeys(Key);
end;

constructor TBlowfishEncryptionAlgorithm.Create(const Passphrase: string);
var
  Key : TBlowfishKey;
begin
  HashPassphrase(Passphrase, Key);
  Create(Key);
end;

procedure TBlowfishEncryptionAlgorithm.DecryptBlock(var Block);
begin
	Cipher(TBlowfishBlock(Block), 0, 1);
end;

procedure TBlowfishEncryptionAlgorithm.EncryptBlock(var Block);
begin
	Cipher(TBlowfishBlock(Block), Rounds + 1, -1);
end;

function TBlowfishEncryptionAlgorithm.GetBlockSize: Integer;
begin
  Result := SizeOf(TBlowfishBlock);
end;

end.
