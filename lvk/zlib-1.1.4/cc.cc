Turbo Dump  Version 5.0.16.6 Copyright (c) 1988, 1999 Inprise Corporation
                    Display of File DEFLATE.OBJ

000000 THEADR  deflate.c
00000E COMENT  Purge: Yes, List: Yes, Class: 0   (000h)
    Translator: Borland C++ 5.5.1
000026 COMENT  Purge: Yes, List: Yes, Class: 232 (0E8h)
    Source File 1: deflate.c             03/11/2002  04:00:19 pm
00003B COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: C:\Program Files\Borland\BCPP551\include\errno.h  06/27/2000  05:01:00 am
000076 COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: C:\Program Files\Borland\BCPP551\include\search.h  06/27/2000  05:01:00 am
0000B2 COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: C:\Program Files\Borland\BCPP551\include\stdlib.h  06/27/2000  05:01:00 am
0000EE COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: C:\Program Files\Borland\BCPP551\include\mem.h  06/27/2000  05:01:00 am
000127 COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: C:\Program Files\Borland\BCPP551\include\_loc.h  06/27/2000  05:01:00 am
000161 COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: C:\Program Files\Borland\BCPP551\include\locale.h  06/27/2000  05:01:00 am
00019D COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: C:\Program Files\Borland\BCPP551\include\_str.h  06/27/2000  05:01:00 am
0001D7 COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: C:\Program Files\Borland\BCPP551\include\string.h  06/27/2000  05:01:00 am
000213 COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: C:\Program Files\Borland\BCPP551\include\_null.h  06/27/2000  05:01:00 am
00024E COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: C:\Program Files\Borland\BCPP551\include\_defs.h  06/27/2000  05:01:00 am
000289 COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: C:\Program Files\Borland\BCPP551\include\_stddef.h  06/27/2000  05:01:00 am
0002C6 COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: C:\Program Files\Borland\BCPP551\include\stddef.h  06/27/2000  05:01:00 am
000302 COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: zconf.h               03/11/2002  02:16:00 pm
000314 COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: zlib.h                03/11/2002  02:56:19 pm
000325 COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: zutil.h               03/11/2002  02:16:00 pm
000337 COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: deflate.h             03/11/2002  02:16:00 pm
00034B COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    Dependency File: deflate.c             03/11/2002  04:00:19 pm
00035F COMENT  Purge: Yes, List: Yes, Class: 233 (0E9h)
    End of Dependency List
000365 LNAMES
    Name  1: '_TEXT'
    Name  2: 'CODE'
    Name  3: ''
    Name  4: '_DATA'
    Name  5: 'DATA'
    Name  6: 'DGROUP'
    Name  7: '_BSS'
    Name  8: 'BSS'
000390 SEGDEF 1 : _TEXT           DWORD PUBLIC  USE32 Class 'CODE'	 Length: 1645
00039A SEGDEF 2 : _DATA           DWORD PUBLIC  USE32 Class 'DATA'	 Length: 00b8
0003A4 SEGDEF 3 : _BSS            DWORD PUBLIC  USE32 Class 'BSS'	 Length: 0000
0003AE GRPDEF Group: DGROUP
    Segment: _BSS           
    Segment: _DATA          
0003B7 EXTDEF 1 : 'Z_ERRMSG'            Type: 0  
        2 : '_LENGTH_CODE'        Type: 0  
        3 : '_DIST_CODE'          Type: 0  
        4 : 'ZCALLOC'             Type: 0  
        5 : 'ZCFREE'              Type: 0  
        6 : 'ADLER32'             Type: 0  
        7 : 'memcpy'              Type: 0  
        8 : '_TR_INIT'            Type: 0  
        9 : '_TR_ALIGN'           Type: 0  
        10: '_TR_STORED_BLOCK'    Type: 0  
        11: 'memset'              Type: 0  
        12: '_TR_FLUSH_BLOCK'     Type: 0  
000441 PUBDEF  'DEFLATE_COPYRIGHT'     Segment: _DATA:007C
00045C PUBDEF  'DEFLATEINIT_'          Segment: _TEXT:0000
000472 PUBDEF  'DEFLATEINIT2_'         Segment: _TEXT:0024
000489 PUBDEF  'DEFLATEEND'            Segment: _TEXT:07B4
00049D PUBDEF  'DEFLATERESET'          Segment: _TEXT:0334
0004B3 PUBDEF  'DEFLATESETDICTIONARY'    Segment: _TEXT:0234
0004D1 PUBDEF  'DEFLATEPARAMS'         Segment: _TEXT:03C0
0004E8 PUBDEF  'DEFLATE'               Segment: _TEXT:0524
0004F9 PUBDEF  'DEFLATECOPY'           Segment: _TEXT:085C
00050E LEDATA  Segment: _TEXT          Offset: 0000  Length: 0400
    0000: 55 8B EC 8B 45 14 50 8B  55 10 52 6A 08 6A 0F 6A   U...E.P.U.Rj.j.j
    0010: 08 6A 00 8B 4D 0C 51 8B  45 08 50 E8 04 00 00 00   .j..M.Q.E.P.....
    0020: 5D C2 10 00 55 8B EC 51  53 56 57 8B 45 0C 8B 7D   ]...U..QSVW.E..}
    0030: 18 8B 75 24 33 D2 89 55  FC 85 C0 74 16 8A 00 8B   ..u$3..U...t....
    0040: 0D 78 00 00 00 8A 11 3A  C2 75 08 8B 45 08 83 F8   .x.....:.u..E...
    0050: 38 74 0A B8 FA FF FF FF  E9 CE 01 00 00 85 F6 75   8t.............u
    0060: 0A B8 FE FF FF FF E9 C0  01 00 00 33 D2 89 56 18   ...........3..V.
    0070: 8B 4E 20 85 C9 75 0C C7  46 20 00 00 00 00 33 C0   .N ..u..F ....3.
    0080: 89 46 28 8B 56 24 85 D2  75 07 C7 46 24 00 00 00   .F(.V$..u..F$...
    0090: 00 8B 4D 20 41 75 07 C7  45 20 06 00 00 00 85 FF   ..M Au..E ......
    00A0: 7D 09 C7 45 FC 01 00 00  00 F7 DF 8B 45 14 48 7C   }..E........E.H|
    00B0: 38 8B 55 14 83 FA 09 7F  30 8B 4D 1C 83 F9 08 75   8.U.....0.M....u
    00C0: 28 83 FF 09 7C 23 83 FF  0F 7F 1E 8B 45 20 85 C0   (...|#......E ..
    00D0: 7C 17 8B 55 20 83 FA 09  7F 0F 8B 4D 10 85 C9 7C   |..U ......M...|
    00E0: 08 8B 45 10 83 F8 02 7E  0A B8 FE FF FF FF E9 38   ..E....~.......8
    00F0: 01 00 00 8B 56 28 52 6A  01 68 B8 16 00 00 FF 56   ....V(Rj.h.....V
    0100: 20 8B D8 85 DB 75 0A B8  FC FF FF FF E9 1A 01 00    ....u..........
    0110: 00 89 5E 1C 89 33 8B 55  FC 8B C7 89 53 18 8B C8   ..^..3.U....S...
    0120: BA 01 00 00 00 89 43 28  D3 E2 89 53 24 4A 89 53   ......C(...S$J.S
    0130: 2C BA 01 00 00 00 8B 45  14 83 C0 07 8B C8 89 43   ,......E.......C
    0140: 48 D3 E2 89 53 44 4A 89  53 4C B9 03 00 00 00 8B   H...SDJ.SL......
    0150: 43 48 33 D2 83 C0 03 48  F7 F1 89 43 50 8B 46 28   CH3....H...CP.F(
    0160: 50 8B 53 24 52 6A 02 FF  56 20 89 43 30 8B 4E 28   P.S$Rj..V .C0.N(
    0170: 51 8B 43 24 50 6A 02 FF  56 20 89 43 38 8B 56 28   Q.C$Pj..V .C8.V(
    0180: 52 8B 4B 44 51 6A 02 FF  56 20 89 43 3C 8B 4D 14   R.KDQj..V .C<.M.
    0190: 83 C1 06 B8 01 00 00 00  D3 E0 89 83 94 16 00 00   ................
    01A0: 8B 56 28 52 50 6A 04 FF  56 20 89 43 08 8B 93 94   .V(RPj..V .C....
    01B0: 16 00 00 C1 E2 02 89 53  0C 8B 4B 30 85 C9 74 15   .......S..K0..t.
    01C0: 8B 53 38 85 D2 74 0E 8B  4B 3C 85 C9 74 07 8B 53   .S8..t..K<..t..S
    01D0: 08 85 D2 75 15 A1 18 00  00 00 89 46 18 56 E8 D1   ...u.......F.V..
    01E0: 05 00 00 B8 FC FF FF FF  EB 41 8B 93 94 16 00 00   .........A......
    01F0: D1 EA 03 D2 03 C2 89 83  9C 16 00 00 8B 8B 94 16   ................
    0200: 00 00 8D 0C 49 8B 43 08  03 C8 89 8B 90 16 00 00   ....I.C.........
    0210: 8B 55 20 89 53 7C 8B 4D  10 89 8B 80 00 00 00 8A   .U .S|.M........
    0220: 45 1C 88 43 1D 56 E8 09  01 00 00 5F 5E 5B 59 5D   E..C.V....._^[Y]
    0230: C2 20 00 90 55 8B EC 51  53 56 57 8B 7D 10 33 C0   . ..U..QSVW.}.3.
    0240: 8B 75 08 89 45 FC 85 FF  74 19 8B 57 1C 85 D2 74   .u..E...t..W...t
    0250: 12 8B 4D 0C 85 C9 74 0B  8B 47 1C 8B 50 04 83 FA   ..M...t..G..P...
    0260: 2A 74 0A B8 FE FF FF FF  E9 BE 00 00 00 8B 5F 1C   *t............_.
    0270: 8B 47 30 50 8B 55 0C 52  8B 4D 08 51 E8 00 00 00   .G0P.U.R.M.Q....
    0280: 00 89 47 30 83 FE 03 73  07 33 C0 E9 9B 00 00 00   ..G0...s.3......
    0290: 8B 43 24 2D 06 01 00 00  3B F0 76 0A 8B F0 8B 45   .C$-....;.v....E
    02A0: 08 2B C6 01 45 0C 56 8B  55 0C 52 8B 4B 30 51 E8   .+..E.V.U.R.K0Q.
    02B0: 00 00 00 00 89 73 64 89  73 54 8B 43 30 83 C4 0C   .....sd.sT.C0...
    02C0: 0F B6 00 89 43 40 8B 53  30 8B 4B 50 D3 E0 0F B6   ....C@.S0.KP....
    02D0: 52 01 33 C2 8B 4B 4C 23  C1 89 43 40 33 C0 EB 3B   R.3..KL#..C@3..;
    02E0: 8B 4B 50 8B 53 40 D3 E2  8B 4B 30 0F B6 4C 01 02   .KP.S@...K0..L..
    02F0: 33 D1 8B 4B 4C 23 D1 89  53 40 8B 4B 3C 0F B7 14   3..KL#..S@.K<...
    0300: 51 89 55 FC 8B 4B 2C 23  C8 8B 7B 38 66 89 14 4F   Q.U..K,#..{8f..O
    0310: 8B 53 3C 8B 4B 40 66 89  04 4A 40 8B D6 83 EA 03   .S<.K@f..J@.....
    0320: 3B C2 76 BC 8B 45 FC 85  C0 33 C0 5F 5E 5B 59 5D   ;.v..E...3._^[Y]
    0330: C2 0C 00 90 55 8B EC 53  8B 45 08 85 C0 74 15 8B   ....U..S.E...t..
    0340: 50 1C 85 D2 74 0E 8B 48  20 85 C9 74 07 8B 50 24   P...t..H ..t..P$
    0350: 85 D2 75 07 B8 FE FF FF  FF EB 5E 33 C9 33 D2 89   ..u.......^3.3..
    0360: 48 14 89 48 08 89 50 18  33 D2 C7 40 2C 02 00 00   H..H..P.3..@,...
    0370: 00 8B 58 1C 89 53 14 8B  4B 08 89 4B 10 8B 53 18   ..X..S..K..K..S.
    0380: 85 D2 7D 05 33 C9 89 4B  18 8B 53 18 85 D2 74 07   ..}.3..K..S...t.
    0390: B9 71 00 00 00 EB 05 B9  2A 00 00 00 89 4B 04 C7   .q......*....K..
    03A0: 40 30 01 00 00 00 33 C0  89 43 20 53 E8 00 00 00   @0....3..C S....
    03B0: 00 53 E8 99 06 00 00 33  C0 5B 5D C2 04 00 90 90   .S.....3.[].....
    03C0: 55 8B EC 51 53 56 57 8B  7D 08 8B 5D 0C 8B 45 10   U..QSVW.}..]..E.
    03D0: 33 D2 85 C0 74 07 8B 48  1C 85 C9 75 0A B8 FE FF   3...t..H...u....
    03E0: FF FF E9 A9 00 00 00 8B  70 1C 83 FB FF 75 05 BB   ........p....u..
    03F0: 06 00 00 00 85 DB 7C 0E  83 FB 09 7F 09 85 FF 7C    ......|........|
000915 FIXU32
    FixUp: 041  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[2]
    FixUp: 07a  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: EI[4]
    FixUp: 08d  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: EI[5]
    FixUp: 1d6  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: EI[1]
    FixUp: 27d  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[6]
    FixUp: 2b0  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[7]
    FixUp: 3ad  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[8]
000935 LEDATA  Segment: _TEXT          Offset: 0400  Length: 0400
    0000: 05 83 FF 02 7E 0A B8 FE  FF FF FF E9 80 00 00 00   ....~...........
    0010: 8B 4E 7C 8D 0C 49 8B 0C  8D 08 00 00 00 89 4D FC   .N|..I........M.
    0020: 8D 0C 5B 8B 0C 8D 08 00  00 00 3B 4D FC 74 11 8B   ..[.......;M.t..
    0030: 48 08 85 C9 74 0A 50 6A  01 E8 E6 00 00 00 8B D0   H...t.Pj........
    0040: 8B 46 7C 3B D8 74 41 89  5E 7C 8D 04 5B 0F B7 0C   .F|;.tA.^|..[...
    0050: 85 02 00 00 00 89 4E 78  8D 04 5B 0F B7 0C 85 00   ......Nx..[.....
    0060: 00 00 00 89 8E 84 00 00  00 8D 04 5B 0F B7 0C 85   ...........[....
    0070: 04 00 00 00 89 8E 88 00  00 00 8D 04 5B 0F B7 0C   ............[...
    0080: 85 06 00 00 00 89 4E 74  89 BE 80 00 00 00 8B C2   ......Nt........
    0090: 5F 5E 5B 59 5D C2 0C 00  55 8B EC 53 8B 55 08 8B   _^[Y]...U..S.U..
    00A0: 45 0C 8B 48 14 FF 40 14  8B 58 08 8D 0C 0B 51 8B   E..H..@..X....Q.
    00B0: CA C1 E9 08 5B 88 0B 8B  48 14 FF 40 14 8B 40 08   ....[...H..@..@.
    00C0: 80 E2 FF 88 14 08 5B 5D  C2 08 00 90 55 8B EC 53   ......[]....U..S
    00D0: 56 8B 5D 08 8B 43 1C 8B  70 14 8B 53 10 3B F2 76   V.]..C..p..S.;.v
    00E0: 02 8B F2 85 F6 74 37 56  8B 40 10 50 8B 53 0C 52   .....t7V.@.P.S.R
    00F0: E8 00 00 00 00 83 C4 0C  8B C6 01 43 0C 8B 53 1C   ...........C..S.
    0100: 01 42 10 01 73 14 29 73  10 8B 43 1C 8B 50 14 2B   .B..s.)s..C..P.+
    0110: D6 89 50 14 85 D2 75 06  8B 48 08 89 48 10 5E 5B   ..P...u..H..H.^[
    0120: 5D C2 04 00 55 8B EC 51  53 56 57 8B 7D 08 8B 75   ]...U..QSVW.}..u
    0130: 0C 85 F6 74 10 8B 46 1C  85 C0 74 09 83 FF 04 7F   ...t..F...t.....
    0140: 04 85 FF 7D 0A B8 FE FF  FF FF E9 5D 02 00 00 8B   ...}.......]....
    0150: 5E 1C 8B 46 0C 85 C0 74  1C 8B 16 85 D2 75 07 8B   ^..F...t.....u..
    0160: 4E 04 85 C9 75 0F 8B 43  04 3D 9A 02 00 00 75 18   N...u..C.=....u.
    0170: 83 FF 04 74 13 8B 15 10  00 00 00 B8 FE FF FF FF   ...t............
    0180: 89 56 18 E9 24 02 00 00  8B 56 10 85 D2 75 13 8B   .V..$....V...u..
    0190: 0D 1C 00 00 00 B8 FB FF  FF FF 89 4E 18 E9 0A 02   ...........N....
    01A0: 00 00 89 33 8B 53 20 89  55 FC 89 7B 20 8B 4B 04   ...3.S .U..{ .K.
    01B0: 83 F9 2A 75 7C 8B 4B 28  8B 43 7C 83 E9 08 48 C1   ..*u|.K(.C|...H.
    01C0: E1 04 83 C1 08 C1 E1 08  D1 F8 83 F8 03 76 05 B8   .............v..
    01D0: 03 00 00 00 C1 E0 06 0B  C8 8B 53 64 85 D2 74 03   ..........Sd..t.
    01E0: 83 C9 20 8B C1 51 33 D2  B9 1F 00 00 00 F7 F1 59   .. ..Q3........Y
    01F0: B8 1F 00 00 00 C7 43 04  71 00 00 00 2B C2 53 03   ......C.q...+.S.
    0200: C8 51 E8 91 FE FF FF 8B  53 64 85 D2 74 1C 53 8B   .Q......Sd..t.S.
    0210: 4E 30 C1 E9 10 51 E8 7D  FE FF FF 53 8B 46 30 25   N0...Q.}...S.F0%
    0220: FF FF 00 00 50 E8 6E FE  FF FF C7 46 30 01 00 00   ....P.n....F0...
    0230: 00 8B 53 14 85 D2 74 1B  56 E8 8E FE FF FF 8B 4E   ..S...t.V......N
    0240: 10 85 C9 75 33 C7 43 20  FF FF FF FF 33 C0 E9 59   ...u3.C ....3..Y
    0250: 01 00 00 8B 56 04 85 D2  75 1E 8B 4D FC 3B F9 7F   ....V...u..M.;..
    0260: 17 83 FF 04 74 12 A1 1C  00 00 00 89 46 18 B8 FB   ....t.......F...
    0270: FF FF FF E9 34 01 00 00  8B 53 04 81 FA 9A 02 00   ....4....S......
    0280: 00 75 19 8B 4E 04 85 C9  74 12 A1 1C 00 00 00 89   .u..N...t.......
    0290: 46 18 B8 FB FF FF FF E9  10 01 00 00 8B 56 04 85   F............V..
    02A0: D2 75 1D 8B 4B 6C 85 C9  75 16 85 FF 0F 84 A9 00   .u..Kl..u.......
    02B0: 00 00 8B 43 04 3D 9A 02  00 00 0F 84 9B 00 00 00   ...C.=..........
    02C0: 53 57 8B 53 7C 8D 14 52  8B 0C 95 08 00 00 00 FF   SW.S|..R........
    02D0: D1 83 F8 02 74 05 83 F8  03 75 07 C7 43 04 9A 02   ....t....u..C...
    02E0: 00 00 85 C0 74 05 83 F8  02 75 15 8B 46 10 85 C0   ....t....u..F...
    02F0: 75 07 C7 43 20 FF FF FF  FF 33 C0 E9 AC 00 00 00   u..C ....3......
    0300: 48 75 58 83 FF 01 75 08  53 E8 00 00 00 00 EB 33   HuX...u.S......3
    0310: 53 6A 00 6A 00 6A 00 E8  00 00 00 00 83 FF 03 75   Sj.j.j.........u
    0320: 22 8B 53 3C 8B 4B 44 66  C7 44 4A FE 00 00 8B 43   ".S<.KDf.DJ....C
    0330: 44 48 03 C0 50 6A 00 8B  53 3C 52 E8 00 00 00 00   DH..Pj..S<R.....
    0340: 83 C4 0C 56 E8 83 FD FF  FF 8B 4E 10 85 C9 75 0B   ...V......N...u.
    0350: C7 43 20 FF FF FF FF 33  C0 EB 51 83 FF 04 74 04   .C ....3..Q...t.
    0360: 33 C0 EB 48 8B 53 18 85  D2 74 07 B8 01 00 00 00   3..H.S...t......
    0370: EB 3A 53 8B 56 30 C1 EA  10 52 E8 19 FD FF FF 53   .:S.V0...R.....S
    0380: 8B 4E 30 81 E1 FF FF 00  00 51 E8 09 FD FF FF 56   .N0......Q.....V
    0390: E8 37 FD FF FF C7 43 18  FF FF FF FF 8B 43 14 85   .7....C......C..
    03A0: C0 74 04 33 C0 EB 05 B8  01 00 00 00 5F 5E 5B 59   .t.3........_^[Y
    03B0: 5D C2 08 00 55 8B EC 53  56 8B 5D 08 85 DB 74 07   ]...U..SV.]...t.
    03C0: 8B 43 1C 85 C0 75 0A B8  FE FF FF FF E9 85 00 00   .C...u..........
    03D0: 00 8B 53 1C 8B 72 04 83  FE 2A 74 14 83 FE 71 74   ..S..r...*t...qt
    03E0: 0F 81 FE 9A 02 00 00 74  07 B8 FE FF FF FF EB 66   .......t.......f
    03F0: 8B 53 1C 8B 42 08 85 C0  74 08 8B 53 28 52 50 FF    .S..B...t..S(RP.
000D3C FIXU32
    FixUp: 019  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[2]
    FixUp: 026  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[2]
    FixUp: 051  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[2]
    FixUp: 05f  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[2]
    FixUp: 070  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[2]
    FixUp: 081  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[2]
    FixUp: 0f1  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[7]
    FixUp: 177  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: EI[1]
    FixUp: 191  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: EI[1]
    FixUp: 267  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: EI[1]
    FixUp: 28b  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: EI[1]
    FixUp: 2cb  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[2]
    FixUp: 30a  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[9]
    FixUp: 318  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[10]
    FixUp: 33c  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[11]
000D7C LEDATA  Segment: _TEXT          Offset: 0800  Length: 0400
    0000: 53 24 8B 4B 1C 8B 41 3C  85 C0 74 08 8B 53 28 52   S$.K..A<..t..S(R
    0010: 50 FF 53 24 8B 4B 1C 8B  41 38 85 C0 74 08 8B 53   P.S$.K..A8..t..S
    0020: 28 52 50 FF 53 24 8B 4B  1C 8B 41 30 85 C0 74 08   (RP.S$.K..A0..t.
    0030: 8B 53 28 52 50 FF 53 24  8B 4B 28 51 8B 43 1C 50   .S(RP.S$.K(Q.C.P
    0040: FF 53 24 33 D2 83 FE 71  89 53 1C 75 07 B8 FD FF   .S$3...q.S.u....
    0050: FF FF EB 02 33 C0 5E 5B  5D C2 04 00 55 8B EC 51   ....3.^[]...U..Q
    0060: 53 56 57 8B 45 08 8B 75  0C 85 C0 74 0B 85 F6 74   SVW.E..u...t...t
    0070: 07 8B 50 1C 85 D2 75 0A  B8 FE FF FF FF E9 6E 01   ..P...u.......n.
    0080: 00 00 8B 78 1C 56 57 8B  FE 8B F0 B9 0E 00 00 00   ...x.VW.........
    0090: F3 A5 5F 5E 8B 46 28 50  6A 01 68 B8 16 00 00 FF   .._^.F(Pj.h.....
    00A0: 56 20 8B D8 85 DB 75 0A  B8 FC FF FF FF E9 3E 01   V ....u.......>.
    00B0: 00 00 89 5E 1C 56 57 8B  F7 8B FB B9 AE 05 00 00   ...^.VW.........
    00C0: F3 A5 5F 5E 89 33 8B 46  28 50 8B 53 24 52 6A 02   .._^.3.F(P.S$Rj.
    00D0: FF 56 20 89 43 30 8B 4E  28 51 8B 43 24 50 6A 02   .V .C0.N(Q.C$Pj.
    00E0: FF 56 20 89 43 38 8B 56  28 52 8B 4B 44 51 6A 02   .V .C8.V(R.KDQj.
    00F0: FF 56 20 89 43 3C 8B 46  28 50 8B 93 94 16 00 00   .V .C<.F(P......
    0100: 52 6A 04 FF 56 20 89 45  FC 8B 4D FC 89 4B 08 8B   Rj..V .E..M..K..
    0110: 43 30 85 C0 74 15 8B 53  38 85 D2 74 0E 8B 4B 3C   C0..t..S8..t..K<
    0120: 85 C9 74 07 8B 43 08 85  C0 75 10 56 E8 83 FE FF   ..t..C...u.V....
    0130: FF B8 FC FF FF FF E9 B5  00 00 00 8B 53 24 03 D2   ............S$..
    0140: 52 8B 4F 30 51 8B 43 30  50 E8 00 00 00 00 83 C4   R.O0Q.C0P.......
    0150: 0C 8B 53 24 03 D2 52 8B  4F 38 51 8B 43 38 50 E8   ..S$..R.O8Q.C8P.
    0160: 00 00 00 00 83 C4 0C 8B  53 44 03 D2 52 8B 4F 3C   ........SD..R.O<
    0170: 51 8B 43 3C 50 E8 00 00  00 00 83 C4 0C 8B 53 0C   Q.C<P.........S.
    0180: 52 8B 4F 08 51 8B 43 08  50 E8 00 00 00 00 8B 57   R.O.Q.C.P......W
    0190: 10 8B 4F 08 2B D1 8B 43  08 03 D0 83 C4 0C 89 53   ..O.+..C.......S
    01A0: 10 8B 93 94 16 00 00 8B  4D FC D1 EA 03 D2 03 D1   ........M.......
    01B0: 8D 8B 8C 00 00 00 89 93  9C 16 00 00 8B 83 94 16   ................
    01C0: 00 00 8B 53 08 8D 04 40  03 C2 8D 93 74 0A 00 00   ...S...@....t...
    01D0: 89 83 90 16 00 00 8D 83  80 09 00 00 89 8B 10 0B   ................
    01E0: 00 00 89 83 1C 0B 00 00  33 C0 89 93 28 0B 00 00   ........3...(...
    01F0: 5F 5E 5B 59 5D C2 08 00  55 8B EC 53 56 8B 45 08   _^[Y]...U..SV.E.
    0200: 8B 75 10 8B 5E 04 3B C3  73 02 8B D8 85 DB 75 04   .u..^.;.s.....u.
    0210: 33 C0 EB 34 29 5E 04 8B  56 1C 8B 4A 18 85 C9 75   3..4)^..V..J...u
    0220: 10 8B 46 30 50 8B 16 52  53 E8 00 00 00 00 89 46   ..F0P..RS......F
    0230: 30 53 8B 0E 51 8B 45 0C  50 E8 00 00 00 00 83 C4   0S..Q.E.P.......
    0240: 0C 8B C3 01 06 01 5E 08  5E 5B 5D C2 0C 00 90 90   ......^.^[].....
    0250: 55 8B EC 53 8B 5D 08 8B  43 24 03 C0 89 43 34 8B   U..S.]..C$...C4.
    0260: 53 3C 8B 4B 44 66 C7 44  4A FE 00 00 8B 43 44 48   S<.KDf.DJ....CDH
    0270: 03 C0 50 6A 00 8B 53 3C  52 E8 00 00 00 00 8B 4B   ..Pj..S<R......K
    0280: 7C 83 C4 0C 8D 0C 49 0F  B7 04 8D 02 00 00 00 89   |.....I.........
    0290: 43 78 8B 53 7C 8D 14 52  0F B7 0C 95 00 00 00 00   Cx.S|..R........
    02A0: 89 8B 84 00 00 00 8B 43  7C 8D 04 40 0F B7 14 85   .......C|..@....
    02B0: 04 00 00 00 89 93 88 00  00 00 33 D2 8B 4B 7C 8D   ..........3..K|.
    02C0: 0C 49 0F B7 04 8D 06 00  00 00 89 43 74 89 53 64   .I.........Ct.Sd
    02D0: 33 C9 33 C0 89 4B 54 89  43 6C BA 02 00 00 00 33   3.3..KT.Cl.....3
    02E0: C0 89 53 70 89 53 58 89  43 60 33 C9 89 4B 40 5B   ..Sp.SX.C`3..K@[
    02F0: 5D C2 04 00 55 8B EC 83  C4 E4 53 56 8B 45 0C 8B   ]...U.....SV.E..
    0300: 50 74 89 55 FC 8B 48 64  8B 50 30 8B 70 70 8B 98   Pt.U..Hd.P0.pp..
    0310: 88 00 00 00 89 5D F8 03  D1 8B 58 24 81 EB 06 01   .....]....X$....
    0320: 00 00 3B CB 76 10 8B 48  24 81 E9 06 01 00 00 8B   ..;.v..H$.......
    0330: 58 64 2B D9 EB 02 33 DB  89 5D F4 8B 48 38 89 4D   Xd+...3..]..H8.M
    0340: F0 8B 48 2C 89 4D EC 8B  48 30 8B 58 64 03 CB 81   ..H,.M..H0.Xd...
    0350: C1 02 01 00 00 89 4D E8  8A 4C 32 FF 88 4D E7 8A   ......M..L2..M..
    0360: 0C 32 88 4D E6 8B 48 70  8B 98 84 00 00 00 3B CB   .2.M..Hp......;.
    0370: 72 04 C1 6D FC 02 8B 48  6C 8B 5D F8 3B CB 73 03   r..m...Hl.].;.s.
    0380: 89 4D F8 8B 48 30 8B 5D  08 03 CB 8A 1C 31 3A 5D   .M..H0.].....1:]
    0390: E6 0F 85 A5 00 00 00 8A  5C 31 FF 3A 5D E7 0F 85   ........\1.:]...
    03A0: 98 00 00 00 8A 19 3A 1A  0F 85 8E 00 00 00 41 8A   ......:.......A.
    03B0: 19 3A 5A 01 0F 85 82 00  00 00 83 C2 02 41 42 41   .:Z..........ABA
    03C0: 8A 1A 3A 19 75 3F 42 41  8A 1A 3A 19 75 37 42 41   ..:.u?BA..:.u7BA
    03D0: 8A 1A 3A 19 75 2F 42 41  8A 1A 3A 19 75 27 42 41   ..:.u/BA..:.u'BA
    03E0: 8A 1A 3A 19 75 1F 42 41  8A 1A 3A 19 75 17 42 41   ..:.u.BA..:.u.BA
    03F0: 8A 1A 3A 19 75 0F 42 41  8A 1A 3A 19 75 07 8B 5D    ..:.u.BA..:.u..]
001183 FIXU32
    FixUp: 14a  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[7]
    FixUp: 160  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[7]
    FixUp: 176  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[7]
    FixUp: 18a  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[7]
    FixUp: 22a  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[6]
    FixUp: 23a  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[7]
    FixUp: 27a  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[11]
    FixUp: 28b  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[2]
    FixUp: 29c  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[2]
    FixUp: 2b0  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[2]
    FixUp: 2c6  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[2]
0011B3 LEDATA  Segment: _TEXT          Offset: 0C00  Length: 0400
    0000: E8 3B D3 72 B9 8B 4D E8  2B CA 51 B9 02 01 00 00   .;.r..M.+.Q.....
    0010: 5A 2B CA 8B 55 E8 81 C2  FE FE FF FF 3B F1 7D 1C   Z+..U.......;.}.
    0020: 8B 5D 08 89 58 68 8B F1  8B 5D F8 3B CB 7D 2F 8A   .]..Xh...].;.}/.
    0030: 4C 32 FF 88 4D E7 8A 0C  32 88 4D E6 8B 4D 08 8B   L2..M...2.M..M..
    0040: 5D EC 23 CB 8B 5D F0 0F  B7 0C 4B 89 4D 08 8B 5D   ].#..]....K.M..]
    0050: F4 3B CB 76 09 FF 4D FC  0F 85 25 FF FF FF 8B 50   .;.v..M...%....P
    0060: 6C 8B CE 3B D1 72 04 8B  C1 EB 02 8B C2 5E 5B 8B   l..;.r.......^[.
    0070: E5 5D C2 08 00 90 90 90  55 8B EC 83 C4 F8 53 56   .]......U.....SV
    0080: 57 8B 5D 08 8B 73 24 8B  43 34 8B 53 6C 2B C2 8B   W.]..s$.C4.Sl+..
    0090: 4B 64 2B C1 89 45 F8 8B  45 F8 85 C0 75 16 8B 53   Kd+..E..E...u..S
    00A0: 64 85 D2 75 0F 8B 4B 6C  85 C9 75 08 89 75 F8 E9   d..u..Kl..u..u..
    00B0: 9B 00 00 00 8B 45 F8 83  F8 FF 75 08 FF 4D F8 E9   .....E....u..M..
    00C0: 8B 00 00 00 8B 53 24 81  EA 06 01 00 00 03 D6 8B   .....S$.........
    00D0: 4B 64 3B D1 77 79 56 8B  43 30 8B D0 8B FE 03 D7   Kd;.wyV.C0......
    00E0: 52 50 E8 00 00 00 00 83  C4 0C 29 73 68 29 73 64   RP........)sh)sd
    00F0: 29 7B 54 8B 43 44 8B D0  03 D2 8B 4B 3C 03 D1 83   ){T.CD.....K<...
    0100: EA 02 0F B7 0A 89 4D FC  8B 4D FC 3B F1 77 09 66   ......M..M.;.w.f
    0110: 8B 4D FC 66 2B CE EB 02  33 C9 66 89 0A 48 75 DF   .M.f+...3.f..Hu.
    0120: 8B C6 8B D0 03 D2 8B 4B  38 03 D1 83 EA 02 0F B7   .......K8.......
    0130: 0A 89 4D FC 8B 4D FC 3B  F1 77 09 66 8B 4D FC 66   ..M..M.;.w.f.M.f
    0140: 2B CE EB 02 33 C9 66 89  0A 48 75 DF 01 75 F8 8B   +...3.f..Hu..u..
    0150: 3B 8B 47 04 85 C0 74 61  57 8B 53 30 8B 4B 64 03   ;.G...taW.S0.Kd.
    0160: D1 8B 43 6C 03 D0 52 8B  55 F8 52 E8 88 FC FF FF   ..Cl..R.U.R.....
    0170: 01 43 6C 8B 4B 6C 83 F9  03 72 27 8B 43 30 8B 53   .Cl.Kl...r'.C0.S
    0180: 64 0F B6 04 10 89 43 40  8B 4B 50 D3 E0 8B 53 30   d.....C@.KP...S0
    0190: 8B 4B 64 0F B6 54 0A 01  33 C2 8B 53 4C 23 C2 89   .Kd..T..3..SL#..
    01A0: 43 40 8B 43 6C 3D 06 01  00 00 73 0D 8B 0B 8B 41   C@.Cl=....s....A
    01B0: 04 85 C0 0F 85 CE FE FF  FF 5F 5E 5B 59 59 5D C2   ........._^[YY].
    01C0: 04 00 90 90 55 8B EC 53  56 8B 5D 0C BE FF FF 00   ....U..SV.].....
    01D0: 00 8B 43 0C 83 E8 05 3B  F0 76 02 8B F0 8B 43 6C   ..C....;.v....Cl
    01E0: 83 F8 01 77 23 53 E8 8D  FE FF FF 8B 43 6C 85 C0   ...w#S......Cl..
    01F0: 75 0E 8B 55 08 85 D2 75  07 33 C0 E9 36 01 00 00   u..U...u.3..6...
    0200: 85 C0 0F 84 C0 00 00 00  8B 53 6C 33 C9 01 53 64   .........Sl3..Sd
    0210: 89 4B 6C 8B 43 54 8B 53  64 03 C6 85 D2 74 04 3B   .Kl.CT.Sd....t.;
    0220: C2 77 4B 2B D0 89 53 6C  89 43 64 53 8B 4B 54 85   .wK+..Sl.CdS.KT.
    0230: C9 7C 0A 8B 43 30 8B 53  54 03 C2 EB 02 33 C0 50   .|..C0.ST....3.P
    0240: 8B 53 64 8B 4B 54 2B D1  52 6A 00 E8 00 00 00 00   .Sd.KT+.Rj......
    0250: 8B 43 64 89 43 54 8B 13  52 E8 6E F6 FF FF 8B 0B   .Cd.CT..R.n.....
    0260: 8B 41 10 85 C0 75 07 33  C0 E9 C8 00 00 00 8B 53   .A...u.3.......S
    0270: 64 8B 43 54 2B D0 8B 4B  24 81 E9 06 01 00 00 3B   d.CT+..K$......;
    0280: D1 0F 82 56 FF FF FF 53  85 C0 7C 0A 8B 53 30 8B   ...V...S..|..S0.
    0290: 43 54 03 D0 EB 02 33 D2  52 8B 4B 64 8B 43 54 2B   CT....3.R.Kd.CT+
    02A0: C8 51 6A 00 E8 00 00 00  00 8B 53 64 89 53 54 8B   .Qj.......Sd.ST.
    02B0: 0B 51 E8 15 F6 FF FF 8B  03 8B 50 10 85 D2 0F 85   .Q........P.....
    02C0: 19 FF FF FF 33 C0 EB 6E  53 8B 53 54 85 D2 7C 0A   ....3..nS.ST..|.
    02D0: 8B 4B 30 8B 43 54 03 C8  EB 02 33 C9 51 8B 53 64   .K0.CT....3.Q.Sd
    02E0: 8B 43 54 2B D0 52 8B 55  08 83 FA 04 0F 94 C1 83   .CT+.R.U........
    02F0: E1 01 51 E8 00 00 00 00  8B 43 64 89 43 54 8B 13   ..Q......Cd.CT..
    0300: 52 E8 C6 F5 FF FF 8B 0B  8B 41 10 85 C0 75 13 8B   R........A...u..
    0310: 55 08 83 FA 04 75 07 B8  02 00 00 00 EB 18 33 C0   U....u........3.
    0320: EB 14 8B 55 08 83 FA 04  75 07 B8 03 00 00 00 EB   ...U....u.......
    0330: 05 B8 01 00 00 00 5E 5B  5D C2 08 00 55 8B EC 53   ......^[]...U..S
    0340: 56 57 33 F6 8B 5D 0C 8B  43 6C 3D 06 01 00 00 73   VW3..]..Cl=....s
    0350: 26 53 E8 21 FD FF FF 8B  43 6C 3D 06 01 00 00 73   &S.!....Cl=....s
    0360: 0E 8B 55 08 85 D2 75 07  33 C0 E9 C8 02 00 00 85   ..U...u.3.......
    0370: C0 0F 84 52 02 00 00 8B  53 6C 83 FA 03 72 41 8B   ...R....Sl...rA.
    0380: 4B 50 8B 43 40 D3 E0 8B  53 30 8B 4B 64 0F B6 54   KP.C@...S0.Kd..T
    0390: 0A 02 33 C2 8B 4B 4C 23  C1 89 43 40 8B 53 3C 0F   ..3..KL#..C@.S<.
    03A0: B7 34 42 8B 43 64 8B 53  2C 23 C2 8B 4B 38 66 89   .4B.Cd.S,#..K8f.
    03B0: 34 41 8B 43 3C 8B 53 40  66 8B 4B 64 66 89 0C 50   4A.C<.S@f.Kdf..P
    03C0: 85 F6 74 27 8B 43 64 8B  53 24 81 EA 06 01 00 00   ..t'.Cd.S$......
    03D0: 2B C6 3B C2 77 15 8B 8B  80 00 00 00 83 F9 02 74   +.;.w..........t
    03E0: 0A 53 56 E8 0C FB FF FF  89 43 58 8B 43 58 83 F8   .SV......CX.CX..
    03F0: 03 0F 82 2E 01 00 00 66  8B 43 64 8B 8B 9C 16 00    .......f.Cd.....
0015BA FIXU32
    FixUp: 0e3  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[7]
    FixUp: 24c  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[12]
    FixUp: 2a5  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[12]
    FixUp: 2f4  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[12]
0015CE LEDATA  Segment: _TEXT          Offset: 1000  Length: 0400
    0000: 00 66 2B 43 68 8B BB 98  16 00 00 8A 53 58 66 89   .f+Ch.......SXf.
    0010: 04 79 80 EA 03 8B 8B 98  16 00 00 48 FF 83 98 16   .y.........H....
    0020: 00 00 8B BB 90 16 00 00  88 14 0F 81 E2 FF 00 00   ................
    0030: 00 33 C9 8A 8A 00 00 00  00 66 FF 84 8B 90 04 00   .3.......f......
    0040: 00 66 3D 00 01 73 0B 0F  B7 D0 8A 8A 00 00 00 00   .f=..s..........
    0050: EB 0C 0F B7 C0 C1 F8 07  8A 88 00 01 00 00 33 C0   ..............3.
    0060: 8A C1 66 FF 84 83 80 09  00 00 8B 93 94 16 00 00   ..f.............
    0070: 4A 8B 83 98 16 00 00 3B  D0 0F 94 C0 83 E0 01 8B   J......;........
    0080: 53 58 29 53 6C 8B 4B 58  8B 53 78 3B CA 77 5F 8B   SX)Sl.KX.Sx;.w_.
    0090: 4B 6C 83 F9 03 72 57 FF  4B 58 FF 43 64 8B 4B 50   Kl...rW.KX.Cd.KP
    00A0: 8B 53 40 D3 E2 8B 4B 30  8B 73 64 0F B6 4C 31 02   .S@...K0.sd..L1.
    00B0: 33 D1 8B 4B 4C 23 D1 89  53 40 8B 53 3C 8B 4B 40   3..KL#..S@.S<.K@
    00C0: 0F B7 34 4A 8B 53 64 8B  4B 2C 23 D1 8B 4B 38 66   ..4J.Sd.K,#..K8f
    00D0: 89 34 51 8B 53 3C 8B 4B  40 66 8B 7B 64 66 89 3C   .4Q.S<.K@f.{df.<
    00E0: 4A FF 4B 58 75 B4 FF 43  64 E9 8F 00 00 00 8B 53   J.KXu..Cd......S
    00F0: 58 33 C9 01 53 64 89 4B  58 8B 4B 64 8B 53 30 0F   X3..Sd.KX.Kd.S0.
    0100: B6 14 0A 89 53 40 8B 53  40 8B 4B 50 D3 E2 8B 4B   ....S@.S@.KP...K
    0110: 30 8B 7B 64 0F B6 4C 39  01 33 D1 8B 4B 4C 23 D1   0.{d..L9.3..KL#.
    0120: 89 53 40 EB 58 8B 43 30  8B 53 64 8A 04 10 8B 93   .S@.X.C0.Sd.....
    0130: 9C 16 00 00 8B 8B 98 16  00 00 66 C7 04 4A 00 00   ..........f..J..
    0140: 8B 93 98 16 00 00 FF 83  98 16 00 00 8B 8B 90 16   ................
    0150: 00 00 88 04 11 25 FF 00  00 00 66 FF 84 83 8C 00   .....%....f.....
    0160: 00 00 8B 83 94 16 00 00  48 8B 93 98 16 00 00 3B   ........H......;
    0170: C2 0F 94 C0 83 E0 01 FF  4B 6C FF 43 64 85 C0 0F   ........Kl.Cd...
    0180: 84 C2 FD FF FF 53 8B 53  54 85 D2 7C 0A 8B 4B 30   .....S.ST..|..K0
    0190: 8B 43 54 03 C8 EB 02 33  C9 51 8B 53 64 8B 43 54   .CT....3.Q.Sd.CT
    01A0: 2B D0 52 6A 00 E8 00 00  00 00 8B 53 64 89 53 54   +.Rj.......Sd.ST
    01B0: 8B 0B 51 E8 14 F3 FF FF  8B 03 8B 50 10 85 D2 0F   ..Q........P....
    01C0: 85 82 FD FF FF 33 C0 EB  6E 53 8B 53 54 85 D2 7C   .....3..nS.ST..|
    01D0: 0A 8B 4B 30 8B 43 54 03  C8 EB 02 33 C9 51 8B 53   ..K0.CT....3.Q.S
    01E0: 64 8B 43 54 2B D0 52 8B  55 08 83 FA 04 0F 94 C1   d.CT+.R.U.......
    01F0: 83 E1 01 51 E8 00 00 00  00 8B 43 64 89 43 54 8B   ...Q......Cd.CT.
    0200: 13 52 E8 C5 F2 FF FF 8B  0B 8B 41 10 85 C0 75 13   .R........A...u.
    0210: 8B 55 08 83 FA 04 75 07  B8 02 00 00 00 EB 18 33   .U....u........3
    0220: C0 EB 14 8B 55 08 83 FA  04 75 07 B8 03 00 00 00   ....U....u......
    0230: EB 05 B8 01 00 00 00 5F  5E 5B 5D C2 08 00 90 90   ......._^[].....
    0240: 55 8B EC 51 53 56 57 8B  5D 0C 33 F6 8B 43 6C 3D   U..QSVW.].3..Cl=
    0250: 06 01 00 00 73 26 53 E8  1C FA FF FF 8B 43 6C 3D   ....s&S......Cl=
    0260: 06 01 00 00 73 0E 8B 55  08 85 D2 75 07 33 C0 E9   ....s..U...u.3..
    0270: C9 03 00 00 85 C0 0F 84  F4 02 00 00 8B 53 6C 83   .............Sl.
    0280: FA 03 72 41 8B 4B 50 8B  43 40 D3 E0 8B 53 30 8B   ..rA.KP.C@...S0.
    0290: 4B 64 0F B6 54 0A 02 33  C2 8B 4B 4C 23 C1 89 43   Kd..T..3..KL#..C
    02A0: 40 8B 53 3C 0F B7 34 42  8B 43 64 8B 53 2C 23 C2   @.S<..4B.Cd.S,#.
    02B0: 8B 4B 38 66 89 34 41 8B  43 3C 8B 53 40 66 8B 4B   .K8f.4A.C<.S@f.K
    02C0: 64 66 89 0C 50 8B 43 58  89 43 70 8B 53 68 89 53   df..P.CX.Cp.Sh.S
    02D0: 5C C7 43 58 02 00 00 00  85 F6 74 61 8B 4B 70 8B   \.CX......ta.Kp.
    02E0: 43 78 3B C8 73 57 8B 53  64 8B 4B 24 81 E9 06 01   Cx;.sW.Sd.K$....
    02F0: 00 00 2B D6 3B D1 77 45  8B 83 80 00 00 00 83 F8   ..+.;.wE........
    0300: 02 74 0A 53 56 E8 EA F7  FF FF 89 43 58 8B 53 58   .t.SV......CX.SX
    0310: 83 FA 05 77 28 8B 8B 80  00 00 00 49 74 18 8B 43   ...w(......It..C
    0320: 58 83 F8 03 75 17 8B 53  64 8B 4B 68 2B D1 81 FA   X...u..Sd.Kh+...
    0330: 00 10 00 00 76 07 C7 43  58 02 00 00 00 8B 43 70   ....v..CX.....Cp
    0340: 83 F8 03 0F 82 66 01 00  00 8B 53 58 3B C2 0F 82   .....f....SX;...
    0350: 5B 01 00 00 8B 4B 64 8B  43 6C 03 C8 83 E9 03 89   [....Kd.Cl......
    0360: 4D FC 66 8B 43 64 8B 8B  9C 16 00 00 48 8B BB 98   M.f.Cd......H...
    0370: 16 00 00 66 2B 43 5C 8A  53 70 66 89 04 79 80 EA   ...f+C\.Spf..y..
    0380: 03 8B 8B 98 16 00 00 48  FF 83 98 16 00 00 8B BB   .......H........
    0390: 90 16 00 00 88 14 0F 81  E2 FF 00 00 00 33 C9 8A   .............3..
    03A0: 8A 00 00 00 00 66 FF 84  8B 90 04 00 00 66 3D 00   .....f.......f=.
    03B0: 01 73 0B 0F B7 D0 8A 8A  00 00 00 00 EB 0C 0F B7   .s..............
    03C0: C0 C1 F8 07 8A 88 00 01  00 00 33 C0 8A C1 66 FF   ..........3...f.
    03D0: 84 83 80 09 00 00 8B 93  94 16 00 00 4A 8B 83 98   ............J...
    03E0: 16 00 00 3B D0 0F 94 C0  83 E0 01 8B 53 70 4A 29   ...;........SpJ)
    03F0: 53 6C 83 6B 70 02 FF 43  64 8B 4B 64 8B 55 FC 3B    Sl.kp..Cd.Kd.U.;
0019D5 FIXU32
    FixUp: 035  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: EI[2]
    FixUp: 04c  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: EI[3]
    FixUp: 05a  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: EI[3]
    FixUp: 1a6  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[12]
    FixUp: 1f5  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[12]
    FixUp: 3a1  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: EI[2]
    FixUp: 3b8  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: EI[3]
    FixUp: 3c6  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: EI[3]
0019F9 LEDATA  Segment: _TEXT          Offset: 1400  Length: 0245
    0000: CA 77 44 8B 4B 50 8B 53  40 D3 E2 8B 4B 30 8B 73   .wD.KP.S@...K0.s
    0010: 64 0F B6 4C 31 02 33 D1  8B 4B 4C 23 D1 89 53 40   d..L1.3..KL#..S@
    0020: 8B 53 3C 8B 4B 40 0F B7  34 4A 8B 53 64 8B 4B 2C   .S<.K@..4J.Sd.K,
    0030: 23 D1 8B 4B 38 66 89 34  51 8B 53 3C 8B 4B 40 66   #..K8f.4Q.S<.K@f
    0040: 8B 7B 64 66 89 3C 4A FF  4B 70 75 AA 33 D2 89 53   .{df.<J.Kpu.3..S
    0050: 60 C7 43 58 02 00 00 00  FF 43 64 85 C0 0F 84 E9   `.CX.....Cd.....
    0060: FD FF FF 53 8B 43 54 85  C0 7C 0A 8B 4B 30 8B 43   ...S.CT..|..K0.C
    0070: 54 03 C8 EB 02 33 C9 51  8B 53 64 8B 43 54 2B D0   T....3.Q.Sd.CT+.
    0080: 52 6A 00 E8 00 00 00 00  8B 53 64 89 53 54 8B 0B   Rj.......Sd.ST..
    0090: 51 E8 36 F0 FF FF 8B 03  8B 50 10 85 D2 0F 85 A9   Q.6......P......
    00A0: FD FF FF 33 C0 E9 93 01  00 00 E9 9D FD FF FF 8B   ...3............
    00B0: 53 60 85 D2 0F 84 A4 00  00 00 8B 4B 30 8B 43 64   S`.........K0.Cd
    00C0: 8A 44 01 FF 8B 93 9C 16  00 00 8B 8B 98 16 00 00   .D..............
    00D0: 66 C7 04 4A 00 00 8B 93  98 16 00 00 FF 83 98 16   f..J............
    00E0: 00 00 8B 8B 90 16 00 00  88 04 11 25 FF 00 00 00   ...........%....
    00F0: 66 FF 84 83 8C 00 00 00  8B 83 94 16 00 00 48 8B   f.............H.
    0100: 93 98 16 00 00 3B C2 0F  94 C0 83 E0 01 85 C0 74   .....;.........t
    0110: 33 53 8B 53 54 85 D2 7C  0A 8B 4B 30 8B 43 54 03   3S.ST..|..K0.CT.
    0120: C8 EB 02 33 C9 51 8B 53  64 8B 43 54 2B D0 52 6A   ...3.Q.Sd.CT+.Rj
    0130: 00 E8 00 00 00 00 8B 53  64 89 53 54 8B 0B 51 E8   .......Sd.ST..Q.
    0140: 88 EF FF FF FF 43 64 FF  4B 6C 8B 03 8B 50 10 85   .....Cd.Kl...P..
    0150: D2 0F 85 F5 FC FF FF 33  C0 E9 DF 00 00 00 C7 43   .......3.......C
    0160: 60 01 00 00 00 FF 43 64  FF 4B 6C E9 DC FC FF FF   `.....Cd.Kl.....
    0170: 8B 53 60 85 D2 74 58 8B  4B 30 8B 43 64 8A 44 01   .S`..tX.K0.Cd.D.
    0180: FF 8B 93 9C 16 00 00 8B  8B 98 16 00 00 66 C7 04   .............f..
    0190: 4A 00 00 8B 93 98 16 00  00 FF 83 98 16 00 00 8B   J...............
    01A0: 8B 90 16 00 00 88 04 11  25 FF 00 00 00 66 FF 84   ........%....f..
    01B0: 83 8C 00 00 00 8B 83 94  16 00 00 48 8B 93 98 16   ...........H....
    01C0: 00 00 3B C2 0F 94 C1 83  E1 01 33 C0 89 43 60 53   ..;.......3..C`S
    01D0: 8B 53 54 85 D2 7C 0A 8B  4B 30 8B 43 54 03 C8 EB   .ST..|..K0.CT...
    01E0: 02 33 C9 51 8B 53 64 8B  43 54 2B D0 52 8B 55 08   .3.Q.Sd.CT+.R.U.
    01F0: 83 FA 04 0F 94 C1 83 E1  01 51 E8 00 00 00 00 8B   .........Q......
    0200: 43 64 89 43 54 8B 13 52  E8 BF EE FF FF 8B 0B 8B   Cd.CT..R........
    0210: 41 10 85 C0 75 13 8B 55  08 83 FA 04 75 07 B8 02   A...u..U....u...
    0220: 00 00 00 EB 18 33 C0 EB  14 8B 55 08 83 FA 04 75   .....3....U....u
    0230: 07 B8 03 00 00 00 EB 05  B8 01 00 00 00 5F 5E 5B   ............._^[
    0240: 59 5D C2 08 00                                     Y]...
001C45 FIXU32
    FixUp: 084  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[12]
    FixUp: 132  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[12]
    FixUp: 1fb  Mode: Self Loc: Offset32    Frame: TARGET  Target: EI[12]
001C55 LEDATA  Segment: _DATA          Offset: 0000  Length: 00B8
    0000: 00 00 00 00 00 00 00 00  C4 0D 00 00 04 00 04 00   ................
    0010: 08 00 04 00 3C 0F 00 00  04 00 05 00 10 00 08 00   ....<...........
    0020: 3C 0F 00 00 04 00 06 00  20 00 20 00 3C 0F 00 00   <....... . .<...
    0030: 04 00 04 00 10 00 10 00  40 12 00 00 08 00 10 00   ........@.......
    0040: 20 00 20 00 40 12 00 00  08 00 10 00 80 00 80 00    . .@...........
    0050: 40 12 00 00 08 00 20 00  80 00 00 01 40 12 00 00   @..... .....@...
    0060: 20 00 80 00 02 01 00 04  40 12 00 00 20 00 02 01    .......@... ...
    0070: 02 01 00 10 40 12 00 00  B1 00 00 00 20 64 65 66   ....@....... def
    0080: 6C 61 74 65 20 31 2E 31  2E 34 20 43 6F 70 79 72   late 1.1.4 Copyr
    0090: 69 67 68 74 20 31 39 39  35 2D 32 30 30 32 20 4A   ight 1995-2002 J
    00A0: 65 61 6E 2D 6C 6F 75 70  20 47 61 69 6C 6C 79 20   ean-loup Gailly 
    00B0: 00 31 2E 31 2E 34 00 00                            .1.1.4..
001D14 FIXU32
    FixUp: 008  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[1]
    FixUp: 014  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[1]
    FixUp: 020  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[1]
    FixUp: 02c  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[1]
    FixUp: 038  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[1]
    FixUp: 044  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[1]
    FixUp: 050  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[1]
    FixUp: 05c  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[1]
    FixUp: 068  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[1]
    FixUp: 074  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[1]
    FixUp: 078  Mode: Seg  Loc: Offset32    Frame: TARGET  Target: SI[2]
001D44 MODE32

