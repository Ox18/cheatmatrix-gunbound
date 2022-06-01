unit winioctl;
//  This file is a part of DiskID for Delphi
//  Original code of DiskID can be taken here:
//  http://www.winsim.com/diskid32/diskid32.html
//  The code was ported from C++ to Delphi by Victor Derevyanko, dvpublic0@gmail.com
//  If you find any error please send me bugreport by email. Thanks in advance.
//  The translation was donated by efaktum (http://www.efaktum.dk).

interface
function CTL_CODE(aDeviceType: Integer; aFunction: Integer; aMethod: Integer; aAccess:Integer): Integer;

const
  FILE_DEVICE_DISK = $00000007;
  IOCTL_DISK_BASE = FILE_DEVICE_DISK;
  METHOD_BUFFERED = 0;

  FILE_ANY_ACCESS = 0;
  FILE_SPECIAL_ACCESS = (FILE_ANY_ACCESS);
  FILE_READ_ACCESS = $0001;    // file & pipe
  FILE_WRITE_ACCESS = $0002;    // file & pipe

  FILE_DEVICE_MASS_STORAGE = $0000002d;
  IOCTL_STORAGE_BASE = FILE_DEVICE_MASS_STORAGE;

type
  STORAGE_BUS_TYPE = (
    BusTypeUnknown = $00,
    BusTypeScsi,
    BusTypeAtapi,
    BusTypeAta,
    BusType1394,
    BusTypeSsa,
    BusTypeFibre,
    BusTypeUsb,
    BusTypeRAID,
    BusTypeiScsi,
    BusTypeSas,
    BusTypeSata,
    BusTypeSd,
    BusTypeMmc,
    BusTypeMax,
    BusTypeMaxReserved = $7F);

    MEDIA_TYPE = (
      Unknown,                // Format is unknown
      F5_1Pt2_512,            // 5.25", 1.2MB,  512 bytes/sector
      F3_1Pt44_512,           // 3.5",  1.44MB, 512 bytes/sector
      F3_2Pt88_512,           // 3.5",  2.88MB, 512 bytes/sector
      F3_20Pt8_512,           // 3.5",  20.8MB, 512 bytes/sector
      F3_720_512,             // 3.5",  720KB,  512 bytes/sector
      F5_360_512,             // 5.25", 360KB,  512 bytes/sector
      F5_320_512,             // 5.25", 320KB,  512 bytes/sector
      F5_320_1024,            // 5.25", 320KB,  1024 bytes/sector
      F5_180_512,             // 5.25", 180KB,  512 bytes/sector
      F5_160_512,             // 5.25", 160KB,  512 bytes/sector
      RemovableMedia,         // Removable media other than floppy
      FixedMedia,             // Fixed hard disk media
      F3_120M_512,            // 3.5", 120M Floppy
      F3_640_512,             // 3.5" ,  640KB,  512 bytes/sector
      F5_640_512,             // 5.25",  640KB,  512 bytes/sector
      F5_720_512,             // 5.25",  720KB,  512 bytes/sector
      F3_1Pt2_512,            // 3.5" ,  1.2Mb,  512 bytes/sector
      F3_1Pt23_1024,          // 3.5" ,  1.23Mb, 1024 bytes/sector
      F5_1Pt23_1024,          // 5.25",  1.23MB, 1024 bytes/sector
      F3_128Mb_512,           // 3.5" MO 128Mb   512 bytes/sector
      F3_230Mb_512,           // 3.5" MO 230Mb   512 bytes/sector
      F8_256_128,             // 8",     256KB,  128 bytes/sector
      F3_200Mb_512,           // 3.5",   200M Floppy (HiFD)
      F3_240M_512,            // 3.5",   240Mb Floppy (HiFD)
      F3_32M_512              // 3.5",   32Mb Floppy
    );

  DISK_GEOMETRY = record
    Cylinders: Int64; //LARGE_INTEGER
    MediaType: MEDIA_TYPE;
    TracksPerCylinder: Cardinal;
    SectorsPerTrack: Cardinal;
    BytesPerSector: Cardinal;
  end;
  PDISK_GEOMETRY = ^DISK_GEOMETRY;



implementation

function CTL_CODE(aDeviceType: Integer; aFunction: Integer; aMethod: Integer; aAccess:Integer): Integer;
begin
    Result := ((aDeviceType) shl 16) or ((aAccess) shl 14) or ((aFunction) shl 2) or (aMethod);
end;
end.
