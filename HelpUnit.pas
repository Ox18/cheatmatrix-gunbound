unit HelpUnit;

interface

uses windows, SysUtils, registry, DCPserpent, DCPrijndael, DCPcrypt2, DCPcast256,
  DCPhaval, DCPmd5, DCPsha1, Classes, SkyUtils;

implementation

//---- Is Registered -----------------------------------------------------------
Function AboutText(Plugin: String): boolean;
    var Valor: string;
    reg: TRegistry;
    Lido:String;
    j:integer;
    a: TDCP_serpent;
    i: integer;
    Cipher1: TDCP_serpent;
    Cipher2: TDCP_rijndael;
    Cipher3: TDCP_cast256;
    KeyStr: string;
    NewKey:string;
    Volume: string;
begin
   result:=false;
   reg:=TRegistry.Create;
   reg.RootKey := HKEY_LOCAL_MACHINE;
   if reg.OpenKey('Software\SkySoft',false) then begin
      if reg.ValueExists(Plugin) then
      begin
          Lido := reg.ReadString(Plugin);
          KeyStr:=lido;
          reg.CloseKey;
          reg.Free;
      end;
      Volume := GetIdeSerialNumber;
      KeyStr:= Plugin;
      Cipher1:= TDCP_serpent.Create(nil);
      Cipher1.InitStr(KeyStr,TDCP_haval);

      NewKey:= Cipher1.EncryptString(Volume);
      Cipher1.Burn;
      Cipher1.Free;

      //--------------------------

      Cipher2:= TDCP_rijndael.Create(nil);
      Cipher2.InitStr(KeyStr,TDCP_md5);

      NewKey:= Cipher2.EncryptString(NewKey);
      Cipher2.Burn;
      Cipher2.Free;

      //--------------------------

      Cipher3:= TDCP_cast256.Create(nil);
      Cipher3.InitStr(KeyStr,TDCP_sha1);

      NewKey:= Cipher3.EncryptString(NewKey);
      Cipher3.Burn;
      Cipher3.Free; 
      if NewKey<>'' then
      if Lido<>'' then
         if NewKey=Lido then
         begin
            result:=true;
            exit;
         end;
   end;
end;

end.
