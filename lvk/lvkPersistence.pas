{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code related to persisting items to a stream. This code
    is used in conjunction with streaming queue items over a network.
}
unit lvkPersistence;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkPersistence.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  ActiveX;

type
{ Description:
    A item class that supports streaming should provide a class factory that
    will be responsible for creating the item from a stream.

    See examples in the other files in this package.
}
  IlvkPersistenceClassFactory = interface(IPersist)
    ['{C44ED631-79C0-11D5-B238-0004761A6377}']

    { Description:
        This method should report back the classID that the factory supports.
    }
    function GetClassID(out classID: TCLSID): HResult; stdcall;

    { Description:
        This method should read data from a stream and convert it into a item
        of the correct type.
    }
    function CreateFromStream(const Stream: IStream; out Instance: IUnknown): HResult;
  end;

{ Description:
    This procedure registers a new item class factory with the streaming
    mechanism.
  Parameters:
    ClassFactory - An instance of an object that supports the class factory
      interface.
  See also:
    IlvkPersistenceClassFactory
}
procedure RegisterClass(const ClassFactory: IlvkPersistenceClassFactory);

{ Description:
    This procedure will figure out what class factory that supports the next
    item found in the stream, instantiate an instance of that object from the
    class factory, and ask the instance to read itself from the stream. The
    instance will then be returned back to the calling code.
  Parameters:
    Stream - The stream to read data from.
    Instance - The resulting instance item from the stream.
  Returns:
    S_OK if successful, anything else is an OLE error code.
  See also:
    SaveToStream
}
function LoadFromStream(const Stream: IStream; out Instance: IUnknown): HResult;

{ Description:
    This procedure takes an instance of an item and saves the item to the
    specified stream.

    Note: The specified instance must support the IPersistStream interface in
      order for the streaming mechanism to work properly.
  Parameters:
    Stream - The stream object to save the data to.
    Instance - The item instance to stream out.
  Returns:
    S_OK if successful, anything else is an OLE error code.
  See also:
    LoadFromStream
}
function SaveToStream(const Stream: IStream; const Instance: IUnknown): HResult;

implementation

uses
  Windows, ComObj, SysUtils, Classes;

var
  ClassFactories  : IInterfaceList  = nil;

procedure RegisterClass(const ClassFactory: IlvkPersistenceClassFactory);
begin
  if not Assigned(ClassFactories) then
    ClassFactories := TInterfaceList.Create as IInterfaceList;

  ClassFactories.Add(ClassFactory);
end;

function GetClassFactoryForClass(const classID: TCLSID): IlvkPersistenceClassFactory;
var
  TempClassID : TCLSID;
  Index       : Integer;
begin
  Result := nil;
  for Index := 0 to ClassFactories.Count-1 do
  begin
    if (ClassFactories[Index] as IlvkPersistenceClassFactory).GetClassID(TempClassID) = S_OK then
    begin
      if CompareMem(@TempClassID, @classID, SizeOf(classID)) then
      begin
        Result := ClassFactories[Index] as IlvkPersistenceClassFactory;
        Break;
      end;
    end;
  end;
end;

function LoadFromStream(const Stream: IStream; out Instance: IUnknown): HResult;
var
  ClassID   : TCLSID;
  BytesRead : LongInt;
  Factory   : IlvkPersistenceClassFactory;
begin
  Result := E_FAIL;
  Instance := nil;

  OleCheck(Stream.Read(@ClassID, SizeOf(ClassID), @BytesRead));
  if BytesRead = 0 then
    Exit;

  if BytesRead < SizeOf(ClassID) then
    raise Exception.Create('Unable to read class id from stream');

  Factory := GetClassFactoryForClass(ClassID);
  if not Assigned(Factory) then
    raise Exception.Create('No class factory registered for class');

  Result := Factory.CreateFromStream(Stream, Instance);
end;

function SaveToStream(const Stream: IStream; const Instance: IUnknown): HResult;
var
  ClassID       : TCLSID;
  BytesWritten  : LongInt;
  PersistStream : IPersistStream;
begin
  Assert(Assigned(Instance), 'No instancen given to SaveToStream');

  if Instance.QueryInterface(IPersistStream, PersistStream) <> S_OK then
    raise Exception.Create('Unable to get IPersistStream interface for class');

  OleCheck(PersistStream.GetClassID(ClassID));
  OleCheck(Stream.Write(@ClassID, SizeOf(ClassID), @BytesWritten));
  if BytesWritten < SizeOf(ClassID) then
    raise Exception.Create('Unable to write class id to stream');

  Result := PersistStream.Save(Stream, True);
end;

end.
