{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkBufferedStream, which buffers access from a
    underlying TStream descendant.
}
unit lvkBufferedStream;

// $Author: Lasse V. Karlsen $
// $Revision: 2 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkBufferedStream.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes;

type
  TlvkBufferedStream = class(TStream)
  private
    FStream     : TStream;
    FOwnsStream : Boolean;

    procedure SwitchToReading;
    procedure ResynchronizeReadPosition;
    procedure SwitchToWriting;
    procedure ResynchronizeWritePosition;

  public
    constructor Create(const Stream: TStream; const OwnsStream: Boolean=True);
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    {$IFDEF DELPHI6UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    {$ENDIF}
  end;

implementation

{ TlvkBufferedStream }

constructor TlvkBufferedStream.Create(const Stream: TStream;
  const OwnsStream: Boolean);
begin
  inherited Create;

  FStream := Stream;
  FOwnsStream := OwnsStream;
end;

destructor TlvkBufferedStream.Destroy;
begin
  if FOwnsStream then
    FStream.Free;

  inherited;
end;

function TlvkBufferedStream.Read(var Buffer; Count: Integer): Integer;
begin
  SwitchToReading;
  ResynchronizeReadPosition;

  Result := FStream.Read(Buffer, Count);
end;

function TlvkBufferedStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := FStream.Seek(Offset, Origin);
end;

procedure TlvkBufferedStream.ResynchronizeReadPosition;
begin

end;

procedure TlvkBufferedStream.ResynchronizeWritePosition;
begin

end;

{$IFDEF DELPHI6UP}
function TlvkBufferedStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset, Origin);
end;
{$ENDIF}

procedure TlvkBufferedStream.SwitchToReading;
begin

end;

procedure TlvkBufferedStream.SwitchToWriting;
begin

end;

function TlvkBufferedStream.Write(const Buffer; Count: Integer): Integer;
begin
  SwitchToWriting;
  ResynchronizeWritePosition;

  Result := FStream.Write(Buffer, Count);
end;

end.
