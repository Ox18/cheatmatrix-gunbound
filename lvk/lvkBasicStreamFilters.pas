{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains base classes for new stream filters.
}
unit lvkBasicStreamFilters;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkBasicStreamFilters.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, SysUtils, Classes, lvkStreamFilters, lvkVersion;

type
{ Description:
    This base class can be used as the basis for stream filters that won't
    be implementing all of the interfaces. For instance, a read-only
    filter might want to descend from this class.
  See also:
    TlvkCompleteStreamFilter
}
  TlvkBaseStreamFilter = class(TInterfacedObject, IStreamFilter,
    IPackageVersion)
  private
    FNextFilter : IStreamFilter;

  protected
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;

    // <ALIAS IStreamFilter.SetNextFilter@IStreamFilter>
    procedure SetNextFilter(const Filter: IStreamFilter); virtual;

    { Description:
        This property returns the next filter in the sequence. It's protected
        so it can only be used from descendant classes.
    }
    property NextFilter: IStreamFilter read FNextFilter;
  end;

{ Description:
    This base class can be used as the basis for stream filters that implements
    or allows reading, writing, seeking and sizing. The individual interfaces
    will in this base class simply be passed down to the next filter in the
    sequence, so the descendant class can implement what's needed.
  See also:
    TlvkBaseStreamFilter
}
  TlvkCompleteStreamFilter = class(TlvkBaseStreamFilter,
    IReadableStreamFilter, IWriteableStreamFilter, ISizeableStreamFilter,
    ISeekableStreamFilter)
  protected
    // <ALIAS IReadableStreamFilter.Read@@Longint>
    function Read(var Buffer; const Count: Longint): Longint; virtual;

    // <ALIAS IWriteableStreamFilter.Write@@Longint>
    function Write(const Buffer; const Count: Longint): Longint; virtual;

    // <ALIAS IWriteableStreamFilter.Flush>
    procedure Flush; virtual;

    // <ALIAS ISeekableStreamFilter.Seek@Int64@TSeekOrigin>
    function Seek(const Offset: Int64; const Origin: TSeekOrigin): Int64; virtual;

    // <ALIAS ISizeableStreamFilter.SetSize@Int64>
    procedure SetSize(const NewSize: Int64); virtual;
  end;

implementation

{ TlvkBaseStreamFilter }

function TlvkBaseStreamFilter.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkBaseStreamFilter.SetNextFilter(const Filter: IStreamFilter);
begin
  FNextFilter := Filter;
end;

{ TlvkCompleteStreamFilter }

procedure TlvkCompleteStreamFilter.Flush;
begin
  // Do nothing
end;

function TlvkCompleteStreamFilter.Read(var Buffer;
  const Count: Integer): Longint;
var
  rsf : IReadableStreamFilter;
begin
  Assert(Assigned(NextFilter), 'No next filter');

  if NextFilter.QueryInterface(IReadableStreamFilter, rsf) = S_OK then
    Result := rsf.Read(Buffer, Count)
  else
    raise EStreamRead.Create('Next filter is not readable in call to Read');
end;

function TlvkCompleteStreamFilter.Seek(const Offset: Int64;
  const Origin: TSeekOrigin): Int64;
var
  ssf : ISeekableStreamFilter;
begin
  Assert(Assigned(NextFilter), 'No next filter');

  if NextFilter.QueryInterface(ISeekableStreamFilter, ssf) = S_OK then
    Result := ssf.Seek(Offset, Origin)
  else
    raise EStreamSeek.Create('Next filter is not seekable in call to Seek');
end;

procedure TlvkCompleteStreamFilter.SetSize(const NewSize: Int64);
var
  ssf : ISizeableStreamFilter;
begin
  Assert(Assigned(NextFilter), 'No next filter');

  if NextFilter.QueryInterface(ISizeableStreamFilter, ssf) = S_OK then
    ssf.SetSize(NewSize)
  else
    raise EStreamSize.Create('Next filter is not sizeable in call to SetSize');
end;

function TlvkCompleteStreamFilter.Write(const Buffer;
  const Count: Integer): Longint;
var
  wsf : IWriteableStreamFilter;
begin
  Assert(Assigned(NextFilter), 'No next filter');

  if NextFilter.QueryInterface(IWriteableStreamFilter, wsf) = S_OK then
    Result := wsf.Write(Buffer, Count)
  else
    raise EStreamWrite.Create('Next filter is not writeable in call to Write');
end;

end.
