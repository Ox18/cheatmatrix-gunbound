{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contain (for now) a single function for decoding strings with
    date/time values in them.
}
unit lvkDateUtils;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkDateUtils.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

{ Description:
    This function decodes a string containing a date/time value, to a TDateTime
    value.

    The format string can have the following format specifiers:

      yyyy = year, four digits
      yy   = year, two digits
      mmmm = full name of month
      mmm  = three-letter abbreviation of month
      mm   = month, two digits
      m    = month, one or two digits
      dddd = full name of day
      ddd  = three-letter abbreviation of day
      dd   = day, two digits
      d    = day, one or two digits
      hh   = hours, two digits, 0-23
      h    = hours, one or two digits, 0-23
      nn   = minutes, two digits
      ss   = seconds, two digits
      ms   = milliseconds, two digits
      am   = am or pm
  Parameters:
    Format        - The format string to use when decoding.
    DateTimeValue - The string containing the date/time value in the given
      format.
  See also:
    -
}
function DecodeDateTime(const Format, DateTimeValue: string): TDateTime;

implementation

uses
  SysUtils;

resourcestring
  SInternalError  = 'Internal error';
  
function DecodeDateTime(const Format, DateTimeValue: string): TDateTime;
var
  iFormat, iDate  : Integer;
  FormatType      : Integer;
  Index           : Integer;
  y, m, d, wd     : Word;
  h, mi, sec, ms  : Word;
  Found           : Boolean;
  HourAdjust      : Word;
const
  Formats : array[1..16] of string = (
    'dddd', 'ddd', 'dd', 'd',           // 1-4 = day
    'mmmm', 'mmm', 'mm', 'm',           // 5-8 = month
    'yyyy', 'yy',                       // 9-10 = year
    'hh', 'h', 'nn', 'ss', 'ms',        // 11-15 = time
    'am'                                // 16 = am/pm
  );
  Days : array[1..7] of string = (
    'MONDAY', 'TUESDAY', 'WEDNESDAY', 'THURSDAY', 'FRIDAY', 'SATURDAY', 'SUNDAY'
  );
  DayAbbrevs : array[1..7] of string = (
    'MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT', 'SUN'
  );
  Months : array[1..12] of string = (
    'JANUARY', 'FEBRUARY', 'MARCH', 'APRIL', 'MAY', 'JUNE', 'JULY', 'AUGUST', 'SEPTEMBER', 'OCTOBER', 'NOVEMBER', 'DECEMBER'
  );
  MonthAbbrevs : array[1..12] of string = (
    'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'
  );
  ConversionTable : array[1..7] of Integer = (
    7, 1, 2, 3, 4, 5, 6
  );
begin
  y := 0;
  m := 0;
  d := 0;
  wd := 0;
  h := 0;
  mi := 0;
  sec := 0;
  ms := 0;
  HourAdjust := 0;

  iFormat := 1;
  iDate := 1;
  while iFormat <= Length(Format) do
  begin
    if iDate > Length(DateTimeValue) then
      raise Exception.Create('Invalid datetime value');

    FormatType := 0;
    for Index := Low(Formats) to High(Formats) do
      if Copy(Format, iFormat, Length(Formats[Index])) = Formats[Index] then
      begin
        FormatType := Index;
        Break;
      end;

    if FormatType <> 0 then
      Inc(iFormat, Length(Formats[FormatType]));
    case FormatType of
      0 : if DateTimeValue[iDate] = Format[iFormat] then
      begin
        Inc(iDate);
        Inc(iFormat);
      end else
        raise Exception.Create('Invalid datetime value, expected ' + Format[iFormat]);

      // Days
      1 : begin
        Found := False;
        for Index := Low(Days) to High(Days) do
          if CompareText(Copy(DateTimeValue, iDate, Length(Days[Index])), Days[Index]) = 0 then
          begin
            Inc(iDate, Length(Days[Index]));
            wd := Index;
            Found := True;
            Break;
          end;
        if not Found then
          raise Exception.Create('Invalid datetime value, expected dayname');
      end;
      2 : begin
        Found := False;
        for Index := Low(DayAbbrevs) to High(DayAbbrevs) do
          if CompareText(Copy(DateTimeValue, iDate, Length(DayAbbrevs[Index])), DayAbbrevs[Index]) = 0 then
          begin
            Inc(iDate, Length(DayAbbrevs[Index]));
            wd := Index;
            Found := True;
            Break;
          end;
        if not Found then
          raise Exception.Create('Invalid datetime value, expected day abbreviation');
      end;
      3 : begin
        d := StrToInt(Copy(DateTimeValue, iDate, 2));
        Inc(iDate, 2);
      end;
      4 : begin
        try
          d := StrToInt(Copy(DateTimeValue, iDate, 2));
          Inc(iDate, 2);
        except
          d := StrToInt(Copy(DateTimeValue, iDate, 1));
          Inc(iDate);
        end;
      end;

      // Months
      5 : begin
        Found := False;
        for Index := Low(Months) to High(Months) do
          if CompareText(Copy(DateTimeValue, iDate, Length(Months[Index])), Months[Index]) = 0 then
          begin
            Inc(iDate, Length(Months[Index]));
            m := Index;
            Found := True;
            Break;
          end;
        if not Found then
          raise Exception.Create('Invalid datetime value, expected month name');
      end;
      6 : begin
        Found := False;
        for Index := Low(MonthAbbrevs) to High(MonthAbbrevs) do
          if CompareText(Copy(DateTimeValue, iDate, Length(MonthAbbrevs[Index])), MonthAbbrevs[Index]) = 0 then
          begin
            Inc(iDate, Length(MonthAbbrevs[Index]));
            m := Index;
            Found := True;
            Break;
          end;
        if not Found then
          raise Exception.Create('Invalid datetime value, expected month abbreviation');
      end;
      7 : begin
        m := StrToInt(Copy(DateTimeValue, iDate, 2));
        Inc(iDate, 2);
      end;
      8 : begin
        try
          m := StrToInt(Copy(DateTimeValue, iDate, 2));
          Inc(iDate, 2);
        except
          m := StrToInt(Copy(DateTimeValue, iDate, 1));
          Inc(iDate);
        end;
      end;

      // Year
      9 : begin
        y := StrToInt(Copy(DateTimeValue, iDate, 4));
        Inc(iDate, 4);
      end;
      10 : begin
        y := StrToInt(Copy(DateTimeValue, iDate, 2));
        if y < 50 then
          y := 2000 + y
        else
          y := 1900 + y;
        Inc(iDate, 2);
      end;

      // Hours
      11 : begin
        h := StrToInt(Copy(DateTimeValue, iDate, 2));
        Inc(iDate, 2);
      end;
      12 : begin
        try
          h := StrToInt(Copy(DateTimeValue, iDate, 2));
          Inc(iDate, 2);
        except
          h := StrToInt(Copy(DateTimeValue, iDate, 1));
          Inc(iDate);
        end;
      end;

      // Minutes
      13 : begin
        mi := StrToInt(Copy(DateTimeValue, iDate, 2));
        Inc(iDate, 2);
      end;

      // Seconds
      14 : begin
        sec := StrToInt(Copy(DateTimeValue, iDate, 2));
        Inc(iDate, 2);
      end;

      // Milliseconds
      15 : begin
        ms := StrToInt(Copy(DateTimeValue, iDate, 2));
        Inc(iDate, 2);
      end;

      // am/pm
      16 : begin
        if CompareText(Copy(DateTimeValue, iDate, 2), 'AM') = 0 then
        begin
          Inc(iDate, 2);
          HourAdjust := 0;
        end else if CompareText(Copy(DateTimeValue, iDate, 2), 'PM') = 0 then
        begin
          Inc(iDate, 2);
          HourAdjust := +12;
        end else
          raise Exception.Create('Invalid datetime value, expected AM or PM');
      end;
    else
      raise Exception.Create(SInternalError);
    end;
  end;

  Assert(iDate = Length(DateTimeValue)+1, 'Invalid datetime value or specifier');

  Result := EncodeDate(y, m, d) + EncodeTime(h + HourAdjust, mi, sec, ms);
  if wd <> 0 then
    Assert(ConversionTable[DayOfWeek(Result)]=wd, 'Invalid datetime value, day name does not match date');
end;

end.
