unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, Printers;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    StringGrid2: TStringGrid;
    Button2: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Memo1: TMemo;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  pi = 3.1415926535897932384626433832795;

implementation

{$R *.dfm}

procedure SaveGridToFile(Grid, Grid2: TStringGrid; const Filename: String);
var
  I, J        : Integer;
  Text        : TextFile;
  Widths      : array of Integer;
  TotalWidth  : Integer;
begin
  AssignFile(Text, Filename);
  Rewrite(Text);

  try
    SetLength(Widths, Grid.ColCount);
    TotalWidth := 0;

    for I := 0 to Grid.ColCount - 1 do
    begin
      Widths[I] := 0;
      for J := 0 to Grid.RowCount - 1 do
      begin
        if Length(Grid.Cells[I, J]) > Widths[I] then
          Widths[I] := Length(Grid.Cells[I, J]);
      end;
      Inc(Widths[I], 2);
      Inc(TotalWidth, Widths[I]);
    end;

    for I := 0 to Grid.RowCount - 1 do
    begin
      if I <> 0 then
      begin
        for J := 0 to TotalWidth + Length(Widths) + 3 do Write(Text, '-');
        Write(Text, #13#10);
      end;

      for J := 0 to Grid.ColCount - 1 do
      begin
        if J <> 0 then
          Write(Text, ' |');
        Write(Text, Grid.Cells[J, I]:Widths[J]);
      end;

      Write(Text, #13#10);
    end;
  finally
    CloseFile(Text);
  end;
end; 

procedure PrintGrid(sGrid: TStringGrid; sTitle: string);
var
  X1, X2: Integer;
  Y1, Y2: Integer;
  TmpI: Integer;
  F: Integer;
  TR: TRect;
begin
  Printer.Title := sTitle;
  Printer.BeginDoc;
  Printer.Canvas.Pen.Color  := 0;
  Printer.Canvas.Font.Name  := 'Times New Roman';
  Printer.Canvas.Font.Size  := 12;
  Printer.Canvas.Font.Style := [fsBold, fsUnderline];
  Printer.Canvas.TextOut(0, 100, Printer.Title);
  for F := 1 to sGrid.ColCount - 1 do
  begin
    X1 := 0;
    for TmpI := 1 to (F - 1) do
      X1 := X1 + 5 * (sGrid.ColWidths[TmpI]);
    Y1 := 300;
    X2 := 0;
    for TmpI := 1 to F do
      X2 := X2 + 5 * (sGrid.ColWidths[TmpI]);
    Y2 := 450;
    TR := Rect(X1, Y1, X2 - 30, Y2);
    Printer.Canvas.Font.Style := [fsBold];
    Printer.Canvas.Font.Size := 7;
    Printer.Canvas.TextRect(TR, X1 + 50, 350, sGrid.Cells[F, 0]);
    Printer.Canvas.Font.Style := [];
    for TmpI := 1 to sGrid.RowCount - 1 do
    begin
      Y1 := 150 * TmpI + 300;
      Y2 := 150 * (TmpI + 1) + 300;
      TR := Rect(X1, Y1, X2 - 30, Y2);
      Printer.Canvas.TextRect(TR, X1 + 50, Y1 + 50, sGrid.Cells[F, TmpI]);
    end;
  end;
  Printer.EndDoc;
end; 

procedure TForm1.Button1Click(Sender: TObject);
var i,j,k,m,n,r: integer;  f: double; s: double;
begin
  StringGrid1.RowCount := 362;
  StringGrid1.ColCount := 28;
  for j := 1 to 26 do
  begin
      StringGrid1.Cells[j,0] := IntToStr(j);
      
      for i := 0 to 360 do
      begin
          StringGrid1.Cells[0,i+1] := IntToStr(i);
                  
          if(RadioButton1.Checked ) then
            s := Sin(i*pi/180)
          else
            s := Cos(i*pi/180);
          f := s * j;
          k := trunc(f);
          StringGrid1.Cells[j,i+1] := IntToStr(k);
          
      end;
  end;

  StringGrid2.ColCount := 28;
  r := 0;
  for j := 1 to 26 do
  begin
      StringGrid2.Cells[j,0] := IntToStr(j);
      r := 0;
      for i := 0 to 360 do
      begin
            n := StrToInt(StringGrid1.Cells[j,i+1]);
            if n <> m then
            begin
                StringGrid2.Cells[0, r+1] := IntToStr(r);
                inc(r);
                if r > StringGrid2.RowCount then
                   StringGrid2.RowCount := r;
                StringGrid2.Cells[j, r+1] := IntToStr(i);
                if j = 26 then
                begin
                    StringGrid2.Cells[j+1, r+1] := FloatToStr( sin(i) );
                    memo1.Text := memo1.Text + FloatToStr(i) + ', ';
                end;
            end;

            m := n;
      end;
  end;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
SaveGridToFile(StringGrid2, StringGrid2, GetCurrentDir+'\teste.txt' );
//PrintGrid(StringGrid2, 'Print Stringgrid');
end;

end.















