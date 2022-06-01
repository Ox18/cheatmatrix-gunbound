object Form10: TForm10
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Form10'
  ClientHeight = 532
  ClientWidth = 495
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 430
    Top = 94
    Width = 20
    Height = 16
    Caption = '----'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 15132390
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object SpinEdit2: TSpinEdit
    Left = 410
    Top = 8
    Width = 54
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 500
  end
  object SpinEdit3: TSpinEdit
    Left = 405
    Top = 93
    Width = 59
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 1
    Visible = False
  end
  object SpinEdit6: TSpinEdit
    Left = 405
    Top = 37
    Width = 59
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 1
    Visible = False
  end
  object SpinEdit7: TSpinEdit
    Left = 405
    Top = 65
    Width = 59
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 15
    Visible = False
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 495
    Height = 532
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 4
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label17: TLabel
        Left = 81
        Top = 65
        Width = 41
        Height = 16
        Caption = 'Status:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15132390
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object Label1: TLabel
        Left = 72
        Top = 8
        Width = 140
        Height = 29
        Caption = 'CALIBRADOR'
        Color = clBackground
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 14803425
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = True
      end
      object Label2: TLabel
        Left = 100
        Top = 33
        Width = 90
        Height = 19
        Caption = 'ShotMatrix'
        Color = clBackground
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 11184810
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = True
      end
      object Label18: TLabel
        Left = 126
        Top = 65
        Width = 7
        Height = 16
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15132390
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object Label19: TLabel
        Left = 314
        Top = 221
        Width = 128
        Height = 13
        Caption = 'CTRL+B : Liga/ Desliga bot'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15132390
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object Label5: TLabel
        Left = 18
        Top = 64
        Width = 32
        Height = 16
        Caption = 'For'#231'a'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15132390
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object Label7: TLabel
        Left = 354
        Top = 64
        Width = 57
        Height = 16
        Caption = 'Gravidade'
        Color = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15132390
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = True
      end
      object Label8: TLabel
        Left = 230
        Top = 64
        Width = 39
        Height = 16
        Caption = 'Angulo'
        Color = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15132390
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = True
      end
      object Label9: TLabel
        Left = 293
        Top = 64
        Width = 32
        Height = 16
        Caption = 'For'#231'a'
        Color = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15132390
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = True
      end
      object Panel1: TPanel
        Left = 270
        Top = 116
        Width = 17
        Height = 17
        TabOrder = 0
      end
      object Button2: TButton
        Left = 416
        Top = 484
        Width = 63
        Height = 17
        Caption = 'Sair'
        TabOrder = 1
        OnClick = Button2Click
      end
      object SpinEdit1: TSpinEdit
        Left = 16
        Top = 86
        Width = 59
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 26
      end
      object Memo1: TMemo
        Left = 15
        Top = 240
        Width = 456
        Height = 86
        ScrollBars = ssVertical
        TabOrder = 3
      end
      object ComboBox1: TComboBox
        Left = 81
        Top = 87
        Width = 141
        Height = 21
        ItemIndex = 0
        TabOrder = 4
        Text = 'Armor'
        OnChange = ComboBox1Change
        Items.Strings = (
          'Armor'
          'Mage'
          'Nak'
          'Trico'
          'BigFoot'
          'Boomer'
          'Raon'
          'Lightning'
          'JD'
          'A.Sate'
          'Ice'
          'Turtle'
          'Grub'
          'Aduka'
          'Kasildon'
          'J.Frog'
          'Dragon'
          'Knight')
      end
      object CheckBox1: TCheckBox
        Left = 132
        Top = 134
        Width = 104
        Height = 17
        Caption = 'Beep ao terminar'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
      end
      object CheckBox2: TCheckBox
        Left = 132
        Top = 116
        Width = 118
        Height = 17
        Caption = 'Beep ao mudar for'#231'a'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
      end
      object modoDebug: TCheckBox
        Left = 16
        Top = 116
        Width = 104
        Height = 17
        Caption = 'Modo debug'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 7
      end
      object SpinEdit4: TSpinEdit
        Left = 293
        Top = 86
        Width = 54
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 8
        Value = 26
      end
      object SpinEdit5: TSpinEdit
        Left = 228
        Top = 86
        Width = 59
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 9
        Value = 345
      end
      object Button1: TButton
        Left = 294
        Top = 161
        Width = 118
        Height = 17
        Caption = 'Gerar Calibra'#231#227'o'
        TabOrder = 10
        OnClick = Button1Click
      end
      object CheckBox3: TCheckBox
        Left = 16
        Top = 170
        Width = 104
        Height = 17
        Caption = 'Mostrar Mira'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 11
      end
      object CheckBox4: TCheckBox
        Left = 132
        Top = 152
        Width = 104
        Height = 17
        Caption = 'Decompor Full'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 12
      end
      object CheckBox5: TCheckBox
        Left = 16
        Top = 134
        Width = 104
        Height = 17
        Caption = 'Gravar Calibracao'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 13
      end
      object Edit1: TEdit
        Left = 353
        Top = 86
        Width = 59
        Height = 21
        TabOrder = 14
        Text = '0'
      end
      object CheckBox6: TCheckBox
        Left = 16
        Top = 152
        Width = 104
        Height = 17
        Caption = 'Imprimir na Tela'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 15
      end
      object Memo2: TMemo
        Left = 16
        Top = 414
        Width = 228
        Height = 63
        ScrollBars = ssBoth
        TabOrder = 16
      end
      object Memo3: TMemo
        Left = 252
        Top = 414
        Width = 222
        Height = 63
        ScrollBars = ssBoth
        TabOrder = 17
      end
      object erroLog: TMemo
        Left = 15
        Top = 332
        Width = 455
        Height = 76
        ScrollBars = ssVertical
        TabOrder = 18
      end
      object Button5: TButton
        Left = 294
        Top = 116
        Width = 118
        Height = 17
        Caption = 'Testar Calibra'#231#227'o'
        TabOrder = 19
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 294
        Top = 184
        Width = 118
        Height = 17
        Caption = 'Checar M'#233'dia'
        TabOrder = 20
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 293
        Top = 138
        Width = 118
        Height = 17
        Caption = 'Carregar Calibra'#231#227'o'
        TabOrder = 21
        OnClick = Button7Click
      end
      object CheckBox8: TCheckBox
        Left = 418
        Top = 184
        Width = 53
        Height = 17
        Caption = 'gravar'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 22
      end
      object CheckBox9: TCheckBox
        Left = 418
        Top = 161
        Width = 53
        Height = 17
        Caption = 'gravar'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 23
      end
      object CheckBox10: TCheckBox
        Left = 418
        Top = 116
        Width = 53
        Height = 17
        Hint = 'Verifica erros de calibra'#231#227'o'
        Caption = 'todas'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 24
      end
      object CheckBox7: TCheckBox
        Left = 132
        Top = 170
        Width = 104
        Height = 17
        Caption = 'Testar calibra'#231#227'o'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 25
      end
      object freezaVento: TCheckBox
        Left = 132
        Top = 193
        Width = 104
        Height = 17
        Caption = 'Freezar Vento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 26
      end
      object anyDC: TCheckBox
        Left = 17
        Top = 193
        Width = 104
        Height = 17
        Caption = 'Qualquer DC'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 27
        WordWrap = True
      end
      object CheckBox11: TCheckBox
        Left = 418
        Top = 93
        Width = 64
        Height = 17
        Hint = 'Verifica erros de calibra'#231#227'o'
        Caption = 'freezar'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 28
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object StringGrid1: TStringGrid
        Left = 3
        Top = 3
        Width = 481
        Height = 448
        FixedCols = 0
        FixedRows = 0
        TabOrder = 0
      end
    end
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 52
    Top = 8
  end
  object Timer2: TTimer
    OnTimer = Timer2Timer
    Left = 84
    Top = 8
  end
  object timerVento: TTimer
    Interval = 10
    OnTimer = timerVentoTimer
    Left = 116
    Top = 8
  end
  object writeTimer: TTimer
    Interval = 5
    OnTimer = writeTimerTimer
    Left = 152
    Top = 8
  end
  object XPManifest1: TXPManifest
    Left = 248
    Top = 8
  end
  object drawTimer: TTimer
    Interval = 5
    OnTimer = drawTimerTimer
    Left = 184
    Top = 8
  end
  object Timer6: TTimer
    OnTimer = Timer6Timer
    Left = 216
    Top = 8
  end
  object Timer3: TTimer
    Interval = 500
    OnTimer = Timer3Timer
    Left = 16
    Top = 8
  end
end
