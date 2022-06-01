object FrameDebugger: TFrameDebugger
  Left = 0
  Top = 0
  Width = 542
  Height = 353
  TabOrder = 0
  TabStop = True
  object Label1: TLabel
    Left = 21
    Top = 63
    Width = 88
    Height = 13
    Caption = 'Nome do Processo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 91
    Top = 59
    Width = 45
    Height = 13
    Caption = 'Endere'#231'o'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 131
    Top = 105
    Width = 24
    Height = 13
    Caption = 'Valor'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 21
    Top = 105
    Width = 17
    Height = 13
    Caption = 'PID'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 79
    Top = 105
    Width = 39
    Height = 13
    Caption = 'PHandle'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 203
    Top = 106
    Width = 19
    Height = 13
    Caption = 'Size'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Button4: TButton
    Left = 152
    Top = 251
    Width = 33
    Height = 16
    Caption = 'Teste'
    TabOrder = 0
    OnClick = Button4Click
  end
  object Button1: TButton
    Left = 152
    Top = 231
    Width = 33
    Height = 16
    Caption = 'Load'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 317
    Top = 231
    Width = 37
    Height = 16
    Caption = 'Unload'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button7: TButton
    Left = 187
    Top = 231
    Width = 66
    Height = 16
    Caption = 'OpenProcess'
    TabOrder = 3
    OnClick = Button7Click
  end
  object Button5: TButton
    Left = 155
    Top = 265
    Width = 44
    Height = 19
    Caption = 'GetVersion'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button3: TButton
    Left = 444
    Top = 154
    Width = 61
    Height = 19
    Caption = 'Clear Log'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Memo1: TMemo
    Left = 16
    Top = 11
    Width = 98
    Height = 46
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object Button6: TButton
    Left = 255
    Top = 251
    Width = 60
    Height = 16
    Caption = 'List Pointers'
    TabOrder = 7
    OnClick = Button6Click
  end
  object Button8: TButton
    Left = 187
    Top = 251
    Width = 66
    Height = 16
    Caption = 'Load Pointers'
    TabOrder = 8
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 16
    Top = 211
    Width = 75
    Height = 19
    Caption = 'Write File'
    TabOrder = 9
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 255
    Top = 231
    Width = 58
    Height = 17
    Caption = 'CheckHook'
    TabOrder = 10
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 97
    Top = 211
    Width = 75
    Height = 19
    Caption = 'List Table'
    TabOrder = 11
    OnClick = Button11Click
  end
  object Edit1: TEdit
    Left = 21
    Top = 78
    Width = 64
    Height = 21
    TabOrder = 12
  end
  object Edit2: TEdit
    Left = 91
    Top = 78
    Width = 66
    Height = 21
    TabOrder = 13
  end
  object Edit3: TEdit
    Left = 131
    Top = 119
    Width = 66
    Height = 21
    TabOrder = 14
    Text = '12 ?? ?3'
  end
  object Button13: TButton
    Left = 163
    Top = 63
    Width = 38
    Height = 16
    Caption = 'Open'
    TabOrder = 15
    OnClick = Button13Click
  end
  object Button14: TButton
    Left = 205
    Top = 63
    Width = 38
    Height = 17
    Caption = 'Read D'
    TabOrder = 16
    OnClick = Button14Click
  end
  object Button15: TButton
    Left = 247
    Top = 63
    Width = 38
    Height = 17
    Caption = 'Write'
    TabOrder = 17
    OnClick = Button15Click
  end
  object Edit4: TEdit
    Left = 21
    Top = 119
    Width = 49
    Height = 21
    TabOrder = 18
  end
  object Edit5: TEdit
    Left = 76
    Top = 119
    Width = 49
    Height = 21
    TabOrder = 19
  end
  object Edit6: TEdit
    Left = 203
    Top = 119
    Width = 66
    Height = 21
    TabOrder = 20
  end
  object Button16: TButton
    Left = 317
    Top = 251
    Width = 39
    Height = 16
    Caption = 'Hide'
    TabOrder = 21
    OnClick = Button16Click
  end
  object Button12: TButton
    Left = 16
    Top = 239
    Width = 75
    Height = 18
    Caption = 'UNHIDE'
    TabOrder = 22
    OnClick = Button12Click
  end
  object Button17: TButton
    Left = 163
    Top = 88
    Width = 38
    Height = 17
    Caption = 'Scan'
    TabOrder = 23
    OnClick = Button17Click
  end
  object ProgressBar1: TProgressBar
    Left = 21
    Top = 195
    Width = 489
    Height = 10
    Max = 153
    Smooth = True
    TabOrder = 24
  end
  object ListBox1: TListBox
    Left = 120
    Top = 11
    Width = 97
    Height = 46
    ItemHeight = 13
    TabOrder = 25
    OnClick = ListBox1Click
  end
  object CheckBox1: TCheckBox
    Left = 186
    Top = 173
    Width = 97
    Height = 17
    Caption = 'Scan Read Only'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 26
  end
  object Edit7: TEdit
    Left = 21
    Top = 146
    Width = 104
    Height = 21
    TabOrder = 27
  end
  object Edit8: TEdit
    Left = 131
    Top = 146
    Width = 66
    Height = 21
    TabOrder = 28
    Text = '12 ?? ?3'
  end
  object Button18: TButton
    Left = 21
    Top = 298
    Width = 70
    Height = 20
    Caption = 'Get Window'
    TabOrder = 29
    OnClick = Button18Click
  end
  object Edit9: TEdit
    Left = 76
    Top = 168
    Width = 49
    Height = 21
    TabOrder = 30
  end
  object Button19: TButton
    Left = 21
    Top = 324
    Width = 70
    Height = 20
    Caption = 'Ligar Timer'
    TabOrder = 31
    OnClick = Button19Click
  end
  object Edit10: TEdit
    Left = 131
    Top = 168
    Width = 49
    Height = 21
    TabOrder = 32
  end
  object Edit11: TEdit
    Left = 21
    Top = 168
    Width = 49
    Height = 21
    TabOrder = 33
  end
  object Button20: TButton
    Left = 21
    Top = 272
    Width = 70
    Height = 20
    Caption = 'GetSelfDC'
    TabOrder = 34
    OnClick = Button20Click
  end
  object Button21: TButton
    Left = 262
    Top = 325
    Width = 70
    Height = 20
    Caption = 'Lista'
    TabOrder = 35
    OnClick = Button21Click
  end
  object Button22: TButton
    Left = 203
    Top = 146
    Width = 40
    Height = 20
    Caption = 'Step1'
    TabOrder = 36
    OnClick = Button22Click
  end
  object Button23: TButton
    Left = 249
    Top = 146
    Width = 40
    Height = 20
    Caption = 'Step2'
    TabOrder = 37
    OnClick = Button23Click
  end
  object Button24: TButton
    Left = 353
    Top = 299
    Width = 102
    Height = 20
    Caption = 'Compare'
    TabOrder = 38
    OnClick = Button24Click
  end
  object Button25: TButton
    Left = 353
    Top = 325
    Width = 102
    Height = 20
    Caption = 'Clear'
    TabOrder = 39
    OnClick = Button25Click
  end
  object Button26: TButton
    Left = 205
    Top = 87
    Width = 38
    Height = 17
    Caption = 'Read S'
    TabOrder = 40
    OnClick = Button26Click
  end
  object Button27: TButton
    Left = 248
    Top = 88
    Width = 36
    Height = 17
    Caption = 'teste'
    TabOrder = 41
    OnClick = Button27Click
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 1
    OnTimer = Timer1Timer
    Left = 416
    Top = 320
  end
end
