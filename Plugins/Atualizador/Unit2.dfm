object Form2: TForm2
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Form2'
  ClientHeight = 245
  ClientWidth = 333
  Color = 14935011
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label27: TLabel
    Left = 170
    Top = 191
    Width = 93
    Height = 13
    Caption = 'Espessura da linha:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label9: TLabel
    Left = 20
    Top = 182
    Width = 119
    Height = 13
    Caption = 'Cor da marca de espelho'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 333
    Height = 245
    ActivePage = TabSheet1
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 0
    OnChange = StatusTimerTimer
    object TabSheet1: TTabSheet
      Caption = 'Geral'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox1: TGroupBox
        Left = 0
        Top = 95
        Width = 322
        Height = 28
        Color = 14737632
        ParentColor = False
        TabOrder = 0
        object Ligado: TRadioButton
          Left = 18
          Top = 6
          Width = 37
          Height = 17
          Caption = 'On'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = LigadoClick
        end
        object Desligado: TRadioButton
          Left = 72
          Top = 6
          Width = 37
          Height = 17
          Caption = 'Off'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          TabStop = True
          OnClick = LigadoClick
        end
      end
      object Button1: TButton
        Left = 324
        Top = 211
        Width = 2
        Height = 6
        Caption = 'Button1'
        TabOrder = 1
        Visible = False
      end
      object GroupBox4: TGroupBox
        Left = 3
        Top = -2
        Width = 319
        Height = 91
        Caption = ' '
        Color = 14737632
        ParentColor = False
        TabOrder = 2
        object Label6: TLabel
          Left = 9
          Top = 13
          Width = 35
          Height = 13
          Caption = 'Plugin: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label7: TLabel
          Left = 8
          Top = 45
          Width = 34
          Height = 13
          Caption = 'Autor: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object sHack: TLabel
          Left = 49
          Top = 13
          Width = 92
          Height = 13
          Caption = 'Atualizador CMX'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object sAutor: TLabel
          Left = 49
          Top = 45
          Width = 114
          Height = 13
          Caption = 'SkyW4rrior && Di4blo'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object Label2: TLabel
          Left = 8
          Top = 29
          Width = 40
          Height = 13
          Caption = 'Vers'#227'o: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 49
          Top = 28
          Width = 17
          Height = 13
          Caption = '1.6'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object sPS: TLabel
          Left = 81
          Top = 67
          Width = 20
          Height = 13
          Caption = 'OFF'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label5: TLabel
          Left = 8
          Top = 67
          Width = 66
          Height = 13
          Caption = 'Plugin Status:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
      end
      object Memo5: TMemo
        Left = 3
        Top = 129
        Width = 319
        Height = 82
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 3
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'tb1'
      ImageIndex = 4
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Edit1: TEdit
        Left = 13
        Top = 13
        Width = 99
        Height = 21
        TabOrder = 0
        Text = '0'
      end
      object Edit2: TEdit
        Left = 13
        Top = 40
        Width = 99
        Height = 21
        TabOrder = 1
        Text = '0'
      end
      object Edit3: TEdit
        Left = 13
        Top = 67
        Width = 99
        Height = 21
        TabOrder = 2
        Text = '0'
      end
      object SpinEdit1: TSpinEdit
        Left = 118
        Top = 14
        Width = 91
        Height = 22
        MaxValue = 10
        MinValue = 0
        TabOrder = 3
        Value = 4
      end
      object SpinEdit2: TSpinEdit
        Left = 118
        Top = 42
        Width = 91
        Height = 22
        MaxValue = 10
        MinValue = 0
        TabOrder = 4
        Value = 3
      end
      object SpinEdit3: TSpinEdit
        Left = 118
        Top = 67
        Width = 91
        Height = 22
        MaxValue = 10
        MinValue = 0
        TabOrder = 5
        Value = 0
      end
      object Edit4: TEdit
        Left = 13
        Top = 90
        Width = 99
        Height = 21
        TabOrder = 6
        Text = '0'
      end
      object SpinEdit4: TSpinEdit
        Left = 118
        Top = 88
        Width = 91
        Height = 22
        MaxValue = 10
        MinValue = 0
        TabOrder = 7
        Value = 0
      end
      object Memo2: TMemo
        Left = 215
        Top = 6
        Width = 106
        Height = 34
        Lines.Strings = (
          'Memo2')
        ScrollBars = ssVertical
        TabOrder = 8
      end
      object SpinEdit555: TSpinEdit
        Left = 13
        Top = 191
        Width = 84
        Height = 22
        MaxValue = 19
        MinValue = 0
        TabOrder = 9
        Value = 6
      end
      object spinVento: TSpinEdit
        Left = 138
        Top = 189
        Width = 71
        Height = 22
        MaxValue = 26
        MinValue = 0
        TabOrder = 10
        Value = 0
      end
      object Edit5: TEdit
        Left = 13
        Top = 115
        Width = 99
        Height = 21
        TabOrder = 11
        Text = '0'
      end
      object SpinEdit5: TSpinEdit
        Left = 119
        Top = 116
        Width = 91
        Height = 22
        MaxValue = 10
        MinValue = 0
        TabOrder = 12
        Value = 2
      end
      object Edit6: TEdit
        Left = 13
        Top = 140
        Width = 99
        Height = 21
        TabOrder = 13
        Text = '0'
      end
      object SpinEdit6: TSpinEdit
        Left = 118
        Top = 141
        Width = 91
        Height = 22
        MaxValue = 10
        MinValue = 0
        TabOrder = 14
        Value = 2
      end
      object SpinEdit7: TSpinEdit
        Left = 118
        Top = 166
        Width = 91
        Height = 22
        MaxValue = 10
        MinValue = 0
        TabOrder = 15
        Value = 1
      end
      object Edit7: TEdit
        Left = 13
        Top = 164
        Width = 99
        Height = 21
        TabOrder = 16
        Text = '180'
      end
      object freezaVento: TCheckBox
        Left = 224
        Top = 194
        Width = 97
        Height = 17
        Caption = 'Freezar Vento'
        TabOrder = 17
      end
      object Button2: TButton
        Left = 216
        Top = 120
        Width = 75
        Height = 25
        Caption = 'Button2'
        TabOrder = 18
      end
      object Memo3: TMemo
        Left = 215
        Top = 44
        Width = 106
        Height = 70
        ScrollBars = ssVertical
        TabOrder = 19
      end
    end
    object TabSheet10: TTabSheet
      Caption = 'tb2'
      ImageIndex = 6
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo4: TMemo
        Left = 7
        Top = 5
        Width = 310
        Height = 167
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Button3: TButton
        Left = 124
        Top = 178
        Width = 75
        Height = 25
        Caption = 'Button2'
        TabOrder = 1
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Op'#231#245'es'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PageControl3: TPageControl
        Left = 0
        Top = 0
        Width = 325
        Height = 214
        ActivePage = TabSheet13
        Align = alClient
        Style = tsButtons
        TabOrder = 0
        object TabSheet13: TTabSheet
          Caption = 'Atalhos'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object labelItem: TLabel
            Left = 3
            Top = 168
            Width = 3
            Height = 13
          end
          object listaAtalhos: TListView
            Left = 1
            Top = 0
            Width = 315
            Height = 135
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Item'
                Width = 178
              end
              item
                Caption = 'Atalho'
                Width = 120
              end>
            GridLines = True
            Items.ItemData = {
              055B0000000100000000000000FFFFFFFFFFFFFFFF03000000FFFFFFFF000000
              000E4C0069006700610072002F004400650073006C0069006700610072000643
              00740072006C002B004200000000000131000000000001300000000000FFFFFF
              FFFFFF}
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
            OnSelectItem = listaAtalhosSelectItem
          end
          object HotKey1: THotKey
            Left = 146
            Top = 141
            Width = 124
            Height = 21
            HotKey = 0
            Modifiers = []
            TabOrder = 1
            OnEnter = HotKey1Enter
          end
          object Button6: TButton
            Left = 276
            Top = 142
            Width = 39
            Height = 19
            Caption = 'ok'
            TabOrder = 2
            OnClick = Button6Click
          end
          object ComboKey: TComboBox
            Left = 1
            Top = 141
            Width = 139
            Height = 22
            Style = csOwnerDrawFixed
            TabOrder = 3
            OnClick = ComboKeyClick
            Items.Strings = (
              ''
              'Enter - [Enter]'
              'Delete - [Del]'
              'Num Lock - [Pause]'
              'Print Screen - [Sys Req]')
          end
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Log'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object listaLog: TMemo
        Left = 3
        Top = 3
        Width = 319
        Height = 208
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 264
    Top = 184
  end
  object timerAutoShot: TTimer
    OnTimer = timerAutoShotTimer
    Left = 176
    Top = 176
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 64
    Top = 112
  end
  object Timer2: TTimer
    Enabled = False
    Interval = 200
    Left = 48
    Top = 192
  end
end
