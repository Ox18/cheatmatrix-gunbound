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
      object GroupBox3: TGroupBox
        Left = 184
        Top = -2
        Width = 134
        Height = 137
        Caption = ' Modo de Mira '
        Color = 14935011
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentBackground = False
        ParentColor = False
        ParentFont = False
        TabOrder = 0
        object radioPerto: TRadioButton
          Left = 10
          Top = 17
          Width = 113
          Height = 17
          Caption = 'Mais pr'#243'ximo'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = radioMouseClick
        end
        object radioLonge: TRadioButton
          Left = 10
          Top = 36
          Width = 113
          Height = 17
          Caption = 'Mais Longe'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = radioMouseClick
        end
        object radioSlice: TRadioButton
          Left = 10
          Top = 54
          Width = 113
          Height = 17
          Caption = 'Slice'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          TabStop = True
          OnClick = radioMouseClick
        end
        object radioPersonalizado: TRadioButton
          Left = 10
          Top = 110
          Width = 113
          Height = 17
          Caption = 'Personalizado'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnClick = radioMouseClick
        end
        object radioRandom: TRadioButton
          Left = 10
          Top = 91
          Width = 113
          Height = 17
          Caption = 'Aleat'#243'rio'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          OnClick = radioMouseClick
        end
        object radioMouse: TRadioButton
          Left = 10
          Top = 72
          Width = 113
          Height = 17
          Caption = 'Mouse'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
          OnClick = radioMouseClick
        end
      end
      object GroupBox1: TGroupBox
        Left = 3
        Top = 103
        Width = 172
        Height = 32
        Color = 14737632
        ParentColor = False
        TabOrder = 1
        object Ligado: TRadioButton
          Left = 18
          Top = 10
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
          Top = 10
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
        TabOrder = 2
        Visible = False
      end
      object GroupBox4: TGroupBox
        Left = 3
        Top = -2
        Width = 172
        Height = 104
        Caption = ' '
        Color = 14737632
        ParentColor = False
        TabOrder = 3
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
          Width = 63
          Height = 13
          Caption = 'ShotMatrix'
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
      object GroupBox2: TGroupBox
        Left = 181
        Top = 164
        Width = 137
        Height = 44
        Color = 14935011
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentBackground = False
        ParentColor = False
        ParentFont = False
        TabOrder = 4
        Visible = False
        object Label31: TLabel
          Left = 10
          Top = 10
          Width = 34
          Height = 13
          Caption = 'Mobile:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label32: TLabel
          Left = 48
          Top = 10
          Width = 4
          Height = 13
          Caption = '-'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label4: TLabel
          Left = 39
          Top = 25
          Width = 4
          Height = 13
          Caption = '-'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label10: TLabel
          Left = 9
          Top = 25
          Width = 25
          Height = 13
          Caption = 'Alvo:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Button8: TButton
          Left = 212
          Top = 14
          Width = 75
          Height = 25
          Caption = 'Button8'
          TabOrder = 0
          Visible = False
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Op'#231#245'es'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PageControl3: TPageControl
        Left = 0
        Top = 0
        Width = 325
        Height = 214
        ActivePage = TabSheet11
        Align = alClient
        Style = tsButtons
        TabOrder = 0
        object TabSheet11: TTabSheet
          Caption = 'Linha'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label15: TLabel
            Left = 11
            Top = 3
            Width = 55
            Height = 13
            Caption = 'Cor da Mira'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object Label28: TLabel
            Left = 177
            Top = 4
            Width = 109
            Height = 13
            Caption = 'Amostragem do Status'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object Label26: TLabel
            Left = 177
            Top = 42
            Width = 89
            Height = 13
            Caption = 'Espessura da linha'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object Label35: TLabel
            Left = 12
            Top = 41
            Width = 112
            Height = 13
            Caption = 'Cor da Mira 2 (SS e TC)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object Label13: TLabel
            Left = 12
            Top = 81
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
          object Label14: TLabel
            Left = 12
            Top = 123
            Width = 120
            Height = 13
            Caption = 'Cor da marca de tornado'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object corLinha1: TColorBox
            Left = 11
            Top = 18
            Width = 129
            Height = 22
            Selected = clBlue
            Style = [cbStandardColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
            TabOrder = 0
            OnChange = corLinha1Change
          end
          object amostragem: TComboBox
            Left = 177
            Top = 19
            Width = 128
            Height = 21
            Style = csDropDownList
            ItemIndex = 3
            TabOrder = 1
            Text = '5 seg'
            OnChange = amostragemChange
            Items.Strings = (
              'Desligado'
              '2 seg'
              '3 seg'
              '5 seg'
              '10 seg'
              'Sempre Ligado')
          end
          object espessuraLinha: TComboBox
            Left = 177
            Top = 57
            Width = 128
            Height = 21
            Hint = 'Linhas grossas podem causar LAG'
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 2
            Text = '1px'
            OnChange = espessuraLinhaChange
            Items.Strings = (
              '1px'
              '2px'
              '3px')
          end
          object corLinha2: TColorBox
            Left = 11
            Top = 56
            Width = 129
            Height = 22
            Hint = 'Cor da mira ap'#243's a marca de SS ou em Tiros Compostos'
            Selected = clRed
            Style = [cbStandardColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
            TabOrder = 3
            OnChange = corLinha2Change
          end
          object corEspelho: TColorBox
            Left = 12
            Top = 95
            Width = 129
            Height = 22
            Selected = clGreen
            Style = [cbStandardColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
            TabOrder = 4
            OnChange = corEspelhoChange
          end
          object corTornado: TColorBox
            Left = 12
            Top = 137
            Width = 129
            Height = 22
            Selected = clNavy
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
            TabOrder = 5
            OnChange = corTornadoChange
          end
        end
        object TabSheet12: TTabSheet
          Caption = 'Configura'#231#245'es'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object graficoAntigo: TCheckBox
            Left = 19
            Top = 2
            Width = 152
            Height = 31
            Caption = 'Modo GDI'
            TabOrder = 0
            WordWrap = True
            OnClick = graficoAntigoClick
          end
          object Button4: TButton
            Left = 0
            Top = 162
            Width = 97
            Height = 18
            Caption = 'Salvar Configs'
            TabOrder = 1
            OnClick = Button4Click
          end
          object Button5: TButton
            Left = 102
            Top = 162
            Width = 99
            Height = 18
            Caption = 'Carregar Configs'
            TabOrder = 2
            OnClick = Button5Click
          end
          object Button7: TButton
            Left = 206
            Top = 162
            Width = 112
            Height = 18
            Caption = 'Restaurar Configs'
            TabOrder = 3
            OnClick = Button7Click
          end
          object modoJudas: TCheckBox
            Tag = 1
            Left = 19
            Top = 29
            Width = 97
            Height = 17
            Caption = 'Mirar em amigos'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 4
            Visible = False
            OnClick = modoJudasClick
          end
        end
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
              05D30700001300000000000000FFFFFFFFFFFFFFFF03000000FFFFFFFF000000
              000E4C0069006700610072002F004400650073006C0069006700610072000A43
              00740072006C002B0045006E0074006500720000000000013100000000000130
              000000000000000000FFFFFFFFFFFFFFFF03000000FFFFFFFF000000000D5300
              74006100740075007300200064006F002000410069006D000A4300740072006C
              002B005000610075007300650000000000013200000000000130000000000000
              000000FFFFFFFFFFFFFFFF03000000FFFFFFFF000000000E50007200F3007800
              69006D006F0020004D006F00620069006C0065000A4300740072006C002B004E
              0075006D0020002D0000000000013300000000000130000000000000000000FF
              FFFFFFFFFFFFFF03000000FFFFFFFF000000000F4D006F00620069006C006500
              200041006E0074006500720069006F00720010530068006900660074002B0043
              00740072006C002B004E0075006D0020002D0000000000013400000000000130
              000000000000000000FFFFFFFFFFFFFFFF03000000FFFFFFFF000000000C5000
              7200F300780069006D006F00200041006C0076006F000A4300740072006C002B
              004E0075006D0020002B00000000000135000000000001300000000000000000
              00FFFFFFFFFFFFFFFF03000000FFFFFFFF000000000D41006C0076006F002000
              41006E0074006500720069006F00720010530068006900660074002B00430074
              0072006C002B004E0075006D0020002B00000000000136000000000001300000
              00000000000000FFFFFFFFFFFFFFFF03000000FFFFFFFF000000001450007200
              F300780069006D006F0020006D006F0064006F0020006400650020006D006900
              72006100064300740072006C002B002E00000000000137000000000001300000
              00000000000000FFFFFFFFFFFFFFFF03000000FFFFFFFF00000000154D006F00
              64006F0020006400650020006D00690072006100200061006E00740065007200
              69006F0072000C530068006900660074002B004300740072006C002B002E0000
              000000013800000000000130000000000000000000FFFFFFFFFFFFFFFF030000
              00FFFFFFFF000000000F4400650074006500630074006100720020004D006F00
              620069006C0065000A4300740072006C002B004E0075006D0020002F00000000
              00013900000000000130000000000000000000FFFFFFFFFFFFFFFF03000000FF
              FFFFFF00000000084200610063006B00730068006F0074000A4300740072006C
              002B004E0075006D0020002A0000000000023100300000000000013000000000
              0000000000FFFFFFFFFFFFFFFF03000000FFFFFFFF00000000174C0069006E00
              68006100200076006500720074006900630061006C0020006E006F0020006D00
              6F007500730065000541006C0074002B00580000000000023100350000000000
              0130000000000000000000FFFFFFFFFFFFFFFF03000000FFFFFFFF0000000016
              4D0061007200630061007200200074006F0072006E00610064006F002F006500
              7300700065006C0068006F00064300740072006C002B00580000000000023200
              3000000000000130000000000000000000FFFFFFFFFFFFFFFF03000000FFFFFF
              FF0000000017520065006D006F00760065007200200074006F0072006E006100
              64006F002F0065007300700065006C0068006F000C530068006900660074002B
              004300740072006C002B00580000000000023200350000000000013000000000
              0000000000FFFFFFFFFFFFFFFF03000000FFFFFFFF00000000184C0069006D00
              700061007200200074006F0072006E00610064006F0073002F00650073007000
              65006C0068006F0073000A4300740072006C002B0041006C0074002B00580000
              0000000232003600000000000130000000000000000000FFFFFFFFFFFFFFFF03
              000000FFFFFFFF000000001841006C007400650072006E006100720020007400
              6F0072006E00610064006F002F0065007300700065006C0068006F0006430074
              0072006C002B004D000000000002330030000000000001300000000000000000
              00FFFFFFFFFFFFFFFF03000000FFFFFFFF000000000E55007300610072002000
              6100750074006F002D00730068006F007400064300740072006C002B00500000
              0000000233003500000000000130000000000000000000FFFFFFFFFFFFFFFF03
              000000FFFFFFFF000000000F530065006C006500630069006F006E0061007200
              200041006C0076006F000C4300540052004C002B00530048004900460054002B
              004300000000000233003600000000000130000000000000000000FFFFFFFFFF
              FFFFFF03000000FFFFFFFF000000001054006900720061007200200053006300
              7200650065006E00730068006F0074000C4300540052004C002B005300790073
              0020005200650071000000000002310031000000000001300000000000000000
              00FFFFFFFFFFFFFFFF03000000FFFFFFFF000000000A4D006F00760065007200
              20004D0065006E0075000C4300540052004C002B00530048004900460054002B
              004D000000000002310032000000000001300000000000FFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFF}
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
    object TabSheet9: TTabSheet
      Caption = 'Mobiles'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object TE_ASate: TCheckBox
        Left = 181
        Top = 11
        Width = 126
        Height = 17
        Caption = 'Raio(s) de A.Sate'
        Checked = True
        State = cbChecked
        TabOrder = 0
        Visible = False
        OnClick = TE_ASateClick
      end
      object Marcar_SS: TCheckBox
        Left = 181
        Top = 29
        Width = 129
        Height = 17
        Caption = 'Marcar SS'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = Marcar_SSClick
      end
      object dbgCHK: TCheckBox
        Left = 181
        Top = 48
        Width = 97
        Height = 17
        Checked = True
        State = cbChecked
        TabOrder = 2
        Visible = False
        OnClick = dbgCHKClick
      end
      object TC_BigFoot: TCheckBox
        Left = 10
        Top = 8
        Width = 142
        Height = 17
        Caption = 'Tiro composto de Big Foot'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = TC_BigFootClick
      end
      object TC_Mage: TCheckBox
        Left = 10
        Top = 29
        Width = 142
        Height = 17
        Caption = 'Tiro composto de Mage'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = TC_MageClick
      end
      object TC_Trico: TCheckBox
        Left = 10
        Top = 48
        Width = 142
        Height = 17
        Caption = 'Tiro composto de Trico'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = TC_TricoClick
      end
      object TE_Nak: TCheckBox
        Left = 10
        Top = 190
        Width = 142
        Height = 17
        Caption = 'Inversao de Nak'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = TE_NakClick
      end
      object TC_Grub: TCheckBox
        Left = 10
        Top = 84
        Width = 142
        Height = 17
        Caption = 'Tiro composto de Grub'
        Checked = True
        State = cbChecked
        TabOrder = 7
        OnClick = TC_GrubClick
      end
      object TC_Kasildon: TCheckBox
        Left = 10
        Top = 102
        Width = 142
        Height = 17
        Caption = 'Tiro composto de Kasildon'
        Checked = True
        State = cbChecked
        TabOrder = 8
        OnClick = TC_KasildonClick
      end
      object TC_Dragao: TCheckBox
        Left = 10
        Top = 138
        Width = 142
        Height = 17
        Caption = 'Tiro composto de Drag'#227'o'
        Checked = True
        State = cbChecked
        TabOrder = 9
        OnClick = TC_DragaoClick
      end
      object TC_Maya: TCheckBox
        Left = 10
        Top = 120
        Width = 142
        Height = 17
        Caption = 'Tiro composto de Maya'
        Checked = True
        State = cbChecked
        TabOrder = 10
        OnClick = TC_MayaClick
      end
      object TC_Raon: TCheckBox
        Left = 10
        Top = 155
        Width = 142
        Height = 17
        Caption = 'Tiro composto de Raon'
        Checked = True
        State = cbChecked
        TabOrder = 11
        OnClick = TC_RaonClick
      end
      object TC_frank: TCheckBox
        Left = 10
        Top = 173
        Width = 142
        Height = 17
        Caption = 'Tiro composto de Frank'
        Checked = True
        State = cbChecked
        TabOrder = 12
        OnClick = TC_frankClick
      end
      object TC_Turtle: TCheckBox
        Left = 10
        Top = 66
        Width = 142
        Height = 17
        Caption = 'Tiro composto de Turtle'
        Checked = True
        State = cbChecked
        TabOrder = 13
        OnClick = TC_TurtleClick
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Atalhos'
      ImageIndex = 3
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 3
        Top = 3
        Width = 31
        Height = 13
        Caption = 'Atalho'
      end
      object Bevel3: TBevel
        Left = 0
        Top = 0
        Width = 316
        Height = 158
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Ajuda'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PageControl2: TPageControl
        Left = 1
        Top = 1
        Width = 324
        Height = 219
        ActivePage = TabSheet4
        Style = tsButtons
        TabOrder = 0
        object TabSheet6: TTabSheet
          Caption = 'atalhos'
          ImageIndex = 1
          TabVisible = False
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label12: TLabel
            Left = 14
            Top = 1
            Width = 3
            Height = 13
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object Memo1: TMemo
            Left = -1
            Top = 3
            Width = 315
            Height = 175
            BorderStyle = bsNone
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Lines.Strings = (
              '[CTRL]+[NUMLOCK] - Mostra/Esconde status do aim.'
              '[CTRL] + [ + ] - Mira no pr'#243'ximo alvo'
              '[CTRL] + [SHIFT] + [ + ] - Mira no alvo anterior'
              '[CTRL] + [ - ] -Troca para o pr'#243'ximo m'#243'bile'
              '[CTRL] + [SHIFT] + [ - ] - Troca para o m'#243'bile anterior'
              '[CTRL] + [ . ] - Pr'#243'ximo modo de mira'
              '[CTRL] + [SHIFT] + [ . ] - Modo de mira anterior'
              '[CTRL] + [ / ] - Liga modo autom'#225'tico de detec'#231#227'o do mobile'
              '[CTRL] + [ * ] - Liga/Desliga modo BackShot'
              '[CTRL] + [ENTER] - Liga/Desliga o aim'
              '[CTRL] + [X] - Seta linha do m'#243'dulo do tornado'
              '[CTRL] + [SHIFT] + [X] - Remove o modulo do tornado'
              '[ALT] + [X] - Mostra linha do m'#243'dulo do tornado na posi'#231#227'o '
              'do mouse'
              '[CTRL] + [ALT] + [X] - Remove todos os m'#243'dulos de tornado '
              'criados'
              '[CTRL]+[M] - Alterna entre os modos Tornado/Espelho'
              '[CTRL]+[P] - Atira usando a marca do aim como ponto de '
              'parada da for'#231'a (deve estar no modo slice)')
            ParentFont = False
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
        object TabSheet4: TTabSheet
          Caption = 'dicas'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Memo5: TMemo
            Left = 3
            Top = 3
            Width = 310
            Height = 174
            BorderStyle = bsNone
            Lines.Strings = (
              '     -=> Se estiver no modo MOUSE, pressione CTRL para '
              'parar a mira no local atual, para que possa atirar com o '
              'mouse ou mov'#234'-lo sem alterar a posi'#231#227'o da mira e '
              'quantidade de for'#231'a indicada pelo aim.')
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
        object TabSheet5: TTabSheet
          Caption = 'descri'#231#227'o'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Memo6: TMemo
            Left = 3
            Top = 3
            Width = 310
            Height = 174
            BorderStyle = bsNone
            Lines.Strings = (
              '          D'#225' uma no'#231#227'o de for'#231'a/distancia para um lan'#231'amento '
              'de proj'#233'til, com a'#231#227'o de for'#231'as externas, para GB. '#201' um '
              'aimbot longo alcance, ou seja, pode mirar de qualquer '
              'parte para qualquer outra parte do mapa.')
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
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
        OnClick = Button3Click
      end
    end
  end
  object ProcuraJanelaTimer: TTimer
    Enabled = False
    Interval = 3000
    OnTimer = ProcuraJanelaTimerTimer
    Left = 57
    Top = 186
  end
  object StatusTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = StatusTimerTimer
    Left = 184
    Top = 146
  end
  object AtualizaBotTimer: TTimer
    Enabled = False
    Interval = 60
    Left = 16
    Top = 146
  end
  object injetaTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = injetaTimerTimer
    Left = 137
    Top = 186
  end
  object SaveDialog1: TSaveDialog
    Left = 264
    Top = 168
  end
  object tornadoTimer: TTimer
    Enabled = False
    Interval = 100
    Left = 288
    Top = 202
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    Left = 274
    Top = 146
  end
  object Timer2: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer2Timer
    Left = 96
    Top = 144
  end
  object timerAutoShot: TTimer
    OnTimer = timerAutoShotTimer
    Left = 224
    Top = 184
  end
  object Timer4: TTimer
    Enabled = False
    Interval = 10
    Left = 296
    Top = 200
  end
end
