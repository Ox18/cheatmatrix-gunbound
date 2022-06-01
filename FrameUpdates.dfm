object frameUpd: TframeUpd
  Left = 0
  Top = 0
  Width = 330
  Height = 246
  Color = 14737632
  ParentColor = False
  TabOrder = 0
  TabStop = True
  object PageControl3: TPageControl
    Left = 0
    Top = -1
    Width = 330
    Height = 246
    ActivePage = TabSheet5
    TabOrder = 0
    object TabSheet5: TTabSheet
      Caption = 'Status'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pTotal: TGauge
        Left = 160
        Top = 77
        Width = 156
        Height = 13
        Color = clWhite
        ForeColor = 16744448
        MaxValue = 10000
        ParentColor = False
        Progress = 0
      end
      object pParcial: TGauge
        Left = 160
        Top = 60
        Width = 156
        Height = 13
        MaxValue = 10000
        Progress = 0
      end
      object GroupBox1: TGroupBox
        Left = 3
        Top = 0
        Width = 146
        Height = 57
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label3: TLabel
          Left = 30
          Top = 11
          Width = 26
          Height = 13
          Caption = 'Novo'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label4: TLabel
          Left = 30
          Top = 25
          Width = 33
          Height = 13
          Caption = 'Normal'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label5: TLabel
          Left = 30
          Top = 38
          Width = 67
          Height = 13
          Caption = 'Desatualizado'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Panel1: TPanel
          Left = 13
          Top = 12
          Width = 12
          Height = 11
          Color = clBlue
          TabOrder = 0
        end
        object Panel3: TPanel
          Left = 13
          Top = 40
          Width = 12
          Height = 11
          Color = clRed
          TabOrder = 1
        end
        object Panel2: TPanel
          Left = 13
          Top = 26
          Width = 12
          Height = 11
          Color = clBlack
          TabOrder = 2
        end
      end
      object GroupBox2: TGroupBox
        Left = 3
        Top = 60
        Width = 146
        Height = 105
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object Label6: TLabel
          Left = 12
          Top = 11
          Width = 92
          Height = 13
          Caption = 'Arquivos a instalar: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblToInstall: TLabel
          Left = 108
          Top = 12
          Width = 6
          Height = 13
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label7: TLabel
          Left = 12
          Top = 27
          Width = 87
          Height = 13
          Caption = 'Arquivos a baixar: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblToDown: TLabel
          Left = 108
          Top = 28
          Width = 6
          Height = 13
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label11: TLabel
          Left = 12
          Top = 65
          Width = 59
          Height = 13
          Caption = 'Velocidade: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblSpeed: TLabel
          Left = 76
          Top = 66
          Width = 6
          Height = 13
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label13: TLabel
          Left = 12
          Top = 81
          Width = 79
          Height = 13
          Caption = 'Temp Restante: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblTime: TLabel
          Left = 92
          Top = 82
          Width = 42
          Height = 13
          Caption = '00:00:00'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label8: TLabel
          Left = 12
          Top = 49
          Width = 92
          Height = 13
          Caption = 'Tamanho restante: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblTRestante: TLabel
          Left = 108
          Top = 50
          Width = 6
          Height = 13
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
      end
      object verDisp: TCheckBox
        Left = 2
        Top = 173
        Width = 152
        Height = 17
        Caption = 'Verificar apenas dispon'#237'veis'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
      object ListAllGames: TCheckBox
        Left = 2
        Top = 192
        Width = 177
        Height = 17
        Caption = 'Listar plugins p/ todos os jogos'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
      end
      object GameList: TComboBox
        Left = 159
        Top = 6
        Width = 156
        Height = 19
        Hint = 'Escolha o jogo para o qual deseja carregar a lista de hacks'
        Style = csOwnerDrawFixed
        BiDiMode = bdLeftToRight
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        ParentBiDiMode = False
        ParentFont = False
        TabOrder = 4
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Plugins'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PageControl2: TPageControl
        Left = 3
        Top = 3
        Width = 316
        Height = 212
        ActivePage = TabSheet4
        TabOrder = 0
        object TabSheet3: TTabSheet
          Caption = 'Todos'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          DesignSize = (
            308
            184)
          object lstPAll: TListView
            Left = 0
            Top = -2
            Width = 307
            Height = 189
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clWhite
            Columns = <
              item
                Caption = 'ID'
              end
              item
                Caption = 'Plugin'
              end
              item
                Caption = 'Arquivo'
              end
              item
                Caption = 'Jogo'
              end
              item
                Caption = 'Tamanho'
              end
              item
                Caption = 'Cria'#231#227'o'
              end
              item
                Caption = 'Autor'
              end
              item
                Caption = 'Status'
              end>
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            GridLines = True
            ParentFont = False
            TabOrder = 0
            ViewStyle = vsReport
            OnCustomDrawItem = lstCAllCustomDrawItem
            OnCustomDrawSubItem = lstCAllCustomDrawSubItem
          end
        end
        object TabSheet4: TTabSheet
          Caption = 'Dispon'#237'veis'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          DesignSize = (
            308
            184)
          object lstPDisp: TListView
            Left = 0
            Top = -2
            Width = 307
            Height = 189
            Anchors = [akLeft, akTop, akRight, akBottom]
            Checkboxes = True
            Color = clWhite
            Columns = <
              item
                Caption = 'ID'
              end
              item
                Caption = 'Plugin'
              end
              item
                Caption = 'Arquivo'
              end
              item
                Caption = 'Jogo'
              end
              item
                Caption = 'Tamanho'
              end
              item
                Caption = 'Cria'#231#227'o'
              end
              item
                Caption = 'Autor'
              end
              item
                Caption = 'Status'
              end>
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            GridLines = True
            ParentFont = False
            TabOrder = 0
            ViewStyle = vsReport
            OnCustomDrawItem = lstCAllCustomDrawItem
            OnCustomDrawSubItem = lstCAllCustomDrawSubItem
          end
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Controles'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PageControl1: TPageControl
        Left = 3
        Top = 3
        Width = 316
        Height = 212
        ActivePage = TabSheet7
        TabOrder = 0
        object TabSheet2: TTabSheet
          Caption = 'Todos'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object lstCAll: TListView
            Left = 0
            Top = -2
            Width = 307
            Height = 189
            Color = clWhite
            Columns = <
              item
                Caption = 'ID'
              end
              item
                Caption = 'Plugin'
              end
              item
                Caption = 'Arquivo'
              end
              item
                Caption = 'Jogo'
              end
              item
                Caption = 'Tamanho'
              end
              item
                Caption = 'Cria'#231#227'o'
              end
              item
                Caption = 'Autor'
              end
              item
                Caption = 'Status'
              end>
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            GridLines = True
            ParentFont = False
            TabOrder = 0
            ViewStyle = vsReport
            OnCustomDrawItem = lstCAllCustomDrawItem
            OnCustomDrawSubItem = lstCAllCustomDrawSubItem
          end
        end
        object TabSheet7: TTabSheet
          Caption = 'Dispon'#237'veis'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object lstCDisp: TListView
            Left = 0
            Top = -2
            Width = 307
            Height = 189
            Checkboxes = True
            Color = clWhite
            Columns = <
              item
                Caption = 'ID'
              end
              item
                Caption = 'Plugin'
              end
              item
                Caption = 'Arquivo'
              end
              item
                Caption = 'Jogo'
              end
              item
                Caption = 'Tamanho'
              end
              item
                Caption = 'Cria'#231#227'o'
              end
              item
                Caption = 'Autor'
              end
              item
                Caption = 'Status'
              end>
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            GridLines = True
            ParentFont = False
            TabOrder = 0
            ViewStyle = vsReport
            OnCustomDrawItem = lstCAllCustomDrawItem
            OnCustomDrawSubItem = lstCAllCustomDrawSubItem
          end
        end
      end
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 152
    Top = 8
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 152
    Top = 40
  end
end
