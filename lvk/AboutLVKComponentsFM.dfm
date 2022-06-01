object fmAboutLVKComponents: TfmAboutLVKComponents
  Left = 442
  Top = 385
  BorderStyle = bsDialog
  Caption = 'About LVK Components for Delphi 5 and 6, version %s'
  ClientHeight = 139
  ClientWidth = 406
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object paBottom: TPanel
    Left = 0
    Top = 107
    Width = 406
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btOK: TButton
      Left = 60
      Top = 4
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = btOKClick
    end
  end
  object AboutConsole: TlvkConsole
    Left = 0
    Top = 0
    Width = 406
    Height = 107
    Options = [coBlinking, coSelection, coAutoCopy, coAutoOpenURLLinks]
    Edge = etNone
    Links.Font.Charset = DEFAULT_CHARSET
    Links.Font.Color = clBlue
    Links.Font.Height = -11
    Links.Font.Name = 'MS Sans Serif'
    Links.Font.Style = []
    Links.HoverFont.Charset = DEFAULT_CHARSET
    Links.HoverFont.Color = clBlue
    Links.HoverFont.Height = -11
    Links.HoverFont.Name = 'MS Sans Serif'
    Links.HoverFont.Style = [fsUnderline]
    PreFont.Charset = DEFAULT_CHARSET
    PreFont.Color = clWindowText
    PreFont.Height = -11
    PreFont.Name = 'MS Sans Serif'
    PreFont.Style = []
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Color = clBtnFace
  end
end
