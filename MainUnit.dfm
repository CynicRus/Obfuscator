object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Simba Script Obfuscator'
  ClientHeight = 427
  ClientWidth = 679
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 37
    Width = 37
    Height = 13
    Caption = 'Source:'
  end
  object Label2: TLabel
    Left = 343
    Top = 37
    Width = 89
    Height = 13
    Caption = 'Obfuscated script:'
  end
  object Button1: TButton
    Left = 8
    Top = 6
    Width = 87
    Height = 25
    Caption = 'Obfuscate'
    TabOrder = 0
    OnClick = Button1Click
  end
  object SynEdit1: TSynEdit
    Left = 8
    Top = 56
    Width = 329
    Height = 363
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Highlighter = SynPasSyn1
    WantTabs = True
    WordWrap = True
  end
  object SynEdit2: TSynEdit
    Left = 343
    Top = 56
    Width = 329
    Height = 363
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Gutter.Visible = False
    Gutter.Width = 0
    Highlighter = SynPasSyn1
    ReadOnly = True
    WantTabs = True
    WordWrap = True
  end
  object Button2: TButton
    Left = 182
    Top = 6
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 101
    Top = 6
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 4
    OnClick = Button3Click
  end
  object SynPasSyn1: TSynPasSyn
    CommentAttri.Foreground = clGreen
    DirectiveAttri.Foreground = clTeal
    DirectiveAttri.Style = []
    NumberAttri.Foreground = clBlue
    FloatAttri.Foreground = clBlue
    HexAttri.Foreground = clBlue
    StringAttri.Foreground = clGray
    CharAttri.Foreground = clHotLight
    DelphiVersion = dvDelphi8
    Left = 272
    Top = 192
  end
  object OpenDialog1: TOpenDialog
    Left = 496
    Top = 208
  end
  object SaveDialog1: TSaveDialog
    Left = 552
    Top = 224
  end
end
