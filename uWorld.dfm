object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Big Worm Game'
  ClientHeight = 289
  ClientWidth = 453
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object backgroud: TImage
    Left = 0
    Top = 0
    Width = 453
    Height = 289
    Align = alClient
    Enabled = False
    Transparent = True
    ExplicitLeft = 112
    ExplicitTop = 56
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 16
    Top = 8
  end
  object Timer2: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = Timer2Timer
    Left = 72
    Top = 8
  end
end
