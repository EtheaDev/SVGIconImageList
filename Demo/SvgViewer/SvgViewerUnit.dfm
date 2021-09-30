object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'SVG Preview & Engine Comparison'
  ClientHeight = 604
  ClientWidth = 805
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object RightPanel: TPanel
    Left = 624
    Top = 0
    Width = 181
    Height = 604
    Align = alRight
    TabOrder = 0
    object ListBox: TListBox
      Left = 1
      Top = 42
      Width = 179
      Height = 561
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBoxClick
    end
    object OpenPanel: TPanel
      Left = 1
      Top = 1
      Width = 179
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object OpenButton: TButton
        Left = 4
        Top = 10
        Width = 75
        Height = 25
        Caption = 'Open...'
        TabOrder = 0
        OnClick = OpenButtonClick
      end
      object SetPathButton: TButton
        Left = 85
        Top = 10
        Width = 75
        Height = 25
        Caption = 'Set Path...'
        TabOrder = 1
        OnClick = SetPathButtonClick
      end
    end
  end
  object LeftPanel: TPanel
    Left = 0
    Top = 0
    Width = 313
    Height = 604
    Align = alLeft
    TabOrder = 1
    inline FrameViewSkia: TFrameView
      Left = 1
      Top = 301
      Width = 311
      Height = 302
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = 301
      ExplicitWidth = 311
      ExplicitHeight = 302
      inherited ClientPanel: TPanel
        Width = 311
        Height = 302
        ExplicitWidth = 311
        ExplicitHeight = 302
        inherited SVGPaintBox: TPaintBox
          Width = 307
          Height = 275
          ExplicitWidth = 311
          ExplicitHeight = 273
        end
        inherited TitlePanel: TPanel
          Width = 307
          ExplicitWidth = 307
        end
      end
    end
    inline FrameViewTSVG: TFrameView
      Left = 1
      Top = 1
      Width = 311
      Height = 300
      Align = alTop
      TabOrder = 1
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 311
      ExplicitHeight = 300
      inherited ClientPanel: TPanel
        Width = 311
        Height = 300
        ExplicitWidth = 311
        ExplicitHeight = 300
        inherited SVGPaintBox: TPaintBox
          Width = 307
          Height = 273
          ExplicitWidth = 303
          ExplicitHeight = 275
        end
        inherited TitlePanel: TPanel
          Width = 307
          ExplicitWidth = 307
        end
      end
    end
  end
  object ClientPanel: TPanel
    Left = 313
    Top = 0
    Width = 311
    Height = 604
    Align = alClient
    TabOrder = 2
    inline FrameViewImage32: TFrameView
      Left = 1
      Top = 301
      Width = 309
      Height = 302
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = 301
      ExplicitWidth = 309
      ExplicitHeight = 302
      inherited ClientPanel: TPanel
        Width = 309
        Height = 302
        ExplicitWidth = 309
        ExplicitHeight = 302
        inherited SVGPaintBox: TPaintBox
          Width = 305
          Height = 275
          ExplicitWidth = 303
          ExplicitHeight = 273
        end
        inherited TitlePanel: TPanel
          Width = 305
          ExplicitWidth = 305
        end
      end
    end
    inline FrameViewerD2D: TFrameView
      Left = 1
      Top = 1
      Width = 309
      Height = 300
      Align = alTop
      TabOrder = 1
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 309
      ExplicitHeight = 300
      inherited ClientPanel: TPanel
        Width = 309
        Height = 300
        ExplicitWidth = 309
        ExplicitHeight = 300
        inherited SVGPaintBox: TPaintBox
          Width = 305
          Height = 273
          ExplicitWidth = 303
          ExplicitHeight = 275
        end
        inherited TitlePanel: TPanel
          Width = 305
          ExplicitWidth = 305
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 456
    Top = 120
  end
end
