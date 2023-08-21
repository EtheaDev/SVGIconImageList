object FrameView: TFrameView
  Left = 0
  Top = 0
  Width = 510
  Height = 432
  TabOrder = 0
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 510
    Height = 432
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    object SVGPaintBox: TPaintBox
      Left = 1
      Top = 24
      Width = 508
      Height = 407
      Align = alClient
      Color = clWhite
      ParentColor = False
      OnPaint = SVGPaintBoxPaint
      ExplicitLeft = 145
      ExplicitTop = 129
      ExplicitWidth = 300
      ExplicitHeight = 300
    end
    object TitlePanel: TPanel
      Left = 1
      Top = 1
      Width = 508
      Height = 23
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Title'
      Color = clHighlight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlightText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      StyleElements = [seBorder]
    end
  end
end
