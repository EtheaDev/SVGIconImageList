object FrameView: TFrameView
  Left = 0
  Top = 0
  Width = 510
  Height = 432
  TabOrder = 0
  object SVGPaintBox: TPaintBox
    Left = 0
    Top = 25
    Width = 510
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
    Left = 0
    Top = 0
    Width = 510
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Title'
    TabOrder = 0
  end
end
