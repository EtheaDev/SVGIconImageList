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
    BevelOuter = bvLowered
    BorderWidth = 1
    TabOrder = 0
    object SVGPaintBox: TPaintBox
      Left = 2
      Top = 25
      Width = 506
      Height = 405
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
      Left = 2
      Top = 2
      Width = 506
      Height = 23
      Align = alTop
      Caption = 'Title'
      TabOrder = 0
    end
  end
end
