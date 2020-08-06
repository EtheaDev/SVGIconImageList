object MainForm: TMainForm
  Left = 916
  Top = 169
  Caption = 
    'SVG Icon ImageList Demo - Copyright (c) Ethea S.r.l. - Apache 2.' +
    '0 Open Source License'
  ClientHeight = 371
  ClientWidth = 502
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object CategoryButtons1: TCategoryButtons
    Left = 120
    Top = 119
    Width = 169
    Height = 187
    ButtonFlow = cbfVertical
    ButtonHeight = 25
    ButtonWidth = 25
    ButtonOptions = [boFullSize, boShowCaptions, boCaptionOnlyBorder]
    Categories = <
      item
        Caption = 'Category 1'
        Color = 15466474
        Collapsed = False
        Items = <
          item
            Caption = 'caption 1'
            ImageIndex = 0
          end
          item
            Caption = 'Caption 2'
            ImageIndex = 1
          end>
      end>
    Images = SVGIconImageList1
    RegularButtonColor = clWhite
    SelectedButtonColor = 15132390
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 8
    Top = 124
    Width = 102
    Height = 102
    Caption = 'Panel1'
    TabOrder = 1
    object SVGIconImage: TSVGIconImage
      Left = 1
      Top = 1
      Width = 100
      Height = 100
      Hint = 'Click to show SVGText property editor'
      AutoSize = False
      Center = False
      Proportional = True
      Stretch = True
      Opacity = 255
      Scale = 1.000000000000000000
      ImageList = SVGIconImageList1
      ImageIndex = 0
      Align = alClient
      OnClick = SVGIconImageClick
      ExplicitLeft = 0
      ExplicitTop = 10
      ExplicitWidth = 50
      ExplicitHeight = 50
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 502
    Height = 31
    AutoSize = True
    ButtonHeight = 31
    ButtonWidth = 32
    Caption = 'ToolBar1'
    Images = SVGIconImageList1
    TabOrder = 2
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Caption = 'ToolButton1'
      ImageIndex = 0
    end
    object ToolButton2: TToolButton
      Left = 32
      Top = 0
      Caption = 'ToolButton2'
      ImageIndex = 0
    end
    object ToolButton3: TToolButton
      Left = 64
      Top = 0
      Caption = 'ToolButton3'
      ImageIndex = 0
    end
    object ToolButton4: TToolButton
      Left = 96
      Top = 0
      Caption = 'ToolButton4'
      ImageIndex = 0
    end
  end
  object SVGIconImageList1: TSVGIconImageList
    Width = 25
    Height = 25
    Size = 25
    Left = 384
    Top = 160
    Images = {
      0200000000000000980000003C7376672076696577426F783D22302030203130
      30203130302220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F
      323030302F737667223E0D0A20203C636972636C652063783D22353022206379
      3D2235302220723D22343822207374726F6B653D22626C61636B22207374726F
      6B652D77696474683D2234222066696C6C3D2272656422202F3E0D0A3C2F7376
      673E0D0A00000000420200003C7376672076696577426F783D22302030203130
      30203130302220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F
      323030302F737667223E0D0A20203C212D2D204E6F207472616E736C6174696F
      6E202D2D3E0D0A20203C7265637420783D22352220793D223522207769647468
      3D22343022206865696768743D223430222066696C6C3D22677265656E22202F
      3E0D0A200D0A20203C212D2D20486F72697A6F6E74616C207472616E736C6174
      696F6E202D2D3E0D0A20203C7265637420783D22352220793D22352220776964
      74683D22343022206865696768743D223430222066696C6C3D22626C7565220D
      0A20202020202020207472616E73666F726D3D227472616E736C617465283530
      2922202F3E0D0A200D0A20203C212D2D20566572746963616C207472616E736C
      6174696F6E202D2D3E0D0A20203C7265637420783D22352220793D2235222077
      696474683D22343022206865696768743D223430222066696C6C3D2272656422
      0D0A20202020202020207472616E73666F726D3D227472616E736C6174652830
      2035302922202F3E0D0A200D0A20203C212D2D20426F746820686F72697A6F6E
      74616C20616E6420766572746963616C207472616E736C6174696F6E202D2D3E
      0D0A20203C7265637420783D22352220793D2235222077696474683D22343022
      206865696768743D223430222066696C6C3D2279656C6C6F77220D0A20202020
      20202020207472616E73666F726D3D227472616E736C6174652835302C353029
      22202F3E0D0A3C2F7376673E0D0A}
  end
end
