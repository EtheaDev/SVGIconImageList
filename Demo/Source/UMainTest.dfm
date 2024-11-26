object MainForm: TMainForm
  Left = 916
  Top = 169
  Caption = 
    'SVG Icon ImageList Demo - Copyright (c) Ethea S.r.l. - Apache 2.' +
    '0 Open Source License'
  ClientHeight = 340
  ClientWidth = 739
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  DesignSize = (
    739
    340)
  TextHeight = 13
  object SVGIconImage: TSVGIconImage
    Left = 224
    Top = 55
    Width = 201
    Height = 187
    Hint = 'Click to show SVGText property editor'
    AutoSize = False
    Center = False
    ImageList = SVGIconImageList1
    ImageIndex = 0
    OnClick = SVGIconImageClick
  end
  object PaintBox1: TPaintBox
    Left = 447
    Top = 37
    Width = 279
    Height = 293
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnPaint = PaintBox1Paint
    ExplicitWidth = 274
    ExplicitHeight = 290
  end
  object CategoryButtons1: TCategoryButtons
    Left = 32
    Top = 55
    Width = 169
    Height = 187
    ButtonFlow = cbfVertical
    ButtonHeight = 50
    ButtonWidth = 40
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
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 739
    Height = 31
    AutoSize = True
    ButtonHeight = 31
    ButtonWidth = 32
    Caption = 'ToolBar1'
    Images = SVGIconImageList1
    TabOrder = 1
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
    Size = 25
    SVGIconItems = <
      item
        SVGText = 
          '<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">'#13#10 +
          '  <circle cx="50" cy="50" r="48" stroke="black" stroke-width="4"' +
          ' fill="red" />'#13#10'</svg>'#13#10
      end
      item
        SVGText = 
          '<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">'#13#10 +
          '  <!-- No translation -->'#13#10'  <rect x="5" y="5" width="40" height' +
          '="40" fill="green" />'#13#10' '#13#10'  <!-- Horizontal translation -->'#13#10'  <' +
          'rect x="5" y="5" width="40" height="40" fill="blue"'#13#10'        tra' +
          'nsform="translate(50)" />'#13#10' '#13#10'  <!-- Vertical translation -->'#13#10' ' +
          ' <rect x="5" y="5" width="40" height="40" fill="red"'#13#10'        tr' +
          'ansform="translate(0 50)" />'#13#10' '#13#10'  <!-- Both horizontal and vert' +
          'ical translation -->'#13#10'  <rect x="5" y="5" width="40" height="40"' +
          ' fill="yellow"'#13#10'         transform="translate(50,50)" />'#13#10'</svg>' +
          #13#10
      end>
    Scaled = True
    Left = 384
    Top = 160
  end
end
