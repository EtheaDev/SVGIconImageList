object SVGIconSetForm: TSVGIconSetForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Set property'
  ClientHeight = 148
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  ShowHint = True
  OnCreate = FormCreate
  TextHeight = 15
  object CategoryGroupBox: TGroupBox
    Left = 0
    Top = 0
    Width = 394
    Height = 50
    Align = alTop
    Caption = 'Category'
    TabOrder = 0
    Visible = False
    object CategoryNameLabel: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 23
      Width = 45
      Height = 22
      Margins.Top = 6
      Align = alLeft
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Name:'
      Transparent = True
    end
    object CategoryEdit: TEdit
      AlignWithMargins = True
      Left = 56
      Top = 21
      Width = 333
      Height = 23
      Margins.Top = 4
      Margins.Bottom = 4
      Align = alClient
      TabOrder = 0
      TextHint = 'Category name'
      OnChange = CategoryEditChange
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 110
    Width = 394
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OKButton: TButton
      AlignWithMargins = True
      Left = 214
      Top = 6
      Width = 85
      Height = 26
      Margins.Left = 2
      Margins.Top = 6
      Margins.Right = 2
      Margins.Bottom = 6
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelButton: TButton
      AlignWithMargins = True
      Left = 303
      Top = 6
      Width = 85
      Height = 26
      Margins.Left = 2
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object PngGroupBox: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 53
    Width = 388
    Height = 49
    Align = alTop
    Caption = 'Png export: size in pixel'
    TabOrder = 2
    Visible = False
    object PngHeightLabel: TLabel
      AlignWithMargins = True
      Left = 237
      Top = 23
      Width = 60
      Height = 21
      Margins.Top = 6
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Height:'
      Transparent = True
    end
    object PngWidthLabel: TLabel
      AlignWithMargins = True
      Left = 121
      Top = 23
      Width = 60
      Height = 21
      Margins.Top = 6
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Width:'
      Transparent = True
    end
    object SizeLabel: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 23
      Width = 60
      Height = 21
      Margins.Top = 6
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Size:'
      Transparent = True
    end
    object PngWidthEdit: TSpinEdit
      AlignWithMargins = True
      Left = 187
      Top = 20
      Width = 44
      Height = 24
      MaxValue = 255
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
    object PngHeightEdit: TSpinEdit
      AlignWithMargins = True
      Left = 303
      Top = 20
      Width = 44
      Height = 24
      MaxValue = 255
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object PngSizeEdit: TSpinEdit
      AlignWithMargins = True
      Left = 71
      Top = 20
      Width = 44
      Height = 24
      MaxValue = 255
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnChange = PngSizeEditChange
    end
  end
end
