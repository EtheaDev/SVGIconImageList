object ExportToPNGDialog: TExportToPNGDialog
  Left = 132
  Top = 168
  BorderStyle = bsDialog
  Caption = 'Export SVG to PNG files'
  ClientHeight = 279
  ClientWidth = 454
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 13
  object OutputFileNameLabel: TLabel
    Left = 247
    Top = 187
    Width = 125
    Height = 13
    Caption = 'Output filename format:'
  end
  object ExampleLabel: TLabel
    Left = 24
    Top = 214
    Width = 133
    Height = 13
    Caption = 'Output filename Example:'
  end
  object ExampleFileName: TLabel
    Left = 24
    Top = 229
    Width = 46
    Height = 13
    Caption = 'Filename'
  end
  object SVGIconImage: TSVGIconImage
    Left = 24
    Top = 8
    Width = 200
    Height = 200
    AutoSize = False
  end
  object FSearchOptions: TGroupBox
    Left = 247
    Top = 8
    Width = 202
    Height = 145
    Caption = 'Image Size'
    TabOrder = 0
    object Export16x16: TCheckBox
      Left = 16
      Top = 17
      Width = 86
      Height = 17
      Caption = '16x16'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBoxClick
    end
    object Export32x32: TCheckBox
      Left = 107
      Top = 17
      Width = 86
      Height = 17
      Caption = '32x32'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBoxClick
    end
    object Export48x48: TCheckBox
      Left = 16
      Top = 40
      Width = 86
      Height = 17
      Caption = '48x48'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBoxClick
    end
    object Export64x64: TCheckBox
      Left = 107
      Top = 40
      Width = 86
      Height = 17
      Caption = '64x64'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBoxClick
    end
    object Export96x96: TCheckBox
      Left = 16
      Top = 63
      Width = 86
      Height = 17
      Caption = '96x96'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = CheckBoxClick
    end
    object Export128x128: TCheckBox
      Left = 107
      Top = 63
      Width = 86
      Height = 17
      Caption = '128x128'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = CheckBoxClick
    end
    object Export192x192: TCheckBox
      Left = 16
      Top = 86
      Width = 86
      Height = 17
      Caption = '192x192'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = CheckBoxClick
    end
    object Export256x256: TCheckBox
      Left = 107
      Top = 86
      Width = 86
      Height = 17
      Caption = '256x256'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = CheckBoxClick
    end
    object ExportCustom: TCheckBox
      Left = 16
      Top = 112
      Width = 86
      Height = 17
      Caption = 'Custom size:'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = CheckBoxClick
    end
    object CustomSizeEdit: TEdit
      Left = 107
      Top = 109
      Width = 57
      Height = 21
      NumbersOnly = True
      TabOrder = 9
      Text = '512'
    end
  end
  object btnOK: TButton
    Left = 295
    Top = 248
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 373
    Top = 248
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
    OnClick = btnCancelClick
  end
  object FormatEdit: TEdit
    Left = 247
    Top = 204
    Width = 201
    Height = 21
    TabOrder = 2
    Text = '%FileName%_%size%'
    OnChange = FormatEditChange
  end
  object OutputButton: TButton
    Left = 24
    Top = 248
    Width = 169
    Height = 23
    Caption = 'Change output filename...'
    TabOrder = 3
    OnClick = OutputButtonClick
  end
  object BtnCopyToClipboard: TButton
    Left = 317
    Top = 159
    Width = 131
    Height = 25
    Caption = 'Copy to clipboard...'
    TabOrder = 1
    OnClick = BtnCopyToClipboardClick
  end
  object SavePNGDialog: TSaveDialog
    Filter = 'PNG Image files (.png)|*.png'
    Left = 92
    Top = 65
  end
end
