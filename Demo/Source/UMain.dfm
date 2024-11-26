object MainForm: TMainForm
  Left = 916
  Top = 169
  Caption = 
    'SVG Icon ImageList Demo - Copyright (c) Ethea S.r.l. - Apache 2.' +
    '0 Open Source License'
  ClientHeight = 547
  ClientWidth = 709
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  DoubleBuffered = True
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
  object Splitter: TSplitter
    Left = 626
    Top = 38
    Height = 509
    Align = alRight
    AutoSnap = False
    MinSize = 80
    ExplicitLeft = 9
    ExplicitTop = 9
    ExplicitHeight = 427
  end
  object Panel1: TPanel
    Left = 0
    Top = 38
    Width = 202
    Height = 509
    Align = alLeft
    TabOrder = 0
    object SelectThemeRadioGroup: TRadioGroup
      Left = 1
      Top = 1
      Width = 200
      Height = 272
      Align = alClient
      Caption = 'Select Theme/Color'
      TabOrder = 0
      OnClick = SelectThemeRadioGroupClick
    end
    object LoadGroupBox: TGroupBox
      Left = 1
      Top = 273
      Width = 200
      Height = 59
      Align = alBottom
      Caption = 'Load SVG from disk'
      TabOrder = 1
      object BuildFromFilesButton: TButton
        Left = 3
        Top = 23
        Width = 189
        Height = 30
        Caption = 'Build from SVG files...'
        TabOrder = 0
        OnClick = BuildFromFilesButtonClick
      end
    end
    object SliderPanel: TPanel
      Left = 1
      Top = 373
      Width = 200
      Height = 62
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      object IconSizeLabel: TLabel
        Left = 8
        Top = 3
        Width = 51
        Height = 13
        Caption = 'Icons size:'
      end
      object TrackBar: TTrackBar
        Left = 0
        Top = 23
        Width = 200
        Height = 39
        Align = alBottom
        Max = 128
        Min = 12
        Frequency = 8
        Position = 32
        PositionToolTip = ptBottom
        TabOrder = 0
        OnChange = TrackBarChange
      end
    end
    object ButtonsPanel: TPanel
      Left = 1
      Top = 332
      Width = 200
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object ClearButton: TButton
        Left = 5
        Top = 5
        Width = 76
        Height = 30
        Caption = 'Clear Icons'
        TabOrder = 0
        OnClick = ClearButtonClick
      end
      object ShowImageEditorButton: TButton
        Left = 86
        Top = 5
        Width = 106
        Height = 30
        Caption = 'Show Image Editor'
        TabOrder = 1
        OnClick = ShowImageEditorButtonClick
      end
    end
    object ColorGroupBox: TGroupBox
      Left = 1
      Top = 435
      Width = 200
      Height = 73
      Align = alBottom
      Caption = 'Fixed color'
      TabOrder = 4
      object FixedColorComboBox: TColorBox
        Left = 10
        Top = 19
        Width = 178
        Height = 22
        DefaultColorColor = clDefault
        NoneColorColor = clNone
        Selected = clDefault
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 0
        OnSelect = FixedColorComboBoxSelect
      end
      object GrayScaleCheckBox: TCheckBox
        Left = 10
        Top = 50
        Width = 97
        Height = 17
        Caption = 'GrayScale'
        TabOrder = 1
        OnClick = GrayScaleCheckBoxClick
      end
    end
  end
  object TopToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 709
    Height = 38
    AutoSize = True
    ButtonHeight = 38
    ButtonWidth = 39
    Images = SVGIconImageList
    TabOrder = 1
    Transparent = False
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = DisabledAction
    end
    object ToolButton2: TToolButton
      Left = 39
      Top = 0
      Action = DeleteIconAction
    end
    object ToolButton3: TToolButton
      Left = 78
      Top = 0
      ImageIndex = 2
    end
    object ToolButton4: TToolButton
      Left = 117
      Top = 0
      ImageIndex = 3
    end
    object ToolButton5: TToolButton
      Left = 156
      Top = 0
      ImageIndex = 4
    end
    object ToolButton6: TToolButton
      Left = 195
      Top = 0
      Action = ChangeIconAction
    end
    object ToolButton7: TToolButton
      Left = 234
      Top = 0
      Caption = 'Change Color'
      Enabled = False
      ImageIndex = 7
      OnClick = ChangeColorActionExecute
    end
  end
  object paButtons: TPanel
    Left = 629
    Top = 38
    Width = 80
    Height = 509
    Align = alRight
    TabOrder = 2
    OnResize = paButtonsResize
    object SVGIconImage: TSVGIconImage
      Left = 1
      Top = 428
      Width = 78
      Height = 80
      Hint = 'Click left - right mouse button to change icon into SVGIconImage'
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      AutoSize = False
      ImageList = SVGIconImageList
      ImageIndex = 100
      Align = alBottom
      OnMouseDown = SVGIconImageMouseDown
    end
    object DeleteButton: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 72
      Height = 60
      Action = DeleteIconAction
      Align = alTop
      ImageAlignment = iaTop
      Images = SVGIconImageList
      TabOrder = 0
    end
    object ChangeIconButton: TButton
      AlignWithMargins = True
      Left = 4
      Top = 70
      Width = 72
      Height = 60
      Action = ChangeIconAction
      Align = alTop
      ImageAlignment = iaTop
      Images = SVGIconImageList
      TabOrder = 1
    end
    object NewFormButton: TButton
      AlignWithMargins = True
      Left = 4
      Top = 136
      Width = 72
      Height = 60
      Action = NewFormAction
      Align = alTop
      ImageAlignment = iaTop
      Images = SVGIconImageList
      TabOrder = 2
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 4
      Top = 202
      Width = 72
      Height = 60
      Action = DisabledAction
      Align = alTop
      ImageAlignment = iaTop
      Images = SVGIconImageList
      TabOrder = 3
    end
  end
  object ClientPanel: TPanel
    Left = 202
    Top = 38
    Width = 424
    Height = 509
    Align = alClient
    TabOrder = 3
    object ImageListLabel: TLabel
      Left = 1
      Top = 209
      Width = 422
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = 'Image List Icons Preview'
      ExplicitWidth = 119
    end
    object TreeView: TTreeView
      Left = 1
      Top = 1
      Width = 422
      Height = 208
      Align = alTop
      Images = SVGIconImageList
      Indent = 35
      TabOrder = 0
      Items.NodeData = {
        0303000000240000000100000001000000FFFFFFFFFFFFFFFF00000000000000
        000100000001036F006E0065002C0000000400000004000000FFFFFFFFFFFFFF
        FF00000000000000000000000001076F006E0065002D006F006E006500240000
        000200000002000000FFFFFFFFFFFFFFFF000000000000000002000000010374
        0077006F002C0000000500000005000000FFFFFFFFFFFFFFFF00000000000000
        00000000000107740077006F0020006F006E0065002C00000006000000070000
        0000000000FFFFFFFF0000000000000000000000000107740077006F00200074
        0077006F00280000000300000003000000FFFFFFFFFFFFFFFF00000000000000
        0000000000010574006800720065006500}
    end
    object ImageView: TListView
      Left = 1
      Top = 222
      Width = 422
      Height = 286
      Align = alClient
      Columns = <>
      IconOptions.AutoArrange = True
      Items.ItemData = {
        05CC0000000500000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
        00034D0061006E0001000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00000000
        0557006F006D0061006E0002000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00
        00000008430061006C0065006E0064006100720003000000FFFFFFFFFFFFFFFF
        00000000FFFFFFFF000000000B49006E0066006F0072006D006100740069006F
        006E0004000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000000A43006100
        6C00630075006C00610074006F007200}
      LargeImages = SVGIconImageList
      SmallImages = SVGIconImageList
      TabOrder = 1
      OnSelectItem = ImageViewSelectItem
      ExplicitLeft = -1
      ExplicitTop = 228
    end
  end
  object ActionList: TActionList
    Images = SVGIconImageList
    Left = 248
    Top = 424
    object ChangeIconAction: TAction
      Category = 'Edit'
      Caption = 'Change icon'
      ImageIndex = 255
      OnExecute = ChangeIconActionExecute
    end
    object DeleteIconAction: TAction
      Category = 'Edit'
      Caption = 'Delete Icon'
      ImageIndex = 39
      OnExecute = DeleteIconActionExecute
    end
    object DisabledAction: TAction
      Category = 'Edit'
      Caption = 'Disabled'
      Enabled = False
      ImageIndex = 0
    end
    object NewFormAction: TAction
      Category = 'View'
      Caption = 'New Form'
      ImageIndex = 107
      OnExecute = NewFormActionExecute
    end
  end
  object ColorDialog: TColorDialog
    Left = 472
    Top = 136
  end
  object OpenDialog: TOpenPictureDialog
    Filter = 'Scalable Vector Graphics (*.svg)|*.svg'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 472
    Top = 72
  end
  object tmrTrackbar: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tmrTrackbarTimer
    Left = 250
    Top = 486
  end
  object SVGIconImageList: TSVGIconImageList
    Size = 32
    SVGIconItems = <
      item
        IconName = 'Business\businessman'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'FF9800" points="24,37 19,31 19,25 29,25 29,31"/>'#13#10'    <g fill="#' +
          'FFA726">'#13#10'        <circle cx="33" cy="19" r="2"/>'#13#10'        <circ' +
          'le cx="15" cy="19" r="2"/>'#13#10'    </g>'#13#10'    <path fill="#FFB74D" d' +
          '="M33,13c0-7.6-18-5-18,0c0,1.1,0,5.9,0,7c0,5,4,9,9,9s9-4,9-9C33,' +
          '18.9,33,14.1,33,13z"/>'#13#10'    <path fill="#424242" d="M24,4c-6.1,0' +
          '-10,4.9-10,11c0,0.8,0,2.3,0,2.3l2,1.7v-5l12-4l4,4v5l2-1.7c0,0,0-' +
          '1.5,0-2.3c0-4-1-8-6-9l-1-2 H24z"/>'#13#10'    <g fill="#784719">'#13#10'    ' +
          '    <circle cx="28" cy="19" r="1"/>'#13#10'        <circle cx="20" cy=' +
          '"19" r="1"/>'#13#10'    </g>'#13#10'    <polygon fill="#fff" points="24,43 1' +
          '9,31 24,32 29,31"/>'#13#10'    <polygon fill="#D32F2F" points="23,35 2' +
          '2.3,39.5 24,43.5 25.7,39.5 25,35 26,34 24,32 22,34"/>'#13#10'    <path' +
          ' fill="#546E7A" d="M29,31L29,31l-5,12l-5-12c0,0-11,2-11,13h32C40' +
          ',33,29,31,29,31z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Business\businesswoman'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="16" y="' +
          '15" fill="#BF360C" width="16" height="18"/>'#13#10'    <path fill="#78' +
          '909C" d="M40,44H8c0-11,11-13,11-13h10C29,31,40,33,40,44z"/>'#13#10'   ' +
          ' <path fill="#FF9800" d="M24,37c-2.2,0-5-6-5-6v-6h10v6C29,31,26.' +
          '2,37,24,37z"/>'#13#10'    <path fill="#FFB74D" d="M33,14c0-7.6-18-5-18' +
          ',0c0,1.1,0,5.9,0,7c0,5,4,9,9,9s9-4,9-9C33,19.9,33,15.1,33,14z"/>' +
          #13#10'    <path fill="#FF5722" d="M24,4C17.9,4,9,7.4,9,27.3l7,4.7V19' +
          'l12-7l4,5v15l7-6c0-4-0.7-20-11-20l-1-2H24z"/>'#13#10'    <path fill="#' +
          'FFB74D" d="M24,38c-4.4,0-5-7-5-7s2.5,4,5,4s5-4,5-4S28.4,38,24,38' +
          'z"/>'#13#10'    <circle fill="#784719" cx="28" cy="21" r="1"/>'#13#10'    <c' +
          'ircle fill="#784719" cx="20" cy="21" r="1"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'calendar'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#CFD' +
          '8DC" d="M5,38V14h38v24c0,2.2-1.8,4-4,4H9C6.8,42,5,40.2,5,38z"/>'#13 +
          #10'    <path fill="#F44336" d="M43,10v6H5v-6c0-2.2,1.8-4,4-4h30C41' +
          '.2,6,43,7.8,43,10z"/>'#13#10'    <g fill="#B71C1C">'#13#10'        <circle c' +
          'x="33" cy="10" r="3"/>'#13#10'        <circle cx="15" cy="10" r="3"/>'#13 +
          #10'    </g>'#13#10'    <g fill="#B0BEC5">'#13#10'        <path d="M33,3c-1.1,0' +
          '-2,0.9-2,2v5c0,1.1,0.9,2,2,2s2-0.9,2-2V5C35,3.9,34.1,3,33,3z"/>'#13 +
          #10'        <path d="M15,3c-1.1,0-2,0.9-2,2v5c0,1.1,0.9,2,2,2s2-0.9' +
          ',2-2V5C17,3.9,16.1,3,15,3z"/>'#13#10'    </g>'#13#10'    <g fill="#90A4AE">'#13 +
          #10'        <rect x="13" y="20" width="4" height="4"/>'#13#10'        <re' +
          'ct x="19" y="20" width="4" height="4"/>'#13#10'        <rect x="25" y=' +
          '"20" width="4" height="4"/>'#13#10'        <rect x="31" y="20" width="' +
          '4" height="4"/>'#13#10'        <rect x="13" y="26" width="4" height="4' +
          '"/>'#13#10'        <rect x="19" y="26" width="4" height="4"/>'#13#10'       ' +
          ' <rect x="25" y="26" width="4" height="4"/>'#13#10'        <rect x="31' +
          '" y="26" width="4" height="4"/>'#13#10'        <rect x="13" y="32" wid' +
          'th="4" height="4"/>'#13#10'        <rect x="19" y="32" width="4" heigh' +
          't="4"/>'#13#10'        <rect x="25" y="32" width="4" height="4"/>'#13#10'   ' +
          '     <rect x="31" y="32" width="4" height="4"/>'#13#10'    </g>'#13#10'</svg' +
          '>'#13#10
      end
      item
        IconName = 'about'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#219' +
          '6F3" d="M37,40H11l-6,6V12c0-3.3,2.7-6,6-6h26c3.3,0,6,2.7,6,6v22C' +
          '43,37.3,40.3,40,37,40z"/>'#13#10'    <g fill="#fff">'#13#10'        <rect x=' +
          '"22" y="20" width="4" height="11"/>'#13#10'        <circle cx="24" cy=' +
          '"15" r="2"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'calculator'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#616' +
          '161" d="M40,16H8v24c0,2.2,1.8,4,4,4h24c2.2,0,4-1.8,4-4V16z"/>'#13#10' ' +
          '   <path fill="#424242" d="M36,4H12C9.8,4,8,5.8,8,8v9h32V8C40,5.' +
          '8,38.2,4,36,4z"/>'#13#10'    <path fill="#9CCC65" d="M36,14H12c-0.6,0-' +
          '1-0.4-1-1V8c0-0.6,0.4-1,1-1h24c0.6,0,1,0.4,1,1v5C37,13.6,36.6,14' +
          ',36,14z"/>'#13#10'    <g fill="#33691E">'#13#10'        <rect x="33" y="10" ' +
          'width="2" height="2"/>'#13#10'        <rect x="29" y="10" width="2" he' +
          'ight="2"/>'#13#10'    </g>'#13#10'    <path fill="#FF5252" d="M36,23h-3c-0.6' +
          ',0-1-0.4-1-1v-2c0-0.6,0.4-1,1-1h3c0.6,0,1,0.4,1,1v2C37,22.6,36.6' +
          ',23,36,23z"/>'#13#10'    <g fill="#E0E0E0">'#13#10'        <path d="M15,23h-' +
          '3c-0.6,0-1-0.4-1-1v-2c0-0.6,0.4-1,1-1h3c0.6,0,1,0.4,1,1v2C16,22.' +
          '6,15.6,23,15,23z"/>'#13#10'        <path d="M22,23h-3c-0.6,0-1-0.4-1-1' +
          'v-2c0-0.6,0.4-1,1-1h3c0.6,0,1,0.4,1,1v2C23,22.6,22.6,23,22,23z"/' +
          '>'#13#10'        <path d="M29,23h-3c-0.6,0-1-0.4-1-1v-2c0-0.6,0.4-1,1-' +
          '1h3c0.6,0,1,0.4,1,1v2C30,22.6,29.6,23,29,23z"/>'#13#10'        <path d' +
          '="M15,29h-3c-0.6,0-1-0.4-1-1v-2c0-0.6,0.4-1,1-1h3c0.6,0,1,0.4,1,' +
          '1v2C16,28.6,15.6,29,15,29z"/>'#13#10'        <path d="M22,29h-3c-0.6,0' +
          '-1-0.4-1-1v-2c0-0.6,0.4-1,1-1h3c0.6,0,1,0.4,1,1v2C23,28.6,22.6,2' +
          '9,22,29z"/>'#13#10'        <path d="M29,29h-3c-0.6,0-1-0.4-1-1v-2c0-0.' +
          '6,0.4-1,1-1h3c0.6,0,1,0.4,1,1v2C30,28.6,29.6,29,29,29z"/>'#13#10'     ' +
          '   <path d="M15,35h-3c-0.6,0-1-0.4-1-1v-2c0-0.6,0.4-1,1-1h3c0.6,' +
          '0,1,0.4,1,1v2C16,34.6,15.6,35,15,35z"/>'#13#10'        <path d="M22,35' +
          'h-3c-0.6,0-1-0.4-1-1v-2c0-0.6,0.4-1,1-1h3c0.6,0,1,0.4,1,1v2C23,3' +
          '4.6,22.6,35,22,35z"/>'#13#10'        <path d="M29,35h-3c-0.6,0-1-0.4-1' +
          '-1v-2c0-0.6,0.4-1,1-1h3c0.6,0,1,0.4,1,1v2C30,34.6,29.6,35,29,35z' +
          '"/>'#13#10'        <path d="M15,41h-3c-0.6,0-1-0.4-1-1v-2c0-0.6,0.4-1,' +
          '1-1h3c0.6,0,1,0.4,1,1v2C16,40.6,15.6,41,15,41z"/>'#13#10'        <path' +
          ' d="M22,41h-3c-0.6,0-1-0.4-1-1v-2c0-0.6,0.4-1,1-1h3c0.6,0,1,0.4,' +
          '1,1v2C23,40.6,22.6,41,22,41z"/>'#13#10'        <path d="M29,41h-3c-0.6' +
          ',0-1-0.4-1-1v-2c0-0.6,0.4-1,1-1h3c0.6,0,1,0.4,1,1v2C30,40.6,29.6' +
          ',41,29,41z"/>'#13#10'    </g>'#13#10'    <g fill="#BDBDBD">'#13#10'        <path d' +
          '="M36,29h-3c-0.6,0-1-0.4-1-1v-2c0-0.6,0.4-1,1-1h3c0.6,0,1,0.4,1,' +
          '1v2C37,28.6,36.6,29,36,29z"/>'#13#10'        <path d="M36,35h-3c-0.6,0' +
          '-1-0.4-1-1v-2c0-0.6,0.4-1,1-1h3c0.6,0,1,0.4,1,1v2C37,34.6,36.6,3' +
          '5,36,35z"/>'#13#10'        <path d="M36,41h-3c-0.6,0-1-0.4-1-1v-2c0-0.' +
          '6,0.4-1,1-1h3c0.6,0,1,0.4,1,1v2C37,40.6,36.6,41,36,41z"/>'#13#10'    <' +
          '/g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'cell_phone'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#546' +
          'E7A" d="M12,40V10h20c2.2,0,4,1.8,4,4v26c0,2.2-1.8,4-4,4H16C13.8,' +
          '44,12,42.2,12,40z"/>'#13#10'    <path fill="#4FC3F7" d="M32,13H16c-0.6' +
          ',0-1,0.4-1,1v8c0,0.6,0.4,1,1,1h16c0.6,0,1-0.4,1-1v-8C33,13.4,32.' +
          '6,13,32,13z"/>'#13#10'    <path fill="#B3E5FC" d="M19,30h-2c-0.6,0-1-0' +
          '.4-1-1v-1c0-0.6,0.4-1,1-1h2c0.6,0,1,0.4,1,1v1C20,29.6,19.6,30,19' +
          ',30z"/>'#13#10'    <path fill="#B3E5FC" d="M25,30h-2c-0.6,0-1-0.4-1-1v' +
          '-1c0-0.6,0.4-1,1-1h2c0.6,0,1,0.4,1,1v1C26,29.6,25.6,30,25,30z"/>' +
          #13#10'    <path fill="#B3E5FC" d="M31,30h-2c-0.6,0-1-0.4-1-1v-1c0-0.' +
          '6,0.4-1,1-1h2c0.6,0,1,0.4,1,1v1C32,29.6,31.6,30,31,30z"/>'#13#10'    <' +
          'path fill="#B3E5FC" d="M19,35h-2c-0.6,0-1-0.4-1-1v-1c0-0.6,0.4-1' +
          ',1-1h2c0.6,0,1,0.4,1,1v1C20,34.6,19.6,35,19,35z"/>'#13#10'    <path fi' +
          'll="#B3E5FC" d="M25,35h-2c-0.6,0-1-0.4-1-1v-1c0-0.6,0.4-1,1-1h2c' +
          '0.6,0,1,0.4,1,1v1C26,34.6,25.6,35,25,35z"/>'#13#10'    <path fill="#B3' +
          'E5FC" d="M31,35h-2c-0.6,0-1-0.4-1-1v-1c0-0.6,0.4-1,1-1h2c0.6,0,1' +
          ',0.4,1,1v1C32,34.6,31.6,35,31,35z"/>'#13#10'    <path fill="#B3E5FC" d' +
          '="M19,40h-2c-0.6,0-1-0.4-1-1v-1c0-0.6,0.4-1,1-1h2c0.6,0,1,0.4,1,' +
          '1v1C20,39.6,19.6,40,19,40z"/>'#13#10'    <path fill="#B3E5FC" d="M25,4' +
          '0h-2c-0.6,0-1-0.4-1-1v-1c0-0.6,0.4-1,1-1h2c0.6,0,1,0.4,1,1v1C26,' +
          '39.6,25.6,40,25,40z"/>'#13#10'    <path fill="#B3E5FC" d="M31,40h-2c-0' +
          '.6,0-1-0.4-1-1v-1c0-0.6,0.4-1,1-1h2c0.6,0,1,0.4,1,1v1C32,39.6,31' +
          '.6,40,31,40z"/>'#13#10'    <path fill="#37474F" d="M16,10h-4V4c0-1.1,0' +
          '.9-2,2-2h0c1.1,0,2,0.9,2,2V10z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'contacts'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FF7' +
          '043" d="M38,44H12V4h26c2.2,0,4,1.8,4,4v32C42,42.2,40.2,44,38,44z' +
          '"/>'#13#10'    <path fill="#BF360C" d="M10,4h2v40h-2c-2.2,0-4-1.8-4-4V' +
          '8C6,5.8,7.8,4,10,4z"/>'#13#10'    <g fill="#AB300B">'#13#10'        <circle ' +
          'cx="26" cy="20" r="4"/>'#13#10'        <path d="M33,30c0,0-1.9-4-7-4c-' +
          '5.1,0-7,4-7,4v2h14V30z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'advertising'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#90CAF9' +
          '">'#13#10'        <path d="M17.4,33H15v-4h4l0.4,1.5C19.7,31.8,18.7,33,' +
          '17.4,33z"/>'#13#10'        <path d="M37,36c0,0-11.8-7-18-7V15c5.8,0,18' +
          '-7,18-7V36z"/>'#13#10'    </g>'#13#10'    <g fill="#283593">'#13#10'        <circl' +
          'e cx="9" cy="22" r="5"/>'#13#10'        <path d="M40,19h-3v6h3c1.7,0,3' +
          '-1.3,3-3S41.7,19,40,19z"/>'#13#10'        <path d="M18.6,41.2c-0.9,0.6' +
          '-2.5,1.2-4.6,1.4c-0.6,0.1-1.2-0.3-1.4-1L8.2,27.9c0,0,8.8-6.2,8.8' +
          ',1.1 c0,5.5,1.5,8.4,2.2,9.5c0.5,0.7,0.5,1.6,0,2.3C19,41,18.8,41.' +
          '1,18.6,41.2z"/>'#13#10'    </g>'#13#10'    <path fill="#3F51B5" d="M9,29h10V' +
          '15H9c-1.1,0-2,0.9-2,2v10C7,28.1,7.9,29,9,29z"/>'#13#10'    <path fill=' +
          '"#42A5F5" d="M38,38L38,38c-1.1,0-2-0.9-2-2V8c0-1.1,0.9-2,2-2h0c1' +
          '.1,0,2,0.9,2,2v28C40,37.1,39.1,38,38,38z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'alphabetical_sorting_az'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '546E7A" points="38,33 38,5 34,5 34,33 28,33 36,43 44,33"/>'#13#10'    ' +
          '<g fill="#2196F3">'#13#10'        <path d="M16.8,17.2h-5.3l-1.1,3H6.9L' +
          '12.6,5h2.9l5.7,15.2h-3.2L16.8,17.2z M12.2,14.5H16l-1.9-5.7L12.2,' +
          '14.5z"/>'#13#10'        <path d="M12.4,40.5H20V43H8.4v-1.9L16,30.3H8.4' +
          'v-2.5h11.4v1.7L12.4,40.5z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'alphabetical_sorting_za'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#2196F3' +
          '">'#13#10'        <path d="M16.8,40h-5.3l-1.1,3H6.9l5.7-15.2h2.9L21.1,' +
          '43h-3.2L16.8,40z M12.2,37.3H16l-1.9-5.7L12.2,37.3z"/>'#13#10'        <' +
          'path d="M12.4,17.7H20v2.5H8.4v-1.9L16,7.5H8.4V5h11.4v1.7L12.4,17' +
          '.7z"/>'#13#10'    </g>'#13#10'    <polygon fill="#546E7A" points="38,33 38,5' +
          ' 34,5 34,33 28,33 36,43 44,33"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'android_os'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 15.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://w' +
          'ww.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#13#10'<svg version="1.1"  ' +
          'xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.or' +
          'g/1999/xlink" x="0px" y="0px"'#13#10#9' width="48px" height="48px" view' +
          'Box="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pre' +
          'serve">'#13#10'<g>'#13#10#9'<path fill="#7CB342" d="M12,29.001c0,1.104-0.896,' +
          '2-2,2l0,0c-1.104,0-2-0.896-2-2v-9c0-1.104,0.896-2,2-2l0,0c1.104,' +
          '0,2,0.896,2,2'#13#10#9#9'V29.001z"/>'#13#10#9'<path fill="#7CB342" d="M40,29.00' +
          '1c0,1.104-0.896,2-2,2l0,0c-1.104,0-2-0.896-2-2v-9c0-1.104,0.896-' +
          '2,2-2l0,0c1.104,0,2,0.896,2,2'#13#10#9#9'V29.001z"/>'#13#10#9'<path fill="#7CB3' +
          '42" d="M22,40c0,1.104-0.896,2-2,2l0,0c-1.104,0-2-0.896-2-2v-9c0-' +
          '1.104,0.896-2,2-2l0,0c1.104,0,2,0.896,2,2V40z"'#13#10#9#9'/>'#13#10#9'<path fil' +
          'l="#7CB342" d="M30,40c0,1.104-0.896,2-2,2l0,0c-1.104,0-2-0.896-2' +
          '-2v-9c0-1.104,0.896-2,2-2l0,0c1.104,0,2,0.896,2,2V40z"'#13#10#9#9'/>'#13#10#9'<' +
          'path fill="#7CB342" d="M14,18.001V33c0,1.104,0.896,2,2,2h16c1.10' +
          '4,0,2-0.896,2-2V18.001H14z"/>'#13#10#9'<path fill="#7CB342" d="M24,8c-6' +
          ',0-9.655,3.645-10,8h20C33.654,11.645,30,8,24,8z M20,13.598c-0.55' +
          '2,0-1-0.448-1-1s0.448-1,1-1'#13#10#9#9's1,0.448,1,1S20.552,13.598,20,13.' +
          '598z M28,13.598c-0.553,0-1-0.448-1-1s0.447-1,1-1s1,0.448,1,1S28.' +
          '553,13.598,28,13.598z"/>'#13#10#9'<line fill="none" stroke="#7CB342" st' +
          'roke-width="2" stroke-linecap="round" x1="30" y1="7" x2="28.334"' +
          ' y2="9.499"/>'#13#10#9'<line fill="none" stroke="#7CB342" stroke-width=' +
          '"2" stroke-linecap="round" x1="18" y1="7" x2="19.333" y2="9.082"' +
          '/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'answers'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '42A5F5" points="36,44 8,44 8,8 28,8 36,16"/>'#13#10'    <polygon fill=' +
          '"#90CAF9" points="40,40 12,40 12,4 32,4 40,12"/>'#13#10'    <polygon f' +
          'ill="#E1F5FE" points="38.5,13 31,13 31,5.5"/>'#13#10'    <path fill="#' +
          '1976D2" d="M23.4,29.9c0-0.2,0-0.4,0.1-0.6s0.2-0.3,0.3-0.5s0.3-0.' +
          '2,0.5-0.3s0.4-0.1,0.6-0.1s0.5,0,0.7,0.1 s0.4,0.2,0.5,0.3s0.2,0.3' +
          ',0.3,0.5s0.1,0.4,0.1,0.6s0,0.4-0.1,0.6s-0.2,0.3-0.3,0.5s-0.3,0.2' +
          '-0.5,0.3s-0.4,0.1-0.7,0.1 s-0.5,0-0.6-0.1s-0.4-0.2-0.5-0.3s-0.2-' +
          '0.3-0.3-0.5S23.4,30.1,23.4,29.9z M26.1,26.8h-2.3L23.4,17h3L26.1,' +
          '26.8z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'approval'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '8BC34A" points="24,3 28.7,6.6 34.5,5.8 36.7,11.3 42.2,13.5 41.4,' +
          '19.3 45,24 41.4,28.7 42.2,34.5 36.7,36.7 34.5,42.2 28.7,41.4 24,' +
          '45 19.3,41.4 13.5,42.2 11.3,36.7 5.8,34.5 6.6,28.7 3,24 6.6,19.3' +
          ' 5.8,13.5 11.3,11.3 13.5,5.8 19.3,6.6"/>'#13#10'    <polygon fill="#CC' +
          'FF90" points="34.6,14.6 21,28.2 15.4,22.6 12.6,25.4 21,33.8 37.4' +
          ',17.4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'approve'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FFCC80' +
          '">'#13#10'        <circle cx="38" cy="26" r="4"/>'#13#10'        <circle cx=' +
          '"10" cy="26" r="4"/>'#13#10'        <path d="M39,19c0-12.7-30-8.3-30,0' +
          'c0,1.8,0,8.2,0,10c0,8.3,6.7,15,15,15s15-6.7,15-15C39,27.2,39,20.' +
          '8,39,19z"/>'#13#10'        <path d="M24,4C15.2,4,8,11.2,8,20c0,1.2,0,3' +
          '.5,0,3.5l2.1,0.6V19l19.5-6.3l8.2,6.3v5.1l2.1-0.6c0,0,0-2.3,0-3.5' +
          ' C40,12.5,34.6,4,24,4z"/>'#13#10'    </g>'#13#10'    <polygon fill="#4CAF50"' +
          ' points="32.6,18.6 22.3,28.9 17.4,24 14.6,26.8 22.3,34.5 35.4,21' +
          '.4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'area_chart'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '3F51B5" points="42,37 6,37 6,25 16,10 30,17 42,6"/>'#13#10'    <polygo' +
          'n fill="#00BCD4" points="42,42 6,42 6,32 16,24 30,26 42,17"/>'#13#10'<' +
          '/svg>'#13#10
      end
      item
        IconName = 'assistant'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FFA726' +
          '">'#13#10'        <circle cx="10" cy="26" r="4"/>'#13#10'        <circle cx=' +
          '"38" cy="26" r="4"/>'#13#10'    </g>'#13#10'    <path fill="#FFB74D" d="M39,' +
          '19c0-12.7-30-8.3-30,0c0,1.8,0,8.2,0,10c0,8.3,6.7,15,15,15s15-6.7' +
          ',15-15C39,27.2,39,20.8,39,19z"/>'#13#10'    <path fill="#FF5722" d="M2' +
          '4,3C14.6,3,7,10.6,7,20c0,1.2,0,3.4,0,3.4L9,25v-3l21-9.8l9,9.8v3l' +
          '2-1.6c0,0,0-2.1,0-3.4 C41,12,35.3,3,24,3z"/>'#13#10'    <g fill="#7847' +
          '19">'#13#10'        <circle cx="31" cy="26" r="2"/>'#13#10'        <circle c' +
          'x="17" cy="26" r="2"/>'#13#10'    </g>'#13#10'    <path fill="#757575" d="M4' +
          '3,24c-0.6,0-1,0.4-1,1v-7c0-8.8-7.2-16-16-16h-7c-0.6,0-1,0.4-1,1s' +
          '0.4,1,1,1h7c7.7,0,14,6.3,14,14v10 c0,0.6,0.4,1,1,1s1-0.4,1-1v2c0' +
          ',3.9-3.1,7-7,7H24c-0.6,0-1,0.4-1,1s0.4,1,1,1h11c5,0,9-4,9-9v-5C4' +
          '4,24.4,43.6,24,43,24z"/>'#13#10'    <g fill="#37474F">'#13#10'        <path ' +
          'd="M43,22h-1c-1.1,0-2,0.9-2,2v4c0,1.1,0.9,2,2,2h1c1.1,0,2-0.9,2-' +
          '2v-4C45,22.9,44.1,22,43,22z"/>'#13#10'        <circle cx="24" cy="38" ' +
          'r="2"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'audio_file'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="204" fi' +
          'll="none" width="48" height="48"/>'#13#10'    <polygon fill="#90CAF9" ' +
          'points="244,45 212,45 212,3 234,3 244,13"/>'#13#10'    <polygon fill="' +
          '#E1F5FE" points="242.5,14 233,14 233,4.5"/>'#13#10'    <g fill="#1976D' +
          '2">'#13#10'        <circle cx="227" cy="30" r="4"/>'#13#10'        <polygon ' +
          'points="234,21 229,19 229,30 231,30 231,22.9 234,24"/>'#13#10'    </g>' +
          #13#10'    <polygon fill="#90CAF9" points="40,45 8,45 8,3 30,3 40,13"' +
          '/>'#13#10'    <polygon fill="#E1F5FE" points="38.5,14 29,14 29,4.5"/>'#13 +
          #10'    <g fill="#1976D2">'#13#10'        <circle cx="23" cy="30" r="4"/>' +
          #13#10'        <polygon points="30,21 25,19 25,30 27,30 27,22.9 30,24' +
          '"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'automatic'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#3F5' +
          '1B5" d="M39,43H9c-2.2,0-4-1.8-4-4V9c0-2.2,1.8-4,4-4h30c2.2,0,4,1' +
          '.8,4,4v30C43,41.2,41.2,43,39,43z"/>'#13#10'    <path fill="#B3E5FC" d=' +
          '"M33.6,25.4c0.1-0.4,0.1-0.9,0.1-1.4s0-0.9-0.1-1.4l2.8-2c0.3-0.2,' +
          '0.4-0.6,0.2-0.9l-2.7-4.6 c-0.2-0.3-0.5-0.4-0.8-0.3L30,16.3c-0.7-' +
          '0.6-1.5-1-2.4-1.4l-0.3-3.4c0-0.3-0.3-0.6-0.6-0.6h-5.3c-0.3,0-0.6' +
          ',0.3-0.6,0.6L20.4,15 c-0.9,0.3-1.6,0.8-2.4,1.4l-3.1-1.4c-0.3-0.1' +
          '-0.7,0-0.8,0.3l-2.7,4.6c-0.2,0.3-0.1,0.7,0.2,0.9l2.8,2c-0.1,0.4-' +
          '0.1,0.9-0.1,1.4 s0,0.9,0.1,1.4l-2.8,2c-0.3,0.2-0.4,0.6-0.2,0.9l2' +
          '.7,4.6c0.2,0.3,0.5,0.4,0.8,0.3l3.1-1.4c0.7,0.6,1.5,1,2.4,1.4l0.3' +
          ',3.4 c0,0.3,0.3,0.6,0.6,0.6h5.3c0.3,0,0.6-0.3,0.6-0.6l0.3-3.4c0.' +
          '9-0.3,1.6-0.8,2.4-1.4l3.1,1.4c0.3,0.1,0.7,0,0.8-0.3l2.7-4.6 c0.2' +
          '-0.3,0.1-0.7-0.2-0.9L33.6,25.4z M24,29c-2.8,0-5-2.2-5-5c0-2.8,2.' +
          '2-5,5-5c2.8,0,5,2.2,5,5C29,26.8,26.8,29,24,29z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'automotive'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="none' +
          '" stroke="#F44336" stroke-width="4" stroke-miterlimit="10" d="M7' +
          ',20v-8c0-2.2,1.8-4,4-4h14c1.2,0,2.4,0.6,3.2,1.6 L35,18"/>'#13#10'    <' +
          'g fill="#37474F">'#13#10'        <circle cx="35" cy="37" r="5"/>'#13#10'    ' +
          '    <circle cx="13" cy="37" r="5"/>'#13#10'    </g>'#13#10'    <path fill="#' +
          'F44336" d="M40.2,17L33,14H7c-1.2,0-2,0.8-2,2v10c0,1.2,0.8,2,2,2h' +
          '1c0-2.8,2.2-5,5-5s5,2.2,5,5h12c0-2.8,2.2-5,5-5 s5,2.2,5,5h1c1.2,' +
          '0,2-0.8,2-2v-5.2C43,19.2,41.8,17.6,40.2,17z"/>'#13#10'    <g fill="#54' +
          '6E7A">'#13#10'        <circle cx="24" cy="37" r="3"/>'#13#10'        <circle' +
          ' cx="35" cy="37" r="2"/>'#13#10'        <circle cx="13" cy="37" r="2"/' +
          '>'#13#10'        <path d="M30.4,39c-0.3-0.6-0.4-1.3-0.4-2s0.2-1.4,0.4-' +
          '2H17.6c0.3,0.6,0.4,1.3,0.4,2s-0.2,1.4-0.4,2H30.4z"/>'#13#10'    </g>'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = 'bad_decision'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FFCC80' +
          '">'#13#10'        <circle cx="38" cy="26" r="4"/>'#13#10'        <circle cx=' +
          '"10" cy="26" r="4"/>'#13#10'        <path d="M39,19c0-12.7-30-8.3-30,0' +
          'c0,1.8,0,8.2,0,10c0,8.3,6.7,15,15,15s15-6.7,15-15C39,27.2,39,20.' +
          '8,39,19z"/>'#13#10'        <path d="M24,4C15.2,4,8,11.2,8,20c0,1.2,0,3' +
          '.5,0,3.5l2.1,0.6V19l19.5-6.3l8.2,6.3v5.1l2.1-0.6c0,0,0-2.3,0-3.5' +
          ' C40,12.5,34.6,4,24,4z"/>'#13#10'    </g>'#13#10'    <rect x="16" y="24" fil' +
          'l="#F44336" width="16" height="4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'bar_chart'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#00BCD4' +
          '">'#13#10'        <rect x="19" y="22" width="10" height="20"/>'#13#10'      ' +
          '  <rect x="6" y="12" width="10" height="30"/>'#13#10'        <rect x="' +
          '32" y="6" width="10" height="36"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'bearish'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#F44336' +
          '">'#13#10'        <rect x="40" y="34" width="4" height="10"/>'#13#10'       ' +
          ' <rect x="34" y="29" width="4" height="15"/>'#13#10'        <rect x="2' +
          '8" y="33" width="4" height="11"/>'#13#10'        <rect x="22" y="25" w' +
          'idth="4" height="19"/>'#13#10'        <rect x="16" y="28" width="4" he' +
          'ight="16"/>'#13#10'        <rect x="10" y="24" width="4" height="20"/>' +
          #13#10'        <rect x="4" y="19" width="4" height="25"/>'#13#10'    </g>'#13#10 +
          '    <g fill="#D32F2F">'#13#10'        <polygon points="34,13.2 30,17.2' +
          ' 20,7.2 15,12.2 7.4,4.6 4.6,7.4 15,17.8 20,12.8 30,22.8 34,18.8 ' +
          '40.1,24.9 42.9,22.1"/>'#13#10'        <polygon points="44,26 35,26 44,' +
          '17"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'binoculars'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#37474F' +
          '">'#13#10'        <circle cx="33" cy="16" r="6"/>'#13#10'        <circle cx=' +
          '"15" cy="16" r="6"/>'#13#10'        <path d="M46.7,25l-15.3,3H16.7L1.4' +
          ',25l4.3-7.9c1.1-1.9,3.1-3.1,5.3-3.1h26.2c2.2,0,4.2,1.2,5.3,3.1L4' +
          '6.7,25z"/>'#13#10'        <circle cx="38" cy="30" r="10"/>'#13#10'        <c' +
          'ircle cx="10" cy="30" r="10"/>'#13#10'        <circle cx="24" cy="28" ' +
          'r="5"/>'#13#10'    </g>'#13#10'    <circle fill="#546E7A" cx="24" cy="28" r=' +
          '"2"/>'#13#10'    <g fill="#a0f">'#13#10'        <circle cx="38" cy="30" r="7' +
          '"/>'#13#10'        <circle cx="10" cy="30" r="7"/>'#13#10'    </g>'#13#10'    <g f' +
          'ill="#CE93D8">'#13#10'        <path d="M41.7,27.7c-1-1.1-2.3-1.7-3.7-1' +
          '.7s-2.8,0.6-3.7,1.7c-0.4,0.4-0.3,1,0.1,1.4c0.4,0.4,1,0.3,1.4-0.1' +
          ' c1.2-1.3,3.3-1.3,4.5,0c0.2,0.2,0.5,0.3,0.7,0.3c0.2,0,0.5-0.1,0.' +
          '7-0.3C42.1,28.7,42.1,28.1,41.7,27.7z"/>'#13#10'        <path d="M10,26' +
          'c-1.4,0-2.8,0.6-3.7,1.7c-0.4,0.4-0.3,1,0.1,1.4c0.4,0.4,1,0.3,1.4' +
          '-0.1c1.2-1.3,3.3-1.3,4.5,0 c0.2,0.2,0.5,0.3,0.7,0.3c0.2,0,0.5-0.' +
          '1,0.7-0.3c0.4-0.4,0.4-1,0.1-1.4C12.8,26.6,11.4,26,10,26z"/>'#13#10'   ' +
          ' </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'biohazard'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#00A' +
          '344" d="M24,13c-7.2,0-13,5.8-13,13s5.8,13,13,13s13-5.8,13-13S31.' +
          '2,13,24,13z M24,35c-5,0-9-4-9-9s4-9,9-9s9,4,9,9 S29,35,24,35z"/>' +
          #13#10'    <path fill="#00C853" d="M8.5,25.4c4-2.2,9-1.1,11.5,2.5c0.1' +
          ',0.1,0.2,0.1,0.3,0.1l1.2-0.7c0.1-0.1,0.2-0.2,0.1-0.3 c0-0.2-0.1-' +
          '0.4-0.1-0.6c0,0,0,0,0,0c0-0.1,0-0.1,0-0.2c0,0,0,0,0,0c0-0.1,0-0.' +
          '1,0-0.2c0,0,0,0,0,0c0-0.1,0-0.1,0-0.2l0,0 c0-0.1,0-0.1,0.1-0.2c0' +
          ',0,0,0,0,0c0-0.1,0-0.1,0.1-0.2c0,0,0,0,0,0c0,0,0-0.1,0.1-0.1c0,0' +
          ',0-0.1,0.1-0.1c0,0,0-0.1,0.1-0.1 c0,0,0.1-0.1,0.1-0.1c0,0,0,0,0,' +
          '0c0,0,0.1-0.1,0.1-0.1c0,0,0,0,0,0c0,0,0.1-0.1,0.1-0.1c0,0,0,0,0,' +
          '0c0,0,0.1-0.1,0.1-0.1 c0,0,0,0,0,0c0,0,0.1-0.1,0.1-0.1c0,0,0,0,0' +
          '.1,0c0.2-0.1,0.4-0.2,0.5-0.2c0.1,0,0.2-0.1,0.2-0.3v-1.3c0-0.1-0.' +
          '1-0.2-0.2-0.2 c-4.5-0.4-8-4.1-8-8.6c0-4.1,3-7.6,6.9-8.4c0.1,0,0.' +
          '2-0.1,0.2-0.3V4.8c0-0.1-0.1-0.2-0.2-0.2C16.4,5.5,12,10.4,12,16.3' +
          ' c0,1.3,0.2,2.6,0.6,3.8c-1.2,0.2-2.5,0.7-3.6,1.3c-5.2,3-7.3,9.2-' +
          '5.2,14.5C3.9,36,4,36,4.1,36l0.3-0.2c0.1-0.1,0.2-0.2,0.1-0.3 C3.3' +
          ',31.7,4.8,27.4,8.5,25.4L8.5,25.4z M39,21.4c-1.2-0.7-2.4-1.1-3.6-' +
          '1.3c0.4-1.2,0.6-2.4,0.6-3.8c0-5.9-4.4-10.8-10.2-11.7 c-0.1,0-0.2' +
          ',0.1-0.2,0.2v0.4c0,0.1,0.1,0.2,0.2,0.3c4,0.8,6.9,4.3,6.9,8.4c0,4' +
          '.5-3.5,8.2-8,8.6c-0.1,0-0.2,0.1-0.2,0.2v1.3 c0,0.1,0.1,0.2,0.2,0' +
          '.3c0.2,0.1,0.4,0.1,0.6,0.2c0,0,0,0,0,0c0,0,0.1,0.1,0.1,0.1c0,0,0' +
          ',0,0,0c0.1,0,0.1,0.1,0.1,0.1c0,0,0,0,0,0 c0.1,0.1,0.2,0.2,0.3,0.' +
          '3c0,0,0,0,0,0c0,0,0.1,0.1,0.1,0.1c0,0,0,0,0,0c0,0,0.1,0.1,0.1,0.' +
          '1c0,0,0,0.1,0,0.1c0,0,0,0.1,0,0.1 c0,0,0,0.1,0.1,0.1c0,0,0,0,0,0' +
          'c0,0.1,0,0.1,0.1,0.2c0,0,0,0,0,0c0,0.1,0,0.1,0,0.2c0,0,0,0,0,0c0' +
          ',0.1,0,0.1,0,0.2c0,0,0,0,0,0.1 c0,0,0,0.1,0,0.1c0,0,0,0,0,0.1c0,' +
          '0.2,0,0.4-0.1,0.6c0,0.1,0,0.2,0.1,0.3l1.2,0.7c0.1,0.1,0.2,0,0.3-' +
          '0.1c2.6-3.6,7.6-4.8,11.5-2.5 c3.6,2.1,5.2,6.3,3.9,10.1c0,0.1,0,0' +
          '.2,0.1,0.3l0.3,0.2c0.1,0.1,0.2,0,0.3-0.1C46.3,30.5,44.2,24.3,39,' +
          '21.4L39,21.4z M30.8,40.3 c-4-2.2-5.5-7.1-3.5-11.1c0.1-0.1,0-0.2-' +
          '0.1-0.3L26,28.2c-0.1-0.1-0.2,0-0.3,0c-0.2,0.1-0.3,0.3-0.5,0.3c0,' +
          '0,0,0,0,0 c-0.1,0-0.1,0.1-0.2,0.1c0,0,0,0,0,0c-0.1,0-0.1,0-0.2,0' +
          '.1c0,0,0,0,0,0c-0.1,0-0.3,0.1-0.4,0.1c0,0,0,0,0,0c-0.1,0-0.1,0-0' +
          '.2,0 c0,0,0,0-0.1,0c0,0-0.1,0-0.1,0c0,0-0.1,0-0.1,0c0,0-0.1,0-0.' +
          '1,0c0,0-0.1,0-0.1,0c0,0,0,0-0.1,0c-0.1,0-0.1,0-0.2,0c0,0,0,0,0,0' +
          ' c-0.1,0-0.1,0-0.2,0c0,0,0,0,0,0c-0.1,0-0.1,0-0.2-0.1c0,0,0,0,0,' +
          '0c0,0-0.1,0-0.1-0.1c0,0,0,0-0.1,0c-0.2-0.1-0.3-0.2-0.5-0.3 c-0.1' +
          '-0.1-0.2-0.1-0.3,0l-1.2,0.7c-0.1,0.1-0.1,0.2-0.1,0.3c1.9,4,0.4,8' +
          '.8-3.5,11.1c-3.6,2.1-8.2,1.3-10.9-1.7 c-0.1-0.1-0.2-0.1-0.3-0.1l' +
          '-0.3,0.2c-0.1,0.1-0.1,0.2-0.1,0.3c3.6,4.5,10.2,5.8,15.4,2.8c1.2-' +
          '0.7,2.2-1.5,3-2.4 c0.8,0.9,1.8,1.8,3,2.4c5.2,3,11.7,1.6,15.4-2.8' +
          'c0.1-0.1,0-0.2-0.1-0.3L42,38.5c-0.1-0.1-0.2,0-0.3,0.1C39,41.5,34' +
          '.4,42.3,30.8,40.3 L30.8,40.3z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'biomass'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#9CC' +
          'C65" d="M32,15V7H16v8L6.2,40c-0.6,1.5,0.5,3,2.1,3h31.5c1.6,0,2.6' +
          '-1.6,2.1-3L32,15z"/>'#13#10'    <path fill="#8BC34A" d="M32,9H16c-1.1,' +
          '0-2-0.9-2-2v0c0-1.1,0.9-2,2-2h16c1.1,0,2,0.9,2,2v0C34,8.1,33.1,9' +
          ',32,9z"/>'#13#10'    <path fill="#2E7D32" d="M28,30c0,4.4-4,8-4,8s-4-3' +
          '.6-4-8s4-8,4-8S28,25.6,28,30z"/>'#13#10'    <path fill="#388E3C" d="M3' +
          '1.1,32.6c-2,4-7.1,5.4-7.1,5.4s-2-5,0-8.9s7.1-5.4,7.1-5.4S33.1,28' +
          '.6,31.1,32.6z"/>'#13#10'    <path fill="#43A047" d="M16.9,32.6c2,4,7.1' +
          ',5.4,7.1,5.4s2-5,0-8.9s-7.1-5.4-7.1-5.4S14.9,28.6,16.9,32.6z"/>'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'biotech'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#AD1' +
          '457" d="M36,4c0,9.3-6,13.2-12.8,17.8C16.1,26.5,8,31.8,8,44h4c0-1' +
          '0.1,6.5-14.4,13.4-18.9C32.2,20.6,40,15.4,40,4 H36z"/>'#13#10'    <path' +
          ' fill="#AD1457" d="M38,41H11c-0.6,0-1-0.4-1-1s0.4-1,1-1h27c0.6,0' +
          ',1,0.4,1,1S38.6,41,38,41z"/>'#13#10'    <path fill="#AD1457" d="M36,37' +
          'H12c-0.6,0-1-0.4-1-1s0.4-1,1-1h24c0.6,0,1,0.4,1,1S36.6,37,36,37z' +
          '"/>'#13#10'    <path fill="#AD1457" d="M34,33H14c-0.6,0-1-0.4-1-1s0.4-' +
          '1,1-1h20c0.6,0,1,0.4,1,1S34.6,33,34,33z"/>'#13#10'    <path fill="#AD1' +
          '457" d="M29,29H19c-0.6,0-1-0.4-1-1s0.4-1,1-1h10c0.6,0,1,0.4,1,1S' +
          '29.6,29,29,29z"/>'#13#10'    <path fill="#E91E63" d="M37,9H10C9.4,9,9,' +
          '8.6,9,8s0.4-1,1-1h27c0.6,0,1,0.4,1,1S37.6,9,37,9z"/>'#13#10'    <path ' +
          'fill="#E91E63" d="M36,13H12c-0.6,0-1-0.4-1-1s0.4-1,1-1h24c0.6,0,' +
          '1,0.4,1,1S36.6,13,36,13z"/>'#13#10'    <path fill="#E91E63" d="M34,17H' +
          '14c-0.6,0-1-0.4-1-1s0.4-1,1-1h20c0.6,0,1,0.4,1,1S34.6,17,34,17z"' +
          '/>'#13#10'    <path fill="#E91E63" d="M29,21H19c-0.6,0-1-0.4-1-1s0.4-1' +
          ',1-1h10c0.6,0,1,0.4,1,1S29.6,21,29,21z"/>'#13#10'    <path fill="#E91E' +
          '63" d="M40,44h-4c0-10.1-6.5-14.4-13.4-18.9C15.8,20.6,8,15.4,8,4h' +
          '4c0,9.3,6,13.2,12.8,17.8 C31.9,26.5,40,31.8,40,44z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'bookmark'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#F44' +
          '336" d="M37,43l-13-6l-13,6V9c0-2.2,1.8-4,4-4h18c2.2,0,4,1.8,4,4V' +
          '43z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'briefcase'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#424' +
          '242" d="M27,7h-6c-1.7,0-3,1.3-3,3v3h2v-3c0-0.6,0.4-1,1-1h6c0.6,0' +
          ',1,0.4,1,1v3h2v-3C30,8.3,28.7,7,27,7z"/>'#13#10'    <path fill="#E6510' +
          '0" d="M40,43H8c-2.2,0-4-1.8-4-4V15c0-2.2,1.8-4,4-4h32c2.2,0,4,1.' +
          '8,4,4v24C44,41.2,42.2,43,40,43z"/>'#13#10'    <path fill="#FF6E40" d="' +
          'M40,28H8c-2.2,0-4-1.8-4-4v-9c0-2.2,1.8-4,4-4h32c2.2,0,4,1.8,4,4v' +
          '9C44,26.2,42.2,28,40,28z"/>'#13#10'    <path fill="#FFF3E0" d="M26,26h' +
          '-4c-0.6,0-1-0.4-1-1v-2c0-0.6,0.4-1,1-1h4c0.6,0,1,0.4,1,1v2C27,25' +
          '.6,26.6,26,26,26z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'bullish'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#4CAF50' +
          '">'#13#10'        <rect x="40" y="21" width="4" height="23"/>'#13#10'       ' +
          ' <rect x="34" y="28" width="4" height="16"/>'#13#10'        <rect x="2' +
          '8" y="23" width="4" height="21"/>'#13#10'        <rect x="22" y="29" w' +
          'idth="4" height="15"/>'#13#10'        <rect x="16" y="32" width="4" he' +
          'ight="12"/>'#13#10'        <rect x="10" y="30" width="4" height="14"/>' +
          #13#10'        <rect x="4" y="34" width="4" height="10"/>'#13#10'    </g>'#13#10 +
          '    <g fill="#388E3C">'#13#10'        <polygon points="40.1,9.1 34,15.' +
          '2 30,11.2 20,21.2 15,16.2 4.6,26.6 7.4,29.4 15,21.8 20,26.8 30,1' +
          '6.8 34,20.8 42.9,11.9"/>'#13#10'        <polygon points="44,8 35,8 44,' +
          '17"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Business\business'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#263238' +
          '">'#13#10'        <path d="M11,44H9c-0.6,0-1-0.4-1-1v-2h4v2C12,43.6,11' +
          '.6,44,11,44z"/>'#13#10'        <path d="M39,44h-2c-0.6,0-1-0.4-1-1v-2h' +
          '4v2C40,43.6,39.6,44,39,44z"/>'#13#10'    </g>'#13#10'    <path fill="#37474F' +
          '" d="M27,7h-6c-1.7,0-3,1.3-3,3v3h2v-3c0-0.6,0.4-1,1-1h6c0.6,0,1,' +
          '0.4,1,1v3h2v-3C30,8.3,28.7,7,27,7z"/>'#13#10'    <path fill="#78909C" ' +
          'd="M40,43H8c-2.2,0-4-1.8-4-4V15c0-2.2,1.8-4,4-4h32c2.2,0,4,1.8,4' +
          ',4v24C44,41.2,42.2,43,40,43z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Business\business_contact'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#673' +
          'AB7" d="M40,7H8c-2.2,0-4,1.8-4,4v26c0,2.2,1.8,4,4,4h5v-1.3c-0.6-' +
          '0.3-1-1-1-1.7c0-1.1,0.9-2,2-2s2,0.9,2,2 c0,0.7-0.4,1.4-1,1.7V41h' +
          '18v-1.3c-0.6-0.3-1-1-1-1.7c0-1.1,0.9-2,2-2s2,0.9,2,2c0,0.7-0.4,1' +
          '.4-1,1.7V41h5c2.2,0,4-1.8,4-4V11 C44,8.8,42.2,7,40,7z"/>'#13#10'    <g' +
          ' fill="#D1C4E9">'#13#10'        <circle cx="24" cy="18" r="4"/>'#13#10'     ' +
          '   <path d="M31,28c0,0-1.9-4-7-4c-5.1,0-7,4-7,4v2h14V28z"/>'#13#10'   ' +
          ' </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'butting_in'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#CFD' +
          '8DC" d="M24,3C12.4,3,3,12.4,3,24c0,11.6,9.4,21,21,21c0.3,0,0.7,0' +
          ',1-0.1V3.1C24.7,3,24.3,3,24,3z"/>'#13#10'    <path fill="#37474F" d="M' +
          '25,3.1v41.9c4.1-0.2,7.9-1.5,11-3.7V6.8C32.9,4.6,29.1,3.2,25,3.1z' +
          '"/>'#13#10'    <path fill="#FFB74D" d="M20.5,13C14.1,13.3,8.9,18.7,9,2' +
          '5.1c0,2.8,1,5.4,2.7,7.5c1.4,1.7,2.3,3.9,2.3,6.1v3.8c3,1.6,6.4,2.' +
          '5,10,2.5 c0.3,0,0.7,0,1-0.1c0.7,0,1.3-0.1,2-0.2v-9.4c3.6-2.1,6-5' +
          '.9,6-10.4C33,18.2,27.4,12.7,20.5,13z"/>'#13#10'    <path fill="#FFB74D' +
          '" d="M29,38.6L25,38v-9h8l-0.7,7C32.1,37.6,30.7,38.8,29,38.6z"/>'#13 +
          #10'    <polygon fill="#FFB74D" points="39,29 32,31 31,26 32,22"/>'#13 +
          #10'    <circle fill="#784719" cx="29.5" cy="25.5" r="1.5"/>'#13#10'    <' +
          'path fill="#FF5722" d="M21,12c-7.2,0-13,5.8-13,13c0,7.6,5.1,9,6,' +
          '13l4-3v-8l5-2l1-4c3.2,0,6-3.9,6-6.1C27.9,13,24.4,12,21,12z"/>'#13#10' ' +
          '   <circle fill="#FFB74D" cx="19" cy="27" r="3"/>'#13#10'    <path fil' +
          'l="#CFD8DC" d="M45,24c0-7.1-3.6-13.4-9-17.2v34.4C41.4,37.4,45,31' +
          '.1,45,24z"/>'#13#10'    <path fill="#FF9800" d="M20,44.6c1.3,0.2,2.6,0' +
          '.4,4,0.4c0.3,0,0.7,0,1-0.1c0.7,0,1.3-0.1,2-0.2v-6.5l-7-1V44.6z"/' +
          '>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'cable_release'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#374' +
          '74F" d="M34.9,29.1c-2.7-2.7-7.1-2.7-9.8,0l-4,4c-1.7,1.7-4.5,1.7-' +
          '6.2,0c-1.7-1.7-1.7-4.5,0-6.2l4.5-4.5l-2.8-2.8 l-4.5,4.5c-3.3,3.3' +
          '-3.3,8.6,0,11.8c3.3,3.3,8.6,3.3,11.8,0l4-4c1.2-1.1,3-1.1,4.2,0c1' +
          '.1,1.2,1.1,3,0,4.2L27,41.2l2.8,2.8l5.1-5.1 C37.6,36.2,37.6,31.8,' +
          '34.9,29.1z"/>'#13#10'    <path fill="#0277BD" d="M16.1,22.9L16.1,22.9c' +
          '-2.8-2.8-2.8-7.3,0-10l6.8-6.8c2.8-2.8,7.3-2.8,10,0l0,0c2.8,2.8,2' +
          '.8,7.3,0,10 l-6.8,6.8C23.3,25.7,18.9,25.7,16.1,22.9z"/>'#13#10'    <ci' +
          'rcle fill="#B3E5FC" cx="28" cy="11" r="4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'call_transfer'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#009' +
          '688" d="M39.2,8.4l-1.8,1.8c-6.3,6.5-5.4,22,0,27.6l1.8,1.8c0.5,0.' +
          '5,1.3,0.5,1.8,0l3.6-3.7c0.5-0.5,0.5-1.3,0-1.8 l-3.4-3.4h-4.8c-1.' +
          '3-1.3-1.3-12.1,0-13.4h4.8l3.3-3.4c0.5-0.5,0.5-1.3,0-1.8L41,8.4C4' +
          '0.5,7.9,39.7,7.9,39.2,8.4z"/>'#13#10'    <path fill="#009688" d="M11.2' +
          ',8.4l-1.8,1.8c-6.3,6.5-5.4,22,0,27.6l1.8,1.8c0.5,0.5,1.3,0.5,1.8' +
          ',0l3.6-3.7c0.5-0.5,0.5-1.3,0-1.8 l-3.4-3.4H8.5c-1.3-1.3-1.3-12.1' +
          ',0-13.4h4.8l3.3-3.4c0.5-0.5,0.5-1.3,0-1.8L13,8.4C12.5,7.9,11.7,7' +
          '.9,11.2,8.4z"/>'#13#10'    <g fill="#2196F3">'#13#10'        <polygon points' +
          '="25.3,18.6 30.7,24 25.3,29.4"/>'#13#10'        <rect x="16" y="22" wi' +
          'dth="11" height="4"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'callback'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#219' +
          '6F3" d="M26.4,33.9c0,0,4-2.6,4.8-3c0.8-0.4,1.7-0.6,2.2-0.2c0.8,0' +
          '.5,7.5,4.9,8.1,5.3c0.6,0.4,0.8,1.5,0.1,2.6 c-0.8,1.1-4.3,5.5-5.8' +
          ',5.4c-1.5,0-8.4,0.4-20.3-11.4C3.6,20.7,4,13.8,4,12.3c0-1.5,4.3-5' +
          '.1,5.4-5.8c1.1-0.8,2.2-0.5,2.6,0.1 c0.4,0.6,4.8,7.3,5.3,8.1c0.3,' +
          '0.5,0.2,1.4-0.2,2.2c-0.4,0.8-3,4.8-3,4.8s0.7,2.8,5,7.2C23.5,33.2' +
          ',26.4,33.9,26.4,33.9z"/>'#13#10'    <g fill="#3F51B5">'#13#10'        <path ' +
          'd="M35,9H25v4h10c1.1,0,2,0.9,2,2v10h4V15C41,11.7,38.3,9,35,9z"/>' +
          #13#10'        <polygon points="28,16 21.3,11 28,6"/>'#13#10'    </g>'#13#10'</sv' +
          'g>'#13#10
      end
      item
        IconName = 'camcorder'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#607' +
          'D8B" d="M20,42H10c-2.2,0-4-1.8-4-4V15c0-5,4-9,9-9h0c5,0,9,4,9,9v' +
          '23C24,40.2,22.2,42,20,42z"/>'#13#10'    <circle fill="#455A64" cx="15"' +
          ' cy="15" r="7"/>'#13#10'    <circle fill="#42A5F5" cx="15" cy="15" r="' +
          '5.2"/>'#13#10'    <path fill="#90CAF9" d="M18.3,13c-0.8-0.9-2-1.5-3.3-' +
          '1.5S12.6,12,11.7,13c-0.3,0.4-0.3,0.9,0.1,1.2c0.4,0.3,0.9,0.3,1.2' +
          '-0.1 c1-1.2,2.9-1.2,3.9,0c0.2,0.2,0.4,0.3,0.7,0.3c0.2,0,0.4-0.1,' +
          '0.6-0.2C18.6,13.9,18.6,13.3,18.3,13z"/>'#13#10'    <path fill="#607D8B' +
          '" d="M40,31H28c-1.1,0-2-0.9-2-2V19c0-1.1,0.9-2,2-2h12c1.1,0,2,0.' +
          '9,2,2v10C42,30.1,41.1,31,40,31z"/>'#13#10'    <rect x="24" y="19" fill' +
          '="#455A64" width="2" height="10"/>'#13#10'    <rect x="28" y="19" fill' +
          '="#03A9F4" width="12" height="10"/>'#13#10'    <polygon fill="#4FC3F7"' +
          ' points="33,22.2 29,28 37,28"/>'#13#10'    <g fill="#B3E5FC">'#13#10'       ' +
          ' <circle cx="37.5" cy="21.5" r="1"/>'#13#10'        <polygon points="3' +
          '6,24.2 33,28 39,28"/>'#13#10'    </g>'#13#10'    <circle fill="#455A64" cx="' +
          '15" cy="35" r="3"/>'#13#10'    <circle fill="#F44336" cx="15" cy="35" ' +
          'r="2"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'camcorder_pro'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="27" y="' +
          '8" fill="#37474F" width="10" height="4"/>'#13#10'    <path fill="#607D' +
          '8B" d="M27,8h-9.7c-1.5,0-2.8,0.8-3.5,2.1l-3.3,6L14,18l3.3-6H27v7' +
          '.2h4V12C31,9.8,29.2,8,27,8z"/>'#13#10'    <path fill="#607D8B" d="M30,' +
          '40H6c-2.2,0-4-1.8-4-4V20c0-2.2,1.8-4,4-4h24c2.2,0,4,1.8,4,4v16C3' +
          '4,38.2,32.2,40,30,40z"/>'#13#10'    <path fill="#607D8B" d="M38,35l5,5' +
          'h3V18h-3l-5,5V35z"/>'#13#10'    <path fill="#546E7A" d="M22,35H8c-1.1,' +
          '0-2-0.9-2-2V23c0-1.1,0.9-2,2-2h14c1.1,0,2,0.9,2,2v10C24,34.1,23.' +
          '1,35,22,35z"/>'#13#10'    <rect x="34" y="23" fill="#455A64" width="4"' +
          ' height="12"/>'#13#10'    <path fill="#263238" d="M41,13h-4c-0.6,0-1-0' +
          '.4-1-1V8c0-0.6,0.4-1,1-1h4c1.7,0,3,1.3,3,3v0C44,11.7,42.7,13,41,' +
          '13z"/>'#13#10'    <rect x="8" y="23" fill="#03A9F4" width="14" height=' +
          '"10"/>'#13#10'    <polygon fill="#4FC3F7" points="13.5,25.5 9,32 18,32' +
          '"/>'#13#10'    <g fill="#B3E5FC">'#13#10'        <circle cx="19.5" cy="25.5"' +
          ' r="1.5"/>'#13#10'        <polygon points="17.5,27.6 14,32 21,32"/>'#13#10' ' +
          '   </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'camera'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#512' +
          'DA8" d="M33.9,12.1H14.2L17.6,7c0.4-0.6,1-0.9,1.7-0.9h9.6c0.7,0,1' +
          '.3,0.3,1.7,0.9L33.9,12.1z"/>'#13#10'    <path fill="#8667C4" d="M14,11' +
          'H8V9.2C8,8.5,8.5,8,9.2,8h3.6C13.5,8,14,8.5,14,9.2V11z"/>'#13#10'    <p' +
          'ath fill="#5E35B1" d="M40,42H8c-2.2,0-4-1.8-4-4V14c0-2.2,1.8-4,4' +
          '-4h32c2.2,0,4,1.8,4,4v24C44,40.2,42.2,42,40,42z"/>'#13#10'    <circle ' +
          'fill="#512DA8" cx="24" cy="26" r="12"/>'#13#10'    <circle fill="#B388' +
          'FF" cx="24" cy="26" r="9"/>'#13#10'    <path fill="#C7A7FF" d="M29,23c' +
          '-1.2-1.4-3-2.2-4.8-2.2c-1.8,0-3.6,0.8-4.8,2.2c-0.5,0.5-0.4,1.3,0' +
          '.1,1.8c0.5,0.5,1.3,0.4,1.8-0.1 c1.5-1.7,4.3-1.7,5.8,0c0.3,0.3,0.' +
          '6,0.4,1,0.4c0.3,0,0.6-0.1,0.9-0.3C29.4,24.4,29.5,23.5,29,23z"/>'#13 +
          #10'    <ellipse fill="#8667C4" cx="11" cy="13.5" rx="2" ry="1.5"/>' +
          #13#10'</svg>'#13#10
      end
      item
        IconName = 'camera_addon'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#512' +
          'DA8" d="M33.9,12.1H14.2L17.6,7c0.4-0.6,1-0.9,1.7-0.9h9.6c0.7,0,1' +
          '.3,0.3,1.7,0.9L33.9,12.1z"/>'#13#10'    <path fill="#8667C4" d="M14,11' +
          'H8V9.2C8,8.5,8.5,8,9.2,8h3.6C13.5,8,14,8.5,14,9.2V11z"/>'#13#10'    <p' +
          'ath fill="#5E35B1" d="M40,42H8c-2.2,0-4-1.8-4-4V14c0-2.2,1.8-4,4' +
          '-4h32c2.2,0,4,1.8,4,4v24C44,40.2,42.2,42,40,42z"/>'#13#10'    <circle ' +
          'fill="#512DA8" cx="24" cy="26" r="12"/>'#13#10'    <circle fill="#B388' +
          'FF" cx="24" cy="26" r="9"/>'#13#10'    <path fill="#C7A7FF" d="M28.8,2' +
          '3c-1.2-1.4-3-2.2-4.8-2.2s-3.6,0.8-4.8,2.2c-0.5,0.5-0.4,1.3,0.1,1' +
          '.8c0.5,0.5,1.3,0.4,1.8-0.1 c1.5-1.7,4.3-1.7,5.8,0c0.3,0.3,0.6,0.' +
          '4,1,0.4c0.3,0,0.6-0.1,0.9-0.3C29.2,24.4,29.3,23.5,28.8,23z"/>'#13#10' ' +
          '   <ellipse fill="#8667C4" cx="11" cy="13.5" rx="2" ry="1.5"/>'#13#10 +
          '    <path fill="#8BC34A" d="M48,33.8c0-1.3-1.1-2.4-2.4-2.4H42c-0' +
          '.4,0-0.7-0.5-0.4-0.8c0.4-0.6,0.5-1.3,0.4-2.1 c-0.2-1.2-1.1-2.1-2' +
          '.3-2.4C37.7,25.7,36,27.1,36,29c0,0.6,0.2,1.1,0.4,1.6c0.2,0.4,0,0' +
          '.8-0.5,0.8h-3.6c-1.3,0-2.4,1.1-2.4,2.4V37 c0,0.4,0.5,0.7,0.8,0.4' +
          'c0.6-0.4,1.3-0.5,2.1-0.4c1.2,0.2,2.1,1.1,2.4,2.3c0.4,1.9-1.1,3.6' +
          '-2.9,3.6c-0.6,0-1.1-0.2-1.6-0.4 c-0.4-0.2-0.8,0-0.8,0.5v2.6c0,1.' +
          '3,1.1,2.4,2.4,2.4h13.2c1.3,0,2.4-1.1,2.4-2.4V33.8z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'cancel'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#D50' +
          '000" d="M24,6C14.1,6,6,14.1,6,24s8.1,18,18,18s18-8.1,18-18S33.9,' +
          '6,24,6z M24,10c3.1,0,6,1.1,8.4,2.8L12.8,32.4 C11.1,30,10,27.1,10' +
          ',24C10,16.3,16.3,10,24,10z M24,38c-3.1,0-6-1.1-8.4-2.8l19.6-19.6' +
          'C36.9,18,38,20.9,38,24C38,31.7,31.7,38,24,38 z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'candle_sticks'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#546E7A' +
          '">'#13#10'        <rect x="38" y="4" width="2" height="20"/>'#13#10'        ' +
          '<rect x="15" y="7" width="2" height="17"/>'#13#10'        <rect x="8" ' +
          'y="27" width="2" height="17"/>'#13#10'        <rect x="28" y="19" widt' +
          'h="2" height="22"/>'#13#10'    </g>'#13#10'    <path fill="#4CAF50" d="M36,7' +
          'h6c1.1,0,2,0.9,2,2v10c0,1.1-0.9,2-2,2h-6c-1.1,0-2-0.9-2-2V9C34,7' +
          '.9,34.9,7,36,7z"/>'#13#10'    <path fill="#4CAF50" d="M13,10h6c1.1,0,2' +
          ',0.9,2,2v7c0,1.1-0.9,2-2,2h-6c-1.1,0-2-0.9-2-2v-7C11,10.9,11.9,1' +
          '0,13,10z"/>'#13#10'    <path fill="#F44336" d="M6,30h6c1.1,0,2,0.9,2,2' +
          'v7c0,1.1-0.9,2-2,2H6c-1.1,0-2-0.9-2-2v-7C4,30.9,4.9,30,6,30z"/>'#13 +
          #10'    <path fill="#F44336" d="M26,22h6c1.1,0,2,0.9,2,2v12c0,1.1-0' +
          '.9,2-2,2h-6c-1.1,0-2-0.9-2-2V24C24,22.9,24.9,22,26,22z"/>'#13#10'</svg' +
          '>'#13#10
      end
      item
        IconName = 'capacitor'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FF9800' +
          '">'#13#10'        <rect y="27" width="25" height="4"/>'#13#10'        <rect ' +
          'y="17" width="25" height="4"/>'#13#10'    </g>'#13#10'    <g fill="#3F51B5">' +
          #13#10'        <path d="M46,35c1.1,0,2-0.9,2-2V15c0-1.1-0.9-2-2-2H27v' +
          '22H46z"/>'#13#10'        <path d="M21,13c-1.1,0-2,0.9-2,2v18c0,1.1,0.9' +
          ',2,2,2h2V13H21z"/>'#13#10'    </g>'#13#10'    <path fill="#303F9F" d="M25,33' +
          'c1.1,0,2,0.9,2,2V13c0,1.1-0.9,2-2,2c-1.1,0-2-0.9-2-2v22C23,33.9,' +
          '23.9,33,25,33z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'cd_logo'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 15.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://w' +
          'ww.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#13#10'<svg version="1.1"  ' +
          'xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.or' +
          'g/1999/xlink" x="0px" y="0px"'#13#10#9' width="48px" height="48px" view' +
          'Box="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pre' +
          'serve">'#13#10'<g>'#13#10#9'<path fill="#2196F3" d="M17.814,18H20.2c0.5,0,0.8' +
          ',0.4,0.8,0.8v16.4c0,0.399-0.4,0.8-0.8,0.8h-2.384c-0.4,0-0.8-0.4-' +
          '0.8-0.8V18.8'#13#10#9#9'C16.916,18.3,17.314,18,17.814,18z"/>'#13#10#9'<path fil' +
          'l="#2196F3" d="M14.2,11h-3.3c-0.5,0-0.9,0.403-0.9,0.807V17H2.2C1' +
          '.6,17,1,17.605,1,18.21v16.58C1,35.396,1.6,36,2.2,36h12'#13#10#9#9'c0.4,0' +
          ',0.8-0.305,0.8-0.809V11.807C15,11.403,14.7,11,14.2,11z M10,31.28' +
          '3c0,0.398-0.4,0.8-0.8,0.8H6.8c-0.4,0-0.8-0.399-0.8-0.8'#13#10#9#9'V21.8C' +
          '6,21.4,6.3,21,6.8,21h2.4c0.5,0,0.8,0.4,0.8,0.8V31.283z"/>'#13#10#9'<pat' +
          'h fill="#2196F3" d="M33.2,25c0.5,0,0.8,0.6,0.8,0.8v9.4c0,0.399-0' +
          '.422,0.8-0.8,0.8h-9.4c-0.425,0-0.8-0.4-0.8-0.8v-2.386'#13#10#9#9'c0-0.5,' +
          '0.4-0.799,0.8-0.799L30,32v-3h-6.2c-0.331,0-0.8-0.4-0.8-0.801V18.' +
          '8c0-0.5,0.4-0.8,0.8-0.8h9.4c0.399,0,0.8,0.4,0.8,0.8v2.4'#13#10#9#9'c0,0.' +
          '3-0.266,0.8-0.8,0.8H27v3H33.2z"/>'#13#10#9'<path fill="#2196F3" d="M48,' +
          '28v7.2c0,0.399-0.4,0.8-0.801,0.8H36.8c-0.2,0-0.8-0.4-0.8-0.8V18.' +
          '8c0-0.5,0.432-0.8,0.831-0.8H47.2'#13#10#9#9'c0,0,0.8,0,0.8,0.8V25h-4v-2.' +
          '2c0,0,0.1-0.8-0.8-0.8h-2.4c-0.5,0-0.8,0.4-0.8,0.8v8.4c0,0.399,0.' +
          '5,0.8,0.8,0.8h2.4'#13#10#9#9'c0.399,0,0.8-0.4,0.8-0.8V28H48z"/>'#13#10'</g>'#13#10'<' +
          'g>'#13#10#9'<polygon fill="#0D47A1" points="45.799,15.98 46.9,15.98 46.' +
          '9,12.881 48,12.881 48,11.98 44.799,11.98 44.799,12.881 '#13#10#9#9'45.79' +
          '9,12.881 '#9'"/>'#13#10#9'<path fill="#0D47A1" d="M44.014,14.476h-1.143v0.' +
          '095c0,0.382-0.096,0.573-0.572,0.573c-0.475,0-0.57-0.191-0.57-0.7' +
          '62v-0.668'#13#10#9#9'c0-0.572,0-0.762,0.57-0.762c0.381,0,0.572,0.095,0.5' +
          '72,0.477v0.095h1.047v-0.095c0-1.047-0.381-1.429-1.523-1.429h-0.2' +
          '87'#13#10#9#9'c-1.141,0-1.523,0.382-1.523,1.618v0.764c0,1.142,0.381,1.61' +
          '8,1.523,1.618h0.383c1.047,0,1.428-0.477,1.428-1.43v-0.095H44.014' +
          'z"/>'#13#10#9'<polygon fill="#0D47A1" points="29.516,12 28.717,14.9 27.' +
          '816,12 26.217,12 26.217,16 27.316,16 27.316,13 28.115,16 29.217,' +
          '16 '#13#10#9#9'30.115,13.1 30.115,16 31.115,16 31.115,12 '#9'"/>'#13#10#9'<path fi' +
          'll="#0D47A1" d="M38.713,12h-0.9h-0.898l-1.199,4h1.199l0.199-0.7h' +
          '0.602h0.799l0.199,0.7h1.199L38.713,12z M37.914,14.5'#13#10#9#9'h-0.4l0.4' +
          '-1.7l0,0l0,0l0.398,1.7H37.914z"/>'#13#10#9'<path fill="#0D47A1" d="M33.' +
          '92,12h-0.199H32.02v4h1.102v-1.1h0.6h0.199c1.102,0,1.5-0.4,1.5-1.' +
          '4v-0.3C35.42,12.3,35.02,12,33.92,12'#13#10#9#9'z M34.32,13.6c0,0.4-0.1,0' +
          '.5-0.4,0.5h-0.199h-0.6v-1.2h0.6h0.1c0.4,0,0.5,0.1,0.5,0.4V13.6z"' +
          '/>'#13#10#9'<path fill="#0D47A1" d="M23.594,12h-0.286h-0.286c-1.143,0-1' +
          '.523,0.382-1.523,1.618v0.762c0,1.144,0.381,1.62,1.523,1.62h0.286' +
          #13#10#9#9'h0.286c1.143,0,1.523-0.477,1.523-1.62v-0.762C25.117,12.382,2' +
          '4.737,12,23.594,12z M23.975,14.19c0,0.571,0,0.763-0.571,0.763'#13#10#9 +
          #9'h-0.095h-0.095c-0.571,0-0.571-0.191-0.571-0.763v-0.572c0-0.57,0' +
          '-0.762,0.571-0.762h0.095h0.095c0.571,0,0.571,0.191,0.571,0.762'#13#10 +
          #9#9'V14.19z"/>'#13#10#9'<path fill="#0D47A1" d="M20.422,14.477h-1.144v0.0' +
          '95c0,0.382-0.095,0.571-0.571,0.571c-0.476,0-0.571-0.189-0.571-0.' +
          '762v-0.666'#13#10#9#9'c0-0.573,0-0.762,0.571-0.762c0.381,0,0.571,0.095,0' +
          '.571,0.475v0.096h1.048v-0.096c0-1.047-0.381-1.428-1.523-1.428h-0' +
          '.286'#13#10#9#9'c-1.143,0-1.524,0.381-1.524,1.618v0.763c0,1.143,0.381,1.' +
          '619,1.524,1.619h0.381c1.048,0,1.429-0.477,1.429-1.429v-0.095H20.' +
          '422z"'#13#10#9#9'/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'charge_battery'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#8BC34A' +
          '">'#13#10'        <path d="M34,44H14c-1.1,0-2-0.9-2-2V8c0-1.1,0.9-2,2-' +
          '2h20c1.1,0,2,0.9,2,2v34C36,43.1,35.1,44,34,44z"/>'#13#10'        <path' +
          ' d="M28,13h-8c-0.6,0-1-0.4-1-1V5c0-0.6,0.4-1,1-1h8c0.6,0,1,0.4,1' +
          ',1v7C29,12.6,28.6,13,28,13z"/>'#13#10'    </g>'#13#10'    <polygon fill="#FF' +
          'EB3B" points="30,24 24.5,24 26.7,13 18,26 23.5,26 21.3,37"/>'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'checkmark'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '43A047" points="40.6,12.1 17,35.7 7.4,26.1 4.6,29 17,41.3 43.4,1' +
          '4.9"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'circuit'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#4CA' +
          'F50" d="M6,10v28c0,2.2,1.8,4,4,4h28c2.2,0,4-1.8,4-4V10c0-2.2-1.8' +
          '-4-4-4H10C7.8,6,6,7.8,6,10z"/>'#13#10'    <g fill="#FFC107">'#13#10'        ' +
          '<path d="M6.6,8l6,6c-0.4,0.6-0.6,1.3-0.6,2c0,2.2,1.8,4,4,4s4-1.8' +
          ',4-4s-1.8-4-4-4c-0.7,0-1.4,0.2-2,0.6l-6-6 C7.4,6.9,6.9,7.4,6.6,8' +
          'z M16,14.5c0.8,0,1.5,0.7,1.5,1.5s-0.7,1.5-1.5,1.5s-1.5-0.7-1.5-1' +
          '.5S15.2,14.5,16,14.5z"/>'#13#10'        <path d="M41.4,40l-6-6c0.4-0.6' +
          ',0.6-1.3,0.6-2c0-2.2-1.8-4-4-4s-4,1.8-4,4s1.8,4,4,4c0.7,0,1.4-0.' +
          '2,2-0.6l6,6 C40.6,41.1,41.1,40.6,41.4,40z M32,33.5c-0.8,0-1.5-0.' +
          '7-1.5-1.5s0.7-1.5,1.5-1.5s1.5,0.7,1.5,1.5S32.8,33.5,32,33.5z"/>'#13 +
          #10'        <path d="M16,36c2.2,0,4-1.8,4-4c0-0.7-0.2-1.4-0.6-2L30,' +
          '19.4c0.6,0.4,1.3,0.6,2,0.6c2.2,0,4-1.8,4-4s-1.8-4-4-4 s-4,1.8-4,' +
          '4c0,0.7,0.2,1.4,0.6,2L18,28.6c-0.6-0.4-1.3-0.6-2-0.6c-2.2,0-4,1.' +
          '8-4,4S13.8,36,16,36z M32,14.5c0.8,0,1.5,0.7,1.5,1.5 s-0.7,1.5-1.' +
          '5,1.5s-1.5-0.7-1.5-1.5S31.2,14.5,32,14.5z M16,30.5c0.8,0,1.5,0.7' +
          ',1.5,1.5s-0.7,1.5-1.5,1.5s-1.5-0.7-1.5-1.5 S15.2,30.5,16,30.5z"/' +
          '>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'clapperboard'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#3F5' +
          '1B5" d="M43.4,8.3L4,15l-0.3-2c-0.4-2.2,1.1-4.2,3.3-4.6l31.6-5.3c' +
          '2.2-0.4,4.2,1.1,4.6,3.3L43.4,8.3z"/>'#13#10'    <path fill="#3F51B5" d' +
          '="M40,41H8c-2.2,0-4-1.8-4-4V15h40v22C44,39.2,42.2,41,40,41z"/>'#13#10 +
          '    <g fill="#9FA8DA">'#13#10'        <polygon points="18.8,6.4 23.7,1' +
          '1.7 27.7,11 22.7,5.7"/>'#13#10'        <polygon points="10.9,7.7 15.8,' +
          '13 19.8,12.3 14.8,7.1"/>'#13#10'        <polygon points="26.7,5.1 31.6' +
          ',10.3 35.5,9.7 30.6,4.4"/>'#13#10'        <polygon points="34.5,3.8 39' +
          '.5,9 43.4,8.3 38.5,3.1"/>'#13#10'    </g>'#13#10'    <circle fill="#9FA8DA" ' +
          'cx="7.5" cy="11.5" r="1.5"/>'#13#10'    <g fill="#9FA8DA">'#13#10'        <p' +
          'olygon points="40,15 36,21 40,21 44,15"/>'#13#10'        <polygon poin' +
          'ts="32,15 28,21 32,21 36,15"/>'#13#10'        <polygon points="24,15 2' +
          '0,21 24,21 28,15"/>'#13#10'        <polygon points="16,15 12,21 16,21 ' +
          '20,15"/>'#13#10'        <polygon points="8,15 4,21 8,21 12,15"/>'#13#10'    ' +
          '</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'clear_filters'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'F57C00" points="29,23 19,23 7,9 41,9"/>'#13#10'    <g fill="#FF9800">'#13 +
          #10'        <polygon points="29,38 19,44 19,23 29,23"/>'#13#10'        <p' +
          'ath d="M41.5,9h-35C5.7,9,5,8.3,5,7.5v0C5,6.7,5.7,6,6.5,6h35C42.3' +
          ',6,43,6.7,43,7.5v0C43,8.3,42.3,9,41.5,9z"/>'#13#10'    </g>'#13#10'    <circ' +
          'le fill="#F44336" cx="38" cy="38" r="10"/>'#13#10'    <rect x="32" y="' +
          '36" fill="#fff" width="12" height="4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'close_up_mode'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#2E7' +
          'D32" d="M5,24c0,18.9,14.8,19,17,19s4,0,4,0S24.1,26.5,5,24z"/>'#13#10' ' +
          '   <rect x="22" y="26" fill="#388E3C" width="4" height="17"/>'#13#10' ' +
          '   <path fill="#C62828" d="M34,16c0,5.1-5.2,8.2-8,8.2s-2-3.1-2-8' +
          '.2s5-9.2,5-9.2S34,10.9,34,16z"/>'#13#10'    <path fill="#C62828" d="M1' +
          '4,16c0,5.1,5.2,8.2,8,8.2s2-3.1,2-8.2s-5-9.2-5-9.2S14,10.9,14,16z' +
          '"/>'#13#10'    <path fill="#E53935" d="M24,27c-2.2-1.6-1.9-4.5,2.4-8.8' +
          'C30.8,13.8,32,7,32,7s5,3.4,5,9C37,21.9,31.3,27,24,27z"/>'#13#10'    <p' +
          'ath fill="#E53935" d="M24,27c2.2-1.6,1.9-4.5-2.4-8.8C17.2,13.8,1' +
          '6,7,16,7s-5,3.4-5,9C11,21.9,16.7,27,24,27z"/>'#13#10'    <path fill="#' +
          'F44336" d="M30,16c0,6.1-2.7,11-6,11s-6-4.9-6-11s6-11,6-11S30,9.9' +
          ',30,16z"/>'#13#10'    <path fill="#4CAF50" d="M22,43c0,0,1.8,0,4,0s17-' +
          '0.1,17-19C23.9,26.5,22,43,22,43z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'cloth'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FF5' +
          '722" d="M6,10v28c0,2.2,1.8,4,4,4h28c2.2,0,4-1.8,4-4V10c0-2.2-1.8' +
          '-4-4-4H10C7.8,6,6,7.8,6,10z"/>'#13#10'    <g fill="#BF360C">'#13#10'        ' +
          '<rect x="6" y="35" width="36" height="2"/>'#13#10'        <rect x="6" ' +
          'y="31" width="36" height="2"/>'#13#10'        <path d="M6.1,39c0.2,0.8' +
          ',0.6,1.5,1.2,2h33.2c0.6-0.5,1-1.2,1.2-2H6.1z"/>'#13#10'        <path d' +
          '="M6.1,9h35.7c-0.2-0.8-0.6-1.5-1.2-2H7.4C6.8,7.5,6.3,8.2,6.1,9z"' +
          '/>'#13#10'        <rect x="6" y="23" width="36" height="2"/>'#13#10'        ' +
          '<rect x="6" y="27" width="36" height="2"/>'#13#10'        <rect x="6" ' +
          'y="15" width="36" height="2"/>'#13#10'        <rect x="6" y="11" width' +
          '="36" height="2"/>'#13#10'        <rect x="6" y="19" width="36" height' +
          '="2"/>'#13#10'    </g>'#13#10'    <g fill="#FF8A65">'#13#10'        <rect x="27" y' +
          '="6" width="2" height="5"/>'#13#10'        <rect x="27" y="13" width="' +
          '2" height="6"/>'#13#10'        <rect x="27" y="29" width="2" height="6' +
          '"/>'#13#10'        <rect x="31" y="6" width="2" height="1"/>'#13#10'        ' +
          '<rect x="19" y="29" width="2" height="6"/>'#13#10'        <rect x="31"' +
          ' y="9" width="2" height="6"/>'#13#10'        <rect x="23" y="6" width=' +
          '"2" height="1"/>'#13#10'        <rect x="23" y="25" width="2" height="' +
          '6"/>'#13#10'        <rect x="23" y="9" width="2" height="6"/>'#13#10'       ' +
          ' <rect x="19" y="21" width="2" height="6"/>'#13#10'        <rect x="23' +
          '" y="17" width="2" height="6"/>'#13#10'        <rect x="23" y="33" wid' +
          'th="2" height="6"/>'#13#10'        <rect x="27" y="21" width="2" heigh' +
          't="6"/>'#13#10'        <rect x="39" y="33" width="2" height="6"/>'#13#10'   ' +
          '     <rect x="39" y="17" width="2" height="6"/>'#13#10'        <rect x' +
          '="39" y="25" width="2" height="6"/>'#13#10'        <path d="M39,6.1V7h' +
          '1.6C40.2,6.6,39.6,6.3,39,6.1z"/>'#13#10'        <rect x="31" y="17" wi' +
          'dth="2" height="6"/>'#13#10'        <path d="M40.6,41H39v0.9C39.6,41.7' +
          ',40.2,41.4,40.6,41z"/>'#13#10'        <rect x="35" y="13" width="2" he' +
          'ight="6"/>'#13#10'        <rect x="31" y="33" width="2" height="6"/>'#13#10 +
          '        <rect x="35" y="29" width="2" height="6"/>'#13#10'        <rec' +
          't x="39" y="9" width="2" height="6"/>'#13#10'        <rect x="35" y="2' +
          '1" width="2" height="6"/>'#13#10'        <rect x="31" y="25" width="2"' +
          ' height="6"/>'#13#10'        <rect x="35" y="37" width="2" height="5"/' +
          '>'#13#10'        <rect x="35" y="6" width="2" height="5"/>'#13#10'        <r' +
          'ect x="31" y="41" width="2" height="1"/>'#13#10'        <rect x="23" y' +
          '="41" width="2" height="1"/>'#13#10'        <rect x="27" y="37" width=' +
          '"2" height="5"/>'#13#10'        <rect x="19" y="37" width="2" height="' +
          '5"/>'#13#10'        <rect x="7" y="17" width="2" height="6"/>'#13#10'       ' +
          ' <path d="M9,41H7.4c0.5,0.4,1,0.7,1.6,0.9V41z"/>'#13#10'        <path ' +
          'd="M7.4,7H9V6.1C8.4,6.3,7.8,6.6,7.4,7z"/>'#13#10'        <rect x="7" y' +
          '="33" width="2" height="6"/>'#13#10'        <rect x="7" y="25" width="' +
          '2" height="6"/>'#13#10'        <rect x="7" y="9" width="2" height="6"/' +
          '>'#13#10'        <rect x="11" y="29" width="2" height="6"/>'#13#10'        <' +
          'rect x="15" y="17" width="2" height="6"/>'#13#10'        <rect x="15" ' +
          'y="33" width="2" height="6"/>'#13#10'        <rect x="15" y="9" width=' +
          '"2" height="6"/>'#13#10'        <rect x="15" y="6" width="2" height="1' +
          '"/>'#13#10'        <rect x="19" y="6" width="2" height="5"/>'#13#10'        ' +
          '<rect x="15" y="25" width="2" height="6"/>'#13#10'        <rect x="15"' +
          ' y="41" width="2" height="1"/>'#13#10'        <rect x="11" y="21" widt' +
          'h="2" height="6"/>'#13#10'        <rect x="11" y="6" width="2" height=' +
          '"5"/>'#13#10'        <rect x="11" y="37" width="2" height="5"/>'#13#10'     ' +
          '   <rect x="19" y="13" width="2" height="6"/>'#13#10'        <rect x="' +
          '11" y="13" width="2" height="6"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'collaboration'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#156' +
          '5C0" d="M25,22h13l6,6V11c0-2.2-1.8-4-4-4H25c-2.2,0-4,1.8-4,4v7C2' +
          '1,20.2,22.8,22,25,22z"/>'#13#10'    <path fill="#2196F3" d="M23,19H10l' +
          '-6,6V8c0-2.2,1.8-4,4-4h15c2.2,0,4,1.8,4,4v7C27,17.2,25.2,19,23,1' +
          '9z"/>'#13#10'    <g fill="#FFA726">'#13#10'        <circle cx="12" cy="31" r' +
          '="5"/>'#13#10'        <circle cx="36" cy="31" r="5"/>'#13#10'    </g>'#13#10'    <' +
          'g fill="#607D8B">'#13#10'        <path d="M20,42c0,0-2.2-4-8-4s-8,4-8,' +
          '4v2h16V42z"/>'#13#10'        <path d="M44,42c0,0-2.2-4-8-4s-8,4-8,4v2h' +
          '16V42z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'collect'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#009688' +
          '">'#13#10'        <rect x="22" y="35" width="4" height="11"/>'#13#10'       ' +
          ' <polygon points="24,29.6 31,38 17,38"/>'#13#10'    </g>'#13#10'    <g fill=' +
          '"#009688">'#13#10'        <rect x="22" y="2" width="4" height="11"/>'#13#10 +
          '        <polygon points="24,18.4 17,10 31,10"/>'#13#10'    </g>'#13#10'    <' +
          'g fill="#009688">'#13#10'        <rect x="2" y="22" width="11" height=' +
          '"4"/>'#13#10'        <polygon points="18.4,24 10,31 10,17"/>'#13#10'    </g>' +
          #13#10'    <g fill="#009688">'#13#10'        <rect x="35" y="22" width="11"' +
          ' height="4"/>'#13#10'        <polygon points="29.6,24 38,17 38,31"/>'#13#10 +
          '    </g>'#13#10'    <circle fill="#F44336" cx="24" cy="24" r="3"/>'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'combo_chart'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#00BCD4' +
          '">'#13#10'        <rect x="37" y="18" width="6" height="24"/>'#13#10'       ' +
          ' <rect x="29" y="26" width="6" height="16"/>'#13#10'        <rect x="2' +
          '1" y="22" width="6" height="20"/>'#13#10'        <rect x="13" y="32" w' +
          'idth="6" height="10"/>'#13#10'        <rect x="5" y="28" width="6" hei' +
          'ght="14"/>'#13#10'    </g>'#13#10'    <g fill="#3F51B5">'#13#10'        <circle cx' +
          '="8" cy="16" r="3"/>'#13#10'        <circle cx="16" cy="18" r="3"/>'#13#10' ' +
          '       <circle cx="24" cy="11" r="3"/>'#13#10'        <circle cx="32" ' +
          'cy="13" r="3"/>'#13#10'        <circle cx="40" cy="9" r="3"/>'#13#10'       ' +
          ' <polygon points="39.1,7.2 31.8,10.9 23.5,8.8 15.5,15.8 8.5,14.1' +
          ' 7.5,17.9 16.5,20.2 24.5,13.2 32.2,15.1 40.9,10.8"/>'#13#10'    </g>'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = 'command_line'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 19.1.1, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<svg version="1.1"  xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"'#13#10#9' vie' +
          'wBox="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pr' +
          'eserve">'#13#10'<g>'#13#10#9'<path fill="#CFD8DC" d="M41,6H7C6.4,6,6,6.4,6,7v' +
          '35h36V7C42,6.4,41.6,6,41,6z"/>'#13#10'</g>'#13#10'<rect x="8" y="13" fill="#' +
          '263238" width="32" height="27"/>'#13#10'<g>'#13#10#9'<path fill="#76FF03" d="' +
          'M22,27.6c-0.1,1.1-0.4,1.9-1,2.5c-0.6,0.6-1.4,0.9-2.5,0.9c-1.1,0-' +
          '2-0.4-2.6-1.1c-0.6-0.7-0.9-1.8-0.9-3.1'#13#10#9#9'v-1.6c0-1.3,0.3-2.4,0.' +
          '9-3.1c0.6-0.7,1.5-1.1,2.6-1.1c1.1,0,1.9,0.3,2.5,0.9c0.6,0.6,0.9,' +
          '1.4,1,2.6h-2c0-0.7-0.1-1.2-0.3-1.4'#13#10#9#9'c-0.2-0.3-0.6-0.4-1.1-0.4c' +
          '-0.5,0-0.9,0.2-1.2,0.6c-0.2,0.4-0.3,1-0.4,1.8v1.8c0,1,0.1,1.6,0.' +
          '3,2c0.2,0.4,0.6,0.5,1.1,0.5'#13#10#9#9'c0.5,0,0.9-0.1,1.1-0.4c0.2-0.3,0.' +
          '3-0.7,0.3-1.4H22z"/>'#13#10#9'<path fill="#76FF03" d="M24,24c0-0.3,0.1-' +
          '0.5,0.3-0.7c0.2-0.2,0.4-0.3,0.7-0.3c0.3,0,0.5,0.1,0.7,0.3c0.2,0.' +
          '2,0.3,0.4,0.3,0.7'#13#10#9#9'c0,0.3-0.1,0.5-0.3,0.7S25.3,25,25,25c-0.3,0' +
          '-0.5-0.1-0.7-0.3S24,24.3,24,24z"/>'#13#10#9'<path fill="#76FF03" d="M24' +
          ',30c0-0.3,0.1-0.5,0.3-0.7c0.2-0.2,0.4-0.3,0.7-0.3c0.3,0,0.5,0.1,' +
          '0.7,0.3c0.2,0.2,0.3,0.4,0.3,0.7'#13#10#9#9'c0,0.3-0.1,0.5-0.3,0.7S25.3,3' +
          '1,25,31c-0.3,0-0.5-0.1-0.7-0.3S24,30.3,24,30z"/>'#13#10#9'<path fill="#' +
          '76FF03" d="M28,21h2l3,10h-2L28,21z"/>'#13#10'</g>'#13#10'<g>'#13#10#9'<circle fill=' +
          '"#90A4AE" cx="13.5" cy="9.5" r="1.5"/>'#13#10#9'<circle fill="#90A4AE" ' +
          'cx="9.5" cy="9.5" r="1.5"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'comments'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#8BC' +
          '34A" d="M37,39H11l-6,6V11c0-3.3,2.7-6,6-6h26c3.3,0,6,2.7,6,6v22C' +
          '43,36.3,40.3,39,37,39z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'compact_camera'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#607' +
          'D8B" d="M40,39H8c-2.2,0-4-1.8-4-4V13c0-2.2,1.8-4,4-4h32c2.2,0,4,' +
          '1.8,4,4v22C44,37.2,42.2,39,40,39z"/>'#13#10'    <circle fill="#455A64"' +
          ' cx="29" cy="24" r="12"/>'#13#10'    <circle fill="#42A5F5" cx="29" cy' +
          '="24" r="9"/>'#13#10'    <path fill="#90CAF9" d="M33.8,21c-1.2-1.4-3-2' +
          '.2-4.8-2.2s-3.6,0.8-4.8,2.2c-0.5,0.5-0.4,1.3,0.1,1.8c0.5,0.5,1.3' +
          ',0.4,1.8-0.1 c1.5-1.7,4.3-1.7,5.8,0c0.3,0.3,0.6,0.4,1,0.4c0.3,0,' +
          '0.6-0.1,0.9-0.3C34.2,22.4,34.3,21.5,33.8,21z"/>'#13#10'    <rect x="8"' +
          ' y="13" fill="#ADD8FB" width="6" height="3"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'conference_call'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#F' +
          'FA726" cx="12" cy="21" r="5"/>'#13#10'    <g fill="#455A64">'#13#10'        ' +
          '<path d="M2,34.7c0,0,2.8-6.3,10-6.3s10,6.3,10,6.3V38H2V34.7z"/>'#13 +
          #10'        <path d="M46,34.7c0,0-2.8-6.3-10-6.3s-10,6.3-10,6.3V38h' +
          '20V34.7z"/>'#13#10'    </g>'#13#10'    <circle fill="#FFB74D" cx="24" cy="17' +
          '" r="6"/>'#13#10'    <path fill="#607D8B" d="M36,34.1c0,0-3.3-7.5-12-7' +
          '.5s-12,7.5-12,7.5V38h24V34.1z"/>'#13#10'    <circle fill="#FFA726" cx=' +
          '"36" cy="21" r="5"/>'#13#10'    <circle fill="#FFA726" cx="12" cy="21"' +
          ' r="5"/>'#13#10'    <circle fill="#FFA726" cx="36" cy="21" r="5"/>'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'crystal_oscillator'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FF9800' +
          '">'#13#10'        <rect x="3" y="28" width="26" height="4"/>'#13#10'        ' +
          '<rect x="3" y="16" width="26" height="4"/>'#13#10'    </g>'#13#10'    <path ' +
          'fill="#2196F3" d="M43,11H20v26h23c1.1,0,2-0.9,2-2V13C45,11.9,44.' +
          '1,11,43,11z"/>'#13#10'    <path fill="#64B5F6" d="M20,9h-2v30h2c1.1,0,' +
          '2-0.9,2-2V11C22,9.9,21.1,9,20,9z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'currency_exchange'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#3' +
          'F51B5" cx="18" cy="18" r="15"/>'#13#10'    <path fill="#FFF59D" d="M20' +
          '.3,16v1.7h-3.8v1.4h3.8v1.7h-3.8c0,0.6,0.1,1.2,0.3,1.6c0.2,0.4,0.' +
          '4,0.8,0.7,1c0.3,0.3,0.7,0.4,1.1,0.6 c0.4,0.1,0.9,0.2,1.4,0.2c0.4' +
          ',0,0.7,0,1.1-0.1c0.4-0.1,0.7-0.1,1-0.3l0.4,2.7c-0.4,0.1-0.9,0.2-' +
          '1.4,0.2c-0.5,0.1-1,0.1-1.5,0.1 c-0.9,0-1.8-0.1-2.6-0.4c-0.8-0.2-' +
          '1.5-0.6-2-1.1c-0.6-0.5-1-1.1-1.4-1.9c-0.3-0.7-0.5-1.6-0.5-2.6h-1' +
          '.9v-1.7h1.9v-1.4h-1.9V16h1.9 c0.1-1,0.3-1.8,0.6-2.6c0.4-0.7,0.8-' +
          '1.4,1.4-1.9c0.6-0.5,1.3-0.9,2.1-1.1c0.8-0.3,1.7-0.4,2.6-0.4c0.4,' +
          '0,0.9,0,1.3,0.1 c0.4,0.1,0.9,0.1,1.3,0.3l-0.4,2.7c-0.3-0.1-0.6-0' +
          '.2-1-0.3c-0.4-0.1-0.7-0.1-1.1-0.1c-0.5,0-1,0.1-1.4,0.2c-0.4,0.1-' +
          '0.8,0.3-1,0.6 c-0.3,0.3-0.5,0.6-0.7,1s-0.3,0.9-0.3,1.5H20.3z"/>'#13 +
          #10'    <circle fill="#4CAF50" cx="30" cy="30" r="15"/>'#13#10'    <path ' +
          'fill="#fff" d="M28.4,27c0.1,0.2,0.2,0.4,0.4,0.6c0.2,0.2,0.4,0.4,' +
          '0.7,0.5c0.3,0.2,0.7,0.3,1.1,0.5c0.7,0.3,1.4,0.6,2,0.9 c0.6,0.3,1' +
          '.1,0.7,1.5,1.1c0.4,0.4,0.8,0.9,1,1.4c0.2,0.5,0.4,1.2,0.4,1.9c0,0' +
          '.7-0.1,1.3-0.3,1.8c-0.2,0.5-0.5,1-0.9,1.4 s-0.9,0.7-1.4,0.9c-0.6' +
          ',0.2-1.2,0.4-1.8,0.5v2.2h-1.8v-2.2c-0.6-0.1-1.2-0.2-1.8-0.4s-1.1' +
          '-0.5-1.5-1c-0.5-0.4-0.8-1-1.1-1.6 c-0.3-0.6-0.4-1.4-0.4-2.3h3.3c' +
          '0,0.5,0.1,1,0.2,1.3c0.1,0.4,0.3,0.6,0.6,0.9c0.2,0.2,0.5,0.4,0.8,' +
          '0.5c0.3,0.1,0.6,0.1,0.9,0.1 c0.4,0,0.7,0,0.9-0.1c0.3-0.1,0.5-0.2' +
          ',0.7-0.4c0.2-0.2,0.3-0.4,0.4-0.6c0.1-0.2,0.1-0.5,0.1-0.8c0-0.3,0' +
          '-0.6-0.1-0.8 c-0.1-0.2-0.2-0.5-0.4-0.7s-0.4-0.4-0.7-0.5c-0.3-0.2' +
          '-0.7-0.3-1.1-0.5c-0.7-0.3-1.4-0.6-2-0.9c-0.6-0.3-1.1-0.7-1.5-1.1' +
          ' c-0.4-0.4-0.8-0.9-1-1.4c-0.2-0.5-0.4-1.2-0.4-1.9c0-0.6,0.1-1.2,' +
          '0.3-1.7c0.2-0.5,0.5-1,0.9-1.4c0.4-0.4,0.9-0.7,1.4-1 c0.5-0.2,1.2' +
          '-0.4,1.8-0.5v-2.4h1.8v2.4c0.6,0.1,1.2,0.3,1.8,0.6c0.5,0.3,1,0.6,' +
          '1.3,1.1c0.4,0.4,0.7,1,0.9,1.6c0.2,0.6,0.3,1.3,0.3,2 h-3.3c0-0.9-' +
          '0.2-1.6-0.6-2c-0.4-0.4-0.9-0.7-1.5-0.7c-0.3,0-0.6,0.1-0.9,0.2c-0' +
          '.2,0.1-0.4,0.2-0.6,0.4c-0.2,0.2-0.3,0.4-0.3,0.6 c-0.1,0.2-0.1,0.' +
          '5-0.1,0.8C28.3,26.5,28.4,26.8,28.4,27z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'cursor'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#E0E' +
          '0E0" d="M27.8,39.7c-0.1,0-0.2,0-0.4-0.1c-0.2-0.1-0.4-0.3-0.6-0.5' +
          'l-3.7-8.6l-4.5,4.2C18.5,34.9,18.3,35,18,35 c-0.1,0-0.3,0-0.4-0.1' +
          'C17.3,34.8,17,34.4,17,34l0-22c0-0.4,0.2-0.8,0.6-0.9C17.7,11,17.9' +
          ',11,18,11c0.2,0,0.5,0.1,0.7,0.3l16,15 c0.3,0.3,0.4,0.7,0.3,1.1c-' +
          '0.1,0.4-0.5,0.6-0.9,0.7l-6.3,0.6l3.9,8.5c0.1,0.2,0.1,0.5,0,0.8c-' +
          '0.1,0.2-0.3,0.5-0.5,0.6l-2.9,1.3 C28.1,39.7,27.9,39.7,27.8,39.7z' +
          '"/>'#13#10'    <path fill="#212121" d="M18,12l16,15l-7.7,0.7l4.5,9.8l-' +
          '2.9,1.3l-4.3-9.9L18,34L18,12 M18,10c-0.3,0-0.5,0.1-0.8,0.2 c-0.7' +
          ',0.3-1.2,1-1.2,1.8l0,22c0,0.8,0.5,1.5,1.2,1.8C17.5,36,17.8,36,18' +
          ',36c0.5,0,1-0.2,1.4-0.5l3.4-3.2l3.1,7.3 c0.2,0.5,0.6,0.9,1.1,1.1' +
          'c0.2,0.1,0.5,0.1,0.7,0.1c0.3,0,0.5-0.1,0.8-0.2l2.9-1.3c0.5-0.2,0' +
          '.9-0.6,1.1-1.1c0.2-0.5,0.2-1.1,0-1.5 l-3.3-7.2l4.9-0.4c0.8-0.1,1' +
          '.5-0.6,1.7-1.3c0.3-0.7,0.1-1.6-0.5-2.1l-16-15C19,10.2,18.5,10,18' +
          ',10L18,10z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'customer_support'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FFB' +
          '74D" d="M29,43v-4.6l2.6,0.5c2.9,0.6,5.6-1.5,5.8-4.4L38,28l2.9-1.' +
          '2c1-0.4,1.4-1.6,0.8-2.6L38,18 c-0.6-7.6-4.9-15-16-15C10.6,3,5,11' +
          '.4,5,20c0,3.7,1.3,6.9,3.3,9.6c1.8,2.5,2.7,5.5,2.7,8.5l0,4.8H29z"' +
          '/>'#13#10'    <polygon fill="#FF9800" points="29,43 29,38.4 22,37 22,4' +
          '3"/>'#13#10'    <circle fill="#784719" cx="33.5" cy="21.5" r="1.5"/>'#13#10 +
          '    <path fill="#FF5722" d="M21.4,3C12.3,3,5,10.3,5,19.4c0,11.1,' +
          '6,11.4,6,18.6l2.6-0.9c2.1-0.7,3.9-2.3,4.7-4.4l2.8-6.8L27,23v-6 c' +
          '0,0,7-3.8,7-10.3C31,4.2,25.7,3,21.4,3z"/>'#13#10'    <g fill="#546E7A"' +
          '>'#13#10'        <path d="M21,2.1c-0.6,0-1,0.4-1,1v13.9c0,0.6,0.4,1,1,' +
          '1s1-0.4,1-1V3.1C22,2.5,21.6,2.1,21,2.1z"/>'#13#10'        <path d="M36' +
          '.9,31.9c-7.9,0-10.3-4.9-10.4-5.1c-0.2-0.5-0.8-0.7-1.3-0.5c-0.5,0' +
          '.2-0.7,0.8-0.5,1.3 c0.1,0.3,3,6.3,12.2,6.3c0.6,0,1-0.4,1-1S37.4,' +
          '31.9,36.9,31.9z"/>'#13#10'    </g>'#13#10'    <circle fill="#37474F" cx="37"' +
          ' cy="33" r="2"/>'#13#10'    <circle fill="#37474F" cx="21" cy="23" r="' +
          '7"/>'#13#10'    <circle fill="#546E7A" cx="21" cy="23" r="4"/>'#13#10'</svg>' +
          #13#10
      end
      item
        IconName = 'dam'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#81D4FA' +
          '">'#13#10'        <rect x="24" y="28" width="18" height="14"/>'#13#10'      ' +
          '  <rect x="6" y="10" width="12" height="32"/>'#13#10'    </g>'#13#10'    <g ' +
          'fill="#1976D2">'#13#10'        <path d="M16,8h-2c0,1.1-0.9,2-2,2s-2-0.' +
          '9-2-2H8c0,1.1-0.9,2-2,2v2c1.2,0,2.3-0.5,3-1.4c0.7,0.8,1.8,1.4,3,' +
          '1.4 s2.3-0.5,3-1.4c0.7,0.8,1.8,1.4,3,1.4v-2C16.9,10,16,9.1,16,8z' +
          '"/>'#13#10'        <path d="M16,14h-2c0,1.1-0.9,2-2,2s-2-0.9-2-2H8c0,1' +
          '.1-0.9,2-2,2v2c1.2,0,2.3-0.5,3-1.4c0.7,0.8,1.8,1.4,3,1.4 s2.3-0.' +
          '5,3-1.4c0.7,0.8,1.8,1.4,3,1.4v-2C16.9,16,16,15.1,16,14z"/>'#13#10'    ' +
          '    <path d="M16,20h-2c0,1.1-0.9,2-2,2s-2-0.9-2-2H8c0,1.1-0.9,2-' +
          '2,2v2c1.2,0,2.3-0.5,3-1.4c0.7,0.8,1.8,1.4,3,1.4 s2.3-0.5,3-1.4c0' +
          '.7,0.8,1.8,1.4,3,1.4v-2C16.9,22,16,21.1,16,20z"/>'#13#10'        <path' +
          ' d="M16,26h-2c0,1.1-0.9,2-2,2s-2-0.9-2-2H8c0,1.1-0.9,2-2,2v2c1.2' +
          ',0,2.3-0.5,3-1.4c0.7,0.8,1.8,1.4,3,1.4 s2.3-0.5,3-1.4c0.7,0.8,1.' +
          '8,1.4,3,1.4v-2C16.9,28,16,27.1,16,26z"/>'#13#10'        <path d="M16,3' +
          '2h-2c0,1.1-0.9,2-2,2s-2-0.9-2-2H8c0,1.1-0.9,2-2,2v2c1.2,0,2.3-0.' +
          '5,3-1.4c0.7,0.8,1.8,1.4,3,1.4 s2.3-0.5,3-1.4c0.7,0.8,1.8,1.4,3,1' +
          '.4v-2C16.9,34,16,33.1,16,32z"/>'#13#10'        <path d="M16,38h-2c0,1.' +
          '1-0.9,2-2,2s-2-0.9-2-2H8c0,1.1-0.9,2-2,2v2c1.2,0,2.3-0.5,3-1.4c0' +
          '.7,0.8,1.8,1.4,3,1.4 s2.3-0.5,3-1.4c0.7,0.8,1.8,1.4,3,1.4v-2C16.' +
          '9,40,16,39.1,16,38z"/>'#13#10'        <path d="M40,32h-2c0,1.1-0.9,2-2' +
          ',2s-2-0.9-2-2h-2c0,1.1-0.9,2-2,2s-2-0.9-2-2h-2c0,1.1-0.9,2-2,2v2' +
          ' c1.2,0,2.3-0.5,3-1.4c0.7,0.8,1.8,1.4,3,1.4s2.3-0.5,3-1.4c0.7,0.' +
          '8,1.8,1.4,3,1.4s2.3-0.5,3-1.4c0.7,0.8,1.8,1.4,3,1.4v-2 C40.9,34,' +
          '40,33.1,40,32z"/>'#13#10'        <path d="M40,26h-2c0,1.1-0.9,2-2,2s-2' +
          '-0.9-2-2h-2c0,1.1-0.9,2-2,2s-2-0.9-2-2h-2c0,1.1-0.9,2-2,2v2 c1.2' +
          ',0,2.3-0.5,3-1.4c0.7,0.8,1.8,1.4,3,1.4s2.3-0.5,3-1.4c0.7,0.8,1.8' +
          ',1.4,3,1.4s2.3-0.5,3-1.4c0.7,0.8,1.8,1.4,3,1.4v-2 C40.9,28,40,27' +
          '.1,40,26z"/>'#13#10'        <path d="M40,38h-2c0,1.1-0.9,2-2,2s-2-0.9-' +
          '2-2h-2c0,1.1-0.9,2-2,2v2c1.2,0,2.3-0.5,3-1.4c0.7,0.8,1.8,1.4,3,1' +
          '.4 s2.3-0.5,3-1.4c0.7,0.8,1.8,1.4,3,1.4v-2C40.9,40,40,39.1,40,38' +
          'z"/>'#13#10'    </g>'#13#10'    <path fill="#455A64" d="M25.1,9.2L31.5,42H18' +
          'V6h3.2C23.1,6,24.8,7.4,25.1,9.2z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'data_sheet'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#90C' +
          'AF9" d="M39,16v7h-6v-7h-2v7h-6v-7h-2v7h-7v2h7v6h-7v2h7v6h-7v2h25' +
          'V16H39z M39,25v6h-6v-6H39z M25,25h6v6h-6V25z M25,33h6v6h-6V33z M' +
          '33,39v-6h6v6H33z"/>'#13#10'    <polygon fill="#00BCD4" points="40,8 8,' +
          '8 8,40 16,40 16,16 40,16"/>'#13#10'    <path fill="#0097A7" d="M7,7v34' +
          'h10V17h24V7H7z M9,23v-6h6v6H9z M15,25v6H9v-6H15z M17,9h6v6h-6V9z' +
          ' M25,9h6v6h-6V9z M15,9v6H9V9H15z M9,39v-6h6v6H9z M39,15h-6V9h6V1' +
          '5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'debt'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FFB' +
          '74D" d="M10,12c-2.8,0-5-2.2-5-5s2.2-5,5-5s5,2.2,5,5S12.8,12,10,1' +
          '2z"/>'#13#10'    <path fill="#607D8B" d="M2,22v8l3,2l1,14h8l1-14l3-2v-' +
          '8c0-4.4-3.6-8-8-8h0C5.6,14,2,17.6,2,22z"/>'#13#10'    <g fill="#263238' +
          '">'#13#10'        <path d="M22.4,40.4c-0.6,2.5-1,3.6-2.4,3.6c-0.6,0-1.' +
          '2-0.5-1.9-1.1c-1-0.8-2.2-1.9-4.1-1.9v2c1.1,0,1.9,0.7,2.8,1.4 c0.' +
          '9,0.7,1.9,1.6,3.2,1.6c3.1,0,3.8-2.9,4.4-5.2C25,38.2,25.4,37,27,3' +
          '7v-2C23.7,35,22.9,38.1,22.4,40.4z"/>'#13#10'        <polygon points="1' +
          '4.4,40 10,40 10,44 14.1,44"/>'#13#10'    </g>'#13#10'    <circle fill="#4CAF' +
          '50" cx="36" cy="36" r="10"/>'#13#10'    <path fill="#fff" d="M35,34c0.' +
          '1,0.2,0.1,0.3,0.3,0.4c0.1,0.1,0.3,0.2,0.5,0.4c0.2,0.1,0.5,0.2,0.' +
          '8,0.3c0.5,0.2,0.9,0.4,1.3,0.6 c0.4,0.2,0.7,0.4,1,0.7c0.3,0.3,0.5' +
          ',0.6,0.7,0.9c0.2,0.4,0.2,0.8,0.2,1.3c0,0.4-0.1,0.8-0.2,1.2c-0.1,' +
          '0.4-0.3,0.7-0.6,0.9 c-0.3,0.3-0.6,0.5-0.9,0.6c-0.4,0.2-0.8,0.3-1' +
          '.2,0.3v1.5h-1.2v-1.5c-0.4,0-0.8-0.1-1.2-0.3c-0.4-0.2-0.7-0.4-1-0' +
          '.6 c-0.3-0.3-0.5-0.6-0.7-1.1c-0.2-0.4-0.3-0.9-0.3-1.5h2.2c0,0.4,' +
          '0,0.7,0.1,0.9c0.1,0.2,0.2,0.4,0.4,0.6c0.2,0.1,0.3,0.2,0.5,0.3 c0' +
          '.2,0.1,0.4,0.1,0.6,0.1c0.2,0,0.4,0,0.6-0.1c0.2-0.1,0.3-0.2,0.4-0' +
          '.3c0.1-0.1,0.2-0.3,0.3-0.4c0.1-0.2,0.1-0.3,0.1-0.5 c0-0.2,0-0.4-' +
          '0.1-0.6c-0.1-0.2-0.1-0.3-0.3-0.4c-0.1-0.1-0.3-0.3-0.5-0.4c-0.2-0' +
          '.1-0.4-0.2-0.7-0.3c-0.5-0.2-0.9-0.4-1.3-0.6 c-0.4-0.2-0.7-0.4-1-' +
          '0.7c-0.3-0.3-0.5-0.6-0.7-0.9c-0.2-0.4-0.2-0.8-0.2-1.3c0-0.4,0.1-' +
          '0.8,0.2-1.2c0.1-0.3,0.3-0.7,0.6-0.9 c0.3-0.3,0.6-0.5,0.9-0.6c0.4' +
          '-0.2,0.8-0.3,1.2-0.3v-1.6h1.2v1.6c0.4,0.1,0.8,0.2,1.2,0.4c0.4,0.' +
          '2,0.6,0.4,0.9,0.7 c0.2,0.3,0.4,0.6,0.6,1c0.1,0.4,0.2,0.9,0.2,1.4' +
          'h-2.2c0-0.6-0.1-1-0.4-1.3c-0.2-0.3-0.6-0.4-1-0.4c-0.2,0-0.4,0-0.' +
          '6,0.1 c-0.2,0.1-0.3,0.2-0.4,0.3C35.1,32.7,35,32.8,35,33s-0.1,0.3' +
          '-0.1,0.5C34.9,33.7,34.9,33.9,35,34z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'department'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'C5CAE9" points="42,42 6,42 6,9 24,2 42,9"/>'#13#10'    <rect x="6" y="' +
          '42" fill="#9FA8DA" width="36" height="2"/>'#13#10'    <rect x="20" y="' +
          '35" fill="#BF360C" width="8" height="9"/>'#13#10'    <g fill="#1565C0"' +
          '>'#13#10'        <rect x="31" y="27" width="6" height="5"/>'#13#10'        <' +
          'rect x="21" y="27" width="6" height="5"/>'#13#10'        <rect x="11" ' +
          'y="27" width="6" height="5"/>'#13#10'        <rect x="31" y="35" width' +
          '="6" height="5"/>'#13#10'        <rect x="11" y="35" width="6" height=' +
          '"5"/>'#13#10'        <rect x="31" y="19" width="6" height="5"/>'#13#10'     ' +
          '   <rect x="21" y="19" width="6" height="5"/>'#13#10'        <rect x="' +
          '11" y="19" width="6" height="5"/>'#13#10'        <rect x="31" y="11" w' +
          'idth="6" height="5"/>'#13#10'        <rect x="21" y="11" width="6" hei' +
          'ght="5"/>'#13#10'        <rect x="11" y="11" width="6" height="5"/>'#13#10' ' +
          '   </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'deployment'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#B0B' +
          'EC5" d="M37,42H5V32h32c2.8,0,5,2.2,5,5v0C42,39.8,39.8,42,37,42z"' +
          '/>'#13#10'    <path fill="#37474F" d="M10,34c-1.7,0-3,1.3-3,3s1.3,3,3,' +
          '3s3-1.3,3-3S11.7,34,10,34z M10,38c-0.6,0-1-0.4-1-1c0-0.6,0.4-1,1' +
          '-1 s1,0.4,1,1C11,37.6,10.6,38,10,38z"/>'#13#10'    <path fill="#37474F' +
          '" d="M19,34c-1.7,0-3,1.3-3,3s1.3,3,3,3s3-1.3,3-3S20.7,34,19,34z ' +
          'M19,38c-0.6,0-1-0.4-1-1c0-0.6,0.4-1,1-1 s1,0.4,1,1C20,37.6,19.6,' +
          '38,19,38z"/>'#13#10'    <path fill="#37474F" d="M37,34c-1.7,0-3,1.3-3,' +
          '3s1.3,3,3,3s3-1.3,3-3S38.7,34,37,34z M37,38c-0.6,0-1-0.4-1-1c0-0' +
          '.6,0.4-1,1-1 s1,0.4,1,1C38,37.6,37.6,38,37,38z"/>'#13#10'    <path fil' +
          'l="#37474F" d="M28,34c-1.7,0-3,1.3-3,3s1.3,3,3,3s3-1.3,3-3S29.7,' +
          '34,28,34z M28,38c-0.6,0-1-0.4-1-1c0-0.6,0.4-1,1-1 s1,0.4,1,1C29,' +
          '37.6,28.6,38,28,38z"/>'#13#10'    <path fill="#FF9800" d="M35,31H11c-1' +
          '.1,0-2-0.9-2-2V7c0-1.1,0.9-2,2-2h24c1.1,0,2,0.9,2,2v22C37,30.1,3' +
          '6.1,31,35,31z"/>'#13#10'    <path fill="#8A5100" d="M26.5,13h-7c-0.8,0' +
          '-1.5-0.7-1.5-1.5v0c0-0.8,0.7-1.5,1.5-1.5h7c0.8,0,1.5,0.7,1.5,1.5' +
          'v0 C28,12.3,27.3,13,26.5,13z"/>'#13#10'    <path fill="#607D8B" d="M37' +
          ',31H5v2h32c2.2,0,4,1.8,4,4s-1.8,4-4,4H5v2h32c3.3,0,6-2.7,6-6S40.' +
          '3,31,37,31z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'diploma_1'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="4" y="9' +
          '" fill="#E8EAF6" width="40" height="30"/>'#13#10'    <g fill="#5C6BC0"' +
          '>'#13#10'        <polygon points="30,34 32.8,34 27.8,29 25,31.8 30,36.' +
          '8"/>'#13#10'        <polygon points="18,34 15.2,34 20.2,29 23,31.8 18,' +
          '36.8"/>'#13#10'    </g>'#13#10'    <rect x="11" y="15" fill="#9FA8DA" width=' +
          '"26" height="4"/>'#13#10'    <path fill="#9FA8DA" d="M24,23c-2.8,0-5,2' +
          '.2-5,5s2.2,5,5,5s5-2.2,5-5S26.8,23,24,23z M24,31c-1.7,0-3-1.3-3-' +
          '3s1.3-3,3-3s3,1.3,3,3 S25.7,31,24,31z"/>'#13#10'    <path fill="#9FA8D' +
          'A" d="M3,8v32h42V8H3z M43,35c-1.7,0-3,1.3-3,3H8c0-1.7-1.3-3-3-3V' +
          '13c1.7,0,3-1.3,3-3h32c0,1.7,1.3,3,3,3V35z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'diploma_2'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="9" y="4' +
          '" fill="#FBE9E7" width="30" height="40"/>'#13#10'    <g fill="#F4511E"' +
          '>'#13#10'        <polygon points="30,37 32.8,37 27.8,32 25,34.8 30,39.' +
          '8"/>'#13#10'        <polygon points="18,37 15.2,37 20.2,32 23,34.8 18,' +
          '39.8"/>'#13#10'    </g>'#13#10'    <rect x="15" y="13" fill="#FF8A65" width=' +
          '"18" height="4"/>'#13#10'    <rect x="15" y="20" fill="#FF8A65" width=' +
          '"18" height="2"/>'#13#10'    <path fill="#FF8A65" d="M24,26c-2.8,0-5,2' +
          '.2-5,5s2.2,5,5,5s5-2.2,5-5S26.8,26,24,26z M24,34c-1.7,0-3-1.3-3-' +
          '3s1.3-3,3-3s3,1.3,3,3 S25.7,34,24,34z"/>'#13#10'    <path fill="#FF8A6' +
          '5" d="M8,3v42h32V3H8z M38,40c-1.7,0-3,1.3-3,3H13c0-1.7-1.3-3-3-3' +
          'V8c1.7,0,3-1.3,3-3h22c0,1.7,1.3,3,3,3V40z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'display'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#80D' +
          'EEA" d="M40,41H8c-2.2,0-4-1.8-4-4V11c0-2.2,1.8-4,4-4h32c2.2,0,4,' +
          '1.8,4,4v26C44,39.2,42.2,41,40,41z"/>'#13#10'    <g fill="#2962FF">'#13#10'  ' +
          '      <polygon points="36,17 31,17 29,15 31,13 36,13 38,15"/>'#13#10' ' +
          '       <polygon points="36,35 31,35 29,33 31,31 36,31 38,33"/>'#13#10 +
          '        <polygon points="37,30 37,18 39,16 41,18 41,30 39,32"/>'#13 +
          #10'        <polygon points="26,30 26,18 28,16 30,18 30,30 28,32"/>' +
          #13#10'        <polygon points="17,17 12,17 10,15 12,13 17,13 19,15"/' +
          '>'#13#10'        <polygon points="17,35 12,35 10,33 12,31 17,31 19,33"' +
          '/>'#13#10'        <polygon points="18,30 18,18 20,16 22,18 22,30 20,32' +
          '"/>'#13#10'        <polygon points="7,30 7,18 9,16 11,18 11,30 9,32"/>' +
          #13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'document'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '90CAF9" points="40,45 8,45 8,3 30,3 40,13"/>'#13#10'    <polygon fill=' +
          '"#E1F5FE" points="38.5,14 29,14 29,4.5"/>'#13#10'    <g fill="#1976D2"' +
          '>'#13#10'        <rect x="16" y="21" width="17" height="2"/>'#13#10'        ' +
          '<rect x="16" y="25" width="13" height="2"/>'#13#10'        <rect x="16' +
          '" y="29" width="17" height="2"/>'#13#10'        <rect x="16" y="33" wi' +
          'dth="13" height="2"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'donate'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'E69329" points="11.7,21.6 16.8,31.5 26.3,27.6 30.7,14.9 15.9,15.' +
          '7"/>'#13#10'    <circle fill="#546E7A" cx="15" cy="36" r="7.8"/>'#13#10'    ' +
          '<g fill="#90A4AE">'#13#10'        <path d="M15,27c-5,0-9,4-9,9c0,5,4,9' +
          ',9,9s9-4,9-9C24,31,20,27,15,27z M15,43c-3.9,0-7-3.1-7-7c0-3.9,3.' +
          '1-7,7-7 s7,3.1,7,7C22,39.9,18.9,43,15,43z"/>'#13#10'        <rect x="1' +
          '4" y="33" width="2" height="8"/>'#13#10'    </g>'#13#10'    <g fill="#FFB74D' +
          '">'#13#10'        <path d="M12.9,36L12.9,36c1,1.9,3.2,2.7,5.1,1.7l16.5' +
          '-8.5c1-0.5,1.7-1.2,2.2-1.9c1.7-3.2,5.6-10.7,8.2-17.2 l-18.2,8.7L' +
          '21.9,26l-6.8,3.6C12.5,30.9,11.7,33.8,12.9,36z"/>'#13#10'        <path ' +
          'd="M30.2,3L13.7,9.3c-0.7,0.2-1.5,1-2.2,1.7l-5.6,7.5c-1,1.5-1.2,3' +
          '.4-0.5,5.1c0.4,1,1.7,3.4,3.1,6.1 c1.6-1.7,3.9-2.7,6.5-2.7c0.4,0,' +
          '0.9,0,1.3,0.1l-2.1-4.2l4.6-4.1h8c0,0,15.5-2.2,18.2-8.7L30.2,3z"/' +
          '>'#13#10'    </g>'#13#10'    <path fill="#FFCDD2" d="M18.2,36c-1.3,0.6-2.8,0' +
          '-3.3-1.3c-0.6-1.3,0-2.8,1.3-3.3C17.4,30.8,19.4,35.4,18.2,36z"/>'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'doughnut_chart'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#00B' +
          'CD4" d="M24,30c-3.3,0-6-2.7-6-6s2.7-6,6-6V5C13.5,5,5,13.5,5,24s8' +
          '.5,19,19,19c4.4,0,8.5-1.5,11.8-4.1l-8-10.2 C26.7,29.5,25.4,30,24' +
          ',30z"/>'#13#10'    <path fill="#448AFF" d="M30,24h13c0-10.5-8.5-19-19-' +
          '19v13C27.3,18,30,20.7,30,24z"/>'#13#10'    <path fill="#3F51B5" d="M43' +
          ',24H30c0,1.9-0.9,3.6-2.3,4.7l8,10.2C40.2,35.4,43,30,43,24z"/>'#13#10'<' +
          '/svg>'#13#10
      end
      item
        IconName = 'down'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#3F51B5' +
          '">'#13#10'        <polygon points="24,44 12.3,30 35.7,30"/>'#13#10'        <' +
          'rect x="20" y="6" width="8" height="27"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'down_left'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '3F51B5" points="4,29 18,17.3 18,40.7"/>'#13#10'    <path fill="#3F51B5' +
          '" d="M42,21V8h-8v13c0,2.2-1.8,4-4,4H13v8h17C36.6,33,42,27.6,42,2' +
          '1z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'down_right'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '3F51B5" points="44,29 30,17.3 30,40.7"/>'#13#10'    <path fill="#3F51B' +
          '5" d="M6,21V8h8v13c0,2.2,1.8,4,4,4h17v8H18C11.4,33,6,27.6,6,21z"' +
          '/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'download'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#1565C0' +
          '">'#13#10'        <polygon points="24,37.1 13,24 35,24"/>'#13#10'        <re' +
          'ct x="20" y="4" width="8" height="4"/>'#13#10'        <rect x="20" y="' +
          '10" width="8" height="4"/>'#13#10'        <rect x="20" y="16" width="8' +
          '" height="11"/>'#13#10'        <rect x="6" y="40" width="36" height="4' +
          '"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'dribbble'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 15.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://w' +
          'ww.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#13#10'<svg version="1.1"  ' +
          'xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.or' +
          'g/1999/xlink" x="0px" y="0px"'#13#10#9' width="48px" height="48px" view' +
          'Box="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pre' +
          'serve">'#13#10'<path fill="#FF4081" d="M42,37c0,2.762-2.238,5-5,5H11c-' +
          '2.761,0-5-2.238-5-5V11c0-2.762,2.239-5,5-5h26c2.762,0,5,2.238,5,' +
          '5V37z"/>'#13#10'<path fill="#FFFFFF" d="M33.061,26.273c-0.703-0.221-1.' +
          '464,0.175-1.686,0.895c-0.824,2.658-2.316,5.419-2.993,5.57'#13#10#9'c-0.' +
          '507,0-1.236-0.43-1.958-1.44c1.674-3.594,2.551-8.106,2.551-11.118' +
          'c0-8.413-2.124-10.18-3.908-10.18'#13#10#9'c-3.757,0-3.8,9.912-3.8,10.01' +
          '2c0,1.166,0.042,2.248,0.121,3.256c-0.518-0.189-1.113-0.319-1.77-' +
          '0.319'#13#10#9'c-3.86,0-5.618,3.809-5.618,7.347C14,33.63,15.871,37,20.0' +
          '46,37c1.972,0,3.634-1.291,4.975-3.221'#13#10#9'c1.188,1.235,2.432,1.696' +
          ',3.36,1.696c2.923,0,4.858-5.233,5.556-7.486C34.16,27.27,33.767,2' +
          '6.502,33.061,26.273z M20.048,34.264'#13#10#9'c-3.031,0-3.36-2.775-3.36-' +
          '3.969c0-0.188,0.034-4.611,2.932-4.611c1.144,0,2.022,0.885,2.022,' +
          '0.885'#13#10#9'c0.065,0.07,0.137,0.131,0.212,0.184c0.375,1.904,0.904,3.' +
          '426,1.516,4.632C22.366,33.123,21.203,34.264,20.048,34.264z'#13#10#9' M2' +
          '4.901,27.926c-0.559-1.93-0.946-4.521-0.946-7.914c0-3.126,0.666-6' +
          '.068,1.219-7.073c0.424,0.644,1.115,2.65,1.115,7.241'#13#10#9'C26.289,22' +
          '.616,25.75,25.446,24.901,27.926z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'dvd_logo'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 15.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://w' +
          'ww.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#13#10'<svg version="1.1"  ' +
          'xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.or' +
          'g/1999/xlink" x="0px" y="0px"'#13#10#9' width="48px" height="48px" view' +
          'Box="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pre' +
          'serve">'#13#10'<path fill="#42A5F5" d="M24.002,27c-12.154,0-22,1.343-2' +
          '2,3.006c0,1.653,9.845,2.994,22,2.994c12.156,0,22-1.341,22-2.994'#13 +
          #10#9'C46.002,28.343,36.158,27,24.002,27z M24.002,30.972c-2.863,0-5.' +
          '191-0.494-5.191-1.104c0-0.609,2.329-1.104,5.191-1.104'#13#10#9'c2.862,0' +
          ',5.193,0.495,5.193,1.104C29.195,30.478,26.864,30.972,24.002,30.9' +
          '72z"/>'#13#10'<path fill="#1565C0" d="M21.29,15l2.371,6.43L29.25,15h9.' +
          '486c4.647,0,7.906,2.148,7.158,4.904c-0.745,2.756-5.178,4.904-9.8' +
          '03,4.904'#13#10#9'h-6.295c0,0,0.141-0.043,0.172-0.126c0.246-0.944,1.707' +
          '-6.264,1.725-6.347c0.02-0.102-0.105-0.133-0.105-0.133h4.572'#13#10#9'c0' +
          ',0-0.088-0.006-0.125,0.133c-0.023,0.078-0.947,3.429-1.145,4.176c' +
          '-0.023,0.094-0.162,0.139-0.162,0.139h1.094'#13#10#9'c2.594,0,5.047-0.82' +
          '8,5.563-2.748c0.473-1.752-1.244-2.746-4.039-2.746h-1.014l-4.375,' +
          '0.004l-10.043,9.932l-3.845-9.754'#13#10#9'c0,0-0.036-0.066-0.065-0.147c' +
          '-0.014-0.026-0.108-0.106-0.206-0.063c-0.065,0.036-0.074,0.117-0.' +
          '066,0.146'#13#10#9'c0.036,0.066,0.042,0.08,0.048,0.111c0.569,0.93,0.467' +
          ',2.009,0.33,2.52c-0.774,2.75-5.186,4.904-9.812,4.904H2.002'#13#10#9'c0,' +
          '0,0.149-0.043,0.172-0.126c0.254-0.946,1.717-6.294,1.726-6.347c0.' +
          '018-0.09-0.099-0.133-0.099-0.133h4.604'#13#10#9'c0,0-0.132,0.037-0.158,' +
          '0.131c-0.024,0.078-0.954,3.432-1.151,4.178c-0.023,0.094-0.178,0.' +
          '139-0.178,0.139h1.118'#13#10#9'c2.597,0,5.032-0.828,5.547-2.748c0.472-1' +
          '.752-1.23-2.746-4.021-2.746H8.539h-4.45c0,0,0.125-0.059,0.147-0.' +
          '139'#13#10#9'c0.123-0.443,0.497-1.834,0.515-1.899C4.771,15.047,4.646,15' +
          ',4.646,15H21.29L21.29,15z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'electrical_sensor'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#B' +
          '2EBF2" cx="32" cy="24" r="9"/>'#13#10'    <path fill="#4DD0E1" d="M32,' +
          '12c-6.6,0-12,5.4-12,12c0,6.6,5.4,12,12,12s12-5.4,12-12C44,17.4,3' +
          '8.6,12,32,12z M32,32 c-4.4,0-8-3.6-8-8s3.6-8,8-8s8,3.6,8,8S36.4,' +
          '32,32,32z"/>'#13#10'    <g fill="#3F51B5">'#13#10'        <polygon points="2' +
          '5.4,22 19.8,5.1 13.6,27.7 11.4,22 4,22 4,26 8.6,26 14.4,40.3 20.' +
          '2,18.9 22.6,26 30,26 30,22"/>'#13#10'        <circle cx="32" cy="24" r' +
          '="4"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'electrical_threshold'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="3" y="1' +
          '2" fill="#80DEEA" width="42" height="24"/>'#13#10'    <rect x="3" y="2' +
          '3" fill="#03A9F4" width="42" height="2"/>'#13#10'    <path fill="none"' +
          ' stroke="#3F51B5" stroke-width="4" stroke-miterlimit="10" d="M4,' +
          '18l4.5-1.5c0.9-0.3,1.9,0.1,2.3,0.9l8.7,14.3 c0.7,1.2,2.4,1.3,3.2' +
          ',0.2l2.3-2.8c0.5-0.6,1.4-0.9,2.2-0.6l3,1c1,0.3,2.1-0.2,2.5-1.1l4' +
          '.3-10.1c0.5-1.1,1.9-1.6,2.9-0.9l4,2.7"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'electricity'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#00B' +
          'CD4" d="M33.7,5L22,17l15,5L21.3,36.7l5.1,2.8L12,43l2.7-14.8l2.9,' +
          '5.1L27,24l-15-5L25,5H33.7z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'electro_devices'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48">'#13#10'    <path fill="#3F51B5" d="M39,43H9c-2.2,0-4-1.8-4-4V' +
          '9c0-2.2,1.8-4,4-4h30c2.2,0,4,1.8,4,4v30C43,41.2,41.2,43,39,43z"/' +
          '>'#13#10'    <path fill="#80DEEA" d="M33.2,5l-9.8,10.1L36,19.3L22.8,31' +
          '.7l4.3,2.4L15,37l2.3-12.5l2.4,4.3l8-7.8L15,16.8L25.9,5H33.2z"/>'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'electronics'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FF9' +
          '800" d="M44,18v-4H34V4h-4v10h-4V4h-4v10h-4V4h-4v10H4v4h10v4H4v4h' +
          '10v4H4v4h10v10h4V34h4v10h4V34h4v10h4V34h10v-4H34 v-4h10v-4H34v-4' +
          'H44z"/>'#13#10'    <path fill="#4CAF50" d="M8,12v24c0,2.2,1.8,4,4,4h24' +
          'c2.2,0,4-1.8,4-4V12c0-2.2-1.8-4-4-4H12C9.8,8,8,9.8,8,12z"/>'#13#10'   ' +
          ' <path fill="#37474F" d="M31,31H17c-1.1,0-2-0.9-2-2V19c0-1.1,0.9' +
          '-2,2-2h14c1.1,0,2,0.9,2,2v10C33,30.1,32.1,31,31,31z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'empty_battery'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#CFD8DC' +
          '">'#13#10'        <path d="M34,44H14c-1.1,0-2-0.9-2-2V8c0-1.1,0.9-2,2-' +
          '2h20c1.1,0,2,0.9,2,2v34C36,43.1,35.1,44,34,44z"/>'#13#10'        <path' +
          ' d="M28,13h-8c-0.6,0-1-0.4-1-1V5c0-0.6,0.4-1,1-1h8c0.6,0,1,0.4,1' +
          ',1v7C29,12.6,28.6,13,28,13z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'empty_filter'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FFCC80' +
          '">'#13#10'        <polygon points="29,23 19,23 7,9 41,9"/>'#13#10'        <p' +
          'olygon points="29,38 19,44 19,23 29,23"/>'#13#10'        <path d="M41.' +
          '5,9h-35C5.7,9,5,8.3,5,7.5v0C5,6.7,5.7,6,6.5,6h35C42.3,6,43,6.7,4' +
          '3,7.5v0C43,8.3,42.3,9,41.5,9z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'empty_trash'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#B39' +
          'DDB" d="M30.6,44H17.4c-2,0-3.7-1.4-4-3.4L9,11h30l-4.5,29.6C34.2,' +
          '42.6,32.5,44,30.6,44z"/>'#13#10'    <path fill="#7E57C2" d="M38,13H10c' +
          '-1.1,0-2-0.9-2-2v0c0-1.1,0.9-2,2-2h28c1.1,0,2,0.9,2,2v0C40,12.1,' +
          '39.1,13,38,13z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'end_call'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#F44' +
          '336" d="M43.5,16.8l-2.3-2.3c-8.1-7.9-27.5-6.8-34.5,0l-2.3,2.3c-0' +
          '.6,0.6-0.6,1.6,0,2.3l4.6,4.5 c0.6,0.6,1.7,0.6,2.3,0l5.1-4.9L16,1' +
          '3.4c1.6-1.6,14.4-1.6,16,0l-0.3,5.5l4.9,4.7c0.6,0.6,1.7,0.6,2.3,0' +
          'l4.6-4.5 C44.2,18.4,44.2,17.4,43.5,16.8z"/>'#13#10'    <g fill="#B71C1' +
          'C">'#13#10'        <polygon points="24,40.5 16,31 32,31"/>'#13#10'        <r' +
          'ect x="21" y="24" width="6" height="7.5"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'engineering'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#EF6' +
          'C00" d="M37.4,24.6l-11.6-2.2l-3.9-11.2l-3.8,1.3L22,23.6l-7.8,9l3' +
          ',2.6l7.8-9l11.6,2.2L37.4,24.6z"/>'#13#10'    <g fill="#FF9800">'#13#10'     ' +
          '   <path d="M24,19c-2.8,0-5,2.2-5,5c0,2.8,2.2,5,5,5s5-2.2,5-5C29' +
          ',21.2,26.8,19,24,19z M24,26c-1.1,0-2-0.9-2-2 c0-1.1,0.9-2,2-2s2,' +
          '0.9,2,2C26,25.1,25.1,26,24,26z"/>'#13#10'        <path d="M40.7,27c0.2' +
          '-1,0.3-2,0.3-3c0-1-0.1-2-0.3-3l3.3-2.4c0.4-0.3,0.6-0.9,0.3-1.4L4' +
          '0,9.8 c-0.3-0.5-0.8-0.7-1.3-0.4L35,11c-1.5-1.3-3.3-2.3-5.2-3l-0.' +
          '4-4.1c-0.1-0.5-0.5-0.9-1-0.9h-8.6c-0.5,0-1,0.4-1,0.9L18.2,8 c-1.' +
          '9,0.7-3.7,1.7-5.2,3L9.3,9.3C8.8,9.1,8.2,9.3,8,9.8l-4.3,7.4c-0.3,' +
          '0.5-0.1,1.1,0.3,1.4L7.3,21C7.1,22,7,23,7,24 c0,1,0.1,2,0.3,3L4,2' +
          '9.4c-0.4,0.3-0.6,0.9-0.3,1.4L8,38.2c0.3,0.5,0.8,0.7,1.3,0.4L13,3' +
          '7c1.5,1.3,3.3,2.3,5.2,3l0.4,4.1 c0.1,0.5,0.5,0.9,1,0.9h8.6c0.5,0' +
          ',1-0.4,1-0.9l0.4-4.1c1.9-0.7,3.7-1.7,5.2-3l3.7,1.7c0.5,0.2,1.1,0' +
          ',1.3-0.4l4.3-7.4 c0.3-0.5,0.1-1.1-0.3-1.4L40.7,27z M24,35c-6.1,0' +
          '-11-4.9-11-11c0-6.1,4.9-11,11-11s11,4.9,11,11C35,30.1,30.1,35,24' +
          ',35z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'entering_heaven_alive'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="17" y="' +
          '29" fill="#039BE5" width="14" height="2"/>'#13#10'    <rect x="13" y="' +
          '33" fill="#039BE5" width="22" height="2"/>'#13#10'    <rect x="9" y="3' +
          '7" fill="#039BE5" width="30" height="2"/>'#13#10'    <rect x="5" y="41' +
          '" fill="#039BE5" width="38" height="2"/>'#13#10'    <path fill="#81D4F' +
          'A" d="M35,13c-0.4,0-0.8,0-1.2,0.1C32.9,8.5,28.9,5,24,5c-4.1,0-7.' +
          '6,2.5-9.2,6c-0.3,0-0.5,0-0.8,0 c-4.4,0-8,3.6-8,8s3.6,8,8,8c2.4,0' +
          ',18.5,0,21,0c3.9,0,7-3.1,7-7C42,16.1,38.9,13,35,13z"/>'#13#10'    <pat' +
          'h fill="#039BE5" d="M28,21c0-2.2-1.8-4-4-4s-4,1.8-4,4c0,0.5,0,6,' +
          '0,6h8C28,27,28,21.5,28,21z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'expand'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '2196F3" points="43,17.1 39.9,14 24,29.9 8.1,14 5,17.1 24,36"/>'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = 'export'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FFC' +
          'CBC" d="M7,40V8c0-2.2,1.8-4,4-4h24c2.2,0,4,1.8,4,4v32c0,2.2-1.8,' +
          '4-4,4H11C8.8,44,7,42.2,7,40z"/>'#13#10'    <g fill="#FF5722">'#13#10'       ' +
          ' <polygon points="42.7,24 32,33 32,15"/>'#13#10'        <rect x="14" y' +
          '="21" width="23" height="6"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'external'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#B' +
          '2DFDB" cx="24" cy="31" r="14"/>'#13#10'    <g fill="#009688">'#13#10'       ' +
          ' <polygon points="24,3.3 33,14 15,14"/>'#13#10'        <rect x="21" y=' +
          '"11" width="6" height="23"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'factory'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#BF3' +
          '60C" d="M41.2,5h-7.3L32,43h11L41.2,5z"/>'#13#10'    <path fill="#E64A1' +
          '9" d="M33,23h-4v-6l-12,6v-6L5,23v20h28V23z"/>'#13#10'    <rect x="9" y' +
          '="27" fill="#FFC107" width="4" height="4"/>'#13#10'    <rect x="17" y=' +
          '"27" fill="#FFC107" width="4" height="4"/>'#13#10'    <rect x="25" y="' +
          '27" fill="#FFC107" width="4" height="4"/>'#13#10'    <rect x="9" y="35' +
          '" fill="#FFC107" width="4" height="4"/>'#13#10'    <rect x="17" y="35"' +
          ' fill="#FFC107" width="4" height="4"/>'#13#10'    <rect x="25" y="35" ' +
          'fill="#FFC107" width="4" height="4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'factory_breakdown'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'E64A19" points="29,23 29,17 21,21 21,23 17,23 17,27 13,27 13,23 ' +
          '5,23 5,43 33,43 33,23"/>'#13#10'    <rect x="25" y="27" fill="#992B0A"' +
          ' width="4" height="4"/>'#13#10'    <rect x="9" y="35" fill="#992B0A" w' +
          'idth="4" height="4"/>'#13#10'    <rect x="25" y="35" fill="#992B0A" wi' +
          'dth="4" height="4"/>'#13#10'    <rect x="17" y="35" fill="#992B0A" wid' +
          'th="4" height="4"/>'#13#10'    <rect x="17" y="27" fill="#992B0A" widt' +
          'h="4" height="4"/>'#13#10'    <polygon fill="#BF360C" points="41.2,5 3' +
          '8,5 38,7 36,7 36,9 33.7,9 32,43 43,43"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'faq'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#558' +
          'B2F" d="M15,40h23l6,6V25c0-2.2-1.8-4-4-4H15c-2.2,0-4,1.8-4,4v11C' +
          '11,38.2,12.8,40,15,40z"/>'#13#10'    <path fill="#1B5E20" d="M28.8,32.' +
          '8h-3.6l-0.7,2.1h-2.2l3.7-10h1.9l3.7,10h-2.2L28.8,32.8z M25.7,31.' +
          '2h2.5L27,27.4L25.7,31.2z"/>'#13#10'    <path fill="#8BC34A" d="M33,25H' +
          '10l-6,6V8c0-2.2,1.8-4,4-4h25c2.2,0,4,1.8,4,4v13C37,23.2,35.2,25,' +
          '33,25z"/>'#13#10'    <path fill="#fff" d="M25.4,14.2c0,1-0.2,1.8-0.5,2' +
          '.5c-0.3,0.7-0.7,1.3-1.3,1.7l1.7,1.3L24,20.9l-2.2-1.7c-0.2,0-0.5,' +
          '0.1-0.8,0.1 c-0.6,0-1.2-0.1-1.8-0.3c-0.5-0.2-1-0.6-1.4-1c-0.4-0.' +
          '4-0.7-1-0.9-1.6c-0.2-0.6-0.3-1.3-0.3-2.1v-0.4c0-0.8,0.1-1.5,0.3-' +
          '2.1 c0.2-0.6,0.5-1.2,0.9-1.6c0.4-0.4,0.8-0.8,1.4-1c0.5-0.2,1.1-0' +
          '.3,1.8-0.3c0.6,0,1.2,0.1,1.8,0.3c0.5,0.2,1,0.6,1.4,1 c0.4,0.4,0.' +
          '7,1,0.9,1.6c0.2,0.6,0.3,1.3,0.3,2.1V14.2z M23.2,13.7c0-1.1-0.2-1' +
          '.9-0.6-2.4c-0.4-0.6-0.9-0.8-1.6-0.8 c-0.7,0-1.3,0.3-1.6,0.8c-0.4' +
          ',0.6-0.6,1.4-0.6,2.4v0.5c0,0.5,0.1,1,0.2,1.4c0.1,0.4,0.2,0.8,0.4' +
          ',1c0.2,0.3,0.4,0.5,0.7,0.6 c0.3,0.1,0.6,0.2,0.9,0.2c0.7,0,1.3-0.' +
          '3,1.6-0.8c0.4-0.6,0.6-1.4,0.6-2.5V13.7z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'feed_in'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#219' +
          '6F3" d="M38,24v12c0,1.1-0.9,2-2,2H12c-1.1,0-2-0.9-2-2V24c0-3.3-2' +
          '.7-6-6-6h0v4h0c1.1,0,2,0.9,2,2v12 c0,3.3,2.7,6,6,6h24c3.3,0,6-2.' +
          '7,6-6V24c0-1.1,0.9-2,2-2h0v-4h0C40.7,18,38,20.7,38,24z"/>'#13#10'    <' +
          'g fill="#3F51B5">'#13#10'        <polygon points="38.6,5.6 29,15.2 29,' +
          '28 33,28 33,16.8 41.4,8.4"/>'#13#10'        <polygon points="6.6,8.4 1' +
          '5,16.8 15,28 19,28 19,15.2 9.4,5.6"/>'#13#10'        <polygon points="' +
          '37,27 31,33 25,27"/>'#13#10'        <polygon points="23,27 17,33 11,27' +
          '"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'feedback'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#789' +
          '09C" d="M40,41H8c-2.2,0-4-1.8-4-4l0-20.9c0-1.3,0.6-2.5,1.7-3.3L2' +
          '4,0l18.3,12.8c1.1,0.7,1.7,2,1.7,3.3V37 C44,39.2,42.2,41,40,41z"/' +
          '>'#13#10'    <rect x="12" y="11" fill="#fff" width="24" height="22"/>'#13 +
          #10'    <polygon fill="#9C27B0" points="24,13.6 18,21.4 30,21.4"/>'#13 +
          #10'    <path fill="#CFD8DC" d="M40,41H8c-2.2,0-4-1.8-4-4l0-20l20,1' +
          '3l20-13v20C44,39.2,42.2,41,40,41z"/>'#13#10'    <polygon fill="#9C27B0' +
          '" points="24,28 26,26.7 26,20 22,20 22,26.7"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'file'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '90CAF9" points="40,45 8,45 8,3 30,3 40,13"/>'#13#10'    <polygon fill=' +
          '"#E1F5FE" points="38.5,14 29,14 29,4.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'filing_cabinet'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="12" y="' +
          '44" fill="#263238" width="4" height="2"/>'#13#10'    <rect x="32" y="4' +
          '4" fill="#263238" width="4" height="2"/>'#13#10'    <path fill="#607D8' +
          'B" d="M8,41V7c0-2.2,1.8-4,4-4h24c2.2,0,4,1.8,4,4v34c0,2.2-1.8,4-' +
          '4,4H12C9.8,45,8,43.2,8,41z"/>'#13#10'    <path fill="#B0BEC5" d="M12,1' +
          '7V8c0-0.6,0.4-1,1-1h22c0.6,0,1,0.4,1,1v9H12z"/>'#13#10'    <rect x="12' +
          '" y="19" fill="#B0BEC5" width="24" height="10"/>'#13#10'    <path fill' +
          '="#B0BEC5" d="M12,40v-9h24v9c0,0.6-0.4,1-1,1H13C12.4,41,12,40.6,' +
          '12,40z"/>'#13#10'    <rect x="20" y="11" fill="#546E7A" width="8" heig' +
          'ht="2"/>'#13#10'    <rect x="20" y="23" fill="#546E7A" width="8" heigh' +
          't="2"/>'#13#10'    <rect x="20" y="35" fill="#546E7A" width="8" height' +
          '="2"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'filled_filter'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'F57C00" points="29,23 19,23 7,9 41,9"/>'#13#10'    <g fill="#FF9800">'#13 +
          #10'        <polygon points="29,38 19,44 19,23 29,23"/>'#13#10'        <p' +
          'ath d="M41.5,9h-35C5.7,9,5,8.3,5,7.5v0C5,6.7,5.7,6,6.5,6h35C42.3' +
          ',6,43,6.7,43,7.5v0C43,8.3,42.3,9,41.5,9z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Delphi_Product icon'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<svg version="1.1" x="0p' +
          'x" y="0px" viewBox="0 0 8000 8000" style="enable-background:new ' +
          '0 0 8000 8000;" xmlns="http://www.w3.org/2000/svg">'#13#10'  <style ty' +
          'pe="text/css">'#13#10#9'.st0{display:none;fill:#FFFFFF;stroke:#C1272D;s' +
          'troke-width:20;stroke-miterlimit:10;}'#13#10#9'.st1{fill:#F32735;}'#13#10#9'.s' +
          't2{fill:#9E0F20;}'#13#10#9'.st3{fill:#FFFFFF;}'#13#10'</style>'#13#10'  <g id="Cama' +
          'da_1">'#13#10'    <g>'#13#10'      <g>'#13#10'        <circle cx="4000" cy="3999.9' +
          '9" r="3989.83" fill="#F32735"/>'#13#10'        <path class="st2" d="M6' +
          '809.98,1167.54L1190.29,6832.7c0.12,0.12,0.24,0.24,0.35,0.35 c156' +
          '4.56,1551.67,4090.76,1541.21,5642.42-23.35C8384.64,5245.24,8374.' +
          '28,2719.23,6809.98,1167.54z" fill="#9E0F20"/>'#13#10'      </g>'#13#10'     ' +
          ' <g>'#13#10'        <path class="st3" d="M 3418.55 4663.88 C 3522.07 4' +
          '594.59 3624.19 4523.2 3727.03 4452.89 C 4140.66 4170.12 4554.35 ' +
          '3887.43 4968.03 3604.73 L 5428.89 3288.46 C 5404.95 3266.46 5383' +
          '.98 3246.07 5361.81 3227.12 C 5230.98 3115.31 5090.63 3017.48 49' +
          '38.4 2936.66 C 4700.77 2810.51 4449.37 2729.12 4180.39 2711.34 C' +
          ' 4041.77 2702.18 3902.62 2709.54 3765 2733.14 C 3558.08 2768.62 ' +
          '3361.74 2834.83 3176.46 2933.94 C 2902.75 3080.34 2676.29 3280.3' +
          '1 2496.85 3533.29 C 2290.06 3824.84 2175.32 4150.3 2145.91 4505.' +
          '62 C 2134.12 4648.07 2140.75 4790.77 2162.88 4932.11 C 2191.21 5' +
          '113.12 2242.41 5287.62 2319.58 5454.1 C 2405.3 5639.02 2517.79 5' +
          '806.05 2656.99 5954.85 C 2690.82 5991.01 2727.46 6024.53 2763.77' +
          ' 6060.2 C 2817.39 5985.65 2890.25 5945 2970.89 5916.33 C 3115.41' +
          ' 5864.96 3266.52 5855.8 3417.66 5852.42 C 3501.78 5850.54 3586.3' +
          '1 5856.38 3670.31 5862.99 C 3765.61 5870.49 3860.58 5882.33 3955' +
          '.6 5893.12 C 4000.64 5898.23 4045.46 5905.29 4094.68 5912.06 C 3' +
          '955.31 5619.42 3818.11 5331.33 3680.95 5043.32 C 3609.84 5057.35' +
          ' 3542.57 5070.62 3473.72 5084.2 C 3471.14 5077.22 3468.89 5072.8' +
          '8 3467.93 5068.27 C 3443.78 4952.61 3421.16 4836.6 3394.77 4721.' +
          '46 C 3388.14 4692.56 3395.56 4679.27 3418.55 4663.88 Z M 3226.93' +
          ' 3228.81 C 2927.75 3477.48 2688.3 3769.68 2564.45 4145.19 C 2529' +
          '.93 4249.84 2510.2 4357.26 2500.53 4467.18 C 2474.02 4768.45 254' +
          '2.53 5050.48 2673.82 5318.67 C 2722.12 5417.34 2782.15 5510.27 2' +
          '836.83 5605.82 C 2840.5 5612.23 2844.1 5618.67 2850.05 5629.2 C ' +
          '2809.29 5649.96 2770.59 5670.63 2730.96 5689.31 C 2717.43 5695.6' +
          '8 2703.71 5698.24 2692.68 5678.93 C 2577.94 5477.92 2483.6 5268.' +
          '42 2426.36 5043.46 C 2386.56 4887.02 2364.04 4727.85 2370.15 456' +
          '6.44 C 2380.37 4296.56 2456.03 4045.66 2598.72 3815.35 C 2723.34' +
          ' 3614.2 2882.25 3444.45 3066.36 3297.08 C 3223.33 3171.43 3392.0' +
          '8 3064.88 3574.56 2980.28 C 3582.92 2976.41 3591.49 2973 3599.96' +
          ' 2969.38 C 3467.86 3045.96 3343.53 3131.9 3226.93 3228.81 Z" fil' +
          'l="#ffffff"/>'#13#10'        <path class="st3" d="M3810.15,5126.95c71.' +
          '34-13.78,141.09-27.73,211.14-40.03c7.06-1.24,17.53,5.85,23.62,11' +
          '.9 c101.26,100.62,199.06,204.95,303.9,301.65c87.91,81.1,183.82,1' +
          '53.54,276.37,229.59c131.92,108.4,247.22,232.56,346.44,371.17 c17' +
          '3.4,242.26,298.75,507.36,368.36,797.5c6.12,25.51-1.66,56.9-10.53' +
          ',82.99c-30.12,88.64-94.02,152.58-164.66,209.75 c-98.96,80.09-209' +
          '.83,139.92-327.3,188.41c-24.42,10.08-33.29,5.25-44.22-18.67c-120' +
          '.95-264.59-243.44-528.48-365.36-792.63 c-165.29-358.1-330.43-716' +
          '.27-495.67-1074.39c-38.48-83.39-77.09-166.71-115.57-250.1 C3814.' +
          '33,5139.04,3812.68,5133.66,3810.15,5126.95z" fill="#ffffff"/>'#13#10' ' +
          '       <path class="st3" d="M 4081.5 6038.59 C 4077.63 6030.26 4' +
          '065.59 6021.46 4056.5 6020.44 C 3877.95 6000.38 3699.53 5978.39 ' +
          '3520.53 5963.31 C 3395.27 5952.76 3269.4 5960.32 3144.79 5977.98' +
          ' C 3056.57 5990.48 2970.21 6011.44 2891.1 6054.82 C 2844.64 6080' +
          '.3 2810.02 6114.49 2788.97 6166.08 C 2727.85 6315.91 2662.5 6464' +
          '.02 2598.74 6612.77 C 2565.64 6689.99 2532.4 6767.16 2497.13 684' +
          '9.23 C 2505.43 6846.75 2508.68 6845.87 2511.87 6844.8 C 2685.37 ' +
          '6786.58 2865.17 6769.89 3046.4 6769.72 C 3154.48 6769.62 3262.77' +
          ' 6777.31 3370.59 6786.11 C 3476.27 6794.74 3581.91 6806.29 3686.' +
          '78 6821.85 C 3823.25 6842.09 3958.95 6867.56 4097.98 6891.29 C 4' +
          '163.34 6773.54 4230.38 6653.11 4296.7 6532.28 C 4300.12 6526.05 ' +
          '4300.05 6515.17 4297.03 6508.53 C 4225.77 6351.62 4154.09 6194.8' +
          '9 4081.5 6038.59 Z M 3060.07 6162.36 C 3045.94 6166.69 3031.07 6' +
          '177.41 3022.39 6189.4 C 2926.42 6321.97 2831.59 6455.36 2736.61 ' +
          '6588.64 C 2734.02 6592.27 2733.28 6597.22 2731.67 6601.55 C 2729' +
          '.19 6599.41 2726.71 6597.28 2724.23 6595.14 C 2740.71 6560.03 27' +
          '56.95 6524.81 2773.72 6489.84 C 2829.13 6374.26 2884.41 6258.62 ' +
          '2940.81 6143.52 C 2945.95 6133.04 2957.44 6121.86 2968.27 6118.5' +
          '8 C 3082.71 6083.93 3199.57 6062.84 3318.94 6052.57 C 3407.4 604' +
          '4.96 3494.47 6047.51 3581.33 6064.57 C 3590.46 6066.36 3599.34 6' +
          '069.44 3608.33 6071.93 C 3417.33 6058.15 3238.21 6107.79 3060.07' +
          ' 6162.36 Z" fill="#ffffff"/>'#13#10'        <path class="st3" d="M 582' +
          '7.13 4252.21 C 5805.6 4119.77 5783.84 3987.37 5762.57 3854.89 C ' +
          '5743.66 3737.14 5725.54 3619.27 5706.57 3501.53 C 5692.11 3411.7' +
          '6 5677.01 3322.06 5661.67 3229.54 L 3491.78 4718.67 C 3484.37 47' +
          '25.39 3477.14 4738.07 3478.95 4745.17 C 3496.71 4814.93 3516.91 ' +
          '4884.06 3536.27 4952.98 C 3541.17 4953.23 3544.13 4953.97 3546.8' +
          '5 4953.43 C 3718.71 4919.31 3890.46 4884.65 4062.4 4850.98 C 439' +
          '1.43 4786.55 4720.62 4722.95 5049.63 4658.49 C 5313.27 4606.83 5' +
          '576.68 4554 5840.45 4503 C 5863.63 4498.52 5865.82 4488.93 5862.' +
          '63 4469.95 C 5850.44 4397.44 5838.93 4324.8 5827.13 4252.21 Z" f' +
          'ill="#ffffff"/>'#13#10'        <g>'#13#10'          <path class="st3" d="M45' +
          '81.5,2438.32c-51.59,74.74-103.23,149.45-154.09,223.08c284.02,48.' +
          '7,508.95,165.56,630.93,236.08 c18.8,8.58,30.36,7.04,45.52-5.97c9' +
          '5.28-81.78,191.04-163.01,287.18-243.8c190.04-159.7,380.58-318.82' +
          ',570.63-478.51 c199.81-167.88,399.34-336.08,598.92-504.23c-326.3' +
          '9-232.07-695.2-407.31-1092.21-511.3 c-91.91,133.75-184.44,267.08' +
          '-276.66,400.62C4988.26,1848.93,4784.91,2143.65,4581.5,2438.32z" ' +
          'fill="#ffffff"/>'#13#10'          <path class="st3" d="M3524.19,2467.4' +
          '8c-21.49,79.87-44.67,165.97-66.98,250.27c46.56-10.37,323.17-115.' +
          '2,718.88-88.82 c19.08,1.16,28.97-5.68,37.86-22.32c85.3-159.66,16' +
          '6.56-311.85,252.4-471.22c124.04-230.28,247.99-460.62,371.99-690.' +
          '93 c66.42-123.36,132.88-246.71,201.16-373.46c-137.35-17.23-277.2' +
          '2-26.16-419.13-26.16c-252.51,0-498.61,28.17-735.43,81.45 c-6.84,' +
          '28.63-14.95,56.97-22.6,85.41C3749.63,1630.3,3636.83,2048.87,3524' +
          '.19,2467.48z" fill="#ffffff"/>'#13#10'          <path class="st3" d="M' +
          '2545.86,1836.53c6.63,78.14,13.34,156.27,19.88,234.42c6.3,75.3,12' +
          '.22,150.63,18.7,225.91 c6.56,76.18,13.88,152.29,20.34,228.48c6.6' +
          '2,78.2,12.58,156.47,18.86,234.7c6.05,75.39,11.98,150.79,18.28,22' +
          '6.16 c5.69,68.08,11.43,139.61,17.71,211.68c77.38-75.63,317.01-28' +
          '9.31,612.48-411.98c13.75-5.71,27.38-8.82,30.84-30.91 c18.46-117.' +
          '67,36.52-215.22,57.03-332.58c28.68-164.11,57.33-328.23,85.9-492.' +
          '37c35.77-205.54,71.52-411.09,107.06-616.67 c5.59-32.33,10.49-64.' +
          '82,15.86-99.36c-375.71,124.54-722.52,313.9-1027.32,554.95c-0.14,' +
          '0.6-0.31,1.21-0.32,1.8 C2540.65,1792.65,2543.99,1814.61,2545.86,' +
          '1836.53z" fill="#ffffff"/>'#13#10'          <path class="st3" d="M2151' +
          '.69,3602.03c28.95,58.26,62.61,126.45,92.44,186.35c62.88-139.38,1' +
          '94.26-347.57,318.13-488.72 c6.41-7.31,10.14-21.49,8.12-30.98c-20' +
          '.81-97.89-43.14-189.66-65.05-287.32c-57.91-258.1-115.86-516.2-17' +
          '3.82-774.29 c-15.21-67.73-30.56-135.42-47.53-210.59c-22.74,24.23' +
          '-40.47,42.85-57.9,61.74c-72.83,78.96-151.57,153.39-216.96,238.1 ' +
          'c-88.09,114.1-168.83,233.86-251.92,351.85c-14.06,22.66-27.86,45.' +
          '51-41.4,68.52c0.01,6.6,2.26,13.25,6.04,20.84 C1865.41,3025.56,20' +
          '08.48,3313.82,2151.69,3602.03z" fill="#ffffff"/>'#13#10'          <pat' +
          'h class="st3" d="M1463.39,3773.25c166.75,147.24,333.58,294.4,500' +
          '.43,441.53c38,33.51,78.9,69.38,120.26,105.61 c15.83-124.87,58.2-' +
          '279.76,105.96-401.87c9.16-23.43-0.04-37.32-12.31-53.78c-168.78-2' +
          '26.42-339.26-452.55-507.93-679.06 c-37.99-51.02-76.32-101.8-115.' +
          '94-154.62c-4.47,8.41-7.33,13.18-9.63,18.2c-5.97,13.06-11.84,26.1' +
          '5-17.63,39.27 c-78.19,180.23-141.29,368.58-187.77,563.37c1.54,6.' +
          '68,5.02,13.54,9.12,17.41C1385.6,3704.84,1424.57,3738.97,1463.39,' +
          '3773.25z" fill="#ffffff"/>'#13#10'          <path class="st3" d="M1937' +
          '.2,4872.06c44.65,15.45,94.49,32.94,140.29,48.94c-26.82-165.76-14' +
          '.78-400.9-11.87-451.56 c1.19-20.64-5.68-32.28-23.85-43.49c-189.0' +
          '8-116.57-381.36-235.03-569.95-352.4c-62.5-38.89-125.13-77.62-190' +
          '.93-118.42 c-23.27,161.02-35.39,325.64-35.39,493.07c0,62.38,1.72' +
          ',124.35,5.03,185.91c2.88,1.62,6.25,3.08,10.22,4.44 C1486.44,4715' +
          '.81,1711.78,4794.03,1937.2,4872.06z" fill="#ffffff"/>'#13#10'         ' +
          ' <path class="st3" d="M2252.92,5507.54c-11.18-19.62-22.95-22.74-' +
          '43.15-17.88c-190.58,45.82-386.45,92.18-577.15,137.54 c-54.15,12.' +
          '88-108.33,25.91-163.62,39.18c62.3,163.36,136.73,320.69,222.21,47' +
          '0.86c234.84-125.41,470.34-250.82,705.49-376.44 C2305.68,5620.1,2' +
          '296.99,5584.9,2252.92,5507.54z" fill="#ffffff"/>'#13#10'          <pat' +
          'h class="st3" d="M1747.15,5373.51c104.59-0.89,209.2-0.18,313.8-0' +
          '.18c43.56,0,87.13,0,133.63,0 c-2.68-10.32-65.65-190.27-85.44-283' +
          '.48c-1.26-8.85-15.9-20.52-26.07-22.69c-231.85-49.45-461.9-96.87-' +
          '693.98-145.24 c-37.34-7.78-74.93-14.84-113.97-22.4c21.62,163.84,' +
          '54.78,324.05,98.74,479.73c0.8,0,1.58,0.01,2.41-0.01 C1499.88,537' +
          '6.5,1623.51,5374.56,1747.15,5373.51z" fill="#ffffff"/>'#13#10'        ' +
          '</g>'#13#10'      </g>'#13#10'    </g>'#13#10'    <g>'#13#10'      <ellipse class="st0" ' +
          'cx="7356.18" cy="801.01" rx="124.6" ry="125.71"/>'#13#10'    </g>'#13#10'  <' +
          '/g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'film'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#3F5' +
          '1B5" d="M45,9H3v30h42V9z M22,37v-4h4v4H22z M30,37v-4h4v4H30z M38' +
          ',37v-4h4v4H38z M14,37v-4h4v4H14z M6,37v-4h4v4H6 z M22,15v-4h4v4H' +
          '22z M30,15v-4h4v4H30z M38,15v-4h4v4H38z M14,15v-4h4v4H14z M6,15v' +
          '-4h4v4H6z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'film_reel'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#3F5' +
          '1B5" d="M43,39V24h-4v15c0,5,4,9,9,9v-4C45.2,44,43,41.8,43,39z"/>' +
          #13#10'    <circle fill="#90A4AE" cx="24" cy="24" r="19"/>'#13#10'    <circ' +
          'le fill="#37474F" cx="24" cy="24" r="2"/>'#13#10'    <g fill="#253278"' +
          '>'#13#10'        <circle cx="24" cy="14" r="5"/>'#13#10'        <circle cx="' +
          '24" cy="34" r="5"/>'#13#10'        <circle cx="34" cy="24" r="5"/>'#13#10'  ' +
          '      <circle cx="14" cy="24" r="5"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'flash_auto'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'FFC107" points="33,22 23.6,22 30,5 19,5 13,26 21.6,26 17,45"/>'#13#10 +
          '    <path fill="#F44336" d="M40.8,14.5h-4.3L35.6,17H33l4.5-12h2.' +
          '3l4.5,12h-2.6L40.8,14.5z M37.1,12.5h3L38.6,8L37.1,12.5z"/>'#13#10'</sv' +
          'g>'#13#10
      end
      item
        IconName = 'flash_on'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'FFC107" points="33,22 23.6,22 30,5 19,5 13,26 21.6,26 17,45"/>'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = 'flow_chart'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'CFD8DC" points="35,36 39,36 39,22 26,22 26,13 22,13 22,22 9,22 9' +
          ',36 13,36 13,26 22,26 22,36 26,36 26,26 35,26"/>'#13#10'    <rect x="1' +
          '7" y="6" fill="#3F51B5" width="14" height="10"/>'#13#10'    <rect x="3' +
          '2" y="32" fill="#00BCD4" width="10" height="10"/>'#13#10'    <rect x="' +
          '6" y="32" fill="#00BCD4" width="10" height="10"/>'#13#10'    <rect x="' +
          '19" y="32" fill="#00BCD4" width="10" height="10"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'folder'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FFA' +
          '000" d="M40,12H22l-4-4H8c-2.2,0-4,1.8-4,4v8h40v-4C44,13.8,42.2,1' +
          '2,40,12z"/>'#13#10'    <path fill="#FFCA28" d="M40,12H8c-2.2,0-4,1.8-4' +
          ',4v20c0,2.2,1.8,4,4,4h32c2.2,0,4-1.8,4-4V16C44,13.8,42.2,12,40,1' +
          '2z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'frame'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#394' +
          '9AB" d="M40.6,40.1h-1.4c-0.2,0-0.3,0-0.5,0l-3.1-0.4c-2.4-0.3-4.9' +
          '-0.2-7.3,0.4l-3.6,0.9c-0.5,0.1-1.1,0.1-1.6,0 L19.6,40c-2.4-0.6-4' +
          '.8-0.7-7.3-0.4l-3.1,0.4c-0.2,0-0.3,0-0.5,0H7.4c-1.9,0-3.4-1.5-3.' +
          '4-3.4v0c0-0.4,0.1-0.9,0.2-1.3l0.2-0.6 c1-2.5,1.1-5.3,0.4-7.9l-0.' +
          '6-2c-0.2-0.7-0.2-1.3,0-2l0.3-0.8c0.9-2.7,0.8-5.7-0.2-8.4l-0.1-0.' +
          '3C4.1,13.1,4,12.7,4,12.3v-1 c0-1.9,1.5-3.4,3.4-3.4l1.4,0c0.2,0,0' +
          '.3,0,0.5,0l3.1,0.4c2.4,0.3,4.9,0.2,7.3-0.4l3.6-0.9c0.5-0.1,1.1-0' +
          '.1,1.6,0L28.4,8 c2.4,0.6,4.8,0.7,7.3,0.4l3.1-0.4c0.2,0,0.3,0,0.5' +
          ',0l1.4,0c1.9,0,3.4,1.5,3.4,3.4v1c0,0.4-0.1,0.9-0.2,1.3l-0.1,0.3 ' +
          'c-1.1,2.7-1.2,5.6-0.2,8.4l0.3,0.8c0.2,0.6,0.2,1.3,0,2l-0.6,2c-0.' +
          '7,2.6-0.6,5.4,0.4,7.9l0.2,0.6c0.2,0.4,0.2,0.8,0.2,1.3v0 C44,38.6' +
          ',42.5,40.1,40.6,40.1z"/>'#13#10'    <path fill="#BBDEFB" d="M38,36H10c' +
          '-0.6,0-1-0.4-1-1V13c0-0.6,0.4-1,1-1h28c0.6,0,1,0.4,1,1v22C39,35.' +
          '6,38.6,36,38,36z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'full_battery'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#8BC34A' +
          '">'#13#10'        <path d="M34,44H14c-1.1,0-2-0.9-2-2V8c0-1.1,0.9-2,2-' +
          '2h20c1.1,0,2,0.9,2,2v34C36,43.1,35.1,44,34,44z"/>'#13#10'        <path' +
          ' d="M28,13h-8c-0.6,0-1-0.4-1-1V5c0-0.6,0.4-1,1-1h8c0.6,0,1,0.4,1' +
          ',1v7C29,12.6,28.6,13,28,13z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'full_trash'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'FF8A65" points="24,21.3 12.7,10 26,1.7 38.3,10"/>'#13#10'    <polygon ' +
          'fill="#FFAB91" points="24,21.3 12.7,10 17,4.7 38.3,10"/>'#13#10'    <p' +
          'ath fill="#B39DDB" d="M30.6,44H17.4c-2,0-3.7-1.4-4-3.4L9,11h30l-' +
          '4.5,29.6C34.2,42.6,32.5,44,30.6,44z"/>'#13#10'    <path fill="#7E57C2"' +
          ' d="M38,13H10c-1.1,0-2-0.9-2-2v0c0-1.1,0.9-2,2-2h28c1.1,0,2,0.9,' +
          '2,2v0C40,12.1,39.1,13,38,13z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'gallery'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#E65' +
          '100" d="M41,42H13c-2.2,0-4-1.8-4-4V18c0-2.2,1.8-4,4-4h28c2.2,0,4' +
          ',1.8,4,4v20C45,40.2,43.2,42,41,42z"/>'#13#10'    <path fill="#F57C00" ' +
          'd="M35,36H7c-2.2,0-4-1.8-4-4V12c0-2.2,1.8-4,4-4h28c2.2,0,4,1.8,4' +
          ',4v20C39,34.2,37.2,36,35,36z"/>'#13#10'    <circle fill="#FFF9C4" cx="' +
          '30" cy="16" r="3"/>'#13#10'    <polygon fill="#942A09" points="17,17.9' +
          ' 8,31 26,31"/>'#13#10'    <polygon fill="#BF360C" points="28,23.5 22,3' +
          '1 34,31"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'genealogy'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'CFD8DC" points="40,9 40,7 31,7 31,12 24,12 15,12 15,23 8,23 8,25' +
          ' 15,25 15,36 24,36 31,36 31,41 40,41 40,39 33,39 33,31 40,31 40,' +
          '29 31,29 31,34 24,34 17,34 17,14 24,14 31,14 31,19 40,19 40,17 3' +
          '3,17 33,9"/>'#13#10'    <rect x="4" y="20" fill="#00BCD4" width="8" he' +
          'ight="8"/>'#13#10'    <g fill="#3F51B5">'#13#10'        <rect x="36" y="14" ' +
          'width="8" height="8"/>'#13#10'        <rect x="36" y="4" width="8" hei' +
          'ght="8"/>'#13#10'        <rect x="20" y="9" width="8" height="8"/>'#13#10'  ' +
          '      <rect x="20" y="31" width="8" height="8"/>'#13#10'        <rect ' +
          'x="36" y="36" width="8" height="8"/>'#13#10'        <rect x="36" y="26' +
          '" width="8" height="8"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'generic_sorting_asc'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="6" y="6' +
          '" fill="#2196F3" width="4" height="4"/>'#13#10'    <rect x="6" y="14" ' +
          'fill="#2196F3" width="12" height="4"/>'#13#10'    <rect x="6" y="22" f' +
          'ill="#2196F3" width="20" height="4"/>'#13#10'    <rect x="6" y="30" fi' +
          'll="#2196F3" width="28" height="4"/>'#13#10'    <rect x="6" y="38" fil' +
          'l="#2196F3" width="36" height="4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'generic_sorting_desc'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="6" y="3' +
          '8" fill="#2196F3" width="4" height="4"/>'#13#10'    <rect x="6" y="30"' +
          ' fill="#2196F3" width="12" height="4"/>'#13#10'    <rect x="6" y="22" ' +
          'fill="#2196F3" width="20" height="4"/>'#13#10'    <rect x="6" y="14" f' +
          'ill="#2196F3" width="28" height="4"/>'#13#10'    <rect x="6" y="6" fil' +
          'l="#2196F3" width="36" height="4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'globe'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#7CB' +
          '342" d="M24,4C13,4,4,13,4,24s9,20,20,20s20-9,20-20S35,4,24,4z"/>' +
          #13#10'    <path fill="#0277BD" d="M45,24c0,11.7-9.5,21-21,21S3,35.7,' +
          '3,24S12.3,3,24,3S45,12.3,45,24z M23.8,33.7c0-0.4-0.2-0.6-0.6-0.8' +
          ' c-1.3-0.4-2.5-0.4-3.6-1.5c-0.2-0.4-0.2-0.8-0.4-1.3c-0.4-0.4-1.5' +
          '-0.6-2.1-0.8c-0.8,0-1.7,0-2.7,0c-0.4,0-1.1,0-1.5,0 c-0.6-0.2-1.1' +
          '-1.1-1.5-1.7c0-0.2,0-0.6-0.4-0.6c-0.4-0.2-0.8,0.2-1.3,0c-0.2-0.2' +
          '-0.2-0.4-0.2-0.6c0-0.6,0.4-1.3,0.8-1.7 c0.6-0.4,1.3,0.2,1.9,0.2c' +
          '0.2,0,0.2,0,0.4,0.2c0.6,0.2,0.8,1,0.8,1.7c0,0.2,0,0.4,0,0.4c0,0.' +
          '2,0.2,0.2,0.4,0.2 c0.2-1.1,0.2-2.1,0.4-3.2c0-1.3,1.3-2.5,2.3-2.9' +
          'c0.4-0.2,0.6,0.2,1.1,0c1.3-0.4,4.4-1.7,3.8-3.4c-0.4-1.5-1.7-2.9-' +
          '3.4-2.7 c-0.4,0.2-0.6,0.4-1,0.6c-0.6,0.4-1.9,1.7-2.5,1.7c-1.1-0.' +
          '2-1.1-1.7-0.8-2.3c0.2-0.8,2.1-3.6,3.4-3.1c0.2,0.2,0.6,0.6,0.8,0.' +
          '8 c0.4,0.2,1.1,0.2,1.7,0.2c0.2,0,0.4,0,0.6-0.2c0.2-0.2,0.2-0.2,0' +
          '.2-0.4c0-0.6-0.6-1.3-1-1.7c-0.4-0.4-1.1-0.8-1.7-1.1 c-2.1-0.6-5.' +
          '5,0.2-7.1,1.7s-2.9,4-3.8,6.1c-0.4,1.3-0.8,2.9-1,4.4c-0.2,1-0.4,1' +
          '.9,0.2,2.9c0.6,1.3,1.9,2.5,3.2,3.4 c0.8,0.6,2.5,0.6,3.4,1.7c0.6,' +
          '0.8,0.4,1.9,0.4,2.9c0,1.3,0.8,2.3,1.3,3.4c0.2,0.6,0.4,1.5,0.6,2.' +
          '1c0,0.2,0.2,1.5,0.2,1.7 c1.3,0.6,2.3,1.3,3.8,1.7c0.2,0,1-1.3,1-1' +
          '.5c0.6-0.6,1.1-1.5,1.7-1.9c0.4-0.2,0.8-0.4,1.3-0.8c0.4-0.4,0.6-1' +
          '.3,0.8-1.9 C23.8,35.1,24,34.3,23.8,33.7z M24.2,14.3c0.2,0,0.4-0.' +
          '2,0.8-0.4c0.6-0.4,1.3-1.1,1.9-1.5c0.6-0.4,1.3-1.1,1.7-1.5 c0.6-0' +
          '.4,1.1-1.3,1.3-1.9c0.2-0.4,0.8-1.3,0.6-1.9c-0.2-0.4-1.3-0.6-1.7-' +
          '0.8c-1.7-0.4-3.1-0.6-4.8-0.6c-0.6,0-1.5,0.2-1.7,0.8 c-0.2,1.1,0.' +
          '6,0.8,1.5,1.1c0,0,0.2,1.7,0.2,1.9c0.2,1-0.4,1.7-0.4,2.7c0,0.6,0,' +
          '1.7,0.4,2.1L24.2,14.3z M41.8,29 c0.2-0.4,0.2-1.1,0.4-1.5c0.2-1,0' +
          '.2-2.1,0.2-3.1c0-2.1-0.2-4.2-0.8-6.1c-0.4-0.6-0.6-1.3-0.8-1.9c-0' +
          '.4-1.1-1-2.1-1.9-2.9 c-0.8-1.1-1.9-4-3.8-3.1c-0.6,0.2-1,1-1.5,1.' +
          '5c-0.4,0.6-0.8,1.3-1.3,1.9c-0.2,0.2-0.4,0.6-0.2,0.8c0,0.2,0.2,0.' +
          '2,0.4,0.2 c0.4,0.2,0.6,0.2,1,0.4c0.2,0,0.4,0.2,0.2,0.4c0,0,0,0.2' +
          '-0.2,0.2c-1,1.1-2.1,1.9-3.1,2.9c-0.2,0.2-0.4,0.6-0.4,0.8 c0,0.2,' +
          '0.2,0.2,0.2,0.4c0,0.2-0.2,0.2-0.4,0.4c-0.4,0.2-0.8,0.4-1.1,0.6c-' +
          '0.2,0.4,0,1.1-0.2,1.5c-0.2,1.1-0.8,1.9-1.3,2.9 c-0.4,0.6-0.6,1.3' +
          '-1,1.9c0,0.8-0.2,1.5,0.2,2.1c1,1.5,2.9,0.6,4.4,1.3c0.4,0.2,0.8,0' +
          '.2,1.1,0.6c0.6,0.6,0.6,1.7,0.8,2.3 c0.2,0.8,0.4,1.7,0.8,2.5c0.2,' +
          '1,0.6,2.1,0.8,2.9c1.9-1.5,3.6-3.1,4.8-5.2C40.6,32.4,41.2,30.7,41' +
          '.8,29z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'good_decision'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FFCC80' +
          '">'#13#10'        <circle cx="38" cy="26" r="4"/>'#13#10'        <circle cx=' +
          '"10" cy="26" r="4"/>'#13#10'        <path d="M39,19c0-12.7-30-8.3-30,0' +
          'c0,1.8,0,8.2,0,10c0,8.3,6.7,15,15,15s15-6.7,15-15C39,27.2,39,20.' +
          '8,39,19z"/>'#13#10'        <path d="M24,4C15.2,4,8,11.2,8,20c0,1.2,0,3' +
          '.5,0,3.5l2.1,0.6V19l19.5-6.3l8.2,6.3v5.1l2.1-0.6c0,0,0-2.3,0-3.5' +
          ' C40,12.5,34.6,4,24,4z"/>'#13#10'    </g>'#13#10'    <g fill="#4CAF50">'#13#10'   ' +
          '     <rect x="22" y="16" width="4" height="18"/>'#13#10'        <rect ' +
          'x="15" y="23" width="18" height="4"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'google'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 15.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://w' +
          'ww.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#13#10'<svg version="1.1"  ' +
          'xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.or' +
          'g/1999/xlink" x="0px" y="0px"'#13#10#9' width="48px" height="48px" view' +
          'Box="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pre' +
          'serve">'#13#10'<path fill="#FFC107" d="M43.611,20.083H42V20H24v8h11.30' +
          '3c-1.649,4.657-6.08,8-11.303,8c-6.627,0-12-5.373-12-12'#13#10#9'c0-6.62' +
          '7,5.373-12,12-12c3.059,0,5.842,1.154,7.961,3.039l5.657-5.657C34.' +
          '046,6.053,29.268,4,24,4C12.955,4,4,12.955,4,24'#13#10#9'c0,11.045,8.955' +
          ',20,20,20c11.045,0,20-8.955,20-20C44,22.659,43.862,21.35,43.611,' +
          '20.083z"/>'#13#10'<path fill="#FF3D00" d="M6.306,14.691l6.571,4.819C14' +
          '.655,15.108,18.961,12,24,12c3.059,0,5.842,1.154,7.961,3.039l5.65' +
          '7-5.657'#13#10#9'C34.046,6.053,29.268,4,24,4C16.318,4,9.656,8.337,6.306' +
          ',14.691z"/>'#13#10'<path fill="#4CAF50" d="M24,44c5.166,0,9.86-1.977,1' +
          '3.409-5.192l-6.19-5.238C29.211,35.091,26.715,36,24,36'#13#10#9'c-5.202,' +
          '0-9.619-3.317-11.283-7.946l-6.522,5.025C9.505,39.556,16.227,44,2' +
          '4,44z"/>'#13#10'<path fill="#1976D2" d="M43.611,20.083H42V20H24v8h11.3' +
          '03c-0.792,2.237-2.231,4.166-4.087,5.571'#13#10#9'c0.001-0.001,0.002-0.0' +
          '01,0.003-0.002l6.19,5.238C36.971,39.205,44,34,44,24C44,22.659,43' +
          '.862,21.35,43.611,20.083z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'graduation_cap'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#37474F' +
          '">'#13#10'        <rect x="9" y="20" width="30" height="13"/>'#13#10'       ' +
          ' <ellipse cx="24" cy="33" rx="15" ry="6"/>'#13#10'    </g>'#13#10'    <path ' +
          'fill="#78909C" d="M23.1,8.2L0.6,18.1c-0.8,0.4-0.8,1.5,0,1.9l22.5' +
          ',9.9c0.6,0.2,1.2,0.2,1.8,0l22.5-9.9c0.8-0.4,0.8-1.5,0-1.9 L24.9,' +
          '8.2C24.3,7.9,23.7,7.9,23.1,8.2z"/>'#13#10'    <g fill="#37474F">'#13#10'    ' +
          '    <path d="M43.2,20.4l-20-3.4c-0.5-0.1-1.1,0.3-1.2,0.8c-0.1,0.' +
          '5,0.3,1.1,0.8,1.2L42,22.2V37c0,0.6,0.4,1,1,1 s1-0.4,1-1V21.4C44,' +
          '20.9,43.6,20.5,43.2,20.4z"/>'#13#10'        <circle cx="43" cy="37" r=' +
          '"2"/>'#13#10'        <path d="M46,40c0,1.7-3,6-3,6s-3-4.3-3-6s1.3-3,3-' +
          '3S46,38.3,46,40z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'grid'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#90C' +
          'AF9" d="M7,7v34h34V7H7z M39,15h-6V9h6V15z M25,15V9h6v6H25z M31,1' +
          '7v6h-6v-6H31z M23,15h-6V9h6V15z M23,17v6h-6v-6 H23z M15,23H9v-6h' +
          '6V23z M15,25v6H9v-6H15z M17,25h6v6h-6V25z M23,33v6h-6v-6H23z M25' +
          ',33h6v6h-6V33z M25,31v-6h6v6H25z M33,25h6v6h-6 V25z M33,23v-6h6v' +
          '6H33z M15,9v6H9V9H15z M9,33h6v6H9V33z M33,39v-6h6v6H33z"/>'#13#10'</sv' +
          'g>'#13#10
      end
      item
        IconName = 'headset'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#009' +
          '7A7" d="M24,5C14.1,5,6,13.1,6,23v15h4V23c0-7.7,6.3-14,14-14s14,6' +
          '.3,14,14v15h4V23C42,13.1,33.9,5,24,5z"/>'#13#10'    <path fill="#37474' +
          'F" d="M38,43h-4V31h4c2.2,0,4,1.8,4,4v4C42,41.2,40.2,43,38,43z"/>' +
          #13#10'    <path fill="#37474F" d="M10,43h4V31h-4c-2.2,0-4,1.8-4,4v4C' +
          '6,41.2,7.8,43,10,43z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'heat_map'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'CFD8DC" points="9,39 9,6 7,6 7,41 42,41 42,39"/>'#13#10'    <g fill="#' +
          '00BCD4">'#13#10'        <circle cx="14" cy="11" r="2"/>'#13#10'        <circ' +
          'le cx="32" cy="11" r="2"/>'#13#10'        <circle cx="39" cy="11" r="2' +
          '"/>'#13#10'        <circle cx="23" cy="11" r="4"/>'#13#10'        <circle cx' +
          '="14" cy="33" r="2"/>'#13#10'        <circle cx="30" cy="33" r="2"/>'#13#10 +
          '        <circle cx="22" cy="33" r="3"/>'#13#10'        <circle cx="38"' +
          ' cy="33" r="4"/>'#13#10'        <circle cx="14" cy="22" r="2"/>'#13#10'     ' +
          '   <circle cx="39" cy="22" r="2"/>'#13#10'        <circle cx="32" cy="' +
          '22" r="3"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'high_battery'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#CFD8DC' +
          '">'#13#10'        <path d="M34,44H14c-1.1,0-2-0.9-2-2V8c0-1.1,0.9-2,2-' +
          '2h20c1.1,0,2,0.9,2,2v34C36,43.1,35.1,44,34,44z"/>'#13#10'        <path' +
          ' d="M28,13h-8c-0.6,0-1-0.4-1-1V5c0-0.6,0.4-1,1-1h8c0.6,0,1,0.4,1' +
          ',1v7C29,12.6,28.6,13,28,13z"/>'#13#10'    </g>'#13#10'    <path fill="#8BC34' +
          'A" d="M34,44H14c-1.1,0-2-0.9-2-2V13h24v29C36,43.1,35.1,44,34,44z' +
          '"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'high_priority'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#F44' +
          '336" d="M21.2,44.8l-18-18c-1.6-1.6-1.6-4.1,0-5.7l18-18c1.6-1.6,4' +
          '.1-1.6,5.7,0l18,18c1.6,1.6,1.6,4.1,0,5.7l-18,18 C25.3,46.4,22.7,' +
          '46.4,21.2,44.8z"/>'#13#10'    <path fill="#fff" d="M21.6,32.7c0-0.3,0.' +
          '1-0.6,0.2-0.9c0.1-0.3,0.3-0.5,0.5-0.7c0.2-0.2,0.5-0.4,0.8-0.5s0.' +
          '6-0.2,1-0.2 s0.7,0.1,1,0.2c0.3,0.1,0.6,0.3,0.8,0.5c0.2,0.2,0.4,0' +
          '.4,0.5,0.7c0.1,0.3,0.2,0.6,0.2,0.9s-0.1,0.6-0.2,0.9s-0.3,0.5-0.5' +
          ',0.7 c-0.2,0.2-0.5,0.4-0.8,0.5c-0.3,0.1-0.6,0.2-1,0.2s-0.7-0.1-1' +
          '-0.2s-0.5-0.3-0.8-0.5c-0.2-0.2-0.4-0.4-0.5-0.7S21.6,33.1,21.6,32' +
          '.7z M25.8,28.1h-3.6L21.7,13h4.6L25.8,28.1z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'home'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'E8EAF6" points="42,39 6,39 6,23 24,6 42,23"/>'#13#10'    <g fill="#C5C' +
          'AE9">'#13#10'        <polygon points="39,21 34,16 34,9 39,9"/>'#13#10'      ' +
          '  <rect x="6" y="39" width="36" height="5"/>'#13#10'    </g>'#13#10'    <pol' +
          'ygon fill="#B71C1C" points="24,4.3 4,22.9 6,25.1 24,8.4 42,25.1 ' +
          '44,22.9"/>'#13#10'    <rect x="18" y="28" fill="#D84315" width="12" he' +
          'ight="16"/>'#13#10'    <rect x="21" y="17" fill="#01579B" width="6" he' +
          'ight="6"/>'#13#10'    <path fill="#FF8A65" d="M27.5,35.5c-0.3,0-0.5,0.' +
          '2-0.5,0.5v2c0,0.3,0.2,0.5,0.5,0.5S28,38.3,28,38v-2C28,35.7,27.8,' +
          '35.5,27.5,35.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'icons8_cup'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#4CA' +
          'F50" d="M40,14H8l3.8,28.3c0.1,1,1,1.7,2,1.7h20.5c1,0,1.8-0.7,2-1' +
          '.7L40,14z"/>'#13#10'    <g fill="#81C784">'#13#10'        <path d="M42,14H6v' +
          '-3c0-2.2,1.8-4,4-4h28c2.2,0,4,1.8,4,4V14z"/>'#13#10'        <path d="M' +
          '37.2,10H10.8l1.7-4.7c0.3-0.8,1-1.3,1.9-1.3h19.2c0.8,0,1.6,0.5,1.' +
          '9,1.3L37.2,10z"/>'#13#10'    </g>'#13#10'    <path fill="#E8F5E9" d="M28,28.' +
          '5c1.2-1.1,2-2.7,2-4.5c0-3.3-2.7-6-6-6c-3.3,0-6,2.7-6,6c0,1.8,0.8' +
          ',3.4,2,4.5c-1.2,1.1-2,2.7-2,4.5 c0,3.3,2.7,6,6,6c3.3,0,6-2.7,6-6' +
          'C30,31.2,29.2,29.6,28,28.5z M24,36c-1.7,0-3-1.3-3-3c0-1.7,1.3-3,' +
          '3-3c1.7,0,3,1.3,3,3 C27,34.7,25.7,36,24,36z M24,27c-1.7,0-3-1.3-' +
          '3-3c0-1.7,1.3-3,3-3c1.7,0,3,1.3,3,3C27,25.7,25.7,27,24,27z"/>'#13#10'<' +
          '/svg>'#13#10
      end
      item
        IconName = 'idea'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#F' +
          'FF59D" cx="24" cy="22" r="20"/>'#13#10'    <path fill="#FBC02D" d="M37' +
          ',22c0-7.7-6.6-13.8-14.5-12.9c-6,0.7-10.8,5.5-11.4,11.5c-0.5,4.6,' +
          '1.4,8.7,4.6,11.3 c1.4,1.2,2.3,2.9,2.3,4.8V37h12v-0.1c0-1.8,0.8-3' +
          '.6,2.2-4.8C35.1,29.7,37,26.1,37,22z"/>'#13#10'    <path fill="#FFF59D"' +
          ' d="M30.6,20.2l-3-2c-0.3-0.2-0.8-0.2-1.1,0L24,19.8l-2.4-1.6c-0.3' +
          '-0.2-0.8-0.2-1.1,0l-3,2 c-0.2,0.2-0.4,0.4-0.4,0.7s0,0.6,0.2,0.8l' +
          '3.8,4.7V37h2V26c0-0.2-0.1-0.4-0.2-0.6l-3.3-4.1l1.5-1l2.4,1.6c0.3' +
          ',0.2,0.8,0.2,1.1,0 l2.4-1.6l1.5,1l-3.3,4.1C25.1,25.6,25,25.8,25,' +
          '26v11h2V26.4l3.8-4.7c0.2-0.2,0.3-0.5,0.2-0.8S30.8,20.3,30.6,20.2' +
          'z"/>'#13#10'    <circle fill="#5C6BC0" cx="24" cy="44" r="3"/>'#13#10'    <p' +
          'ath fill="#9FA8DA" d="M26,45h-4c-2.2,0-4-1.8-4-4v-5h12v5C30,43.2' +
          ',28.2,45,26,45z"/>'#13#10'    <g fill="#5C6BC0">'#13#10'        <path d="M30' +
          ',41l-11.6,1.6c0.3,0.7,0.9,1.4,1.6,1.8l9.4-1.3C29.8,42.5,30,41.8,' +
          '30,41z"/>'#13#10'        <polygon points="18,38.7 18,40.7 30,39 30,37"' +
          '/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'image_file'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '90CAF9" points="40,45 8,45 8,3 30,3 40,13"/>'#13#10'    <polygon fill=' +
          '"#E1F5FE" points="38.5,14 29,14 29,4.5"/>'#13#10'    <polygon fill="#1' +
          '565C0" points="21,23 14,33 28,33"/>'#13#10'    <polygon fill="#1976D2"' +
          ' points="28,26.4 23,33 33,33"/>'#13#10'    <circle fill="#1976D2" cx="' +
          '31.5" cy="24.5" r="1.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'import'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#F8B' +
          'BD0" d="M7,40V8c0-2.2,1.8-4,4-4h24c2.2,0,4,1.8,4,4v32c0,2.2-1.8,' +
          '4-4,4H11C8.8,44,7,42.2,7,40z"/>'#13#10'    <g fill="#E91E63">'#13#10'       ' +
          ' <polygon points="13.3,24 24,15 24,33"/>'#13#10'        <rect x="19" y' +
          '="21" width="23" height="6"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'in_transit'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FFC' +
          '107" d="M44,36H30V16c0-1.1,0.9-2,2-2h8c0.6,0,1.2,0.3,1.6,0.8l6,7' +
          '.7c0.3,0.4,0.4,0.8,0.4,1.2V32 C48,34.2,46.2,36,44,36z"/>'#13#10'    <g' +
          ' fill="#9575CD">'#13#10'        <path d="M8,36h22V13c0-2.2-1.8-4-4-4H4' +
          'v23C4,34.2,5.8,36,8,36z"/>'#13#10'        <rect y="9" width="10" heigh' +
          't="2"/>'#13#10'        <rect y="14" width="10" height="2"/>'#13#10'        <' +
          'rect y="19" width="10" height="2"/>'#13#10'        <rect y="24" width=' +
          '"10" height="2"/>'#13#10'    </g>'#13#10'    <g fill="#7E57C2">'#13#10'        <re' +
          'ct x="4" y="11" width="16" height="2"/>'#13#10'        <rect x="4" y="' +
          '16" width="12" height="2"/>'#13#10'        <rect x="4" y="21" width="8' +
          '" height="2"/>'#13#10'        <rect x="4" y="26" width="4" height="2"/' +
          '>'#13#10'    </g>'#13#10'    <g fill="#37474F">'#13#10'        <circle cx="39" cy=' +
          '"36" r="5"/>'#13#10'        <circle cx="16" cy="36" r="5"/>'#13#10'    </g>'#13 +
          #10'    <g fill="#78909C">'#13#10'        <circle cx="39" cy="36" r="2.5"' +
          '/>'#13#10'        <circle cx="16" cy="36" r="2.5"/>'#13#10'    </g>'#13#10'    <pa' +
          'th fill="#455A64" d="M44,26h-3.6c-0.3,0-0.5-0.1-0.7-0.3l-1.4-1.4' +
          'c-0.2-0.2-0.4-0.3-0.7-0.3H34c-0.6,0-1-0.4-1-1v-6 c0-0.6,0.4-1,1-' +
          '1h5.5c0.3,0,0.6,0.1,0.8,0.4l4.5,5.4c0.1,0.2,0.2,0.4,0.2,0.6V25C4' +
          '5,25.6,44.6,26,44,26z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'info'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#2' +
          '196F3" cx="24" cy="24" r="21"/>'#13#10'    <rect x="22" y="22" fill="#' +
          'fff" width="4" height="11"/>'#13#10'    <circle fill="#fff" cx="24" cy' +
          '="16.5" r="2.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'inspection'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#455' +
          'A64" d="M36,4H26c0,1.1-0.9,2-2,2s-2-0.9-2-2H12C9.8,4,8,5.8,8,8v3' +
          '2c0,2.2,1.8,4,4,4h24c2.2,0,4-1.8,4-4V8 C40,5.8,38.2,4,36,4z"/>'#13#10 +
          '    <path fill="#fff" d="M36,41H12c-0.6,0-1-0.4-1-1V8c0-0.6,0.4-' +
          '1,1-1h24c0.6,0,1,0.4,1,1v32C37,40.6,36.6,41,36,41z"/>'#13#10'    <g fi' +
          'll="#90A4AE">'#13#10'        <path d="M26,4c0,1.1-0.9,2-2,2s-2-0.9-2-2' +
          'h-7v4c0,1.1,0.9,2,2,2h14c1.1,0,2-0.9,2-2V4H26z"/>'#13#10'        <path' +
          ' d="M24,0c-2.2,0-4,1.8-4,4s1.8,4,4,4s4-1.8,4-4S26.2,0,24,0z M24,' +
          '6c-1.1,0-2-0.9-2-2s0.9-2,2-2s2,0.9,2,2 S25.1,6,24,6z"/>'#13#10'    </g' +
          '>'#13#10'    <polygon fill="#4CAF50" points="30.6,18.6 21.6,27.6 17.4,' +
          '23.3 14.9,25.8 21.7,32.5 33.1,21.1"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'integrated_webcam'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#607' +
          'D8B" d="M38,42H10c-2.2,0-4-1.8-4-4V10c0-2.2,1.8-4,4-4h28c2.2,0,4' +
          ',1.8,4,4v28C42,40.2,40.2,42,38,42z"/>'#13#10'    <circle fill="#455A64' +
          '" cx="24" cy="24" r="12"/>'#13#10'    <circle fill="#42A5F5" cx="24" c' +
          'y="24" r="9"/>'#13#10'    <path fill="#90CAF9" d="M28.8,21c-1.2-1.4-3-' +
          '2.2-4.8-2.2s-3.6,0.8-4.8,2.2c-0.5,0.5-0.4,1.3,0.1,1.8c0.5,0.5,1.' +
          '3,0.4,1.8-0.1 c1.5-1.7,4.3-1.7,5.8,0c0.3,0.3,0.6,0.4,1,0.4c0.3,0' +
          ',0.6-0.1,0.9-0.3C29.2,22.4,29.3,21.5,28.8,21z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'internal'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#B' +
          '3E5FC" cx="24" cy="30" r="15"/>'#13#10'    <g fill="#1565C0">'#13#10'       ' +
          ' <polygon points="24,38.7 15,28 33,28"/>'#13#10'        <rect x="21" y' +
          '="5" width="6" height="26"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'invite'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#789' +
          '09C" d="M40,41H8c-2.2,0-4-1.8-4-4l0-20.9c0-1.3,0.6-2.5,1.7-3.3L2' +
          '4,0l18.3,12.8c1.1,0.7,1.7,2,1.7,3.3V37 C44,39.2,42.2,41,40,41z"/' +
          '>'#13#10'    <rect x="12" y="11" fill="#fff" width="24" height="22"/>'#13 +
          #10'    <path fill="#CFD8DC" d="M40,41H8c-2.2,0-4-1.8-4-4l0-20l20,1' +
          '3l20-13v20C44,39.2,42.2,41,40,41z"/>'#13#10'    <g fill="#4CAF50">'#13#10'  ' +
          '      <rect x="22" y="14" width="4" height="12"/>'#13#10'        <rect' +
          ' x="18" y="18" width="12" height="4"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ipad'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#E38' +
          '939" d="M8,41V7c0-2.2,1.8-4,4-4h24c2.2,0,4,1.8,4,4v34c0,2.2-1.8,' +
          '4-4,4H12C9.8,45,8,43.2,8,41z"/>'#13#10'    <path fill="#FFF3E0" d="M36' +
          ',6H12c-0.6,0-1,0.4-1,1v31c0,0.6,0.4,1,1,1h24c0.6,0,1-0.4,1-1V7C3' +
          '7,6.4,36.6,6,36,6z"/>'#13#10'    <circle fill="#A6642A" cx="24" cy="42' +
          '" r="1.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'iphone'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#E38' +
          '939" d="M12,40V8c0-2.2,1.8-4,4-4h16c2.2,0,4,1.8,4,4v32c0,2.2-1.8' +
          ',4-4,4H16C13.8,44,12,42.2,12,40z"/>'#13#10'    <path fill="#FFF3E0" d=' +
          '"M32,7H16c-0.6,0-1,0.4-1,1v29c0,0.6,0.4,1,1,1h16c0.6,0,1-0.4,1-1' +
          'V8C33,7.4,32.6,7,32,7z"/>'#13#10'    <circle fill="#A6642A" cx="24" cy' +
          '="41" r="1.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'key'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FFA000' +
          '">'#13#10'        <polygon points="30,41 26,45 22,45 18,41 18,21 30,21' +
          ' 30,29 28,31 30,33 30,35 28,37 30,39"/>'#13#10'        <path d="M38,7.' +
          '8C37.5,6,36,4.7,34.3,4.2C31.9,3.7,28.2,3,24,3s-7.9,0.7-10.3,1.2C' +
          '12,4.7,10.5,6,10,7.8 c-0.5,1.7-1,4.1-1,6.7c0,2.6,0.5,5,1,6.7c0.5' +
          ',1.8,1.9,3.1,3.7,3.5C16.1,25.3,19.8,26,24,26s7.9-0.7,10.3-1.2 c1' +
          '.8-0.4,3.2-1.8,3.7-3.5c0.5-1.7,1-4.1,1-6.7C39,11.9,38.5,9.5,38,7' +
          '.8z M29,13H19c-1.1,0-2-0.9-2-2V9c0-0.6,3.1-1,7-1s7,0.4,7,1v2 C31' +
          ',12.1,30.1,13,29,13z"/>'#13#10'    </g>'#13#10'    <rect x="23" y="26" fill=' +
          '"#D68600" width="2" height="19"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'kindle'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#374' +
          '74F" d="M8,41V7c0-2.2,1.8-4,4-4h24c2.2,0,4,1.8,4,4v34c0,2.2-1.8,' +
          '4-4,4H12C9.8,45,8,43.2,8,41z"/>'#13#10'    <path fill="#eee" d="M35,6H' +
          '13c-0.6,0-1,0.4-1,1v29c0,0.6,0.4,1,1,1h22c0.6,0,1-0.4,1-1V7C36,6' +
          '.4,35.6,6,35,6z"/>'#13#10'    <rect x="20" y="40" fill="#546E7A" width' +
          '="8" height="2"/>'#13#10'    <g fill="#A1A1A1">'#13#10'        <rect x="16" ' +
          'y="11" width="16" height="3"/>'#13#10'        <rect x="16" y="18" widt' +
          'h="16" height="2"/>'#13#10'        <rect x="16" y="22" width="12" heig' +
          'ht="2"/>'#13#10'        <rect x="16" y="26" width="16" height="2"/>'#13#10' ' +
          '       <rect x="16" y="30" width="12" height="2"/>'#13#10'    </g>'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'landscape'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FF9800' +
          '">'#13#10'        <rect x="36.1" y="8.1" transform="matrix(.707 .707 -' +
          '.707 .707 21.201 -25.184)" width="9.9" height="9.9"/>'#13#10'        <' +
          'rect x="36" y="8" width="10" height="10"/>'#13#10'    </g>'#13#10'    <circl' +
          'e fill="#FFEB3B" cx="41" cy="13" r="3"/>'#13#10'    <polygon fill="#2E' +
          '7D32" points="16.5,18 0,42 33,42"/>'#13#10'    <polygon fill="#4CAF50"' +
          ' points="33.6,24 19.2,42 48,42"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'leave'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FFC' +
          'DD2" d="M5,38V14h38v24c0,2.2-1.8,4-4,4H9C6.8,42,5,40.2,5,38z"/>'#13 +
          #10'    <path fill="#F44336" d="M43,10v6H5v-6c0-2.2,1.8-4,4-4h30C41' +
          '.2,6,43,7.8,43,10z"/>'#13#10'    <g fill="#B71C1C">'#13#10'        <circle c' +
          'x="33" cy="10" r="3"/>'#13#10'        <circle cx="15" cy="10" r="3"/>'#13 +
          #10'    </g>'#13#10'    <g fill="#BDBDBD">'#13#10'        <path d="M33,3c-1.1,0' +
          '-2,0.9-2,2v5c0,1.1,0.9,2,2,2s2-0.9,2-2V5C35,3.9,34.1,3,33,3z"/>'#13 +
          #10'        <path d="M15,3c-1.1,0-2,0.9-2,2v5c0,1.1,0.9,2,2,2s2-0.9' +
          ',2-2V5C17,3.9,16.1,3,15,3z"/>'#13#10'    </g>'#13#10'    <path fill="#F44336' +
          '" d="M22.2,35.3c0-0.2,0-0.5,0.1-0.7c0.1-0.2,0.2-0.4,0.4-0.5s0.3-' +
          '0.3,0.5-0.3c0.2-0.1,0.5-0.1,0.7-0.1 s0.5,0,0.7,0.1c0.2,0.1,0.4,0' +
          '.2,0.6,0.3s0.3,0.3,0.4,0.5c0.1,0.2,0.1,0.4,0.1,0.7c0,0.2,0,0.5-0' +
          '.1,0.7c-0.1,0.2-0.2,0.4-0.4,0.5 c-0.2,0.1-0.3,0.3-0.6,0.3S24.3,3' +
          '7,24,37s-0.5,0-0.7-0.1c-0.2-0.1-0.4-0.2-0.5-0.3c-0.2-0.1-0.3-0.3' +
          '-0.4-0.5 C22.3,35.8,22.2,35.6,22.2,35.3z M25.3,31h-2.6l-0.4-11h3' +
          '.3L25.3,31z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'left'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#3F51B5' +
          '">'#13#10'        <polygon points="4,24 18,12.3 18,35.7"/>'#13#10'        <r' +
          'ect x="15" y="20" width="27" height="8"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'left_down2'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '3F51B5" points="19,44 30.7,30 7.3,30"/>'#13#10'    <path fill="#3F51B5' +
          '" d="M27,6h13v8H27c-2.2,0-4,1.8-4,4v17h-8V18C15,11.4,20.4,6,27,6' +
          'z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'left_up2'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '3F51B5" points="19,4 30.7,18 7.3,18"/>'#13#10'    <path fill="#3F51B5"' +
          ' d="M27,42h13v-8H27c-2.2,0-4-1.8-4-4V13h-8v17C15,36.6,20.4,42,27' +
          ',42z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'library'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FF9800' +
          '">'#13#10'        <rect x="1" y="38" width="46" height="2"/>'#13#10'        ' +
          '<rect x="25" y="18" width="4" height="16"/>'#13#10'        <rect x="31' +
          '" y="18" width="4" height="16"/>'#13#10'        <rect x="37" y="18" wi' +
          'dth="4" height="16"/>'#13#10'        <rect x="19" y="18" width="4" hei' +
          'ght="16"/>'#13#10'        <rect x="13" y="18" width="4" height="16"/>'#13 +
          #10'        <rect x="7" y="18" width="4" height="16"/>'#13#10'        <po' +
          'lygon points="43,16 5,16 5,13 24,4 43,13"/>'#13#10'        <rect x="5"' +
          ' y="34" width="38" height="2"/>'#13#10'    </g>'#13#10'    <g fill="#EF6C00"' +
          '>'#13#10'        <rect x="25" y="16" width="4" height="2"/>'#13#10'        <' +
          'rect x="31" y="16" width="4" height="2"/>'#13#10'        <rect x="37" ' +
          'y="16" width="4" height="2"/>'#13#10'        <rect x="19" y="16" width' +
          '="4" height="2"/>'#13#10'        <rect x="13" y="16" width="4" height=' +
          '"2"/>'#13#10'        <rect x="7" y="16" width="4" height="2"/>'#13#10'      ' +
          '  <rect x="3" y="36" width="42" height="2"/>'#13#10'        <circle cx' +
          '="24" cy="11" r="2"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'light_at_the_end_of_tunnel'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#3F5' +
          '1B5" d="M6,10v28c0,2.2,1.8,4,4,4h28c2.2,0,4-1.8,4-4V10c0-2.2-1.8' +
          '-4-4-4H10C7.8,6,6,7.8,6,10z"/>'#13#10'    <path fill="#CCF2F6" d="M27.' +
          '9,28.9h-5.8l-8.4,7.2l6-7.2v-2.4l-3,0.8l3-1.9v-1.5c0-0.8,0.1-1.7,' +
          '0.6-2.4l-7.5-8.3l8.7,7.2 c0.7-0.7,1.5-1.1,2.5-1.2l0.6-7.3l1.1,7.' +
          '3c0.3,0,0.6,0.1,0.8,0.1l1.2-1.2l-0.3,1.7c0.3,0.1,0.4,0.3,0.7,0.6' +
          'l4.4-2.8l-3.6,3.9 c0.3,0.4,0.6,1,0.7,1.7l2.2,0.1l-2.2,0.8c0,0.3,' +
          '0,1.5,0,1.5l2.6,1.4l-2.6-0.3c0,0,0,1.8,0,2.2l6.2,7.1L27.9,28.9z"' +
          '/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'like'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#F44' +
          '336" d="M34,9c-4.2,0-7.9,2.1-10,5.4C21.9,11.1,18.2,9,14,9C7.4,9,' +
          '2,14.4,2,21c0,11.9,22,24,22,24s22-12,22-24 C46,14.4,40.6,9,34,9z' +
          '"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'like_placeholder'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FFC' +
          'DD2" d="M34,9c-4.2,0-7.9,2.1-10,5.4C21.9,11.1,18.2,9,14,9C7.4,9,' +
          '2,14.4,2,21c0,11.9,22,24,22,24s22-12,22-24 C46,14.4,40.6,9,34,9z' +
          '"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'line_chart'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#3F51B5' +
          '">'#13#10'        <circle cx="8" cy="38" r="3"/>'#13#10'        <circle cx="' +
          '16" cy="40" r="3"/>'#13#10'        <circle cx="24" cy="33" r="3"/>'#13#10'  ' +
          '      <circle cx="32" cy="35" r="3"/>'#13#10'        <circle cx="40" c' +
          'y="31" r="3"/>'#13#10'        <polygon points="39.1,29.2 31.8,32.9 23.' +
          '5,30.8 15.5,37.8 8.5,36.1 7.5,39.9 16.5,42.2 24.5,35.2 32.2,37.1' +
          ' 40.9,32.8"/>'#13#10'    </g>'#13#10'    <g fill="#00BCD4">'#13#10'        <circle' +
          ' cx="8" cy="20" r="3"/>'#13#10'        <circle cx="16" cy="22" r="3"/>' +
          #13#10'        <circle cx="24" cy="15" r="3"/>'#13#10'        <circle cx="3' +
          '2" cy="20" r="3"/>'#13#10'        <circle cx="40" cy="8" r="3"/>'#13#10'    ' +
          '    <path d="M38.3,6.9c-2.1,3.2-5.3,8-6.9,10.4c-1.2-0.7-3.1-2-6.' +
          '4-4l-1.3-0.8l-8.3,7.3l-7-1.7l-1,3.9l9,2.3l7.7-6.7 c2.6,1.6,5.8,3' +
          '.6,6.5,4.1l0.5,0.5l0.9-0.1c1.1-0.1,1.1-0.1,9.5-12.9L38.3,6.9z"/>' +
          #13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'link'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#1976D2' +
          '">'#13#10'        <path d="M38,13h-3c-5.5,0-10,4.5-10,10s4.5,10,10,10h' +
          '3c5.5,0,10-4.5,10-10S43.5,13,38,13z M38,29h-3 c-3.3,0-6-2.7-6-6s' +
          '2.7-6,6-6h3c3.3,0,6,2.7,6,6S41.3,29,38,29z"/>'#13#10'        <path d="' +
          'M13,13h-3C4.5,13,0,17.5,0,23s4.5,10,10,10h3c5.5,0,10-4.5,10-10S1' +
          '8.5,13,13,13z M13,29h-3 c-3.3,0-6-2.7-6-6s2.7-6,6-6h3c3.3,0,6,2.' +
          '7,6,6S16.3,29,13,29z"/>'#13#10'    </g>'#13#10'    <path fill="#42A5F5" d="M' +
          '33,21H15c-1.1,0-2,0.9-2,2s0.9,2,2,2h18c1.1,0,2-0.9,2-2S34.1,21,3' +
          '3,21z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'linux'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 19.1.1, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<svg version="1.1"  xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"'#13#10#9' vie' +
          'wBox="0 2 48 48" enable-background="new 0 2 48 48" xml:space="pr' +
          'eserve">'#13#10'<polygon fill="#ECEFF1" points="20.1,18.2 20.2,20.5 18' +
          '.6,23.5 16.1,28.4 15.6,32.5 17.4,38.3 21.5,40.6 27.7,40.6 33.5,3' +
          '6.2 '#13#10#9'36.1,29.3 30.1,22 28.4,17.9 "/>'#13#10'<path fill="#263238" d="' +
          'M34.3,23.9c-1.6-2.3-2.9-3.7-3.6-6.6c-0.7-2.9,0.2-2.1-0.4-4.6c-0.' +
          '3-1.3-0.8-2.2-1.3-2.9'#13#10#9'c-0.6-0.7-1.3-1.1-1.7-1.2c-0.9-0.5-3-1.3' +
          '-5.6,0.1c-2.7,1.4-2.4,4.4-1.9,10.5c0,0.4-0.1,0.9-0.3,1.3c-0.4,0.' +
          '9-1.1,1.7-1.7,2.4'#13#10#9'c-0.7,1-1.4,2-1.9,3.1c-1.2,2.3-2.3,5.2-2,6.3' +
          'c0.5-0.1,6.8,9.5,6.8,9.7c0.4-0.1,2.1-0.1,3.6-0.1c2.1-0.1,3.3-0.2' +
          ',5,0.2'#13#10#9'c0-0.3-0.1-0.6-0.1-0.9c0-0.6,0.1-1.1,0.2-1.8c0.1-0.5,0.' +
          '2-1,0.3-1.6c-1,0.9-2.8,1.9-4.5,2.2c-1.5,0.3-4-0.2-5.2-1.7'#13#10#9'c0.1' +
          ',0,0.3,0,0.4-0.1c0.3-0.1,0.6-0.2,0.7-0.4c0.3-0.5,0.1-1-0.1-1.3c-' +
          '0.2-0.3-1.7-1.4-2.4-2c-0.7-0.6-1.1-0.9-1.5-1.3'#13#10#9'c0,0-0.6-0.6-0.' +
          '8-0.8c-0.2-0.2-0.3-0.4-0.4-0.5c-0.2-0.5-0.3-1.1-0.2-1.9c0.1-1.1,' +
          '0.5-2,1-3c0.2-0.4,0.7-1.2,0.7-1.2'#13#10#9's-1.7,4.2-0.8,5.5c0,0,0.1-1.' +
          '3,0.5-2.6c0.3-0.9,0.8-2.2,1.4-2.9s2.1-3.3,2.2-4.9c0-0.7,0.1-1.4,' +
          '0.1-1.9c-0.4-0.4,6.6-1.4,7-0.3'#13#10#9'c0.1,0.4,1.5,4,2.3,5.9c0.4,0.9,' +
          '0.9,1.7,1.2,2.7c0.3,1.1,0.5,2.6,0.5,4.1c0,0.3,0,0.8-0.1,1.3c0.2,' +
          '0,4.1-4.2-0.5-7.7'#13#10#9'c0,0,2.8,1.3,2.9,3.9c0.1,2.1-0.8,3.8-1,4.1c0' +
          '.1,0,2.1,0.9,2.2,0.9c0.4,0,1.2-0.3,1.2-0.3c0.1-0.3,0.4-1.1,0.4-1' +
          '.4'#13#10#9'C37.6,29.9,35.9,26.2,34.3,23.9z"/>'#13#10'<g>'#13#10#9'<ellipse fill="#E' +
          'CEFF1" cx="21.6" cy="15.3" rx="1.3" ry="2"/>'#13#10#9'<ellipse fill="#E' +
          'CEFF1" cx="26.1" cy="15.2" rx="1.7" ry="2.3"/>'#13#10'</g>'#13#10'<g>'#13#10#9#13#10#9#9 +
          '<ellipse transform="matrix(-0.1254 -0.9921 0.9921 -0.1254 8.9754' +
          ' 38.9969)" fill="#212121" cx="21.7" cy="15.5" rx="1.2" ry="0.7"/' +
          '>'#13#10#9'<ellipse fill="#212121" cx="26" cy="15.6" rx="1" ry="1.3"/>'#13 +
          #10'</g>'#13#10'<g>'#13#10#9'<path fill="#FFC107" d="M39.3,37.6c-0.4-0.2-1.1-0.5' +
          '-1.7-1.4c-0.3-0.5-0.2-1.9-0.7-2.5c-0.3-0.4-0.7-0.2-0.8-0.2'#13#10#9#9'c-' +
          '0.9,0.2-3,1.6-4.4,0c-0.2-0.2-0.5-0.5-1-0.5c-0.5,0-0.7,0.2-0.9,0.' +
          '6s-0.2,0.7-0.2,1.7c0,0.8,0,1.7-0.1,2.4'#13#10#9#9'c-0.2,1.7-0.5,2.7-0.5,' +
          '3.7c0,1.1,0.3,1.8,0.7,2.1c0.3,0.3,0.8,0.5,1.9,0.5c1.1,0,1.8-0.4,' +
          '2.5-1.1c0.5-0.5,0.9-0.7,2.3-1.7'#13#10#9#9'c1.1-0.7,2.8-1.6,3.1-1.9c0.2-' +
          '0.2,0.5-0.3,0.5-0.9C40,37.9,39.6,37.7,39.3,37.6z"/>'#13#10#9'<path fill' +
          '="#FFC107" d="M19.2,37.9c-1-1.6-1.1-1.9-1.8-2.9c-0.6-1-1.9-2.9-2' +
          '.7-2.9c-0.6,0-0.9,0.3-1.3,0.7'#13#10#9#9'c-0.4,0.4-0.8,1.3-1.5,1.8c-0.6,' +
          '0.5-2.3,0.4-2.7,1c-0.4,0.6,0.4,1.5,0.4,3c0,0.6-0.5,1-0.6,1.4c-0.' +
          '1,0.5-0.2,0.8,0,1.2'#13#10#9#9'c0.4,0.6,0.9,0.8,4.3,1.5c1.8,0.4,3.5,1.4,' +
          '4.6,1.5c1.1,0.1,3,0,3-2.7C21,39.9,20.1,39.5,19.2,37.9z"/>'#13#10#9'<pat' +
          'h fill="#FFC107" d="M21.1,19.8C20.5,19.4,20,19,20,18.4c0-0.6,0.4' +
          '-0.8,1-1.3c0.1-0.1,1.2-1.1,2.3-1.1s2.4,0.7,2.9,0.9'#13#10#9#9'c0.9,0.2,1' +
          '.8,0.4,1.7,1.1c-0.1,1-0.2,1.2-1.2,1.7c-0.7,0.2-2,1.3-2.9,1.3c-0.' +
          '4,0-1,0-1.4-0.1C22.1,20.8,21.6,20.3,21.1,19.8z"/>'#13#10'</g>'#13#10'<g>'#13#10#9'<' +
          'path fill="#634703" d="M20.9,19c0.2,0.2,0.5,0.4,0.8,0.5c0.2,0.1,' +
          '0.5,0.2,0.5,0.2c0.4,0,0.7,0,0.9,0c0.5,0,1.2-0.2,1.9-0.6'#13#10#9#9'c0.7-' +
          '0.3,0.8-0.5,1.3-0.7c0.5-0.3,1-0.6,0.8-0.7c-0.2-0.1-0.4,0-1.1,0.4' +
          'c-0.6,0.4-1.1,0.6-1.7,0.9c-0.3,0.1-0.7,0.3-1,0.3'#13#10#9#9'c-0.3,0-0.6,' +
          '0-0.9,0c-0.3,0-0.5-0.1-0.8-0.2c-0.2-0.1-0.3-0.2-0.4-0.2c-0.2-0.1' +
          '-0.6-0.5-0.8-0.6c0,0-0.2,0-0.1,0.1'#13#10#9#9'C20.6,18.7,20.7,18.8,20.9,' +
          '19z"/>'#13#10#9'<path fill="#634703" d="M23.9,16.8c0.1,0.2,0.3,0.2,0.4,' +
          '0.3c0.1,0.1,0.2,0.1,0.2,0.1c0.1-0.1,0-0.3-0.1-0.3'#13#10#9#9'C24.4,16.7,' +
          '23.9,16.7,23.9,16.8z"/>'#13#10#9'<path fill="#634703" d="M22.3,17c0,0.1' +
          ',0.2,0.2,0.2,0.1c0.1-0.1,0.2-0.2,0.3-0.2c0.2-0.1,0.1-0.2-0.2-0.2' +
          #13#10#9#9'C22.4,16.8,22.4,16.9,22.3,17z"/>'#13#10'</g>'#13#10'<path fill="#455A64"' +
          ' d="M32,34.7c0,0.1,0,0.2,0,0.3c0.2,0.4,0.7,0.5,1.1,0.5c0.6,0,1.2' +
          '-0.4,1.5-0.8c0-0.1,0.1-0.2,0.2-0.3'#13#10#9'c0.2-0.3,0.3-0.5,0.4-0.6c0,' +
          '0-0.1-0.1-0.1-0.2c-0.1-0.2-0.4-0.4-0.8-0.5c-0.3-0.1-0.8-0.2-1-0.' +
          '2c-0.9-0.1-1.4,0.2-1.7,0.5'#13#10#9'c0,0,0.1,0,0.1,0.1c0.2,0.2,0.3,0.4,' +
          '0.3,0.7C32.1,34.4,32,34.5,32,34.7z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'list'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#2196F3' +
          '">'#13#10'        <rect x="6" y="22" width="4" height="4"/>'#13#10'        <' +
          'rect x="6" y="14" width="4" height="4"/>'#13#10'        <rect x="6" y=' +
          '"30" width="4" height="4"/>'#13#10'        <rect x="6" y="6" width="4"' +
          ' height="4"/>'#13#10'        <rect x="6" y="38" width="4" height="4"/>' +
          #13#10'    </g>'#13#10'    <g fill="#2196F3">'#13#10'        <rect x="14" y="22" ' +
          'width="28" height="4"/>'#13#10'        <rect x="14" y="14" width="28" ' +
          'height="4"/>'#13#10'        <rect x="14" y="30" width="28" height="4"/' +
          '>'#13#10'        <rect x="14" y="6" width="28" height="4"/>'#13#10'        <' +
          'rect x="14" y="38" width="28" height="4"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'lock'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#424' +
          '242" d="M24,4c-5.5,0-10,4.5-10,10v4h4v-4c0-3.3,2.7-6,6-6s6,2.7,6' +
          ',6v4h4v-4C34,8.5,29.5,4,24,4z"/>'#13#10'    <path fill="#FB8C00" d="M3' +
          '6,44H12c-2.2,0-4-1.8-4-4V22c0-2.2,1.8-4,4-4h24c2.2,0,4,1.8,4,4v1' +
          '8C40,42.2,38.2,44,36,44z"/>'#13#10'    <circle fill="#C76E00" cx="24" ' +
          'cy="31" r="3"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'lock_landscape'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#374' +
          '74F" d="M7,10h34c2.2,0,4,1.8,4,4v20c0,2.2-1.8,4-4,4H7c-2.2,0-4-1' +
          '.8-4-4V14C3,11.8,4.8,10,7,10z"/>'#13#10'    <path fill="#BBDEFB" d="M4' +
          '2,34V14c0-0.6-0.4-1-1-1H7c-0.6,0-1,0.4-1,1v20c0,0.6,0.4,1,1,1h34' +
          'C41.6,35,42,34.6,42,34z"/>'#13#10'    <g fill="#3F51B5">'#13#10'        <pat' +
          'h d="M29,31H19c-0.6,0-1-0.4-1-1v-6c0-0.6,0.4-1,1-1h10c0.6,0,1,0.' +
          '4,1,1v6C30,30.6,29.6,31,29,31z"/>'#13#10'        <path d="M24,17c-2.2,' +
          '0-4,1.8-4,4v3h2v-3c0-1.1,0.9-2,2-2s2,0.9,2,2v3h2v-3C28,18.8,26.2' +
          ',17,24,17z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'lock_portrait'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#374' +
          '74F" d="M10,41V7c0-2.2,1.8-4,4-4h20c2.2,0,4,1.8,4,4v34c0,2.2-1.8' +
          ',4-4,4H14C11.8,45,10,43.2,10,41z"/>'#13#10'    <path fill="#BBDEFB" d=' +
          '"M34,6H14c-0.6,0-1,0.4-1,1v34c0,0.6,0.4,1,1,1h20c0.6,0,1-0.4,1-1' +
          'V7C35,6.4,34.6,6,34,6z"/>'#13#10'    <g fill="#3F51B5">'#13#10'        <path' +
          ' d="M29,30H19c-0.6,0-1-0.4-1-1v-6c0-0.6,0.4-1,1-1h10c0.6,0,1,0.4' +
          ',1,1v6C30,29.6,29.6,30,29,30z"/>'#13#10'        <path d="M24,16c-2.2,0' +
          '-4,1.8-4,4v3h2v-3c0-1.1,0.9-2,2-2s2,0.9,2,2v3h2v-3C28,17.8,26.2,' +
          '16,24,16z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'low_battery'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#CFD8DC' +
          '">'#13#10'        <path d="M34,44H14c-1.1,0-2-0.9-2-2V8c0-1.1,0.9-2,2-' +
          '2h20c1.1,0,2,0.9,2,2v34C36,43.1,35.1,44,34,44z"/>'#13#10'        <path' +
          ' d="M28,13h-8c-0.6,0-1-0.4-1-1V5c0-0.6,0.4-1,1-1h8c0.6,0,1,0.4,1' +
          ',1v7C29,12.6,28.6,13,28,13z"/>'#13#10'    </g>'#13#10'    <path fill="#8BC34' +
          'A" d="M34,44H14c-1.1,0-2-0.9-2-2v-9h24v9C36,43.1,35.1,44,34,44z"' +
          '/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'low_priority'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#4CA' +
          'F50" d="M21.2,44.8l-18-18c-1.6-1.6-1.6-4.1,0-5.7l18-18c1.6-1.6,4' +
          '.1-1.6,5.7,0l18,18c1.6,1.6,1.6,4.1,0,5.7l-18,18 C25.3,46.4,22.7,' +
          '46.4,21.2,44.8z"/>'#13#10'    <g fill="#FFEB3B">'#13#10'        <polygon poi' +
          'nts="24,33.4 17,25 31,25"/>'#13#10'        <rect x="22" y="14.8" width' +
          '="4" height="12.3"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'make_decision'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FFCC80' +
          '">'#13#10'        <circle cx="38" cy="26" r="4"/>'#13#10'        <circle cx=' +
          '"10" cy="26" r="4"/>'#13#10'        <path d="M39,19c0-12.7-30-8.3-30,0' +
          'c0,1.8,0,8.2,0,10c0,8.3,6.7,15,15,15s15-6.7,15-15C39,27.2,39,20.' +
          '8,39,19z"/>'#13#10'        <path d="M24,4C15.2,4,8,11.2,8,20c0,1.2,0,3' +
          '.5,0,3.5l2.1,0.6V19l19.5-6.3l8.2,6.3v5.1l2.1-0.6c0,0,0-2.3,0-3.5' +
          ' C40,12.5,34.6,4,24,4z"/>'#13#10'    </g>'#13#10'    <polygon fill="#FF5722"' +
          ' points="24,23.5 24,12.5 30.6,18"/>'#13#10'    <path fill="#FF5722" d=' +
          '"M28.9,24.4c0,0.2,0.1,0.4,0.1,0.6c0,2.8-2.2,5-5,5s-5-2.2-5-5s2.2' +
          '-5,5-5c0.7,0,1.4,0.2,2,0.4v-4.2 c-0.6-0.1-1.3-0.2-2-0.2c-5,0-9,4' +
          '-9,9s4,9,9,9s9-4,9-9c0-1.2-0.2-2.4-0.7-3.4L28.9,24.4z"/>'#13#10'</svg>' +
          #13#10
      end
      item
        IconName = 'manager'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'FF9800" points="24,37 19,31 19,25 29,25 29,31"/>'#13#10'    <g fill="#' +
          'FFA726">'#13#10'        <circle cx="33" cy="19" r="2"/>'#13#10'        <circ' +
          'le cx="15" cy="19" r="2"/>'#13#10'    </g>'#13#10'    <path fill="#FFB74D" d' +
          '="M33,13c0-7.6-18-5-18,0c0,1.1,0,5.9,0,7c0,5,4,9,9,9s9-4,9-9C33,' +
          '18.9,33,14.1,33,13z"/>'#13#10'    <path fill="#FF5722" d="M24,4c-6.1,0' +
          '-10,4.9-10,11c0,0.8,0,2.3,0,2.3l2,1.7v-5l12-4l4,4v5l2-1.7c0,0,0-' +
          '1.5,0-2.3c0-4-1-8-6-9l-1-2 H24z"/>'#13#10'    <g fill="#784719">'#13#10'    ' +
          '    <circle cx="28" cy="19" r="1"/>'#13#10'        <circle cx="20" cy=' +
          '"19" r="1"/>'#13#10'    </g>'#13#10'    <path fill="#CFD8DC" d="M29,31L29,31' +
          'l-5,1l-5-1c0,0-11,2-11,13h32C40,33,29,31,29,31z"/>'#13#10'    <polygon' +
          ' fill="#3F51B5" points="23,35 22,44 26,44 25,35 26,34 24,32 22,3' +
          '4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'medium_priority'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FFC' +
          '107" d="M21.2,44.8l-18-18c-1.6-1.6-1.6-4.1,0-5.7l18-18c1.6-1.6,4' +
          '.1-1.6,5.7,0l18,18c1.6,1.6,1.6,4.1,0,5.7l-18,18 C25.3,46.4,22.7,' +
          '46.4,21.2,44.8z"/>'#13#10'    <g fill="#37474F">'#13#10'        <circle cx="' +
          '24" cy="24" r="2"/>'#13#10'        <circle cx="32" cy="24" r="2"/>'#13#10'  ' +
          '      <circle cx="16" cy="24" r="2"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'menu'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#607D8B' +
          '">'#13#10'        <rect x="6" y="22" width="36" height="4"/>'#13#10'        ' +
          '<rect x="6" y="10" width="36" height="4"/>'#13#10'        <rect x="6" ' +
          'y="34" width="36" height="4"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'middle_battery'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#CFD8DC' +
          '">'#13#10'        <path d="M34,44H14c-1.1,0-2-0.9-2-2V8c0-1.1,0.9-2,2-' +
          '2h20c1.1,0,2,0.9,2,2v34C36,43.1,35.1,44,34,44z"/>'#13#10'        <path' +
          ' d="M28,13h-8c-0.6,0-1-0.4-1-1V5c0-0.6,0.4-1,1-1h8c0.6,0,1,0.4,1' +
          ',1v7C29,12.6,28.6,13,28,13z"/>'#13#10'    </g>'#13#10'    <path fill="#8BC34' +
          'A" d="M34,44H14c-1.1,0-2-0.9-2-2V23h24v19C36,43.1,35.1,44,34,44z' +
          '"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'mind_map'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'CFD8DC" points="39.4,23 38.6,19 26,21.6 26,8 22,8 22,20.3 8.1,11' +
          '.3 5.9,14.7 21.1,24.5 9.4,39.8 12.6,42.2 23.9,27.4 32.3,40.1 35.' +
          '7,37.9 27.3,25.4"/>'#13#10'    <circle fill="#3F51B5" cx="24" cy="24" ' +
          'r="7"/>'#13#10'    <g fill="#00BCD4">'#13#10'        <circle cx="24" cy="8" ' +
          'r="5"/>'#13#10'        <circle cx="39" cy="21" r="5"/>'#13#10'        <circl' +
          'e cx="7" cy="13" r="5"/>'#13#10'        <circle cx="11" cy="41" r="5"/' +
          '>'#13#10'        <circle cx="34" cy="39" r="5"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'missed_call'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#F44336' +
          '">'#13#10'        <polygon points="30.3,12.9 24,19.2 15.7,10.9 12.9,13' +
          '.7 24,24.8 33.1,15.7"/>'#13#10'        <polygon points="36,19 27,10 36' +
          ',10"/>'#13#10'    </g>'#13#10'    <path fill="#009688" d="M44.5,30.8l-2.4-2.' +
          '4c-8.5-8.3-28.9-7.1-36.2,0l-2.4,2.4c-0.7,0.7-0.7,1.7,0,2.4l4.8,4' +
          '.7 c0.7,0.7,1.7,0.7,2.4,0l5.3-5.1l-0.4-5.6c1.7-1.7,15.1-1.7,16.8' +
          ',0L32.1,33l5.1,4.9c0.7,0.7,1.7,0.7,2.4,0l4.8-4.7 C45.2,32.5,45.2' +
          ',31.4,44.5,30.8z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'mms'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#E91' +
          'E63" d="M37,39H11l-6,6V11c0-3.3,2.7-6,6-6h26c3.3,0,6,2.7,6,6v22C' +
          '43,36.3,40.3,39,37,39z"/>'#13#10'    <polygon fill="#F48FB1" points="2' +
          '0,16.5 10,31 30,31"/>'#13#10'    <g fill="#F8BBD0">'#13#10'        <circle c' +
          'x="34" cy="15" r="3"/>'#13#10'        <polygon points="30,21 22,31 38,' +
          '31"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'money_transfer'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#789' +
          '09C" d="M40,41H8c-2.2,0-4-1.8-4-4l0-20.9c0-1.3,0.6-2.5,1.7-3.3L2' +
          '4,0l18.3,12.8c1.1,0.7,1.7,2,1.7,3.3V37 C44,39.2,42.2,41,40,41z"/' +
          '>'#13#10'    <rect x="14" y="1" fill="#AED581" width="20" height="31"/' +
          '>'#13#10'    <g fill="#558B2F">'#13#10'        <path d="M13,0v33h22V0H13z M3' +
          '3,31H15V2h18V31z"/>'#13#10'        <path d="M34,3c0,1.7-0.3,3-2,3c-1.7' +
          ',0-3-1.3-3-3s1.3-2,3-2C33.7,1,34,1.3,34,3z"/>'#13#10'        <path d="' +
          'M16,1c1.7,0,3,0.3,3,2s-1.3,3-3,3s-2-1.3-2-3S14.3,1,16,1z"/>'#13#10'   ' +
          '     <circle cx="24" cy="8" r="2"/>'#13#10'        <circle cx="24" cy=' +
          '"20" r="6"/>'#13#10'    </g>'#13#10'    <path fill="#CFD8DC" d="M40,41H8c-2.' +
          '2,0-4-1.8-4-4l0-20l20,13l20-13v20C44,39.2,42.2,41,40,41z"/>'#13#10'</s' +
          'vg>'#13#10
      end
      item
        IconName = 'multiple_cameras'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#455' +
          'A64" d="M42,41H12c-2.2,0-4-1.8-4-4V17c0-2.2,1.8-4,4-4h30c2.2,0,4' +
          ',1.8,4,4v20C46,39.2,44.2,41,42,41z"/>'#13#10'    <path fill="#78909C" ' +
          'd="M36,36H6c-2.2,0-4-1.8-4-4V12c0-2.2,1.8-4,4-4h30c2.2,0,4,1.8,4' +
          ',4v20C40,34.2,38.2,36,36,36z"/>'#13#10'    <circle fill="#455A64" cx="' +
          '26" cy="22" r="10"/>'#13#10'    <circle fill="#42A5F5" cx="26" cy="22"' +
          ' r="7"/>'#13#10'    <path fill="#90CAF9" d="M29.7,19.7c-1-1.1-2.3-1.7-' +
          '3.7-1.7s-2.8,0.6-3.7,1.7c-0.4,0.4-0.3,1,0.1,1.4c0.4,0.4,1,0.3,1.' +
          '4-0.1 c1.2-1.3,3.3-1.3,4.5,0c0.2,0.2,0.5,0.3,0.7,0.3c0.2,0,0.5-0' +
          '.1,0.7-0.3C30.1,20.7,30.1,20.1,29.7,19.7z"/>'#13#10'    <rect x="6" y=' +
          '"12" fill="#ADD8FB" width="6" height="3"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'multiple_devices'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#546' +
          'E7A" d="M4,28V8c0-2.2,1.8-4,4-4h28c2.2,0,4,1.8,4,4v20c0,2.2-1.8,' +
          '4-4,4H8C5.8,32,4,30.2,4,28z"/>'#13#10'    <path fill="#BBDEFB" d="M36,' +
          '7H8C7.4,7,7,7.4,7,8v20c0,0.6,0.4,1,1,1h28c0.6,0,1-0.4,1-1V8C37,7' +
          '.4,36.6,7,36,7z"/>'#13#10'    <path fill="#37474F" d="M38,33H6c-2.2,0-' +
          '4-1.8-4-4v0h40v0C42,31.2,40.2,33,38,33z"/>'#13#10'    <path fill="#E38' +
          '939" d="M24,40V16c0-2.2,1.8-4,4-4h12c2.2,0,4,1.8,4,4v24c0,2.2-1.' +
          '8,4-4,4H28C25.8,44,24,42.2,24,40z"/>'#13#10'    <path fill="#FFF3E0" d' +
          '="M40,15H28c-0.6,0-1,0.4-1,1v22c0,0.6,0.4,1,1,1h12c0.6,0,1-0.4,1' +
          '-1V16C41,15.4,40.6,15,40,15z"/>'#13#10'    <circle fill="#A6642A" cx="' +
          '34" cy="41.5" r="1.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'multiple_inputs'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#90A' +
          '4AE" d="M40,35v5H8v-5H4v5c0,2.2,1.8,4,4,4h32c2.2,0,4-1.8,4-4v-5H' +
          '40z"/>'#13#10'    <g fill="#1565C0">'#13#10'        <polygon points="24,23.4' +
          ' 17,15 31,15"/>'#13#10'        <rect x="22" y="4" width="4" height="14' +
          '"/>'#13#10'        <path d="M31.5,26.9L30.8,28l3.5,1.9l0.6-1.2c1.6-3,2' +
          '.6-4.7,3.5-5.2C39.3,23,41,23,44,23v-4 C36.3,19,35.6,19.4,31.5,26' +
          '.9z"/>'#13#10'        <polygon points="38.4,31 29.4,35 28,25"/>'#13#10'     ' +
          '   <path d="M16.5,26.9l0.6,1.2L13.6,30L13,28.8c-1.6-3-2.6-4.7-3.' +
          '5-5.2C8.7,23,7,23,4,23v-4 C11.7,19,12.4,19.4,16.5,26.9z"/>'#13#10'    ' +
          '    <polygon points="20,25 18.6,35 9.6,31"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'multiple_smartphones'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#455' +
          'A64" d="M4,31V8c0-2.2,1.8-4,4-4h12c2.2,0,4,1.8,4,4v23c0,2.2-1.8,' +
          '4-4,4H8C5.8,35,4,33.2,4,31z"/>'#13#10'    <path fill="#BBDEFB" d="M20,' +
          '7H8C7.4,7,7,7.4,7,8v21c0,0.6,0.4,1,1,1h12c0.6,0,1-0.4,1-1V8C21,7' +
          '.4,20.6,7,20,7z"/>'#13#10'    <circle fill="#37474F" cx="14" cy="32.5"' +
          ' r="1.5"/>'#13#10'    <path fill="#546E7A" d="M14,36V13c0-2.2,1.8-4,4-' +
          '4h12c2.2,0,4,1.8,4,4v23c0,2.2-1.8,4-4,4H18C15.8,40,14,38.2,14,36' +
          'z"/>'#13#10'    <path fill="#BBDEFB" d="M30,12H18c-0.6,0-1,0.4-1,1v21c' +
          '0,0.6,0.4,1,1,1h12c0.6,0,1-0.4,1-1V13C31,12.4,30.6,12,30,12z"/>'#13 +
          #10'    <circle fill="#37474F" cx="24" cy="37.5" r="1.5"/>'#13#10'    <pa' +
          'th fill="#E38939" d="M24,40V18c0-2.2,1.8-4,4-4h12c2.2,0,4,1.8,4,' +
          '4v22c0,2.2-1.8,4-4,4H28C25.8,44,24,42.2,24,40z"/>'#13#10'    <path fil' +
          'l="#FFF3E0" d="M40,17H28c-0.6,0-1,0.4-1,1v20c0,0.6,0.4,1,1,1h12c' +
          '0.6,0,1-0.4,1-1V18C41,17.4,40.6,17,40,17z"/>'#13#10'    <circle fill="' +
          '#A6642A" cx="34" cy="41.5" r="1.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'music'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#E91E63' +
          '">'#13#10'        <circle cx="19" cy="33" r="9"/>'#13#10'        <polygon po' +
          'ints="24,6 24,33 28,33 28,14 39,17 39,10"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'neutral_decision'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FFCC80' +
          '">'#13#10'        <circle cx="38" cy="26" r="4"/>'#13#10'        <circle cx=' +
          '"10" cy="26" r="4"/>'#13#10'        <path d="M39,19c0-12.7-30-8.3-30,0' +
          'c0,1.8,0,8.2,0,10c0,8.3,6.7,15,15,15s15-6.7,15-15C39,27.2,39,20.' +
          '8,39,19z"/>'#13#10'        <path d="M24,4C15.2,4,8,11.2,8,20c0,1.2,0,3' +
          '.5,0,3.5l2.1,0.6V19l19.5-6.3l8.2,6.3v5.1l2.1-0.6c0,0,0-2.3,0-3.5' +
          ' C40,12.5,34.6,4,24,4z"/>'#13#10'    </g>'#13#10'    <g fill="#37474F">'#13#10'   ' +
          '     <circle cx="24" cy="25" r="2"/>'#13#10'        <circle cx="32" cy' +
          '="25" r="2"/>'#13#10'        <circle cx="16" cy="25" r="2"/>'#13#10'    </g>' +
          #13#10'</svg>'#13#10
      end
      item
        IconName = 'neutral_trading'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#1565C0' +
          '">'#13#10'        <polygon points="43.4,13 35,20 35,6"/>'#13#10'        <rec' +
          't x="4" y="11" width="34" height="4"/>'#13#10'    </g>'#13#10'    <g fill="#' +
          '2196F3">'#13#10'        <rect x="40" y="23" width="4" height="19"/>'#13#10' ' +
          '       <rect x="34" y="23" width="4" height="19"/>'#13#10'        <rec' +
          't x="28" y="23" width="4" height="19"/>'#13#10'        <rect x="22" y=' +
          '"23" width="4" height="19"/>'#13#10'        <rect x="16" y="23" width=' +
          '"4" height="19"/>'#13#10'        <rect x="10" y="23" width="4" height=' +
          '"19"/>'#13#10'        <rect x="4" y="23" width="4" height="19"/>'#13#10'    ' +
          '</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'news'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FF5' +
          '722" d="M32,15v28H10c-2.2,0-4-1.8-4-4V15H32z"/>'#13#10'    <path fill=' +
          '"#FFCCBC" d="M14,5v34c0,2.2-1.8,4-4,4h29c2.2,0,4-1.8,4-4V5H14z"/' +
          '>'#13#10'    <g fill="#FF5722">'#13#10'        <rect x="20" y="10" width="18' +
          '" height="4"/>'#13#10'        <rect x="20" y="17" width="8" height="2"' +
          '/>'#13#10'        <rect x="30" y="17" width="8" height="2"/>'#13#10'        ' +
          '<rect x="20" y="21" width="8" height="2"/>'#13#10'        <rect x="30"' +
          ' y="21" width="8" height="2"/>'#13#10'        <rect x="20" y="25" widt' +
          'h="8" height="2"/>'#13#10'        <rect x="30" y="25" width="8" height' +
          '="2"/>'#13#10'        <rect x="20" y="29" width="8" height="2"/>'#13#10'    ' +
          '    <rect x="30" y="29" width="8" height="2"/>'#13#10'        <rect x=' +
          '"20" y="33" width="8" height="2"/>'#13#10'        <rect x="30" y="33" ' +
          'width="8" height="2"/>'#13#10'        <rect x="20" y="37" width="8" he' +
          'ight="2"/>'#13#10'        <rect x="30" y="37" width="8" height="2"/>'#13#10 +
          '    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'next'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '2196F3" points="17.1,5 14,8.1 29.9,24 14,39.9 17.1,43 36,24"/>'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = 'nfc_sign'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 19.1.1, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<svg version="1.1"  xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"'#13#10#9' vie' +
          'wBox="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pr' +
          'eserve">'#13#10'<path fill="#2196F3" d="M37,42c-0.3,0-0.7-0.1-1-0.3c-1' +
          '-0.5-1.3-1.8-0.8-2.7c0-0.1,3.7-6.8,3.7-15S35.3,9,35.3,9'#13#10#9'c-0.5-' +
          '1-0.2-2.2,0.8-2.7c1-0.5,2.2-0.2,2.7,0.8c0.2,0.3,4.3,7.6,4.3,17s-' +
          '4.1,16.7-4.3,17C38.4,41.6,37.7,42,37,42z M32.8,35.8'#13#10#9'c0.1-0.2,2' +
          '.2-5,2.2-11.8c0-6.8-2.1-11.6-2.2-11.8c-0.4-1-1.6-1.5-2.6-1c-1,0.' +
          '4-1.5,1.6-1,2.6c0,0,1.8,4.3,1.8,10.2'#13#10#9'c0,5.9-1.8,10.2-1.8,10.2c' +
          '-0.4,1,0,2.2,1,2.6c0.3,0.1,0.5,0.2,0.8,0.2C31.8,37,32.5,36.6,32.' +
          '8,35.8z M23.3,33c0.6-0.1,1.1-0.5,1.4-1'#13#10#9'c0.1-0.2,2.3-3.9,2.3-8c' +
          '0-4.1-2.2-7.9-2.3-8c-0.6-1-1.8-1.3-2.7-0.7c-1,0.6-1.3,1.8-0.7,2.' +
          '7c0,0,1.7,3,1.7,6c0,1.3-0.3,2.7-0.7,3.7'#13#10#9'l-13-11.2c-0.5-0.4-1.2' +
          '-0.6-1.8-0.4c-0.6,0.2-1.2,0.6-1.4,1.3C6.1,17.5,5,20.5,5,24c0,3.5' +
          ',1.1,6.5,1.1,6.7c0.4,1,1.5,1.6,2.6,1.2'#13#10#9'c1-0.4,1.6-1.5,1.2-2.6c' +
          '0,0-0.9-2.6-0.9-5.3c0-0.8,0.1-1.6,0.2-2.3l12.5,10.8c0.4,0.3,0.8,' +
          '0.5,1.3,0.5C23.1,33,23.2,33,23.3,33z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'night_landscape'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '673AB7" points="16.5,18 0,42 33,42"/>'#13#10'    <polygon fill="#9575C' +
          'D" points="33.6,24 19.2,42 48,42"/>'#13#10'    <path fill="#40C4FF" d=' +
          '"M42.9,6.3C43.6,7.4,44,8.6,44,10c0,3.9-3.1,7-7,7c-0.7,0-1.3-0.1-' +
          '1.9-0.3c1.2,2,3.4,3.3,5.9,3.3 c3.9,0,7-3.1,7-7C48,9.8,45.9,7.1,4' +
          '2.9,6.3z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'night_portrait'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#40C' +
          '4FF" d="M42.9,6.3C43.6,7.4,44,8.6,44,10c0,3.9-3.1,7-7,7c-0.7,0-1' +
          '.3-0.1-1.9-0.3c1.2,2,3.4,3.3,5.9,3.3 c3.9,0,7-3.1,7-7C48,9.8,45.' +
          '9,7.1,42.9,6.3z"/>'#13#10'    <g fill="#B39DDB">'#13#10'        <circle cx="' +
          '31" cy="19" r="2"/>'#13#10'        <circle cx="13" cy="19" r="2"/>'#13#10'  ' +
          '      <polygon points="22,37 17,31 17,25 27,25 27,31"/>'#13#10'    </g' +
          '>'#13#10'    <path fill="#D1C4E9" d="M31,13c0-7.6-18-5-18,0c0,1.1,0,5.' +
          '9,0,7c0,5,4,9,9,9s9-4,9-9C31,18.9,31,14.1,31,13z"/>'#13#10'    <g fill' +
          '="#673AB7">'#13#10'        <circle cx="26" cy="19" r="1"/>'#13#10'        <c' +
          'ircle cx="18" cy="19" r="1"/>'#13#10'        <path d="M22,4c-6.1,0-10,' +
          '4.9-10,11c0,0.8,0,2.3,0,2.3l2,1.7v-5l12-4l4,4v5l2-1.7c0,0,0-1.5,' +
          '0-2.3c0-4-1-8-6-9l-1-2 H22z"/>'#13#10'        <path d="M27,31L27,31c0,' +
          '0-2,1-5,1s-5-1-5-1S6,33,6,44h32C38,33,27,31,27,31z"/>'#13#10'    </g>'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'no_idea'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FBC' +
          '02D" d="M37,22c0-7.7-6.6-13.8-14.5-12.9c-6,0.7-10.8,5.5-11.4,11.' +
          '5c-0.5,4.6,1.4,8.7,4.6,11.3 c1.4,1.2,2.3,2.9,2.3,4.8V37h12v-0.1c' +
          '0-1.8,0.8-3.6,2.2-4.8C35.1,29.7,37,26.1,37,22z"/>'#13#10'    <path fil' +
          'l="#FFF59D" d="M30.6,20.2l-3-2c-0.3-0.2-0.8-0.2-1.1,0L24,19.8l-2' +
          '.4-1.6c-0.3-0.2-0.8-0.2-1.1,0l-3,2 c-0.2,0.2-0.4,0.4-0.4,0.7s0,0' +
          '.6,0.2,0.8l3.8,4.7V37h2V26c0-0.2-0.1-0.4-0.2-0.6l-3.3-4.1l1.5-1l' +
          '2.4,1.6c0.3,0.2,0.8,0.2,1.1,0 l2.4-1.6l1.5,1l-3.3,4.1C25.1,25.6,' +
          '25,25.8,25,26v11h2V26.4l3.8-4.7c0.2-0.2,0.3-0.5,0.2-0.8S30.8,20.' +
          '3,30.6,20.2z"/>'#13#10'    <circle fill="#5C6BC0" cx="24" cy="44" r="3' +
          '"/>'#13#10'    <path fill="#9FA8DA" d="M26,45h-4c-2.2,0-4-1.8-4-4v-5h1' +
          '2v5C30,43.2,28.2,45,26,45z"/>'#13#10'    <g fill="#5C6BC0">'#13#10'        <' +
          'path d="M30,41l-11.6,1.6c0.3,0.7,0.9,1.4,1.6,1.8l9.4-1.3C29.8,42' +
          '.5,30,41.8,30,41z"/>'#13#10'        <polygon points="18,38.7 18,40.7 3' +
          '0,39 30,37"/>'#13#10'    </g>'#13#10'    <rect x="22" y="-2.9" transform="ma' +
          'trix(.707 -.707 .707 .707 -9.941 24)" fill="#37474F" width="4" h' +
          'eight="53.7"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'no_video'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#4CA' +
          'F50" d="M8,12h22c2.2,0,4,1.8,4,4v16c0,2.2-1.8,4-4,4H8c-2.2,0-4-1' +
          '.8-4-4V16C4,13.8,5.8,12,8,12z"/>'#13#10'    <polygon fill="#388E3C" po' +
          'ints="44,35 34,29 34,19 44,13"/>'#13#10'    <line fill="none" stroke="' +
          '#212121" stroke-width="4" stroke-linejoin="round" stroke-miterli' +
          'mit="10" x1="5" y1="5" x2="43" y2="43"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'nook'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#90A' +
          '4AE" d="M8,39V9c0-3.3,2.7-6,6-6h20c3.3,0,6,2.7,6,6v30c0,3.3-2.7,' +
          '6-6,6H14C10.7,45,8,42.3,8,39z"/>'#13#10'    <path fill="#ECEFF1" d="M3' +
          '4,7H14c-1.1,0-2,0.9-2,2v26c0,1.1,0.9,2,2,2h20c1.1,0,2-0.9,2-2V9C' +
          '36,7.9,35.1,7,34,7z"/>'#13#10'    <g fill="#B0BEC5">'#13#10'        <rect x=' +
          '"16" y="12" width="16" height="3"/>'#13#10'        <rect x="16" y="19"' +
          ' width="16" height="2"/>'#13#10'        <rect x="16" y="23" width="12"' +
          ' height="2"/>'#13#10'        <rect x="16" y="27" width="16" height="2"' +
          '/>'#13#10'        <rect x="16" y="31" width="12" height="2"/>'#13#10'    </g' +
          '>'#13#10'    <path fill="none" stroke="#eee" stroke-width="2" stroke-m' +
          'iterlimit="10" d="M22,43v-1c0-1.1,0.9-2,2-2h0c1.1,0,2,0.9,2,2v1"' +
          '/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'numerical_sorting_12'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '546E7A" points="38,33 38,5 34,5 34,33 28,33 36,43 44,33"/>'#13#10'    ' +
          '<g fill="#2196F3">'#13#10'        <path d="M16.4,20h-3V8.6L9.9,9.7V7.3' +
          'L16,5.1h0.3V20z"/>'#13#10'        <path d="M19.4,43H9.2v-2l4.8-5.1c0.4' +
          '-0.4,0.7-0.8,0.9-1.1c0.2-0.3,0.5-0.6,0.6-0.9c0.2-0.3,0.3-0.5,0.3' +
          '-0.8 c0.1-0.2,0.1-0.5,0.1-0.7c0-0.7-0.2-1.2-0.5-1.6c-0.3-0.4-0.8' +
          '-0.6-1.4-0.6c-0.3,0-0.7,0.1-0.9,0.2c-0.3,0.1-0.5,0.3-0.7,0.5 c-0' +
          '.2,0.2-0.3,0.5-0.4,0.8s-0.1,0.6-0.1,1h-3c0-0.7,0.1-1.3,0.4-1.9c0' +
          '.2-0.6,0.6-1.1,1-1.6c0.5-0.4,1-0.8,1.6-1.1 c0.6-0.3,1.4-0.4,2.2-' +
          '0.4c0.8,0,1.5,0.1,2.1,0.3c0.6,0.2,1.1,0.5,1.5,0.8s0.7,0.8,0.9,1.' +
          '3s0.3,1.1,0.3,1.8c0,0.5-0.1,1-0.2,1.4 S18.3,34.5,18,35s-0.6,0.9-' +
          '1,1.4c-0.4,0.5-0.9,1-1.4,1.5L13,40.6h6.4V43z"/>'#13#10'    </g>'#13#10'</svg' +
          '>'#13#10
      end
      item
        IconName = 'numerical_sorting_21'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '546E7A" points="38,33 38,5 34,5 34,33 28,33 36,43 44,33"/>'#13#10'    ' +
          '<g fill="#2196F3">'#13#10'        <path d="M19.2,20H9v-2l4.8-5.1c0.4-0' +
          '.4,0.7-0.8,0.9-1.1c0.2-0.3,0.5-0.6,0.6-0.9c0.2-0.3,0.3-0.5,0.3-0' +
          '.8 c0.1-0.2,0.1-0.5,0.1-0.7c0-0.7-0.2-1.2-0.5-1.6c-0.3-0.4-0.8-0' +
          '.6-1.4-0.6c-0.3,0-0.7,0.1-0.9,0.2c-0.3,0.1-0.5,0.3-0.7,0.5 c-0.2' +
          ',0.2-0.3,0.5-0.4,0.8s-0.1,0.6-0.1,1h-3c0-0.7,0.1-1.3,0.4-1.9c0.2' +
          '-0.6,0.6-1.1,1-1.6c0.5-0.4,1-0.8,1.6-1.1 c0.6-0.3,1.4-0.4,2.2-0.' +
          '4c0.8,0,1.5,0.1,2.1,0.3c0.6,0.2,1.1,0.5,1.5,0.8s0.7,0.8,0.9,1.3c' +
          '0.2,0.5,0.3,1.1,0.3,1.8 c0,0.5-0.1,1-0.2,1.4s-0.4,0.9-0.7,1.4s-0' +
          '.6,0.9-1,1.4c-0.4,0.5-0.9,1-1.4,1.5l-2.6,2.8h6.4V20z"/>'#13#10'       ' +
          ' <path d="M16.2,43h-3V31.6l-3.5,1.1v-2.4l6.2-2.2h0.3V43z"/>'#13#10'   ' +
          ' </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ok'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#4' +
          'CAF50" cx="24" cy="24" r="21"/>'#13#10'    <polygon fill="#CCFF90" poi' +
          'nts="34.6,14.6 21,28.2 15.4,22.6 12.6,25.4 21,33.8 37.4,17.4"/>'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'old_time_camera'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#546' +
          'E7A" d="M14,13H8v-1.8C8,10.5,8.5,10,9.2,10h3.6c0.7,0,1.2,0.5,1.2' +
          ',1.2V13z"/>'#13#10'    <path fill="#5E35B1" d="M40,40H8c-2.2,0-4-1.8-4' +
          '-4V22h40v14C44,38.2,42.2,40,40,40z"/>'#13#10'    <path fill="#42257A" ' +
          'd="M12.7,22c-0.4,1.3-0.7,2.6-0.7,4c0,6.6,5.4,12,12,12s12-5.4,12-' +
          '12c0-1.4-0.3-2.7-0.7-4H12.7z"/>'#13#10'    <path fill="#78909C" d="M8,' +
          '12h32c2.2,0,4,1.8,4,4v6H4v-6C4,13.8,5.8,12,8,12z"/>'#13#10'    <path f' +
          'ill="#78909C" d="M33.9,13.1H14.2L17.6,8c0.4-0.6,1-0.9,1.7-0.9h9.' +
          '6c0.7,0,1.3,0.3,1.7,0.9L33.9,13.1z"/>'#13#10'    <path fill="#455A64" ' +
          'd="M35.3,22c-1.6-4.7-6.1-8-11.3-8s-9.7,3.3-11.3,8H35.3z"/>'#13#10'    ' +
          '<circle fill="#B388FF" cx="24" cy="26" r="9"/>'#13#10'    <path fill="' +
          '#C7A7FF" d="M29,23c-1.2-1.4-3-2.2-4.8-2.2c-1.8,0-3.6,0.8-4.8,2.2' +
          'c-0.5,0.5-0.4,1.3,0.1,1.8c0.5,0.5,1.3,0.4,1.8-0.1 c1.5-1.7,4.3-1' +
          '.7,5.8,0c0.3,0.3,0.6,0.4,1,0.4c0.3,0,0.6-0.1,0.9-0.3C29.4,24.4,2' +
          '9.5,23.5,29,23z"/>'#13#10'    <rect x="36" y="15" fill="#DBE2E5" width' +
          '="5" height="4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'online_support'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="13" y="' +
          '30" fill="#BF360C" width="22" height="12"/>'#13#10'    <g fill="#FFA72' +
          '6">'#13#10'        <circle cx="10" cy="26" r="4"/>'#13#10'        <circle cx' +
          '="38" cy="26" r="4"/>'#13#10'    </g>'#13#10'    <path fill="#FFB74D" d="M39' +
          ',19c0-12.7-30-8.3-30,0c0,1.8,0,8.2,0,10c0,8.3,6.7,15,15,15s15-6.' +
          '7,15-15C39,27.2,39,20.8,39,19z"/>'#13#10'    <g fill="#784719">'#13#10'     ' +
          '   <circle cx="30" cy="26" r="2"/>'#13#10'        <circle cx="18" cy="' +
          '26" r="2"/>'#13#10'    </g>'#13#10'    <path fill="#FF5722" d="M24,2C15.5,2,' +
          '3,7.8,3,35.6L13,42V24l16.8-9.8L35,21v21l10-8.2c0-5.6-0.9-29-15.4' +
          '-29L28.2,2H24z"/>'#13#10'    <path fill="#757575" d="M45,24c-0.6,0-1,0' +
          '.4-1,1v-7c0-8.8-7.2-16-16-16h-9c-0.6,0-1,0.4-1,1s0.4,1,1,1h9c7.7' +
          ',0,14,6.3,14,14v10 c0,0.6,0.4,1,1,1s1-0.4,1-1v2c0,3.9-3.1,7-7,7H' +
          '24c-0.6,0-1,0.4-1,1s0.4,1,1,1h13c5,0,9-4,9-9v-5C46,24.4,45.6,24,' +
          '45,24z"/>'#13#10'    <g fill="#37474F">'#13#10'        <path d="M45,22h-1c-1' +
          '.1,0-2,0.9-2,2v4c0,1.1,0.9,2,2,2h1c1.1,0,2-0.9,2-2v-4C47,22.9,46' +
          '.1,22,45,22z"/>'#13#10'        <circle cx="24" cy="38" r="2"/>'#13#10'    </' +
          'g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'opened_folder'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FFA' +
          '000" d="M38,12H22l-4-4H8c-2.2,0-4,1.8-4,4v24c0,2.2,1.8,4,4,4h31c' +
          '1.7,0,3-1.3,3-3V16C42,13.8,40.2,12,38,12z"/>'#13#10'    <path fill="#F' +
          'FCA28" d="M42.2,18H15.3c-1.9,0-3.6,1.4-3.9,3.3L8,40h31.7c1.9,0,3' +
          '.6-1.4,3.9-3.3l2.5-14C46.6,20.3,44.7,18,42.2,18z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'org_unit'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#90C' +
          'AF9" d="M10,10v28h28V10H10z M34,34H14V14h20V34z"/>'#13#10'    <rect x=' +
          '"6" y="6" fill="#D81B60" width="12" height="12"/>'#13#10'    <g fill="' +
          '#2196F3">'#13#10'        <rect x="30" y="6" width="12" height="12"/>'#13#10 +
          '        <rect x="6" y="30" width="12" height="12"/>'#13#10'        <re' +
          'ct x="30" y="30" width="12" height="12"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'organization'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#90C' +
          'AF9" d="M42,42H6V10c0-2.2,1.8-4,4-4h28c2.2,0,4,1.8,4,4V42z"/>'#13#10' ' +
          '   <rect x="6" y="42" fill="#64B5F6" width="36" height="2"/>'#13#10'  ' +
          '  <g fill="#1565C0">'#13#10'        <rect x="31" y="27" width="6" heig' +
          'ht="5"/>'#13#10'        <rect x="21" y="27" width="6" height="5"/>'#13#10'  ' +
          '      <rect x="11" y="27" width="6" height="5"/>'#13#10'        <rect ' +
          'x="31" y="35" width="6" height="5"/>'#13#10'        <rect x="11" y="35' +
          '" width="6" height="5"/>'#13#10'        <rect x="31" y="19" width="6" ' +
          'height="5"/>'#13#10'        <rect x="21" y="19" width="6" height="5"/>' +
          #13#10'        <rect x="11" y="19" width="6" height="5"/>'#13#10'        <r' +
          'ect x="31" y="11" width="6" height="5"/>'#13#10'        <rect x="21" y' +
          '="11" width="6" height="5"/>'#13#10'        <rect x="11" y="11" width=' +
          '"6" height="5"/>'#13#10'        <rect x="21" y="35" width="6" height="' +
          '9"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'package'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FF9' +
          '800" d="M38,42H10c-2.2,0-4-1.8-4-4V10c0-2.2,1.8-4,4-4h28c2.2,0,4' +
          ',1.8,4,4v28C42,40.2,40.2,42,38,42z"/>'#13#10'    <path fill="#8A5100" ' +
          'd="M29.5,16h-11c-0.8,0-1.5-0.7-1.5-1.5v0c0-0.8,0.7-1.5,1.5-1.5h1' +
          '1c0.8,0,1.5,0.7,1.5,1.5v0 C31,15.3,30.3,16,29.5,16z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'paid'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#2E7' +
          'D32" d="M25.4,5.6c-0.8-0.8-2-0.8-2.8,0l-12,12c-0.8,0.8-0.8,2,0,2' +
          '.8C11,20.8,11.5,21,12,21s1-0.2,1.4-0.6l12-12 C26.2,7.6,26.2,6.4,' +
          '25.4,5.6z"/>'#13#10'    <path fill="#1B5E20" d="M37.4,17.6l-12-12c-0.8' +
          '-0.8-2-0.8-2.8,0c-0.8,0.8-0.8,2,0,2.8l12,12C35,20.8,35.5,21,36,2' +
          '1s1-0.2,1.4-0.6 C38.2,19.6,38.2,18.4,37.4,17.6z"/>'#13#10'    <path fi' +
          'll="#388E3C" d="M37.4,41H10.6c-1,0-1.8-0.7-2-1.6L5,21h38l-3.7,18' +
          '.4C39.1,40.3,38.3,41,37.4,41z"/>'#13#10'    <path fill="#4CAF50" d="M4' +
          '3,23H5c-1.1,0-2-0.9-2-2v-2c0-1.1,0.9-2,2-2h38c1.1,0,2,0.9,2,2v2C' +
          '45,22.1,44.1,23,43,23z"/>'#13#10'    <polygon fill="#DCEDC8" points="3' +
          '0.8,24.8 22.9,32.7 19.2,28.9 17,31.1 22.9,37 33,26.9"/>'#13#10'</svg>'#13 +
          #10
      end
      item
        IconName = 'panorama'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#F57' +
          'C00" d="M4,9v32c0,0,8.4-3,20-3s20,3,20,3V9c0,0-6.7,3-20,3S4,9,4,' +
          '9z"/>'#13#10'    <path fill="#942A09" d="M24,34c0.1,0,0.3,0,0.4,0L15,1' +
          '9L6.9,36.2C10.3,35.3,16.5,34,24,34z"/>'#13#10'    <path fill="#BF360C"' +
          ' d="M24,34c3.3,0,6.3,0.2,9,0.6l-8-11.8l-7.8,11.5C19.3,34.1,21.6,' +
          '34,24,34z"/>'#13#10'    <path fill="#E65100" d="M40.7,36L35,26.5l-5,7.' +
          '8C34.5,34.7,38.2,35.4,40.7,36z"/>'#13#10'    <ellipse fill="#FFF9C4" c' +
          'x="36" cy="19.5" rx="2" ry="2.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'parallel_tasks'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '90CAF9" points="36,13 36,9 22,9 22,22 13,22 13,26 22,26 22,39 36' +
          ',39 36,35 26,35 26,26 36,26 36,22 26,22 26,13"/>'#13#10'    <rect x="6' +
          '" y="17" fill="#D81B60" width="10" height="14"/>'#13#10'    <rect x="3' +
          '2" y="6" fill="#2196F3" width="10" height="10"/>'#13#10'    <rect x="3' +
          '2" y="32" fill="#2196F3" width="10" height="10"/>'#13#10'    <rect x="' +
          '32" y="19" fill="#2196F3" width="10" height="10"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'phone'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#009' +
          '688" d="M39.1,7l-3.7,0C22.2,7.2,7.1,24.1,7,35.4l0,3.7c0,1,0.8,1.' +
          '9,1.9,1.9l7.5-0.1c1,0,1.9-0.9,1.9-1.9l0.2-8.2 l-4.7-4c0-2.6,10.5' +
          '-13.1,13.2-13.2l4.3,4.7l7.9-0.2c1,0,1.9-0.9,1.9-1.9L41,8.9C41,7.' +
          '8,40.2,7,39.1,7z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'phone_android'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#374' +
          '74F" d="M12,40V8c0-2.2,1.8-4,4-4h16c2.2,0,4,1.8,4,4v32c0,2.2-1.8' +
          ',4-4,4H16C13.8,44,12,42.2,12,40z"/>'#13#10'    <path fill="#BBDEFB" d=' +
          '"M32,7H16c-0.6,0-1,0.4-1,1v29c0,0.6,0.4,1,1,1h16c0.6,0,1-0.4,1-1' +
          'V8C33,7.4,32.6,7,32,7z"/>'#13#10'    <rect x="21" y="40" fill="#78909C' +
          '" width="6" height="2"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'photo_reel'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#673' +
          'AB7" d="M10,9c-2.2,0-4,1.8-4,4v26c0,2.2,1.8,4,4,4h16c2.2,0,4-1.8' +
          ',4-4V13c0-2.2-1.8-4-4-4"/>'#13#10'    <g fill="#311B92">'#13#10'        <rec' +
          't x="14" y="13" width="2" height="26"/>'#13#10'        <path d="M24,9V' +
          '7c0-1.2-0.8-2-2-2h-8c-1.2,0-2,0.8-2,2v2H24z"/>'#13#10'    </g>'#13#10'    <p' +
          'ath fill="#D84315" d="M30,13H16v26h14V13z M21,37h-3v-4h3V37z M21' +
          ',19h-3v-4h3V19z M27,37h-3v-4h3V37z M24,19v-4h3v4H24z"/>'#13#10'    <pa' +
          'th fill="#FF5722" d="M30,13v2h3v4h-3v14h3v4h-3v2h12V13H30z M39,3' +
          '7h-3v-4h3V37z M39,19h-3v-4h3V19z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'picture'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#F57' +
          'C00" d="M40,41H8c-2.2,0-4-1.8-4-4V11c0-2.2,1.8-4,4-4h32c2.2,0,4,' +
          '1.8,4,4v26C44,39.2,42.2,41,40,41z"/>'#13#10'    <circle fill="#FFF9C4"' +
          ' cx="35" cy="16" r="3"/>'#13#10'    <polygon fill="#942A09" points="20' +
          ',16 9,32 31,32"/>'#13#10'    <polygon fill="#BF360C" points="31,22 23,' +
          '32 39,32"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'pie_chart'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#00B' +
          'CD4" d="M24,6C14.1,6,6,14.1,6,24s8.1,18,18,18c5.2,0,9.9-2.2,13.1' +
          '-5.7L24,24V6z"/>'#13#10'    <path fill="#448AFF" d="M42,24c0-9.9-8.1-1' +
          '8-18-18v18H42z"/>'#13#10'    <path fill="#3F51B5" d="M24,24l13.1,12.3c' +
          '3-3.2,4.9-7.5,4.9-12.3H24z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'planner'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#CFD' +
          '8DC" d="M5,38V14h38v24c0,2.2-1.8,4-4,4H9C6.8,42,5,40.2,5,38z"/>'#13 +
          #10'    <path fill="#F44336" d="M43,10v6H5v-6c0-2.2,1.8-4,4-4h30C41' +
          '.2,6,43,7.8,43,10z"/>'#13#10'    <g fill="#B71C1C">'#13#10'        <circle c' +
          'x="33" cy="10" r="3"/>'#13#10'        <circle cx="15" cy="10" r="3"/>'#13 +
          #10'    </g>'#13#10'    <g fill="#B0BEC5">'#13#10'        <path d="M33,3c-1.1,0' +
          '-2,0.9-2,2v5c0,1.1,0.9,2,2,2s2-0.9,2-2V5C35,3.9,34.1,3,33,3z"/>'#13 +
          #10'        <path d="M15,3c-1.1,0-2,0.9-2,2v5c0,1.1,0.9,2,2,2s2-0.9' +
          ',2-2V5C17,3.9,16.1,3,15,3z"/>'#13#10'    </g>'#13#10'    <g fill="#B0BEC5">'#13 +
          #10'        <rect x="13" y="21" width="6" height="6"/>'#13#10'        <re' +
          'ct x="21" y="21" width="6" height="6"/>'#13#10'        <rect x="29" y=' +
          '"21" width="6" height="6"/>'#13#10'        <rect x="13" y="29" width="' +
          '6" height="6"/>'#13#10'        <rect x="21" y="29" width="6" height="6' +
          '"/>'#13#10'    </g>'#13#10'    <rect x="29" y="29" fill="#F44336" width="6" ' +
          'height="6"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'plus'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#4' +
          'CAF50" cx="24" cy="24" r="21"/>'#13#10'    <g fill="#fff">'#13#10'        <r' +
          'ect x="21" y="14" width="6" height="20"/>'#13#10'        <rect x="14" ' +
          'y="21" width="20" height="6"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'podium_with_audience'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'B0BEC5" points="41,12 7,12 6,16 11,19 9,16 39,16 37,19 42,16"/>'#13 +
          #10'    <polygon fill="#78909C" points="9,16 39,16 35,28 13,28"/>'#13#10 +
          '    <circle fill="#FFB74D" cx="24" cy="28" r="4"/>'#13#10'    <circle ' +
          'fill="#FFB74D" cx="36" cy="28" r="4"/>'#13#10'    <circle fill="#FFB74' +
          'D" cx="12" cy="28" r="4"/>'#13#10'    <circle fill="#FFB74D" cx="18" c' +
          'y="37" r="5"/>'#13#10'    <circle fill="#FFB74D" cx="30" cy="37" r="5"' +
          '/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'podium_with_speaker'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#F' +
          'FB74D" cx="24" cy="11" r="6"/>'#13#10'    <path fill="#607D8B" d="M36,' +
          '26.1c0,0-3.3-7.1-12-7.1s-12,7.1-12,7.1V30h24V26.1z"/>'#13#10'    <poly' +
          'gon fill="#B0BEC5" points="41,25 7,25 6,29 11,32 9,29 39,29 37,3' +
          '2 42,29"/>'#13#10'    <polygon fill="#78909C" points="9,29 39,29 35,41' +
          ' 13,41"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'podium_without_speaker'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'B0BEC5" points="43,16 5,16 4,20 9,23 7,20 41,20 39,23 44,20"/>'#13#10 +
          '    <polygon fill="#78909C" points="7,20 41,20 37,36 11,36"/>'#13#10'<' +
          '/svg>'#13#10
      end
      item
        IconName = 'portrait_mode'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FF9' +
          '800" d="M22,38c-4.8,0-5-7-5-7v-6h10v6C27,31,26.8,38,22,38z"/>'#13#10' ' +
          '   <g fill="#FFA726">'#13#10'        <circle cx="31" cy="19" r="2"/>'#13#10 +
          '        <circle cx="13" cy="19" r="2"/>'#13#10'    </g>'#13#10'    <path fil' +
          'l="#FFB74D" d="M31,13c0-7.6-18-5-18,0c0,1.1,0,5.9,0,7c0,5,4,9,9,' +
          '9s9-4,9-9C31,18.9,31,14.1,31,13z"/>'#13#10'    <path fill="#424242" d=' +
          '"M22,4c-6.1,0-10,4.9-10,11c0,0.8,0,2.3,0,2.3l2,1.7v-5l12-4l4,4v5' +
          'l2-1.7c0,0,0-1.5,0-2.3c0-4-1-8-6-9l-1-2 H22z"/>'#13#10'    <g fill="#7' +
          '84719">'#13#10'        <circle cx="26" cy="19" r="1"/>'#13#10'        <circl' +
          'e cx="18" cy="19" r="1"/>'#13#10'    </g>'#13#10'    <path fill="#009688" d=' +
          '"M27,31L27,31c0,0-1.8,2-5,2s-5-2-5-2S6,33,6,44h32C38,33,27,31,27' +
          ',31z"/>'#13#10'    <g fill="#FF9800">'#13#10'        <rect x="36.1" y="6.1" ' +
          'transform="matrix(.707 .707 -.707 .707 19.787 -25.77)" width="9.' +
          '9" height="9.9"/>'#13#10'        <rect x="36" y="6" width="10" height=' +
          '"10"/>'#13#10'    </g>'#13#10'    <circle fill="#FFEB3B" cx="41" cy="11" r="' +
          '3"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'previous'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '2196F3" points="30.9,43 34,39.9 18.1,24 34,8.1 30.9,5 12,24"/>'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = 'print'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="9" y="1' +
          '1" fill="#424242" width="30" height="3"/>'#13#10'    <path fill="#6161' +
          '61" d="M4,25h40v-7c0-2.2-1.8-4-4-4H8c-2.2,0-4,1.8-4,4V25z"/>'#13#10'  ' +
          '  <path fill="#424242" d="M8,36h32c2.2,0,4-1.8,4-4v-8H4v8C4,34.2' +
          ',5.8,36,8,36z"/>'#13#10'    <circle fill="#00E676" cx="40" cy="18" r="' +
          '1"/>'#13#10'    <rect x="11" y="4" fill="#90CAF9" width="26" height="1' +
          '0"/>'#13#10'    <path fill="#242424" d="M37.5,31h-27C9.7,31,9,30.3,9,2' +
          '9.5v0c0-0.8,0.7-1.5,1.5-1.5h27c0.8,0,1.5,0.7,1.5,1.5v0 C39,30.3,' +
          '38.3,31,37.5,31z"/>'#13#10'    <rect x="11" y="31" fill="#90CAF9" widt' +
          'h="26" height="11"/>'#13#10'    <rect x="11" y="29" fill="#42A5F5" wid' +
          'th="26" height="2"/>'#13#10'    <g fill="#1976D2">'#13#10'        <rect x="1' +
          '6" y="33" width="17" height="2"/>'#13#10'        <rect x="16" y="37" w' +
          'idth="13" height="2"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'privacy'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#424' +
          '242" d="M24,4c-5.5,0-10,4.5-10,10v4h4v-4c0-3.3,2.7-6,6-6s6,2.7,6' +
          ',6v4h4v-4C34,8.5,29.5,4,24,4z"/>'#13#10'    <path fill="#FB8C00" d="M3' +
          '6,44H12c-2.2,0-4-1.8-4-4V22c0-2.2,1.8-4,4-4h24c2.2,0,4,1.8,4,4v1' +
          '8C40,42.2,38.2,44,36,44z"/>'#13#10'    <circle fill="#EFEBE9" cx="24" ' +
          'cy="31" r="6"/>'#13#10'    <circle fill="#1E88E5" cx="24" cy="31" r="3' +
          '"/>'#13#10'    <circle fill="#fff" cx="26" cy="29" r="1"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'process'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#9C27B0' +
          '">'#13#10'        <polygon points="31,8 42.9,9.6 33.1,19.4"/>'#13#10'       ' +
          ' <polygon points="17,40 5.1,38.4 14.9,28.6"/>'#13#10'        <polygon ' +
          'points="8,17 9.6,5.1 19.4,14.9"/>'#13#10'        <path d="M9.3,21.2L5.' +
          '1,22C5,22.7,5,23.3,5,24c0,4.6,1.6,9,4.6,12.4l3-2.6C10.3,31.1,9,2' +
          '7.6,9,24 C9,23.1,9.1,22.1,9.3,21.2z"/>'#13#10'        <path d="M24,5c-' +
          '5.4,0-10.2,2.3-13.7,5.9l2.8,2.8C15.9,10.8,19.7,9,24,9c0.9,0,1.9,' +
          '0.1,2.8,0.3l0.7-3.9 C26.4,5.1,25.2,5,24,5z"/>'#13#10'        <path d="' +
          'M38.7,26.8l4.2-0.8c0.1-0.7,0.1-1.3,0.1-2c0-4.4-1.5-8.7-4.3-12.1l' +
          '-3.1,2.5c2.2,2.7,3.4,6.1,3.4,9.5 C39,24.9,38.9,25.9,38.7,26.8z"/' +
          '>'#13#10'        <path d="M34.9,34.3C32.1,37.2,28.3,39,24,39c-0.9,0-1.' +
          '9-0.1-2.8-0.3l-0.7,3.9c1.2,0.2,2.4,0.3,3.5,0.3 c5.4,0,10.2-2.3,1' +
          '3.7-5.9L34.9,34.3z"/>'#13#10'        <polygon points="40,31 38.4,42.9 ' +
          '28.6,33.1"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'puzzle'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#8BC' +
          '34A" d="M39,15c0-2.2-1.8-4-4-4h-6c-0.7,0-1.1-0.8-0.7-1.4c0.6-1,0' +
          '.9-2.2,0.6-3.5c-0.4-2-1.9-3.6-3.8-4 C21.8,1.4,19,3.9,19,7c0,1,0.' +
          '3,1.8,0.7,2.6c0.4,0.6,0,1.4-0.8,1.4h-6c-2.2,0-4,1.8-4,4v7c0,0.7,' +
          '0.8,1.1,1.4,0.7 c1-0.6,2.2-0.9,3.5-0.6c2,0.4,3.6,1.9,4,3.8c0.7,3' +
          '.2-1.8,6.1-4.9,6.1c-1,0-1.8-0.3-2.6-0.7C9.8,30.9,9,31.3,9,32v6c0' +
          ',2.2,1.8,4,4,4 h22c2.2,0,4-1.8,4-4V15z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'questions'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '42A5F5" points="36,44 8,44 8,8 28,8 36,16"/>'#13#10'    <polygon fill=' +
          '"#90CAF9" points="40,40 12,40 12,4 32,4 40,12"/>'#13#10'    <polygon f' +
          'ill="#E1F5FE" points="38.5,13 31,13 31,5.5"/>'#13#10'    <path fill="#' +
          '1976D2" d="M24.5,28.3c0-4.7,3.6-4.4,3.6-7.2c0-0.7-0.2-2.1-2-2.1c' +
          '-2,0-2.1,1.6-2.1,2h-2.7c0-0.7,0.3-4.2,4.8-4.2 c4.6,0,4.7,3.6,4.7' +
          ',4.3c0,3.5-3.8,4-3.8,7.3H24.5z M24.3,31.8c0-0.2,0-1.5,1.5-1.5c1.' +
          '4,0,1.5,1.3,1.5,1.5c0,0.4-0.2,1.4-1.5,1.4 C24.5,33.2,24.3,32.2,2' +
          '4.3,31.8z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'radar_plot'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#CFD' +
          '8DC" d="M38.4,13L24.1,6.4L4.6,12.1l8.8,13.2l-2.2,15.1h22.7l6.6-1' +
          '3.3L38.4,13z M32.1,37.5H14.7l1.8-12.9L9.4,13.9 l14.5-4.3L35.6,15' +
          'l1.8,11.7L32.1,37.5z"/>'#13#10'    <g fill="#00BCD4">'#13#10'        <circle' +
          ' cx="24" cy="8" r="4"/>'#13#10'        <circle cx="37" cy="14" r="4"/>' +
          #13#10'        <circle cx="39" cy="27" r="4"/>'#13#10'        <circle cx="7' +
          '" cy="13" r="4"/>'#13#10'        <circle cx="13" cy="39" r="4"/>'#13#10'    ' +
          '    <circle cx="15" cy="25" r="4"/>'#13#10'        <circle cx="33" cy=' +
          '"39" r="4"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'rating'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#F' +
          '44336" cx="24" cy="24" r="21"/>'#13#10'    <polygon fill="#FFCA28" poi' +
          'nts="24,11 27.9,18.9 36.6,20.2 30.3,26.3 31.8,35 24,30.9 16.2,35' +
          ' 17.7,26.3 11.4,20.2 20.1,18.9"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ratings'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '42A5F5" points="36,44 8,44 8,8 28,8 36,16"/>'#13#10'    <polygon fill=' +
          '"#90CAF9" points="40,40 12,40 12,4 32,4 40,12"/>'#13#10'    <polygon f' +
          'ill="#E1F5FE" points="38.5,13 31,13 31,5.5"/>'#13#10'    <polygon fill' +
          '="#1976D2" points="34,20 27,20 29.4,22.4 27,24.9 23,20.9 16.9,26' +
          '.9 19.1,29.1 23,25.1 27,29.1 31.6,24.6 34,27"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'reading'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#5C6' +
          'BC0" d="M40,40c-6.9,0-16,4-16,4V22c0,0,9-4,18-4L40,40z"/>'#13#10'    <' +
          'path fill="#7986CB" d="M8,40c6.9,0,16,4,16,4V22c0,0-9-4-18-4L8,4' +
          '0z"/>'#13#10'    <g fill="#FFB74D">'#13#10'        <circle cx="24" cy="12" r' +
          '="8"/>'#13#10'        <path d="M41,32h1c0.6,0,1-0.4,1-1v-4c0-0.6-0.4-1' +
          '-1-1h-1c-1.7,0-3,1.3-3,3v0C38,30.7,39.3,32,41,32z"/>'#13#10'        <p' +
          'ath d="M7,26H6c-0.6,0-1,0.4-1,1v4c0,0.6,0.4,1,1,1h1c1.7,0,3-1.3,' +
          '3-3v0C10,27.3,8.7,26,7,26z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'reading_ebook'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#219' +
          '6F3" d="M33.5,27c-2.2-3-5.2-5-9.5-5s-7.3,2-9.5,5H33.5z"/>'#13#10'    <' +
          'path fill="#546E7A" d="M34.1,43H13.9c-1.1,0-1.9-0.8-2-1.9l-0.8-1' +
          '3C11.1,27,12,26,13.1,26h21.8c1.2,0,2.1,1,2,2.1l-0.8,13 C36,42.2,' +
          '35.2,43,34.1,43z"/>'#13#10'    <circle fill="#B0BEC5" cx="34" cy="29" ' +
          'r="1"/>'#13#10'    <g fill="#FFB74D">'#13#10'        <circle cx="24" cy="12"' +
          ' r="8"/>'#13#10'        <path d="M16.1,42.4L15,43.5c-0.6,0.6-1.6,0.6-2' +
          '.2,0l-3.3-3.3c-0.6-0.6-0.6-1.6,0-2.2l1.1-1.1c1.3-1.3,3.1-1.3,4.4' +
          ',0 l1.1,1.1C17.3,39.3,17.3,41.2,16.1,42.4z"/>'#13#10'        <path d="' +
          'M31.9,38l1.1-1.1c1.3-1.3,3.1-1.3,4.4,0l1.1,1.1c0.6,0.6,0.6,1.6,0' +
          ',2.2l-3.3,3.3c-0.6,0.6-1.6,0.6-2.2,0 l-1.1-1.1C30.7,41.2,30.7,39' +
          '.3,31.9,38z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'reddit'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 15.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://w' +
          'ww.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#13#10'<svg version="1.1"  ' +
          'xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.or' +
          'g/1999/xlink" x="0px" y="0px"'#13#10#9' width="48px" height="48px" view' +
          'Box="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pre' +
          'serve">'#13#10'<g>'#13#10#9'<path fill="#FFFFFF" d="M12.193,19.555c-1.94-1.74' +
          '1-4.79-1.727-6.365,0.029c-1.576,1.756-1.301,5.023,0.926,6.632L12' +
          '.193,19.555z"'#13#10#9#9'/>'#13#10#9'<path fill="#FFFFFF" d="M35.807,19.555c1.9' +
          '39-1.741,4.789-1.727,6.365,0.029c1.575,1.756,1.302,5.023-0.927,6' +
          '.632L35.807,19.555z"'#13#10#9#9'/>'#13#10#9'<g>'#13#10#9#9'<circle fill="#FFFFFF" cx="3' +
          '8.32" cy="10.475" r="3.5"/>'#13#10#9'</g>'#13#10#9'<g>'#13#10#9#9'<ellipse fill="#FFFF' +
          'FF" cx="24.085" cy="28.611" rx="18.085" ry="12.946"/>'#13#10#9'</g>'#13#10'</' +
          'g>'#13#10'<g>'#13#10#9'<circle fill="#D84315" cx="30.365" cy="26.39" r="2.884' +
          '"/>'#13#10#9'<circle fill="#D84315" cx="17.635" cy="26.39" r="2.884"/>'#13 +
          #10'</g>'#13#10'<g>'#13#10#9'<path fill="#37474F" d="M24.002,34.902c-3.252,0-6.1' +
          '4-0.745-8.002-1.902c1.024,2.044,4.196,4,8.002,4c3.802,0,6.976-1.' +
          '956,7.998-4'#13#10#9#9'C30.143,34.157,27.254,34.902,24.002,34.902z"/>'#13#10#9 +
          '<path fill="#37474F" d="M41.83,27.026l-1.17-1.621c0.831-0.6,1.37' +
          '3-1.556,1.488-2.623c0.105-0.98-0.157-1.903-0.721-2.531'#13#10#9#9'c-0.57' +
          '1-0.637-1.391-0.99-2.307-0.994c-0.927,0.013-1.894,0.365-2.646,1.' +
          '041l-1.336-1.488c1.123-1.008,2.545-1.523,3.991-1.553'#13#10#9#9'c1.488,0' +
          '.007,2.833,0.596,3.786,1.658c0.942,1.05,1.387,2.537,1.221,4.081C' +
          '43.961,24.626,43.121,26.096,41.83,27.026z"/>'#13#10#9'<path fill="#3747' +
          '4F" d="M6.169,27.026c-1.29-0.932-2.131-2.401-2.306-4.031c-0.166-' +
          '1.543,0.279-3.03,1.221-4.079'#13#10#9#9'c0.953-1.062,2.297-1.651,3.785-1' +
          '.658c0.009,0,0.018,0,0.027,0c1.441,0,2.849,0.551,3.965,1.553l-1.' +
          '336,1.488'#13#10#9#9'c-0.753-0.676-1.689-1.005-2.646-1.041c-0.916,0.004-' +
          '1.735,0.357-2.306,0.994c-0.563,0.628-0.826,1.55-0.721,2.53'#13#10#9#9'c0' +
          '.115,1.067,0.657,2.023,1.488,2.624L6.169,27.026z"/>'#13#10#9'<path fill' +
          '="#37474F" d="M25,16.84h-2c0-2.885,0-10.548,4.979-10.548c2.154,0' +
          ',3.193,1.211,3.952,2.096'#13#10#9#9'c0.629,0.734,0.961,1.086,1.616,1.086' +
          'h1.37v2h-1.37c-1.604,0-2.453-0.99-3.135-1.785c-0.67-0.781-1.198-' +
          '1.398-2.434-1.398'#13#10#9#9'C25.975,8.292,25,11.088,25,16.84z"/>'#13#10#9'<pat' +
          'h fill="#37474F" d="M24.085,16.95c9.421,0,17.085,5.231,17.085,11' +
          '.661c0,6.431-7.664,11.662-17.085,11.662S7,35.042,7,28.611'#13#10#9#9'C7,' +
          '22.181,14.664,16.95,24.085,16.95 M24.085,14.95C13.544,14.95,5,21' +
          '.066,5,28.611c0,7.546,8.545,13.662,19.085,13.662'#13#10#9#9'c10.54,0,19.' +
          '085-6.116,19.085-13.662C43.17,21.066,34.625,14.95,24.085,14.95L2' +
          '4.085,14.95z"/>'#13#10#9'<path fill="#37474F" d="M38.32,7.975c1.379,0,2' +
          '.5,1.122,2.5,2.5s-1.121,2.5-2.5,2.5s-2.5-1.122-2.5-2.5S36.941,7.' +
          '975,38.32,7.975'#13#10#9#9' M38.32,5.975c-2.484,0-4.5,2.015-4.5,4.5s2.01' +
          '6,4.5,4.5,4.5c2.486,0,4.5-2.015,4.5-4.5S40.807,5.975,38.32,5.975' +
          'L38.32,5.975z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'redo'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#00BCD4' +
          '">'#13#10'        <polygon points="43,18 29,6.3 29,29.7"/>'#13#10'        <p' +
          'ath d="M20,14h12v8H20c-2.8,0-5,2.2-5,5s2.2,5,5,5h3v8h-3c-7.2,0-1' +
          '3-5.8-13-13S12.8,14,20,14z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'refresh'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#1565C0' +
          '">'#13#10'        <path d="M13,13c0-3.3,2.7-6,6-6h10c3.3,0,6,2.7,6,6h4' +
          'c0-5.5-4.5-10-10-10H19C13.5,3,9,7.5,9,13v11.2h4V13z"/>'#13#10'        ' +
          '<polygon points="4.6,22 11,30.4 17.4,22"/>'#13#10'    </g>'#13#10'    <g fil' +
          'l="#1565C0">'#13#10'        <path d="M35,35c0,3.3-2.7,6-6,6H19c-3.3,0-' +
          '6-2.7-6-6H9c0,5.5,4.5,10,10,10h10c5.5,0,10-4.5,10-10V23h-4V35z"/' +
          '>'#13#10'        <polygon points="30.6,26 37,17.6 43.4,26"/>'#13#10'    </g>' +
          #13#10'</svg>'#13#10
      end
      item
        IconName = 'registered_trademark'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#9' +
          'C27B0" cx="24" cy="24" r="21"/>'#13#10'    <path fill="#E1BEE7" d="M25' +
          ',26.8h-4.5v9h-4V12.5h8.2c1.3,0,2.5,0.2,3.6,0.5c1,0.3,1.9,0.8,2.6' +
          ',1.3c0.7,0.6,1.3,1.3,1.6,2.2 s0.6,1.9,0.6,3c0,1.6-0.4,2.9-1.1,3.' +
          '9c-0.8,1-1.8,1.9-3.1,2.4l5.2,9.7v0.2h-4.3L25,26.8z M20.5,23.6h4.' +
          '2c0.7,0,1.4-0.1,1.9-0.3 c0.5-0.2,1-0.5,1.4-0.8c0.4-0.3,0.6-0.7,0' +
          '.8-1.2c0.2-0.5,0.3-1,0.3-1.6c0-0.6-0.1-1.1-0.3-1.6c-0.2-0.5-0.4-' +
          '0.9-0.8-1.2 c-0.4-0.3-0.8-0.6-1.4-0.8c-0.5-0.2-1.2-0.3-2-0.3h-4.' +
          '1V23.6z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'remove_image'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#8CB' +
          'CD6" d="M40,41H8c-2.2,0-4-1.8-4-4V11c0-2.2,1.8-4,4-4h32c2.2,0,4,' +
          '1.8,4,4v26C44,39.2,42.2,41,40,41z"/>'#13#10'    <circle fill="#B3DDF5"' +
          ' cx="35" cy="16" r="3"/>'#13#10'    <polygon fill="#9AC9E3" points="20' +
          ',16 9,32 31,32"/>'#13#10'    <polygon fill="#B3DDF5" points="31,22 23,' +
          '32 39,32"/>'#13#10'    <circle fill="#F44336" cx="38" cy="38" r="10"/>' +
          #13#10'    <g fill="#fff">'#13#10'        <rect x="36.5" y="32" transform="' +
          'matrix(-.707 .707 -.707 -.707 91.74 38)" width="3" height="12"/>' +
          #13#10'        <rect x="36.5" y="32" transform="matrix(-.707 -.707 .7' +
          '07 -.707 38 91.74)" width="3" height="12"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'reuse'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '455A64" points="12.1,42 17.2,42 16.5,18.2 10.9,20.2"/>'#13#10'    <cir' +
          'cle fill="#FFB74D" cx="36.5" cy="10" r="5"/>'#13#10'    <polygon fill=' +
          '"#607D8B" points="11,42 6,42 7.8,18.6 14.2,20.9"/>'#13#10'    <path fi' +
          'll="#607D8B" d="M31.7,15.9c-0.6-2-1.3-4-2.5-5.8c-1.3-1.6-3.2-3.1' +
          '-6.1-2c-3.1,1.3-9.2,3.6-11.2,4.5 c-2.3,1.1-4.1,2.7-4.1,5.9c0,3.4' +
          ',4.3,5.3,4.3,5.3l14.7-6.1l1.7,4.5l5.3,0.1C33.8,22.4,32.3,17.9,31' +
          '.7,15.9z"/>'#13#10'    <path fill="#B39DDB" d="M37.9,42h-7.9c-1,0-1.8-' +
          '0.7-2-1.7l-2.6-17.1h17l-2.6,17.1C39.8,41.3,38.9,42,37.9,42z"/>'#13#10 +
          '    <path fill="#7E57C2" d="M42,24H26c-0.6,0-1-0.4-1-1v0c0-0.6,0' +
          '.4-1,1-1h16c0.6,0,1,0.4,1,1v0C43,23.6,42.6,24,42,24z"/>'#13#10'</svg>'#13 +
          #10
      end
      item
        IconName = 'right'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#3F51B5' +
          '">'#13#10'        <polygon points="44,24 30,35.7 30,12.3"/>'#13#10'        <' +
          'rect x="6" y="20" width="27" height="8"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'right_down2'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '3F51B5" points="29,44 17.3,30 40.7,30"/>'#13#10'    <path fill="#3F51B' +
          '5" d="M21,6H8v8h13c2.2,0,4,1.8,4,4v17h8V18C33,11.4,27.6,6,21,6z"' +
          '/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'right_up2'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '3F51B5" points="29,4 17.3,18 40.7,18"/>'#13#10'    <path fill="#3F51B5' +
          '" d="M21,42H8v-8h13c2.2,0,4-1.8,4-4V13h8v17C33,36.6,27.6,42,21,4' +
          '2z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'rotate_camera'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#5E35B1' +
          '">'#13#10'        <path d="M33.9,12.1H14.2L17.6,7c0.4-0.6,1-0.9,1.7-0.' +
          '9h9.6c0.7,0,1.3,0.3,1.7,0.9L33.9,12.1z"/>'#13#10'        <path d="M14,' +
          '11H8V9.2C8,8.5,8.5,8,9.2,8h3.6C13.5,8,14,8.5,14,9.2V11z"/>'#13#10'    ' +
          '    <path d="M40,42H8c-2.2,0-4-1.8-4-4V14c0-2.2,1.8-4,4-4h32c2.2' +
          ',0,4,1.8,4,4v24C44,40.2,42.2,42,40,42z"/>'#13#10'    </g>'#13#10'    <path f' +
          'ill="#E8EAF6" d="M34,25c0-5.5-4.5-10-10-10s-10,4.5-10,10s4.5,10,' +
          '10,10v-2c-4.4,0-8-3.6-8-8s3.6-8,8-8s8,3.6,8,8h-3.5 l4.5,5.6l4.5-' +
          '5.6H34z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'rotate_to_landscape'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#374' +
          '74F" d="M10,41V7c0-2.2,1.8-4,4-4h20c2.2,0,4,1.8,4,4v34c0,2.2-1.8' +
          ',4-4,4H14C11.8,45,10,43.2,10,41z"/>'#13#10'    <path fill="#F3E5F5" d=' +
          '"M34,6H14c-0.6,0-1,0.4-1,1v34c0,0.6,0.4,1,1,1h20c0.6,0,1-0.4,1-1' +
          'V7C35,6.4,34.6,6,34,6z"/>'#13#10'    <polygon fill="#9C27B0" points="2' +
          '2,34 27.9,27 16.1,27"/>'#13#10'    <path fill="#9C27B0" d="M26,16c-3.3' +
          ',0-6,2.7-6,6v6h4v-6c0-1.1,0.9-2,2-2s2,0.9,2,2v2h4v-2C32,18.7,29.' +
          '3,16,26,16z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'rotate_to_portrait'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#374' +
          '74F" d="M41,38H7c-2.2,0-4-1.8-4-4V14c0-2.2,1.8-4,4-4h34c2.2,0,4,' +
          '1.8,4,4v20C45,36.2,43.2,38,41,38z"/>'#13#10'    <path fill="#F3E5F5" d' +
          '="M6,14v20c0,0.6,0.4,1,1,1h34c0.6,0,1-0.4,1-1V14c0-0.6-0.4-1-1-1' +
          'H7C6.4,13,6,13.4,6,14z"/>'#13#10'    <polygon fill="#9C27B0" points="2' +
          '6,15 20.1,22 31.9,22"/>'#13#10'    <path fill="#9C27B0" d="M24,21v6c0,' +
          '1.1-0.9,2-2,2s-2-0.9-2-2v-2h-4v2c0,3.3,2.7,6,6,6s6-2.7,6-6v-6H24' +
          'z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'rules'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#42A' +
          '5F5" d="M39,45H9c0,0-3-0.1-3-8h36C42,44.9,39,45,39,45z"/>'#13#10'    <' +
          'rect x="8" y="3" fill="#90CAF9" width="32" height="34"/>'#13#10'    <g' +
          ' fill="#1976D2">'#13#10'        <rect x="18" y="15" width="16" height=' +
          '"2"/>'#13#10'        <rect x="18" y="19" width="16" height="2"/>'#13#10'    ' +
          '    <rect x="18" y="23" width="16" height="2"/>'#13#10'        <rect x' +
          '="18" y="27" width="16" height="2"/>'#13#10'        <rect x="18" y="31' +
          '" width="16" height="2"/>'#13#10'    </g>'#13#10'    <g fill="#1976D2">'#13#10'   ' +
          '     <rect x="14" y="15" width="2" height="2"/>'#13#10'        <rect x' +
          '="14" y="19" width="2" height="2"/>'#13#10'        <rect x="14" y="23"' +
          ' width="2" height="2"/>'#13#10'        <rect x="14" y="27" width="2" h' +
          'eight="2"/>'#13#10'        <rect x="14" y="31" width="2" height="2"/>'#13 +
          #10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'safe'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="8" y="3' +
          '9" fill="#455A64" width="6" height="3"/>'#13#10'    <rect x="34" y="39' +
          '" fill="#455A64" width="6" height="3"/>'#13#10'    <path fill="#78909C' +
          '" d="M40,41H8c-2.2,0-4-1.8-4-4V11c0-2.2,1.8-4,4-4h32c2.2,0,4,1.8' +
          ',4,4v26C44,39.2,42.2,41,40,41z"/>'#13#10'    <path fill="#90A4AE" d="M' +
          '40,38H8c-0.6,0-1-0.4-1-1V11c0-0.6,0.4-1,1-1h32c0.6,0,1,0.4,1,1v2' +
          '6C41,37.6,40.6,38,40,38z"/>'#13#10'    <path fill="#37474F" d="M29,14c' +
          '-5.5,0-10,4.5-10,10c0,5.5,4.5,10,10,10s10-4.5,10-10C39,18.5,34.5' +
          ',14,29,14z M29,31 c-3.9,0-7-3.1-7-7c0-3.9,3.1-7,7-7s7,3.1,7,7C36' +
          ',27.9,32.9,31,29,31z"/>'#13#10'    <g fill="#B0BEC5">'#13#10'        <path d' +
          '="M35.3,19.1l0.4-0.4c0.4-0.4,0.4-1,0-1.4s-1-0.4-1.4,0l-0.4,0.4C3' +
          '4.4,18.1,34.9,18.6,35.3,19.1z"/>'#13#10'        <path d="M22.7,19.1c0.' +
          '4-0.5,0.9-1,1.4-1.4l-0.4-0.4c-0.4-0.4-1-0.4-1.4,0s-0.4,1,0,1.4L2' +
          '2.7,19.1z"/>'#13#10'        <path d="M21,24c0-0.3,0-0.7,0.1-1h-0.6c-0.' +
          '6,0-1,0.4-1,1s0.4,1,1,1h0.6C21,24.7,21,24.3,21,24z"/>'#13#10'        <' +
          'path d="M29,16c0.3,0,0.7,0,1,0.1v-0.6c0-0.6-0.4-1-1-1s-1,0.4-1,1' +
          'v0.6C28.3,16,28.7,16,29,16z"/>'#13#10'        <path d="M35.3,28.9c-0.4' +
          ',0.5-0.9,1-1.4,1.4l0.4,0.4c0.2,0.2,0.5,0.3,0.7,0.3s0.5-0.1,0.7-0' +
          '.3c0.4-0.4,0.4-1,0-1.4 L35.3,28.9z"/>'#13#10'        <path d="M22.7,28' +
          '.9l-0.4,0.4c-0.4,0.4-0.4,1,0,1.4c0.2,0.2,0.5,0.3,0.7,0.3s0.5-0.1' +
          ',0.7-0.3l0.4-0.4 C23.6,29.9,23.1,29.4,22.7,28.9z"/>'#13#10'        <pa' +
          'th d="M37.5,23h-0.6c0,0.3,0.1,0.7,0.1,1s0,0.7-0.1,1h0.6c0.6,0,1-' +
          '0.4,1-1S38.1,23,37.5,23z"/>'#13#10'        <path d="M29,32c-0.3,0-0.7,' +
          '0-1-0.1v0.6c0,0.6,0.4,1,1,1s1-0.4,1-1v-0.6C29.7,32,29.3,32,29,32' +
          'z"/>'#13#10'    </g>'#13#10'    <path fill="#455A64" d="M12,20c-1.1,0-2,0.9-' +
          '2,2v8c0,1.1,0.9,2,2,2s2-0.9,2-2v-8C14,20.9,13.1,20,12,20z"/>'#13#10'  ' +
          '  <path fill="#CFD8DC" d="M12,18c-1.1,0-2,0.9-2,2v8c0,1.1,0.9,2,' +
          '2,2s2-0.9,2-2v-8C14,18.9,13.1,18,12,18z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'sales_performance'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#FFA000' +
          '">'#13#10'        <path d="M38,13c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1' +
          '.1,2.7,2,6,2s6-0.9,6-2c0-0.4,0-1.6,0-2C44,12.1,41.3,13,38,13 z"/' +
          '>'#13#10'        <path d="M38,10c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.' +
          '1,2.7,2,6,2s6-0.9,6-2c0-0.4,0-1.6,0-2C44,9.1,41.3,10,38,10z"/>'#13#10 +
          '        <path d="M38,16c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2' +
          '.7,2,6,2s6-0.9,6-2c0-0.4,0-1.6,0-2C44,15.1,41.3,16,38,16 z"/>'#13#10' ' +
          '       <path d="M38,19c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.' +
          '7,2,6,2s6-0.9,6-2c0-0.4,0-1.6,0-2C44,18.1,41.3,19,38,19 z"/>'#13#10'  ' +
          '      <path d="M38,22c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7' +
          ',2,6,2s6-0.9,6-2c0-0.4,0-1.6,0-2C44,21.1,41.3,22,38,22 z"/>'#13#10'   ' +
          '     <path d="M38,25c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,' +
          '2,6,2s6-0.9,6-2c0-0.4,0-1.6,0-2C44,24.1,41.3,25,38,25 z"/>'#13#10'    ' +
          '    <path d="M38,28c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2' +
          ',6,2s6-0.9,6-2c0-0.4,0-1.6,0-2C44,27.1,41.3,28,38,28 z"/>'#13#10'     ' +
          '   <path d="M38,31c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,' +
          '6,2s6-0.9,6-2c0-0.4,0-1.6,0-2C44,30.1,41.3,31,38,31 z"/>'#13#10'      ' +
          '  <path d="M38,34c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,6' +
          ',2s6-0.9,6-2c0-0.4,0-1.6,0-2C44,33.1,41.3,34,38,34 z"/>'#13#10'       ' +
          ' <path d="M38,37c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,6,' +
          '2s6-0.9,6-2c0-0.4,0-1.6,0-2C44,36.1,41.3,37,38,37 z"/>'#13#10'        ' +
          '<path d="M38,40c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,6,2' +
          's6-0.9,6-2c0-0.4,0-1.6,0-2C44,39.1,41.3,40,38,40 z"/>'#13#10'    </g>'#13 +
          #10'    <g fill="#FFC107">'#13#10'        <ellipse cx="38" cy="8" rx="6" ' +
          'ry="2"/>'#13#10'        <path d="M38,12c-2.8,0-5.1-0.6-5.8-1.5C32.1,10' +
          '.7,32,10.8,32,11c0,1.1,2.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5' +
          ' C43.1,11.4,40.8,12,38,12z"/>'#13#10'        <path d="M38,15c-2.8,0-5.' +
          '1-0.6-5.8-1.5C32.1,13.7,32,13.8,32,14c0,1.1,2.7,2,6,2s6-0.9,6-2c' +
          '0-0.2-0.1-0.3-0.2-0.5 C43.1,14.4,40.8,15,38,15z"/>'#13#10'        <pat' +
          'h d="M38,18c-2.8,0-5.1-0.6-5.8-1.5C32.1,16.7,32,16.8,32,17c0,1.1' +
          ',2.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 C43.1,17.4,40.8,18,38' +
          ',18z"/>'#13#10'        <path d="M38,21c-2.8,0-5.1-0.6-5.8-1.5C32.1,19.' +
          '7,32,19.8,32,20c0,1.1,2.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 ' +
          'C43.1,20.4,40.8,21,38,21z"/>'#13#10'        <path d="M38,24c-2.8,0-5.1' +
          '-0.6-5.8-1.5C32.1,22.7,32,22.8,32,23c0,1.1,2.7,2,6,2s6-0.9,6-2c0' +
          '-0.2-0.1-0.3-0.2-0.5 C43.1,23.4,40.8,24,38,24z"/>'#13#10'        <path' +
          ' d="M38,27c-2.8,0-5.1-0.6-5.8-1.5C32.1,25.7,32,25.8,32,26c0,1.1,' +
          '2.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 C43.1,26.4,40.8,27,38,' +
          '27z"/>'#13#10'        <path d="M38,30c-2.8,0-5.1-0.6-5.8-1.5C32.1,28.7' +
          ',32,28.8,32,29c0,1.1,2.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 C' +
          '43.1,29.4,40.8,30,38,30z"/>'#13#10'        <path d="M38,33c-2.8,0-5.1-' +
          '0.6-5.8-1.5C32.1,31.7,32,31.8,32,32c0,1.1,2.7,2,6,2s6-0.9,6-2c0-' +
          '0.2-0.1-0.3-0.2-0.5 C43.1,32.4,40.8,33,38,33z"/>'#13#10'        <path ' +
          'd="M38,36c-2.8,0-5.1-0.6-5.8-1.5C32.1,34.7,32,34.8,32,35c0,1.1,2' +
          '.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 C43.1,35.4,40.8,36,38,3' +
          '6z"/>'#13#10'        <path d="M38,39c-2.8,0-5.1-0.6-5.8-1.5C32.1,37.7,' +
          '32,37.8,32,38c0,1.1,2.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 C4' +
          '3.1,38.4,40.8,39,38,39z"/>'#13#10'    </g>'#13#10'    <g fill="#FFA000">'#13#10'  ' +
          '      <path d="M10,19c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7' +
          ',2,6,2s6-0.9,6-2c0-0.4,0-1.6,0-2C16,18.1,13.3,19,10,19 z"/>'#13#10'   ' +
          '     <path d="M10,16c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,' +
          '2,6,2s6-0.9,6-2c0-0.4,0-1.6,0-2C16,15.1,13.3,16,10,16 z"/>'#13#10'    ' +
          '    <path d="M10,22c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2' +
          ',6,2s6-0.9,6-2c0-0.4,0-1.6,0-2C16,21.1,13.3,22,10,22 z"/>'#13#10'     ' +
          '   <path d="M10,25c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,' +
          '6,2s6-0.9,6-2c0-0.4,0-1.6,0-2C16,24.1,13.3,25,10,25 z"/>'#13#10'      ' +
          '  <path d="M10,28c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,6' +
          ',2s6-0.9,6-2c0-0.4,0-1.6,0-2C16,27.1,13.3,28,10,28 z"/>'#13#10'       ' +
          ' <path d="M10,31c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,6,' +
          '2s6-0.9,6-2c0-0.4,0-1.6,0-2C16,30.1,13.3,31,10,31 z"/>'#13#10'        ' +
          '<path d="M10,34c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,6,2' +
          's6-0.9,6-2c0-0.4,0-1.6,0-2C16,33.1,13.3,34,10,34 z"/>'#13#10'        <' +
          'path d="M10,37c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,6,2s' +
          '6-0.9,6-2c0-0.4,0-1.6,0-2C16,36.1,13.3,37,10,37 z"/>'#13#10'        <p' +
          'ath d="M10,40c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,6,2s6' +
          '-0.9,6-2c0-0.4,0-1.6,0-2C16,39.1,13.3,40,10,40 z"/>'#13#10'    </g>'#13#10' ' +
          '   <g fill="#FFC107">'#13#10'        <ellipse cx="10" cy="14" rx="6" r' +
          'y="2"/>'#13#10'        <path d="M10,18c-2.8,0-5.1-0.6-5.8-1.5C4.1,16.7' +
          ',4,16.8,4,17c0,1.1,2.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 C15' +
          '.1,17.4,12.8,18,10,18z"/>'#13#10'        <path d="M10,21c-2.8,0-5.1-0.' +
          '6-5.8-1.5C4.1,19.7,4,19.8,4,20c0,1.1,2.7,2,6,2s6-0.9,6-2c0-0.2-0' +
          '.1-0.3-0.2-0.5 C15.1,20.4,12.8,21,10,21z"/>'#13#10'        <path d="M1' +
          '0,24c-2.8,0-5.1-0.6-5.8-1.5C4.1,22.7,4,22.8,4,23c0,1.1,2.7,2,6,2' +
          's6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 C15.1,23.4,12.8,24,10,24z"/>'#13#10' ' +
          '       <path d="M10,27c-2.8,0-5.1-0.6-5.8-1.5C4.1,25.7,4,25.8,4,' +
          '26c0,1.1,2.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 C15.1,26.4,12' +
          '.8,27,10,27z"/>'#13#10'        <path d="M10,30c-2.8,0-5.1-0.6-5.8-1.5C' +
          '4.1,28.7,4,28.8,4,29c0,1.1,2.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2' +
          '-0.5 C15.1,29.4,12.8,30,10,30z"/>'#13#10'        <path d="M10,33c-2.8,' +
          '0-5.1-0.6-5.8-1.5C4.1,31.7,4,31.8,4,32c0,1.1,2.7,2,6,2s6-0.9,6-2' +
          'c0-0.2-0.1-0.3-0.2-0.5 C15.1,32.4,12.8,33,10,33z"/>'#13#10'        <pa' +
          'th d="M10,36c-2.8,0-5.1-0.6-5.8-1.5C4.1,34.7,4,34.8,4,35c0,1.1,2' +
          '.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 C15.1,35.4,12.8,36,10,3' +
          '6z"/>'#13#10'        <path d="M10,39c-2.8,0-5.1-0.6-5.8-1.5C4.1,37.7,4' +
          ',37.8,4,38c0,1.1,2.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 C15.1' +
          ',38.4,12.8,39,10,39z"/>'#13#10'    </g>'#13#10'    <g fill="#FFA000">'#13#10'     ' +
          '   <path d="M24,28c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,' +
          '6,2s6-0.9,6-2c0-0.4,0-1.6,0-2C30,27.1,27.3,28,24,28 z"/>'#13#10'      ' +
          '  <path d="M24,25c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,6' +
          ',2s6-0.9,6-2c0-0.4,0-1.6,0-2C30,24.1,27.3,25,24,25 z"/>'#13#10'       ' +
          ' <path d="M24,31c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,6,' +
          '2s6-0.9,6-2c0-0.4,0-1.6,0-2C30,30.1,27.3,31,24,31 z"/>'#13#10'        ' +
          '<path d="M24,34c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,6,2' +
          's6-0.9,6-2c0-0.4,0-1.6,0-2C30,33.1,27.3,34,24,34 z"/>'#13#10'        <' +
          'path d="M24,37c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,6,2s' +
          '6-0.9,6-2c0-0.4,0-1.6,0-2C30,36.1,27.3,37,24,37 z"/>'#13#10'        <p' +
          'ath d="M24,40c-3.3,0-6-0.9-6-2c0,0.4,0,1.6,0,2c0,1.1,2.7,2,6,2s6' +
          '-0.9,6-2c0-0.4,0-1.6,0-2C30,39.1,27.3,40,24,40 z"/>'#13#10'    </g>'#13#10' ' +
          '   <g fill="#FFC107">'#13#10'        <ellipse cx="24" cy="23" rx="6" r' +
          'y="2"/>'#13#10'        <path d="M24,27c-2.8,0-5.1-0.6-5.8-1.5C18.1,25.' +
          '7,18,25.8,18,26c0,1.1,2.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 ' +
          'C29.1,26.4,26.8,27,24,27z"/>'#13#10'        <path d="M24,30c-2.8,0-5.1' +
          '-0.6-5.8-1.5C18.1,28.7,18,28.8,18,29c0,1.1,2.7,2,6,2s6-0.9,6-2c0' +
          '-0.2-0.1-0.3-0.2-0.5 C29.1,29.4,26.8,30,24,30z"/>'#13#10'        <path' +
          ' d="M24,33c-2.8,0-5.1-0.6-5.8-1.5C18.1,31.7,18,31.8,18,32c0,1.1,' +
          '2.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 C29.1,32.4,26.8,33,24,' +
          '33z"/>'#13#10'        <path d="M24,36c-2.8,0-5.1-0.6-5.8-1.5C18.1,34.7' +
          ',18,34.8,18,35c0,1.1,2.7,2,6,2s6-0.9,6-2c0-0.2-0.1-0.3-0.2-0.5 C' +
          '29.1,35.4,26.8,36,24,36z"/>'#13#10'        <path d="M24,39c-2.8,0-5.1-' +
          '0.6-5.8-1.5C18.1,37.7,18,37.8,18,38c0,1.1,2.7,2,6,2s6-0.9,6-2c0-' +
          '0.2-0.1-0.3-0.2-0.5 C29.1,38.4,26.8,39,24,39z"/>'#13#10'    </g>'#13#10'</sv' +
          'g>'#13#10
      end
      item
        IconName = 'scatter_plot'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          'CFD8DC" points="9,39 9,6 7,6 7,41 42,41 42,39"/>'#13#10'    <g fill="#' +
          '00BCD4">'#13#10'        <circle cx="39" cy="11" r="3"/>'#13#10'        <circ' +
          'le cx="31" cy="13" r="3"/>'#13#10'        <circle cx="37" cy="19" r="3' +
          '"/>'#13#10'        <circle cx="34" cy="26" r="3"/>'#13#10'        <circle cx' +
          '="28" cy="20" r="3"/>'#13#10'        <circle cx="26" cy="28" r="3"/>'#13#10 +
          '        <circle cx="20" cy="23" r="3"/>'#13#10'        <circle cx="21"' +
          ' cy="33" r="3"/>'#13#10'        <circle cx="14" cy="30" r="3"/>'#13#10'    <' +
          '/g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'search'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#616161' +
          '">'#13#10'        <rect x="34.6" y="28.1" transform="matrix(.707 -.707' +
          ' .707 .707 -15.154 36.586)" width="4" height="17"/>'#13#10'        <ci' +
          'rcle cx="20" cy="20" r="16"/>'#13#10'    </g>'#13#10'    <rect x="36.2" y="3' +
          '2.1" transform="matrix(.707 -.707 .707 .707 -15.839 38.239)" fil' +
          'l="#37474F" width="4" height="12.3"/>'#13#10'    <circle fill="#64B5F6' +
          '" cx="20" cy="20" r="13"/>'#13#10'    <path fill="#BBDEFB" d="M26.9,14' +
          '.2c-1.7-2-4.2-3.2-6.9-3.2s-5.2,1.2-6.9,3.2c-0.4,0.4-0.3,1.1,0.1,' +
          '1.4c0.4,0.4,1.1,0.3,1.4-0.1 C16,13.9,17.9,13,20,13s4,0.9,5.4,2.5' +
          'c0.2,0.2,0.5,0.4,0.8,0.4c0.2,0,0.5-0.1,0.6-0.2C27.2,15.3,27.2,14' +
          '.6,26.9,14.2z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'self_service_kiosk'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#546' +
          'E7A" d="M44,30H4V11c0-2.2,1.8-4,4-4h32c2.2,0,4,1.8,4,4V30z"/>'#13#10' ' +
          '   <path fill="#64B5F6" d="M40,27H8c-0.6,0-1-0.4-1-1V11c0-0.6,0.' +
          '4-1,1-1h32c0.6,0,1,0.4,1,1v15C41,26.6,40.6,27,40,27z"/>'#13#10'    <pa' +
          'th fill="#78909C" d="M40,41H8c-2.2,0-4-1.8-4-4v-7h40v7C44,39.2,4' +
          '2.2,41,40,41z"/>'#13#10'    <g fill="#37474F">'#13#10'        <rect x="27" y' +
          '="34" width="12" height="2"/>'#13#10'        <rect x="9" y="34" width=' +
          '"12" height="2"/>'#13#10'        <path d="M18,35c0,1.1-1.3,2-3,2s-3-0.' +
          '9-3-2H18z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'selfie'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FFB' +
          '74D" d="M32.9,22c0-0.3,0.1-0.7,0.1-1c0-1.1,0-5.9,0-7c0-7.6-18-5-' +
          '18,0c0,1.1,0,5.9,0,7c0,0.3,0,0.7,0.1,1H32.9z"/>'#13#10'    <path fill=' +
          '"#37474F" d="M40,44H8c-2.2,0-4-1.8-4-4V26c0-2.2,1.8-4,4-4h32c2.2' +
          ',0,4,1.8,4,4v14C44,42.2,42.2,44,40,44z"/>'#13#10'    <path fill="#BBDE' +
          'FB" d="M7,26v14c0,0.6,0.4,1,1,1h29c0.6,0,1-0.4,1-1V26c0-0.6-0.4-' +
          '1-1-1H8C7.4,25,7,25.4,7,26z"/>'#13#10'    <rect x="40" y="30" fill="#7' +
          '8909C" width="2" height="6"/>'#13#10'    <rect x="19" y="32" fill="#BF' +
          '360C" width="8" height="9"/>'#13#10'    <rect x="20.5" y="37.5" fill="' +
          '#FF9800" width="5" height="3.5"/>'#13#10'    <path fill="#FFB74D" d="M' +
          '27.5,32c0-3.8-9-2.5-9,0c0,0.5,0,3,0,3.5c0,2.5,2,4.5,4.5,4.5s4.5-' +
          '2,4.5-4.5C27.5,35,27.5,32.5,27.5,32z"/>'#13#10'    <g fill="#784719">'#13 +
          #10'        <circle cx="28" cy="21" r="1"/>'#13#10'        <circle cx="20' +
          '" cy="21" r="1"/>'#13#10'        <circle cx="25" cy="35.5" r=".5"/>'#13#10' ' +
          '       <circle cx="21" cy="35.5" r=".5"/>'#13#10'    </g>'#13#10'    <g fill' +
          '="#FF5722">'#13#10'        <path d="M23,27c-3,0-8,1.3-8,11l4,3v-6.5l6-' +
          '3.5l2,2.5V41l4-3c0-2-0.8-10-6-10l-0.5-1H23z"/>'#13#10'        <path d=' +
          '"M16,22v-3l12-7l4,5v5h6.8C38.3,15.8,36.1,6,28,6l-1-2h-3C18.5,4,1' +
          '0.7,6.8,9.2,22H16z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'serial_tasks'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#90C' +
          'AF9" d="M33,9H11v4h22c1.1,0,2,0.9,2,2v20H23v4h16V15C39,11.7,36.3' +
          ',9,33,9z"/>'#13#10'    <rect x="6" y="6" fill="#D81B60" width="10" hei' +
          'ght="10"/>'#13#10'    <g fill="#2196F3">'#13#10'        <rect x="32" y="17" ' +
          'width="10" height="10"/>'#13#10'        <rect x="16" y="32" width="10"' +
          ' height="10"/>'#13#10'        <circle cx="26" cy="11" r="5"/>'#13#10'       ' +
          ' <circle cx="37" cy="37" r="5"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'service_mark'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#9' +
          'C27B0" cx="24" cy="24" r="21"/>'#13#10'    <g fill="#E1BEE7">'#13#10'       ' +
          ' <path d="M16.7,28.2c0-3.8-7.3-2.2-7.3-8.1c0-0.7,0.4-4.8,5.5-4.8' +
          'c5.1,0,5.4,4.5,5.4,5.3h-3.5c0-0.4,0-2.5-2-2.5 c-1.8,0-1.9,1.7-1.' +
          '9,2c0,3,7.4,2,7.4,8.1c0,2-1.1,4.8-5.3,4.8C10.3,33,9,29.6,9,27.3h' +
          '3.5c0,0.5-0.2,2.8,2.5,2.8 C16.8,30.2,16.7,28.5,16.7,28.2z"/>'#13#10'  ' +
          '      <path d="M27.1,15.6L30.3,28l3.2-12.4h4.5v17.2h-3.5v-4.6l0.' +
          '3-7.2l-3.4,11.8h-2.4l-3.4-11.8l0.3,7.2v4.6h-3.5V15.6 H27.1z"/>'#13#10 +
          '    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'services'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#E65' +
          '100" d="M25.6,34.4c0.1-0.4,0.1-0.9,0.1-1.4s0-0.9-0.1-1.4l2.8-2c0' +
          '.3-0.2,0.4-0.6,0.2-0.9l-2.7-4.6 c-0.2-0.3-0.5-0.4-0.8-0.3L22,25.' +
          '3c-0.7-0.6-1.5-1-2.4-1.4l-0.3-3.4c0-0.3-0.3-0.6-0.6-0.6h-5.3c-0.' +
          '3,0-0.6,0.3-0.6,0.6L12.4,24 c-0.9,0.3-1.6,0.8-2.4,1.4l-3.1-1.4c-' +
          '0.3-0.1-0.7,0-0.8,0.3l-2.7,4.6c-0.2,0.3-0.1,0.7,0.2,0.9l2.8,2c-0' +
          '.1,0.4-0.1,0.9-0.1,1.4 s0,0.9,0.1,1.4l-2.8,2c-0.3,0.2-0.4,0.6-0.' +
          '2,0.9l2.7,4.6c0.2,0.3,0.5,0.4,0.8,0.3l3.1-1.4c0.7,0.6,1.5,1,2.4,' +
          '1.4l0.3,3.4 c0,0.3,0.3,0.6,0.6,0.6h5.3c0.3,0,0.6-0.3,0.6-0.6l0.3' +
          '-3.4c0.9-0.3,1.6-0.8,2.4-1.4l3.1,1.4c0.3,0.1,0.7,0,0.8-0.3l2.7-4' +
          '.6 c0.2-0.3,0.1-0.7-0.2-0.9L25.6,34.4z M16,38c-2.8,0-5-2.2-5-5c0' +
          '-2.8,2.2-5,5-5c2.8,0,5,2.2,5,5C21,35.8,18.8,38,16,38z"/>'#13#10'    <p' +
          'ath fill="#FFA000" d="M41.9,15.3C42,14.8,42,14.4,42,14s0-0.8-0.1' +
          '-1.3l2.5-1.8c0.3-0.2,0.3-0.5,0.2-0.8l-2.5-4.3 c-0.2-0.3-0.5-0.4-' +
          '0.8-0.2l-2.9,1.3c-0.7-0.5-1.4-0.9-2.2-1.3l-0.3-3.1C36,2.2,35.8,2' +
          ',35.5,2h-4.9c-0.3,0-0.6,0.2-0.6,0.5l-0.3,3.1 c-0.8,0.3-1.5,0.7-2' +
          '.2,1.3l-2.9-1.3c-0.3-0.1-0.6,0-0.8,0.2l-2.5,4.3c-0.2,0.3-0.1,0.6' +
          ',0.2,0.8l2.5,1.8C24,13.2,24,13.6,24,14 s0,0.8,0.1,1.3l-2.5,1.8c-' +
          '0.3,0.2-0.3,0.5-0.2,0.8l2.5,4.3c0.2,0.3,0.5,0.4,0.8,0.2l2.9-1.3c' +
          '0.7,0.5,1.4,0.9,2.2,1.3l0.3,3.1 c0,0.3,0.3,0.5,0.6,0.5h4.9c0.3,0' +
          ',0.6-0.2,0.6-0.5l0.3-3.1c0.8-0.3,1.5-0.7,2.2-1.3l2.9,1.3c0.3,0.1' +
          ',0.6,0,0.8-0.2l2.5-4.3 c0.2-0.3,0.1-0.6-0.2-0.8L41.9,15.3z M33,1' +
          '9c-2.8,0-5-2.2-5-5c0-2.8,2.2-5,5-5c2.8,0,5,2.2,5,5C38,16.8,35.8,' +
          '19,33,19z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'settings'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#607' +
          'D8B" d="M39.6,27.2c0.1-0.7,0.2-1.4,0.2-2.2s-0.1-1.5-0.2-2.2l4.5-' +
          '3.2c0.4-0.3,0.6-0.9,0.3-1.4L40,10.8 c-0.3-0.5-0.8-0.7-1.3-0.4l-5' +
          ',2.3c-1.2-0.9-2.4-1.6-3.8-2.2l-0.5-5.5c-0.1-0.5-0.5-0.9-1-0.9h-8' +
          '.6c-0.5,0-1,0.4-1,0.9l-0.5,5.5 c-1.4,0.6-2.7,1.3-3.8,2.2l-5-2.3c' +
          '-0.5-0.2-1.1,0-1.3,0.4l-4.3,7.4c-0.3,0.5-0.1,1.1,0.3,1.4l4.5,3.2' +
          'c-0.1,0.7-0.2,1.4-0.2,2.2 s0.1,1.5,0.2,2.2L4,30.4c-0.4,0.3-0.6,0' +
          '.9-0.3,1.4L8,39.2c0.3,0.5,0.8,0.7,1.3,0.4l5-2.3c1.2,0.9,2.4,1.6,' +
          '3.8,2.2l0.5,5.5 c0.1,0.5,0.5,0.9,1,0.9h8.6c0.5,0,1-0.4,1-0.9l0.5' +
          '-5.5c1.4-0.6,2.7-1.3,3.8-2.2l5,2.3c0.5,0.2,1.1,0,1.3-0.4l4.3-7.4' +
          ' c0.3-0.5,0.1-1.1-0.3-1.4L39.6,27.2z M24,35c-5.5,0-10-4.5-10-10c' +
          '0-5.5,4.5-10,10-10c5.5,0,10,4.5,10,10C34,30.5,29.5,35,24,35z"/>'#13 +
          #10'    <path fill="#455A64" d="M24,13c-6.6,0-12,5.4-12,12c0,6.6,5.' +
          '4,12,12,12s12-5.4,12-12C36,18.4,30.6,13,24,13z M24,30 c-2.8,0-5-' +
          '2.2-5-5c0-2.8,2.2-5,5-5s5,2.2,5,5C29,27.8,26.8,30,24,30z"/>'#13#10'</s' +
          'vg>'#13#10
      end
      item
        IconName = 'share'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#197' +
          '6D2" d="M38.1,31.2L19.4,24l18.7-7.2c1.5-0.6,2.3-2.3,1.7-3.9c-0.6' +
          '-1.5-2.3-2.3-3.9-1.7l-26,10C8.8,21.6,8,22.8,8,24 s0.8,2.4,1.9,2.' +
          '8l26,10c0.4,0.1,0.7,0.2,1.1,0.2c1.2,0,2.3-0.7,2.8-1.9C40.4,33.5,' +
          '39.6,31.8,38.1,31.2z"/>'#13#10'    <g fill="#1E88E5">'#13#10'        <circle' +
          ' cx="11" cy="24" r="7"/>'#13#10'        <circle cx="37" cy="14" r="7"/' +
          '>'#13#10'        <circle cx="37" cy="34" r="7"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'shipped'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#8BC' +
          '34A" d="M43,36H29V14h10.6c0.9,0,1.6,0.6,1.9,1.4L45,26v8C45,35.1,' +
          '44.1,36,43,36z"/>'#13#10'    <path fill="#388E3C" d="M29,36H5c-1.1,0-2' +
          '-0.9-2-2V9c0-1.1,0.9-2,2-2h22c1.1,0,2,0.9,2,2V36z"/>'#13#10'    <g fil' +
          'l="#37474F">'#13#10'        <circle cx="37" cy="36" r="5"/>'#13#10'        <' +
          'circle cx="13" cy="36" r="5"/>'#13#10'    </g>'#13#10'    <g fill="#78909C">' +
          #13#10'        <circle cx="37" cy="36" r="2"/>'#13#10'        <circle cx="1' +
          '3" cy="36" r="2"/>'#13#10'    </g>'#13#10'    <path fill="#37474F" d="M41,25' +
          'h-7c-0.6,0-1-0.4-1-1v-7c0-0.6,0.4-1,1-1h5.3c0.4,0,0.8,0.3,0.9,0.' +
          '7l1.7,5.2c0,0.1,0.1,0.2,0.1,0.3V24 C42,24.6,41.6,25,41,25z"/>'#13#10' ' +
          '   <polygon fill="#DCEDC8" points="21.8,13.8 13.9,21.7 10.2,17.9' +
          ' 8,20.1 13.9,26 24,15.9"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'shop'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="5" y="1' +
          '9" fill="#CFD8DC" width="38" height="19"/>'#13#10'    <rect x="5" y="3' +
          '8" fill="#B0BEC5" width="38" height="4"/>'#13#10'    <rect x="27" y="2' +
          '4" fill="#455A64" width="12" height="18"/>'#13#10'    <rect x="9" y="2' +
          '4" fill="#E3F2FD" width="14" height="11"/>'#13#10'    <rect x="10" y="' +
          '25" fill="#1E88E5" width="12" height="9"/>'#13#10'    <path fill="#90A' +
          '4AE" d="M36.5,33.5c-0.3,0-0.5,0.2-0.5,0.5v2c0,0.3,0.2,0.5,0.5,0.' +
          '5S37,36.3,37,36v-2C37,33.7,36.8,33.5,36.5,33.5z"/>'#13#10'    <g fill=' +
          '"#558B2F">'#13#10'        <circle cx="24" cy="19" r="3"/>'#13#10'        <ci' +
          'rcle cx="36" cy="19" r="3"/>'#13#10'        <circle cx="12" cy="19" r=' +
          '"3"/>'#13#10'    </g>'#13#10'    <path fill="#7CB342" d="M40,6H8C6.9,6,6,6.9' +
          ',6,8v3h36V8C42,6.9,41.1,6,40,6z"/>'#13#10'    <rect x="21" y="11" fill' +
          '="#7CB342" width="6" height="8"/>'#13#10'    <polygon fill="#7CB342" p' +
          'oints="37,11 32,11 33,19 39,19"/>'#13#10'    <polygon fill="#7CB342" p' +
          'oints="11,11 16,11 15,19 9,19"/>'#13#10'    <g fill="#FFA000">'#13#10'      ' +
          '  <circle cx="30" cy="19" r="3"/>'#13#10'        <path d="M45,19c0,1.7' +
          '-1.3,3-3,3s-3-1.3-3-3s1.3-3,3-3L45,19z"/>'#13#10'        <circle cx="1' +
          '8" cy="19" r="3"/>'#13#10'        <path d="M3,19c0,1.7,1.3,3,3,3s3-1.3' +
          ',3-3s-1.3-3-3-3L3,19z"/>'#13#10'    </g>'#13#10'    <g fill="#FFC107">'#13#10'    ' +
          '    <polygon points="32,11 27,11 27,19 33,19"/>'#13#10'        <polygo' +
          'n points="42,11 37,11 39,19 45,19"/>'#13#10'        <polygon points="1' +
          '6,11 21,11 21,19 15,19"/>'#13#10'        <polygon points="6,11 11,11 9' +
          ',19 3,19"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'sim_card'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#009' +
          '688" d="M36,45H12c-2.2,0-4-1.8-4-4V7c0-2.2,1.8-4,4-4h16.3c1.1,0,' +
          '2.1,0.4,2.8,1.2l7.7,7.7c0.8,0.8,1.2,1.8,1.2,2.8 V41C40,43.2,38.2' +
          ',45,36,45z"/>'#13#10'    <path fill="#FF9800" d="M32,38H16c-1.1,0-2-0.' +
          '9-2-2V24c0-1.1,0.9-2,2-2h16c1.1,0,2,0.9,2,2v12C34,37.1,33.1,38,3' +
          '2,38z"/>'#13#10'    <path fill="#FFD54F" d="M29,30v3h5v2h-5v3h-2V22h2v' +
          '6h5v2H29z M14,29v2h5v2h-5v2h5v3h2v-9H14z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'sim_card_chip'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FF9' +
          '800" d="M5,35V13c0-2.2,1.8-4,4-4h30c2.2,0,4,1.8,4,4v22c0,2.2-1.8' +
          ',4-4,4H9C6.8,39,5,37.2,5,35z"/>'#13#10'    <g fill="#FFD54F">'#13#10'       ' +
          ' <path d="M43,21v-2H31c-1.1,0-2-0.9-2-2s0.9-2,2-2h1v-2h-1c-2.2,0' +
          '-4,1.8-4,4s1.8,4,4,4h3v6h-3c-2.8,0-5,2.2-5,5 s2.2,5,5,5h2v-2h-2c' +
          '-1.7,0-3-1.3-3-3s1.3-3,3-3h12v-2h-7v-6H43z"/>'#13#10'        <path d="' +
          'M17,27h-3v-6h3c2.2,0,4-1.8,4-4s-1.8-4-4-4h-3v2h3c1.1,0,2,0.9,2,2' +
          's-0.9,2-2,2H5v2h7v6H5v2h12 c1.7,0,3,1.3,3,3s-1.3,3-3,3h-2v2h2c2.' +
          '8,0,5-2.2,5-5S19.8,27,17,27z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'slr_back_side'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#5E3' +
          '5B1" d="M40,10h-7.6l-2-3c-0.4-0.6-1-0.9-1.7-0.9h-9.6c-0.7,0-1.3,' +
          '0.3-1.7,0.9l-2,3H8c-2.2,0-4,1.8-4,4v24 c0,2.2,1.8,4,4,4h32c2.2,0' +
          ',4-1.8,4-4V14C44,11.8,42.2,10,40,10z"/>'#13#10'    <path fill="#F57C00' +
          '" d="M11,16h20c0.6,0,1,0.4,1,1v16c0,0.6-0.4,1-1,1H11c-0.6,0-1-0.' +
          '4-1-1V17C10,16.4,10.4,16,11,16z"/>'#13#10'    <polygon fill="#942A09" ' +
          'points="18.9,22 12,32 25.8,32"/>'#13#10'    <circle fill="#FFF9C4" cx=' +
          '"27" cy="21" r="2"/>'#13#10'    <polygon fill="#BF360C" points="25.2,2' +
          '6 20.4,32 30,32"/>'#13#10'    <g fill="#8667C4">'#13#10'        <path d="M34' +
          ',10h6V9.2C40,8.5,39.5,8,38.8,8h-3.6C34.5,8,34,8.5,34,9.2V10z"/>'#13 +
          #10'        <circle cx="38" cy="18" r="2"/>'#13#10'        <circle cx="38' +
          '" cy="24" r="2"/>'#13#10'        <circle cx="38" cy="30" r="2"/>'#13#10'    ' +
          '</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'smartphone_tablet'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#374' +
          '74F" d="M4,39V7c0-2.2,1.8-4,4-4h22c2.2,0,4,1.8,4,4v32c0,2.2-1.8,' +
          '4-4,4H8C5.8,43,4,41.2,4,39z"/>'#13#10'    <path fill="#BBDEFB" d="M30,' +
          '6H8C7.4,6,7,6.4,7,7v29c0,0.6,0.4,1,1,1h22c0.6,0,1-0.4,1-1V7C31,6' +
          '.4,30.6,6,30,6z"/>'#13#10'    <rect x="15" y="39" fill="#78909C" width' +
          '="6" height="2"/>'#13#10'    <path fill="#E38939" d="M24,41V17c0-2.2,1' +
          '.8-4,4-4h12c2.2,0,4,1.8,4,4v24c0,2.2-1.8,4-4,4H28C25.8,45,24,43.' +
          '2,24,41z"/>'#13#10'    <path fill="#FFF3E0" d="M40,16H28c-0.6,0-1,0.4-' +
          '1,1v22c0,0.6,0.4,1,1,1h12c0.6,0,1-0.4,1-1V17C41,16.4,40.6,16,40,' +
          '16z"/>'#13#10'    <circle fill="#A6642A" cx="34" cy="42.5" r="1.5"/>'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = 'sms'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#009' +
          '688" d="M37,39H11l-6,6V11c0-3.3,2.7-6,6-6h26c3.3,0,6,2.7,6,6v22C' +
          '43,36.3,40.3,39,37,39z"/>'#13#10'    <g fill="#fff">'#13#10'        <circle ' +
          'cx="24" cy="22" r="3"/>'#13#10'        <circle cx="34" cy="22" r="3"/>' +
          #13#10'        <circle cx="14" cy="22" r="3"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'sound_recording_copyright'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#9' +
          'C27B0" cx="24" cy="24" r="21"/>'#13#10'    <path fill="#E1BEE7" d="M20' +
          '.7,27.2v8.4h-3.9V12.9h8.7c1.3,0,2.5,0.2,3.5,0.5c1,0.4,1.9,0.9,2.' +
          '6,1.5c0.7,0.6,1.2,1.4,1.6,2.3 c0.4,0.9,0.6,1.8,0.6,2.9c0,1.1-0.2' +
          ',2.1-0.6,3c-0.4,0.9-0.9,1.6-1.6,2.2c-0.7,0.6-1.6,1.1-2.6,1.4c-1,' +
          '0.3-2.2,0.5-3.5,0.5H20.7z M20.7,24h4.7c0.8,0,1.4-0.1,2-0.3c0.5-0' +
          '.2,1-0.5,1.4-0.8c0.4-0.3,0.6-0.8,0.8-1.2c0.2-0.5,0.2-1,0.2-1.6c0' +
          '-0.5-0.1-1-0.2-1.5 c-0.2-0.5-0.4-0.9-0.8-1.3c-0.4-0.4-0.8-0.7-1.' +
          '4-0.9c-0.5-0.2-1.2-0.3-2-0.3h-4.7V24z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'speaker'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#81D' +
          '4FA" d="M28,7.1v2c7.3,1,13,7.3,13,14.9s-5.7,13.9-13,14.9v2c8.4-1' +
          ',15-8.2,15-16.9S36.4,8.1,28,7.1z"/>'#13#10'    <path fill="#546E7A" d=' +
          '"M14,32H7c-1.1,0-2-0.9-2-2V18c0-1.1,0.9-2,2-2h7V32z"/>'#13#10'    <pol' +
          'ygon fill="#78909C" points="26,42 14,32 14,16 26,6"/>'#13#10'    <path' +
          ' fill="#03A9F4" d="M28,17.3v2.1c1.8,0.8,3,2.5,3,4.6s-1.2,3.8-3,4' +
          '.6v2.1c2.9-0.9,5-3.5,5-6.7S30.9,18.2,28,17.3z"/>'#13#10'    <path fill' +
          '="#4FC3F7" d="M28,12.2v2c4.6,0.9,8,5,8,9.8s-3.4,8.9-8,9.8v2c5.7-' +
          '1,10-5.9,10-11.8S33.7,13.1,28,12.2z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'sports_mode'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#F' +
          'F9800" cx="28" cy="9" r="5"/>'#13#10'    <path fill="#00796B" d="M29,2' +
          '7.3l-9.2-4.1c-1-0.5-1.5,1-2,2c-0.5,1-4.1,7.2-3.8,8.3c0.3,0.9,1.1' +
          ',1.4,1.9,1.4c0.2,0,0.4,0,0.6-0.1 L28.8,31c0.8-0.2,1.4-1,1.4-1.8C' +
          '30.2,28.4,29.7,27.6,29,27.3z"/>'#13#10'    <path fill="#009688" d="M26' +
          '.8,15.2l-2.2-1c-1.3-0.6-2.9,0-3.5,1.3L9.2,41.1c-0.5,1,0,2.2,1,2.' +
          '7c0.3,0.1,0.6,0.2,0.9,0.2 c0.8,0,1.5-0.4,1.8-1.1c0,0,9.6-13.3,10' +
          '.4-14.9s4.9-9.3,4.9-9.3C28.7,17.4,28.2,15.8,26.8,15.2z"/>'#13#10'    <' +
          'path fill="#FF9800" d="M40.5,15.7c-0.7-0.8-2-1-2.8-0.3l-5,4.2l-6' +
          '.4-3.5c-1.1-0.6-2.6-0.4-3.3,0.9c-0.8,1.3-0.4,2.9,0.8,3.4 l8.3,3.' +
          '4c0.3,0.1,0.6,0.2,0.9,0.2c0.5,0,0.9-0.2,1.3-0.5l6-5C41.1,17.8,41' +
          '.2,16.6,40.5,15.7z"/>'#13#10'    <path fill="#FF9800" d="M11.7,23.1l3.' +
          '4-5.1l4.6,0.6l1.5-3.1c0.4-0.9,1.2-1.4,2.1-1.5c-0.1,0-0.2,0-0.2,0' +
          'h-9c-0.7,0-1.3,0.3-1.7,0.9 l-4,6c-0.6,0.9-0.4,2.2,0.6,2.8C9.2,23' +
          '.9,9.6,24,10,24C10.6,24,11.3,23.7,11.7,23.1z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'stack_of_photos'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="12.3" y' +
          '="12.3" transform="matrix(.948 .318 -.318 .948 9.725 -6.994)" fi' +
          'll="#64B5F6" width="28" height="28"/>'#13#10'    <rect x="15.6" y="15.' +
          '4" transform="matrix(.951 .31 -.31 .951 9.176 -6.977)" fill="#1E' +
          '88E5" width="22" height="20"/>'#13#10'    <rect x="8.1" y="8.1" transf' +
          'orm="matrix(.983 .181 -.181 .983 4.385 -3.65)" fill="#90CAF9" wi' +
          'dth="28" height="28"/>'#13#10'    <rect x="11.3" y="11.2" transform="m' +
          'atrix(.985 .175 -.175 .985 4.048 -3.566)" fill="#42A5F5" width="' +
          '22" height="20"/>'#13#10'    <rect x="4" y="4" fill="#BBDEFB" width="2' +
          '8" height="28"/>'#13#10'    <rect x="7" y="7" fill="#4CAF50" width="22' +
          '" height="20"/>'#13#10'    <path fill="#fff" d="M16,13c0-1.1,0.9-2,2-2' +
          's2,0.9,2,2s-2,4-2,4S16,14.1,16,13z"/>'#13#10'    <path fill="#fff" d="' +
          'M20,21c0,1.1-0.9,2-2,2s-2-0.9-2-2s2-4,2-4S20,19.9,20,21z"/>'#13#10'   ' +
          ' <path fill="#fff" d="M13.5,16.7c-1-0.6-1.3-1.8-0.7-2.7c0.6-1,1.' +
          '8-1.3,2.7-0.7c1,0.6,2.5,3.7,2.5,3.7S14.5,17.3,13.5,16.7z"/>'#13#10'   ' +
          ' <path fill="#fff" d="M22.5,17.3c1,0.6,1.3,1.8,0.7,2.7c-0.6,1-1.' +
          '8,1.3-2.7,0.7C19.5,20.2,18,17,18,17S21.5,16.7,22.5,17.3z"/>'#13#10'   ' +
          ' <path fill="#fff" d="M22.5,16.7c1-0.6,1.3-1.8,0.7-2.7c-0.6-1-1.' +
          '8-1.3-2.7-0.7C19.5,13.8,18,17,18,17S21.5,17.3,22.5,16.7z"/>'#13#10'   ' +
          ' <path fill="#fff" d="M13.5,17.3c-1,0.6-1.3,1.8-0.7,2.7c0.6,1,1.' +
          '8,1.3,2.7,0.7c1-0.6,2.5-3.7,2.5-3.7S14.5,16.7,13.5,17.3z"/>'#13#10'   ' +
          ' <circle fill="#FFC107" cx="18" cy="17" r="2"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'start'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#F44' +
          '336" d="M38,42H10c-2.2,0-4-1.8-4-4V10c0-2.2,1.8-4,4-4h28c2.2,0,4' +
          ',1.8,4,4v28C42,40.2,40.2,42,38,42z"/>'#13#10'    <polygon fill="#fff" ' +
          'points="31,24 20,16 20,32"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'steam'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 15.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://w' +
          'ww.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#13#10'<svg version="1.1"  ' +
          'xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.or' +
          'g/1999/xlink" x="0px" y="0px"'#13#10#9' width="48px" height="48px" view' +
          'Box="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pre' +
          'serve">'#13#10'<path fill="#FFFFFF" d="M42,38c0,2.209-1.791,4-4,4H10c-' +
          '2.209,0-4-1.791-4-4V10c0-2.209,1.791-4,4-4h28c2.209,0,4,1.791,4,' +
          '4V38z"/>'#13#10'<g>'#13#10#9'<path fill="#455A64" d="M18.459,33.645c-0.288,0-' +
          '0.56-0.057-0.822-0.141l-0.005,0.02l-3.67-1.062'#13#10#9#9'c0.644,1.878,2' +
          '.406,3.237,4.5,3.237c2.641,0,4.776-2.136,4.776-4.776s-2.135-4.77' +
          '7-4.776-4.777c-1.141,0-2.175,0.418-2.998,1.087'#13#10#9#9'L19,28.255c0.0' +
          '29,0.007,0.055,0.018,0.084,0.024l0.049,0.016v0.002c1.177,0.301,2' +
          '.049,1.359,2.049,2.626'#13#10#9#9'C21.184,32.424,19.964,33.645,18.459,33' +
          '.645z"/>'#13#10#9'<path fill="#455A64" d="M30.923,24.299c3.222,0,5.837-' +
          '2.615,5.837-5.838c0-3.222-2.615-5.837-5.837-5.837'#13#10#9#9'c-3.221,0-5' +
          '.837,2.615-5.837,5.837C25.086,21.684,27.702,24.299,30.923,24.299' +
          'z M30.92,14.409c2.24,0,4.056,1.813,4.056,4.052'#13#10#9#9'c0,2.241-1.815' +
          ',4.053-4.056,4.053c-2.236,0-4.049-1.812-4.049-4.053C26.871,16.22' +
          '3,28.684,14.409,30.92,14.409z"/>'#13#10#9'<path fill="#455A64" d="M38,6' +
          'h-2.75h-22.5H10c-2.209,0-4,1.791-4,4v2.75v4.236v7.509l7.027,2.03' +
          '3'#13#10#9#9'c1.287-1.59,3.229-2.626,5.434-2.626c0.07,0,0.135,0.02,0.204' +
          ',0.021l3.876-5.355c0-0.035-0.005-0.072-0.005-0.105'#13#10#9#9'c0-4.63,3.' +
          '755-8.388,8.387-8.388c4.633,0,8.386,3.757,8.386,8.386c0,4.633-3.' +
          '753,8.387-8.386,8.387'#13#10#9#9'c-0.044,0-0.087-0.006-0.132-0.007l-5.33' +
          ',3.871c0.002,0.07,0.021,0.14,0.021,0.211c0,3.878-3.142,7.021-7.0' +
          '21,7.021'#13#10#9#9'c-3.593,0-6.52-2.707-6.937-6.188L6,30.158v2.583v2.50' +
          '9V38c0,2.209,1.791,4,4,4h2.75h22.5H38c2.209,0,4-1.791,4-4v-2.75v' +
          '-22.5V10'#13#10#9#9'C42,7.791,40.209,6,38,6z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'stumbleupon'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 15.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://w' +
          'ww.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#13#10'<svg version="1.1"  ' +
          'xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.or' +
          'g/1999/xlink" x="0px" y="0px"'#13#10#9' width="48px" height="48px" view' +
          'Box="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pre' +
          'serve">'#13#10'<path fill="#E64A19" d="M24.001,5c-10.494,0-19,8.506-19' +
          ',19c0,10.493,8.506,19,19,19c10.493,0,19-8.507,19-19'#13#10#9'C43.001,13' +
          '.506,34.494,5,24.001,5z"/>'#13#10'<g>'#13#10#9'<path fill="#FFFFFF" d="M24.00' +
          '1,19C23.998,19,24.004,19,24.001,19c-0.062-0.004-1,0-1,1v7.876C22' +
          '.916,29.888,21.504,33,17.959,33'#13#10#9#9'c-3.607,0-4.958-3.065-4.958-4' +
          '.958V24h4v4c0.038,0.709,0.629,1,1,1c0.665,0,0.972-0.361,1-1v-8.1' +
          '24c0-2.01,1.332-5,5-5'#13#10#9#9'c0.045,0,0.086,0.006,0.131,0.007c0,0,4.' +
          '869-0.009,4.869,5.117c0,1.104-0.896,1.876-2,1.876s-2-0.771-2-1.8' +
          '76'#13#10#9#9'C25.001,19.124,24.041,19.003,24.001,19z M35.001,27.876c0,2' +
          '.01-1.331,5.124-5,5.124s-5-3.114-5-5.124v-3.439'#13#10#9#9'c0.614,0.272,' +
          '1.285,0.439,2,0.439c0.712,0,1.386-0.154,2-0.424V28c0.038,1,0.663' +
          ',1,1,1c0.247,0,1,0,1-1v-4h4V27.876z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'support'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#607' +
          'D8B" d="M44.7,11L36,19.6c0,0-2.6,0-5.2-2.6s-2.6-5.2-2.6-5.2l8.7-' +
          '8.7c-4.9-1.2-10.8,0.4-14.4,4 c-5.4,5.4-0.6,12.3-2,13.7C12.9,28.7' +
          ',5.1,34.7,4.9,35c-2.3,2.3-2.4,6-0.2,8.2c2.2,2.2,5.9,2.1,8.2-0.2c' +
          '0.3-0.3,6.7-8.4,14.2-15.9 c1.4-1.4,8,3.7,13.6-1.8C44.2,21.7,45.9' +
          ',15.9,44.7,11z M9.4,41.1c-1.4,0-2.5-1.1-2.5-2.5C6.9,37.1,8,36,9.' +
          '4,36 c1.4,0,2.5,1.1,2.5,2.5C11.9,39.9,10.8,41.1,9.4,41.1z"/>'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'survey'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#455' +
          'A64" d="M36,4H26c0,1.1-0.9,2-2,2s-2-0.9-2-2H12C9.8,4,8,5.8,8,8v3' +
          '2c0,2.2,1.8,4,4,4h24c2.2,0,4-1.8,4-4V8 C40,5.8,38.2,4,36,4z"/>'#13#10 +
          '    <path fill="#fff" d="M36,41H12c-0.6,0-1-0.4-1-1V8c0-0.6,0.4-' +
          '1,1-1h24c0.6,0,1,0.4,1,1v32C37,40.6,36.6,41,36,41z"/>'#13#10'    <g fi' +
          'll="#90A4AE">'#13#10'        <path d="M26,4c0,1.1-0.9,2-2,2s-2-0.9-2-2' +
          'h-7v4c0,1.1,0.9,2,2,2h14c1.1,0,2-0.9,2-2V4H26z"/>'#13#10'        <path' +
          ' d="M24,0c-2.2,0-4,1.8-4,4s1.8,4,4,4s4-1.8,4-4S26.2,0,24,0z M24,' +
          '6c-1.1,0-2-0.9-2-2s0.9-2,2-2s2,0.9,2,2 S25.1,6,24,6z"/>'#13#10'    </g' +
          '>'#13#10'    <g fill="#CFD8DC">'#13#10'        <rect x="21" y="20" width="12' +
          '" height="2"/>'#13#10'        <rect x="15" y="19" width="4" height="4"' +
          '/>'#13#10'    </g>'#13#10'    <g fill="#03A9F4">'#13#10'        <rect x="21" y="29' +
          '" width="12" height="2"/>'#13#10'        <rect x="15" y="28" width="4"' +
          ' height="4"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'switch_camera'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#5E35B1' +
          '">'#13#10'        <path d="M33.9,12.1H14.2L17.6,7c0.4-0.6,1-0.9,1.7-0.' +
          '9h9.6c0.7,0,1.3,0.3,1.7,0.9L33.9,12.1z"/>'#13#10'        <path d="M14,' +
          '11H8V9.2C8,8.5,8.5,8,9.2,8h3.6C13.5,8,14,8.5,14,9.2V11z"/>'#13#10'    ' +
          '    <path d="M40,42H8c-2.2,0-4-1.8-4-4V14c0-2.2,1.8-4,4-4h32c2.2' +
          ',0,4,1.8,4,4v24C44,40.2,42.2,42,40,42z"/>'#13#10'    </g>'#13#10'    <path f' +
          'ill="#E8EAF6" d="M34,25c0-5.5-4.5-10-10-10c-2.4,0-4.6,0.8-6.3,2.' +
          '2l1.2,1.6c1.4-1.1,3.1-1.8,5.1-1.8c4.4,0,8,3.6,8,8h-3.5 l4.5,5.6l' +
          '4.5-5.6H34z"/>'#13#10'    <path fill="#E8EAF6" d="M29.1,31.2C27.7,32.3' +
          ',25.9,33,24,33c-4.4,0-8-3.6-8-8h3.5L15,19.4L10.5,25H14c0,5.5,4.5' +
          ',10,10,10 c2.4,0,4.6-0.8,6.3-2.2L29.1,31.2z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'synchronize'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#FF6' +
          'F00" d="M38.7,11.9l-3.1,2.5c2.2,2.7,3.4,6.1,3.4,9.5c0,8.3-6.7,15' +
          '-15,15c-0.9,0-1.9-0.1-2.8-0.3l-0.7,3.9 c1.2,0.2,2.4,0.3,3.5,0.3c' +
          '10.5,0,19-8.5,19-19C43,19.6,41.5,15.3,38.7,11.9z"/>'#13#10'    <polygo' +
          'n fill="#FF6F02" points="31,8 42.9,9.6 33.1,19.4"/>'#13#10'    <path f' +
          'ill="#FF6F00" d="M24,5C13.5,5,5,13.5,5,24c0,4.6,1.6,9,4.6,12.4l3' +
          '-2.6C10.3,31.1,9,27.6,9,24c0-8.3,6.7-15,15-15 c0.9,0,1.9,0.1,2.8' +
          ',0.3l0.7-3.9C26.4,5.1,25.2,5,24,5z"/>'#13#10'    <polygon fill="#FF6F0' +
          '2" points="17,40 5.1,38.4 14.9,28.6"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'tablet_android'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#374' +
          '74F" d="M8,41V7c0-2.2,1.8-4,4-4h24c2.2,0,4,1.8,4,4v34c0,2.2-1.8,' +
          '4-4,4H12C9.8,45,8,43.2,8,41z"/>'#13#10'    <path fill="#BBDEFB" d="M36' +
          ',6H12c-0.6,0-1,0.4-1,1v31c0,0.6,0.4,1,1,1h24c0.6,0,1-0.4,1-1V7C3' +
          '7,6.4,36.6,6,36,6z"/>'#13#10'    <rect x="21" y="41" fill="#78909C" wi' +
          'dth="6" height="2"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'template'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="4" y="7' +
          '" fill="#BBDEFB" width="40" height="34"/>'#13#10'    <rect x="9" y="12' +
          '" fill="#3F51B5" width="30" height="5"/>'#13#10'    <g fill="#2196F3">' +
          #13#10'        <rect x="9" y="21" width="13" height="16"/>'#13#10'        <' +
          'rect x="26" y="21" width="13" height="16"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'timeline'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#3F5' +
          '1B5" d="M42,29H20.8c-0.5,0-1-0.2-1.4-0.6l-3.7-3.7c-0.4-0.4-0.4-1' +
          ',0-1.4l3.7-3.7c0.4-0.4,0.9-0.6,1.4-0.6H42 c0.6,0,1,0.4,1,1v8C43,' +
          '28.6,42.6,29,42,29z"/>'#13#10'    <rect x="9" y="6" fill="#CFD8DC" wid' +
          'th="2" height="36"/>'#13#10'    <g fill="#90A4AE">'#13#10'        <circle cx' +
          '="10" cy="10" r="3"/>'#13#10'        <circle cx="10" cy="24" r="3"/>'#13#10 +
          '        <circle cx="10" cy="38" r="3"/>'#13#10'    </g>'#13#10'    <path fil' +
          'l="#448AFF" d="M34,43H20.8c-0.5,0-1-0.2-1.4-0.6l-3.7-3.7c-0.4-0.' +
          '4-0.4-1,0-1.4l3.7-3.7c0.4-0.4,0.9-0.6,1.4-0.6H34 c0.6,0,1,0.4,1,' +
          '1v8C35,42.6,34.6,43,34,43z"/>'#13#10'    <path fill="#00BCD4" d="M35,1' +
          '5H20.8c-0.5,0-1-0.2-1.4-0.6l-3.7-3.7c-0.4-0.4-0.4-1,0-1.4l3.7-3.' +
          '7C19.8,5.2,20.3,5,20.8,5H35 c0.6,0,1,0.4,1,1v8C36,14.6,35.6,15,3' +
          '5,15z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'todo_list'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#3F51B5' +
          '">'#13#10'        <polygon points="17.8,18.1 10.4,25.4 6.2,21.3 4,23.5' +
          ' 10.4,29.9 20,20.3"/>'#13#10'        <polygon points="17.8,5.1 10.4,12' +
          '.4 6.2,8.3 4,10.5 10.4,16.9 20,7.3"/>'#13#10'        <polygon points="' +
          '17.8,31.1 10.4,38.4 6.2,34.3 4,36.5 10.4,42.9 20,33.3"/>'#13#10'    </' +
          'g>'#13#10'    <g fill="#90CAF9">'#13#10'        <rect x="24" y="22" width="2' +
          '0" height="4"/>'#13#10'        <rect x="24" y="9" width="20" height="4' +
          '"/>'#13#10'        <rect x="24" y="35" width="20" height="4"/>'#13#10'    </' +
          'g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'touchscreen_smartphone'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#E38' +
          '939" d="M12,40V8c0-2.2,1.8-4,4-4h16c2.2,0,4,1.8,4,4v32c0,2.2-1.8' +
          ',4-4,4H16C13.8,44,12,42.2,12,40z"/>'#13#10'    <path fill="#FFF3E0" d=' +
          '"M32,7H16c-0.6,0-1,0.4-1,1v29c0,0.6,0.4,1,1,1h16c0.6,0,1-0.4,1-1' +
          'V8C33,7.4,32.6,7,32,7z"/>'#13#10'    <circle fill="#A6642A" cx="24" cy' +
          '="41" r="1.5"/>'#13#10'    <circle fill="#E91E63" cx="24" cy="23" r="2' +
          '"/>'#13#10'    <circle fill="none" stroke="#F48FB1" stroke-width="2" s' +
          'troke-miterlimit="10" cx="24" cy="23" r="4"/>'#13#10'    <circle fill=' +
          '"none" stroke="#F8BBD0" stroke-miterlimit="10" cx="24" cy="23" r' +
          '="6.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'trademark'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <circle fill="#9' +
          'C27B0" cx="24" cy="24" r="21"/>'#13#10'    <g fill="#E1BEE7">'#13#10'       ' +
          ' <path d="M20.6,18.5h-4.2v14.2h-3.5V18.5H8.7v-2.9h11.9V18.5z"/>'#13 +
          #10'        <path d="M27.1,15.6L30.3,28l3.2-12.4h4.5v17.1h-3.5v-4.6' +
          'l0.3-7.1l-3.4,11.8h-2.4L25.7,21l0.3,7.1v4.6h-3.5V15.6 H27.1z"/>'#13 +
          #10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'tree_structure'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '90CAF9" points="36.9,13.8 35.1,10.2 7.5,24 35.1,37.8 36.9,34.2 1' +
          '6.5,24"/>'#13#10'    <rect x="6" y="18" fill="#D81B60" width="12" heig' +
          'ht="12"/>'#13#10'    <g fill="#2196F3">'#13#10'        <rect x="30" y="6" wi' +
          'dth="12" height="12"/>'#13#10'        <rect x="30" y="30" width="12" h' +
          'eight="12"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'two_smartphones'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#374' +
          '74F" d="M6,36V8c0-2.2,1.8-4,4-4h14c2.2,0,4,1.8,4,4v28c0,2.2-1.8,' +
          '4-4,4H10C7.8,40,6,38.2,6,36z"/>'#13#10'    <path fill="#BBDEFB" d="M24' +
          ',7H10C9.4,7,9,7.4,9,8v25c0,0.6,0.4,1,1,1h14c0.6,0,1-0.4,1-1V8C25' +
          ',7.4,24.6,7,24,7z"/>'#13#10'    <rect x="14" y="36" fill="#78909C" wid' +
          'th="6" height="2"/>'#13#10'    <path fill="#E38939" d="M20,40V12c0-2.2' +
          ',1.8-4,4-4h14c2.2,0,4,1.8,4,4v28c0,2.2-1.8,4-4,4H24C21.8,44,20,4' +
          '2.2,20,40z"/>'#13#10'    <path fill="#FFF3E0" d="M38,11H24c-0.6,0-1,0.' +
          '4-1,1v25c0,0.6,0.4,1,1,1h14c0.6,0,1-0.4,1-1V12C39,11.4,38.6,11,3' +
          '8,11z"/>'#13#10'    <circle fill="#A6642A" cx="31" cy="41" r="1.5"/>'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = 'undo'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#00BCD4' +
          '">'#13#10'        <polygon points="5,18 19,6.3 19,29.7"/>'#13#10'        <pa' +
          'th d="M28,14H16v8h12c2.8,0,5,2.2,5,5s-2.2,5-5,5h-3v8h3c7.2,0,13-' +
          '5.8,13-13S35.2,14,28,14z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'unlock'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#424' +
          '242" d="M24,4c-5.5,0-10,4.5-10,10v4h4v-4c0-3.3,2.7-6,6-6s6,2.7,6' +
          ',6h4C34,8.5,29.5,4,24,4z"/>'#13#10'    <path fill="#FB8C00" d="M36,44H' +
          '12c-2.2,0-4-1.8-4-4V22c0-2.2,1.8-4,4-4h24c2.2,0,4,1.8,4,4v18C40,' +
          '42.2,38.2,44,36,44z"/>'#13#10'    <circle fill="#C76E00" cx="24" cy="3' +
          '1" r="3"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'up'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#3F51B5' +
          '">'#13#10'        <polygon points="24,4 35.7,18 12.3,18"/>'#13#10'        <r' +
          'ect x="20" y="15" width="8" height="27"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'up_left'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '3F51B5" points="4,19 18,30.7 18,7.3"/>'#13#10'    <path fill="#3F51B5"' +
          ' d="M42,27v13h-8V27c0-2.2-1.8-4-4-4H13v-8h17C36.6,15,42,20.4,42,' +
          '27z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'up_right'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '3F51B5" points="44,19 30,30.7 30,7.3"/>'#13#10'    <path fill="#3F51B5' +
          '" d="M6,27v13h8V27c0-2.2,1.8-4,4-4h17v-8H18C11.4,15,6,20.4,6,27z' +
          '"/>'#13#10'    <polygon fill="#3F51B5" points="44,19 30,30.7 30,7.3"/>' +
          #13#10'    <path fill="#3F51B5" d="M6,27v13h8V27c0-2.2,1.8-4,4-4h17v-' +
          '8H18C11.4,15,6,20.4,6,27z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'upload'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#009688' +
          '">'#13#10'        <polygon points="24,10.9 35,24 13,24"/>'#13#10'        <re' +
          'ct x="20" y="40" width="8" height="4"/>'#13#10'        <rect x="20" y=' +
          '"34" width="8" height="4"/>'#13#10'        <rect x="20" y="21" width="' +
          '8" height="11"/>'#13#10'        <rect x="6" y="4" width="36" height="4' +
          '"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'usb'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 15.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://w' +
          'ww.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#13#10'<svg version="1.1"  ' +
          'xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.or' +
          'g/1999/xlink" x="0px" y="0px"'#13#10#9' width="48px" height="48px" view' +
          'Box="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pre' +
          'serve">'#13#10'<path fill="#1565C0" d="M38.701,24.355h-2.189l-0.467,2.' +
          '265c0,0,2.277,0,2.51,0c0.233,0,1.545-0.167,1.545-1.267'#13#10#9'C40.1,2' +
          '4.266,38.701,24.355,38.701,24.355z M39.521,20.339h-2.15l-0.374,1' +
          '.796c0,0,2.161,0,2.337,0c0.188,0,1.113-0.146,1.113-1.006'#13#10#9'C40.4' +
          '47,20.271,39.521,20.339,39.521,20.339z M44.064,23.109c0,0,1.436-' +
          '0.743,1.436-3.093c0-3.715-4.377-3.516-4.377-3.516h-2.865'#13#10#9'l0.67' +
          '4-3c0,0-12.4,0-20.971,0c-9.344,0-12.158,6.774-12.158,6.774L5.736' +
          ',20.5H2.547l-1.047,6h3.37l0.001,0.143'#13#10#9'c0,0-0.285,6.857,10.463,' +
          '6.857c10.747,0,19.042,0,19.042,0l0.679-3c0.84,0,2.2,0,4.389,0c4.' +
          '729,0,5.591-3.354,5.591-4.9'#13#10#9'C45.032,23.838,44.064,23.109,44.06' +
          '4,23.109z"/>'#13#10'<path fill="#FFFFFF" d="M38.701,24.355h-2.189l-0.4' +
          '67,2.265c0,0,2.277,0,2.51,0c0.233,0,1.545-0.167,1.545-1.267'#13#10#9'C4' +
          '0.1,24.266,38.701,24.355,38.701,24.355z M39.521,20.339h-2.15l-0.' +
          '374,1.796c0,0,2.161,0,2.337,0c0.188,0,1.113-0.146,1.113-1.006'#13#10#9 +
          'C40.447,20.271,39.521,20.339,39.521,20.339z M14.022,29.5c-5.306,' +
          '0-5.306-3.624-5.238-3.986c0.069-0.363,1.789-8.014,1.789-8.014'#13#10#9 +
          'h3.84l-1.358,6.354c0,0-0.971,2.728,1.251,2.728c2.081,0,2.336-2.5' +
          '35,2.336-2.535l1.465-6.543h3.839l-1.582,6.979'#13#10#9'C20.365,24.48,20' +
          '.258,29.5,14.022,29.5z M26.098,29.521c-2.674,0-4.958-1.262-4.856' +
          '-4.14h3.438c0,0.576,0.086,1.627,1.633,1.627'#13#10#9'c0.627,0,1.688-0.2' +
          '66,1.688-1.133c0-1.631-5.597-0.785-5.597-4.57c0-2.063,1.899-3.78' +
          '5,4.989-3.785c4.976,0,4.613,3.749,4.613,3.749'#13#10#9'h-3.369c0-1.044-' +
          '0.664-1.204-1.463-1.204c-0.8,0-1.372,0.343-1.372,0.944c0,1.471,5' +
          '.634,0.456,5.634,4.531'#13#10#9'C31.436,27.305,30.012,29.521,26.098,29.' +
          '521z M39.366,29.5c-0.419,0-7.515,0-7.515,0l2.601-12c0,0,5.444,0,' +
          '6.556,0'#13#10#9'c1.113,0,3.43,0.234,3.43,2.542c0,2.602-2.227,3.013-2.2' +
          '27,3.013s1.764,0.407,1.764,2.473C43.975,29.457,39.775,29.5,39.36' +
          '6,29.5z"'#13#10#9'/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'video_call'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#4CA' +
          'F50" d="M8,12h22c2.2,0,4,1.8,4,4v16c0,2.2-1.8,4-4,4H8c-2.2,0-4-1' +
          '.8-4-4V16C4,13.8,5.8,12,8,12z"/>'#13#10'    <polygon fill="#388E3C" po' +
          'ints="44,35 34,29 34,19 44,13"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'video_file'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <polygon fill="#' +
          '90CAF9" points="40,45 8,45 8,3 30,3 40,13"/>'#13#10'    <polygon fill=' +
          '"#E1F5FE" points="38.5,14 29,14 29,4.5"/>'#13#10'    <polygon fill="#1' +
          '976D2" points="30,28 20,22 20,34"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'video_projector'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <g fill="#546E7A' +
          '">'#13#10'        <rect x="5" y="34" width="6" height="3"/>'#13#10'        <' +
          'rect x="37" y="34" width="6" height="3"/>'#13#10'    </g>'#13#10'    <path f' +
          'ill="#78909C" d="M44,35H4c-2.2,0-4-1.8-4-4V17c0-2.2,1.8-4,4-4h40' +
          'c2.2,0,4,1.8,4,4v14C48,33.2,46.2,35,44,35z"/>'#13#10'    <g fill="#374' +
          '74F">'#13#10'        <rect x="5" y="19" width="2" height="2"/>'#13#10'      ' +
          '  <rect x="5" y="23" width="2" height="2"/>'#13#10'        <rect x="5"' +
          ' y="27" width="2" height="2"/>'#13#10'        <rect x="9" y="19" width' +
          '="2" height="2"/>'#13#10'        <rect x="9" y="23" width="2" height="' +
          '2"/>'#13#10'        <rect x="9" y="27" width="2" height="2"/>'#13#10'       ' +
          ' <rect x="13" y="19" width="2" height="2"/>'#13#10'        <rect x="13' +
          '" y="23" width="2" height="2"/>'#13#10'        <rect x="13" y="27" wid' +
          'th="2" height="2"/>'#13#10'        <rect x="17" y="19" width="2" heigh' +
          't="2"/>'#13#10'        <rect x="17" y="23" width="2" height="2"/>'#13#10'   ' +
          '     <rect x="17" y="27" width="2" height="2"/>'#13#10'        <rect x' +
          '="21" y="19" width="2" height="2"/>'#13#10'        <rect x="21" y="23"' +
          ' width="2" height="2"/>'#13#10'        <rect x="21" y="27" width="2" h' +
          'eight="2"/>'#13#10'    </g>'#13#10'    <circle fill="#37474F" cx="37" cy="24' +
          '" r="8"/>'#13#10'    <circle fill="#a0f" cx="37" cy="24" r="6"/>'#13#10'    ' +
          '<path fill="#EA80FC" d="M40.7,21.7c-1-1.1-2.3-1.7-3.7-1.7s-2.8,0' +
          '.6-3.7,1.7c-0.4,0.4-0.3,1,0.1,1.4c0.4,0.4,1,0.3,1.4-0.1 c1.2-1.3' +
          ',3.3-1.3,4.5,0c0.2,0.2,0.5,0.3,0.7,0.3c0.2,0,0.5-0.1,0.7-0.3C41.' +
          '1,22.7,41.1,22.1,40.7,21.7z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'view_details'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="7" y="4' +
          '" fill="#BBDEFB" width="34" height="40"/>'#13#10'    <g fill="#2196F3"' +
          '>'#13#10'        <rect x="13" y="26" width="4" height="4"/>'#13#10'        <' +
          'rect x="13" y="18" width="4" height="4"/>'#13#10'        <rect x="13" ' +
          'y="34" width="4" height="4"/>'#13#10'        <rect x="13" y="10" width' +
          '="4" height="4"/>'#13#10'        <rect x="21" y="26" width="14" height' +
          '="4"/>'#13#10'        <rect x="21" y="18" width="14" height="4"/>'#13#10'   ' +
          '     <rect x="21" y="34" width="14" height="4"/>'#13#10'        <rect ' +
          'x="21" y="10" width="14" height="4"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'vip'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#880' +
          'E4F" d="M38,43H10c-2.2,0-4-1.8-4-4V11c0-2.2,1.8-4,4-4h28c2.2,0,4' +
          ',1.8,4,4v28C42,41.2,40.2,43,38,43z"/>'#13#10'    <g fill="#FFD54F">'#13#10' ' +
          '       <path d="M15.9,28l2.1-9.1h2.8l-3.6,12.6h-2.6L11,18.9h2.8L' +
          '15.9,28z"/>'#13#10'        <path d="M25.6,31.5h-2.5V18.9h2.5V31.5z"/>'#13 +
          #10'        <path d="M31.2,27.1v4.4h-2.5V18.9h4.3c3.7,0,4.1,3.4,4.1' +
          ',4.2c0,1.2-0.5,4-4.1,4H31.2z M31.2,24.9h1.7 c1.3,0,1.5-1.1,1.5-1' +
          '.9c0-1.6-0.9-2.1-1.5-2.1h-1.7V24.9z"/>'#13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'vlc'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 15.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://w' +
          'ww.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#13#10'<svg version="1.1"  ' +
          'xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.or' +
          'g/1999/xlink" x="0px" y="0px"'#13#10#9' width="48px" height="48px" view' +
          'Box="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pre' +
          'serve">'#13#10'<path fill="#F57C00" d="M36.258,28.837c0,0-0.11-0.837-1' +
          '.257-0.837c-0.216,0-2.392,0-3.719,0c0.798,2.671,1.497,5.135,1.49' +
          '7,5.279'#13#10#9'c0,2.387-3.401,3.393-8.917,3.393c-5.515,0-8.651-0.94-8' +
          '.651-3.326c0-0.167,0.998-2.692,1.791-5.346c-1.591,0-3.863,0-4.06' +
          '3,0'#13#10#9'c-0.806,0-0.937,0.749-0.937,0.749L8.159,40.986L8.815,42h30' +
          '.652l0.376-1.014L36.258,28.837z"/>'#13#10'<path fill="#E0E0E0" d="M24.' +
          '001,6c-1.029,0-1.864,0.179-1.864,0.398c-0.492,1.483-8.122,26.143' +
          '-8.122,26.774'#13#10#9'c0,2.388,4.471,3.827,9.985,3.827s9.986-1.439,9.9' +
          '86-3.827c0-0.549-7.614-25.268-8.122-26.774C25.865,6.179,25.031,6' +
          ',24.001,6'#13#10#9'L24.001,6z"/>'#13#10'<g>'#13#10#9'<path fill="#FF9800" d="M33.196' +
          ',30.447C32.032,32.232,28.341,34,24.046,34c-4.34,0-8.156-1.696-9.' +
          '281-3.51'#13#10#9#9'c-0.499,1.483-0.892,2.647-0.892,3.28c0,2.386,4.533,4' +
          '.229,10.128,4.229c5.595,0,10.131-1.844,10.131-4.229'#13#10#9#9'C34.132,3' +
          '3.222,33.713,31.955,33.196,30.447z"/>'#13#10#9'<path fill="#FF9800" d="' +
          'M31.387,24.314l-2.074-6.794c0,0-1.857,1.479-5.311,1.479c-3.453,0' +
          '-5.316-1.479-5.316-1.479l-2.081,6.806'#13#10#9#9'c0,0,2.068,2.674,7.397,' +
          '2.674C29.375,27,31.387,24.314,31.387,24.314z"/>'#13#10#9'<path fill="#F' +
          'F9800" d="M27.241,10.809l-1.376-4.41c0,0-0.083-0.398-1.864-0.398' +
          'c-1.844,0-1.864,0.398-1.864,0.398l-1.376,4.407'#13#10#9#9'c0,0,0.885,1.1' +
          '94,3.239,1.194C26.355,12,27.241,10.809,27.241,10.809z"/>'#13#10'</g>'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = 'voice_presentation'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#219' +
          '6F3" d="M40,22h-8l-4,4V12c0-2.2,1.8-4,4-4h8c2.2,0,4,1.8,4,4v6C44' +
          ',20.2,42.2,22,40,22z"/>'#13#10'    <circle fill="#FFA726" cx="17" cy="' +
          '19" r="8"/>'#13#10'    <path fill="#607D8B" d="M30,36.7c0,0-3.6-6.7-13' +
          '-6.7S4,36.7,4,36.7V40h26V36.7z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'voicemail'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#4CA' +
          'F50" d="M48,24c0-6.1-4.9-11-11-11s-11,4.9-11,11c0,2.7,0.9,5.1,2.' +
          '5,7h-9c1.6-1.9,2.5-4.3,2.5-7c0-6.1-4.9-11-11-11 S0,17.9,0,24s4.9' +
          ',11,11,11h27v-0.1C43.6,34.4,48,29.7,48,24z M4,24c0-3.9,3.1-7,7-7' +
          's7,3.1,7,7s-3.1,7-7,7S4,27.9,4,24z M37,31 c-3.9,0-7-3.1-7-7s3.1-' +
          '7,7-7c3.9,0,7,3.1,7,7S40.9,31,37,31z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'webcam'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <path fill="#455' +
          'A64" d="M36.5,44H11.5c-1.1,0-1.8-1.2-1.3-2.2L13,37h22l2.7,4.8C38' +
          '.3,42.8,37.6,44,36.5,44z"/>'#13#10'    <circle fill="#78909C" cx="24" ' +
          'cy="23" r="18"/>'#13#10'    <path fill="#455A64" d="M24,35c-6.6,0-12-5' +
          '.4-12-12c0-6.6,5.4-12,12-12s12,5.4,12,12C36,29.6,30.6,35,24,35z"' +
          '/>'#13#10'    <circle fill="#42A5F5" cx="24" cy="23" r="9"/>'#13#10'    <pat' +
          'h fill="#90CAF9" d="M28.8,20c-1.2-1.4-3-2.2-4.8-2.2s-3.6,0.8-4.8' +
          ',2.2c-0.5,0.5-0.4,1.3,0.1,1.8c0.5,0.5,1.3,0.4,1.8-0.1 c1.5-1.7,4' +
          '.3-1.7,5.8,0c0.3,0.3,0.6,0.4,1,0.4c0.3,0,0.6-0.1,0.9-0.3C29.2,21' +
          '.4,29.3,20.5,28.8,20z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'wi-fi_logo'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 15.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://w' +
          'ww.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#13#10'<svg version="1.1"  ' +
          'xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.or' +
          'g/1999/xlink" x="0px" y="0px"'#13#10#9' width="48px" height="48px" view' +
          'Box="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pre' +
          'serve">'#13#10'<g>'#13#10#9'<path fill="#3F51B5" d="M46,26.48c0,4.527-3.268,7' +
          '.52-7.3,7.52H9.299C5.269,34,2,30.634,2,26.48V21.52C2,17.366,5.26' +
          '9,14,9.299,14'#13#10#9#9'H38.7c4.032,0,7.3,3.366,7.3,7.52V26.48z"/>'#13#10#9'<e' +
          'llipse fill="#3F51B5" cx="24" cy="24" rx="14.902" ry="15"/>'#13#10'</g' +
          '>'#13#10'<g>'#13#10#9'<polygon fill="#FFFFFF" points="17,19 14.264,19 13.427,' +
          '24.859 12.388,19.028 9.93,19.028 8.864,24.859 8.054,19.028 5.266' +
          ',19.028 '#13#10#9#9'7.597,29 10.056,29 11.12,22.854 12.209,29 14.693,29 ' +
          #9'"/>'#13#10#9'<rect x="19" y="22" fill="#FFFFFF" width="2.508" height="' +
          '7"/>'#13#10#9'<path fill="#FFFFFF" d="M21.5,19.747C21.5,20.44,20.94,21,' +
          '20.25,21S19,20.44,19,19.747c0-0.696,0.56-1.258,1.25-1.258'#13#10#9#9'S21' +
          '.5,19.051,21.5,19.747z"/>'#13#10#9'<path fill="#FFFFFF" d="M38.561,16c-' +
          '4.818,0-7.979,0-7.979,0S25,16.193,25,21.914v4.336c0,0,0.101,2.94' +
          '1-3,5.75h16.785'#13#10#9#9'c0,0,5.215,0,5.215-5.553c0-4.879,0-4.879,0-4.' +
          '879S43.772,16,38.561,16z M37.339,21.369h-5.651v2.236h5.094v2.344' +
          'h-5.094V29H29V19'#13#10#9#9'h8.339V21.369z M40.25,18.489c0.689,0,1.25,0.' +
          '562,1.25,1.258C41.5,20.44,40.939,21,40.25,21S39,20.44,39,19.747'#13 +
          #10#9#9'C39,19.051,39.561,18.489,40.25,18.489z M41.508,29H39v-7h2.508' +
          'V29z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'wikipedia'
        SVGText = 
          '<?xml version="1.0" encoding="utf-8"?>'#13#10'<!-- Generator: Adobe Il' +
          'lustrator 15.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0' +
          ')  -->'#13#10'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://w' +
          'ww.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#13#10'<svg version="1.1"  ' +
          'xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.or' +
          'g/1999/xlink" x="0px" y="0px"'#13#10#9' width="48px" height="48px" view' +
          'Box="0 0 48 48" enable-background="new 0 0 48 48" xml:space="pre' +
          'serve">'#13#10'<path fill="#CFD8DC" d="M6,10c0-2.209,1.791-4,4-4h28c2.' +
          '209,0,4,1.791,4,4v28c0,2.209-1.791,4-4,4H10c-2.209,0-4-1.791-4-4' +
          'V10z"/>'#13#10'<path fill="#37474F" d="M39,17.271c0,0.191-0.148,0.349-' +
          '0.334,0.349h-1.799l-8.164,18.179c-0.052,0.12-0.17,0.2-0.297,0.20' +
          '2h-0.004'#13#10#9'c-0.127,0-0.242-0.074-0.298-0.193l-3.874-8.039l-4.18,' +
          '8.049c-0.06,0.116-0.167,0.181-0.303,0.184'#13#10#9'c-0.125-0.004-0.239-' +
          '0.082-0.292-0.199l-8.252-18.182h-1.87C9.149,17.619,9,17.462,9,17' +
          '.271V16.35C9,16.155,9.149,16,9.333,16h6.657'#13#10#9'c0.184,0,0.333,0.1' +
          '55,0.333,0.35v0.921c0,0.191-0.149,0.349-0.333,0.349h-1.433l5.696' +
          ',13.748l2.964-5.793l-3.757-7.953h-0.904'#13#10#9'c-0.184,0-0.333-0.157-' +
          '0.333-0.35V16.35c0-0.191,0.149-0.348,0.333-0.348h4.924c0.184,0,0' +
          '.333,0.156,0.333,0.348v0.922'#13#10#9'c0,0.192-0.149,0.35-0.333,0.35h-0' +
          '.867l2.162,4.948l2.572-4.948H25.77c-0.187,0-0.334-0.157-0.334-0.' +
          '35V16.35'#13#10#9'c0-0.191,0.147-0.348,0.334-0.348h4.784c0.187,0,0.333,' +
          '0.156,0.333,0.348v0.922c0,0.192-0.146,0.35-0.333,0.35h-1.05l-3.7' +
          '57,7.141'#13#10#9'l3.063,6.584l5.905-13.725h-1.872c-0.184,0-0.334-0.157' +
          '-0.334-0.35V16.35c0-0.191,0.15-0.348,0.334-0.348h5.822'#13#10#9'c0.186,' +
          '0,0.334,0.156,0.334,0.348V17.271z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'workflow'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 48 48" enable-background="new 0 0 48 48">'#13#10'    <rect x="7" y="3' +
          '1" fill="#00BCD4" width="10" height="10"/>'#13#10'    <path fill="#00B' +
          'CD4" d="M35.3,19.3l-5.6-5.6c-0.4-0.4-0.4-1,0-1.4l5.6-5.6c0.4-0.4' +
          ',1-0.4,1.4,0l5.6,5.6c0.4,0.4,0.4,1,0,1.4 l-5.6,5.6C36.3,19.7,35.' +
          '7,19.7,35.3,19.3z"/>'#13#10'    <circle fill="#3F51B5" cx="12" cy="13"' +
          ' r="6"/>'#13#10'    <circle fill="#448AFF" cx="36" cy="36" r="6"/>'#13#10'  ' +
          '  <g fill="#90A4AE">'#13#10'        <rect x="11" y="24" width="2" heig' +
          'ht="5"/>'#13#10'        <polygon points="12,21 9,25 15,25"/>'#13#10'    </g>' +
          #13#10'    <g fill="#90A4AE">'#13#10'        <rect x="20" y="12" width="5" ' +
          'height="2"/>'#13#10'        <polygon points="28,13 24,10 24,16"/>'#13#10'   ' +
          ' </g>'#13#10'    <g fill="#90A4AE">'#13#10'        <rect x="35" y="21" width' +
          '="2" height="5"/>'#13#10'        <polygon points="36,29 39,25 33,25"/>' +
          #13#10'    </g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Pyton'
        SVGText = 
          '<svg viewBox="0 0 32 32" >'#13#10'<g transform="translate(-275.1 -318.' +
          '4)">'#13#10'    <path d="M290.9,318.8h-11.3c-1.6,0-2.9,1.3-2.8,2.9v23.' +
          '4c0,1.6,1.2,2.9,2.8,2.9h17c1.6,0,2.9-1.3,2.8-2.9'#13#10'        v-17.6' +
          'L290.9,318.8z M279.6,345.2v-23.4h9.9v7.3h7.1v16.1L279.6,345.2L27' +
          '9.6,345.2z"/>'#13#10'</g>'#13#10'<g transform="translate(-16.5,-18) scale(1.' +
          '45)">'#13#10'    <path fill="#4488FF" d="M20.8,22.5H25c1.2,0,2.1-1,2.1' +
          '-2.2v-4c-0.1-1.2-1-2.1-2.1-2.2c0,0,0,0-0.1,0c-0.6-0.1-3.4-0.2-4.' +
          '2,0'#13#10'        c-1.2,0.3-1.8,0.6-2,1.2c-0.1,0.3-0.1,0.6-0.1,1v1.6h' +
          '4.2v0.6h-5.9c-1.3,0-2.4,0.9-2.7,2.1c-0.1,0.3-0.1,0.5-0.2,0.8'#13#10'  ' +
          '      c0,0.1,0,0.2-0.1,0.3c-0.1,0.4-0.1,0.8-0.1,1.2c0,0.7,0.1,1.' +
          '4,0.3,2.1c0.1,0.3,0.2,0.6,0.3,0.8c0.3,0.6,0.6,1,1.1,1.2'#13#10'       ' +
          ' c0.3,0.1,0.5,0.2,0.8,0.2h1.5v-2C18.1,23.6,19.3,22.5,20.8,22.5z ' +
          'M20.5,16.7c-0.4,0-0.8-0.4-0.8-0.8s0.4-0.8,0.8-0.8'#13#10'        s0.8,' +
          '0.4,0.8,0.8C21.4,16.3,21,16.7,20.5,16.7z"/>'#13#10'    <path fill="#FF' +
          'CE00" d="M24.9,23.5h-4.2c-1.2,0-2.1,1-2.1,2.2v4c0.1,1.2,1,2.1,2.' +
          '1,2.2c0,0,0,0,0.1,0c0.6,0.1,3.4,0.2,4.2,0'#13#10'        c1.2-0.3,1.8-' +
          '0.6,2-1.2c0.1-0.3,0.1-0.6,0.1-1v-1.6h-4.2v-0.6h5.9c1.3,0,2.4-0.9' +
          ',2.7-2.1c0.1-0.3,0.1-0.5,0.2-0.8'#13#10'        c0-0.1,0-0.2,0.1-0.3c0' +
          '.1-0.4,0.1-0.8,0.1-1.2c0-0.7-0.1-1.4-0.3-2.1c-0.1-0.3-0.2-0.6-0.' +
          '3-0.8c-0.3-0.6-0.6-1-1.1-1.2'#13#10'        c-0.3-0.1-0.5-0.2-0.8-0.2h' +
          '-1.5v2C27.6,22.4,26.4,23.5,24.9,23.5z M25.2,29.3c0.4,0,0.8,0.4,0' +
          '.8,0.8c0,0.4-0.4,0.8-0.8,0.8'#13#10'        s-0.8-0.4-0.8-0.8C24.3,29.' +
          '7,24.7,29.3,25.2,29.3z"/>'#13#10'</g>'#13#10'</svg>'
      end
      item
        IconName = 'monochrome'
        SVGText = 
          '<svg version="1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0' +
          ' 24 24">'#13#10'    <path stroke="red" stroke-width="2" d="M14,19H18V5' +
          'H14M6,19H10V5H6V19Z" />'#13#10'</svg>'
      end>
    Left = 423
    Top = 398
  end
end
