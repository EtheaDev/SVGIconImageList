object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'SVGMagic library demo'
  ClientHeight = 822
  ClientWidth = 719
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnResize = FormResize
  TextHeight = 13
  object pcMain: TPageControl
    Left = 50
    Top = 0
    Width = 669
    Height = 822
    ActivePage = tsComponentGallery
    Align = alClient
    TabOrder = 0
    OnChange = pcMainChange
    object tsComponentGallery: TTabSheet
      Caption = 'tsComponentGallery'
      ImageIndex = 4
      object sbGallery: TScrollBox
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 661
        Height = 794
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Constraints.MaxWidth = 661
        Constraints.MinWidth = 660
        Color = clWhite
        ParentColor = False
        TabOrder = 0
        object paGalleryLine3: TPanel
          AlignWithMargins = True
          Left = 5
          Top = 840
          Width = 634
          Height = 195
          Margins.Left = 5
          Margins.Top = 30
          Margins.Right = 5
          Margins.Bottom = 0
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 0
          object paGalleryLine3Captions: TPanel
            Left = 0
            Top = 25
            Width = 634
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 0
            object paGalleryStyledCheckBox: TPanel
              Left = 0
              Top = 0
              Width = 145
              Height = 25
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 0
              object laGalleryStyledCheckBox: TLabel
                Left = 0
                Top = 0
                Width = 145
                Height = 25
                Align = alClient
                Caption = 'Styled check box'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -15
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                Layout = tlCenter
                ExplicitWidth = 110
                ExplicitHeight = 18
              end
            end
            object paGalleryNormalCheckBox: TPanel
              Left = 145
              Top = 0
              Width = 145
              Height = 25
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 1
              object laGalleryNormalCheckBox: TLabel
                Left = 0
                Top = 0
                Width = 145
                Height = 25
                Align = alClient
                Caption = 'Normal check box'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -15
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                Layout = tlCenter
                ExplicitWidth = 117
                ExplicitHeight = 18
              end
            end
            object paGalleryStyledRadioButton: TPanel
              AlignWithMargins = True
              Left = 298
              Top = 0
              Width = 145
              Height = 25
              Margins.Left = 8
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 2
              object laGalleryStyledRadioButton: TLabel
                Left = 0
                Top = 0
                Width = 145
                Height = 25
                Align = alClient
                Caption = 'Styled radio button'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -15
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                Layout = tlCenter
                ExplicitWidth = 122
                ExplicitHeight = 18
              end
            end
            object paGalleryNormalRadioButton: TPanel
              Left = 443
              Top = 0
              Width = 145
              Height = 25
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 3
              object laGalleryNormalRadioButton: TLabel
                Left = 0
                Top = 0
                Width = 145
                Height = 25
                Align = alClient
                Caption = 'Normal radio button'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -15
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                Layout = tlCenter
                ExplicitWidth = 129
                ExplicitHeight = 18
              end
            end
          end
          object paGalleryLine3Components: TPanel
            Left = 0
            Top = 50
            Width = 634
            Height = 100
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 1
            object blGalleryLine3ComponentsSeparator: TBevel
              AlignWithMargins = True
              Left = 293
              Top = 0
              Width = 2
              Height = 100
              Margins.Top = 0
              Margins.Bottom = 0
              Align = alLeft
            end
            object paGalleryLine3ComponentsStyledCB: TPanel
              Left = 0
              Top = 0
              Width = 145
              Height = 100
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 0
              object ckGalleryStyledCBDisabledGrayed: TCheckBox
                Left = 0
                Top = 85
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Disabled Grayed'
                Enabled = False
                State = cbGrayed
                TabOrder = 0
              end
              object ckGalleryStyledCBDisabledChecked: TCheckBox
                Left = 0
                Top = 68
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Disabled Checked'
                Checked = True
                Enabled = False
                State = cbChecked
                TabOrder = 1
              end
              object ckGalleryStyledCBDisabled: TCheckBox
                Left = 0
                Top = 51
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Disabled'
                Enabled = False
                TabOrder = 2
              end
              object ckGalleryStyledCBGrayed: TCheckBox
                Left = 0
                Top = 34
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Grayed'
                State = cbGrayed
                TabOrder = 3
              end
              object ckGalleryStyledCBChecked: TCheckBox
                Left = 0
                Top = 17
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Checked'
                Checked = True
                State = cbChecked
                TabOrder = 4
              end
              object ckGalleryStyledCBDefault: TCheckBox
                Left = 0
                Top = 0
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Default'
                TabOrder = 5
              end
            end
            object paGalleryLine2ComponentsUnstyledCB: TPanel
              Left = 145
              Top = 0
              Width = 145
              Height = 100
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 1
              object ckGalleryUnstyledCBDisabledGrayed: TCheckBox
                Left = 0
                Top = 85
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Disabled Grayed'
                Enabled = False
                State = cbGrayed
                TabOrder = 0
              end
              object ckGalleryUnstyledCBDisabledChecked: TCheckBox
                Left = 0
                Top = 68
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Disabled Checked'
                Checked = True
                Enabled = False
                State = cbChecked
                TabOrder = 1
              end
              object ckGalleryUnstyledCBDisabled: TCheckBox
                Left = 0
                Top = 51
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Disabled'
                Enabled = False
                TabOrder = 2
              end
              object ckGalleryUnstyledCBGrayed: TCheckBox
                Left = 0
                Top = 34
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Grayed'
                State = cbGrayed
                TabOrder = 3
              end
              object ckGalleryUnstyledCBChecked: TCheckBox
                Left = 0
                Top = 17
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Checked'
                Checked = True
                State = cbChecked
                TabOrder = 4
              end
              object ckGalleryUnstyledCBDefault: TCheckBox
                Left = 0
                Top = 0
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Default'
                TabOrder = 5
              end
            end
            object paGalleryStyledRB: TPanel
              Left = 298
              Top = 0
              Width = 145
              Height = 100
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 2
              object rbGalleryStyledRBDefault: TRadioButton
                Left = 0
                Top = 0
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Default'
                TabOrder = 0
              end
              object rbGalleryStyledRBChecked: TRadioButton
                Left = 0
                Top = 17
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Checked'
                Checked = True
                TabOrder = 1
                TabStop = True
              end
              object paGalleryStyledRBDisabled: TPanel
                Left = 0
                Top = 34
                Width = 145
                Height = 66
                Align = alClient
                BevelOuter = bvNone
                Color = clWhite
                ParentBackground = False
                TabOrder = 2
                object rbGalleryStyledRBDisabled: TRadioButton
                  Left = 0
                  Top = 0
                  Width = 145
                  Height = 17
                  Align = alTop
                  Caption = 'Disabled'
                  Enabled = False
                  TabOrder = 0
                end
                object rbGalleryStyledRBDisabledChecked: TRadioButton
                  Left = 0
                  Top = 17
                  Width = 145
                  Height = 17
                  Align = alTop
                  Caption = 'Disabled checked'
                  Checked = True
                  Enabled = False
                  TabOrder = 1
                  TabStop = True
                end
              end
            end
            object paGalleryUnstyledRB: TPanel
              Left = 443
              Top = 0
              Width = 145
              Height = 100
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 3
              object rbGalleryUnstyledRBDefault: TRadioButton
                Left = 0
                Top = 0
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Default'
                TabOrder = 0
              end
              object rbGalleryUnstyledRBChecked: TRadioButton
                Left = 0
                Top = 17
                Width = 145
                Height = 17
                Align = alTop
                Caption = 'Checked'
                Checked = True
                TabOrder = 1
                TabStop = True
              end
              object paGalleryUnstyledRBDisabled: TPanel
                Left = 0
                Top = 34
                Width = 145
                Height = 66
                Align = alClient
                BevelOuter = bvNone
                Color = clWhite
                ParentBackground = False
                TabOrder = 2
                object rbGalleryUnstyledRBDisabled: TRadioButton
                  Left = 0
                  Top = 0
                  Width = 145
                  Height = 17
                  Align = alTop
                  Caption = 'Disabled'
                  Enabled = False
                  TabOrder = 0
                end
                object rbGalleryUnstyledRBDisabledChecked: TRadioButton
                  Left = 0
                  Top = 17
                  Width = 145
                  Height = 17
                  Align = alTop
                  Caption = 'Disabled checked'
                  Checked = True
                  Enabled = False
                  TabOrder = 1
                  TabStop = True
                end
              end
            end
          end
          object paGalleryLine3Header: TPanel
            Left = 0
            Top = 0
            Width = 634
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 2
            object laGalleryLine3Title: TLabel
              Left = 0
              Top = 0
              Width = 588
              Height = 25
              Align = alLeft
              Alignment = taCenter
              AutoSize = False
              Caption = 'Styles'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 1052688
              Font.Height = -20
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentColor = False
              ParentFont = False
              Layout = tlCenter
            end
          end
          object paGalleryLine3Desc: TPanel
            AlignWithMargins = True
            Left = 0
            Top = 160
            Width = 634
            Height = 35
            Margins.Left = 0
            Margins.Top = 10
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 3
            object paGalleryCheckBoxDesc: TPanel
              Left = 0
              Top = 0
              Width = 588
              Height = 35
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 0
              object laGalleryCheckBoxDesc: TLabel
                AlignWithMargins = True
                Left = 0
                Top = 5
                Width = 588
                Height = 30
                Margins.Left = 0
                Margins.Top = 5
                Margins.Right = 0
                Margins.Bottom = 0
                Align = alClient
                Caption = 
                  'The component styles turn possible to overload the default compo' +
                  'nent glyphes. This allows to create a custom form style using yo' +
                  'ur own SVG images without efforts.'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                WordWrap = True
                ExplicitWidth = 584
                ExplicitHeight = 26
              end
            end
          end
        end
        object paGalleryLine1: TPanel
          AlignWithMargins = True
          Left = 5
          Top = 0
          Width = 634
          Height = 630
          Margins.Left = 5
          Margins.Top = 0
          Margins.Right = 5
          Margins.Bottom = 0
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 1
          object paGalleryLine1Captions: TPanel
            Left = 0
            Top = 25
            Width = 634
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 0
            object paGalleryTImage: TPanel
              Left = 0
              Top = 0
              Width = 200
              Height = 25
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 0
              object laGalleryTImage: TLabel
                Left = 0
                Top = 0
                Width = 200
                Height = 25
                Align = alClient
                Alignment = taCenter
                Caption = 'TImage'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -16
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                Layout = tlCenter
                ExplicitWidth = 55
                ExplicitHeight = 19
              end
            end
            object paGalleryTWSVGImage: TPanel
              AlignWithMargins = True
              Left = 420
              Top = 0
              Width = 200
              Height = 25
              Margins.Left = 10
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 1
              object laGalleryTWSVGImage: TLabel
                Left = 0
                Top = 0
                Width = 200
                Height = 25
                Align = alClient
                Alignment = taCenter
                Caption = 'TWSVGImageButton'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -16
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                Layout = tlCenter
                ExplicitWidth = 145
                ExplicitHeight = 19
              end
            end
            object paGalleryTWSVGImageButton: TPanel
              AlignWithMargins = True
              Left = 210
              Top = 0
              Width = 200
              Height = 25
              Margins.Left = 10
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 2
              object laGalleryTWSVGImageButton: TLabel
                Left = 0
                Top = 0
                Width = 200
                Height = 25
                Align = alClient
                Alignment = taCenter
                Caption = 'TWSVGImage'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -16
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                Layout = tlCenter
                ExplicitWidth = 99
                ExplicitHeight = 19
              end
            end
          end
          object paGalleryLine1Components: TPanel
            Left = 0
            Top = 50
            Width = 634
            Height = 200
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 1
            object imGalleryTImage: TImage
              Left = 0
              Top = 0
              Width = 200
              Height = 200
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                223F3E0A3C7376670A20202020786D6C6E733A696E6B73636170653D22687474
                703A2F2F7777772E696E6B73636170652E6F72672F6E616D657370616365732F
                696E6B7363617065220A20202020786D6C6E733A7264663D22687474703A2F2F
                7777772E77332E6F72672F313939392F30322F32322D7264662D73796E746178
                2D6E7323220A20202020786D6C6E733D22687474703A2F2F7777772E77332E6F
                72672F323030302F737667220A20202020786D6C6E733A736F6469706F64693D
                22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E6574
                2F4454442F736F6469706F64692D302E647464220A20202020786D6C6E733A63
                633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E73
                23220A20202020786D6C6E733A786C696E6B3D22687474703A2F2F7777772E77
                332E6F72672F313939392F786C696E6B220A20202020786D6C6E733A7868746D
                6C3D22687474703A2F2F7777772E77332E6F72672F313939392F7868746D6C22
                0A20202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64
                632F656C656D656E74732F312E312F220A20202020786D6C6E733A7376673D22
                687474703A2F2F7777772E77332E6F72672F323030302F737667220A20202020
                786D6C6E733A6E73313D22687474703A2F2F736F7A692E62616965726F756765
                2E6672220A2020202069643D22737667323330373735220A20202020736F6469
                706F64693A646F636E616D653D22666F726569676E6F626A6563742E73766722
                0A2020202076696577426F783D223020302034303420373733220A2020202076
                657273696F6E3D22312E31220A20202020696E6B73636170653A76657273696F
                6E3D22302E343870726530207239343436220A20203E3C7469746C650A202020
                20202069643D227469746C65323431333735220A202020203E54686520496E63
                72656469626C65204A61766173637269707420416E64726F69642050686F6E65
                2042726F777365723C2F7469746C650A20203E3C646566730A20202020202069
                643D2264656673323330373833220A202020203E3C6C696E6561724772616469
                656E740A202020202020202069643D226C696E6561724772616469656E743436
                3536220A202020202020202079323D223636352E38220A202020202020202067
                72616469656E74556E6974733D227573657253706163654F6E557365220A2020
                20202020202079313D223636352E38220A20202020202020206772616469656E
                745472616E73666F726D3D226D6174726978282E3933363520302030202E3739
                39303420322E38353835203133332E373229220A202020202020202078323D22
                36322E343934220A202020202020202078313D2232382E393934220A20202020
                20202020696E6B73636170653A636F6C6C6563743D22616C77617973220A2020
                202020203E3C73746F700A2020202020202020202069643D2273746F70353230
                37220A202020202020202020207374796C653D2273746F702D636F6C6F723A72
                67622836372C203136372C2032313729220A202020202020202020206F666673
                65743D2230220A2020202020202F3E3C73746F700A2020202020202020202069
                643D2273746F7035323039220A202020202020202020207374796C653D227374
                6F702D636F6C6F723A726762283137312C203231332C2032333729220A202020
                202020202020206F66667365743D2231220A2020202020202F3E3C2F6C696E65
                61724772616469656E740A202020203E3C72616469616C4772616469656E740A
                202020202020202069643D2272616469616C4772616469656E7434363336220A
                20202020202020206772616469656E74556E6974733D22757365725370616365
                4F6E557365220A202020202020202063783D223335372E3632220A2020202020
                20202063793D223634312E3739220A2020202020202020723D22362E35313336
                220A20202020202020206772616469656E745472616E73666F726D3D226D6174
                72697828312E35313132202E3132393636202D2E313034373120312E32323034
                202D3131352E3431202D3138372E363229220A2020202020202020696E6B7363
                6170653A636F6C6C6563743D22616C77617973220A2020202020203E3C73746F
                700A2020202020202020202069643D2273746F7033393235220A202020202020
                202020207374796C653D2273746F702D636F6C6F723A726762283230342C2032
                30342C2032303729220A202020202020202020206F66667365743D2230220A20
                20202020202F3E3C73746F700A2020202020202020202069643D2273746F7033
                393237220A202020202020202020207374796C653D2273746F702D636F6C6F72
                3A726762283233372C203233372C2032333729220A202020202020202020206F
                66667365743D2231220A2020202020202F3E3C2F72616469616C477261646965
                6E740A202020203E3C6C696E6561724772616469656E740A2020202020202020
                69643D226C696E6561724772616469656E74333635312D362D31220A20202020
                20202020696E6B73636170653A636F6C6C6563743D22616C77617973220A2020
                202020203E3C73746F700A2020202020202020202069643D2273746F70333635
                332D372D35220A202020202020202020207374796C653D2273746F702D636F6C
                6F723A726762283233382C203233382C2032333829220A202020202020202020
                206F66667365743D2230220A2020202020202F3E3C73746F700A202020202020
                2020202069643D2273746F70333635352D312D39220A20202020202020202020
                7374796C653D2273746F702D636F6C6F723A726762283230342C203230342C20
                32303729220A202020202020202020206F66667365743D2231220A2020202020
                202F3E3C2F6C696E6561724772616469656E740A202020203E3C6C696E656172
                4772616469656E740A202020202020202069643D226C696E6561724772616469
                656E7433383036220A2020202020202020696E6B73636170653A636F6C6C6563
                743D22616C77617973220A2020202020203E3C73746F700A2020202020202020
                202069643D2273746F7033383038220A202020202020202020207374796C653D
                2273746F702D636F6C6F723A726762283233382C203233382C2032333829220A
                202020202020202020206F66667365743D2230220A2020202020202F3E3C7374
                6F700A2020202020202020202069643D2273746F7033383130220A2020202020
                20202020207374796C653D2273746F702D636F6C6F723A726762283233382C20
                3233382C20323338293B73746F702D6F7061636974793A30220A202020202020
                202020206F66667365743D2231220A2020202020202F3E3C2F6C696E65617247
                72616469656E740A202020203E3C6C696E6561724772616469656E740A202020
                202020202069643D226C696E6561724772616469656E74323430343139220A20
                2020202020202079323D223632342E3434220A2020202020202020786C696E6B
                3A687265663D22236C696E6561724772616469656E7433383036220A20202020
                202020206772616469656E74556E6974733D227573657253706163654F6E5573
                65220A202020202020202078323D223337352E3137220A202020202020202079
                313D223431302E3438220A202020202020202078313D223335302E3436220A20
                20202020202020696E6B73636170653A636F6C6C6563743D22616C7761797322
                0A202020202F3E3C6C696E6561724772616469656E740A202020202020202069
                643D226C696E6561724772616469656E74323430353333220A20202020202020
                2079323D223634352E3732220A2020202020202020786C696E6B3A687265663D
                22236C696E6561724772616469656E74333635312D362D31220A202020202020
                20206772616469656E74556E6974733D227573657253706163654F6E55736522
                0A202020202020202078323D223431312E3539220A202020202020202079313D
                223634352E3732220A202020202020202078313D223337362E3036220A202020
                2020202020696E6B73636170653A636F6C6C6563743D22616C77617973220A20
                2020202F3E3C6C696E6561724772616469656E740A202020202020202069643D
                226C696E6561724772616469656E74323430353336220A202020202020202079
                323D223633302E3634220A2020202020202020786C696E6B3A687265663D2223
                6C696E6561724772616469656E74333635312D362D31220A2020202020202020
                6772616469656E74556E6974733D227573657253706163654F6E557365220A20
                2020202020202078323D223333322E3839220A202020202020202079313D2236
                33302E3634220A202020202020202078313D223331362E3631220A2020202020
                202020696E6B73636170653A636F6C6C6563743D22616C77617973220A202020
                202F3E3C6C696E6561724772616469656E740A202020202020202069643D226C
                696E6561724772616469656E74323430353339220A202020202020202079323D
                223633302E32220A2020202020202020786C696E6B3A687265663D22236C696E
                6561724772616469656E74333635312D362D31220A2020202020202020677261
                6469656E74556E6974733D227573657253706163654F6E557365220A20202020
                2020202078323D223338342E3536220A202020202020202079313D223633302E
                32220A202020202020202078313D223336382E3237220A202020202020202069
                6E6B73636170653A636F6C6C6563743D22616C77617973220A202020202F3E3C
                6C696E6561724772616469656E740A202020202020202069643D226C696E6561
                724772616469656E74323430353432220A202020202020202079323D22363330
                2E32220A2020202020202020786C696E6B3A687265663D22236C696E65617247
                72616469656E74333635312D362D31220A20202020202020206772616469656E
                74556E6974733D227573657253706163654F6E557365220A2020202020202020
                78323D223430392E3238220A202020202020202079313D223633302E32220A20
                2020202020202078313D223339322E3939220A2020202020202020696E6B7363
                6170653A636F6C6C6563743D22616C77617973220A202020202F3E3C6C696E65
                61724772616469656E740A202020202020202069643D226C696E656172477261
                6469656E74323430353435220A202020202020202079323D223633302E363422
                0A2020202020202020786C696E6B3A687265663D22236C696E65617247726164
                69656E74333635312D362D31220A20202020202020206772616469656E74556E
                6974733D227573657253706163654F6E557365220A202020202020202078323D
                223335382E35220A202020202020202079313D223633302E3634220A20202020
                2020202078313D223334322E3232220A2020202020202020696E6B7363617065
                3A636F6C6C6563743D22616C77617973220A202020202F3E3C6C696E65617247
                72616469656E740A202020202020202069643D226C696E656172477261646965
                6E74323430353635220A202020202020202079323D223437392E3036220A2020
                202020202020786C696E6B3A687265663D22236C696E6561724772616469656E
                7433383036220A20202020202020206772616469656E74556E6974733D227573
                657253706163654F6E557365220A202020202020202078323D223334352E3735
                220A202020202020202079313D223437352E3036220A20202020202020207831
                3D223334352E3635220A2020202020202020696E6B73636170653A636F6C6C65
                63743D22616C77617973220A202020202F3E3C6C696E6561724772616469656E
                740A202020202020202069643D226C696E6561724772616469656E7432343035
                3732220A202020202020202079323D223438322E3837220A2020202020202020
                6772616469656E74556E6974733D227573657253706163654F6E557365220A20
                2020202020202078323D223334332E3435220A202020202020202079313D2234
                37332E3938220A202020202020202078313D223334332E3135220A2020202020
                202020696E6B73636170653A636F6C6C6563743D22616C77617973220A202020
                2020203E3C73746F700A2020202020202020202069643D2273746F7033393533
                220A202020202020202020207374796C653D2273746F702D636F6C6F723A7267
                6228302C20302C203029220A202020202020202020206F66667365743D223022
                0A2020202020202F3E3C73746F700A2020202020202020202069643D2273746F
                7033393535220A202020202020202020207374796C653D2273746F702D636F6C
                6F723A7267622835312C2035312C20353129220A202020202020202020206F66
                667365743D2231220A2020202020202F3E3C2F6C696E6561724772616469656E
                740A202020203E3C6C696E6561724772616469656E740A202020202020202069
                643D226C696E6561724772616469656E74323430353736220A20202020202020
                2079323D223437342E3936220A20202020202020206772616469656E74556E69
                74733D227573657253706163654F6E557365220A202020202020202078323D22
                3337382E3933220A202020202020202079313D223631312E3436220A20202020
                2020202078313D223338342E38220A2020202020202020696E6B73636170653A
                636F6C6C6563743D22616C77617973220A2020202020203E3C73746F700A2020
                202020202020202069643D2273746F7033393435220A20202020202020202020
                7374796C653D2273746F702D636F6C6F723A726762283137392C203232312C20
                32353329220A202020202020202020206F66667365743D2230220A2020202020
                202F3E3C73746F700A2020202020202020202069643D2273746F703339343722
                0A202020202020202020207374796C653D2273746F702D636F6C6F723A726762
                2835312C203130322C2031353229220A202020202020202020206F6666736574
                3D2231220A2020202020202F3E3C2F6C696E6561724772616469656E740A2020
                20203E3C6C696E6561724772616469656E740A202020202020202069643D226C
                696E6561724772616469656E74323430353836220A202020202020202079323D
                223631302E3332220A20202020202020206772616469656E74556E6974733D22
                7573657253706163654F6E557365220A202020202020202078323D223431392E
                3732220A202020202020202079313D223631372E3133220A2020202020202020
                78313D223330362E3134220A2020202020202020696E6B73636170653A636F6C
                6C6563743D22616C77617973220A2020202020203E3C73746F700A2020202020
                202020202069643D2273746F7033393930220A20202020202020202020737479
                6C653D2273746F702D636F6C6F723A7267622835312C2035312C20353129220A
                202020202020202020206F66667365743D2230220A2020202020202F3E3C7374
                6F700A2020202020202020202069643D2273746F7033393932220A2020202020
                20202020207374796C653D2273746F702D636F6C6F723A7267622835312C2035
                312C203531293B73746F702D6F7061636974793A30220A202020202020202020
                206F66667365743D2231220A2020202020202F3E3C2F6C696E65617247726164
                69656E740A202020203E3C6C696E6561724772616469656E740A202020202020
                202069643D226C696E6561724772616469656E74323430353839220A20202020
                2020202079323D223634372E3839220A20202020202020206772616469656E74
                556E6974733D227573657253706163654F6E557365220A202020202020202078
                323D223431362E3836220A202020202020202079313D223634382E3732220A20
                2020202020202078313D223330382E3137220A2020202020202020696E6B7363
                6170653A636F6C6C6563743D22616C77617973220A2020202020203E3C73746F
                700A2020202020202020202069643D2273746F7033393137220A202020202020
                202020207374796C653D2273746F702D636F6C6F723A726762283130322C2031
                30322C2031303229220A202020202020202020206F66667365743D2230220A20
                20202020202F3E3C73746F700A2020202020202020202069643D2273746F7033
                393139220A202020202020202020207374796C653D2273746F702D636F6C6F72
                3A726762283130322C203130322C20313032293B73746F702D6F706163697479
                3A30220A202020202020202020206F66667365743D2231220A2020202020202F
                3E3C2F6C696E6561724772616469656E740A202020203E3C2F646566730A2020
                3E3C736F6469706F64693A6E616D6564766965770A20202020202069643D226E
                616D656476696577323330373831220A2020202020206669742D6D617267696E
                2D6C6566743D223230220A202020202020696E6B73636170653A73686F777061
                6765736861646F773D2266616C7365220A202020202020696E6B73636170653A
                7A6F6F6D3D22302E3933363236353938220A202020202020626F726465726F70
                61636974793D2231220A202020202020696E6B73636170653A63757272656E74
                2D6C617965723D226C6179657231220A202020202020696E6B73636170653A63
                783D223238322E3137313033220A202020202020696E6B73636170653A63793D
                223339332E3835323537220A202020202020696E6B73636170653A77696E646F
                772D6D6178696D697A65643D2231220A20202020202073686F77677269643D22
                66616C7365220A2020202020206669742D6D617267696E2D72696768743D2232
                30220A202020202020626F72646572636F6C6F723D2223363636363636220A20
                2020202020696E6B73636170653A77696E646F772D783D2230220A2020202020
                206775696465746F6C6572616E63653D223130220A2020202020206F626A6563
                74746F6C6572616E63653D223130220A202020202020696E6B73636170653A77
                696E646F772D793D2230220A2020202020206669742D6D617267696E2D626F74
                746F6D3D223230220A202020202020696E6B73636170653A77696E646F772D77
                696474683D2231323830220A202020202020696E6B73636170653A706167656F
                7061636974793D2230220A202020202020696E6B73636170653A706167657368
                61646F773D2232220A20202020202070616765636F6C6F723D22236666666666
                66220A20202020202067726964746F6C6572616E63653D223130220A20202020
                2020696E6B73636170653A77696E646F772D6865696768743D22393630220A20
                20202020206669742D6D617267696E2D746F703D223230220A20202F3E3C670A
                20202020202069643D226C6179657231220A202020202020696E6B7363617065
                3A6C6162656C3D224C617965722031220A2020202020207472616E73666F726D
                3D226D617472697828332E323330322030203020332E32333032202D3937312E
                3933202D313431342E3729220A202020203E3C726563740A2020202020202020
                69643D227265637433363638220A20202020202020207374796C653D2266696C
                6C3A23333333333333220A202020202020202072783D22352E30303537220A20
                2020202020202072793D22342E37353731220A20202020202020206865696768
                743D2231312E313237220A202020202020202077696474683D2232312E303435
                220A2020202020202020793D223635392E39220A2020202020202020783D2233
                35322E3032220A202020202F3E3C726563740A202020202020202069643D2272
                65637432383930220A20202020202020207374796C653D2266696C6C3A233333
                33333333220A202020202020202072783D2231322E3137220A20202020202020
                2072793D223132220A20202020202020206865696768743D223232342E383722
                0A202020202020202077696474683D223131322E3633220A2020202020202020
                793D223434342E3134220A2020202020202020783D223330372E3038220A2020
                20202F3E3C726563740A202020202020202069643D227265637433363932220A
                20202020202020207374796C653D2266696C6C3A75726C28236C696E65617247
                72616469656E7432343035383929220A202020202020202072783D2231302E35
                32220A20202020202020206865696768743D223231392E3336220A2020202020
                20202077696474683D223130372E3135220A2020202020202020793D22343436
                2E3931220A2020202020202020783D223330392E3731220A202020202F3E3C70
                6174680A202020202020202069643D227265637433363936220A202020202020
                2020643D226D3331392E3235203434342E3136632D362E3734323220302D3132
                2E31353620352E3332312D31322E3135362031312E393639763138312E353968
                3131322E3632762D3138312E353963302D362E363437382D352E343435332D31
                312E3936392D31322E3138382D31312E393639682D38382E3238317A220A2020
                2020202020207374796C653D2266696C6C3A75726C28236C696E656172477261
                6469656E7432343035383629220A2020202020202020696E6B73636170653A63
                6F6E6E6563746F722D6375727661747572653D2230220A202020202F3E3C7061
                74680A202020202020202069643D227061746833363730220A20202020202020
                20736F6469706F64693A72783D22362E35313336323536220A20202020202020
                20736F6469706F64693A72793D22362E35313336323536220A20202020202020
                207374796C653D2266696C6C3A23333333333333220A2020202020202020736F
                6469706F64693A747970653D22617263220A2020202020202020643D226D3336
                342E333220363432633020332E353937342D322E3931363220362E353133362D
                362E3531333620362E35313336732D362E353133362D322E393136322D362E35
                3133362D362E3531333620322E393136322D362E3531333620362E353133362D
                362E3531333620362E3531333620322E3931363220362E3531333620362E3531
                33367A220A20202020202020207472616E73666F726D3D226D6174726978282E
                383733333620302030202E38373333362035302E3739312038362E313929220A
                2020202020202020736F6469706F64693A63793D223634322E3030333738220A
                2020202020202020736F6469706F64693A63783D223335372E38303533220A20
                2020202F3E3C726563740A202020202020202069643D22726563743336373222
                0A20202020202020207374796C653D2266696C6C3A23323432343234220A2020
                20202020202072783D22352E30303537220A202020202020202072793D22342E
                37353731220A20202020202020206865696768743D22382E37333432220A2020
                20202020202077696474683D2233352E353239220A2020202020202020793D22
                3634322E33220A2020202020202020783D223337362E3136220A202020202F3E
                3C726563740A202020202020202069643D227265637433363736220A20202020
                202020207374796C653D2266696C6C3A23323432343234220A20202020202020
                2072783D2234220A202020202020202072793D2234220A202020202020202068
                65696768743D22362E35313336220A202020202020202077696474683D223136
                2E323834220A2020202020202020793D223632382E3234220A20202020202020
                20783D223334322E3131220A202020202F3E3C726563740A2020202020202020
                69643D227265637433363738220A20202020202020207374796C653D2266696C
                6C3A23323432343234220A202020202020202072783D2234220A202020202020
                202072793D2234220A20202020202020206865696768743D22362E3531333622
                0A202020202020202077696474683D2231362E323834220A2020202020202020
                793D223632372E3739220A2020202020202020783D223339322E3839220A2020
                20202F3E3C726563740A202020202020202069643D227265637433363830220A
                20202020202020207374796C653D2266696C6C3A23323432343234220A202020
                202020202072783D2234220A202020202020202072793D2234220A2020202020
                2020206865696768743D22362E35313336220A20202020202020207769647468
                3D2231362E323834220A2020202020202020793D223632372E3739220A202020
                2020202020783D223336382E3137220A202020202F3E3C726563740A20202020
                2020202069643D227265637433363832220A20202020202020207374796C653D
                2266696C6C3A23323432343234220A202020202020202072783D2234220A2020
                20202020202072793D2234220A20202020202020206865696768743D22362E35
                313336220A202020202020202077696474683D2231362E323834220A20202020
                20202020793D223632382E3234220A2020202020202020783D223331362E3522
                0A202020202F3E3C726563740A202020202020202069643D2272656374333638
                34220A20202020202020207374796C653D2266696C6C3A23323432343234220A
                202020202020202072783D2234220A202020202020202072793D2234220A2020
                2020202020206865696768743D223136352E3138220A20202020202020207769
                6474683D223130342E3035220A2020202020202020793D223435392E3236220A
                2020202020202020783D223331312E3532220A202020202F3E3C726563740A20
                2020202020202069643D227265637433363836220A2020202020202020737479
                6C653D2266696C6C3A75726C28236C696E6561724772616469656E7432343035
                373629220A20202020202020206865696768743D223133382E3338220A202020
                202020202077696474683D2239332E313633220A2020202020202020793D2234
                37342E3936220A2020202020202020783D223331362E3736220A202020202F3E
                3C726563740A202020202020202069643D227265637433363838220A20202020
                202020207374796C653D2266696C6C3A23363636363636220A20202020202020
                2072783D2232220A202020202020202072793D2232220A202020202020202068
                65696768743D22332E39373738220A202020202020202077696474683D223536
                2E313037220A2020202020202020793D223435322E3636220A20202020202020
                20783D223333352E3439220A202020202F3E3C726563740A2020202020202020
                69643D227265637433363930220A20202020202020207374796C653D2266696C
                6C3A75726C28236C696E6561724772616469656E7432343035373229220A2020
                2020202020206865696768743D22382E323931220A2020202020202020776964
                74683D2239332E393138220A2020202020202020793D223437342E3538220A20
                20202020202020783D223331362E3338220A202020202F3E3C670A2020202020
                20202069643D226734303433220A20202020202020207472616E73666F726D3D
                226D6174726978282E323533363220302030202E3235353134203335372E3633
                203330392E363429220A2020202020203E3C706174680A202020202020202020
                2069643D227265637434303239220A202020202020202020207374796C653D22
                66696C6C3A23656565656565220A202020202020202020207472616E73666F72
                6D3D227472616E736C617465282D3139342E3633203238342E323729220A2020
                2020202020202020696E6B73636170653A636F6E6E6563746F722D6375727661
                747572653D2230220A20202020202020202020643D226D34352E393639203337
                302E30367631392E3933386833322E393639762D31392E393338682D302E3238
                3132356C2D31362E32352031302E3138382D31362E3036322D31302E31383868
                2D302E3337357A220A2020202020202F3E3C706174680A202020202020202020
                2069643D227061746834303335220A202020202020202020207374796C653D22
                66696C6C3A23656565656565220A20202020202020202020696E6B7363617065
                3A636F6E6E6563746F722D6375727661747572653D2230220A20202020202020
                202020643D226D2D3134382E3532203635322E34322031362E3238342031302E
                3333312031362E3631312D31302E3339392D33322E38393620302E303637317A
                220A2020202020202F3E3C2F670A202020203E3C706174680A20202020202020
                2069643D227061746833373135220A2020202020202020643D226D3333392E39
                31203435322E363676332E393638386834362E343639762D332E39363838682D
                34362E3436397A220A20202020202020207374796C653D2266696C6C3A236363
                63636366220A2020202020202020696E6B73636170653A636F6E6E6563746F72
                2D6375727661747572653D2230220A202020202F3E3C726563740A2020202020
                20202069643D227265637433373230220A20202020202020207374796C653D22
                6F7061636974793A2E36333138303B66696C6C3A75726C28236C696E65617247
                72616469656E7432343035363529220A202020202020202072783D2232220A20
                202020202020206865696768743D22342E35303636220A202020202020202077
                696474683D2239332E393832220A2020202020202020793D223437342E353522
                0A2020202020202020783D223331362E3335220A202020202F3E3C706174680A
                202020202020202069643D227061746833383030220A2020202020202020736F
                6469706F64693A72783D22362E35313336323536220A2020202020202020736F
                6469706F64693A72793D22362E35313336323536220A20202020202020207374
                796C653D2266696C6C3A75726C282372616469616C4772616469656E74343633
                3629220A2020202020202020736F6469706F64693A747970653D22617263220A
                2020202020202020643D226D3336342E333220363432633020332E353937342D
                322E3931363220362E353133362D362E3531333620362E35313336732D362E35
                3133362D322E393136322D362E353133362D362E3531333620322E393136322D
                362E3531333620362E353133362D362E3531333620362E3531333620322E3931
                363220362E3531333620362E353133367A220A20202020202020207472616E73
                666F726D3D226D6174726978282E373539373220302030202E37353937322039
                312E3435203135392E313429220A2020202020202020736F6469706F64693A63
                793D223634322E3030333738220A2020202020202020736F6469706F64693A63
                783D223335372E38303533220A202020202F3E3C726563740A20202020202020
                2069643D227265637433393537220A20202020202020207374796C653D226669
                6C6C3A75726C28236C696E6561724772616469656E7432343035343529220A20
                2020202020202072783D2234220A202020202020202072793D2234220A202020
                20202020206865696768743D22362E35313336220A2020202020202020776964
                74683D2231362E323834220A2020202020202020793D223632372E3339220A20
                20202020202020783D223334322E3232220A202020202F3E3C726563740A2020
                20202020202069643D227265637433393539220A20202020202020207374796C
                653D2266696C6C3A75726C28236C696E6561724772616469656E743234303534
                3229220A202020202020202072783D2234220A202020202020202072793D2234
                220A20202020202020206865696768743D22362E35313336220A202020202020
                202077696474683D2231362E323834220A2020202020202020793D223632362E
                3934220A2020202020202020783D223339322E3939220A202020202F3E3C7265
                63740A202020202020202069643D227265637433393631220A20202020202020
                207374796C653D2266696C6C3A75726C28236C696E6561724772616469656E74
                32343035333929220A202020202020202072783D2234220A2020202020202020
                72793D2234220A20202020202020206865696768743D22362E35313336220A20
                2020202020202077696474683D2231362E323834220A2020202020202020793D
                223632362E3934220A2020202020202020783D223336382E3237220A20202020
                2F3E3C726563740A202020202020202069643D227265637433393633220A2020
                2020202020207374796C653D2266696C6C3A75726C28236C696E656172477261
                6469656E7432343035333629220A202020202020202072783D2234220A202020
                202020202072793D2234220A20202020202020206865696768743D22362E3531
                3336220A202020202020202077696474683D2231362E323834220A2020202020
                202020793D223632372E3339220A2020202020202020783D223331362E363122
                0A202020202F3E3C726563740A202020202020202069643D2272656374333938
                36220A20202020202020207374796C653D2266696C6C3A75726C28236C696E65
                61724772616469656E7432343035333329220A202020202020202072783D2235
                2E30303537220A202020202020202072793D22342E37353731220A2020202020
                2020206865696768743D22382E37333432220A20202020202020207769647468
                3D2233352E353239220A2020202020202020793D223634312E3336220A202020
                2020202020783D223337362E3036220A202020202F3E3C670A20202020202020
                2069643D226734303437220A20202020202020207472616E73666F726D3D226D
                6174726978282E323833303720302030202E3238333037203336332E38203239
                302E343429220A2020202020203E3C726563740A202020202020202020206964
                3D227265637433393933220A202020202020202020207374796C653D2266696C
                6C3A23616161616161220A202020202020202020206865696768743D2231382E
                383335220A2020202020202020202077696474683D22332E34303038220A2020
                2020202020202020793D223635342E3937220A20202020202020202020783D22
                38362E383531220A2020202020202F3E3C726563740A20202020202020202020
                69643D227265637433393935220A202020202020202020207374796C653D2266
                696C6C3A23656565656565220A202020202020202020206865696768743D2231
                362E343831220A2020202020202020202077696474683D22332E34303038220A
                20202020202020202020793D223635372E3333220A2020202020202020202078
                3D2238302E383531220A2020202020202F3E3C726563740A2020202020202020
                202069643D227265637433393937220A202020202020202020207374796C653D
                2266696C6C3A23656565656565220A202020202020202020206865696768743D
                2231342E313236220A2020202020202020202077696474683D22332E34303038
                220A20202020202020202020793D223635392E3638220A202020202020202020
                20783D2237342E383531220A2020202020202F3E3C726563740A202020202020
                2020202069643D227265637433393939220A202020202020202020207374796C
                653D2266696C6C3A23656565656565220A202020202020202020206865696768
                743D22382E36313431220A2020202020202020202077696474683D22332E3430
                3038220A20202020202020202020793D223636352E3139220A20202020202020
                202020783D2236382E383531220A2020202020202F3E3C2F670A202020203E3C
                670A202020202020202069643D226734303232220A2020202020202020747261
                6E73666F726D3D226D6174726978282E323535353620302030202E3235353536
                203336352E3431203330382E383229220A2020202020203E3C706174680A2020
                202020202020202069643D227265637434303031220A20202020202020202020
                7374796C653D2266696C6C3A23656565656565220A2020202020202020202069
                6E6B73636170653A636F6E6E6563746F722D6375727661747572653D2230220A
                20202020202020202020643D226D33302E353937203635362E333576352E3035
                3231682D332E3435323576382E37343468332E3435323576352E303931683333
                2E313932762D31382E383837682D33332E3139327A220A2020202020202F3E3C
                706174680A2020202020202020202069643D227061746834303134220A202020
                20202020202020643D226D33322E313531203635382E3276342E30363235682D
                332E3135363276372E3033313268332E3135363276342E303933386833302E33
                3434762D31352E313838682D33302E3334347A220A2020202020202020202073
                74796C653D2266696C6C3A23353635323438220A20202020202020202020696E
                6B73636170653A636F6E6E6563746F722D6375727661747572653D2230220A20
                20202020202F3E3C706174680A2020202020202020202069643D227061746834
                303138220A202020202020202020207374796C653D2266696C6C3A75726C2823
                6C696E6561724772616469656E743436353629220A2020202020202020202069
                6E6B73636170653A636F6E6E6563746F722D6375727661747572653D2230220A
                20202020202020202020643D226D33322E393637203635392E363476332E3234
                3631682D322E3935353876352E3631383268322E3935353876332E3237313168
                32382E343137762D31322E313335682D32382E3431377A220A2020202020202F
                3E3C2F670A202020203E3C670A202020202020202069643D2274657874343035
                33220A20202020202020207374796C653D2266696C6C3A23656565656565220A
                20202020202020207472616E73666F726D3D226D6174726978282E3236323230
                20302030202E3236323230203338322E3136203330352E303629220A20202020
                20203E3C706174680A2020202020202020202069643D22706174683430353822
                0A20202020202020202020643D226D33332E333032203636332E3133632D302E
                3030303030312D322E3938353620302E35333536392D352E3037343820312E36
                3037312D362E3236373620312E303738352D312E3139323820322E373137372D
                312E3738393220342E393137362D312E3738393220312E3035373120302E3030
                30303120312E3932343920302E313332313520322E3630333520302E33393634
                3120302E363738353320302E323537313420312E3233323120302E3539363431
                20312E3636303620312E3031373820302E343238353420302E34313432382030
                2E373634323420302E383533353520312E3030373120312E3331373820302E32
                3439393820302E343537313320302E343439393720302E393932383220302E35
                3939393720312E3630373120302E323932383320312E3137313420302E343339
                323520322E3339323820302E343339323720332E363634312D302E3030303031
                3420322E383439392D302E343832313420342E393335352D312E343436342036
                2E32353639732D322E3632343920312E393832312D342E3938313920312E3938
                3231632D312E3332313420302D322E333839322D302E32313037312D332E3230
                33342D302E36333231322D302E38313432352D302E34323134312D312E343832
                312D312E303339322D322E303033352D312E383533352D302E33373835362D30
                2E35373835342D302E36373439372D312E333637382D302E38383932352D322E
                333637382D302E32303731332D312E303037312D302E333130372D322E313137
                382D302E333130372D332E3333326D342E3332383420302E30313037632D302E
                30303030303520312E3939393920302E313734393920332E3336373720302E35
                3234393820342E3130333420302E333537313220302E373238353420302E3837
                31333820312E3039323820312E3534323820312E3039323820302E3434323833
                203020302E38323439362D302E313533353620312E313436342D302E34363037
                20302E33323835352D302E333134323620302E35363738322D302E3830373120
                302E37313738332D312E3437383520302E31353731332D302E3637313420302E
                323335372D312E3731373820302E323335372D332E313339322D302E30303030
                30392D322E303835362D302E31373835372D332E343835362D302E3533353639
                2D342E313939382D302E33343939392D302E37323133392D302E38373835342D
                312E303832312D312E353835362D312E303832312D302E3732313420302E3030
                3030312D312E3234323820302E33363738352D312E3536343220312E31303335
                2D302E333231343220302E37323835352D302E343832313320322E303832312D
                302E343832313220342E30363035220A20202020202020202020696E6B736361
                70653A636F6E6E6563746F722D6375727661747572653D2230220A2020202020
                202F3E3C706174680A2020202020202020202069643D22706174683430363022
                0A20202020202020202020643D226D34372E393538203636332E3133632D302E
                3030303030312D322E3938353620302E35333536392D352E3037343820312E36
                3037312D362E3236373620312E303738352D312E3139323820322E373137372D
                312E3738393220342E393137362D312E3738393220312E3035373120302E3030
                30303120312E3932343920302E313332313520322E3630333520302E33393634
                3120302E363738353320302E323537313420312E3233323120302E3539363431
                20312E3636303620312E3031373820302E343238353420302E34313432382030
                2E373634323420302E383533353520312E3030373120312E3331373820302E32
                3439393820302E343537313320302E343439393720302E393932383220302E35
                3939393720312E3630373120302E323932383320312E3137313420302E343339
                323520322E3339323820302E343339323720332E363634312D302E3030303031
                3420322E383439392D302E343832313420342E393335352D312E343436342036
                2E32353639732D322E3632343920312E393832312D342E3938313920312E3938
                3231632D312E3332313420302D322E333839322D302E32313037312D332E3230
                33342D302E36333231322D302E38313432352D302E34323134312D312E343832
                312D312E303339322D322E303033352D312E383533352D302E33373835362D30
                2E35373835342D302E36373439372D312E333637382D302E38383932352D322E
                333637382D302E32303731332D312E303037312D302E333130372D322E313137
                382D302E333130372D332E3333326D342E3332383420302E30313037632D302E
                30303030303520312E3939393920302E313734393920332E3336373720302E35
                3234393820342E3130333420302E333537313220302E373238353420302E3837
                31333820312E3039323820312E3534323820312E3039323820302E3434323833
                203020302E38323439362D302E313533353620312E313436342D302E34363037
                20302E33323835352D302E333134323620302E35363738322D302E3830373120
                302E37313738332D312E3437383520302E31353731332D302E3637313420302E
                32333536392D312E3731373820302E323335372D332E313339322D302E303030
                30312D322E303835362D302E31373835372D332E343835362D302E3533353639
                2D342E313939382D302E33343939392D302E37323133392D302E38373835342D
                312E303832312D312E353835362D312E303832312D302E3732313420302E3030
                3030312D312E3234323820302E33363738352D312E3536343220312E31303335
                2D302E333231343220302E37323835352D302E343832313320322E303832312D
                302E343832313220342E30363035220A20202020202020202020696E6B736361
                70653A636F6E6E6563746F722D6375727661747572653D2230220A2020202020
                202F3E3C706174680A2020202020202020202069643D22706174683430363222
                0A20202020202020202020643D226D36332E303433203635392E363768342E36
                36303576342E33373132682D342E36363035762D342E333731326D3020372E30
                30363868342E3636303576342E33373132682D342E36363035762D342E333731
                32220A20202020202020202020696E6B73636170653A636F6E6E6563746F722D
                6375727661747572653D2230220A2020202020202F3E3C706174680A20202020
                20202020202069643D227061746834303634220A20202020202020202020643D
                226D36392E393433203636332E3133632D302E3030303030312D322E39383536
                20302E35333536392D352E3037343820312E363037312D362E3236373620312E
                303738352D312E3139323820322E373137372D312E3738393220342E39313736
                2D312E3738393220312E3035373120302E303030303120312E3932343920302E
                313332313520322E3630333520302E333936343120302E363738353320302E32
                3537313420312E3233323120302E353936343120312E3636303620312E303137
                3820302E343238353420302E343134323820302E373634323420302E38353335
                3520312E3030373120312E3331373820302E323439393820302E343537313320
                302E343439393720302E393932383220302E353939393720312E363037312030
                2E323932383320312E3137313420302E343339323520322E3339323820302E34
                3339323720332E363634312D302E30303030313420322E383439392D302E3438
                32313420342E393335352D312E3434363420362E32353639732D322E36323439
                20312E393832312D342E3938313920312E39383231632D312E3332313420302D
                322E333839322D302E32313037312D332E323033342D302E36333231322D302E
                38313432352D302E34323134312D312E343832312D312E303339322D322E3030
                33352D312E383533352D302E33373835362D302E35373835342D302E36373439
                372D312E333637382D302E38383932352D322E333637382D302E32303731332D
                312E303037312D302E333130372D322E313137382D302E333130372D332E3333
                326D342E3332383420302E30313037632D302E30303030303520312E39393939
                20302E313734393920332E3336373720302E353234393820342E313033342030
                2E333537313220302E373238353420302E383731333820312E3039323820312E
                3534323820312E3039323820302E3434323833203020302E38323439362D302E
                313533353620312E313436342D302E3436303720302E33323835352D302E3331
                34323620302E35363738322D302E3830373120302E37313738332D312E343738
                3520302E31353731332D302E3637313420302E32333536392D312E3731373820
                302E323335372D332E313339322D302E30303030312D322E303835362D302E31
                373835372D332E343835362D302E35333536392D342E313939382D302E333439
                39392D302E37323133392D302E38373835342D312E303832312D312E35383536
                2D312E303832312D302E3732313420302E30303030312D312E3234323820302E
                33363738352D312E3536343220312E313033352D302E333231343220302E3732
                3835352D302E343832313320322E303832312D302E343832313220342E303630
                35220A20202020202020202020696E6B73636170653A636F6E6E6563746F722D
                6375727661747572653D2230220A2020202020202F3E3C706174680A20202020
                20202020202069643D227061746834303636220A20202020202020202020643D
                226D38342E353939203636332E3133632D302E3030303030312D322E39383536
                20302E35333536392D352E3037343820312E363037312D362E3236373620312E
                303738352D312E3139323820322E373137372D312E3738393220342E39313736
                2D312E3738393220312E3035373120302E303030303120312E3932343920302E
                313332313520322E3630333520302E333936343120302E363738353320302E32
                3537313420312E3233323120302E353936343120312E3636303620312E303137
                3820302E343238353420302E343134323820302E373634323420302E38353335
                3520312E3030373120312E3331373820302E323439393820302E343537313320
                302E343439393720302E393932383220302E353939393720312E363037312030
                2E323932383320312E3137313420302E343339323520322E3339323820302E34
                3339323720332E363634312D302E30303030313420322E383439392D302E3438
                32313420342E393335352D312E3434363420362E32353639732D322E36323439
                20312E393832312D342E3938313920312E39383231632D312E3332313420302D
                322E333839322D302E32313037312D332E323033342D302E36333231322D302E
                38313432352D302E34323134312D312E343832312D312E303339322D322E3030
                33352D312E383533352D302E33373835362D302E35373835342D302E36373439
                372D312E333637382D302E38383932352D322E333637382D302E32303731342D
                312E303037312D302E333130372D322E313137382D302E333130372D332E3333
                326D342E3332383420302E30313037632D302E30303030303520312E39393939
                20302E313734393920332E3336373720302E353234393820342E313033342030
                2E333537313220302E373238353420302E383731333820312E3039323820312E
                3534323820312E3039323820302E3434323833203020302E38323439362D302E
                313533353620312E313436342D302E3436303720302E33323835352D302E3331
                34323620302E35363738322D302E3830373120302E37313738332D312E343738
                3520302E31353731332D302E3637313420302E32333536392D312E3731373820
                302E323335372D332E313339322D302E30303030312D322E303835362D302E31
                373835372D332E343835362D302E35333536392D342E313939382D302E333439
                39392D302E37323133392D302E38373835342D312E303832312D312E35383536
                2D312E303832312D302E3732313420302E30303030312D312E3234323820302E
                33363738352D312E3536343220312E313033352D302E333231343220302E3732
                3835352D302E343832313320322E303832312D302E343832313220342E303630
                35220A20202020202020202020696E6B73636170653A636F6E6E6563746F722D
                6375727661747572653D2230220A2020202020202F3E3C2F670A202020203E3C
                670A202020202020202069643D22673230333238220A20202020202020207472
                616E73666F726D3D226D6174726978282E313636303720302030202E31363630
                37203431322E3833203339332E343129220A2020202020203E3C706174680A20
                20202020202020202069643D22706174683230323530220A2020202020202020
                2020736F6469706F64693A72783D223730220A20202020202020202020736F64
                69706F64693A72793D223730220A202020202020202020207374796C653D2266
                696C6C3A23656565656565220A20202020202020202020736F6469706F64693A
                747970653D22617263220A20202020202020202020643D226D3431342E323920
                3339362E363563302033382E36362D33312E33342037302D3730203730732D37
                302D33312E33342D37302D37302033312E33342D37302037302D373020373020
                33312E33342037302037307A220A202020202020202020207472616E73666F72
                6D3D226D6174726978282E30353033313520302030202E303435353231202D32
                39322E3236203530392E363629220A20202020202020202020736F6469706F64
                693A63793D223339362E3634373839220A20202020202020202020736F646970
                6F64693A63783D223334342E3238353731220A2020202020202F3E3C70617468
                0A2020202020202020202069643D22706174683230323532220A202020202020
                202020207374796C653D2266696C6C3A23656565656565220A20202020202020
                202020696E6B73636170653A636F6E6E6563746F722D6375727661747572653D
                2230220A20202020202020202020643D226D2D3237342E3231203530352E3634
                632D362E34303420302D31322E30373420322E323032362D31352E3535382035
                2E353838346C322E3838373720342E30323763322E353335312D332E30363436
                20372E323538372D352E313235312031322E36372D352E3132353120352E3031
                203020392E3433313320312E373637362031322E30373220342E343631386C32
                2E393933382D332E38323239632D332E353132382D332E313235322D382E3935
                35362D352E313239322D31352E3036362D352E313239327A220A202020202020
                2F3E3C706174680A2020202020202020202069643D2270617468323032353422
                0A202020202020202020207374796C653D2266696C6C3A23656565656565220A
                20202020202020202020696E6B73636170653A636F6E6E6563746F722D637572
                7661747572653D2230220A20202020202020202020643D226D2D3237342E3732
                203531352E3732632D332E3634323620302D362E3936373920312E323236372D
                392E3439373920332E323435336C322E3433383820332E3430323463312E3930
                36342D312E3435323320342E333638382D322E3332383820372E303539322D32
                2E3332383820322E35323536203020342E3834393720302E373731363720362E
                3730323720322E303637366C322E353837372D332E33303635632D322E353033
                342D312E393231352D352E373436342D332E30382D392E323930342D332E3038
                7A220A2020202020202F3E3C706174680A2020202020202020202069643D2270
                6174683230323536220A20202020202020202020643D226D2D3237342E323920
                3439352E3634632D382E3431363620302D31352E39373720322E393437342D32
                312E31383420372E363239356C322E3733363620332E3831343763332E383438
                2D342E303330312031302E3631332D362E373036392031382E3332312D362E37
                30363920372E3634383620302031342E33373220322E363334372031382E3233
                3620362E3631336C322E393832352D332E38303636632D352E323035362D342E
                363332362D31322E3732362D372E353433382D32312E3039322D372E35343338
                7A220A202020202020202020207374796C653D2266696C6C3A23656565656565
                220A20202020202020202020696E6B73636170653A636F6E6E6563746F722D63
                75727661747572653D2230220A2020202020202F3E3C2F670A202020203E3C2F
                670A20203E0A2F2F204E6F7576656C6C652042616C69736520666F726569676E
                4F626A6563740A203C670A20202020202069643D224E657742616C220A202020
                2020207472616E73666F726D3D227472616E736C617465283430203133302920
                7363616C65282E3529220A202020203E3C666F726569676E4F626A6563740A20
                2020202020202069643D22666F726569676E4F626A6563743333220A20202020
                20202020793D223234220A2020202020202020783D223134220A202020202020
                20206865696768743D223835307078220A202020202020202077696474683D22
                3631367078220A2020202020203E3C7868746D6C3A626F64790A202020202020
                20203E3C7868746D6C3A696672616D650A202020202020202020202020737263
                3D22687474703A2F2F7777772E6F70656E636C69706172742E6F72672F70656F
                706C652F66696C7472652F220A2020202020202020202020207374796C653D22
                77696474683A313030253B6865696768743A3833387078220A20202020202020
                202F3E3C2F7868746D6C3A626F64790A2020202020203E3C2F666F726569676E
                4F626A6563740A202020203E3C2F670A20203E0A203C670A2020202020206964
                3D2267323431343639220A2020202020207472616E73666F726D3D227363616C
                65283229207472616E736C617465282D323430202D34303029220A202020203E
                3C706174680A202020202020202069643D227265637433383032220A20202020
                20202020643D226D3334352E3838203435392E32352032392E3132203136352E
                31396833362E35363263322E323136203020342D312E37383420342D34762D31
                35372E313963302D322E3231362D312E3738342D342D342D34682D36352E3638
                387A220A20202020202020207374796C653D226F7061636974793A2E393B6669
                6C6C3A75726C28236C696E6561724772616469656E7432343034313929220A20
                20202020202020696E6B73636170653A636F6E6E6563746F722D637572766174
                7572653D2230220A202020202F3E3C2F670A20203E0A200A3C6D657461646174
                610A202020203E3C7264663A5244460A2020202020203E3C63633A576F726B0A
                20202020202020203E3C64633A666F726D61740A202020202020202020203E69
                6D6167652F7376672B786D6C3C2F64633A666F726D61740A2020202020202020
                3E3C64633A747970650A2020202020202020202020207264663A7265736F7572
                63653D22687474703A2F2F7075726C2E6F72672F64632F64636D69747970652F
                5374696C6C496D616765220A20202020202020202F3E3C63633A6C6963656E73
                650A2020202020202020202020207264663A7265736F757263653D2268747470
                3A2F2F6372656174697665636F6D6D6F6E732E6F72672F6C6963656E7365732F
                7075626C6963646F6D61696E2F220A20202020202020202F3E3C64633A707562
                6C69736865720A202020202020202020203E3C63633A4167656E740A20202020
                202020202020202020207264663A61626F75743D22687474703A2F2F6F70656E
                636C69706172742E6F72672F220A2020202020202020202020203E3C64633A74
                69746C650A20202020202020202020202020203E4F70656E636C69706172743C
                2F64633A7469746C650A2020202020202020202020203E3C2F63633A4167656E
                740A202020202020202020203E3C2F64633A7075626C69736865720A20202020
                202020203E3C64633A7469746C650A202020202020202020203E54686520496E
                6372656469626C65204A61766173637269707420416E64726F69642050686F6E
                652042726F77736572203C2F64633A7469746C650A20202020202020203E3C64
                633A646174650A202020202020202020203E323031302D30392D31375431373A
                30303A32303C2F64633A646174650A20202020202020203E3C64633A64657363
                72697074696F6E0A202020202020202020203E54686520496E6372656469626C
                65204A61766173637269707420416E64726F69642050686F6E652042726F7773
                6572207573696E672053564720466F726569676E4F626A6563742E2028416E64
                726F69642050686F6E652062792073686F6B756E696E29262331333B5C6E596F
                75722062726F77736572206D75737420737570706F72742074686520466F7265
                69676E4F626A65637420535647207461672E3C2F64633A646573637269707469
                6F6E0A20202020202020203E3C64633A736F757263650A202020202020202020
                203E68747470733A2F2F6F70656E636C69706172742E6F72672F64657461696C
                2F38353233372F7468652D696E6372656469626C652D6A617661736372697074
                2D616E64726F69642D70686F6E652D62726F777365722D2D62792D66696C7472
                653C2F64633A736F757263650A20202020202020203E3C64633A63726561746F
                720A202020202020202020203E3C63633A4167656E740A202020202020202020
                2020203E3C64633A7469746C650A20202020202020202020202020203E66696C
                7472653C2F64633A7469746C650A2020202020202020202020203E3C2F63633A
                4167656E740A202020202020202020203E3C2F64633A63726561746F720A2020
                2020202020203E3C64633A7375626A6563740A202020202020202020203E3C72
                64663A4261670A2020202020202020202020203E3C7264663A6C690A20202020
                202020202020202020203E416E64726F69643C2F7264663A6C690A2020202020
                202020202020203E3C7264663A6C690A20202020202020202020202020203E42
                726F777365723C2F7264663A6C690A2020202020202020202020203E3C726466
                3A6C690A20202020202020202020202020203E466F726569676E4F626A656374
                3C2F7264663A6C690A2020202020202020202020203E3C7264663A6C690A2020
                2020202020202020202020203E4A6176617363726970743C2F7264663A6C690A
                2020202020202020202020203E3C7264663A6C690A2020202020202020202020
                2020203E50686F6E653C2F7264663A6C690A2020202020202020202020203E3C
                7264663A6C690A20202020202020202020202020203E5356473C2F7264663A6C
                690A2020202020202020202020203E3C7264663A6C690A202020202020202020
                20202020203E696672616D653C2F7264663A6C690A2020202020202020202020
                203E3C7264663A6C690A20202020202020202020202020203E7868746D6C3C2F
                7264663A6C690A2020202020202020202020203E3C2F7264663A4261670A2020
                20202020202020203E3C2F64633A7375626A6563740A20202020202020203E3C
                2F63633A576F726B0A2020202020203E3C63633A4C6963656E73650A20202020
                2020202020207264663A61626F75743D22687474703A2F2F6372656174697665
                636F6D6D6F6E732E6F72672F6C6963656E7365732F7075626C6963646F6D6169
                6E2F220A20202020202020203E3C63633A7065726D6974730A20202020202020
                20202020207264663A7265736F757263653D22687474703A2F2F637265617469
                7665636F6D6D6F6E732E6F72672F6E7323526570726F64756374696F6E220A20
                202020202020202F3E3C63633A7065726D6974730A2020202020202020202020
                207264663A7265736F757263653D22687474703A2F2F6372656174697665636F
                6D6D6F6E732E6F72672F6E7323446973747269627574696F6E220A2020202020
                2020202F3E3C63633A7065726D6974730A202020202020202020202020726466
                3A7265736F757263653D22687474703A2F2F6372656174697665636F6D6D6F6E
                732E6F72672F6E732344657269766174697665576F726B73220A202020202020
                20202F3E3C2F63633A4C6963656E73650A2020202020203E3C2F7264663A5244
                460A202020203E3C2F6D657461646174610A20203E3C2F7376670A3E0A}
              Proportional = True
              Stretch = True
              Transparent = True
            end
            object imGalleryTWSVGImage: TWSVGImage
              AlignWithMargins = True
              Left = 210
              Top = 0
              Width = 200
              Height = 200
              Margins.Left = 10
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C7376672076657273696F6E3D22312E312220
                786D6C6E733D22687474703A2F2F7777772E77332E6F72672F323030302F7376
                672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E77332E6F72
                672F313939392F786C696E6B2220783D223070782220793D2230707822207769
                6474683D22343530707822206865696768743D22323739707822207669657742
                6F783D2230203020343530203237392220656E61626C652D6261636B67726F75
                6E643D226E65772030203020343530203237392220786D6C3A73706163653D22
                7072657365727665223E0D0A3C672069643D224C617965725F31223E0D0A0D0A
                093C6C696E652066696C6C3D226E6F6E6522207374726F6B653D222336433534
                433922207374726F6B652D77696474683D223622207374726F6B652D6D697465
                726C696D69743D223130222078313D2239222079313D22323137222078323D22
                3831222079323D2231373222206F7061636974793D222E3635223E0D0A09093C
                616E696D617465206174747269627574654E616D653D227931222066726F6D3D
                223231372220746F3D223231372220626567696E3D22307322206475723D2234
                73222076616C7565733D223231373B3234343B3234343B3230383B3230383B32
                31373B32313722206B657953706C696E65733D22302E3120302E3820302E3220
                313B20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B
                20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B2030
                2E3120302E3820302E32203122206B657954696D65733D22303B302E32323B30
                2E33333B302E35353B302E36363B302E38383B31222063616C634D6F64653D22
                73706C696E652220726570656174436F756E743D22696E646566696E69746522
                2F3E0D0A09093C616E696D617465206174747269627574654E616D653D227932
                222066726F6D3D223137322220746F3D223137322220626567696E3D22307322
                206475723D223473222076616C7565733D223137323B3233353B3233353B3231
                373B3231373B3137323B31373222206B657953706C696E65733D22302E312030
                2E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E38
                20302E3220313B20302E3120302E3820302E3220313B20302E3120302E382030
                2E3220313B20302E3120302E3820302E32203122206B657954696D65733D2230
                3B302E32323B302E33333B302E35353B302E36363B302E38383B31222063616C
                634D6F64653D2273706C696E652220726570656174436F756E743D22696E6465
                66696E697465222F3E0D0A093C2F6C696E653E0D0A0D0A093C6C696E65206669
                6C6C3D226E6F6E6522207374726F6B653D222336433534433922207374726F6B
                652D77696474683D223622207374726F6B652D6D697465726C696D69743D2231
                30222078313D223831222079313D22313732222078323D22313533222079323D
                2231383122206F7061636974793D222E3635223E0D0A09093C616E696D617465
                206174747269627574654E616D653D227931222066726F6D3D22313732222074
                6F3D223137322220626567696E3D22307322206475723D223473222076616C75
                65733D223137323B3233353B3233353B3231373B3231373B3137323B31373222
                206B657953706C696E65733D22302E3120302E3820302E3220313B20302E3120
                302E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E
                3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E3820
                302E32203122206B657954696D65733D22303B302E32323B302E33333B302E35
                353B302E36363B302E38383B31222063616C634D6F64653D2273706C696E6522
                20726570656174436F756E743D22696E646566696E697465222F3E0D0A09093C
                616E696D617465206174747269627574654E616D653D227932222066726F6D3D
                223135332220746F3D223135332220626567696E3D22307322206475723D2234
                73222076616C7565733D223135333B39393B39393B3134343B3134343B313533
                3B31353322206B657953706C696E65733D22302E3120302E3820302E3220313B
                20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B2030
                2E3120302E3820302E3220313B20302E3120302E3820302E3220313B20302E31
                20302E3820302E32203122206B657954696D65733D22303B302E32323B302E33
                333B302E35353B302E36363B302E38383B31222063616C634D6F64653D227370
                6C696E652220726570656174436F756E743D22696E646566696E697465222F3E
                0D0A093C2F6C696E653E0D0A0D0A093C6C696E652066696C6C3D226E6F6E6522
                207374726F6B653D222336433534433922207374726F6B652D77696474683D22
                3622207374726F6B652D6D697465726C696D69743D223130222078313D223135
                33222079313D22313831222078323D22323235222079323D2231343522206F70
                61636974793D222E3635223E0D0A09093C616E696D6174652061747472696275
                74654E616D653D227931222066726F6D3D223135332220746F3D223135332220
                626567696E3D22307322206475723D223473222076616C7565733D223135333B
                39393B39393B3134343B3134343B3135333B31353322206B657953706C696E65
                733D22302E3120302E3820302E3220313B20302E3120302E3820302E3220313B
                20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B2030
                2E3120302E3820302E3220313B20302E3120302E3820302E32203122206B6579
                54696D65733D22303B302E32323B302E33333B302E35353B302E36363B302E38
                383B31222063616C634D6F64653D2273706C696E652220726570656174436F75
                6E743D22696E646566696E697465222F3E0D0A09093C616E696D617465206174
                747269627574654E616D653D227932222066726F6D3D223134352220746F3D22
                3134352220626567696E3D22307322206475723D223473222076616C7565733D
                223134353B36333B36333B3130383B3130383B3134353B31343522206B657953
                706C696E65733D22302E3120302E3820302E3220313B20302E3120302E382030
                2E3220313B20302E3120302E3820302E3220313B20302E3120302E3820302E32
                20313B20302E3120302E3820302E3220313B20302E3120302E3820302E322031
                22206B657954696D65733D22303B302E32323B302E33333B302E35353B302E36
                363B302E38383B31222063616C634D6F64653D2273706C696E65222072657065
                6174436F756E743D22696E646566696E697465222F3E0D0A093C2F6C696E653E
                0D0A0D0A093C6C696E652066696C6C3D226E6F6E6522207374726F6B653D2223
                36433534433922207374726F6B652D77696474683D223622207374726F6B652D
                6D697465726C696D69743D223130222078313D22323235222079313D22313435
                222078323D22323937222079323D22353522206F7061636974793D222E363522
                3E0D0A09093C616E696D617465206174747269627574654E616D653D22793122
                2066726F6D3D223134352220746F3D223134352220626567696E3D2230732220
                6475723D223473222076616C7565733D223134353B36333B36333B3130383B31
                30383B3134353B31343522206B657953706C696E65733D22302E3120302E3820
                302E3220313B20302E3120302E3820302E3220313B20302E3120302E3820302E
                3220313B20302E3120302E3820302E3220313B20302E3120302E3820302E3220
                313B20302E3120302E3820302E32203122206B657954696D65733D22303B302E
                32323B302E33333B302E35353B302E36363B302E38383B31222063616C634D6F
                64653D2273706C696E652220726570656174436F756E743D22696E646566696E
                697465222F3E0D0A09093C616E696D617465206174747269627574654E616D65
                3D227932222066726F6D3D2235352220746F3D2235352220626567696E3D2230
                7322206475723D223473222076616C7565733D2235353B34353B34353B36333B
                36333B35353B353522206B657953706C696E65733D22302E3120302E3820302E
                3220313B20302E3120302E3820302E3220313B20302E3120302E3820302E3220
                313B20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B
                20302E3120302E3820302E32203122206B657954696D65733D22303B302E3232
                3B302E33333B302E35353B302E36363B302E38383B31222063616C634D6F6465
                3D2273706C696E652220726570656174436F756E743D22696E646566696E6974
                65222F3E0D0A093C2F6C696E653E0D0A0D0A093C6C696E652066696C6C3D226E
                6F6E6522207374726F6B653D222336433534433922207374726F6B652D776964
                74683D223622207374726F6B652D6D697465726C696D69743D22313022207831
                3D22323937222079313D223535222078323D22333639222079323D2237322220
                6F7061636974793D222E3635223E0D0A09093C616E696D617465206174747269
                627574654E616D653D227931222066726F6D3D2235352220746F3D2235352220
                626567696E3D22307322206475723D223473222076616C7565733D2235353B34
                353B34353B36333B36333B35353B353522206B657953706C696E65733D22302E
                3120302E3820302E3220313B20302E3120302E3820302E3220313B20302E3120
                302E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E
                3820302E3220313B20302E3120302E3820302E32203122206B657954696D6573
                3D22303B302E32323B302E33333B302E35353B302E36363B302E38383B312220
                63616C634D6F64653D2273706C696E652220726570656174436F756E743D2269
                6E646566696E697465222F3E0D0A09093C616E696D6174652061747472696275
                74654E616D653D227932222066726F6D3D2237322220746F3D22373222206265
                67696E3D22307322206475723D223473222076616C7565733D2237323B36333B
                36333B39393B39393B37323B373222206B657953706C696E65733D22302E3120
                302E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E
                3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E3820
                302E3220313B20302E3120302E3820302E32203122206B657954696D65733D22
                303B302E32323B302E33333B302E35353B302E36363B302E38383B3122206361
                6C634D6F64653D2273706C696E652220726570656174436F756E743D22696E64
                6566696E697465222F3E0D0A093C2F6C696E653E0D0A0D0A093C6C696E652066
                696C6C3D226E6F6E6522207374726F6B653D222336433534433922207374726F
                6B652D77696474683D223622207374726F6B652D6D697465726C696D69743D22
                3130222078313D22333639222079313D223732222078323D2234343122207932
                3D22343522206F7061636974793D222E3635223E0D0A09093C616E696D617465
                206174747269627574654E616D653D227931222066726F6D3D2237322220746F
                3D2237322220626567696E3D22307322206475723D223473222076616C756573
                3D2237323B36333B36333B39393B39393B37323B373222206B657953706C696E
                65733D22302E3120302E3820302E3220313B20302E3120302E3820302E322031
                3B20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B20
                302E3120302E3820302E3220313B20302E3120302E3820302E32203122206B65
                7954696D65733D22303B302E32323B302E33333B302E35353B302E36363B302E
                38383B31222063616C634D6F64653D2273706C696E652220726570656174436F
                756E743D22696E646566696E697465222F3E0D0A09093C616E696D6174652061
                74747269627574654E616D653D227932222066726F6D3D2234352220746F3D22
                34352220626567696E3D22307322206475723D223473222076616C7565733D22
                34353B33363B33363B35343B35343B34353B343522206B657953706C696E6573
                3D22302E3120302E3820302E3220313B20302E3120302E3820302E3220313B20
                302E3120302E3820302E3220313B20302E3120302E3820302E3220313B20302E
                3120302E3820302E3220313B20302E3120302E3820302E32203122206B657954
                696D65733D22303B302E32323B302E33333B302E35353B302E36363B302E3838
                3B31222063616C634D6F64653D2273706C696E652220726570656174436F756E
                743D22696E646566696E697465222F3E0D0A093C2F6C696E653E0D0A0D0A093C
                636972636C652066696C6C3D222346464646464622207374726F6B653D222336
                433534433922207374726F6B652D77696474683D223622207374726F6B652D6D
                697465726C696D69743D223130222063783D2239222063793D22323137222072
                3D2236223E0D0A09093C616E696D617465206174747269627574654E616D653D
                226379222066726F6D3D223231372220746F3D223231372220626567696E3D22
                307322206475723D223473222076616C7565733D223231373B3234343B323434
                3B3230383B3230383B3231373B32313722206B657953706C696E65733D22302E
                3120302E3820302E3220313B20302E3120302E3820302E3220313B20302E3120
                302E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E
                3820302E3220313B20302E3120302E3820302E32203122206B657954696D6573
                3D22303B302E32323B302E33333B302E35353B302E36363B302E38383B312220
                63616C634D6F64653D2273706C696E652220726570656174436F756E743D2269
                6E646566696E697465222F3E0D0A093C2F636972636C653E0D0A0D0A093C6369
                72636C652066696C6C3D222346464646464622207374726F6B653D2223364335
                34433922207374726F6B652D77696474683D223622207374726F6B652D6D6974
                65726C696D69743D223130222063783D223831222063793D223137322220723D
                2236223E0D0A09093C616E696D617465206174747269627574654E616D653D22
                6379222066726F6D3D223137322220746F3D223137322220626567696E3D2230
                7322206475723D223473222076616C7565733D223137323B3233353B3233353B
                3231373B3231373B3137323B31373222206B657953706C696E65733D22302E31
                20302E3820302E3220313B20302E3120302E3820302E3220313B20302E312030
                2E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E38
                20302E3220313B20302E3120302E3820302E32203122206B657954696D65733D
                22303B302E32323B302E33333B302E35353B302E36363B302E38383B31222063
                616C634D6F64653D2273706C696E652220726570656174436F756E743D22696E
                646566696E697465222F3E0D0A093C2F636972636C653E0D0A0D0A093C636972
                636C652066696C6C3D222346464646464622207374726F6B653D222336433534
                433922207374726F6B652D77696474683D223622207374726F6B652D6D697465
                726C696D69743D223130222063783D22313533222063793D223138312220723D
                2236223E0D0A09093C616E696D617465206174747269627574654E616D653D22
                6379222066726F6D3D223135332220746F3D223135332220626567696E3D2230
                7322206475723D223473222076616C7565733D223135333B39393B39393B3134
                343B3134343B3135333B31353322206B657953706C696E65733D22302E312030
                2E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E38
                20302E3220313B20302E3120302E3820302E3220313B20302E3120302E382030
                2E3220313B20302E3120302E3820302E32203122206B657954696D65733D2230
                3B302E32323B302E33333B302E35353B302E36363B302E38383B31222063616C
                634D6F64653D2273706C696E652220726570656174436F756E743D22696E6465
                66696E697465222F3E0D0A093C2F636972636C653E0D0A0D0A093C636972636C
                652066696C6C3D222346464646464622207374726F6B653D2223364335344339
                22207374726F6B652D77696474683D223622207374726F6B652D6D697465726C
                696D69743D223130222063783D22323235222063793D223134352220723D2236
                223E0D0A09093C616E696D617465206174747269627574654E616D653D226379
                222066726F6D3D223134352220746F3D223134352220626567696E3D22307322
                206475723D223473222076616C7565733D223134353B36333B36333B3130383B
                3130383B3134353B31343522206B657953706C696E65733D22302E3120302E38
                20302E3220313B20302E3120302E3820302E3220313B20302E3120302E382030
                2E3220313B20302E3120302E3820302E3220313B20302E3120302E3820302E32
                20313B20302E3120302E3820302E32203122206B657954696D65733D22303B30
                2E32323B302E33333B302E35353B302E36363B302E38383B31222063616C634D
                6F64653D2273706C696E652220726570656174436F756E743D22696E64656669
                6E697465222F3E0D0A093C2F636972636C653E0D0A0D0A093C636972636C6520
                66696C6C3D222346464646464622207374726F6B653D22233643353443392220
                7374726F6B652D77696474683D223622207374726F6B652D6D697465726C696D
                69743D223130222063783D22323937222063793D2235352220723D2236223E0D
                0A09093C616E696D617465206174747269627574654E616D653D226379222066
                726F6D3D2235352220746F3D2235352220626567696E3D22307322206475723D
                223473222076616C7565733D2235353B34353B34353B36333B36333B35353B35
                3522206B657953706C696E65733D22302E3120302E3820302E3220313B20302E
                3120302E3820302E3220313B20302E3120302E3820302E3220313B20302E3120
                302E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E
                3820302E32203122206B657954696D65733D22303B302E32323B302E33333B30
                2E35353B302E36363B302E38383B31222063616C634D6F64653D2273706C696E
                652220726570656174436F756E743D22696E646566696E697465222F3E0D0A09
                3C2F636972636C653E0D0A0D0A093C636972636C652066696C6C3D2223464646
                46464622207374726F6B653D222336433534433922207374726F6B652D776964
                74683D223622207374726F6B652D6D697465726C696D69743D22313022206378
                3D22333639222063793D2237322220723D2236223E0D0A09093C616E696D6174
                65206174747269627574654E616D653D226379222066726F6D3D223732222074
                6F3D2237322220626567696E3D22307322206475723D223473222076616C7565
                733D2237323B36333B36333B39393B39393B37323B373222206B657953706C69
                6E65733D22302E3120302E3820302E3220313B20302E3120302E3820302E3220
                313B20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B
                20302E3120302E3820302E3220313B20302E3120302E3820302E32203122206B
                657954696D65733D22303B302E32323B302E33333B302E35353B302E36363B30
                2E38383B31222063616C634D6F64653D2273706C696E65222072657065617443
                6F756E743D22696E646566696E697465222F3E0D0A093C2F636972636C653E0D
                0A0D0A093C636972636C652066696C6C3D222346464646464622207374726F6B
                653D222336433534433922207374726F6B652D77696474683D22362220737472
                6F6B652D6D697465726C696D69743D223130222063783D22343431222063793D
                2234352220723D2236223E0D0A09093C616E696D617465206174747269627574
                654E616D653D226379222066726F6D3D2234352220746F3D2234352220626567
                696E3D22307322206475723D223473222076616C7565733D2234353B33363B33
                363B35343B35343B34353B343522206B657953706C696E65733D22302E312030
                2E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E38
                20302E3220313B20302E3120302E3820302E3220313B20302E3120302E382030
                2E3220313B20302E3120302E3820302E32203122206B657954696D65733D2230
                3B302E32323B302E33333B302E35353B302E36363B302E38383B31222063616C
                634D6F64653D2273706C696E652220726570656174436F756E743D22696E6465
                66696E697465222F3E0D0A093C2F636972636C653E0D0A3C2F673E0D0A0D0A0D
                0A3C672069643D224C617965725F32223E0D0A093C6C696E652066696C6C3D22
                6E6F6E6522207374726F6B653D222342343530464622207374726F6B652D7769
                6474683D223622207374726F6B652D6D697465726C696D69743D223130222078
                313D2239222079313D22313831222078323D223831222079323D223230382220
                6F7061636974793D222E3635223E0D0A09093C616E696D617465206174747269
                627574654E616D653D227931222066726F6D3D223138312220746F3D22313831
                2220626567696E3D22307322206475723D223473222076616C7565733D223138
                313B3137313B3137313B3139303B3139303B3138313B31383122206B65795370
                6C696E65733D22302E3120302E3820302E3220313B20302E3120302E3820302E
                3220313B20302E3120302E3820302E3220313B20302E3120302E3820302E3220
                313B20302E3120302E3820302E3220313B20302E3120302E3820302E32203122
                206B657954696D65733D22303B302E32323B302E33333B302E35353B302E3636
                3B302E38383B31222063616C634D6F64653D2273706C696E6522207265706561
                74436F756E743D22696E646566696E697465222F3E0D0A09093C616E696D6174
                65206174747269627574654E616D653D227932222066726F6D3D223230382220
                746F3D223230382220626567696E3D22307322206475723D223473222076616C
                7565733D223230383B3136323B3136323B3139303B3139303B3230383B323038
                22206B657953706C696E65733D22302E3120302E3820302E3220313B20302E31
                20302E3820302E3220313B20302E3120302E3820302E3220313B20302E312030
                2E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E38
                20302E32203122206B657954696D65733D22303B302E32323B302E33333B302E
                35353B302E36363B302E38383B31222063616C634D6F64653D2273706C696E65
                2220726570656174436F756E743D22696E646566696E697465222F3E0D0A093C
                2F6C696E653E0D0A0D0A093C6C696E652066696C6C3D226E6F6E652220737472
                6F6B653D222342343530464622207374726F6B652D77696474683D2236222073
                74726F6B652D6D697465726C696D69743D223130222078313D22383122207931
                3D22323038222078323D22313533222079323D2231323722206F706163697479
                3D222E3635223E0D0A09093C616E696D617465206174747269627574654E616D
                653D227931222066726F6D3D223230382220746F3D223230382220626567696E
                3D22307322206475723D223473222076616C7565733D223230383B3136323B31
                36323B3139303B3139303B3230383B32303822206B657953706C696E65733D22
                302E3120302E3820302E3220313B20302E3120302E3820302E3220313B20302E
                3120302E3820302E3220313B20302E3120302E3820302E3220313B20302E3120
                302E3820302E3220313B20302E3120302E3820302E32203122206B657954696D
                65733D22303B302E32323B302E33333B302E35353B302E36363B302E38383B31
                222063616C634D6F64653D2273706C696E652220726570656174436F756E743D
                22696E646566696E697465222F3E0D0A09093C616E696D617465206174747269
                627574654E616D653D227932222066726F6D3D223132372220746F3D22313237
                2220626567696E3D22307322206475723D223473222076616C7565733D223132
                373B3131373B3131373B39303B39303B3132373B31323722206B657953706C69
                6E65733D22302E3120302E3820302E3220313B20302E3120302E3820302E3220
                313B20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B
                20302E3120302E3820302E3220313B20302E3120302E3820302E32203122206B
                657954696D65733D22303B302E32323B302E33333B302E35353B302E36363B30
                2E38383B31222063616C634D6F64653D2273706C696E65222072657065617443
                6F756E743D22696E646566696E697465222F3E0D0A093C2F6C696E653E0D0A0D
                0A093C6C696E652066696C6C3D226E6F6E6522207374726F6B653D2223423435
                30464622207374726F6B652D77696474683D223622207374726F6B652D6D6974
                65726C696D69743D223130222078313D22313533222079313D22313237222078
                323D22323235222079323D22383222206F7061636974793D222E3635223E0D0A
                09093C616E696D617465206174747269627574654E616D653D22793122206672
                6F6D3D223132372220746F3D223132372220626567696E3D2230732220647572
                3D223473222076616C7565733D223132373B3131373B3131373B39303B39303B
                3132373B31323722206B657953706C696E65733D22302E3120302E3820302E32
                20313B20302E3120302E3820302E3220313B20302E3120302E3820302E322031
                3B20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B20
                302E3120302E3820302E32203122206B657954696D65733D22303B302E32323B
                302E33333B302E35353B302E36363B302E38383B31222063616C634D6F64653D
                2273706C696E652220726570656174436F756E743D22696E646566696E697465
                222F3E0D0A09093C616E696D617465206174747269627574654E616D653D2279
                32222066726F6D3D2238322220746F3D2238322220626567696E3D2230732220
                6475723D223473222076616C7565733D2238323B3132373B3132373B3134353B
                3134353B38323B383222206B657953706C696E65733D22302E3120302E382030
                2E3220313B20302E3120302E3820302E3220313B20302E3120302E3820302E32
                20313B20302E3120302E3820302E3220313B20302E3120302E3820302E322031
                3B20302E3120302E3820302E32203122206B657954696D65733D22303B302E32
                323B302E33333B302E35353B302E36363B302E38383B31222063616C634D6F64
                653D2273706C696E652220726570656174436F756E743D22696E646566696E69
                7465222F3E0D0A093C2F6C696E653E0D0A0D0A093C6C696E652066696C6C3D22
                6E6F6E6522207374726F6B653D222342343530464622207374726F6B652D7769
                6474683D223622207374726F6B652D6D697465726C696D69743D223130222078
                313D22323235222079313D223832222078323D22323937222079323D22313138
                22206F7061636974793D222E3635223E0D0A09093C616E696D61746520617474
                7269627574654E616D653D227931222066726F6D3D2238322220746F3D223832
                2220626567696E3D22307322206475723D223473222076616C7565733D223832
                3B3132373B3132373B3134353B3134353B38323B383222206B657953706C696E
                65733D22302E3120302E3820302E3220313B20302E3120302E3820302E322031
                3B20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B20
                302E3120302E3820302E3220313B20302E3120302E3820302E32203122206B65
                7954696D65733D22303B302E32323B302E33333B302E35353B302E36363B302E
                38383B31222063616C634D6F64653D2273706C696E652220726570656174436F
                756E743D22696E646566696E697465222F3E0D0A09093C616E696D6174652061
                74747269627574654E616D653D227932222066726F6D3D223131382220746F3D
                223131382220626567696E3D22307322206475723D223473222076616C756573
                3D223131383B3130383B3130383B3132373B3132373B3131383B31313822206B
                657953706C696E65733D22302E3120302E3820302E3220313B20302E3120302E
                3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E3820
                302E3220313B20302E3120302E3820302E3220313B20302E3120302E3820302E
                32203122206B657954696D65733D22303B302E32323B302E33333B302E35353B
                302E36363B302E38383B31222063616C634D6F64653D2273706C696E65222072
                6570656174436F756E743D22696E646566696E697465222F3E0D0A093C2F6C69
                6E653E0D0A0D0A093C6C696E652066696C6C3D226E6F6E6522207374726F6B65
                3D222342343530464622207374726F6B652D77696474683D223622207374726F
                6B652D6D697465726C696D69743D223130222078313D22323937222079313D22
                313138222078323D22333639222079323D22343522206F7061636974793D222E
                3635223E0D0A09093C616E696D617465206174747269627574654E616D653D22
                7931222066726F6D3D223131382220746F3D223131382220626567696E3D2230
                7322206475723D223473222076616C7565733D223131383B3130383B3130383B
                3132373B3132373B3131383B31313822206B657953706C696E65733D22302E31
                20302E3820302E3220313B20302E3120302E3820302E3220313B20302E312030
                2E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E38
                20302E3220313B20302E3120302E3820302E32203122206B657954696D65733D
                22303B302E32323B302E33333B302E35353B302E36363B302E38383B31222063
                616C634D6F64653D2273706C696E652220726570656174436F756E743D22696E
                646566696E697465222F3E0D0A09093C616E696D617465206174747269627574
                654E616D653D227932222066726F6D3D2234352220746F3D2234352220626567
                696E3D22307322206475723D223473222076616C7565733D2234353B32373B32
                373B33363B33363B34353B343522206B657953706C696E65733D22302E312030
                2E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E38
                20302E3220313B20302E3120302E3820302E3220313B20302E3120302E382030
                2E3220313B20302E3120302E3820302E32203122206B657954696D65733D2230
                3B302E32323B302E33333B302E35353B302E36363B302E38383B31222063616C
                634D6F64653D2273706C696E652220726570656174436F756E743D22696E6465
                66696E697465222F3E0D0A093C2F6C696E653E0D0A0D0A093C6C696E65206669
                6C6C3D226E6F6E6522207374726F6B653D222342343530464622207374726F6B
                652D77696474683D223622207374726F6B652D6D697465726C696D69743D2231
                30222078313D22333639222079313D223435222078323D22343431222079323D
                22333622206F7061636974793D222E3635223E0D0A09093C616E696D61746520
                6174747269627574654E616D653D227931222066726F6D3D2234352220746F3D
                2234352220626567696E3D22307322206475723D223473222076616C7565733D
                2234353B32373B32373B33363B33363B34353B343522206B657953706C696E65
                733D22302E3120302E3820302E3220313B20302E3120302E3820302E3220313B
                20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B2030
                2E3120302E3820302E3220313B20302E3120302E3820302E32203122206B6579
                54696D65733D22303B302E32323B302E33333B302E35353B302E36363B302E38
                383B31222063616C634D6F64653D2273706C696E652220726570656174436F75
                6E743D22696E646566696E697465222F3E0D0A09093C616E696D617465206174
                747269627574654E616D653D227932222066726F6D3D2233362220746F3D2233
                362220626567696E3D22307322206475723D223473222076616C7565733D2233
                363B31323B31323B31383B31383B33363B333622206B657953706C696E65733D
                22302E3120302E3820302E3220313B20302E3120302E3820302E3220313B2030
                2E3120302E3820302E3220313B20302E3120302E3820302E3220313B20302E31
                20302E3820302E3220313B20302E3120302E3820302E32203122206B65795469
                6D65733D22303B302E32323B302E33333B302E35353B302E36363B302E38383B
                31222063616C634D6F64653D2273706C696E652220726570656174436F756E74
                3D22696E646566696E697465222F3E0D0A093C2F6C696E653E0D0A0D0A093C63
                6972636C652066696C6C3D222346464646464622207374726F6B653D22234234
                3530464622207374726F6B652D77696474683D223622207374726F6B652D6D69
                7465726C696D69743D223130222063783D2239222063793D223138312220723D
                2236223E0D0A09093C616E696D617465206174747269627574654E616D653D22
                6379222066726F6D3D223138312220746F3D223138312220626567696E3D2230
                7322206475723D223473222076616C7565733D223138313B3137313B3137313B
                3139303B3139303B3138313B31383122206B657953706C696E65733D22302E31
                20302E3820302E3220313B20302E3120302E3820302E3220313B20302E312030
                2E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E38
                20302E3220313B20302E3120302E3820302E32203122206B657954696D65733D
                22303B302E32323B302E33333B302E35353B302E36363B302E38383B31222063
                616C634D6F64653D2273706C696E652220726570656174436F756E743D22696E
                646566696E697465222F3E0D0A093C2F636972636C653E0D0A0D0A093C636972
                636C652066696C6C3D222346464646464622207374726F6B653D222342343530
                464622207374726F6B652D77696474683D223622207374726F6B652D6D697465
                726C696D69743D223130222063783D223831222063793D223230382220723D22
                36223E0D0A09093C616E696D617465206174747269627574654E616D653D2263
                79222066726F6D3D223230382220746F3D223230382220626567696E3D223073
                22206475723D223473222076616C7565733D223230383B3136323B3136323B31
                39303B3139303B3230383B32303822206B657953706C696E65733D22302E3120
                302E3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E
                3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E3820
                302E3220313B20302E3120302E3820302E32203122206B657954696D65733D22
                303B302E32323B302E33333B302E35353B302E36363B302E38383B3122206361
                6C634D6F64653D2273706C696E652220726570656174436F756E743D22696E64
                6566696E697465222F3E0D0A093C2F636972636C653E0D0A0D0A093C63697263
                6C652066696C6C3D222346464646464622207374726F6B653D22234234353046
                4622207374726F6B652D77696474683D223622207374726F6B652D6D69746572
                6C696D69743D223130222063783D22313533222063793D223132372220723D22
                36223E0D0A09093C616E696D617465206174747269627574654E616D653D2263
                79222066726F6D3D223132372220746F3D223132372220626567696E3D223073
                22206475723D223473222076616C7565733D223132373B3131373B3131373B39
                303B39303B3132373B31323722206B657953706C696E65733D22302E3120302E
                3820302E3220313B20302E3120302E3820302E3220313B20302E3120302E3820
                302E3220313B20302E3120302E3820302E3220313B20302E3120302E3820302E
                3220313B20302E3120302E3820302E32203122206B657954696D65733D22303B
                302E32323B302E33333B302E35353B302E36363B302E38383B31222063616C63
                4D6F64653D2273706C696E652220726570656174436F756E743D22696E646566
                696E697465222F3E0D0A093C2F636972636C653E0D0A0D0A093C636972636C65
                2066696C6C3D222346464646464622207374726F6B653D222342343530464622
                207374726F6B652D77696474683D223622207374726F6B652D6D697465726C69
                6D69743D223130222063783D22323235222063793D2238322220723D2236223E
                0D0A09093C616E696D617465206174747269627574654E616D653D2263792220
                66726F6D3D2238322220746F3D2238322220626567696E3D2230732220647572
                3D223473222076616C7565733D2238323B3132373B3132373B3134353B313435
                3B38323B383222206B657953706C696E65733D22302E3120302E3820302E3220
                313B20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B
                20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B2030
                2E3120302E3820302E32203122206B657954696D65733D22303B302E32323B30
                2E33333B302E35353B302E36363B302E38383B31222063616C634D6F64653D22
                73706C696E652220726570656174436F756E743D22696E646566696E69746522
                2F3E0D0A093C2F636972636C653E0D0A0D0A093C636972636C652066696C6C3D
                222346464646464622207374726F6B653D222342343530464622207374726F6B
                652D77696474683D223622207374726F6B652D6D697465726C696D69743D2231
                30222063783D22323937222063793D223131382220723D2236223E0D0A09093C
                616E696D617465206174747269627574654E616D653D226379222066726F6D3D
                223131382220746F3D223131382220626567696E3D22307322206475723D2234
                73222076616C7565733D223131383B3130383B3130383B3132373B3132373B31
                31383B31313822206B657953706C696E65733D22302E3120302E3820302E3220
                313B20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B
                20302E3120302E3820302E3220313B20302E3120302E3820302E3220313B2030
                2E3120302E3820302E32203122206B657954696D65733D22303B302E32323B30
                2E33333B302E35353B302E36363B302E38383B31222063616C634D6F64653D22
                73706C696E652220726570656174436F756E743D22696E646566696E69746522
                2F3E0D0A093C2F636972636C653E0D0A0D0A093C636972636C652066696C6C3D
                222346464646464622207374726F6B653D222342343530464622207374726F6B
                652D77696474683D223622207374726F6B652D6D697465726C696D69743D2231
                30222063783D22333639222063793D2234352220723D2236223E0D0A09093C61
                6E696D617465206174747269627574654E616D653D226379222066726F6D3D22
                34352220746F3D2234352220626567696E3D22307322206475723D2234732220
                76616C7565733D2234353B32373B32373B33363B33363B34353B343522206B65
                7953706C696E65733D22302E3120302E3820302E3220313B20302E3120302E38
                20302E3220313B20302E3120302E3820302E3220313B20302E3120302E382030
                2E3220313B20302E3120302E3820302E3220313B20302E3120302E3820302E32
                203122206B657954696D65733D22303B302E32323B302E33333B302E35353B30
                2E36363B302E38383B31222063616C634D6F64653D2273706C696E6522207265
                70656174436F756E743D22696E646566696E697465222F3E0D0A093C2F636972
                636C653E0D0A0D0A093C636972636C652066696C6C3D22234646464646462220
                7374726F6B653D222342343530464622207374726F6B652D77696474683D2236
                22207374726F6B652D6D697465726C696D69743D223130222063783D22343431
                222063793D2233362220723D2236223E0D0A09093C616E696D61746520617474
                7269627574654E616D653D226379222066726F6D3D2233362220746F3D223336
                2220626567696E3D22307322206475723D223473222076616C7565733D223336
                3B31323B31323B31383B31383B33363B333622206B657953706C696E65733D22
                302E3120302E3820302E3220313B20302E3120302E3820302E3220313B20302E
                3120302E3820302E3220313B20302E3120302E3820302E3220313B20302E3120
                302E3820302E3220313B20302E3120302E3820302E32203122206B657954696D
                65733D22303B302E32323B302E33333B302E35353B302E36363B302E38383B31
                222063616C634D6F64653D2273706C696E652220726570656174436F756E743D
                22696E646566696E697465222F3E0D0A093C2F636972636C653E0D0A3C2F673E
                0D0A3C2F7376673E}
              Proportional = True
              Stretch = True
              Transparent = True
              Animation.FrameCount = 25
              Animation.Position = 0
              Animation.Animate = True
            end
            object imGalleryTWSVGImageButton: TWSVGImageButton
              AlignWithMargins = True
              Left = 420
              Top = 0
              Width = 200
              Height = 200
              Margins.Left = 10
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                223F3E0A3C212D2D2053766720566563746F722049636F6E73203A2068747470
                3A2F2F7777772E6F6E6C696E65776562666F6E74732E636F6D2F69636F6E202D
                2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
                6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
                3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
                6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
                2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
                202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
                323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
                332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
                693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
                65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
                696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
                672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
                6E3D22312E31220A202020783D22307078220A202020793D22307078220A2020
                2076696577426F783D2230203020313030302031303030220A202020656E6162
                6C652D6261636B67726F756E643D226E65772030203020313030302031303030
                220A202020786D6C3A73706163653D227072657365727665220A20202069643D
                2273766732220A202020696E6B73636170653A76657273696F6E3D22302E3931
                20723133373235220A202020736F6469706F64693A646F636E616D653D226275
                74746F6E5F686F6D655F64656661756C742E737667223E3C646566730A202020
                202069643D2264656673343822202F3E3C736F6469706F64693A6E616D656476
                6965770A202020202070616765636F6C6F723D2223666666666666220A202020
                2020626F72646572636F6C6F723D2223363636363636220A2020202020626F72
                6465726F7061636974793D2231220A20202020206F626A656374746F6C657261
                6E63653D223130220A202020202067726964746F6C6572616E63653D22313022
                0A20202020206775696465746F6C6572616E63653D223130220A202020202069
                6E6B73636170653A706167656F7061636974793D2230220A2020202020696E6B
                73636170653A70616765736861646F773D2232220A2020202020696E6B736361
                70653A77696E646F772D77696474683D2231393230220A2020202020696E6B73
                636170653A77696E646F772D6865696768743D2231303238220A202020202069
                643D226E616D6564766965773436220A202020202073686F77677269643D2266
                616C7365220A2020202020696E6B73636170653A7A6F6F6D3D22302E32333622
                0A2020202020696E6B73636170653A63783D22353030220A2020202020696E6B
                73636170653A63793D22353030220A2020202020696E6B73636170653A77696E
                646F772D783D222D38220A2020202020696E6B73636170653A77696E646F772D
                793D222D38220A2020202020696E6B73636170653A77696E646F772D6D617869
                6D697A65643D2231220A2020202020696E6B73636170653A63757272656E742D
                6C617965723D227376673222202F3E3C6D657461646174610A20202020206964
                3D226D6574616461746134223E2053766720566563746F722049636F6E73203A
                20687474703A2F2F7777772E6F6E6C696E65776562666F6E74732E636F6D2F69
                636F6E203C7264663A5244463E3C63633A576F726B0A20202020207264663A61
                626F75743D22223E3C64633A666F726D61743E696D6167652F7376672B786D6C
                3C2F64633A666F726D61743E3C64633A747970650A202020202020207264663A
                7265736F757263653D22687474703A2F2F7075726C2E6F72672F64632F64636D
                69747970652F5374696C6C496D61676522202F3E3C2F63633A576F726B3E3C2F
                7264663A5244463E3C2F6D657461646174613E3C670A202020202069643D2267
                36220A20202020207374796C653D2266696C6C3A233462346234623B66696C6C
                2D6F7061636974793A31223E3C670A2020202020202069643D226738220A2020
                20202020207374796C653D2266696C6C3A233462346234623B66696C6C2D6F70
                61636974793A31223E3C670A20202020202020202069643D22673130220A2020
                202020202020207374796C653D2266696C6C3A233462346234623B66696C6C2D
                6F7061636974793A31223E3C706174680A2020202020202020202020643D224D
                3534372E362C3231382E32632D31322E342D31342E352D33302E362D32322E39
                2D34392E372D32322E39632D31392E312C302D33372E332C382E342D34392E37
                2C32322E394C3136392E342C353435632D31372E332C32302E332D32342E382C
                34372E322D32302E332C37332E356C35302E362C3330312E3963352E332C3331
                2E352C33322E352C35342E332C36342E342C35342E33683234332E32632D302E
                312D312E312C302E312D312E362C302E312D322E365637353363302D31342E32
                2C31312E332D32352E392C32352E362D32352E39683131372E326331342E322C
                302C32352E392C31312E372C32352E392C32352E39763232312E386835352E38
                6333312E392C302C35392E322D32322E392C36342E342D35342E336C35302E36
                2D33303263342E342D32362E332D332D35332E312D32302E342D37332E344C35
                34372E362C3231382E327A204D3433312E372C3833302E3863302C31342E322D
                31312E352C32352E382D32352E382C32352E38682D38392E35632D31342E322C
                302D32352E382D31312E352D32352E382D32352E38762D38392E3563302D3134
                2E322C31312E352D32352E382C32352E382D32352E386838392E356331342E32
                2C302C32352E382C31312E352C32352E382C32352E38563833302E387A220A20
                2020202020202020202069643D22706174683132220A20202020202020202020
                207374796C653D2266696C6C3A233462346234623B66696C6C2D6F7061636974
                793A3122202F3E3C706174680A2020202020202020202020643D224D3937362E
                312C3436394C3632392E372C38332E38433539362E322C34362E352C3534382C
                32342E362C3439372E392C32352E33632D35302E322C302E312D39382E312C32
                312E372D3133312E352C35392E324C32332E372C3436392E32632D31392E392C
                32322E342D31372E392C35362E372C342E342C37362E366331302E342C392E32
                2C32332E322C31332E372C33362E312C31332E376331342E392C302C32392E38
                2D362E312C34302E352D31382E326C3334322E372D3338342E366331322E392D
                31342E352C33312E342D32322E362C35302E372D32322E3663302E312C302C30
                2E312C302C302E322C306331392E332C302C33372E382C382C35302E372C3232
                2E346C3334362E342C3338352E326332302C32322E332C35342E342C32342E31
                2C37362E362C34433939342E332C3532352E372C3939362E312C3439312E332C
                3937362E312C3436397A220A202020202020202020202069643D227061746831
                34220A20202020202020202020207374796C653D2266696C6C3A233462346234
                623B66696C6C2D6F7061636974793A3122202F3E3C2F673E3C2F673E3C670A20
                20202020202069643D22673136220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673138220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673230220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673232220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673234220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673236220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673238220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673330220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673332220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673334220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673336220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673338220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673430220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673432220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C670A20
                20202020202069643D22673434220A202020202020207374796C653D2266696C
                6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C2F673E
                3C2F7376673E}
              Proportional = True
              Stretch = True
              Transparent = True
              Animation.FrameCount = 0
              Animation.Position = 0
              HoveredAnimation.FrameCount = 0
              HoveredAnimation.Position = 0
              HoveredPicture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                223F3E0A3C212D2D2053766720566563746F722049636F6E73203A2068747470
                3A2F2F7777772E6F6E6C696E65776562666F6E74732E636F6D2F69636F6E202D
                2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
                6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
                3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
                6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
                2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
                202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
                323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
                332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
                693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
                65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
                696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
                672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
                6E3D22312E31220A202020783D22307078220A202020793D22307078220A2020
                2076696577426F783D2230203020313030302031303030220A202020656E6162
                6C652D6261636B67726F756E643D226E65772030203020313030302031303030
                220A202020786D6C3A73706163653D227072657365727665220A20202069643D
                2273766732220A202020696E6B73636170653A76657273696F6E3D22302E3931
                20723133373235220A202020736F6469706F64693A646F636E616D653D226275
                74746F6E5F686F6D655F686F76657265642E737667223E3C646566730A202020
                202069643D2264656673343822202F3E3C736F6469706F64693A6E616D656476
                6965770A202020202070616765636F6C6F723D2223666666666666220A202020
                2020626F72646572636F6C6F723D2223363636363636220A2020202020626F72
                6465726F7061636974793D2231220A20202020206F626A656374746F6C657261
                6E63653D223130220A202020202067726964746F6C6572616E63653D22313022
                0A20202020206775696465746F6C6572616E63653D223130220A202020202069
                6E6B73636170653A706167656F7061636974793D2230220A2020202020696E6B
                73636170653A70616765736861646F773D2232220A2020202020696E6B736361
                70653A77696E646F772D77696474683D2231393230220A2020202020696E6B73
                636170653A77696E646F772D6865696768743D2231303238220A202020202069
                643D226E616D6564766965773436220A202020202073686F77677269643D2266
                616C7365220A2020202020696E6B73636170653A7A6F6F6D3D22302E32333622
                0A2020202020696E6B73636170653A63783D22353030220A2020202020696E6B
                73636170653A63793D22353030220A2020202020696E6B73636170653A77696E
                646F772D783D222D38220A2020202020696E6B73636170653A77696E646F772D
                793D222D38220A2020202020696E6B73636170653A77696E646F772D6D617869
                6D697A65643D2231220A2020202020696E6B73636170653A63757272656E742D
                6C617965723D227376673222202F3E3C6D657461646174610A20202020206964
                3D226D6574616461746134223E2053766720566563746F722049636F6E73203A
                20687474703A2F2F7777772E6F6E6C696E65776562666F6E74732E636F6D2F69
                636F6E203C7264663A5244463E3C63633A576F726B0A20202020207264663A61
                626F75743D22223E3C64633A666F726D61743E696D6167652F7376672B786D6C
                3C2F64633A666F726D61743E3C64633A747970650A202020202020207264663A
                7265736F757263653D22687474703A2F2F7075726C2E6F72672F64632F64636D
                69747970652F5374696C6C496D61676522202F3E3C64633A7469746C653E3C2F
                64633A7469746C653E3C2F63633A576F726B3E3C2F7264663A5244463E3C2F6D
                657461646174613E3C670A202020202069643D226736220A2020202020737479
                6C653D2266696C6C3A233063356530613B66696C6C2D6F7061636974793A3122
                3E3C670A2020202020202069643D226738220A202020202020207374796C653D
                2266696C6C3A233063356530613B66696C6C2D6F7061636974793A31223E3C67
                0A20202020202020202069643D22673130220A2020202020202020207374796C
                653D2266696C6C3A233063356530613B66696C6C2D6F7061636974793A31223E
                3C706174680A2020202020202020202020643D224D3534372E362C3231382E32
                632D31322E342D31342E352D33302E362D32322E392D34392E372D32322E3963
                2D31392E312C302D33372E332C382E342D34392E372C32322E394C3136392E34
                2C353435632D31372E332C32302E332D32342E382C34372E322D32302E332C37
                332E356C35302E362C3330312E3963352E332C33312E352C33322E352C35342E
                332C36342E342C35342E33683234332E32632D302E312D312E312C302E312D31
                2E362C302E312D322E365637353363302D31342E322C31312E332D32352E392C
                32352E362D32352E39683131372E326331342E322C302C32352E392C31312E37
                2C32352E392C32352E39763232312E386835352E386333312E392C302C35392E
                322D32322E392C36342E342D35342E336C35302E362D33303263342E342D3236
                2E332D332D35332E312D32302E342D37332E344C3534372E362C3231382E327A
                204D3433312E372C3833302E3863302C31342E322D31312E352C32352E382D32
                352E382C32352E38682D38392E35632D31342E322C302D32352E382D31312E35
                2D32352E382D32352E38762D38392E3563302D31342E322C31312E352D32352E
                382C32352E382D32352E386838392E356331342E322C302C32352E382C31312E
                352C32352E382C32352E38563833302E387A220A202020202020202020202069
                643D22706174683132220A20202020202020202020207374796C653D2266696C
                6C3A233063356530613B66696C6C2D6F7061636974793A3122202F3E3C706174
                680A2020202020202020202020643D224D3937362E312C3436394C3632392E37
                2C38332E38433539362E322C34362E352C3534382C32342E362C3439372E392C
                32352E33632D35302E322C302E312D39382E312C32312E372D3133312E352C35
                392E324C32332E372C3436392E32632D31392E392C32322E342D31372E392C35
                362E372C342E342C37362E366331302E342C392E322C32332E322C31332E372C
                33362E312C31332E376331342E392C302C32392E382D362E312C34302E352D31
                382E326C3334322E372D3338342E366331322E392D31342E352C33312E342D32
                322E362C35302E372D32322E3663302E312C302C302E312C302C302E322C3063
                31392E332C302C33372E382C382C35302E372C32322E346C3334362E342C3338
                352E326332302C32322E332C35342E342C32342E312C37362E362C3443393934
                2E332C3532352E372C3939362E312C3439312E332C3937362E312C3436397A22
                0A202020202020202020202069643D22706174683134220A2020202020202020
                2020207374796C653D2266696C6C3A233063356530613B66696C6C2D6F706163
                6974793A3122202F3E3C2F673E3C2F673E3C670A2020202020202069643D2267
                3136220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3138220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3230220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3232220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3234220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3236220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3238220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3330220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3332220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3334220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3336220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3338220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3430220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3432220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3434220A202020202020207374796C653D2266696C6C3A233063356530613B66
                696C6C2D6F7061636974793A3122202F3E3C2F673E3C2F7376673E}
              ClickedAnimation.FrameCount = 0
              ClickedAnimation.Position = 0
              ClickedPicture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                223F3E0A3C212D2D2053766720566563746F722049636F6E73203A2068747470
                3A2F2F7777772E6F6E6C696E65776562666F6E74732E636F6D2F69636F6E202D
                2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
                6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
                3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
                6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
                2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
                202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
                323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
                332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
                693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
                65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
                696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
                672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
                6E3D22312E31220A202020783D22307078220A202020793D22307078220A2020
                2076696577426F783D2230203020313030302031303030220A202020656E6162
                6C652D6261636B67726F756E643D226E65772030203020313030302031303030
                220A202020786D6C3A73706163653D227072657365727665220A20202069643D
                2273766732220A202020696E6B73636170653A76657273696F6E3D22302E3931
                20723133373235220A202020736F6469706F64693A646F636E616D653D226275
                74746F6E5F686F6D655F636C69636B65642E737667223E3C646566730A202020
                202069643D2264656673343822202F3E3C736F6469706F64693A6E616D656476
                6965770A202020202070616765636F6C6F723D2223666666666666220A202020
                2020626F72646572636F6C6F723D2223363636363636220A2020202020626F72
                6465726F7061636974793D2231220A20202020206F626A656374746F6C657261
                6E63653D223130220A202020202067726964746F6C6572616E63653D22313022
                0A20202020206775696465746F6C6572616E63653D223130220A202020202069
                6E6B73636170653A706167656F7061636974793D2230220A2020202020696E6B
                73636170653A70616765736861646F773D2232220A2020202020696E6B736361
                70653A77696E646F772D77696474683D2231393230220A2020202020696E6B73
                636170653A77696E646F772D6865696768743D2231303238220A202020202069
                643D226E616D6564766965773436220A202020202073686F77677269643D2266
                616C7365220A2020202020696E6B73636170653A7A6F6F6D3D22302E32333622
                0A2020202020696E6B73636170653A63783D22353030220A2020202020696E6B
                73636170653A63793D22353030220A2020202020696E6B73636170653A77696E
                646F772D783D222D38220A2020202020696E6B73636170653A77696E646F772D
                793D222D38220A2020202020696E6B73636170653A77696E646F772D6D617869
                6D697A65643D2231220A2020202020696E6B73636170653A63757272656E742D
                6C617965723D227376673222202F3E3C6D657461646174610A20202020206964
                3D226D6574616461746134223E2053766720566563746F722049636F6E73203A
                20687474703A2F2F7777772E6F6E6C696E65776562666F6E74732E636F6D2F69
                636F6E203C7264663A5244463E3C63633A576F726B0A20202020207264663A61
                626F75743D22223E3C64633A666F726D61743E696D6167652F7376672B786D6C
                3C2F64633A666F726D61743E3C64633A747970650A202020202020207264663A
                7265736F757263653D22687474703A2F2F7075726C2E6F72672F64632F64636D
                69747970652F5374696C6C496D61676522202F3E3C64633A7469746C653E3C2F
                64633A7469746C653E3C2F63633A576F726B3E3C2F7264663A5244463E3C2F6D
                657461646174613E3C670A202020202069643D226736220A2020202020737479
                6C653D2266696C6C3A233063623030613B66696C6C2D6F7061636974793A3122
                3E3C670A2020202020202069643D226738220A202020202020207374796C653D
                2266696C6C3A233063623030613B66696C6C2D6F7061636974793A31223E3C67
                0A20202020202020202069643D22673130220A2020202020202020207374796C
                653D2266696C6C3A233063623030613B66696C6C2D6F7061636974793A31223E
                3C706174680A2020202020202020202020643D224D3534372E362C3231382E32
                632D31322E342D31342E352D33302E362D32322E392D34392E372D32322E3963
                2D31392E312C302D33372E332C382E342D34392E372C32322E394C3136392E34
                2C353435632D31372E332C32302E332D32342E382C34372E322D32302E332C37
                332E356C35302E362C3330312E3963352E332C33312E352C33322E352C35342E
                332C36342E342C35342E33683234332E32632D302E312D312E312C302E312D31
                2E362C302E312D322E365637353363302D31342E322C31312E332D32352E392C
                32352E362D32352E39683131372E326331342E322C302C32352E392C31312E37
                2C32352E392C32352E39763232312E386835352E386333312E392C302C35392E
                322D32322E392C36342E342D35342E336C35302E362D33303263342E342D3236
                2E332D332D35332E312D32302E342D37332E344C3534372E362C3231382E327A
                204D3433312E372C3833302E3863302C31342E322D31312E352C32352E382D32
                352E382C32352E38682D38392E35632D31342E322C302D32352E382D31312E35
                2D32352E382D32352E38762D38392E3563302D31342E322C31312E352D32352E
                382C32352E382D32352E386838392E356331342E322C302C32352E382C31312E
                352C32352E382C32352E38563833302E387A220A202020202020202020202069
                643D22706174683132220A20202020202020202020207374796C653D2266696C
                6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C706174
                680A2020202020202020202020643D224D3937362E312C3436394C3632392E37
                2C38332E38433539362E322C34362E352C3534382C32342E362C3439372E392C
                32352E33632D35302E322C302E312D39382E312C32312E372D3133312E352C35
                392E324C32332E372C3436392E32632D31392E392C32322E342D31372E392C35
                362E372C342E342C37362E366331302E342C392E322C32332E322C31332E372C
                33362E312C31332E376331342E392C302C32392E382D362E312C34302E352D31
                382E326C3334322E372D3338342E366331322E392D31342E352C33312E342D32
                322E362C35302E372D32322E3663302E312C302C302E312C302C302E322C3063
                31392E332C302C33372E382C382C35302E372C32322E346C3334362E342C3338
                352E326332302C32322E332C35342E342C32342E312C37362E362C3443393934
                2E332C3532352E372C3939362E312C3439312E332C3937362E312C3436397A22
                0A202020202020202020202069643D22706174683134220A2020202020202020
                2020207374796C653D2266696C6C3A233063623030613B66696C6C2D6F706163
                6974793A3122202F3E3C2F673E3C2F673E3C670A2020202020202069643D2267
                3136220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3138220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3230220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3232220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3234220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3236220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3238220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3330220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3332220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3334220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3336220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3338220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3430220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3432220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D2267
                3434220A202020202020207374796C653D2266696C6C3A233063623030613B66
                696C6C2D6F7061636974793A3122202F3E3C2F673E3C2F7376673E}
              DisabledAnimation.FrameCount = 0
              DisabledAnimation.Position = 0
            end
          end
          object paGalleryLine1Header: TPanel
            Left = 0
            Top = 0
            Width = 634
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 2
            object laGalleryLine1Title: TLabel
              Left = 0
              Top = 0
              Width = 620
              Height = 25
              Align = alLeft
              Alignment = taCenter
              AutoSize = False
              Caption = 'Image components'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 1052688
              Font.Height = -20
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentColor = False
              ParentFont = False
              Layout = tlCenter
            end
          end
          object paGalleryLine1Desc: TPanel
            Left = 0
            Top = 250
            Width = 634
            Height = 55
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 3
            object paGalleryTImageDesc: TPanel
              Left = 0
              Top = 0
              Width = 200
              Height = 55
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 0
              object laGalleryTImageDesc: TLabel
                Left = 0
                Top = 0
                Width = 200
                Height = 55
                Align = alClient
                Alignment = taCenter
                Caption = 'A standard TImage component allows you to display any SVG image.'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                WordWrap = True
                ExplicitWidth = 183
                ExplicitHeight = 26
              end
            end
            object paGalleryTWSVGImageDesc: TPanel
              AlignWithMargins = True
              Left = 210
              Top = 0
              Width = 200
              Height = 55
              Margins.Left = 10
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 1
              object laGalleryTWSVGImageDesc: TLabel
                Left = 0
                Top = 0
                Width = 200
                Height = 55
                Align = alClient
                Alignment = taCenter
                Caption = 
                  'A TWSVGImage is identical to a TImage, except that it also suppo' +
                  'rts the animations.'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                WordWrap = True
                ExplicitWidth = 195
                ExplicitHeight = 39
              end
            end
            object paGalleryTWSVGImageButtonDesc: TPanel
              AlignWithMargins = True
              Left = 420
              Top = 0
              Width = 200
              Height = 55
              Margins.Left = 10
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 2
              object laGalleryTWSVGImageButtonDesc: TLabel
                Left = 0
                Top = 0
                Width = 200
                Height = 55
                Align = alClient
                Alignment = taCenter
                Caption = 
                  'A TWSVGImageButton is an advanced image that provides the same s' +
                  'tates and acts as a button.'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                WordWrap = True
                ExplicitWidth = 199
                ExplicitHeight = 39
              end
            end
          end
          object paGalleryLine1Resizeable: TPanel
            AlignWithMargins = True
            Left = 0
            Top = 325
            Width = 634
            Height = 250
            Margins.Left = 0
            Margins.Top = 20
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 4
            object paGalleryLine1ResizeableLeft: TPanel
              Left = 0
              Top = 0
              Width = 317
              Height = 250
              Align = alLeft
              BevelOuter = bvNone
              ParentBackground = False
              ParentColor = True
              TabOrder = 0
              object paGalleryLine1ResizeableImageControls: TPanel
                Left = 0
                Top = 0
                Width = 25
                Height = 250
                Align = alLeft
                AutoSize = True
                BevelOuter = bvNone
                TabOrder = 0
                object tbGalleryLine1ResizeableImage: TTrackBar
                  AlignWithMargins = True
                  Left = 0
                  Top = 0
                  Width = 25
                  Height = 250
                  Margins.Left = 0
                  Margins.Top = 0
                  Margins.Right = 0
                  Margins.Bottom = 0
                  Align = alLeft
                  Ctl3D = True
                  Max = 2500
                  Min = 50
                  Orientation = trVertical
                  ParentCtl3D = False
                  Position = 250
                  ShowSelRange = False
                  TabOrder = 0
                  TickStyle = tsNone
                  OnChange = tbGalleryLine1ResizeableImageChange
                end
              end
              object paGalleryLine1ResizeableImageView: TPanel
                Left = 25
                Top = 0
                Width = 292
                Height = 250
                Align = alClient
                BevelOuter = bvNone
                Color = clWhite
                ParentBackground = False
                TabOrder = 1
                object imGalleryLine1ResizeableImage: TImage
                  Left = 0
                  Top = 0
                  Width = 292
                  Height = 250
                  Center = True
                  Picture.Data = {
                    0C5457535647477261706869633C7376672069643D22737667322220786D6C6E
                    733D22687474703A2F2F7777772E77332E6F72672F323030302F737667222076
                    696577426F783D223020302039303020393030222076657273696F6E3D22312E
                    31223E0D0A203C672069643D226734222066696C6C3D226E6F6E652220747261
                    6E73666F726D3D226D617472697828312E373635363436332C302C302C312E37
                    3635363436332C3332342E39303731362C3235352E303039343229223E0D0A20
                    203C672069643D22673622207374726F6B652D77696474683D22302E31373230
                    3030303122207374726F6B653D2223303030222066696C6C3D2223464646223E
                    0D0A2020203C706174682069643D2270617468382220643D226D2D3132322E33
                    2C38342E32383573302E312C312E3839342D302E37332C312E383735632D302E
                    38322D302E3031392D31372E32372D34382E3039342D33372E382D34352E3835
                    312C302C302C31372E37382D372E3335332C33382E35332C34332E3937367A22
                    2F3E0D0A20203C2F673E0D0A20203C672069643D2267313022207374726F6B65
                    2D77696474683D22302E313732303030303122207374726F6B653D2223303030
                    222066696C6C3D2223464646223E0D0A2020203C706174682069643D22706174
                    6831322220643D226D2D3131382E37372C38312E323632732D302E35352C312E
                    3831362D312E33322C312E353137632D302E37372D302E3239382C302E31312D
                    35312E3130342D31392E39352D35352E3937382C302C302C31392E32322D302E
                    3836342C32312E32372C35342E3436317A222F3E0D0A20203C2F673E0D0A2020
                    3C672069643D2267313422207374726F6B652D77696474683D22302E31373230
                    3030303122207374726F6B653D2223303030222066696C6C3D2223464646223E
                    0D0A2020203C706174682069643D227061746831362220643D226D2D39312E32
                    38342C3132332E353973312E3633362C302E39362C312E3136362C312E363463
                    2D302E3437312C302E36372D34392E3634322D31322E31332D35392E3130322C
                    362E32332C302C302C332E36382D31382E38392C35372E3933362D372E38377A
                    222F3E0D0A20203C2F673E0D0A20203C672069643D2267313822207374726F6B
                    652D77696474683D22302E313732303030303122207374726F6B653D22233030
                    30222066696C6C3D2223464646223E0D0A2020203C706174682069643D227061
                    746832302220643D226D2D39342E3039332C3133332E3873312E3835362C302E
                    342C312E3632322C312E3139632D302E3233332C302E37392D35302E3933392C
                    342E31332D35342E3132392C32342E35332C302C302D322E34362D31392E3038
                    2C35322E3530372D32352E37327A222F3E0D0A20203C2F673E0D0A20203C6720
                    69643D2267323222207374726F6B652D77696474683D22302E31373230303030
                    3122207374726F6B653D2223303030222066696C6C3D2223464646223E0D0A20
                    20203C706174682069643D227061746832342220643D226D2D39382E3330342C
                    3132382E323873312E3737382C302E36362C312E3433322C312E34312D35302E
                    3939382D332E33342D35372E3132382C31362E333763302C302C302E33352D31
                    392E32342C35352E3639362D31372E37387A222F3E0D0A20203C2F673E0D0A20
                    203C672069643D2267323622207374726F6B652D77696474683D22302E313732
                    303030303122207374726F6B653D2223303030222066696C6C3D222346464622
                    3E0D0A2020203C706174682069643D227061746832382220643D226D2D313039
                    2E30312C3131302E303773312E33312C312E33382C302E36372C312E392D3434
                    2E33382D32352E3333362D35382E35332D31302E323963302C302C382E37342D
                    31372E3134372C35372E38362C382E33397A222F3E0D0A20203C2F673E0D0A20
                    203C672069643D2267333022207374726F6B652D77696474683D22302E313732
                    303030303122207374726F6B653D2223303030222066696C6C3D222346464622
                    3E0D0A2020203C706174682069643D227061746833322220643D226D2D313136
                    2E35352C3131342E323673312E34352C312E32322C302E38382C312E3831632D
                    302E35382C302E35392D34362E39372D32302E3134382D35392E33322D332E36
                    2C302C302C362E37342D31382E3032332C35382E34342C312E37397A222F3E0D
                    0A20203C2F673E0D0A20203C672069643D2267333422207374726F6B652D7769
                    6474683D22302E313732303030303122207374726F6B653D2223303030222066
                    696C6C3D2223464646223E0D0A2020203C706174682069643D22706174683336
                    2220643D226D2D3131392E31352C3131382E333473312E362C312C312E31312C
                    312E3637632D302E34392C302E36362D34392E32372D31332E35362D35392E32
                    352C342E35312C302C302C342E32322D31382E37372C35382E31342D362E3138
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D2267333822207374726F
                    6B652D77696474683D22302E313732303030303122207374726F6B653D222330
                    3030222066696C6C3D2223464646223E0D0A2020203C706174682069643D2270
                    61746834302220643D226D2D3130382E34322C3131382E393573312E31322C31
                    2E35332C302E34322C312E3937632D302E372C302E34332D34302E37372D3330
                    2E3831382D35362E37332D31372E37312C302C302C31302E38372D31352E3838
                    342C35362E33312C31352E37347A222F3E0D0A20203C2F673E0D0A20203C6720
                    69643D2267343222207374726F6B652D77696474683D22302E31373230303030
                    3122207374726F6B653D2223303030222066696C6C3D2223464646223E0D0A20
                    20203C706174682069643D227061746834342220643D226D2D3132382E322C39
                    3073302E362C312E382D302E322C322D32392E342D34312E382D34382E362D33
                    342E3263302C302C31352E322D31312E382C34382E382C33322E327A222F3E0D
                    0A20203C2F673E0D0A20203C672069643D2267343622207374726F6B652D7769
                    6474683D22302E313732303030303122207374726F6B653D2223303030222066
                    696C6C3D2223464646223E0D0A2020203C706174682069643D22706174683438
                    2220643D226D2D3132372E352C39362E39373973302E39372C312E3632392C30
                    2E32332C312E393936632D302E37342C302E3336382D33372E37322D33342E34
                    37362D35342E38332D32322E3931342C302C302C31322E332D31342E382C3534
                    2E362C32302E3931387A222F3E0D0A20203C2F673E0D0A20203C672069643D22
                    67353022207374726F6B652D77696474683D22302E3137323030303031222073
                    74726F6B653D2223303030222066696C6C3D2223464646223E0D0A2020203C70
                    6174682069643D227061746835322220643D226D2D3132372E36322C3130312E
                    333573312E31322C312E35332C302E34322C312E3937632D302E372C302E3433
                    2D34302E37372D33302E3831382D35362E37332D31372E3731332C302C302C31
                    302E38372D31352E3838312C35362E33312C31352E3734337A222F3E0D0A2020
                    3C2F673E0D0A20203C672069643D2267353422207374726F6B653D2223303030
                    222066696C6C3D2223464646223E0D0A2020203C706174682069643D22706174
                    6835362220643D226D2D3132392E38332C3130332E303663302E352C362E3035
                    2C312E34392C31322E36322C332E32332C31352E37342C302C302D332E362C31
                    322E342C352E322C32352E362C302C302D302E342C372E322C312E322C31302E
                    342C302C302C342C382E342C382E382C392E322C332E38382C302E36352C3132
                    2E3630372C332E37322C32322E3436382C352E31322C302C302C31372E313332
                    2C31342E30382C31332E3933322C32362E38382C302C302D302E342C31362E34
                    2D342C31382C302C302C31312E362D31312E322C322C352E366C2D342E342C31
                    382E387332352E362D32312E362C31302D332E326C2D31302C32367331392E36
                    2D31382E342C31322E342D31306C2D332E322C382E387334332E322D32372E32
                    2C31322E342C322E3463302C302C382D332E362C31322E342D302E382C302C30
                    2C362E382D312E322C362C302E342C302C302D32302E382C31302E342D32342E
                    342C32382E382C302C302C382E342D31302C352E322C302E386C302E342C3131
                    2E3673342D32312E362C332E362C313663302C302C31392E322D31382C372E36
                    2C322E387631362E387331352E322D31362E342C382E382D332E3663302C302C
                    31302D382E382C362C362E342C302C302D302E382C31302E342C332E362D302E
                    382C302C302C31362D33302E362C31302D342E342C302C302D302E382C31392E
                    322C342C342E342C302C302C302E342C31302E342C392E362C31372E362C302C
                    302D312E322D35302E382C31312E362D31342E386C342C31362E3473322E382D
                    392E322C322E342D31342E346C382C387331352E322D32322E382C31322D392E
                    3663302C302D372E362C31362D362C32302E382C302C302C31362E382D33342E
                    382C31382D33362E342C302C302D322C34322E342C382E382C362E342C302C30
                    2C352E362C31322C322E382C31362E342C302C302C382D382C372E322D31312E
                    322C302C302C342E362D382E322C372E342C352E342C302C302C312E382C392E
                    342C332E342C362E322C302C302C342C32342C352E322C312E322C302C302C31
                    2E362D31332E362D352E362D32352E322C302C302C302E382D332E322D322D37
                    2E322C302C302C31332E362C32312E362C362E342D372E322C302C302C31312E
                    3230312C382C31322E3430312C382C302C302D31332E3630312D32332E322D34
                    2E3830312D31382E342C302C302D352E322D31302E342C31322E3830312C312E
                    362C302C302D31362E3030312D31362C312E362D362E342C302C302C372E3939
                    392C362E342C302E342D332E362C302C302D31342E3430312D31362C372E3539
                    392C322C302C302C31312E362C31362E342C31322E342C31392E322C302C302D
                    31302D32392E322D31342E342D33322C302C302C382E342D33362E342C34392E
                    362D32302E382C302C302C362E382C31372E322C31312E322D312E322C302C30
                    2C31322E382D362E342C32342C32312E322C302C302C342D31332E362C332E32
                    2D31362E342C302C302C362E382C312E322C362C302C302C302C31332E322C34
                    2E342C31342E342C332E362C302C302C362E382C362E382C372E322C332E322C
                    302C302C392E322C322E382C372E322D302E382C302C302C382E382C31352E36
                    2C392E322C31392E326C322E342D31342C322C322E3873312E362D372E362C30
                    2E382D382E382C32302C362E382C32342E382C32372E366C322C382E3473362D
                    31342E382C342E342D31382E3863302C302C352E322C302E382C352E362C352E
                    322C302C302C342D32332E322D302E382D32392E322C302C302C342E342D302E
                    382C352E362C322E38762D372E3273372E322C302E382C372E322D312E366330
                    2C302C342E342D342C362E342C302E382C302C302D31322E342D33352E322C36
                    2D31362C302C302C372E322C31302E382C332E362D38732D372E362D32302E34
                    2D322E382D32302E3863302C302C302E382D332E362D312E322D352E3273312E
                    322C302C312E322C302C342E382C342D302E342D313863302C302C362E342C31
                    2E362D352E362D32372E362C302C302C322E382D322E342D312E322D31302E38
                    2C302C302C382C342E342C31302E382C322E382C302C302D302E342D312E362D
                    332E362D352E362C302C302D32312E362D35342E382D312E322D33322E382C30
                    2C302C31312E38352C31332E35352C352E34352D392E32352C302C302D392E31
                    312D32342E3030392D382E33332D32382E3330356C2D3432392E35352C32332E
                    3031357A222F3E0D0A20203C2F673E0D0A20203C672069643D22673538222073
                    74726F6B653D2223303030222066696C6C3D2223636337323236223E0D0A2020
                    203C706174682069643D227061746836302220643D226D3239392E37322C3830
                    2E32343563302E36322C302E3138312C322E38332C312E3330352C342E30382C
                    322E3935352C302C302C362E382C31302E382C312E362D372E362C302C302D39
                    2E322D32382E382D302E342D31372E362C302C302C362C372E322C322E382D36
                    2E342D332E38362D31362E3432372D362E342D32322E382D362E342D32322E38
                    7331312E362C342E382D31352E322D33342E386C382E382C332E36732D31392E
                    362D33392E362D34312E322D34342E386C2D382D367333382E342D33382C3235
                    2E362D37342E3863302C302D362E382D352E322D31362E342C342C302C302D36
                    2E342C342E382D31322E342C332E322C302C302D33302E382C312E322D33322E
                    382C312E32732D33362E382D33372E322D3130322E342D31392E3663302C302D
                    352E322C322D392E3539392C302E382C302C302D31382E3430312D31362D3637
                    2E3230312C362E382C302C302D31302C322D31312E362C32732D342E342C302D
                    31322E342C362E342D382E342C372E322D31302E342C382E3863302C302D3136
                    2E342C31312E322D32312E322C31322C302C302D31312E362C362E342D31362C
                    31362E346C2D332E362C312E32732D312E362C372E322D322C382E3463302C30
                    2D342E382C332E362D352E362C392E322C302C302D382E382C362D382E342C31
                    302E342C302C302D312E362C352E322D322E342C31302C302C302D372E322C34
                    2E382D362E342C372E362C302C302D372E362C31342D362E342C32302E382C30
                    2C302D362E342D302E342D392E322C322C302C302D302E382C342E382D322E34
                    2C352E322C302C302D322E382C312E322D302E342C352E322C302C302D312E36
                    2C322E382D322C342E342C302C302C302E382C322E382D332E362C382E342C30
                    2C302D362E342C31382E382D342E342C32342C302C302C302E342C342E382D32
                    2E342C362E342C302C302D332E362D302E342C342E382C31312E362C302C302C
                    302E382C312E322D322E342C332E362C302C302D31372E322C332E362D31392E
                    362C32302C302C302D31332E362C31342E382D31332E362C32302C302C322E33
                    30352C302E32372C352E3435322C302E39372C31302E30362C302C302D302E35
                    372C382E33342C32372E30332C392E3134733430322E37322D33312E3335352C
                    3430322E37322D33312E3335357A222F3E0D0A20203C2F673E0D0A20203C6720
                    69643D22673632222066696C6C3D2223636337323236223E0D0A2020203C7061
                    74682069643D227061746836342220643D226D2D3131352E362C3130322E3663
                    2D32352D33392E342D31302E362C31372D31302E362C31372C382E382C33342E
                    342C3133382E342D332E322C3133382E342D332E32733136382E382D33302E34
                    2C3138302D33342E342C3130362E342C322E342C3130362E342C322E346C2D35
                    2E362D31362E38632D36342E382D34362E342D38342D32332E322D39372E362D
                    32372E32732D31312E322C352E362D31342E342C362E342D34322E342D32342D
                    34382E382D32332E322D33312E37342D32322E3935312D31362E382C382E3863
                    31362C33342D35382E342C33392E322D37352E322C323873372E322C31382E34
                    2C372E322C31382E346331382E342C32302D31362C332E322D31362C332E322D
                    33342E342D31322E382D35382E342C31322E382D36312E362C31332E36732D38
                    2C342D382E382D322E342D382E33312D32332E3130312D34302C332E32632D32
                    302C31362E362D33332E382D352E342D33332E382D352E346C2D322E382C3131
                    2E367A222F3E0D0A20203C2F673E0D0A20203C672069643D2267363622206669
                    6C6C3D2223653837663361223E0D0A2020203C706174682069643D2270617468
                    36382220643D226D3133332E35312C32352E333436632D362E342C302E382D33
                    312E37372D32322E3933392D31362E382C382E382C31362E362C33352E322D35
                    382E342C33392E322D37352E322C32382D31362E3830312D31312E322C372E32
                    2C31382E342C372E322C31382E342C31382E342C32302E3030342D31362E3030
                    312C332E322D31362E3030312C332E322D33342E342D31322E382D35382E342C
                    31322E382D36312E362C31332E36732D382C342E3030342D382E382D322E3463
                    2D302E382D362E342D382E3137392D32322E3933342D34302C332E322D32312E
                    3233362C31372E3334342D33342E3732392D342E3130392D33342E3732392D34
                    2E3130396C2D332E322C31302E313133632D32352D33392E3830342D392E3933
                    2C31382E35312D392E39332C31382E35312C382E38312C33342E342C3133392E
                    30362D342E35312C3133392E30362D342E3531733136382E382D33302E343034
                    2C3138302D33342E3430342C3130352E35332C322E3332372C3130352E35332C
                    322E3332376C2D352E35332D31372E333039632D36342E382D34362E342D3833
                    2E322D32322E3631382D39362E382D32362E363138732D31312E322C352E362D
                    31342E342C362E342D34322E342D32342D34382E382D32332E327A222F3E0D0A
                    20203C2F673E0D0A20203C672069643D22673730222066696C6C3D2223656138
                    633464223E0D0A2020203C706174682069643D227061746837322220643D226D
                    3133342E38322C32372E303931632D362E342C302E382D33312E31342D32332E
                    3232392D31362E382C382E382C31362E322C33362E3230312D35382E3430312C
                    33392E3230312D37352E3230312C32382E30303173372E322C31382E342C372E
                    322C31382E346331382E342C31392E3939382D31362C332E322D31362C332E32
                    2D33342E342D31322E382D35382E3430312C31322E382D36312E3630312C3133
                    2E36732D382C332E3939382D382E382D322E34632D302E382D362E342D382E30
                    34382D32322E3736372D34302C332E322D32322E3437332C31382E3038382D33
                    352E3635382D322E3831382D33352E3635382D322E3831386C2D332E362C382E
                    363136632D32332E382D33382E3939382D392E32352C32302E30322D392E3235
                    2C32302E30322C382E382C33342E342C3133392E37312D352E38322C3133392E
                    37312D352E3832733136382E382D33302E3339382C3138302D33342E3339382C
                    3130342E36352C322E3235342C3130342E36352C322E3235346C2D352E34352D
                    31372E383138632D36342E382D34362E342D38322E342D32322E3033372D3936
                    2D32362E303337732D31312E322C352E362D31342E342C362E343031632D332E
                    322C302E382D34322E342D32342E3030312D34382E382D32332E3230317A222F
                    3E0D0A20203C2F673E0D0A20203C672069643D22673734222066696C6C3D2223
                    656339393631223E0D0A2020203C706174682069643D22706174683736222064
                    3D226D3133362E31332C32382E383337632D362E342C302E382D33312E31332D
                    32332E3233322D31362E382C382E382C31362E382C33372E3535362D35382E39
                    33362C33382E3834352D37352E3230322C32382D31362E382D31312E322C372E
                    322C31382E342C372E322C31382E342C31382E342C32302E3030332D31362C33
                    2E322D31362C332E322D33342E342D31322E382D35382E342C31322E3830332D
                    36312E362C31332E363033732D382C342D382E382D322E343033632D302E382D
                    362E342D372E3931372D32322E3539382D34302E3030312C332E3230332D3233
                    2E3730392C31382E38332D33362E3538372D312E35332D33362E3538372D312E
                    35336C2D342C372E3133632D32312E382D33362E3830332D382E35382C32312E
                    35322D382E35382C32312E35322C382E382C33342E342C3134302E33372D372E
                    31322C3134302E33372D372E3132733136382E382D33302E3430332C3138302D
                    33342E3430332C3130332E37382C322E3138322C3130332E37382C322E313832
                    6C2D352E33382D31382E333237632D36342E382D34362E3430312D38312E362D
                    32312E3435352D39352E322D32352E343535732D31312E322C352E362D31342E
                    342C362E342D34322E342D32342D34382E382D32332E327A222F3E0D0A20203C
                    2F673E0D0A20203C672069643D22673738222066696C6C3D2223656561353735
                    223E0D0A2020203C706174682069643D227061746838302220643D226D313337
                    2E34342C33302E353833632D362E342C302E382D33302E36332D32332E343534
                    2D31362E382C382E382C31362E382C33392E322D35382E3430332C33392E322D
                    37352E3230332C323873372E322C31382E342C372E322C31382E346331382E34
                    2C31392E3939372D31362C332E322D31362C332E322D33342E342D31322E382D
                    35382E342C31322E3739372D36312E362C31332E353937732D382C342D382E38
                    2D322E34632D302E382D362E3339372D372E3738352D32322E3432382D34302C
                    332E322D32342E3934362C31392E35382D33372E3530372D302E32332D33372E
                    3530372D302E32336C2D342E342C352E3633632D31392E382D33342E3739382D
                    372E39312C32332E30342D372E39312C32332E30342C382E382C33342E342C31
                    34312E30322D382E34342C3134312E30322D382E3434733136382E382D33302E
                    3339372C3138302D33342E3339372C3130322E39312C322E3130392C3130322E
                    39312C322E3130396C2D352E33312D31382E383337632D36342E382D34362E34
                    2D38302E382D32302E3837322D39342E342D32342E383732732D31312E322C35
                    2E362D31342E342C362E342D34322E342D32342D34382E382D32332E327A222F
                    3E0D0A20203C2F673E0D0A20203C672069643D22673832222066696C6C3D2223
                    663162323838223E0D0A2020203C706174682069643D22706174683834222064
                    3D226D3133382E37352C33322E333238632D362E342C302E382D33322E33372D
                    32322E3635312D31362E382C382E382C31392E322C33382E382D35382E343034
                    2C33392E322D37352E3230342C323873372E322C31382E342C372E322C31382E
                    346331382E342C32302E3030322D31362C332E322D31362C332E322D33342E34
                    2D31322E382D35382E342C31322E3830322D36312E362C31332E363032732D38
                    2C342D382E382D322E34632D302E382D362E3430322D372E3635342D32322E32
                    36352D34302C332E322D32362E3138322C32302E33332D33382E3433362C312E
                    30352D33382E3433362C312E30356C2D342E382C342E3135632D31382D33332E
                    3230322D372E32342C32342E35342D372E32342C32342E35342C382E382C3334
                    2E342C3134312E36382D392E37342C3134312E36382D392E3734733136382E38
                    2D33302E3430322C3138302D33342E3430322C3130322E30332C322E3033362C
                    3130322E30332C322E3033366C2D352E32332D31392E333435632D36342E382D
                    34362E342D38302D32302E3239312D39332E362D32342E323931732D31312E32
                    2C352E362D31342E342C362E342D34322E342D32342D34382E382D32332E327A
                    222F3E0D0A20203C2F673E0D0A20203C672069643D22673836222066696C6C3D
                    2223663362663963223E0D0A2020203C706174682069643D2270617468383822
                    20643D226D3134302E30362C33342E303733632D362E342C302E382D33322E37
                    352D32322E34362D31362E382C382E382C32302E342C34302E3030312D35382E
                    3430352C33392E3230312D37352E3230352C32382E30303173372E322C31382E
                    342C372E322C31382E346331382E342C31392E3939362D31362C332E322D3136
                    2C332E322D33342E342D31322E382D35382E342C31322E3739362D36312E362C
                    31332E353936732D382C342D382E382D322E34632D302E382D362E3339362D37
                    2E3532332D32322E3039322D34302C332E322D32372E3431392C32312E30382D
                    33392E3336352C322E33352D33392E3336352C322E33356C2D352E322C322E36
                    35632D31362D33302E3139362D362E35362C32362E30362D362E35362C32362E
                    30362C382E382C33342E342C3134322E33322D31312E30362C3134322E33322D
                    31312E3036733136382E382D33302E3339362C3138302D33342E3339362C3130
                    312E31362C312E3936332C3130312E31362C312E3936336C2D352E31362D3139
                    2E383534632D36342E382D34362E342D37392E322D31392E3730392D39322E38
                    2D32332E3730392D31332E362D342E3030312D31312E322C352E362D31342E34
                    2C362E34732D34322E342D32342E3030312D34382E382D32332E3230317A222F
                    3E0D0A20203C2F673E0D0A20203C672069643D22673930222066696C6C3D2223
                    663563636230223E0D0A2020203C706174682069643D22706174683932222064
                    3D226D3134312E33362C33352E383139632D362E342C302E382D33332E38342D
                    32312E3837352D31362E382C382E382C32322C33392E362D35382E3339362C33
                    392E322D37352E3139362C323873372E322C31382E342C372E322C31382E3463
                    31382E342C32302E3030312D31362C332E322D31362C332E322D33342E342D31
                    322E382D35382E342C31322E3830312D36312E362C31332E363031732D382C34
                    2D382E382D322E34632D302E382D362E3430312D372E3339312D32312E393238
                    2D34302C332E322D32382E3635352C32312E38322D34302E3239342C332E3634
                    2D34302E3239342C332E36346C2D352E362C312E3136632D31342E342D32382E
                    3430312D352E38392C32372E35362D352E38392C32372E35362C382E382C3334
                    2E342C3134322E39382D31322E33362C3134322E39382D31322E333673313638
                    2E382D33302E3430312C3138302D33342E3430312C3130302E332C312E383931
                    2C3130302E332C312E3839316C2D352E312D32302E333634632D36342E382D34
                    362E342D37382E342D31392E3132372D39322D32332E313237732D31312E322C
                    352E362D31342E342C362E342D34322E342D32342D34382E382D32332E327A22
                    2F3E0D0A20203C2F673E0D0A20203C672069643D22673934222066696C6C3D22
                    23663864386334223E0D0A2020203C706174682069643D227061746839362220
                    643D226D3134322E36372C33372E353635632D362E342C302E382D33332E3834
                    2D32312E3837362D31362E382C382E382C32322C33392E362D35382E3339362C
                    33392E322D37352E3139362C323873372E322C31382E342C372E322C31382E34
                    6331382E342C31392E3939352D31362C332E322D31362C332E322D33342E3430
                    312D31322E382D35382E3430312C31322E3739352D36312E3630312C31332E35
                    3935732D382C342D382E382D322E342D372E3235392D32312E3735352D34302C
                    332E32632D32392E3839312C32322E35372D34312E3231332C342E39332D3431
                    2E3231332C342E39336C2D362D302E3333632D31332E36312D32362E3339362D
                    352E32322C32392E30382D352E32322C32392E30382C382E382C33342E342C31
                    34332E36332D31332E36382C3134332E36332D31332E3638733136382E382D33
                    302E3339352C3138302D33342E3339352C39392E34322C312E3831382C39392E
                    34322C312E3831386C2D352E30312D32302E383733632D36342E38312D34362E
                    342D37372E36312D31382E3534352D39312E32312D32322E353435732D31312E
                    322C352E362D31342E342C362E342D34322E342D32342D34382E382D32332E32
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D22673938222066696C6C
                    3D2223666165356437223E0D0A2020203C706174682069643D22706174683130
                    302220643D226D3134332E39382C33392E3331632D362E342C302E382D33332E
                    34352D32322E3038372D31362E382C382E382C32322C34302E382D35382E3339
                    372C33392E322D37352E3139372C323873372E322C31382E342C372E322C3138
                    2E346331382E342C32302D31362C332E322D31362C332E322D33342E342D3132
                    2E382D35382E342C31322E382D36312E362C31332E362D332E3230312C302E38
                    2D382E3030312C342D382E3830312D322E34732D372E3132382D32312E353932
                    2D34302C332E32632D33312E3132372C32332E33312D34322E3134322C362E32
                    322D34322E3134322C362E32326C2D362E342D312E3832632D31332D32342D34
                    2E35352C33302E35382D342E35352C33302E35382C382E382C33342E342C3134
                    342E32392D31342E39382C3134342E32392D31342E3938733136382E382D3330
                    2E342C3138302D33342E342C39382E35352C312E3734362C39382E35352C312E
                    3734366C2D342E39352D32312E333832632D36342E382D34362E3430312D3736
                    2E382D31372E3936342D39302E342D32312E393634732D31312E322C352E362D
                    31342E342C362E342D34322E342D32342D34382E382D32332E327A222F3E0D0A
                    20203C2F673E0D0A20203C672069643D2267313032222066696C6C3D22236663
                    66326562223E0D0A2020203C706174682069643D22706174683130342220643D
                    226D3134352E32392C34312E303535632D362E342C302E382D33322E33372D32
                    322E3634342D31362E382C382E382C32312E322C34322E3830312D35382E3339
                    382C33392E3230312D37352E3139382C32382E30303173372E322C31382E342C
                    372E322C31382E346331382E342C32302E3030342D31362C332E322D31362C33
                    2E322D33342E342D31322E382D35382E342C31322E3830342D36312E362C3133
                    2E363034732D382C342D382E382D322E342D362E3939372D32312E3432382D34
                    302C332E32632D33322E3336352C32342E30352D34332E3037322C372E352D34
                    332E3037322C372E356C2D362E382D332E33632D31322E382D32332E3230342D
                    332E38372C33322E30392D332E38372C33322E30392C382E382C33342E342C31
                    34342E39342D31362E32392C3134342E39342D31362E3239733136382E382D33
                    302E342C3138302D33342E3430346331312E322D342C39372E36372C312E3637
                    342C39372E36372C312E3637346C2D342E38372D32312E383933632D36342E38
                    2D34362E342D37362D31372E3338312D38392E362D32312E3338312D31332E36
                    2D342E3030312D31312E322C352E362D31342E342C362E34732D34322E342D32
                    342E3030312D34382E382D32332E3230317A222F3E0D0A20203C2F673E0D0A20
                    203C672069643D2267313036222066696C6C3D2223464646223E0D0A2020203C
                    706174682069643D22706174683130382220643D226D2D3131352E382C313139
                    2E36632D31322E382D32322D332E322C33332E362D332E322C33332E362C382E
                    382C33342E342C3134352E362D31372E362C3134352E362D31372E3673313638
                    2E382D33302E342C3138302D33342E342C39362E382C312E362C39362E382C31
                    2E366C2D342E382D32322E34632D36342E382D34362E342D37352E322D31362E
                    382D38382E382D32302E38732D31312E322C352E362D31342E342C362E342D34
                    322E342D32342D34382E382D32332E322D33312E36322D32332E3030372D3136
                    2E382C382E386332322E32332C34372E3730372D36302E3735392C33372E3632
                    372D37352E322C32382D31362E382D31312E322C372E322C31382E342C372E32
                    2C31382E342C31382E342C32302D31362C332E322D31362C332E322D33342E34
                    2D31322E382D35382E342C31322E382D36312E362C31332E36732D382C342D38
                    2E382D322E342D362E3836352D32312E3235362D34302C332E32632D33332E36
                    2C32342E382D34342C382E382D34342C382E386C2D372E322D342E387A222F3E
                    0D0A20203C2F673E0D0A20203C672069643D2267313130222066696C6C3D2223
                    303030223E0D0A2020203C706174682069643D22706174683131322220643D22
                    6D2D37342E322C3134392E36732D372E322C31312E362C31332E362C32342E38
                    63302C302C312E342C312E342D31362E362D322E382C302C302D362E322D322D
                    372E382D31322E342C302C302D342E382D342E342D392E362D31307332302E34
                    2C302E342C32302E342C302E347A222F3E0D0A20203C2F673E0D0A20203C6720
                    69643D2267313134222066696C6C3D2223434343223E0D0A2020203C70617468
                    2069643D22706174683131362220643D226D36352E382C3130327331372E3639
                    382C32362E38322C31372E312C33312E36632D312E332C31302E342D312E352C
                    32302C312E372C32342C332E3230312C342C31322E3030312C33372E322C3132
                    2E3030312C33372E32732D302E342C312E322C31312E3939392D33362E386330
                    2C302C31312E362D31362D382E342D33342E342C302C302D33352E322D32382E
                    382D33342E342D32312E367A222F3E0D0A20203C2F673E0D0A20203C67206964
                    3D2267313138222066696C6C3D2223303030223E0D0A2020203C706174682069
                    643D22706174683132302220643D226D2D35342E322C3137362E347331312E32
                    2C372E322D332E322C33382E346C362E342D322E34732D302E382C31312E322D
                    342C31332E366C372E322D332E3273342E382C382C302E382C31322E3863302C
                    302C31362E382C382C31362C31342E342C302C302C362E342D382C322E342D31
                    342E34732D31312E322D322E342D31302E342D32302E386C2D382E382C332E32
                    73352E362D382E382C352E362D31352E326C2D382C322E347331352E3436392D
                    32362E35382C342E382D3238632D362D302E382D382E382D302E382D382E382D
                    302E387A222F3E0D0A20203C2F673E0D0A20203C672069643D22673132322220
                    66696C6C3D2223434343223E0D0A2020203C706174682069643D227061746831
                    32342220643D226D2D32312E382C3139332E3273322E382D342E342C302D332E
                    362D33342C31352E362D34302C32352E3263302C302C33342E342D32342E342C
                    34302D32312E367A222F3E0D0A20203C2F673E0D0A20203C672069643D226731
                    3236222066696C6C3D2223434343223E0D0A2020203C706174682069643D2270
                    6174683132382220643D226D2D31312E342C3230312E3273322E382D342E342C
                    302D332E362D33342C31352E362D34302C32352E3263302C302C33342E342D32
                    342E342C34302D32312E367A222F3E0D0A20203C2F673E0D0A20203C67206964
                    3D2267313330222066696C6C3D2223434343223E0D0A2020203C706174682069
                    643D22706174683133322220643D226D312E382C31383673322E382D342E342C
                    302D332E362D33342C31352E362D34302C32352E3263302C302C33342E342D32
                    342E342C34302D32312E367A222F3E0D0A20203C2F673E0D0A20203C67206964
                    3D2267313334222066696C6C3D2223434343223E0D0A2020203C706174682069
                    643D22706174683133362220643D226D2D32312E342C3232392E3673302D362D
                    322E382D352E322D33382E382C31382E342D34342E382C323863302C302C3432
                    2D32352E362C34372E362D32322E387A222F3E0D0A20203C2F673E0D0A20203C
                    672069643D2267313338222066696C6C3D2223434343223E0D0A2020203C7061
                    74682069643D22706174683134302220643D226D2D32302E322C3231382E3873
                    312E322D342E382D312E362D34632D322C302D32382E342C31312E362D33342E
                    342C32312E322C302C302C32392E362D32312E362C33362D31372E327A222F3E
                    0D0A20203C2F673E0D0A20203C672069643D2267313432222066696C6C3D2223
                    434343223E0D0A2020203C706174682069643D22706174683134342220643D22
                    6D2D33342E362C3236362E342D31302C372E367331302E342D372E362C31342D
                    362E3463302C302D362E382C31312E322D372E362C31362E342C302C302C3130
                    2E342D31322E382C31362D31322E342C302C302C372E362C302E342C372E362C
                    31312E322C302C302C352E362D31302E342C382E382D31302C302C302C312E32
                    2C362E342C302C31332E322C302C302C342D372E362C382D362C302C302C362E
                    342D322C352E362C392E362C302C302C302C31302E342D302E382C31332E322C
                    302C302C352E362D32362E342C382D32362E382C302C302C382D312E322C3132
                    2E382C372E362C302C302D342D372E362C302E382D352E362C302C302C31302E
                    382C312E362C31342C382E342C302C302D362E382D31322D312E322D382E386C
                    382C362E3473382E342C32312E322C31302E342C32322E3863302C302D372E36
                    2D32312E362D362D32312E362C302C302D322D31322C332E322C322E382C302C
                    302D332E322D31342C322E342D31332E327331302C31302E382C31382E342C38
                    2E3463302C302C392E3630312C352E362C31312E3630312D36332E366C2D3132
                    342C34362E387A222F3E0D0A20203C2F673E0D0A20203C672069643D22673134
                    36222066696C6C3D2223303030223E0D0A2020203C706174682069643D227061
                    74683134382220643D226D2D32392E382C3137332E367331342E382D362C3534
                    2E382C3063302C302C372E322C302E342C31342D382E347333332E362D31362C
                    34302D31346C392E3630312C362E342C302E382C312E327331322E3339392C31
                    302E342C31322E3739392C31382D31342E3339392C35352E362D32342C37312E
                    36632D392E362C31362D31392E322C32382E342D33382E342C32362C302C302D
                    32302E382D342D34362E342C302C302C302D32392E322D312E362D33322D392E
                    367331312E322D32332E322C31312E322D32332E322C342E342D382E342C332E
                    322D32322E382D302E382D34322E342D352E362D34352E327A222F3E0D0A2020
                    3C2F673E0D0A20203C672069643D2267313530222066696C6C3D222365353636
                    3863223E0D0A2020203C706174682069643D22706174683135322220643D224D
                    2D372E382C3137352E3663382E342C31382E342D32312E322C38332E362D3231
                    2E322C38332E362D322C312E362C31322E36362C372E36352C32322E382C352E
                    322C31302E3934362D322E36342C35312E322C312E362C35312E322C312E362C
                    32332E362D31352E362C33362E342D36302C33362E342D36307331302E343031
                    2D32342D372E322D32372E32632D31372E362D332E322D38322D332E322D3832
                    2D332E327A222F3E0D0A20203C2F673E0D0A20203C672069643D226731353422
                    2066696C6C3D2223623233323539223E0D0A2020203C706174682069643D2270
                    6174683135362220643D226D2D392E3833312C3230362E3563332E3332362D31
                    322E37392C342E39312D32342E35392C322E3033312D33302E392C302C302C36
                    322E342C362E342C37332E362D31342E342C342E3234312D372E38372C31392E
                    3030312C32322E382C31382E362C33322E342C302C302D36332C31342E342D37
                    372E382C332E326C2D31362E3433312C392E377A222F3E0D0A20203C2F673E0D
                    0A20203C672069643D2267313538222066696C6C3D2223613532363463223E0D
                    0A2020203C706174682069643D22706174683136302220643D226D2D352E342C
                    3232322E3873322C372E322D302E342C31312E3263302C302D312E362C302E38
                    2D322E382C312E322C302C302C312E322C332E362C372E322C352E322C302C30
                    2C322C342E342C342E342C342E3873372E322C362C31312E322C342E382C3135
                    2E322D352E322C31352E322D352E322C352E362D332E322C31342E342C302E34
                    63302C302C322E3337352D302E382C322E382D342E382C302E352D342E372C33
                    2E362D382E342C352E362D31302E347331312E362D31342E382C31302E342D31
                    352E322D36382C382D36382C387A222F3E0D0A20203C2F673E0D0A20203C6720
                    69643D226731363222207374726F6B653D2223303030222066696C6C3D222366
                    6637323766223E0D0A2020203C706174682069643D2270617468313634222064
                    3D226D2D392E382C3137342E34732D322E382C32322E342C302E342C33302E38
                    2C322E342C31302E342C312E362C31342E342C332E362C31342C392E322C3230
                    6C31322C312E367331352E322D332E362C32342E342D302E3863302C302C382E
                    3939342C312E33342C31322E342D31332E362C302C302C342E382D362E342C31
                    322D392E327331342E342D34342E342C31302E342D35322E342D31382E342D31
                    322E342D33342E342C332E322D31382D312E322D34382C367A222F3E0D0A2020
                    3C2F673E0D0A20203C672069643D226731363622207374726F6B652D77696474
                    683D22302E3522207374726F6B653D2223303030222066696C6C3D2223464643
                    223E0D0A2020203C706174682069643D22706174683136382220643D226D2D38
                    2E322C3234392E32732D302E382D322D352E322D322E3463302C302D32322E34
                    2D332E362D33302E382D31362C302C302D362E382D352E362D322E342C362C30
                    2C302C31302E342C32302E342C31372E322C32332E322C302C302C31362E342C
                    342C32312E322D31302E387A222F3E0D0A20203C2F673E0D0A20203C67206964
                    3D2267313730222066696C6C3D2223636333663463223E0D0A2020203C706174
                    682069643D22706174683137322220643D226D37312E3734322C3138352E3233
                    63302E3635392D372E39312C322E3631322D31362E35322C302E3835382D3230
                    2E30332D362E3434362D31322E38392D32332E3431392D372E352D33342E342C
                    332E322D31362C31352E362D31382D312E322D34382C362C302C302D312E3734
                    352C31332E39362D302E3930352C32332E39382C302C302C33372E3330352D31
                    312E35382C33382E3130352D352E39382C302C302C312E362D332E322C31302E
                    382D332E327333312E3934322D312E31372C33332E3534322D332E39377A222F
                    3E0D0A20203C2F673E0D0A20203C672069643D226731373422207374726F6B65
                    2D77696474683D223222207374726F6B653D2223613531393236223E0D0A2020
                    203C706174682069643D22706174683137362220643D226D32382E362C313735
                    2E3273342E382C342E382C312E322C31342E3463302C302D31342E342C31362D
                    31322E342C3330222F3E0D0A20203C2F673E0D0A20203C672069643D22673137
                    3822207374726F6B652D77696474683D22302E3522207374726F6B653D222330
                    3030222066696C6C3D2223464643223E0D0A2020203C706174682069643D2270
                    6174683138302220643D226D2D31392E342C323630732D342E342D31322E382C
                    342E342D366C332E362C332E36632D312E322C312E362D362E382C352E362D38
                    2C322E347A222F3E0D0A20203C2F673E0D0A20203C672069643D226731383222
                    207374726F6B652D77696474683D22302E3522207374726F6B653D2223303030
                    222066696C6C3D2223464643223E0D0A2020203C706174682069643D22706174
                    683138342220643D226D2D31342E33362C3236312E32732D332E35322D31302E
                    32342C332E35322D342E386C322E38382C322E3838632D342E35362C312E3238
                    2C302C332E38342D362E342C312E39327A222F3E0D0A20203C2F673E0D0A2020
                    3C672069643D226731383622207374726F6B652D77696474683D22302E352220
                    7374726F6B653D2223303030222066696C6C3D2223464643223E0D0A2020203C
                    706174682069643D22706174683138382220643D226D2D392E35362C3236312E
                    32732D332E35322D31302E32342C332E35322D342E386C322E38382C322E3838
                    632D332E33362C312E32382C302C332E38342D362E342C312E39327A222F3E0D
                    0A20203C2F673E0D0A20203C672069643D226731393022207374726F6B652D77
                    696474683D22302E3522207374726F6B653D2223303030222066696C6C3D2223
                    464643223E0D0A2020203C706174682069643D22706174683139322220643D22
                    6D2D322E39362C3236312E34732D332E35322D31302E32342C332E35322D342E
                    3863302C302C342E3338332C322E33332C322E3838312C322E38382D322E3936
                    312C312E30382C302C332E38342D362E3430312C312E39327A222F3E0D0A2020
                    3C2F673E0D0A20203C672069643D226731393422207374726F6B652D77696474
                    683D22302E3522207374726F6B653D2223303030222066696C6C3D2223464643
                    223E0D0A2020203C706174682069643D22706174683139362220643D226D332E
                    35322C3236312E3332732D332E35322D31302E32342C332E3532312D342E386C
                    322E38382C322E3838632D302E39362C312E32382C302C332E38342D362E3430
                    312C312E39327A222F3E0D0A20203C2F673E0D0A20203C672069643D22673139
                    3822207374726F6B652D77696474683D22302E3522207374726F6B653D222330
                    3030222066696C6C3D2223464643223E0D0A2020203C706174682069643D2270
                    6174683230302220643D226D31302E322C323632732D342E382D31322E342C34
                    2E342D366C332E362C332E36632D312E322C312E362C302C342E382D382C322E
                    347A222F3E0D0A20203C2F673E0D0A20203C672069643D226732303222207374
                    726F6B652D77696474683D223222207374726F6B653D2223613532363463223E
                    0D0A2020203C706174682069643D22706174683230342220643D226D2D31382E
                    322C3234342E387331332E322D322E382C31392E322C302E3463302C302C362C
                    312E322C372E322C302E3873342E342D302E382C342E342D302E38222F3E0D0A
                    20203C2F673E0D0A20203C672069643D226732303622207374726F6B652D7769
                    6474683D223222207374726F6B653D2223613532363463223E0D0A2020203C70
                    6174682069643D22706174683230382220643D226D31352E382C3235332E3673
                    31322D31332E362C32342D392E3263372E3031362C322E35372C362D302E382C
                    362E382D332E3673312D372C362D3130222F3E0D0A20203C2F673E0D0A20203C
                    672069643D226732313022207374726F6B652D77696474683D22302E35222073
                    74726F6B653D2223303030222066696C6C3D2223464643223E0D0A2020203C70
                    6174682069643D22706174683231322220643D226D33332C3233372E36732D34
                    2D31302E382D362E382C322D362C31362E342D372E362C31392E3263302C302C
                    302C352E322C382E342C342E382C302C302C31302E382D302E342C31312E322D
                    332E32732D312E322D31342E342D352E322D32322E387A222F3E0D0A20203C2F
                    673E0D0A20203C672069643D226732313422207374726F6B652D77696474683D
                    223222207374726F6B653D2223613532363463223E0D0A2020203C7061746820
                    69643D22706174683231362220643D226D34372C3234342E3873332E362D322E
                    342C362D312E32222F3E0D0A20203C2F673E0D0A20203C672069643D22673231
                    3822207374726F6B652D77696474683D223222207374726F6B653D2223613532
                    363463223E0D0A2020203C706174682069643D22706174683232302220643D22
                    6D35332E352C3232382E3473322E392D342E392C372E372D352E37222F3E0D0A
                    20203C2F673E0D0A20203C672069643D2267323232222066696C6C3D22236232
                    62326232223E0D0A2020203C706174682069643D22706174683232342220643D
                    226D2D32352E382C3236352E327331382C332E322C32322E342C312E366C302E
                    342C322D32302E382D312E32732D31312E362D352E362D322D322E347A222F3E
                    0D0A20203C2F673E0D0A20203C672069643D226732323622207374726F6B652D
                    77696474683D22302E3522207374726F6B653D2223303030222066696C6C3D22
                    23464643223E0D0A2020203C706174682069643D22706174683232382220643D
                    226D2D31312E382C3137322C31392E362C302E3873372E322C33302E382C332E
                    362C33382E3463302C302D312E322C322E382D342D322E382C302C302D31382E
                    342D33322E382D32312E362D33342E3873312E322D312E362C322E342D312E36
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D22673233302220737472
                    6F6B652D77696474683D22302E3522207374726F6B653D222330303022206669
                    6C6C3D2223464643223E0D0A2020203C706174682069643D2270617468323332
                    2220643D226D2D38382E392C3136392E3373382E392C312E372C32312E352C34
                    2E3363302C302C342E382C32322E342C382C32372E32732D302E342C342E382D
                    342C322D31382E342D31362E382D32302E342D32312E322D352E312D31322E33
                    2D352E312D31322E337A222F3E0D0A20203C2F673E0D0A20203C672069643D22
                    6732333422207374726F6B652D77696474683D22302E3522207374726F6B653D
                    2223303030222066696C6C3D2223464643223E0D0A2020203C70617468206964
                    3D22706174683233362220643D226D2D36372E3033392C3137332E383273352E
                    382C312E35352C362E3830392C332E373663312E3030382C322E32322D312E32
                    30322C352E35312D312E3230322C352E3531732D312C332E33312D322E323032
                    2C312E3135632D312E3230322D322E31372D342E3037342D392E38332D332E34
                    30352D31302E34327A222F3E0D0A20203C2F673E0D0A20203C672069643D2267
                    323338222066696C6C3D2223303030223E0D0A2020203C706174682069643D22
                    706174683234302220643D226D2D36372C3137332E3673332E362C352E322C37
                    2E322C352E322C332E3938322D302E34312C362E382C302E3263342E362C312C
                    342E322D312C31302E382C302E322C322E36342C302E34382C352E322D302E34
                    2C382C302E3873362C302E342C372E322D312E362C362D362E322C362D362E32
                    2D31322E382C312E382D31352E362C322E3663302C302D32322E342C312E322D
                    33302E342D312E327A222F3E0D0A20203C2F673E0D0A20203C672069643D2267
                    32343222207374726F6B652D77696474683D22302E3522207374726F6B653D22
                    23303030222066696C6C3D2223464643223E0D0A2020203C706174682069643D
                    22706174683234342220643D226D2D32322E342C3137332E38732D362E34352C
                    332E352D362E38352C352E392C352E32352C362E312C352E32352C362E312C32
                    2E37352C342E362C332E33352C322E322D302E39352D31332E382D312E37352D
                    31342E327A222F3E0D0A20203C2F673E0D0A20203C672069643D226732343622
                    207374726F6B652D77696474683D22302E3522207374726F6B653D2223303030
                    222066696C6C3D2223464643223E0D0A2020203C706174682069643D22706174
                    683234382220643D226D2D35392E3838352C3137392E323673372E3030372C31
                    312E31392C372E3232342D302E303263302C302C302E3535372D312E32362D31
                    2E3230332D312E32382D362E3037352D302E30372D342E3535342D342E31382D
                    362E3032312C312E337A222F3E0D0A20203C2F673E0D0A20203C672069643D22
                    6732353022207374726F6B652D77696474683D22302E3522207374726F6B653D
                    2223303030222066696C6C3D2223464643223E0D0A2020203C70617468206964
                    3D22706174683235322220643D226D2D35322E3730372C3137392E353173372E
                    3932312C31312E31392C372E3238352D302E303963302C302C302E3030372D30
                    2E33332D312E3734362D302E34382D342E3734372D302E34322D342E3430322D
                    342E39342D352E3533392C302E35377A222F3E0D0A20203C2F673E0D0A20203C
                    672069643D226732353422207374726F6B652D77696474683D22302E35222073
                    74726F6B653D2223303030222066696C6C3D2223464643223E0D0A2020203C70
                    6174682069643D22706174683235362220643D226D2D34352E3439342C313739
                    2E353273372E39362C31302E36332C372E3239312C302E393663302C302C302E
                    3131392D312E32332D312E3533352D312E35332D332E3839322D302E37312D34
                    2E3130332D332E39352D352E3735362C302E35377A222F3E0D0A20203C2F673E
                    0D0A20203C672069643D226732353822207374726F6B652D77696474683D2230
                    2E3522207374726F6B653D2223303030222066696C6C3D2223464643223E0D0A
                    2020203C706174682069643D22706174683236302220643D226D2D33382E3631
                    382C3137392E3673372E392C31312E35362C382E3234382C312E373863302C30
                    2C312E3634342D312E33382D302E3130322D312E362D352E3831382D302E3734
                    2D352E30322D352E31392D382E3134362D302E31387A222F3E0D0A20203C2F67
                    3E0D0A20203C672069643D2267323632222066696C6C3D222365356535623222
                    3E0D0A2020203C706174682069643D22706174683236342220643D226D2D3734
                    2E3739322C3138332E31332D372E3635382D312E3533632D322E362D352D342E
                    372D31312E31352D342E372D31312E313573362E33352C312C31382E38352C33
                    2E3863302C302C302E3837362C332E33322C322E3334382C392E31316C2D382E
                    38342D302E32337A222F3E0D0A20203C2F673E0D0A20203C672069643D226732
                    3636222066696C6C3D2223653565356232223E0D0A2020203C70617468206964
                    3D22706174683236382220643D226D2D392E3732342C3137382E3437632D312E
                    3636362D322E35312D322E3938332D342E32362D332E3633332D342E36372D33
                    2E3031332D312E38382C312E31332D312E35312C322E3235392D312E35316C31
                    382E3435342C302E373673302E3532342C322E32342C312E3230382C352E3633
                    63302C302D31302E3038382D322E30312D31382E3238382D302E32317A222F3E
                    0D0A20203C2F673E0D0A20203C672069643D2267323730222066696C6C3D2223
                    636337323236223E0D0A2020203C706174682069643D22706174683237322220
                    643D226D34332E38382C34302E3332316332372E3732312C332E39362C35332E
                    3234312D33312E36382C35352E3030312D34312E3336312C312E3735392D392E
                    36382D382E33362D32312E35362D382E33362D32312E35362C312E33322D332E
                    30382D332E35322D31372E31362D382E382D32362E34732D32312E3138312D38
                    2E3236362D33382E3732312D392E3234632D31352E38342D302E38382D33342E
                    33322C32322E34342D33352E36342C32342E3273342E38342C34302E3034312C
                    362E31362C34352E3736312D312E33322C33322E31322D312E33322C33322E31
                    326333342E32342D392E312C332E39362D372E34382C33312E36382D332E3532
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D2267323734222066696C
                    6C3D2223656138653531223E0D0A2020203C706174682069643D227061746832
                    37362220643D226D382E3038382D33332E333932632D312E3239362C312E3732
                    382C342E3735322C33392E3331332C362E3034382C34342E393239732D312E32
                    39362C33312E3533362D312E3239362C33312E3533366333322E3637322D382E
                    38382C332E3838382D372E3334342C33312E3130342D332E3435362C32372E32
                    31372C332E3838382C35322E3237332D33312E3130342C35342E3030312D3430
                    2E3630392C312E3732382D392E3530342D382E3230382D32312E3136382D382E
                    3230382D32312E3136382C312E3239362D332E3032342D332E3435362D31362E
                    3834382D382E36342D32352E3932732D32302E3739352D382E3131352D33382E
                    3031372D392E303732632D31352E3535322D302E3836342D33332E3639362C32
                    322E3033322D33342E3939322C32332E37367A222F3E0D0A20203C2F673E0D0A
                    20203C672069643D2267323738222066696C6C3D2223656661613763223E0D0A
                    2020203C706174682069643D22706174683238302220643D226D382E3831362D
                    33322E373434632D312E3237322C312E3639362C342E3636342C33382E353835
                    2C352E3933362C34342E303937732D312E3237322C33302E3935322D312E3237
                    322C33302E3935326333312E3430342D392E31362C332E3831362D372E323038
                    2C33302E3532382D332E3339322C32362E3731332C332E3831362C35312E3330
                    352D33302E3532382C35332E3030312D33392E3835372C312E3639362D392E33
                    32382D382E3035362D32302E3737362D382E3035362D32302E3737362C312E32
                    37322D322E3936382D332E3339322D31362E3533362D382E34382D32352E3434
                    732D32302E34312D372E3936352D33372E3331332D382E393034632D31352E32
                    36342D302E3834382D33332E3037322C32312E3632342D33342E3334342C3233
                    2E33327A222F3E0D0A20203C2F673E0D0A20203C672069643D22673238322220
                    66696C6C3D2223663463366138223E0D0A2020203C706174682069643D227061
                    74683238342220643D226D392E3534342D33322E303936632D312E3234382C31
                    2E3636342C342E3537362C33372E3835372C352E3832342C34332E323635732D
                    312E3234382C33302E3336382D312E3234382C33302E3336386332392E343336
                    2D392E30342C332E3734342D372E3037322C32392E3935322D332E3332382C32
                    362E3230392C332E3734342C35302E3333372D32392E3935322C35322E303031
                    2D33392E3130342C312E3636342D392E3135332D372E3930342D32302E333835
                    2D372E3930342D32302E3338352C312E3234382D322E3931322D332E3332382D
                    31362E3232342D382E33322D32342E3936732D32302E3032352D372E3831352D
                    33362E3630392D382E373336632D31342E3937362D302E3833322D33322E3434
                    382C32312E3231362D33332E3639362C32322E38387A222F3E0D0A20203C2F67
                    3E0D0A20203C672069643D2267323836222066696C6C3D222366396532643322
                    3E0D0A2020203C706174682069643D22706174683238382220643D226D31302E
                    3237322D33312E343438632D312E3232342C312E3633322C342E3438382C3337
                    2E3132392C352E3731322C34322E343333732D312E3232342C32392E3738342D
                    312E3232342C32392E3738346332372E3836382D382E39322C332E3637322D36
                    2E3933362C32392E3337362D332E3236342C32352E3730352C332E3637322C34
                    392E3336392D32392E3337362C35312E3030312D33382E3335332C312E363332
                    2D382E3937362D372E3735322D31392E3939322D372E3735322D31392E393932
                    2C312E3232342D322E3835362D332E3236342D31352E3931322D382E31362D32
                    342E3438732D31392E36342D372E3636352D33352E3930352D382E353638632D
                    31342E3638382D302E3831362D33312E3832342C32302E3830382D33332E3034
                    382C32322E34347A222F3E0D0A20203C2F673E0D0A20203C672069643D226732
                    3930222066696C6C3D2223464646223E0D0A2020203C706174682069643D2270
                    6174683239322220643D224D34342E322C33362E386332352E322C332E362C34
                    382E3430312D32382E382C35302E3030312D33372E36732D372E362D31392E36
                    2D372E362D31392E3663312E322D322E382D332E3230312D31352E362D382E30
                    30312D3234732D31392E3235342D372E3531342D33352E322D382E34632D3134
                    2E342D302E382D33312E322C32302E342D33322E342C323273342E342C33362E
                    342C352E362C34312E362D312E322C32392E322D312E322C32392E326332352E
                    352D382E362C332E362D362E382C32382E382D332E327A222F3E0D0A20203C2F
                    673E0D0A20203C672069643D2267323934222066696C6C3D2223434343223E0D
                    0A2020203C706174682069643D22706174683239362220643D226D39302E3630
                    312C322E38732D32372E3830312C372E362D33392E3430312C3663302C302D31
                    352E382D362E362D32342E362C31352E322C302C302D332E362C372E322D352E
                    362C392E327336392E3630312D33302E342C36392E3630312D33302E347A222F
                    3E0D0A20203C2F673E0D0A20203C672069643D2267323938222066696C6C3D22
                    23303030223E0D0A2020203C706174682069643D22706174683330302220643D
                    226D39342E3430312C302E36732D32392E3030312C31322E322D33392E303031
                    2C31312E3863302C302D31362E342D342E362D32342E382C31302C302C302D38
                    2E342C392E322D31312E362C31302E382C302C302D302E342C312E362C362D32
                    2E346C31302E342C352E327331342E382C392E362C32342E342D362E3463302C
                    302C342D31312E322C342D31332E327332312E322D372E362C32322E3830312D
                    3863312E362D302E342C382E322D342E362C372E382D372E387A222F3E0D0A20
                    203C2F673E0D0A20203C672069643D2267333032222066696C6C3D2223393963
                    633332223E0D0A2020203C706174682069643D22706174683330342220643D22
                    6D34372C33362E353134632D362E3837322C302D31352E3234352D332E383635
                    2D31352E3234352D31302E3131342C302D362E3234382C382E3337332D31322E
                    3531332C31352E3234352D31322E3531332C362E3837342C302C31322E343436
                    2C352E3036352C31322E3434362C31312E3331332C302C362E3234392D352E35
                    37322C31312E3331342D31322E3434362C31312E3331347A222F3E0D0A20203C
                    2F673E0D0A20203C672069643D2267333036222066696C6C3D22233635393930
                    30223E0D0A2020203C706174682069643D22706174683330382220643D226D34
                    332E3337372C31392E3833632D342E3834362C302E3732322D392E3933352C32
                    2E3232352D392E3836332C322E3030392C312E35342D342E3631392C372E3930
                    312D372E3935322C31332E3438362D372E3935322C342E3239362C302C382E30
                    38342C312E3937382C31302E33322C342E3938382C302C302D352E3331362D30
                    2E33332D31332E3934332C302E3935357A222F3E0D0A20203C2F673E0D0A2020
                    3C672069643D2267333130222066696C6C3D2223464646223E0D0A2020203C70
                    6174682069643D22706174683331322220643D226D35352E342C31392E36732D
                    342E342D332E322D342E342D3163302C302C332E362C342E342C342E342C317A
                    222F3E0D0A20203C2F673E0D0A20203C672069643D2267333134222066696C6C
                    3D2223303030223E0D0A2020203C706174682069643D22706174683331362220
                    643D226D34352E342C32372E373236632D322E3439392C302D342E3532352D32
                    2E3032362D342E3532352D342E3532362C302D322E3439392C322E3032362D34
                    2E3532352C342E3532352D342E3532352C322E352C302C342E3532362C322E30
                    32362C342E3532362C342E3532352C302C322E352D322E3032362C342E353236
                    2D342E3532362C342E3532367A222F3E0D0A20203C2F673E0D0A20203C672069
                    643D2267333138222066696C6C3D2223636337323236223E0D0A2020203C7061
                    74682069643D22706174683332302220643D226D2D35382E362C31342E34732D
                    332E322D32312E322D302E382D32352E3663302C302C31302E382D31302C3130
                    2E342D31332E362C302C302D302E342D31382D312E362D31382E38732D382E38
                    2D362E382D31342E382D302E3463302C302D31302E342C31382D392E362C3234
                    2E347632732D372E362D302E342D392E322C312E3663302C302D312E322C352E
                    322D322E342C352E362C302C302D322E382C322E342D302E382C352E322C302C
                    302D322C322E342D312E362C362E346C372E362C3473322C31342E342C31322E
                    382C31392E3663342E3833362C322E3332392C382D342E342C31302D31302E34
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D2267333232222066696C
                    6C3D2223464646223E0D0A2020203C706174682069643D227061746833323422
                    20643D226D2D35392E362C31322E3536732D322E38382D31392E30382D302E37
                    322D32332E303463302C302C392E37322D392C392E33362D31322E32342C302C
                    302D302E33362D31362E322D312E34342D31362E3932732D372E39322D362E31
                    322D31332E33322D302E333663302C302D392E33362C31362E322D382E36342C
                    32312E393676312E38732D362E38342D302E33362D382E32382C312E34346330
                    2C302D312E30382C342E36382D322E31362C352E30342C302C302D322E35322C
                    322E31362D302E37322C342E36382C302C302D312E382C322E31362D312E3434
                    2C352E37366C362E38342C332E3673312E382C31322E39362C31312E35322C31
                    372E363463342E3335322C322E3039352C372E322D332E39362C392D392E3336
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D2267333236222066696C
                    6C3D2223656239353563223E0D0A2020203C706174682069643D227061746833
                    32382220643D226D2D35312E30352D34322E3631632D312E30392D302E38362D
                    382E35382D362E36332D31342E34332D302E33392C302C302D31302E31342C31
                    372E35352D392E33362C32332E373976312E3935732D372E34312D302E33392D
                    382E39372C312E353663302C302D312E31372C352E30372D322E33342C352E34
                    362C302C302D322E37332C322E33342D302E37382C352E30372C302C302D312E
                    39352C322E33342D312E35362C362E32346C372E34312C332E3973312E39352C
                    31342E30342C31322E34382C31392E313163342E3731342C322E32372C372E38
                    2D342E32392C392E37352D31302E31342C302C302D332E31322D32302E36372D
                    302E37382D32342E39362C302C302C31302E35332D392E37352C31302E31342D
                    31332E32362C302C302D302E33392D31372E35352D312E35362D31382E33337A
                    222F3E0D0A20203C2F673E0D0A20203C672069643D2267333330222066696C6C
                    3D2223663262383932223E0D0A2020203C706174682069643D22706174683333
                    322220643D226D2D35312E352D34312E3632632D302E39382D302E39322D382E
                    33362D362E34362D31342E30362D302E33382C302C302D392E38382C31372E31
                    2D392E31322C32332E313876312E39732D372E32322D302E33382D382E37342C
                    312E353263302C302D312E31342C342E39342D322E32382C352E33322C302C30
                    2D322E36362C322E32382D302E37362C342E39342C302C302D312E392C322E32
                    382D312E35322C362E30386C372E32322C332E3873312E392C31332E36382C31
                    322E31362C31382E363263342E3539342C322E3231322C372E362D342E31382C
                    392E352D392E38382C302C302D332E30342D32302E31342D302E37362D32342E
                    33322C302C302C31302E32362D392E352C392E38382D31322E39322C302C302D
                    302E33382D31372E312D312E35322D31372E38367A222F3E0D0A20203C2F673E
                    0D0A20203C672069643D2267333334222066696C6C3D2223663864636338223E
                    0D0A2020203C706174682069643D22706174683333362220643D226D2D35312E
                    39352D34302E3633632D302E38372D302E39382D382E31342D362E32392D3133
                    2E36392D302E33372C302C302D392E36322C31362E36352D382E38382C32322E
                    353776312E3835732D372E30332D302E33372D382E35312C312E343863302C30
                    2D312E31312C342E38312D322E32322C352E31382C302C302D322E35392C322E
                    32322D302E37342C342E38312C302C302D312E38352C322E32322D312E34382C
                    352E39326C372E30332C332E3773312E38352C31332E33322C31312E38342C31
                    382E313363342E3437332C322E3135342C372E342D342E30372C392E32352D39
                    2E36322C302C302D322E39362D31392E36312D302E37342D32332E36382C302C
                    302C392E39392D392E32352C392E36322D31322E35382C302C302D302E33372D
                    31362E36352D312E34382D31372E33397A222F3E0D0A20203C2F673E0D0A2020
                    3C672069643D2267333338222066696C6C3D2223464646223E0D0A2020203C70
                    6174682069643D22706174683334302220643D226D2D35392E362C31322E3436
                    732D322E38382D31382E39382D302E37322D32322E393463302C302C392E3732
                    2D392C392E33362D31322E32342C302C302D302E33362D31362E322D312E3434
                    2D31362E39322D302E37362D312E30342D372E39322D362E31322D31332E3332
                    2D302E33362C302C302D392E33362C31362E322D382E36342C32312E39367631
                    2E38732D362E38342D302E33362D382E32382C312E343463302C302D312E3038
                    2C342E36382D322E31362C352E30342C302C302D322E35322C322E31362D302E
                    37322C342E36382C302C302D312E382C322E31362D312E34342C352E37366C36
                    2E38342C332E3673312E382C31322E39362C31312E35322C31372E363463342E
                    3335322C322E3039352C372E322D342E30362C392D392E34367A222F3E0D0A20
                    203C2F673E0D0A20203C672069643D2267333432222066696C6C3D2223434343
                    223E0D0A2020203C706174682069643D22706174683334342220643D226D2D36
                    322E372C362E32732D32312E362D31302E322D32322E352D313163302C302C39
                    2E312C382E322C392E392C382E327331322E362C322E382C31322E362C322E38
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D2267333436222066696C
                    6C3D2223303030223E0D0A2020203C706174682069643D227061746833343822
                    20643D226D2D37392E382C307331382E342C332E362C31382E342C3863302C32
                    2E3931322D302E3234332C31362E3333312D352E362C31342E382D382E342D32
                    2E342D342E382D31362E382D31322E382D32322E387A222F3E0D0A20203C2F67
                    3E0D0A20203C672069643D2267333530222066696C6C3D222339396363333222
                    3E0D0A2020203C706174682069643D22706174683335322220643D226D2D3731
                    2E342C332E3873382E3937382C312E3437342C31302C342E3263302E362C312E
                    362C312E3236332C392E3930382D342E322C31312D342E3535322C302E393131
                    2D362E3738322D392E33312D352E382D31352E327A222F3E0D0A20203C2F673E
                    0D0A20203C672069643D2267333534222066696C6C3D2223303030223E0D0A20
                    20203C706174682069643D22706174683335362220643D226D31342E3539352C
                    34362E333439632D302E3439372D312E3734322C302E3831342D312E3631312C
                    322E3630352D322E3134392C322D302E362C31342E322D342E342C31352D3773
                    31342C312E382C31342C312E3863312E382C302E382C362E322C332E342C362E
                    322C332E342C342E382C312E322C31312E342C312E362C31312E342C312E362C
                    322E342C312C352E382C332E382C352E382C332E382C31342E362C31302E322C
                    32372E3030312C332C32372E3030312C332C31392E3939392D362E362C31332E
                    3939392D32332E382C31332E3939392D32332E382D332D392C302E322D31322E
                    342C302E322D31322E342C302E322D332E382C372E342C322E362C372E342C32
                    2E362C322E362C342E322C332E342C392E322C332E342C392E322C382C31312E
                    322C342E362D362E362C342E362D362E362C302E322D312D322E362D342E362D
                    322E362D352E38732D312E382D342E362D312E382D342E36632D332D332E342D
                    302E362D31302E342D302E362D31302E342C312E382D31332E382D302E342D31
                    322D302E342D31322D312E322D312E382D31302E342C382E322D31302E342C38
                    2E322D322E322C332E342D382E322C352D382E322C352D322E3739392C312E38
                    2D362E3139392C302E342D362E3139392C302E342D322E362D302E342D382E32
                    2C362E362D382E322C362E362C322E382D302E322C352E322C342E322C372E36
                    2C342E3473342E322D322E342C352E3739392D3363312E362D302E362C342E34
                    2C352E322C342E342C352E322C302E342C322E362D352E322C372E342D352E32
                    2C372E342D302E342C342E362D312E3939392C332D312E3939392C332D332D30
                    2E362D342E322C332E322D352E322C372E38732D352E322C352D352E322C3563
                    2D312E362C372E342D322E3830312C342E342D322E3830312C342E342D302E32
                    2D352E362D362E322C302E322D362E322C302E322D312E322C322D352E382D30
                    2E322D352E382D302E322D362E382D322D342E342D342D342E342D342C312E38
                    2D322E322C31332C302C31332C302C322E322D312E362D352E382D352E362D35
                    2E382D352E362D302E362D312E382C302E342D362E322C302E342D362E322C31
                    2E322D332E322C382D382E382C382D382E382C392E3430312D312E322C362E36
                    30312D322E382C362E3630312D322E382D362E322D352E322D31322E3030312C
                    322E342D31322E3030312C322E342D322E322C362E322D31392E362C32312E32
                    2D31392E362C32312E322D342E382C332E342D322E322D332E342D362E322C30
                    732D32342E362D352E362D32342E362D352E36632D31312E3536322D312E3139
                    332D31342E3239342C31342E3534392D31372E3832332C31312E3432392C302C
                    302C352E3431382C382E35322C332E3831382C322E39327A222F3E0D0A20203C
                    2F673E0D0A20203C672069643D2267333538222066696C6C3D2223303030223E
                    0D0A2020203C706174682069643D22706174683336302220643D226D3230392E
                    342D313230732D32352E362C382D32382E342C32362E3863302C302D322E342C
                    32322E382C31382C34302E342C302C302C302E342C362E342C322E342C392E36
                    2C302C302D312E362C342E382C31372E322D322E386C32372E322D382E347336
                    2E342D322E342C31312E362D31312E322C32302E342D32372E362C31362E382D
                    35322E3863302C302C312E322D31312E322D342E382D31312E362C302C302D38
                    2E342D312E362D31352E362C362C302C302D362E382C332E322D392E322C322E
                    386C2D33352E322C312E327A222F3E0D0A20203C2F673E0D0A20203C67206964
                    3D2267333632222066696C6C3D2223303030223E0D0A2020203C706174682069
                    643D22706174683336342220643D226D3236342E30322D3132302E393973322E
                    312D382E39332D322E37342D342E303963302C302D372E30342C352E37322D31
                    342E35322C352E37322C302C302D31342E35322C322E322D31382E39322C3135
                    2E342C302C302D332E39362C32362E38342C332E39362C33322E35362C302C30
                    2C342E38342C372E34382C31312E38382C302E38387332322E35342D33362E38
                    332C32302E33342D35302E34377A222F3E0D0A20203C2F673E0D0A20203C6720
                    69643D2267333636222066696C6C3D2223333233323332223E0D0A2020203C70
                    6174682069643D22706174683336382220643D226D3236332E36352D3132302E
                    363373322E30392D382E37352D322E36362D332E393963302C302D362E39322C
                    352E36312D31342E32362C352E36312C302C302D31342E32362C322E31362D31
                    382E35382C31352E31322C302C302D332E38392C32362E3335342C332E38392C
                    33312E39372C302C302C342E37352C372E3334342C31312E36362C302E383634
                    2C362E39322D362E34382C32322E31312D33362E3138342C31392E39352D3439
                    2E3537347A222F3E0D0A20203C2F673E0D0A20203C672069643D226733373022
                    2066696C6C3D2223363636223E0D0A2020203C706174682069643D2270617468
                    3337322220643D226D3236332E32372D3132302E323773322E30382D382E3536
                    2D322E35382D332E3963302C302D362E37382C352E35312D31332E39392C352E
                    35312C302C302D31342C322E31322D31382E32342C31342E38342C302C302D33
                    2E38312C32352E3836382C332E38322C33312E33382C302C302C342E36362C37
                    2E3230382C31312E34352C302E3834382C362E37382D362E33362C32312E3636
                    2D33352E3533382C31392E35342D34382E3637387A222F3E0D0A20203C2F673E
                    0D0A20203C672069643D2267333734222066696C6C3D2223393939223E0D0A20
                    20203C706174682069643D22706174683337362220643D226D3236322E392D31
                    31392E393273322E30372D382E33372D322E35312D332E373963302C302D362E
                    36352C352E34312D31332E37332C352E34312C302C302D31332E37322C322E30
                    382D31372E38382C31342E35362C302C302D332E37352C32352E3337322C332E
                    37342C33302E37382C302C302C342E35382C372E3037322C31312E32332C302E
                    3833322C362E36362D362E32342C32312E32332D33342E3839322C31392E3135
                    2D34372E3739327A222F3E0D0A20203C2F673E0D0A20203C672069643D226733
                    3738222066696C6C3D2223434343223E0D0A2020203C706174682069643D2270
                    6174683338302220643D226D3236322E35332D3131392E353673322E30362D38
                    2E31382D322E34332D332E3763302C302D362E35332C352E33312D31332E3437
                    2C352E33312C302C302D31332E34362C322E30342D31372E35342C31342E3238
                    2C302C302D332E36372C32342E3838362C332E36372C33302E31392C302C302C
                    342E34392C362E3933362C31312E30322C302E3831362C362E35322D362E3132
                    2C32302E37392D33342E3234362C31382E37352D34362E3839367A222F3E0D0A
                    20203C2F673E0D0A20203C672069643D2267333832222066696C6C3D22234646
                    46223E0D0A2020203C706174682069643D22706174683338342220643D226D32
                    36322E31352D3131392E3273322E30352D382D322E33352D332E3663302C302D
                    362E342C352E322D31332E322C352E322C302C302D31332E322C322D31372E32
                    2C31342C302C302D332E362C32342E342C332E362C32392E362C302C302C342E
                    342C362E382C31302E382C302E387332302E33352D33332E362C31382E33352D
                    34367A222F3E0D0A20203C2F673E0D0A20203C672069643D2267333836222066
                    696C6C3D2223393932363030223E0D0A2020203C706174682069643D22706174
                    683338382220643D226D35302E362C3834732D32302E342D31392E322D32382E
                    342D323063302C302D33342E342D342D34392E322C31342C302C302C31372E36
                    2D32302E342C34352E322D31342E382C302C302D32312E362D342E342D33342D
                    312E326C2D32362E342C31342D322E382C342E3873342D31342E382C32322E34
                    2D32302E3863302C302C32322E382D342E382C33332E362C302C302C302D3231
                    2E362D362E382D33312E362D342E382C302C302D33302E342D322E342D34332E
                    322C32342C302C302C342D31342E342C31382E382D32312E362C302C302C3133
                    2E362D382E382C33342D362C302C302C31342E342C332E322C31392E362C352E
                    3673342D302E342D342E342D352E3263302C302D352E362D31302D31392E362D
                    392E362C302C302D34322E382C332E362D35332E322C31352E362C302C302C31
                    332E362D31312E322C32342D31342C302C302C32322E342D382C33302E382D37
                    2E322C302C302C32342E382C312C33322E342D332C302C302D31312E322C352D
                    382C382E327331302C31302E382C31302C31322C32342E322C32332E332C3237
                    2E382C32372E376C322E322C322E337A222F3E0D0A20203C2F673E0D0A20203C
                    672069643D2267333930222066696C6C3D2223434343223E0D0A2020203C7061
                    74682069643D22706174683339322220643D226D3138392C323738732D31352E
                    352D33362E352D32382D343663302C302C32362C31362C32392E352C33342C30
                    2C302C302C31302D312E352C31327A222F3E0D0A20203C2F673E0D0A20203C67
                    2069643D2267333934222066696C6C3D2223434343223E0D0A2020203C706174
                    682069643D22706174683339362220643D226D3233362C3238352E35732D3236
                    2E352D35352D34352D373963302C302C34332E352C33372E352C34382E352C36
                    346C302E352C352E352D332D322E35732D302E352C392D312C31327A222F3E0D
                    0A20203C2F673E0D0A20203C672069643D2267333938222066696C6C3D222343
                    4343223E0D0A2020203C706174682069643D22706174683430302220643D226D
                    3239322E352C323337732D36322E352D35392E352D36342D363263302C302C36
                    302E352C36362C36332E352C37332E352C302C302D322D392C302E352D31312E
                    357A222F3E0D0A20203C2F673E0D0A20203C672069643D226734303222206669
                    6C6C3D2223434343223E0D0A2020203C706174682069643D2270617468343034
                    2220643D226D3130342C3238302E357331392E352D35322C33382E352D32392E
                    3563302C302C31352C31302C31342E352C31332C302C302D342D362E352D3232
                    2D362C302C302D31392D332D33312C32322E357A222F3E0D0A20203C2F673E0D
                    0A20203C672069643D2267343036222066696C6C3D2223434343223E0D0A2020
                    203C706174682069643D22706174683430382220643D226D3239342E352C3135
                    33732D34352D32382E352D35322E352D3330632D31312E38312D322E33362C34
                    392E352C32392C35342E352C33392E352C302C302C322D322E352D322D392E35
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D2267343130222066696C
                    6C3D2223303030223E0D0A2020203C706174682069643D227061746834313222
                    20643D226D3134332E382C3235392E367332302E342D322C32372E322D382E38
                    6C342E342C332E362C31372E362D33382E342C332E362C352E327331342E342D
                    31342E382C31332E362D32322E382C31322E382C362C31322E382C362D302E38
                    2D31312E362C362E342D342E3863302C302D322E342D31352E362C362D372E36
                    2C302C302D31302E35342D33302E31362C31322D342E342C352E362C362E342C
                    312E322D302E342C312E322D302E34732D32362D34382D342E342D33332E3663
                    302C302C322D32322E382C302E382D32372E32732D332E322D32362E382D382D
                    33322C302E342D362E382C362D312E3663302C302D31312E322D32342C322D31
                    322C302C302D332E362D31352E322D382D31382C302C302D352E362D31372E32
                    2C392E362D362E342C302C302D342E342D31322E342D372E362D31352E362C30
                    2C302D31312E362D32372E362D342E342D32322E386C342E342C332E36732D36
                    2E382D31342D302E342D392E362C362E342C342C362E342C342D32312E322D33
                    332E322D302E382D31352E3663302C302D382E31362D31332E3931382D31312E
                    362D32302E382C302C302D31382E382D32302E342D342E342D31346C342E382C
                    312E36732D382E382D31302D31362E382D31312E362C322E342D382C382E382D
                    362C32322C392E362C32322C392E362C31322E382C31382E382C31362E382C31
                    392E3263302C302D32302D372E362D31342C302E342C302C302C31342E342C31
                    342C372E322C31332E362C302C302D362C372E322D312E322C31362C302C302D
                    31382E34362D31382E3339312D332E362C372E326C362E382C31362E34732D32
                    342E342D32342E382D31332E322D322E3863302C302C31372E322C32332E362C
                    31392E322C323473362E342C392E322C362E342C392E326C2D342E342D322C35
                    2E322C382E38732D31312E322D31322D352E322C312E326C352E362C31342E34
                    732D32302E342D32322D362E382C372E3663302C302D31362E342D352E322D37
                    2E362C31322C302C302D312E362C31362D312E322C32312E3273312E362C3333
                    2E362D322E382C34312E362C362C32372E322C382C33312E322C352E362C3134
                    2E382D332E322C352E362D342E342D332E362D322E342C352E322C382C32342E
                    342C372E322C333063302C302D312E322C312E322D342E342D322E342C302C30
                    2D31342E382D32322E382D31332E322D382E342C302C302D312E322C382D342E
                    342C31362E382C302C302D332E322C31302E382D332E322C322C302C302D332E
                    322D31362E382D362D392E32732D362E342C31332E362D392E322C31362D382D
                    32302E342D392E322D313063302C302D31322D31322E342D31362E382C346C2D
                    31312E362C31362E34732D302E342D31322E342D312E362D362E3463302C302D
                    33302C362D34302E342C312E367A222F3E0D0A20203C2F673E0D0A20203C6720
                    69643D2267343134222066696C6C3D2223303030223E0D0A2020203C70617468
                    2069643D22706174683431362220643D226D3130392E342D39372E32732D3131
                    2E3539392D382D31352E3539392D372E362C32372E3539392D382E382C36382E
                    3739392C31382E3863302C302C342E382C322E382C382E342C322E342C302C30
                    2C332E322C322E342C302E342C362C302C302D382E382C392E362C322E342C32
                    302E382C302C302C31382E342C362E382C31322E382D322C302C302C31302E38
                    2C342C31332E322C3873312E322C302C312E322C306C2D31322E342D31322E34
                    732D352E322D322D382D31302E342D352E322D31382E342D302E382D32312E36
                    63302C302D342C342E342D332E322C302E3473342E342D372E362C362D382C31
                    382D31362E322C32342E382D31362E3663302C302D392E322C312E342D31322E
                    322C302E34732D32392E362D31322E342D33352E362D31332E3663302C302D31
                    362E382D362E362D342E382D342E362C302C302C33352E382C332E382C35342C
                    31372C302C302D372E322D382E342D32352E362D31352E342C302C302D32322E
                    322D31322E362D35372E342D372E362C302C302D31372E382C332E322D32352E
                    362C352C302C302D322E3539392D302E362D332E3139392D31732D31322E3430
                    312D392E342D34302E3030312D322E3463302C302D31372C342E362D32352E36
                    2C392E342C302C302D31352E322C312E322D31382E382C342E342C302C302D31
                    382E362C31342E362D32302E362C31352E34732D31332E342C382E342D31342E
                    322C382E3863302C302C32342E362D362E362C32372D397331392E382D352C32
                    322E322D332E362C31302E382C302E382C312E322C312E3463302C302C37352E
                    362C31342E382C37362E342C31362E3873342E382C302E382C342E382C302E38
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D2267343138222066696C
                    6C3D2223636337323236223E0D0A2020203C706174682069643D227061746834
                    32302220643D226D3138302E382D3130362E34732D31302E322D372E342D3132
                    2E322D372E342D31342E342D31302E322D31382E362D392E382D31362E342D39
                    2E362D34332E382D312E3463302C302D302E362D322C332D322E382C302C302C
                    362E342D322E322C362E382D322E382C302C302C32302E322D342E322C32372E
                    342D302E362C302C302C392E322C322E362C31352E342C382E382C302C302C31
                    312E322C332E322C31342E342C322E322C302C302C382E382C322E322C392E32
                    2C342C302C302C352E382C332C342C352E362C302C302C302E342C312E362D35
                    2E362C342E327A222F3E0D0A20203C2F673E0D0A20203C672069643D22673432
                    32222066696C6C3D2223636337323236223E0D0A2020203C706174682069643D
                    22706174683432342220643D226D3136382E33332D3130382E353163302E3831
                    2C302E36332C312E38332C302E37332C322E34332C312E35342C302E32342C30
                    2E33312D302E30352C302E36342D302E33372C302E37342D312E30342C302E33
                    312D322E312D302E32362D332E32342C302E33332D302E342C302E32312D312E
                    30342C302E30332D312E362D302E31322D312E36332D302E34342D332E34362D
                    302E34372D352E31352C302E32322D312E39382D312E31332D342E33342D302E
                    35342D362E34322D312E35352D302E30362D302E30322D302E32382C302E3332
                    2D302E33362C302E332D332E30342D312E31352D362E37392D302E38372D392E
                    32322D332E31352D322E34332D302E34312D342E37382D302E38372D372E3231
                    2D312E35352D312E38322D302E35312D332E32332D312E352D342E38352D322E
                    33332D312E33382D302E37312D322E38332D312E32332D342E33372D312E3631
                    2D312E38362D302E34352D332E36392D302E33342D352E35382D302E38362D30
                    2E312D302E30322D302E32392C302E33322D302E33372C302E332D302E33322D
                    302E31312D302E36322D302E36392D302E37392D302E36342D312E36382C302E
                    35322D332E31372D302E34352D342E38332D302E31312D312E31382D312E3232
                    2D322E392D302E39382D342E34352D312E34322D322E39372D302E38352D362E
                    31322C302E34322D392E31352D302E35382C342E31312D312E38342C382E382D
                    302E36312C31322E38362D322E36382C322E33332D312E31382C342E39392D30
                    2E30382C372E35362D302E38342C302E34392D302E31352C312E31382D302E33
                    352C312E35382C302E33322C302E31342D302E31342C302E33322D302E33372C
                    302E33382D302E33352C322E34342C312E31362C342E37362C322E34332C372E
                    32342C332E352C302E33342C302E31352C302E38382D302E30392C312E31332C
                    302E31322C312E35322C312E32312C332E34362C312E31312C342E38352C322E
                    33332C312E372D302E352C332E34392D302E31322C352E32322D302E37352C30
                    2E30382D302E30322C302E33312C302E33322C302E33342C302E332C312E3134
                    2D302E37352C322E32392D302E34382C332E31382D302E31382C302E33342C30
                    2E31322C312C302E33372C312E33312C302E34342C312E31322C302E32372C31
                    2E39382C302E37352C332E31362C302E39342C302E31312C302E30322C302E33
                    2D302E33322C302E33372D302E332C312E31322C302E34342C322E31362C302E
                    33392C322E38322C312E35352C302E31342D302E31342C302E332D302E33372C
                    302E33382D302E33352C312E30332C302E33342C312E36382C312E312C322E37
                    382C312E33342C302E34382C302E312C312E312C302E37332C312E36372C302E
                    39312C322E33392C302E37332C342E32342C322E32362C362E34332C332E3135
                    2C302E37362C302E33312C312E36342C302E35352C322E32372C312E30347A22
                    2F3E0D0A20203C2F673E0D0A20203C672069643D2267343236222066696C6C3D
                    2223636337323236223E0D0A2020203C706174682069643D2270617468343238
                    2220643D226D39312E3639362D3132322E3734632D322E3531382D312E37322D
                    342E3838362D322E38332D372E3332382D342E36322D302E3138312D302E3133
                    2D302E3534312C302E30342D302E3734332D302E30382D312E3030372D302E36
                    312D312E3839352D312E31392D322E3837372D312E38392D302E3533392D302E
                    33382D312E33362D302E33372D312E3836382D302E36332D322E3534342D312E
                    32392D352E3137332D312E38352D372E36382D332E30342C302E3638322D302E
                    36342C312E3830342D302E33392C322E342D312E322C302E3139352C302E3238
                    2C302E3433332C302E35362C302E3738362C302E33372C312E3637382D302E39
                    2C332E3532382D312E30352C352E3230342D302E39362C312E3730342C302E30
                    392C332E3432342C302E33392C352E3139392C302E36372C302E3330372C302E
                    30342C302E3530362C302E35362C302E3832392C302E36362C322E3232382C30
                    2E36362C342E3631372C302E31342C362E3733362C302E39382C312E3539312C
                    302E36332C332E3136312C312E34352C342E342C322E37322C302E3235322C30
                    2E32362D302E3037332C302E35372D302E3335332C302E37362C302E3338382D
                    302E31312C302E3636312C302E312C302E3737322C302E34312C302E3038342C
                    302E32342C302E3038342C302E35342C302C302E37382D302E3131322C302E33
                    312D302E3339312C302E34312D302E3736352C302E34362D312E3430372C302E
                    31392C302E3336352D312E31392D302E3333352D302E37342D312E3237332C30
                    2E38322D302E3532372C322E32322D312E3237322C332E34392D302E32382D30
                    2E31392D302E35312D302E34312D302E342D302E382C302E3233342C302E3532
                    2D302E3336382C302E38312D302E3533362C312E31332D302E3338352C302E37
                    322D312E3238342C322E31342D322E3136392C312E35337A222F3E0D0A20203C
                    2F673E0D0A20203C672069643D2267343330222066696C6C3D22236363373232
                    36223E0D0A2020203C706174682069643D22706174683433322220643D226D35
                    392E3139382D3131352E3339632D332E3135342D302E37392D362E3230342D30
                    2E36382D392E32322D312E39362D302E3036372D302E30322D302E32392C302E
                    33322D302E3335342C302E332D312E3336362D302E362D322E3238342D312E35
                    362D332E33362D322E36312D302E3931332D302E38392D322E3537312D302E35
                    2D332E3834352D302E39392D302E3332342D302E31322D302E3532372D302E36
                    332D302E3832382D302E36372D312E3231392D302E31362D322E3134362D312E
                    31312D332E3139312D312E36382C322E3333362D302E382C342E3734372D302E
                    37362C372E3230392D312E31352C302E3131332D302E30322C302E3235382C30
                    2E33312C302E3339312C302E33312C302E3133362C302C302E3236362D302E32
                    332C302E342D302E33362C302E3139352C302E32382C302E3439372C302E3631
                    2C302E3735342C302E33352C302E3534382D302E35342C312E3130342D302E33
                    352C312E3634342D302E33312C302E3134342C302E30312C302E3236392C302E
                    33322C302E3430322C302E33322C302E3133362C302C302E3236372D302E3332
                    2C302E342D302E33322C302E3133362C302C302E3236372C302E33322C302E34
                    2C302E33322C302E3133362C302C302E3236362D302E32332C302E342D302E33
                    362C302E3639322C302E37382C312E3537372C302E32332C322E3339392C302E
                    34312C312E3033382C302E32322C312E3330352C312E33372C322E3337392C31
                    2E36372C342E3731352C312E332C382E3835322C332E34352C31332E3231352C
                    352E35342C302E3330372C302E31342C302E3531372C302E33392C302E343037
                    2C302E37382C302E3236372C302C302E35382D302E30392C302E37372C302E30
                    342C312E3035382C302E37342C322E3039392C312E32382C322E3739362C322E
                    33382C302E3231362C302E33342D302E3131332C302E37352D302E3334362C30
                    2E372D342E3432392D312D382E3433352D312E36312D31322E3832322D322E37
                    317A222F3E0D0A20203C2F673E0D0A20203C672069643D226734333422206669
                    6C6C3D2223636337323236223E0D0A2020203C706174682069643D2270617468
                    3433362220643D226D34352E3333382D37312E313739632D312E3539322D312E
                    3231392D322E3137362D332E32352D332E3330342D352E3034322D302E323134
                    2D302E33342C302E30362D302E3635342C302E3337372D302E3734332C302E35
                    362D302E3135392C312E3130332C302E3331392C312E3531322C302E3532312C
                    312E3734352C302E3836322C332E32382C322E3130342C352E3237372C322E32
                    34332C312E39392C322E3233342C362E32352C322E3631392C362E3235372C36
                    2C302E3030312C302E3835392D312E3432372D302E3035392D312E3835372C30
                    2E382D322E3435312D312E3030332D342E38342D302E392D372E32322D322E33
                    36372D302E3631372D302E3338312D302E3238372D302E3833342D312E303432
                    2D312E3431327A222F3E0D0A20203C2F673E0D0A20203C672069643D22673433
                    38222066696C6C3D2223636337323236223E0D0A2020203C706174682069643D
                    22706174683434302220643D226D31372E382D3132332E373663302E3133352C
                    302C372E3136362C302E32342C372E3134392C302E33352D302E3034352C302E
                    33312D372E3737352C312E33362D382E3133392C312E31392D302E3136342D30
                    2E30382D372E3637362C322E33352D372E38312C322E32322C302E3236382D30
                    2E31342C382E3533342D332E37362C382E382D332E37367A222F3E0D0A20203C
                    2F673E0D0A20203C672069643D2267343432222066696C6C3D2223303030223E
                    0D0A2020203C706174682069643D22706174683434342220643D226D33332E32
                    2D313134732D31342E382C312E382D31392E322C332D32332C382E382D32362C
                    31302E3863302C302D31332E342C352E342D33302E342C32352E342C302C302C
                    372E362D332E342C392E382D362E322C302C302C31332E362D31322E362C3133
                    2E342D31302C302C302C31322E322D382E362C31312E362D362E342C302C302C
                    32342E342D31312E322C32322E342D382C302C302C32312E362D342E362C3230
                    2E362D322E362C302C302C31382E382C342E342C31362C342E362C302C302D35
                    2E382C312E322C302E362C342E382C302C302D332E342C342E342D382E382C30
                    2E34732D322E342D312E382D372E342D302E3863302C302D322E362C302E382D
                    372E322D332E322C302C302D352E362D342E362D31342E342D312C302C302D33
                    302E362C31322E362D33322E362C31332E322C302C302D332E362C322E382D36
                    2C362E342C302C302D352E382C342E342D382E382C352E382C302C302D31322E
                    382C31312E362D31342C31332C302C302D332E342C352E322D342E322C352E36
                    2C302C302C362E342D332E382C382E342D352E382C302C302C31342D31302C31
                    392E342D31302E382C302C302C342E342D332C352E322D342E342C302C302C31
                    342E342D392E322C31382E362D392E322C302C302C392E322C352E322C31312E
                    362D312E382C302C302C352E382D312E382C31312E342D302E362C302C302C33
                    2E322D322E362C322E342D342E382C302C302C312E362D312E382C322E362C32
                    2C302C302C332E342C332E362C382E322C312E362C302C302C342D302E322C32
                    2C322E322C302C302D342E342C332E382D31362E322C342C302C302D31322E34
                    2C302E362D32382E382C382E322C302C302D32392E382C31302E342D33392C32
                    302E382C302C302D362E342C382E382D31312E382C31302C302C302D352E382C
                    302E382D31312E382C382E322C302C302C392E382D352E382C31382E382D352E
                    382C302C302C342D322E342C302E322C312E322C302C302D332E362C372E362D
                    322C31332C302C302D302E362C352E322D312E342C362E382C302C302D372E38
                    2C31322E382D372E382C31352E3273312E322C31322E322C312E362C31322E38
                    2D312D312E362C322E382C302E382C362E362C342C372E342C362E382D322D35
                    2E342D322E322D372E322D342E342D392D332E362D31312E3463302C302C312C
                    312C312E382C322E342C302C302D302E362D302E362C302D342E322C302C302C
                    302E382D352E322C322E322D382E3473332E342D372C332E382D372E382C302E
                    342D362E362C312E382D346C332E342C322E36732D322E382D322E362D302E36
                    2D342E3863302C302D312D352E362C302E382D382E322C302C302C372D382E34
                    2C382E362D392E3473302E322D302E362C302E322D302E362C362D342E322C30
                    2E322D322E3663302C302D342C312E362D372C312E362C302C302D372E362C32
                    2D332E362D322E327331342D392E362C31372E382D392E346C302E382C312E36
                    2C31312E322D322E342D312E322C302E38732D302E322D302E322C342D302E36
                    2C31302C312C31312E342D302E382C342E382D322E382C342E342D312E342D30
                    2E362C332E342D302E362C332E342C352D352E382C342E342D332E362D382E38
                    2C372E342D31302E322C31332E366C31302E342D382E322C332E362D3373332E
                    362C322E322C332E382C302E362C342E382D372E342C362D372E322C332E322D
                    322E362C332C302C372E342C382C372E342C382C332E322D312E382C342E362D
                    302E342C352E362D31392E382C352E362D31392E386C32352D31302E362C3433
                    2E362D332E342D31362E3939392D362E382D36312E3030312D31312E347A222F
                    3E0D0A20203C2F673E0D0A20203C672069643D226734343622207374726F6B65
                    2D77696474683D223222207374726F6B653D2223346330303030223E0D0A2020
                    203C706174682069643D22706174683434382220643D226D35312E342C383573
                    2D31352D31362E382D32332E342D31392E3463302C302D31332E342D362E382D
                    33382C31222F3E0D0A20203C2F673E0D0A20203C672069643D22673435302220
                    7374726F6B652D77696474683D223222207374726F6B653D2223346330303030
                    223E0D0A2020203C706174682069643D22706174683435322220643D226D3234
                    2E382C36342E32732D32352E322D382D34302E362D332E3863302C302D31382E
                    342C322D32362E382C31352E38222F3E0D0A20203C2F673E0D0A20203C672069
                    643D226734353422207374726F6B652D77696474683D223222207374726F6B65
                    3D2223346330303030223E0D0A2020203C706174682069643D22706174683435
                    362220643D226D32312E322C3633732D31372D372E322D33312E382D392E3463
                    302C302D31362E362D322E362D33332E322C342E362C302C302D31322E322C36
                    2D31372E362C31362E32222F3E0D0A20203C2F673E0D0A20203C672069643D22
                    6734353822207374726F6B652D77696474683D223222207374726F6B653D2223
                    346330303030223E0D0A2020203C706174682069643D22706174683436302220
                    643D226D32322E322C36332E34732D31352E342D31312D31362E342D31322E34
                    63302C302D372D31312D32302D31312E342C302C302D32312E342C302E382D33
                    382E362C382E38222F3E0D0A20203C2F673E0D0A20203C672069643D22673436
                    32222066696C6C3D2223303030223E0D0A2020203C706174682069643D227061
                    74683436342220643D224D32302E3839352C35342E34303763312E3534322C31
                    2E3436332C32382E3530352C33302E3339332C32382E3530352C33302E333933
                    2C33352E322C33362E362C372E322C322E342C372E322C322E342D372E362D34
                    2E382D31362E382D32332E362D31362E382D32332E362D312E322D322E382C31
                    342C372E322C31342C372E322C342C302E382C31372E362C32302C31372E362C
                    32302D362E382D322E342D322C342E382D322C342E382C322E382C322C32332E
                    3230312C31372E362C32332E3230312C31372E362C332E362C342C372E353939
                    2C352E362C372E3539392C352E362C31342D352E322C372E362C382C372E362C
                    382C322E342C362E382C382D342E382C382D342E382C31312E322D31362E382D
                    352E322D31342E342D352E322D31342E342D33302C322E382D33362E382D3133
                    2E322D33362E382D31332E322D322E342D322E342C362E342C302C362E342C30
                    2C382E3430312C322D372E322D31322E342D372E322D31322E342C322E342C30
                    2C31312E362C362E382C31312E362C362E382C31302E3430312C392E322C3132
                    2E3430312C372E322C31322E3430312C372E322C31372E3939392D382E382C32
                    382E3339392D312E322C32382E3339392D312E322C322C312E362D332E362C38
                    2E342D322C31332E3673362E342C31372E362C362E342C31372E36632D322E34
                    2C312E362D322C31322E342D322C31322E342C31362E382C32332E322C372E32
                    2C32312E322C372E322C32312E322D31352E362D302E342D302E382C372E322D
                    302E382C372E322C332E322C322C31322C392E322C31322C392E322D322E382D
                    312E322D342E342C342D342E342C342C342E382C342C322C382E382C322C382E
                    382D362C312E322D372E322C352E322D372E322C352E322C362E382C382D332E
                    322C382E342D332E322C382E342C332E362C342E342D312E322C31362E342D31
                    2E322C31362E342D342E382C302D31312E322C352E362D31312E322C352E362C
                    322E342C342E382D382C31302E342D382C31302E342D382E342C312E362D352E
                    362C382E342D352E362C382E342D372E3939392C362D31302E3339392C32322D
                    31302E3339392C32322D302E382C31302E342D332E322C31332E362C322C3131
                    2E362C352E3139392D322C342E3339392D31342E342C342E3339392D31342E34
                    2D342E3739392D31352E362C33382D33312E362C33382D33312E362C342D312E
                    362C342E382D362E382C342E382D362E382C322C302E342C31302E382C382C31
                    302E382C382C372E362C31312E322C382C322C382C322C312E322D332E362D30
                    2E342D392E362D302E342D392E362C362D32312E362D382D32382D382D32382D
                    31302D33332E362C342D32352E322C342D32352E322C322E382C352E362C3133
                    2E362C31302E382C31332E362C31302E386C332E362D322E34632D312E362D34
                    2E382C362E382D31302E382C362E382D31302E382C322E382C362E342C382E38
                    2D312E362C382E382D312E362C332E362D32342E342C31362D31302C31362D31
                    302C342C312E322C352E322D352E362C352E322D352E362C332E362D31302E34
                    2C302D32342C302D32342C332E362D302E342C31332E322C352E362C31332E32
                    2C352E362C322E382D332E362D362E342D32302E342D322E342D313873382E34
                    2C342C382E342C3463302E382D322D392E322D31342E342D392E322D31342E34
                    2D342E342D322E382D392E362D32332E322D392E362D32332E322C372E322C33
                    2E362D322E382D31312E362D322E382D31312E362C302D332E322C362D31342E
                    342C362D31342E342D302E382D362E382C302D362E342C302D362E342C322E38
                    2C312E322C31302E382C322E382C342D332E3673302E382D31312E322C302E38
                    2D31312E3263342E342D322E382D392E322D322E342D392E322D322E342D352E
                    322D342E342D342E382D382E342D342E382D382E342C382C322D362E342D3132
                    2E342D382E382D313673372E322D382E382C372E322D382E386331332E322D33
                    2E362C312E362D362E382C312E362D362E382D31392E362C302E342D382E382D
                    31302E342D382E382D31302E342C362C302E342C342E342D322C342E342D322D
                    352E322D312E322D31342E382D372E362D31342E382D372E362D342D332E362D
                    302E342D322E382D302E342D322E382C31362E382C312E322D31322D31302D31
                    322D31302C382C302D31302D31302E342D31302D31302E342D322D312E362D35
                    2E322D392E322D352E322D392E322D362D352E322D31302E382D31322D31302E
                    382D31322D302E342D342E342D352E322D392E322D352E322D392E322D31312E
                    362D31332E362D31372E322D31332E322D31372E322D31332E322D31342E382D
                    332E362D32302D322E382D32302D322E386C2D35322E382C342E34632D32362E
                    342C31322E382D31382E362C33332E382D31382E362C33332E382C362E342C38
                    2E342C31352E362C342E362C31352E362C342E362C342E362D362E322C31362E
                    322D342C31362E322D342C32302E3430312C332E322C31372E3830312D302E34
                    2C31372E3830312D302E342D322E342D342E362D31382E3630312D31302E382D
                    31382E3830312D31312E34732D392D342D392D34632D332D312E322D372E342D
                    31302E342D372E342D31302E342D332E322D332E342C31322E362C322E342C31
                    322E362C322E342D312E322C312C362E322C352C362E322C352C31372E343031
                    2D312C32382E3030312C392E382C32382E3030312C392E382C31302E3739392C
                    31362E362C31302E3939392C382E342C31302E3939392C382E342C322E382D39
                    2E342D392D33302E362D392D33302E362C302E342D322C382E362C342E362C38
                    2E362C342E362C312E342D322C322E322C332E382C322E322C332E382C302E32
                    2C322E342C342C31302E342C342C31302E342C322E382C31332C362E342C352E
                    362C362E342C352E366C342E362C392E3463312E342C322E362D342E362C3130
                    2E322D342E362C31302E322D302E322C322E382C302E362C322E362D352C3130
                    2E32732D322E322C31322D322E322C3132632D312E342C362E362C372E342C36
                    2E322C372E342C362E322C322E362C322E322C362C322E322C362C322E322C31
                    2E382C322C342E322C312E342C342E322C312E342C312E362D332E382C372E38
                    2D312E382C372E382D312E382C312E342D322E342C392E362D322E382C392E36
                    2D322E382C312D322E362C312E342D342E322C342E382D342E38732D32312E32
                    2D34332E362D32312E322D34332E3663362E342D302E382D312E382D31332E32
                    2D312E382D31332E322D322E322D362E362C392E322C382C31312E342C392E34
                    73332E322C332E362C312E362C332E342D332E342C322D322C322E322C31342E
                    342C31352E322C31372E382C32352E342C392E342C31342E322C31352E362C32
                    302E322C352E342C33302E322C352E342C33302E32632D302E342C382E382C35
                    2E362C31392E342C352E362C31392E342C322C332E382D322E322C32322D322E
                    322C32322D322C322E322D302E362C332D302E362C332C312C312E322C372E38
                    2C31342E342C372E382C31342E342D312E382D302E322C312E382C332E342C31
                    2E382C332E342C352E322C362D312E322C332D312E322C332D362D312E362C31
                    2C382E322C312C382E322C312E322C312E382D372E382D322E382D372E382D32
                    2E382D392E322D302E362C322E342C362E362C322E342C362E362C382E362C37
                    2E322D322E382C322E382D322E382C322E382D342E362D312E382D312E342C35
                    2D312E342C352C332E322C312E362C32302E342C382E362C32302E342C382E36
                    2C302E342C332E382D322E362C382E382D322E362C382E382C302E342C342D31
                    2E382C372E342D312E382C372E342D312E322C382E322D312E382C392D312E38
                    2C392D342E322C302E322D31312E362C31342D31312E362C31342D312E382C32
                    2E362D31322C31342E362D31322C31342E362D322C372D32302D302E322D3230
                    2D302E322D362E362C332E342D342E362C302D342E362C302D302E342D322E32
                    2C342E342D382E322C342E342D382E322C372D322E362C342E342D31332E342C
                    342E342D31332E342C342D312E342D372E322D342E322D372D352E3473362D32
                    2E362C362D322E3663382D322C332E362D342E342C332E362D342E342D302E36
                    2D342C322E342D392E362C322E342D392E362C31312E362D302E382C302D3137
                    2C302D31372D31302E382D372E362D31312E382D31332E342D31312E382D3133
                    2E342C31322E362D382E322C342E342D32302E362C342E362D32342E3273312E
                    342D32352E322C312E342D32352E32632D322D362E322D352D31392E382D352D
                    31392E382C322E322D352E322C392E362D31372E382C392E362D31372E382C32
                    2E382D342E322C31312E362D392C392E342D3132732D31302D312E322D31302D
                    312E32632D372E382D312E342D372E322C332E382D372E322C332E382D312E36
                    2C312D322E342C362D322E342C362D302E37322C372E3933332D392E362C3134
                    2E322D392E362C31342E322D31312E322C362E322D322C31302E322D322C3130
                    2E322C362C362E362D332E382C362E382D332E382C362E382D31312D312E382D
                    322E382C382E342D322E382C382E342C31302E382C31322E382C372E382C3135
                    2E362C372E382C31352E362D31302E322C312C322E342C31302E322C322E342C
                    31302E32732D302E382D322D302E362D302E322C332E322C362C342C382D332E
                    322C322E322D332E322C322E3263302E362C392E362D31342E382C352E342D31
                    342E382C352E346C2D312E362C302E32632D312E362C302E322D31322E382D30
                    2E362D31382E362D322E38732D31322E3539392D322E322D31322E3539392D32
                    2E322D342C312E382D31312E3630312C312E36632D372E362D302E322D31352E
                    362C322E362D31352E362C322E362D342E342D302E342C342E322D342E382C34
                    2E342D342E3673352E382D352E342D322E322D342E38632D32312E3739372C31
                    2E3633352D33322E362D382E362D33322E362D382E362D322D312E342D342E36
                    2D342E322D342E362D342E322D31302D322C312E342C31322E342C312E342C31
                    322E342C312E322C312E342D302E322C322E342D302E322C322E342D302E382D
                    312E362D382E362D372D382E362D372D322E3831312D302E3937332D342E3137
                    342D322E3330372D362E3530352D342E3739337A222F3E0D0A20203C2F673E0D
                    0A20203C672069643D2267343636222066696C6C3D2223346330303030223E0D
                    0A2020203C706174682069643D22706174683436382220643D226D2D332C3432
                    2E387331312E362C352E362C31342E322C382E342C31362E362C31342E322C31
                    362E362C31342E322D352E342D322D382D332E382D31332E342D31302D31332E
                    342D31302D332E382D362D392E342D382E387A222F3E0D0A20203C2F673E0D0A
                    20203C672069643D2267343730222066696C6C3D2223393963633332223E0D0A
                    2020203C706174682069643D22706174683437322220643D224D2D36312E3030
                    392C31312E36303363302E3333372D302E3134382D302E3138372D322E38362D
                    302E3339312D332E3430332D312E3032322D322E3732362D31302D342E322D31
                    302D342E322D302E3232372C312E3336352D302E3238322C322E3936312D302E
                    3137362C342E3539392C302C302C342E3836382C352E3531392C31302E353637
                    2C332E3030347A222F3E0D0A20203C2F673E0D0A20203C672069643D22673437
                    34222066696C6C3D2223363539393030223E0D0A2020203C706174682069643D
                    22706174683437362220643D224D2D36312E3030392C31312E343033632D302E
                    3434392C302E3135382D302E3031352D322E3733342D302E3139312D332E3230
                    332D312E3032322D322E3732362D31302E322D342E332D31302E322D342E332D
                    302E3232372C312E3336352D302E3238322C322E3936312D302E3137362C342E
                    3539392C302C302C342E3236382C352E3131392C31302E3536372C322E393034
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D2267343738222066696C
                    6C3D2223303030223E0D0A2020203C706174682069643D227061746834383022
                    20643D226D2D36352E342C31312E353436632D302E3632352C302D312E313331
                    2D312E31342D312E3133312D322E3534362C302D312E3430352C302E3530362D
                    322E3534352C312E3133312D322E35343573312E3133322C312E31342C312E31
                    33322C322E35343563302C312E3430362D302E3530372C322E3534362D312E31
                    33322C322E3534367A222F3E0D0A20203C2F673E0D0A20203C672069643D2267
                    343832222066696C6C3D2223303030223E0D0A2020203C706174682069643D22
                    706174683438342220643D224D2D36352E342C397A222F3E0D0A20203C2F673E
                    0D0A20203C672069643D2267343836222066696C6C3D2223303030223E0D0A20
                    20203C706174682069643D22706174683438382220643D226D2D3131312C3130
                    392E36732D352E362C31302C31392E322C3463302C302C31342D312E322C3136
                    2E342D332E362C312E322C302E382C392E3536362C332E37332C31322E342C34
                    2E342C362E382C312E362C31352E322D382E342C31352E322D382E3473342E36
                    2D31302E352C372E342D31302E352D302E342C312E362D302E342C312E362D36
                    2E362C31302E312D362E322C31312E3763302C302D352E322C32302D32312E32
                    2C32302E382C302C302D31362E31352C302E39352D31342E382C362E382C302C
                    302C382E382D322E342C31312E322C302C302C302C31302E382D302E342C322E
                    382C366C2D362E382C31312E3673302E31342C332E39322D31302C302E34632D
                    392E382D332E342D32302E312D31362E332D32302E312D31362E33732D31352E
                    39352D31342E35352D352E312D32382E357A222F3E0D0A20203C2F673E0D0A20
                    203C672069643D2267343930222066696C6C3D2223653539393939223E0D0A20
                    20203C706174682069643D22706174683439322220643D226D2D3131322E322C
                    3131332E36732D322C392E362C33342E382D302E386C362E382C302E3863322E
                    342C302E382C31342E342C332E362C31362E342C322E342C302C302D372E322C
                    31332E362D31382E382C31322C302C302D31332E322C312E362D31322E382C36
                    2E342C302C302C342C372E322C382E382C392E362C302C302C322E382C322E34
                    2C322E342C352E36732D332E322C342E382D352E322C352E362D352E322D322E
                    342D362E382D322E342D31302D362E342D31342E342D31312E322D31322E382D
                    31362E382D31322E342D31392E362C312E322D382E342C312E322D382E347A22
                    2F3E0D0A20203C2F673E0D0A20203C672069643D2267343934222066696C6C3D
                    2223623236353635223E0D0A2020203C706174682069643D2270617468343936
                    2220643D226D2D3130392C3133312E303563322E362C332E39352C352E382C38
                    2E31352C382C31302E35352C342E342C342E382C31322E382C31312E322C3134
                    2E342C31312E3273342E382C332E322C362E382C322E342C342E382D322E342C
                    352E322D352E362D322E342D352E362D322E342D352E36632D332E3036362D31
                    2E35332D352E3830362D352E30322D372E3338352D372E33352C302C302C302E
                    3138352C322E35352D352E3031352C312E3735732D31302E342D332E362D3132
                    2D362E382D342D352E362D322E342D322C342C372E322C352E362C372E362C31
                    2E322C312E362D312E322C312E322D352E322D302E382D392E362D367A222F3E
                    0D0A20203C2F673E0D0A20203C672069643D2267343938222066696C6C3D2223
                    393932363030223E0D0A2020203C706174682069643D22706174683530302220
                    643D226D2D3131312E362C31313073312E382D31332E362C332D31372E366330
                    2C302D302E382D362E382C312E362D313173342E342D31302E342C372E342D31
                    352E382C332E322D392E342C372E322D31312C31302D31302E322C31322E382D
                    31312E322C322E362D302E322C322E362D302E322C362E382D31342E382C3230
                    2E342D31302E3863302C302D31362E322D322E382D302E342D31322E322C302C
                    302D342E382C312E312D312E352D352E392C322E3230312D342E3636382C312E
                    372C322E312D392E332C31332E392C302C302D352C382E362D31302E322C3131
                    2E36732D31372E322C31302D31382E342C31332E382D342E342C392E362D362E
                    342C31312E322D342E382C352E382D352E322C392E3263302C302D312E322C34
                    2D322E362C352E32732D312E362C342E342D312E362C362E342D322C342E382D
                    312E382C372E3263302C302C302E382C31392C302E342C32316C322D332E387A
                    222F3E0D0A20203C2F673E0D0A20203C672069643D2267353032222066696C6C
                    3D2223464646223E0D0A2020203C706174682069643D22706174683530342220
                    643D226D2D3132302E322C3131342E36732D322D312E342D362E342C342E3663
                    302C302C372E332C33332C372E332C33342E342C302C302C312E312D322E312D
                    302E322D392E33732D322E322D31392E392D322E322D31392E396C312E352D39
                    2E387A222F3E0D0A20203C2F673E0D0A20203C672069643D2267353036222066
                    696C6C3D2223393932363030223E0D0A2020203C706174682069643D22706174
                    683530382220643D226D2D39382E362C3534732D31372E362C332E322D31372E
                    322C33322E346C2D302E382C32342E38732D312E322D32352E362D322E342D32
                    372E322C322E382D31322E382D302E342D362E3863302C302D31342C31342D36
                    2C33352E322C302C302C312E352C332E332D312E352D312E332C302C302D342E
                    362D31322E362D332E352D31392C302C302C302E322D322E322C322E312D352C
                    302C302C382E362D31312E372C31312E332D31342C302C302C312E382D31342E
                    342C31372E322D31392E362C302C302C352E372D322E332C312E322C302E357A
                    222F3E0D0A20203C2F673E0D0A20203C672069643D2267353130222066696C6C
                    3D2223303030223E0D0A2020203C706174682069643D22706174683531322220
                    643D226D34302E382D31322E3263302E36362D302E3335342C302E3635312D31
                    2E3332342C312E3233312D312E3439372C312E3134392D302E3334342C312E33
                    31332D312E3431312C312E3833312D322E3139352C302E3837332D312E333139
                    2C312E3036362D322E3835322C312E3634382D342E3334332C302E3237322D30
                    2E372C302E3239392D312E3635352D302E3031342D322E3331352D312E313734
                    2D322E3438312D312E3837362D342E39332D332E3331382D372E3335362D302E
                    3236382D302E34352D302E35332D312E3234342D302E3733312D312E3834322D
                    302E3436332D312E3338342D312E37322D322E3337352D322E35382D332E3639
                    352D302E3238382D302E3434312C302E3233372D312E3336362D302E3437392D
                    312E34352D302E3839372D302E3130352D322E3334362D302E3638352D322E35
                    37392C302E3334312D302E3538382C322E3538372C302E3432332C352E31312C
                    312E3339312C372E3535322D302E3738322C302E3639322D302E3434382C312E
                    3631332D302E3239362C322E33382C302E37312C332E3630362D302E3438382C
                    362E3935382D312E3234392C31302E3433322D302E3032332C302E3130342C30
                    2E3331392C302E3330322C302E3239312C302E3336342D312E3232322C322E36
                    38362D322E3637342C352E3133312D342E3439332C372E3531322D302E373538
                    2C302E3939322D312E36332C312E3930382D322E3132372C322E3937312D302E
                    3336382C302E3738372D302E3737362C312E3735332D302E3532362C322E3734
                    312D332E3433352C322E37382D352E3638352C362E3632352D382E3239362C31
                    302E3437312D302E3436322C302E36382D302E3137312C312E3838392C302E33
                    382C322E3135382C302E3831332C302E3339382C312E3736392D302E3632362C
                    322E3233392D312E3437322C302E3338392D302E3639382C302E3734322D312E
                    3334382C312E3233332D312E3939312C302E3133332D302E3137352D302E3034
                    362D302E3539342C302E3038392D302E3731352C322E3633332D322E3334372C
                    342E3330322D352E3238332C362E3735352D372E3635312C312E39352D302E33
                    32392C332E3438372D312E3332372C352E3233352D322E33342C302E3330382D
                    302E3137392C302E3833322C302E30372C312E3132322D302E3132352C312E37
                    35332D312E3137372C312E3735312D332E3231332C312E3835372D352E313233
                    2C302E30352D302E3838342C302E3234362D322E3230312C312E3338362D322E
                    3831327A222F3E0D0A20203C2F673E0D0A20203C672069643D22673531342220
                    66696C6C3D2223303030223E0D0A2020203C706174682069643D227061746835
                    31362220643D226D33312E3935392D31362E36363663302E3132342D302E3037
                    372D302E3033312D302E352C302E3037382D302E3731362C302E3136322D302E
                    3332342C302E3536352D302E3531322C302E3732372D302E3833362C302E3130
                    392D302E3231362D302E3035342D302E3539362C302E3038322D302E3733382C
                    322E3333332D322E3434372C322E35392D352E3437312C312E3535342D382E34
                    34342C312E3032342D302E36322C312E3038352D312E3838322C302E36362D32
                    2E3732392D302E3835332D312E372D312E3034362D332E3632362D322E303231
                    2D352E3136392D302E3830322D312E3236392D322E33382D322E3531332D332E
                    3735312D312E32312D302E3432312C302E342D302E3734322C312E3138372D30
                    2E3436342C312E3839392C302E3036342C302E3136332C302E3334392C302E33
                    30392C302E3332322C302E3339312D302E3130372C302E3332342D302E363533
                    2C302E3534382D302E3635392C302E38322D302E30332C312E3439362D302E39
                    38342C332E3030372D302E3335342C342E3333362C302E3737322C312E363239
                    2C312E3539312C332E3438362C322E3236372C352E3236322D312E3233342C32
                    2E3131362D302E3230312C342E3536352D312E3935342C362E3434322D302E31
                    33362C302E3134362D302E3132372C302E3533322D302E3030352C302E373334
                    2C302E3239322C302E3438362C302E3639382C302E3839322C312E3138342C31
                    2E3138342C302E3230322C302E3132312C302E35352C302E3132332C302E3735
                    2D302E3030312C302E3537382D302E3336322C302E3937362D302E3834392C31
                    2E3538342D312E3232357A222F3E0D0A20203C2F673E0D0A20203C672069643D
                    2267353138222066696C6C3D2223303030223E0D0A2020203C70617468206964
                    3D22706174683532302220643D226D39342E3737312D32362E39373763312E33
                    38392C312E3739322C312E3637392C342E3538372D302E33372C352E3937372C
                    302E35352C332E3330392C332E3930312C312E33332C352E3939392C302E382D
                    302E31312D302E3338382C302E31322D302E3733322C302E342D302E3733372C
                    312E30362D302E3031352C312E37342D312E3034372C322E382D302E3836332C
                    302E34342D312E3535372C322E30372D322E3235392C322E37322D332E363339
                    2C312E37322D332E3639352C312E31332D372E3936382D312E34352D31312E32
                    31342D302E322D302E3235342C302E30312D302E3737312D302E31312D312E31
                    33332D302E37362D322E3231312D322E38322D322E3532362D342E37362D332E
                    3231342D312E3137362D332E3837352D312E3833372D372E3930362D332E3539
                    392D31312E362D312E3631342D302E32352D322E3331322D312E3938392D332E
                    3634392D322E3730392D312E3333332D302E3731392D312E3930312C302E3836
                    2D312E38362C312E3930362C302E3030372C302E3230352C302E3435392C302E
                    3432392C302E3238392C302E3739342D302E3037362C302E3136342D302E3333
                    362C302E3237352D302E3333362C302E3430392C302E3030312C302E3133352C
                    302E3232322C302E3236362C302E3335362C302E342D302E3931382C302E3832
                    2D322E3334312C312E3239372D322E3633362C322E3434322D302E3935342C33
                    2E37312C312E3631392C362E3833352C332E3238372C31302E3033362C302E35
                    39312C312E3133352D302E3134352C322E3430362D302E3930352C332E363134
                    2D302E3433382C302E3639352D302E33332C312E3832322D302E3035342C322E
                    3637382C302E3735322C322E3333312C322E3334332C342E30372C332E383738
                    2C362E3035337A222F3E0D0A20203C2F673E0D0A20203C672069643D22673532
                    32222066696C6C3D2223303030223E0D0A2020203C706174682069643D227061
                    74683532342220643D226D35372E3631312D382E353931632D312E3438372C31
                    2E3835312D342E3839392C342E34322D312E3938322C362E3334382C302E3139
                    342C302E3132392C302E3536342C302E3133332C302E3733372D302E3030312C
                    322E3032312D312E3536352C342E3032342D322E3436382C362E34362D332E30
                    352C302E3132342D302E3032392C302E3339382C302E3433382C302E3736372C
                    302E3237372C312E3631332D302E3730332C332E3632332D302E3634352C342E
                    3830372D312E3938332C332E3736372C302E3232342C372E3333322D302E3839
                    322C31302E3732332D322E322C312E3136312D302E3434382C322E3433312D31
                    2E3030372C332E3633322D312E3530392C312E3337362D302E3537362C322E35
                    382D312E3530342C332E3639322D322E3634352C302E3133332D302E3133362C
                    302E3438372D302E3034362C302E3735342D302E3034362D302E30342D302E38
                    36332C302E3932322D302E39392C312E3136392D312E3631322C302E3039322D
                    302E3233322D302E3035382D302E3632382C302E3037352D302E37332C322E31
                    33382D312E36332C332E3035382D332E3634382C312E3838392D362E3032352D
                    302E3238352D302E3537382D302E3533342D312E3139362D312E312D312E3637
                    322D312E3038352D302E3931312D322E3138372D302E3035372D332E3233342D
                    302E3336312D302E3135392C302E3632382D302E3838382C302E3435362D312E
                    3237342C302E3635342D302E3835392C302E3433392D322E3139322D302E3134
                    362D332E3035312C302E3239322D312E3336322C302E3639352D322E3630332C
                    302E3836342D342E3032352C312E3234312D302E3331322C302E3038322D312E
                    30392D302E3031342D312E32352C302E3631332D302E3133342D302E3133342D
                    302E3238322D302E3336382D302E3338382D302E3334362D312E3930382C302E
                    3339362D332E3136382C302E36312D342E3436392C322E3330322D302E313033
                    2C302E3133332D302E3534352D302E3034362D302E3730342C302E3038392D30
                    2E3935372C302E3830382D312E3336322C322E3034322D322E3436332C322E37
                    31342D302E3230312C302E3132332D302E3535332D302E3034352D302E373437
                    2C302E3038342D302E3634362C302E3433312D312E3031332C312E3037322D31
                    2E3635352C312E3531392D302E3332392C302E3232392D302E3732392D302E30
                    39362D302E3639372D302E3335322C302E3234352D312E3934372C302E383938
                    2D332E3733342C302E3332332D352E36312C322E3037372D322E35322C342E35
                    39342D342E3436392C362E342D372E322C302E3031352D322E3136362C302E37
                    30372D342E3331322C302E3539342D362E3338392D302E30312D302E3139332D
                    302E3239382D302E3932362D302E3432342D312E3237332D302E3331322D302E
                    3835342C302E3539342D312E39322D302E32352D322E3634342D312E3430342D
                    312E3230332D322E3639362D302E3332372D332E35322C312E3130362D312E38
                    33382C302E33392D332E3930342C312E3038332D352E3438322D302E3135312D
                    312E3030372D302E3738372D312E3538352D312E3639332D322E3338342D322E
                    3734392D302E3938352D312E3330322D302E36352D322E3733382D302E35382D
                    342E3330322C302E3030362D302E3132382D302E3330392D302E3236342D302E
                    3330392D302E3339382C302E3030312D302E3133352C302E3232312D302E3236
                    362C302E3335352D302E342D302E3730362D302E3632362D302E3938312D312E
                    3638342D322D322C302E3330352D312E3039322D302E3337312D312E3937362D
                    312E3234322D322E3237382D312E3939352D302E3639312D332E3637322C312E
                    3232312D352E3536342C312E3239342D302E3531342C302E3031392D302E3938
                    312D312E3031392D312E36332D312E3334342D302E3433322D302E3231362D31
                    2E3133362D302E3234392D312E3439382C302E3031372D302E3638382C302E35
                    30342D312E3237372C302E3631382D322E3033352C302E3832332D312E363137
                    2C302E3433362D322E3839352C312E35332D342E3337352C322E3338352D312E
                    3438352C302E3835372D322E34342C322E3239342D332E35322C332E3631342D
                    302E3934312C312E3135322D312E3037372C332E3536362C302E3334332C342E
                    3036362C312E3834332C302E36352C332E3134372D322E3035332C352E313133
                    2D312E3732372C302E3331322C302E3035312C302E3531382C302E3336322C30
                    2E3430382C302E37352C302E3338392C302E3130392C302E3630372D302E3132
                    2C302E382D302E342C302E3835382C312E3031392C322E3032322C312E333536
                    2C322E39362C322E3232392C302E39372C302E3930342C322E3731362C302E34
                    38362C332E3733312C312E3438332C312E3532392C312E3530322C302E39372C
                    342E3138332C322E3930392C352E3438382D302E3538362C312E3331332D312E
                    3139332C322E35392D312E3532382C342E3031372D302E3238322C312E323036
                    2C302E3731322C322E3430332C312E3932332C322E3331322C312E3235382D30
                    2E3039342C312E35322D302E3835332C322E3030352D312E3932392C302E3236
                    372C302E3236372C302E3733362C302E3536342C302E3639352C302E37382D30
                    2E3435372C322E3338372D312E3438342C342E33382D312E3934322C362E3831
                    312D302E3035392C302E3331372D302E3336342C302E3531392D302E3735332C
                    302E3430392D302E3436382C342E3134392D342E35322C362E3534332D372E30
                    36352C392E3730382D302E3430332C302E3530322D302E3430372C312E373531
                    2C302E3030322C322E3135342C312E3430332C312E3338372C332E3336332D30
                    2E3135392C352E3036332D302E3636322C302E3231332D312E3230362C312E30
                    37322D322E3134382C322E3430342D322E3039322C302E3235362C302E30312C
                    302E3439312D302E3533322C302E3831352D302E3636322C302E3334382D302E
                    3133382C302E38352C302E3038362C312E3133362D302E3131322C312E373239
                    2D312E3139352C332E3133372D322E3330312C342E3837352D332E34392C302E
                    3139322D302E3133312C302E3533362C302E3032382C302E3735322D302E3038
                    2C302E3332352D302E3136322C302E3531322D302E3534392C302E3833352D30
                    2E3733342C302E3334382D302E322C302E35392C302E30392C302E3738332C30
                    2E33372D302E3634362C302E3334392D302E36352C312E3330362D312E323332
                    2C312E3530382D302E3737352C302E3236382D312E3333362C302E3738312D32
                    2E30312C312E3232382D302E3239322C302E3139332D302E3935312D302E3035
                    352D312E3035352C302E3132342D302E3539382C312E3032382D312E3738322C
                    312E3436362D322E3439322C322E3334397A222F3E0D0A20203C2F673E0D0A20
                    203C672069643D2267353236222066696C6C3D2223303030223E0D0A2020203C
                    706174682069643D22706174683532382220643D226D322E322D3538732D392E
                    3233382D322E3837322D32302E342C32322E3863302C302D322E342C352E322D
                    342E382C372E32732D31332E362C352E362D31352E362C392E366C2D31302E34
                    2C31367331342E382D31362C31382D31382E3463302C302C382D382E342C342E
                    382D312E362C302C302D31342C31302E382D31322E382C32302C302C302D352E
                    362C31342E342D362E342C31362E342C302C302C31362D33322C31382E342D33
                    332E3273332E362D312E322C322E342C322E342D312E362C32302D342E342C32
                    3263302C302C382D32302E342C372E322D32332E362C302C302C332E322D332E
                    362C352E362C312E366C2D312E322C31362C342E342C3132732D322E342D3131
                    2E322D302E382D32362E3863302C302D322D31302E342C322D342E387331332E
                    362C31312E362C31332E362C31362E3463302C302D352E322D31372E362D3134
                    2E342D32322E346C2D342C362D312E322D32732D332E362D302E382C302E382D
                    372E362C342D372E362C342D372E362C362E342C372E322C382C372E3263302C
                    302C31332E322D372E362C31342E342C31362E382C302C302C362E382D31342E
                    342D322E342D32312E322C302C302D31342E382D322D31332E362D372E326C37
                    2E322D31322E3463332E362D352E322C322D322E342C322D322E347A222F3E0D
                    0A20203C2F673E0D0A20203C672069643D2267353330222066696C6C3D222330
                    3030223E0D0A2020203C706174682069643D22706174683533322220643D226D
                    2D31372E382D34312E362D31362C352E322D372E322C392E367331372E322D31
                    302C32312E322D31312E322C322D332E362C322D332E367A222F3E0D0A20203C
                    2F673E0D0A20203C672069643D2267353334222066696C6C3D2223303030223E
                    0D0A2020203C706174682069643D22706174683533362220643D226D2D35372E
                    382D33352E32732D322C312E322D322E342C342D322E382C332E322D322C362C
                    322E382C352E322C322E382C312E322C312E362D362C322E342D372E322C322E
                    342D352E362D302E382D347A222F3E0D0A20203C2F673E0D0A20203C67206964
                    3D2267353338222066696C6C3D2223303030223E0D0A2020203C706174682069
                    643D22706174683534302220643D226D2D36362E362C3236732D382E342D342D
                    31312E362D372E362D322E3734382C312E3536362D372E362C312E32632D352E
                    3834372D302E3434312D342E382D31362E342D342E382D31362E346C2D342C37
                    2E36732D312E322C31342E342C362E382C313263332E3930372D312E3137322C
                    352E322C302E342C332E362C312E3273352E362C312E322C322E382C322E382C
                    31312E362D332E362C392E322C362E386C352E362D372E367A222F3E0D0A2020
                    3C2F673E0D0A20203C672069643D2267353432222066696C6C3D222330303022
                    3E0D0A2020203C706174682069643D22706174683534342220643D226D2D3739
                    2E322C34302E34732D31352E342C342E342D31392D352E3263302C302D342E38
                    2C322E342D322E362C352E3473332E342C332E342C332E342C332E342C352E34
                    2C312E322C342E382C322D332C342E322D332C342E322C31302E322D362C3136
                    2E342D392E387A222F3E0D0A20203C2F673E0D0A20203C672069643D22673534
                    36222066696C6C3D2223464646223E0D0A2020203C706174682069643D227061
                    74683534382220643D226D3134392E322C3131382E36632D302E34332C322E31
                    342D322E312C322E39342D342C332E362D312E39322D302E39362D342E35312D
                    342E30362D362E342D322D302E34372D302E34382D312E32352D302E35342D31
                    2E362D312E322D302E34362D302E392D302E31392D312E39342D302E35332D32
                    2E37342D302E35352D312E32382D312E32352D322E36342D312E30372D342E30
                    362C312E38312D302E37312C322E342D322E36322C312E39332D342E33382D30
                    2E30372D302E32362D302E352D302E34352D302E332D302E382C302E31392D30
                    2E33332C302E352D302E35352C302E37372D302E38322D302E31332C302E3134
                    2D302E32382C302E33372D302E33392C302E33352D302E36312D302E31312D30
                    2E34392D302E37352D302E33362D312E31332C302E35392D312E37352C322E36
                    2D322E30312C332E39352D302E38322C302E32362D302E35362C302E37372D30
                    2E33372C312E322D302E342D302E30352D302E35382C302E33362D312E31312C
                    302E35362D312E35332C302E35322D312E30392C322E31342C302E30312C322E
                    39342D302E362C312E30382D302E38332C322E31342D312E35322C332E32322D
                    302E39322C312E38312C312E30312C332E35322C322E32322C342E37322C332E
                    39372C302E35372C302E38332C302E38312C322E31312C302E37352C332E3037
                    2D302E30342C302E36352D312E34322C302E32392D312E37362C312E32322D30
                    2E36352C312E37352C312E31392C322E32372C312E39342C332E36312C302E32
                    2C302E33352D302E30362C302E36352D302E33382C302E37352D302E34312C30
                    2E31332D312E31392D302E30362D312E30362C302E33392C302E39382C332E31
                    392D312E37382C332E38372D342E31332C342E34347A222F3E0D0A20203C2F67
                    3E0D0A20203C672069643D2267353530222066696C6C3D2223464646223E0D0A
                    2020203C706174682069643D22706174683535322220643D226D3133392E362C
                    3133382E32632D302E30312D312E37342D312E36312D332E34392D302E342D35
                    2E322C302E31342C302E31342C302E32372C302E33362C302E342C302E33362C
                    302E31342C302C302E32372D302E32322C302E342D302E33362C312E352C322E
                    32322C352E31352C332E31342C352E30312C352E39392D302E30332C302E3435
                    2D312E31312C312E33372D302E32312C322E30312D312E38312C312E33352D31
                    2E38372C332E37322D322E382C352E362D312E32342D302E32382D322E34352D
                    302E36352D332E362D312E322C302E33352D312E34382C302E32342D332E3137
                    2C312E30362D342E34392C302E34332D302E372C302E31342D312E37382C302E
                    31342D322E37317A222F3E0D0A20203C2F673E0D0A20203C672069643D226735
                    3534222066696C6C3D2223434343223E0D0A2020203C706174682069643D2270
                    6174683535362220643D226D2D32362E362C3132392E32732D31362E3835382C
                    31302E31342D322E382D352E3263382E382D392E362C31382E382D31352E322C
                    31382E382D31352E327331302E342D342E342C31342D352E362C31382E382D36
                    2E342C32322D362E382C31322E382D342E342C31392E362D302E342C31342E38
                    2C382E342C31342E382C382E342D31362E342D382E342D32302D362D31302E38
                    2C322D31362E382C352E3263302C302D31342E382C342E342D31382C362E3473
                    2D31332E362C31332E362D31352E322C31322E382C302E342D312E322C312E36
                    2D342D302E382D342E342D382E382C322D392E322C382E342D392E322C382E34
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D2267353538222066696C
                    6C3D2223303030223E0D0A2020203C706174682069643D227061746835363022
                    20643D226D2D31392E3139352C3132332E323373312E34312D31332E30342C39
                    2E3838382D31312E333763302C302C382E3232362D342E31372C31302E393438
                    2D362E31342C302C302C382E3133392D312E372C392E3434392D322E33322C31
                    382E3437392D382E3639382C33332E3139382D342E3137392C33332E3734352D
                    352E3239392C302E3534362D312E3131392C32302E3137312C352E3939392C32
                    332E37382C31302E3037392C302E3339312C302E34352D31302E3233312D352E
                    35392D31392E3932392D372E34382D382E3237332D312E3631372D32392E3837
                    352C302E32342D34302E3738312C352E37382D322E3937332C312E35312D3131
                    2E3931382C372E32392D31342E3434392C372E3138732D31322E3635312C392E
                    35372D31322E3635312C392E35377A222F3E0D0A20203C2F673E0D0A20203C67
                    2069643D2267353632222066696C6C3D2223434343223E0D0A2020203C706174
                    682069643D22706174683536342220643D226D2D32332C3134382E38732D3135
                    2E322D322E342C312E362D3463302C302C31382D322C32322D372E322C302C30
                    2C31332E362D392E322C31362E342D392E367333322E382D372E362C33332E32
                    2D31302C362D322E342C372E362D312E362C302E382C322D322C322E382D3334
                    2C31372E322D34302E342C31382E342D31382C382E382D32322E382C31302D31
                    352E362C312E322D31352E362C312E327A222F3E0D0A20203C2F673E0D0A2020
                    3C672069643D2267353636222066696C6C3D2223303030223E0D0A2020203C70
                    6174682069643D22706174683536382220643D226D2D332E34382C3134312E34
                    732D382E3538322D302E38332C302E3031392D312E363463302C302C382E3831
                    362D332E34332C31302E3836342D362E30392C302C302C362E3936342D342E37
                    312C382E3339372D342E39322C312E3433342D302E322C31352E3339342D332E
                    38392C31352E3539392D352E31327333342E3237312D31332E38312C33382E36
                    39312D31302E363263322E3931312C322E312D362E39392C302E34332D31362E
                    3632342C342E38342D312E3335352C302E36322D33352E3230382C31352E322D
                    33382E3438352C31352E38322D332E3237372C302E36312D392E3231362C342E
                    352D31312E3637342C352E31322D322E3435372C302E36312D362E3738372C32
                    2E36312D362E3738372C322E36317A222F3E0D0A20203C2F673E0D0A20203C67
                    2069643D2267353730222066696C6C3D2223303030223E0D0A2020203C706174
                    682069643D22706174683537322220643D226D2D31312E342C3134332E367335
                    2E322D302E342C342C312E322D332E362C302E382D332E362C302E386C2D302E
                    342D327A222F3E0D0A20203C2F673E0D0A20203C672069643D22673537342220
                    66696C6C3D2223303030223E0D0A2020203C706174682069643D227061746835
                    37362220643D226D2D31382E362C3134352E3273352E322D302E342C342C312E
                    322D332E362C302E382D332E362C302E386C2D302E342D327A222F3E0D0A2020
                    3C2F673E0D0A20203C672069643D2267353738222066696C6C3D222330303022
                    3E0D0A2020203C706174682069643D22706174683538302220643D226D2D3239
                    2C3134362E3873352E322D302E342C342C312E322D332E362C302E382D332E36
                    2C302E386C2D302E342D327A222F3E0D0A20203C2F673E0D0A20203C67206964
                    3D2267353832222066696C6C3D2223303030223E0D0A2020203C706174682069
                    643D22706174683538342220643D226D2D33362E362C3134372E3673352E322D
                    302E342C342C312E322D332E362C302E382D332E362C302E386C2D302E342D32
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D2267353836222066696C
                    6C3D2223303030223E0D0A2020203C706174682069643D227061746835383822
                    20643D226D312E382C3130382C332E322C312E36632D312E322C312E362D342E
                    342C312E322D342E342C312E326C312E322D322E387A222F3E0D0A20203C2F67
                    3E0D0A20203C672069643D2267353930222066696C6C3D2223303030223E0D0A
                    2020203C706174682069643D22706174683539322220643D226D2D382E322C31
                    31332E3673362E3530362D322E31342C342C312E32632D312E322C312E362D33
                    2E362C302E382D332E362C302E386C2D302E342D327A222F3E0D0A20203C2F67
                    3E0D0A20203C672069643D2267353934222066696C6C3D2223303030223E0D0A
                    2020203C706174682069643D22706174683539362220643D226D2D31392E342C
                    3131382E3473352E322D302E342C342C312E322D332E362C302E382D332E362C
                    302E386C2D302E342D327A222F3E0D0A20203C2F673E0D0A20203C672069643D
                    2267353938222066696C6C3D2223303030223E0D0A2020203C70617468206964
                    3D22706174683630302220643D226D2D32372C3132342E3473352E322D302E34
                    2C342C312E322D332E362C302E382D332E362C302E386C2D302E342D327A222F
                    3E0D0A20203C2F673E0D0A20203C672069643D2267363032222066696C6C3D22
                    23303030223E0D0A2020203C706174682069643D22706174683630342220643D
                    226D2D33332E382C3132392E3273352E322D302E342C342C312E322D332E362C
                    302E382D332E362C302E386C2D302E342D327A222F3E0D0A20203C2F673E0D0A
                    20203C672069643D2267363036222066696C6C3D2223303030223E0D0A202020
                    3C706174682069643D22706174683630382220643D226D352E3238322C313335
                    2E3673362E3932312D302E35332C352E3332342C312E36632D312E3539372C32
                    2E31322D342E3739322C312E30362D342E3739322C312E30366C2D302E353332
                    2D322E36367A222F3E0D0A20203C2F673E0D0A20203C672069643D2267363130
                    222066696C6C3D2223303030223E0D0A2020203C706174682069643D22706174
                    683631322220643D226D31352E3638322C3133302E3873362E3932312D302E35
                    332C352E3332342C312E36632D312E3539372C322E31322D342E3739322C312E
                    30362D342E3739322C312E30366C2D302E3533322D322E36367A222F3E0D0A20
                    203C2F673E0D0A20203C672069643D2267363134222066696C6C3D2223303030
                    223E0D0A2020203C706174682069643D22706174683631362220643D226D3236
                    2E3438322C3132362E3473362E3932312D302E35332C352E3332342C312E3663
                    2D312E3539372C322E31322D342E3739322C312E30362D342E3739322C312E30
                    366C2D302E3533322D322E36367A222F3E0D0A20203C2F673E0D0A20203C6720
                    69643D2267363138222066696C6C3D2223303030223E0D0A2020203C70617468
                    2069643D22706174683632302220643D226D33362E3838322C3132312E367336
                    2E3932312D302E35332C352E3332342C312E36632D312E3539372C322E31322D
                    342E3739322C312E30362D342E3739322C312E30366C2D302E3533322D322E36
                    367A222F3E0D0A20203C2F673E0D0A20203C672069643D226736323222206669
                    6C6C3D2223303030223E0D0A2020203C706174682069643D2270617468363234
                    2220643D226D392E3238322C3130332E3673362E3932312D302E35332C352E33
                    32342C312E36632D312E3539372C322E31322D352E3539322C312E38362D352E
                    3539322C312E38366C302E3236382D332E34367A222F3E0D0A20203C2F673E0D
                    0A20203C672069643D2267363236222066696C6C3D2223303030223E0D0A2020
                    203C706174682069643D22706174683632382220643D226D31392E3238322C31
                    30302E3473362E3932312D302E3533342C352E3332342C312E36632D312E3539
                    372C322E31322D352E3939322C312E38362D352E3939322C312E38366C302E36
                    36382D332E34367A222F3E0D0A20203C2F673E0D0A20203C672069643D226736
                    3330222066696C6C3D2223303030223E0D0A2020203C706174682069643D2270
                    6174683633322220643D226D2D332E342C3134302E3473352E322D302E342C34
                    2C312E322D332E362C302E382D332E362C302E386C2D302E342D327A222F3E0D
                    0A20203C2F673E0D0A20203C672069643D2267363334222066696C6C3D222339
                    3932363030223E0D0A2020203C706174682069643D2270617468363336222064
                    3D226D2D37362E362C34312E32732D342E342C382E382D342E382C313263302C
                    302C302E382D382E382C322D31302E3873322E382D312E322C322E382D312E32
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D2267363338222066696C
                    6C3D2223393932363030223E0D0A2020203C706174682069643D227061746836
                    34302220643D226D2D39352C35352E32732D332E322C31342E342D322E382C31
                    372E3263302C302D312E322D31312E362D302E382D31322E3873332E362D342E
                    342C332E362D342E347A222F3E0D0A20203C2F673E0D0A20203C672069643D22
                    67363432222066696C6C3D2223434343223E0D0A2020203C706174682069643D
                    22706174683634342220643D226D2D37342E322D31392E342D302E322C332E32
                    2D322E322C302E327331342E322C31322E362C31342E382C32302E3263302C30
                    2C302E382D382E322D31322E342D32332E367A222F3E0D0A20203C2F673E0D0A
                    20203C672069643D2267363436222066696C6C3D2223303030223E0D0A202020
                    3C706174682069643D22706174683634382220643D226D2D37302E3231362D31
                    382E313335632D302E3433312D302E3431362D302E3231322D312E3136312D30
                    2E36322D312E3432312D302E3830392D302E3531362C312E3239382D302E3537
                    332C312E30372D312E3238392D302E3338332D312E3230362D302E3139362D31
                    2E3232372D302E3331382D322E3530332D302E3035372D302E3539382C302E35
                    33312D322E3133382C302E3931362D322E3537382C312E3434362D312E363532
                    2C302E3132322D342E3538342C312E3736322D362E3133352C302E3330342D30
                    2E3238392C302E36382D302E3834312C302E3936352D312E3235392C302E3635
                    392D302E3936332C312E3834332D312E3435312C322E3739332D322E3237392C
                    302E3331382D302E3237362C302E3131372D312E3130332C302E3638362D312E
                    3031312C302E3731342C302E3131352C312E3935352D302E3031352C312E3931
                    2C302E3832362D302E3131332C322E31322D312E3434322C332E38342D322E37
                    32322C352E3530382C302E3435312C302E3730342D302E3030372C312E333339
                    2D302E3239312C312E3839362D312E3333352C322E36322D312E3134362C352E
                    3436312D312E33322C382E3330312D302E3030352C302E3038352D302E333132
                    2C302E3136332D302E3330342C302E3231362C302E3335332C322E3333352C30
                    2E3933372C342E3533342C312E3831362C362E3736332C302E3336362C302E39
                    332C302E3833372C312E3832352C302E3938372C322E3735322C302E3131312C
                    302E3638362C302E3231342C312E3531392D302E3139342C322E3232342C322E
                    3033352C322E38392C302E3732362C352E3534312C312E3839352C392E303732
                    2C302E3230372C302E3632352C312E3839392C322E3533392C312E3433362C32
                    2E3337382D322E3531332D302E3837312D322E3632352D312E3236392D322E38
                    30322D322E3032322D302E3134362D302E3632332D302E3437362D322D302E37
                    31332D322E3630322D302E3036342D302E3136342D302E3233352D322E303438
                    2D302E3331332D322E31372D312E3531332D322E3338322D302E3135352D322E
                    3230362D312E3532352D342E3536342D312E3432382D302E36382D322E333934
                    2D312E3738342D332E3531372D322E3934362D302E3139382D302E3230342C30
                    2E3934352D302E3932382C302E3736342D312E3134312D312E3039322D312E32
                    38392D322E3234352D322E3035362D312E3930392D332E3534392C302E313535
                    2D302E36392C302E3239322D312E3734372D302E3435322D322E3436377A222F
                    3E0D0A20203C2F673E0D0A20203C672069643D2267363530222066696C6C3D22
                    23303030223E0D0A2020203C706174682069643D22706174683635322220643D
                    226D2D37332E382D31362E3473302E342C362E382C322E382C382E342C312E32
                    2C302E382D322D302E342D322D322D322D322D322E382C302E342D302E342C32
                    2E342C362C342E342C342E342C342E342D392E322D342D392E322D362E382D31
                    2D362E392D312D362E392C312E312D302E382C352E392D302E3763302C302C31
                    2E342C302E372C312E352C312E367A222F3E0D0A20203C2F673E0D0A20203C67
                    2069643D226736353422207374726F6B652D77696474683D22302E3122207374
                    726F6B653D2223303030222066696C6C3D2223464646223E0D0A2020203C7061
                    74682069643D22706174683635362220643D226D2D37342E362C322E32732D38
                    2E35322D322E3739312D32372C302E3663302C302C392E3033312D322E303738
                    2C32372E382C302E322C31302E332C312E32352D302E382D302E382D302E382D
                    302E387A222F3E0D0A20203C2F673E0D0A20203C672069643D22673635382220
                    7374726F6B652D77696474683D22302E3122207374726F6B653D222330303022
                    2066696C6C3D2223464646223E0D0A2020203C706174682069643D2270617468
                    3636302220643D226D2D37322E3530322C322E313239732D382E3234362D332E
                    3531382D32362E3935312D312E37333763302C302C392E3137382D312E323839
                    2C32372E3637392C322E3630332C31302E3135342C322E3133362D302E373238
                    2D302E3836362D302E3732382D302E3836367A222F3E0D0A20203C2F673E0D0A
                    20203C672069643D226736363222207374726F6B652D77696474683D22302E31
                    22207374726F6B653D2223303030222066696C6C3D2223464646223E0D0A2020
                    203C706174682069643D22706174683636342220643D226D2D37302E3731342C
                    322E323232732D372E3936322D342E3132312D32362E3734372D332E37333663
                    302C302C392E3234382D302E3630342C32372E3430392C342E3635342C392E39
                    36362C322E3838352D302E3636322D302E3931382D302E3636322D302E393138
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D22673636362220737472
                    6F6B652D77696474683D22302E3122207374726F6B653D222330303022206669
                    6C6C3D2223464646223E0D0A2020203C706174682069643D2270617468363638
                    2220643D226D2D36392E3434342C322E343435732D362E3832342D342E333037
                    2D32332E3639382D352E34303563302C302C382E3333392C302E31372C32342E
                    32322C362E3237392C382E3731362C332E3335332D302E3532322D302E383734
                    2D302E3532322D302E3837347A222F3E0D0A20203C2F673E0D0A20203C672069
                    643D226736373022207374726F6B652D77696474683D22302E3122207374726F
                    6B653D2223303030222066696C6C3D2223464646223E0D0A2020203C70617468
                    2069643D22706174683637322220643D226D34352E38342C31322E393631732D
                    302E39332C302E3634342D302E3731362D302E35333763302E3231352D312E31
                    38312C32382E3432332D31342E3335312C33322E3033372D31342E3130312C30
                    2C302D33302E3234382C31332E3230362D33312E3332312C31342E3633387A22
                    2F3E0D0A20203C2F673E0D0A20203C672069643D226736373422207374726F6B
                    652D77696474683D22302E3122207374726F6B653D2223303030222066696C6C
                    3D2223464646223E0D0A2020203C706174682069643D22706174683637362220
                    643D226D34322E3434362C31332E36732D302E3837362C302E3731352D302E37
                    35352D302E3437392C32372E3230382D31362E3533392C33302E38332D31362E
                    35373363302C302D32392E3131372C31352E3534312D33302E3037352C31372E
                    3035327A222F3E0D0A20203C2F673E0D0A20203C672069643D22673637382220
                    7374726F6B652D77696474683D22302E3122207374726F6B653D222330303022
                    2066696C6C3D2223464646223E0D0A2020203C706174682069643D2270617468
                    3638302220643D226D33392E31362C31342E393735732D302E3832382C302E37
                    37322D302E3738362D302E34323863302E3034322D312E3139392C31392E3835
                    392D31362E3639362C32392E3637312D31382E35372C302C302D31382E30332C
                    382E3132372D32382E3838352C31382E3939387A222F3E0D0A20203C2F673E0D
                    0A20203C672069643D226736383222207374726F6B652D77696474683D22302E
                    3122207374726F6B653D2223303030222066696C6C3D2223464646223E0D0A20
                    20203C706174682069643D22706174683638342220643D226D33362E3238342C
                    31362E383338732D302E3734352C302E3639342D302E3730372D302E33383563
                    302E3033382D312E30382C31372E3837322D31352E3032372C32362E3730332D
                    31362E3731332C302C302D31362E3232362C372E3331342D32352E3939362C31
                    372E3039387A222F3E0D0A20203C2F673E0D0A20203C672069643D2267363836
                    222066696C6C3D2223434343223E0D0A2020203C706174682069643D22706174
                    683638382220643D226D342E362C3136342E38732D31352E322D322E342C312E
                    362D3463302C302C31382D322C32322D372E322C302C302C31332E362D392E32
                    2C31362E342D392E367331392E322D342C31392E362D362E342C362E342D342E
                    382C382D342C312E362C31302D312E322C31302E382D32312E362C382D32382C
                    392E322D31382C382E382D32322E382C31302D31352E362C312E322D31352E36
                    2C312E327A222F3E0D0A20203C2F673E0D0A20203C672069643D226736393022
                    2066696C6C3D2223303030223E0D0A2020203C706174682069643D2270617468
                    3639322220643D226D37372E362C3132372E34732D332C312E362D342E322C34
                    2E3263302C302D362E342C31302E362D32302E362C31332E382C302C302D3233
                    2C392D33302E382C31312C302C302D31332E342C352D32302E382C342E322C30
                    2C302D372C302E322D302E382C312E382C302C302C32302E322D322C32332E36
                    2D332E382C302C302C31352E362D352E322C31382E362D372E387332312E322D
                    372E362C32332E342D392E362C31322D31302E342C31312E362D31332E387A22
                    2F3E0D0A20203C2F673E0D0A20203C672069643D2267363934222066696C6C3D
                    2223303030223E0D0A2020203C706174682069643D2270617468363936222064
                    3D226D31382E3838322C3135382E393173352E3232392D302E32332C342E3037
                    362C312E33322D332E3630312C302E36382D332E3630312C302E36386C2D302E
                    3437352D327A222F3E0D0A20203C2F673E0D0A20203C672069643D2267363938
                    222066696C6C3D2223303030223E0D0A2020203C706174682069643D22706174
                    683730302220643D226D31312E36382C3136302E323673352E3232382D302E32
                    322C342E3037362C312E3333632D312E3135332C312E35352D332E3630312C30
                    2E36372D332E3630312C302E36376C2D302E3437352D327A222F3E0D0A20203C
                    2F673E0D0A20203C672069643D2267373032222066696C6C3D2223303030223E
                    0D0A2020203C706174682069643D22706174683730342220643D226D312E3235
                    312C3136312E353173352E3232392D302E32332C342E3037362C312E33322D33
                    2E3630312C302E36382D332E3630312C302E36386C2D302E3437352D327A222F
                    3E0D0A20203C2F673E0D0A20203C672069643D2267373036222066696C6C3D22
                    23303030223E0D0A2020203C706174682069643D22706174683730382220643D
                    226D2D362E3338332C3136322E303673352E3232392D302E32332C342E303736
                    2C312E33322D332E3630312C302E36372D332E3630312C302E36376C2D302E34
                    37352D312E39397A222F3E0D0A20203C2F673E0D0A20203C672069643D226737
                    3130222066696C6C3D2223303030223E0D0A2020203C706174682069643D2270
                    6174683731322220643D226D33352E3431352C3135312E353173362E39362D30
                    2E332C352E3432352C312E3736632D312E3533342C322E30372D342E3739332C
                    302E392D342E3739332C302E396C2D302E3633322D322E36367A222F3E0D0A20
                    203C2F673E0D0A20203C672069643D2267373134222066696C6C3D2223303030
                    223E0D0A2020203C706174682069643D22706174683731362220643D226D3435
                    2E37332C3134372E303973352E3935392D332E332C352E3432352C312E373663
                    2D302E32372C322E35352D342E3739332C302E392D342E3739332C302E396C2D
                    302E3633322D322E36367A222F3E0D0A20203C2F673E0D0A20203C672069643D
                    2267373138222066696C6C3D2223303030223E0D0A2020203C70617468206964
                    3D22706174683732302220643D226D35342E3836322C3134342E323773372E31
                    35392D332E372C352E3432352C312E3737632D302E3737382C322E34342D342E
                    3739342C302E392D342E3739342C302E396C2D302E3633312D322E36377A222F
                    3E0D0A20203C2F673E0D0A20203C672069643D2267373232222066696C6C3D22
                    23303030223E0D0A2020203C706174682069643D22706174683732342220643D
                    226D36342E3337362C3133392E343573342E3335392D342E392C352E3432352C
                    312E373663302E3430362C322E35342D342E3739332C302E392D342E3739332C
                    302E396C2D302E3633322D322E36367A222F3E0D0A20203C2F673E0D0A20203C
                    672069643D2267373236222066696C6C3D2223303030223E0D0A2020203C7061
                    74682069643D22706174683732382220643D226D32362E3833342C3135367335
                    2E3232382D302E32332C342E3037362C312E3332632D312E3135332C312E3535
                    2D332E3630322C302E36382D332E3630322C302E36386C2D302E3437342D327A
                    222F3E0D0A20203C2F673E0D0A20203C672069643D226737333022207374726F
                    6B652D77696474683D22302E3122207374726F6B653D2223303030222066696C
                    6C3D2223464646223E0D0A2020203C706174682069643D227061746837333222
                    20643D226D36322E3433342C33342E363033732D302E3732362C302E3636352D
                    302E3732372D302E34303663302D312E30372C31372E3438342D31342E333334
                    2C32362E3332372D31352E3731382C302C302D31362E3039392C362E3732392D
                    32352E362C31362E3132347A222F3E0D0A20203C2F673E0D0A20203C67206964
                    3D2267373334222066696C6C3D2223303030223E0D0A2020203C706174682069
                    643D22706174683733362220643D226D36352E342C39382E347332322E303031
                    2C32322E342C33312E3230312C323663302C302C392E3139392C31312E322C35
                    2E3139392C33372E322C302C302D332E3139392C372E362D362E3339392D3133
                    2E322C302C302C332E322D32352E322D382D392E322C302C302D382E3430312D
                    392E392D322E3030312D392E362C302C302C332E3230312C322C332E3630312C
                    302E34732D372E3630312D31352E322D32342E3830312D32392E362C312E322D
                    322C312E322D327A222F3E0D0A20203C2F673E0D0A20203C672069643D226737
                    333822207374726F6B652D77696474683D22302E3122207374726F6B653D2223
                    303030222066696C6C3D2223464646223E0D0A2020203C706174682069643D22
                    706174683734302220643D226D372C3133372E32732D302E322D312E382C312E
                    362D312C39362C372C3132372E362C333163302C302D34352E3139392D32332E
                    322D3132392E322D33307A222F3E0D0A20203C2F673E0D0A20203C672069643D
                    226737343222207374726F6B652D77696474683D22302E3122207374726F6B65
                    3D2223303030222066696C6C3D2223464646223E0D0A2020203C706174682069
                    643D22706174683734342220643D226D31372E342C3133322E38732D302E322D
                    312E382C312E362D312C3133382E342D302E322C3136322C33322E3263302C30
                    2D32322D32352E322D3136332E362D33312E327A222F3E0D0A20203C2F673E0D
                    0A20203C672069643D226737343622207374726F6B652D77696474683D22302E
                    3122207374726F6B653D2223303030222066696C6C3D2223464646223E0D0A20
                    20203C706174682069643D22706174683734382220643D226D32392C3132382E
                    38732D302E322D312E382C312E362D312C3137352E322D31322E322C3139382E
                    382C32302E3263302C302D392E362D32352E362D3230302E342D31392E327A22
                    2F3E0D0A20203C2F673E0D0A20203C672069643D226737353022207374726F6B
                    652D77696474683D22302E3122207374726F6B653D2223303030222066696C6C
                    3D2223464646223E0D0A2020203C706174682069643D22706174683735322220
                    643D226D33392C313234732D302E322D312E382C312E362D312C3132342D3337
                    2E382C3134372E362D352E3463302C302D31332E342D32342E362D3134392E32
                    2C362E347A222F3E0D0A20203C2F673E0D0A20203C672069643D226737353422
                    207374726F6B652D77696474683D22302E3122207374726F6B653D2223303030
                    222066696C6C3D2223464646223E0D0A2020203C706174682069643D22706174
                    683735362220643D226D2D31392C3134362E38732D302E322D312E382C312E36
                    2D312C31392E362C332C32312E362C34312E3863302C302D372E322D34322D32
                    332E322D34302E387A222F3E0D0A20203C2F673E0D0A20203C672069643D2267
                    37353822207374726F6B652D77696474683D22302E3122207374726F6B653D22
                    23303030222066696C6C3D2223464646223E0D0A2020203C706174682069643D
                    22706174683736302220643D226D2D32372E382C3134382E34732D302E322D31
                    2E382C312E362D312C31362D332E382C31332E322C333563302C302C312E322D
                    33352E322D31342E382D33347A222F3E0D0A20203C2F673E0D0A20203C672069
                    643D226737363222207374726F6B652D77696474683D22302E3122207374726F
                    6B653D2223303030222066696C6C3D2223464646223E0D0A2020203C70617468
                    2069643D22706174683736342220643D226D2D33352E382C3134382E38732D30
                    2E322D312E382C312E362D312C31372E322C312E342C342E382C32332E386330
                    2C302C392E362D32342D362E342D32322E387A222F3E0D0A20203C2F673E0D0A
                    20203C672069643D226737363622207374726F6B652D77696474683D22302E31
                    22207374726F6B653D2223303030222066696C6C3D2223464646223E0D0A2020
                    203C706174682069643D22706174683736382220643D226D31312E3532362C31
                    30342E3436732D302E3434342C322C312E3130352C302E37396331362E303638
                    2D31322E3632382C34382E35312D37312E35332C3130342E322D37372E313634
                    2C302C302D33382E3331322D31322E31312D3130352E332C37362E3337347A22
                    2F3E0D0A20203C2F673E0D0A20203C672069643D226737373022207374726F6B
                    652D77696474683D22302E3122207374726F6B653D2223303030222066696C6C
                    3D2223464646223E0D0A2020203C706174682069643D22706174683737322220
                    643D226D32322E3732362C3130322E3636732D312E3336332D312E31392C302E
                    3530352D312E383163312E3836382D302E36332C3131342E33312D37332E3133
                    2C3135332E362D36352E3136342C302C302D32372E31312D372E35312D313534
                    2E312C36362E3937347A222F3E0D0A20203C2F673E0D0A20203C672069643D22
                    6737373422207374726F6B652D77696474683D22302E3122207374726F6B653D
                    2223303030222066696C6C3D2223464646223E0D0A2020203C70617468206964
                    3D22706174683737362220643D226D312E3838352C3130382E3737732D302E35
                    30392C312E362C312E3230322C302E363263382E3937352D352E31322C31322E
                    35392D36322E3333312C35362E3136372D36332E3538362C302C302D33322E34
                    31312D31342E3731342D35372E3336392C36322E3936367A222F3E0D0A20203C
                    2F673E0D0A20203C672069643D226737373822207374726F6B652D7769647468
                    3D22302E3122207374726F6B653D2223303030222066696C6C3D222346464622
                    3E0D0A2020203C706174682069643D22706174683738302220643D226D2D3138
                    2E3033382C3131392E3739732D312E3037372C312E32392C302E3837362C312E
                    30336331302E3234362D312E33332C33312E3635312D34322E3539382C37362E
                    30392D33372E3531392C302C302D33312E3936362D31342E3334362D37362E39
                    36362C33362E3438397A222F3E0D0A20203C2F673E0D0A20203C672069643D22
                    6737383222207374726F6B652D77696474683D22302E3122207374726F6B653D
                    2223303030222066696C6C3D2223464646223E0D0A2020203C70617468206964
                    3D22706174683738342220643D226D2D362E382C3131332E3637732D302E3831
                    312C312E34372C312E3035382C302E383463392E3739392D332E32372C32322E
                    3838332D34372E3838352C36372E3437312D35312E3433322C302C302D33342E
                    3132362D372E3934332D36382E3532392C35302E3539327A222F3E0D0A20203C
                    2F673E0D0A20203C672069643D226737383622207374726F6B652D7769647468
                    3D22302E3122207374726F6B653D2223303030222066696C6C3D222346464622
                    3E0D0A2020203C706174682069643D22706174683738382220643D226D2D3235
                    2E3037382C3132342E3931732D302E3837332C312E30342C302E3730392C302E
                    383463382E3239392D312E30382C32352E3633372D33342E35312C36312E3633
                    332D33302E3339362C302C302D32352E3839332D31312E36322D36322E333432
                    2C32392E3535367A222F3E0D0A20203C2F673E0D0A20203C672069643D226737
                    393022207374726F6B652D77696474683D22302E3122207374726F6B653D2223
                    303030222066696C6C3D2223464646223E0D0A2020203C706174682069643D22
                    706174683739322220643D226D2D33322E3637372C3133302E3832732D312E30
                    30352C312E30352C302E3538362C302E393363342E3136382D302E33312C3334
                    2E3830362D33332E33392C35332E3237342D31372E38392C302C302D31322E30
                    31352D31382E3732312D35332E38362C31362E39367A222F3E0D0A20203C2F67
                    3E0D0A20203C672069643D226737393422207374726F6B652D77696474683D22
                    302E3122207374726F6B653D2223303030222066696C6C3D2223464646223E0D
                    0A2020203C706174682069643D22706174683739362220643D226D33362E3835
                    352C39382E383938732D312E3230312D312E3335352C302E3733312D312E3734
                    63312E3933322D302E3338342C3132322E36332D35382E3039372C3136302E35
                    392D34352E3233312C302C302D32352E39342D31302E3837342D3136312E3332
                    2C34362E3937317A222F3E0D0A20203C2F673E0D0A20203C672069643D226737
                    393822207374726F6B652D77696474683D22302E3122207374726F6B653D2223
                    303030222066696C6C3D2223464646223E0D0A2020203C706174682069643D22
                    706174683830302220643D226D332E342C3136332E32732D302E322D312E382C
                    312E362D312C31372E322C312E342C342E382C32332E3863302C302C392E362D
                    32342D362E342D32322E387A222F3E0D0A20203C2F673E0D0A20203C67206964
                    3D226738303222207374726F6B652D77696474683D22302E3122207374726F6B
                    653D2223303030222066696C6C3D2223464646223E0D0A2020203C7061746820
                    69643D22706174683830342220643D226D31332E382C3136312E36732D302E32
                    2D312E382C312E362D312C31392E362C332C32312E362C34312E3863302C302D
                    372E322D34322D32332E322D34302E387A222F3E0D0A20203C2F673E0D0A2020
                    3C672069643D226738303622207374726F6B652D77696474683D22302E312220
                    7374726F6B653D2223303030222066696C6C3D2223464646223E0D0A2020203C
                    706174682069643D22706174683830382220643D226D32302E362C313630732D
                    302E322D312E382C312E362D312C32362E342C342E322C35302C33362E366330
                    2C302D33352E362D33362E382D35312E362D33352E367A222F3E0D0A20203C2F
                    673E0D0A20203C672069643D226738313022207374726F6B652D77696474683D
                    22302E3122207374726F6B653D2223303030222066696C6C3D2223464646223E
                    0D0A2020203C706174682069643D22706174683831322220643D226D32382E32
                    32352C3135372E3937732D302E3433372D312E37362C312E3435332D312E3263
                    312E38392C302E35352C32322E3332342D312E33352C36302E3432312C33322E
                    38332C302C302D34362E3137352D33342E39342D36312E3837342D33312E3633
                    7A222F3E0D0A20203C2F673E0D0A20203C672069643D22673831342220737472
                    6F6B652D77696474683D22302E3122207374726F6B653D222330303022206669
                    6C6C3D2223464646223E0D0A2020203C706174682069643D2270617468383136
                    2220643D226D33382E3632352C3135332E3537732D302E3433372D312E37362C
                    312E3435332D312E3263312E38392C302E35352C33362E3732342C352E30352C
                    38382E3432322C34302E30332C302C302D37342E3137362D34322E31342D3839
                    2E3837352D33382E38337A222F3E0D0A20203C2F673E0D0A20203C672069643D
                    226738313822207374726F6B652D77696474683D22302E3122207374726F6B65
                    3D2223303030222066696C6C3D2223464646223E0D0A2020203C706174682069
                    643D22706174683832302220643D226D2D312E382C313432732D302E322D312E
                    382C312E362D312C35352E322C332E342C38352E362C33302E3263302C302D33
                    342E3930312D32342E37372D38372E322D32392E327A222F3E0D0A20203C2F67
                    3E0D0A20203C672069643D226738323222207374726F6B652D77696474683D22
                    302E3122207374726F6B653D2223303030222066696C6C3D2223464646223E0D
                    0A2020203C706174682069643D22706174683832342220643D226D2D31312E38
                    2C313436732D302E322D312E382C312E362D312C32362E342C342E322C35302C
                    33362E3663302C302D33352E362D33362E382D35312E362D33352E367A222F3E
                    0D0A20203C2F673E0D0A20203C672069643D226738323622207374726F6B652D
                    77696474683D22302E3122207374726F6B653D2223303030222066696C6C3D22
                    23464646223E0D0A2020203C706174682069643D22706174683832382220643D
                    226D34392E3530332C3134382E3936732D302E3536352D312E37322C312E3336
                    312D312E3363312E3932362C302E34312C33362E3939362C322E33342C39312E
                    3131362C33332E34342C302C302D37372E3636332D33342E342D39322E343737
                    2D33322E31347A222F3E0D0A20203C2F673E0D0A20203C672069643D22673833
                    3022207374726F6B652D77696474683D22302E3122207374726F6B653D222330
                    3030222066696C6C3D2223464646223E0D0A2020203C706174682069643D2270
                    6174683833322220643D226D35372E3930332C3134362E3536732D302E353635
                    2D312E37322C312E3336312D312E3363312E3932362C302E34312C33362E3939
                    362C322E33342C39312E3131362C33332E34342C302C302D37372E3036332D33
                    342E382D39322E3437372D33322E31347A222F3E0D0A20203C2F673E0D0A2020
                    3C672069643D226738333422207374726F6B652D77696474683D22302E312220
                    7374726F6B653D2223303030222066696C6C3D2223464646223E0D0A2020203C
                    706174682069643D22706174683833362220643D226D36372E3530332C313431
                    2E3536732D302E3536352D312E37322C312E3336312D312E3363312E3932362C
                    302E34312C34342E3939362C342E37342C3133342E37322C33392E30342C302C
                    302D3132302E36362D34302E342D3133362E30382D33372E37347A222F3E0D0A
                    20203C2F673E0D0A20203C672069643D2267383338222066696C6C3D22233030
                    30223E0D0A2020203C706174682069643D22706174683834302220643D226D2D
                    34332E382C3134382E3473352E322D302E342C342C312E322D332E362C302E38
                    2D332E362C302E386C2D302E342D327A222F3E0D0A20203C2F673E0D0A20203C
                    672069643D2267383432222066696C6C3D2223303030223E0D0A2020203C7061
                    74682069643D22706174683834342220643D226D2D31332C3136322E3473352E
                    322D302E342C342C312E322D332E362C302E382D332E362C302E386C2D302E34
                    2D327A222F3E0D0A20203C2F673E0D0A20203C672069643D2267383436222066
                    696C6C3D2223303030223E0D0A2020203C706174682069643D22706174683834
                    382220643D226D2D32312E382C31363273352E322D302E342C342C312E322D33
                    2E362C302E382D332E362C302E386C2D302E342D327A222F3E0D0A20203C2F67
                    3E0D0A20203C672069643D2267383530222066696C6C3D2223303030223E0D0A
                    2020203C706174682069643D22706174683835322220643D226D2D3131372E31
                    372C3135302E313873352E30352C312E33322C332E33392C322E34342D332E36
                    372D302E34322D332E36372D302E34326C302E32382D322E30327A222F3E0D0A
                    20203C2F673E0D0A20203C672069643D2267383534222066696C6C3D22233030
                    30223E0D0A2020203C706174682069643D22706174683835362220643D226D2D
                    3131352E31372C3134302E353873352E30352C312E33322C332E33392C322E34
                    342D332E36372D302E34322D332E36372D302E34326C302E32382D322E30327A
                    222F3E0D0A20203C2F673E0D0A20203C672069643D2267383538222066696C6C
                    3D2223303030223E0D0A2020203C706174682069643D22706174683836302220
                    643D226D2D3132322E33372C3133362E313873352E30352C312E33322C332E33
                    392C322E34342D332E36372D302E34322D332E36372D302E34326C302E32382D
                    322E30327A222F3E0D0A20203C2F673E0D0A20203C672069643D226738363222
                    2066696C6C3D2223434343223E0D0A2020203C706174682069643D2270617468
                    3836342220643D226D2D34322E362C3231312E322D352E362C32632D322C302D
                    31332E322C332E362D31382E382C31332E362C302C302C31322E342D392E362C
                    32342E342D31352E367A222F3E0D0A20203C2F673E0D0A20203C672069643D22
                    67383636222066696C6C3D2223434343223E0D0A2020203C706174682069643D
                    22706174683836382220643D226D34352E3131362C3330332E383563302E3134
                    312C302E32352C302E3139362C302E36372C302E3438382C302E36392C302E36
                    35382C302E30342C312E3839312C302E33342C312E3736362D302E32392D302E
                    3834382D342E33312D312E3732322D392E32352D352E3835352D31312E30352D
                    302E3633392D302E32382D322E3038312C302E31332D322E3135352C312E3032
                    2D302E3132372C312E35322D302E3234342C322E38372C302E3036352C342E33
                    332C302E332C312E34332C322E3435382C312E34332C332E3337352C302E3035
                    2C302E3933362C312E36372C312E3336382C332E35322C322E3331362C352E32
                    357A222F3E0D0A20203C2F673E0D0A20203C672069643D226738373022206669
                    6C6C3D2223434343223E0D0A2020203C706174682069643D2270617468383732
                    2220643D226D33342E3033382C3330382E353863302E3734382C312E34312C30
                    2E3632312C332E32372C322E3033362C332E38342C302E37342C302E32392C32
                    2E35392D302E36382C322E3137322D312E37362D302E3830322D322E30362D31
                    2E31392D342E332D322E3537392D362E31312D302E322D302E32362C302E3034
                    2D302E37392D302E31322D312E31322D302E3539342D312E32322D312E373339
                    2D312E39362D332E3134372D312E36332D312E3131352C322E322C302E303333
                    2C342E33332C312E3535352C362E30342C302E3133362C302E31352D302E3033
                    2C302E35332C302E3038332C302E37347A222F3E0D0A20203C2F673E0D0A2020
                    3C672069643D2267383734222066696C6C3D2223434343223E0D0A2020203C70
                    6174682069643D22706174683837362220643D226D2D352E3536342C3330332E
                    3339632D302E3130382D302E33382D302E3134362D302E38342C302E3031392D
                    312E31362C302E3533312D312E30332C312E3332342D322E31352C302E393837
                    2D332E31382D302E3334382D312E30352D312E3436342D302E38372D322E3131
                    342D302E332D312E3133352C302E39392D312E3138342C322E38322D312E3837
                    352C342E31382D302E3139362C302E33382D302E3134352C302E39362D302E35
                    38362C312E33352D302E3437342C302E34322D302E3931342C312E39342D302E
                    3831382C322E35312C302E3035332C302E33322D302E31332C31302E32322C30
                    2E3039322C392E39362C302E3631392D302E37332C332E3636392D31302E3437
                    2C332E3733382D31312E33362C302E3035372D302E37332C302E3738392D312E
                    31392C302E3535372D327A222F3E0D0A20203C2F673E0D0A20203C672069643D
                    2267383738222066696C6C3D2223434343223E0D0A2020203C70617468206964
                    3D22706174683838302220643D226D2D33312E3230322C3239362E3663322E36
                    33342D322E352C352E3432342D352E34362C342E3938322D392E31372D302E31
                    31362D302E39382D312E3839312D302E34352D322E3037382C302E33392D302E
                    3830322C332E36332D322E3834312C362E32392D352E3430392C382E36382D32
                    2E3139362C322E30352D342E3035382C382E33392D342E3239332C382E392C33
                    2E3639372D352E32362C352E3935342D382C362E3739382D382E387A222F3E0D
                    0A20203C2F673E0D0A20203C672069643D2267383832222066696C6C3D222343
                    4343223E0D0A2020203C706174682069643D22706174683838342220643D226D
                    2D34342E3737362C3239302E363463302E3532332D302E33382C302E3232312D
                    302E38372C302E3433382D312E322C302E3935332D312E34362C322E3235342D
                    322E372C322E3237322D342E34342C302E3030332D302E32382D302E3337352D
                    302E35392D302E37312D302E33362D302E3237372C302E31382D302E3631392C
                    302E33312D302E3732372C302E34342D322E30332C322E34352D332E34332C35
                    2E31322D342E3837332C372E39332D302E3138332C302E33362D312E3332372C
                    342E38352D312E3031342C342E39362C302E3233392C302E30392C312E393539
                    2D342E30392C322E3136392D342E32312C312E3236332D302E36382C312E3237
                    352D322E332C322E3434352D332E31327A222F3E0D0A20203C2F673E0D0A2020
                    3C672069643D2267383836222066696C6C3D2223434343223E0D0A2020203C70
                    6174682069643D22706174683838382220643D226D2D32382E3034332C333130
                    2E313863302E3434342D302E38372C322E30322D322E30372C312E3930372D32
                    2E39362D302E3131382D302E39332C302E33352D322E33372D302E3536322D31
                    2E36382D312E3235372C302E39342D342E3730362C322E32392D342E3937362C
                    382E312D302E3032362C302E35372C322E3934382D322E31322C332E3633312D
                    332E34367A222F3E0D0A20203C2F673E0D0A20203C672069643D226738393022
                    2066696C6C3D2223434343223E0D0A2020203C706174682069643D2270617468
                    3839322220643D226D2D31332E362C32393363302E342D302E36372C312E3130
                    382D302E31392C312E3536372D302E34362C302E3634382D302E33372C312E32
                    35392D302E39332C312E3535312D312E35382C302E39372D322E31342C322E37
                    33392D332E39362C322E3838322D362E33362D312E3439312D312E342D322E31
                    372C302E36342D322E382C312E362D312E3332332D312E36352D322E3332322C
                    302E32332D332E3632322C302E37352D302E30372C302E30332D302E3238332D
                    302E33322D302E3335382D302E32392D312E3137372C302E34342D312E383537
                    2C312E35322D322E3835352C322E332D302E3137312C302E31332D302E353736
                    2D302E30352D302E3732332C302E30392D302E3635322C302E362D312E363235
                    2C302E39332D312E3930352C312E36312D312E31312C322E372D342E32352C34
                    2E382D362E3133372C31322E33342C302E3338312C302E39312C342E3531322D
                    362E36342C342E3939392D372E33342C302E3833362D312E322C302E3935342C
                    312E36362C322E32332C312C302E3035312D302E30332C302E3233372C302E32
                    312C302E3337312C302E33342C302E3139342D302E32382C302E3431322D302E
                    35312C302E382D302E342C302D302E342D302E3133342D302E39362C302E3036
                    372D312E31312C312E3233372D302E39382C312E3135332D322E30352C312E39
                    33332D332E32392C302E3435382C302E37392C312E3531392C302E30372C322C
                    302E387A222F3E0D0A20203C2F673E0D0A20203C672069643D22673839342220
                    66696C6C3D2223434343223E0D0A2020203C706174682069643D227061746838
                    39362220643D226D34362E322C3334372E3473372E342D32302E342C332D3331
                    2E3663302C302C31312E342C32312E362C362E382C33322E382C302C302D302E
                    342D31302E342D342E342D31352E342C302C302D342C31322E382D352E342C31
                    342E327A222F3E0D0A20203C2F673E0D0A20203C672069643D22673839382220
                    66696C6C3D2223434343223E0D0A2020203C706174682069643D227061746839
                    30302220643D226D33312E342C3334342E3873352E342D382E382D322E362D32
                    372E3263302C302D302E382C32302E342D372E362C33312E342C302C302C3134
                    2E322D32302E322C31302E322D342E327A222F3E0D0A20203C2F673E0D0A2020
                    3C672069643D2267393032222066696C6C3D2223434343223E0D0A2020203C70
                    6174682069643D22706174683930342220643D226D32312E342C3334322E3873
                    2D302E322D32302C302E322D323363302C302D332E382C31362E362D31342C32
                    362E322C302C302C31342E342D31322C31332E382D332E327A222F3E0D0A2020
                    3C2F673E0D0A20203C672069643D2267393036222066696C6C3D222343434322
                    3E0D0A2020203C706174682069643D22706174683930382220643D226D31312E
                    382C3331302E3873362C31332E362D342C333263302C302C362E342D31322E32
                    2C312E362D31392E322C302C302C322E362D332E342C322E342D31322E387A22
                    2F3E0D0A20203C2F673E0D0A20203C672069643D2267393130222066696C6C3D
                    2223434343223E0D0A2020203C706174682069643D2270617468393132222064
                    3D226D2D372E342C3334322E34732D312D31352E362C302E382D31372E386330
                    2C302C302E322D362E342D302E322D372E342C302C302C342D362E322C342E32
                    2C312E322C302C302C312E342C372E382C342E322C31322E342C302C302C332E
                    362C352E342C332E342C31312E382C302C302D31302D33302E322D31322E342D
                    302E327A222F3E0D0A20203C2F673E0D0A20203C672069643D22673931342220
                    66696C6C3D2223434343223E0D0A2020203C706174682069643D227061746839
                    31362220643D226D2D31312C3331342E38732D362E362C31302E382D382E342C
                    32392E3863302C302D312E342D362E322C322E342D32302E362C302C302C342E
                    322D31352E342C362D392E327A222F3E0D0A20203C2F673E0D0A20203C672069
                    643D2267393138222066696C6C3D2223434343223E0D0A2020203C7061746820
                    69643D22706174683932302220643D226D2D33322E382C3333342E3673352D35
                    2E342C362E342D31302E3463302C302C332E362D31352E382D322E382D372E32
                    2C302C302C302E322C382D382C31352E342C302C302C342E382D322E342C342E
                    342C322E327A222F3E0D0A20203C2F673E0D0A20203C672069643D2267393232
                    222066696C6C3D2223434343223E0D0A2020203C706174682069643D22706174
                    683932342220643D226D2D33382E362C3332392E3673332E342D31372E342C34
                    2E322D31382E3263302C302C312E382D332E342D312D302E322C302C302D382E
                    382C31392E322D31322E382C32352E382C302C302C382D392E322C392E362D37
                    2E347A222F3E0D0A20203C2F673E0D0A20203C672069643D2267393236222066
                    696C6C3D2223434343223E0D0A2020203C706174682069643D22706174683932
                    382220643D226D2D34342E342C3331337331312E362D32322E342D31302E322C
                    332E3463302C302C31312D392E382C31302E322D332E347A222F3E0D0A20203C
                    2F673E0D0A20203C672069643D2267393330222066696C6C3D2223434343223E
                    0D0A2020203C706174682069643D22706174683933322220643D226D2D35392E
                    382C3239382E3473342E382D31382E382C372E342D31382E366C312E362C312E
                    36732D362C392E362D352E342C31392E3463302C302D302E362D392E362D332E
                    362D322E347A222F3E0D0A20203C2F673E0D0A20203C672069643D2267393334
                    222066696C6C3D2223434343223E0D0A2020203C706174682069643D22706174
                    683933362220643D226D3237302E352C323837732D31322D31302D31342E352D
                    31332E3563302C302C31332E352C31382E352C31332E352C32352E352C302C30
                    2C322E352D372E352C312D31327A222F3E0D0A20203C2F673E0D0A20203C6720
                    69643D2267393338222066696C6C3D2223434343223E0D0A2020203C70617468
                    2069643D22706174683934302220643D226D3237362C323635732D32312D3135
                    2D32342E352D32322E3563302C302C32362E352C32392E352C32362E352C3334
                    2C302C302C302E352D392D322D31312E357A222F3E0D0A20203C2F673E0D0A20
                    203C672069643D2267393432222066696C6C3D2223434343223E0D0A2020203C
                    706174682069643D22706174683934342220643D226D3239332C313131732D31
                    322D382D31332E352D3663302C302C31302E352C362E352C31332C31352C302C
                    302D312E352D392C302E352D397A222F3E0D0A20203C2F673E0D0A20203C6720
                    69643D2267393436222066696C6C3D2223434343223E0D0A2020203C70617468
                    2069643D22706174683934382220643D226D3330312E352C3139312E352D3137
                    2E352D31327331392C31372C31392E352C32316C2D322D397A222F3E0D0A2020
                    3C2F673E0D0A20203C672069643D226739353022207374726F6B653D22233030
                    30223E0D0A2020203C706174682069643D22706174683935322220643D226D2D
                    38392E32352C3136392C32322C342E3735222F3E0D0A20203C2F673E0D0A2020
                    3C672069643D226739353422207374726F6B653D2223303030223E0D0A202020
                    3C706174682069643D22706174683935362220643D226D2D33392C333331732D
                    302E352D332E352D392E352C37222F3E0D0A20203C2F673E0D0A20203C672069
                    643D226739353822207374726F6B653D2223303030223E0D0A2020203C706174
                    682069643D22706174683936302220643D226D2D33332E352C33333673322D36
                    2E352D342E352D32222F3E0D0A20203C2F673E0D0A20203C672069643D226739
                    363222207374726F6B653D2223303030223E0D0A2020203C706174682069643D
                    22706174683936342220643D226D32302E352C3334342E3573312E352D31312D
                    31302C32222F3E0D0A20203C2F673E0D0A203C2F673E0D0A3C2F7376673E}
                  Proportional = True
                  Stretch = True
                end
              end
            end
            object paGalleryLine1ResizeableRight: TPanel
              Left = 317
              Top = 0
              Width = 317
              Height = 250
              Align = alClient
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 1
              object imGalleryLine1StepByStepImage: TWSVGImage
                Left = 0
                Top = 0
                Width = 292
                Height = 250
                Align = alClient
                Center = True
                Picture.Data = {
                  0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                  20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                  223F3E0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
                  6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
                  3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
                  6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
                  2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
                  202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
                  323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
                  332E6F72672F323030302F737667220A20202069643D2273766734313336220A
                  202020786D6C3A73706163653D227072657365727665220A202020656E61626C
                  652D6261636B67726F756E643D226E657720302030203138322E352032333735
                  2E35220A20202076696577426F783D22302030203138322E3520323537220A20
                  20206865696768743D22323537220A20202077696474683D223138322E35220A
                  202020793D2230220A202020783D2230220A20202076657273696F6E3D22312E
                  31223E0A20203C67207472616E73666F726D3D227472616E736C61746528302C
                  2D3429222069643D226734313936223E0A202020203C706174680A2020202020
                  20207374796C653D2266696C6C3A6E6F6E653B7374726F6B653A233238326132
                  633B7374726F6B652D6D697465726C696D69743A3130220A2020202020202073
                  74726F6B652D6D697465726C696D69743D223130220A20202020202020643D22
                  6D2038302E332C3731206320302C3020362E352C2D34202D342E352C2D31322E
                  3220302C30202D33302C2D33322E352031342E352C2D34332E3720302C302032
                  342E352C302E382032352E332C323320302C3020342E382C32312E33202D392E
                  352C333320302C30202D342E352C2D302E35202D372E352C2D322E3220302C30
                  202D362E372C302E33202D332E352C38206C2031332E382C31382E3520632030
                  2C3020372E352C31372E3320302E382C33362E3520302C30202D332E372C3235
                  202D31302E352C33302E3320302C30202D33332E352C35302E38202D33332E31
                  2C35312E3520302E342C302E37202D312E312C31342E31202D33362E312C3231
                  2E38206C202D392E352C342E3520302C332031312C31372E35202D31332E352C
                  30202D352C2D32322E32206320302C30202D312C2D322E3220332C2D3320302C
                  302031312C2D342E322031392C2D313620302C3020392E332C2D31302E322031
                  382C2D372E3720302C3020352C2D353320372E332C2D353420432036322E362C
                  3135362E362035392C3134352E352038322C3131322038322C3131322036332E
                  382C37322E382038302E332C3731205A204D2034362E382C3131332E38203735
                  2E352C3739206320302C3020322E382C2D372E3720372E352C3020302C302032
                  2C392E35202D342E352C392E38204C2035352C313137206C20372E342C33332E
                  31202D382E372C32322E36202D31342E352C3131206320302C30202D332C2D31
                  20342C2D392E3720302C3020342E332C2D322E3520332E382C2D313120302C30
                  202D312E372C2D322E3520392E352C2D31312E3220302C30202D31332C2D3338
                  202D392E372C2D3338207A206D2036322E352C31392E322032342E322C323320
                  6320302C3020342E352C3320302C3135202D342E352C3132202D352E372C2D31
                  30202D352E372C2D313020302C30202D362E382C2D332E37202D362E352C2D31
                  3020302C3020332E342C2D312E38202D31332E332C2D3131206C20312E332C2D
                  37207A206D202D342E312C31382031382E322C35352E34206320302C30203135
                  2C392E392032362C33372E3620302C30202D312E372C31332E322032382C2D36
                  2E3420302C3020382C2D312E31202D31352C32312E39206C202D372E342C3020
                  6320302C30202D33372E382C2D32362E36202D33342E372C2D33392E38204C20
                  38342E372C3138332E38203130352E322C313531205A220A2020202020202069
                  643D22706174683431343222202F3E0A202020203C6C696E650A202020202020
                  207374796C653D2266696C6C3A6E6F6E65220A2020202020202078313D223731
                  2E383030303033220A2020202020202079313D2238342E303939393938220A20
                  20202020202078323D2237342E393030303032220A2020202020202079323D22
                  38302E35220A2020202020202069643D226C696E653431343422202F3E0A2020
                  20203C6C696E650A202020202020207374796C653D2266696C6C3A6E6F6E6522
                  0A2020202020202078313D2237362E303939393938220A202020202020207931
                  3D2239312E35220A2020202020202078323D2237322E353939393938220A2020
                  202020202079323D2239352E353939393938220A2020202020202069643D226C
                  696E653431343622202F3E0A202020203C616E696D6174650A20202020202069
                  643D226672616D6531220A2020202020206174747269627574654E616D653D22
                  646973706C6179220A20202020202076616C7565733D22696E6C696E653B6E6F
                  6E653B6E6F6E653B6E6F6E653B6E6F6E653B6E6F6E653B6E6F6E653B6E6F6E65
                  3B6E6F6E65220A2020202020206B657954696D65733D22303B302E3132353B30
                  2E32353B302E3337353B302E353B302E3632353B302E37353B302E3837353B31
                  220A2020202020206475723D223173220A202020202020626567696E3D223073
                  220A202020202020726570656174436F756E743D22696E646566696E69746522
                  202F3E0A20203C2F673E0A20203C67207472616E73666F726D3D227472616E73
                  6C61746528302C2D3236392E303135323329222069643D226734323031223E0A
                  202020203C706174680A202020202020207374796C653D2266696C6C3A6E6F6E
                  653B7374726F6B653A233238326132633B7374726F6B652D6D697465726C696D
                  69743A3130220A202020202020207374726F6B652D6D697465726C696D69743D
                  223130220A20202020202020643D226D2038362E362C3334392E31206320302C
                  3020352E392C2D352E3320302E322C2D31302E3820302C30202D31372E322C2D
                  31312E35202D382E372C2D32382E3220302C302031382C2D32382E372034322E
                  352C2D342E3220302C302031302E382C32332E31202D362E352C34312E352030
                  2C30202D312C312E39202D31302E372C2D312E3820302C30202D372E352C322E
                  3620322E352C31332E3120302C302032312E332C31362E342031302C34392E39
                  20302C30202D32352C3732202D34352E322C38382E3820302C30202D302E322C
                  39202D35372E322C372E38206C202D372E322C31392E35202D332E382C302030
                  2C2D32392E37206320302C3020382E352C3020392E382C3520302C3020302E38
                  2C332E372031312E382C2D3320302C302032362E352C2D31312E352033362E33
                  2C2D392E3520302C30202D312E372C2D36342E372032392E352C2D39372E3220
                  2D302E312C30202D31372E312C2D33302E39202D332E332C2D34312E32207A20
                  6D202D312E362C392E35206320302C3020332E332C2D31342E3120392E382C2D
                  322E3120302C3020302E332C352E33202D332E322C382E33206C202D372E372C
                  32392E39206320302C30202D352E322C33382E31202D31302E372C34302E3320
                  6C202D31312E372C31352E33206320302C30202D332E372C2D312E3520332E35
                  2C2D32302E3220302C3020362C2D31322E3220392E382C2D31322E35204C2037
                  342E362C3339342E382038352C3335382E36205A206D2032362E382C36312E32
                  2032332E392C35302E34202D372E372C34312E382032312E352C31322E35202D
                  32362E372C31206320302C30202D31302E352C2D3437202D322C2D35352E3220
                  6C202D32332E332C2D31352E382031342E332C2D33342E37207A206D20322E39
                  2C2D372E3620392E342C3234206320302C302031342C31352E3920352E362C32
                  312E32220A2020202020202069643D22706174683431343822202F3E0A202020
                  203C616E696D6174650A20202020202069643D226672616D6532220A20202020
                  20206174747269627574654E616D653D22646973706C6179220A202020202020
                  76616C7565733D226E6F6E653B696E6C696E653B6E6F6E653B6E6F6E653B6E6F
                  6E653B6E6F6E653B6E6F6E653B6E6F6E653B6E6F6E65220A2020202020206B65
                  7954696D65733D22303B302E3132353B302E32353B302E3337353B302E353B30
                  2E3632353B302E37353B302E3837353B31220A2020202020206475723D223173
                  220A202020202020626567696E3D223073220A20202020202072657065617443
                  6F756E743D22696E646566696E69746522202F3E0A20203C2F673E0A20203C67
                  0A20202020207472616E73666F726D3D227472616E736C61746528302C2D3533
                  332E3129220A202020202069643D226734323034223E0A202020203C70617468
                  0A202020202020207374796C653D2266696C6C3A6E6F6E653B7374726F6B653A
                  233238326132633B7374726F6B652D6D697465726C696D69743A3130220A2020
                  20202020207374726F6B652D6D697465726C696D69743D223130220A20202020
                  202020643D226D2038332E362C3736302E38202D31382E332C32302032352E39
                  2C382E38202D33352E322C30206320302C3020362E312C2D31352E352031322E
                  352C2D32382E3920342E342C2D392E3220382E392C2D31372E342031312E352C
                  2D31382E3820302C30202D31382E372C2D33362E37202D31332E352C2D35332E
                  3220302C3020352E352C2D32392E372032302C2D34312E3220302C30202D3132
                  2E372C2D33362E3220322C2D343620302C3020352E352C2D352E32202D332E37
                  2C2D31332E3220302C30202D32332E312C2D31382E3720322E372C2D34302030
                  2C302033302E362C2D31322E322033362E382C313820302C3020302E332C3336
                  202D31302C33352E3320302C30202D31372C2D392E32202D392E352C372E3820
                  302C302032342C31372031352E352C35302E3520302C30202D31372E322C3234
                  20322E352C353420302C3020382C32332E3520382C32352E3520302C3220352E
                  382C31302E38202D31352E352C31352E38206C202D33302E352C342E35206320
                  302C302031332E332C2D382031312E382C2D313520302C302031302E352C2D35
                  2031392C2D3420302C30202D31342C2D31322E37202D31362E352C2D31332E32
                  202D322E352C2D302E35202D322E372C31362E38202D322E372C31362E38220A
                  2020202020202069643D22706174683431353022202F3E0A202020203C706F6C
                  79676F6E0A202020202020207374796C653D2266696C6C3A6E6F6E653B737472
                  6F6B653A233238326132633B7374726F6B652D6D697465726C696D69743A3130
                  220A202020202020207374726F6B652D6D697465726C696D69743D223130220A
                  20202020202020706F696E74733D2236382E352C3735382E382035382E382C37
                  38322E362035362C3736322E3520220A2020202020202069643D22706F6C7967
                  6F6E3431353222202F3E0A202020203C706174680A202020202020207374796C
                  653D2266696C6C3A6E6F6E653B7374726F6B653A233238326132633B7374726F
                  6B652D6D697465726C696D69743A3130220A202020202020207374726F6B652D
                  6D697465726C696D69743D223130220A20202020202020643D226D2039332E37
                  2C3631362E37206320302C3020332E332C2D31312E34202D372E322C2D372E39
                  20302C30202D322E372C342E3120302C372E39206C20302E332C33322E332063
                  20302C30202D342E332C31382E3820332C34332E3520302C30202D31322C3130
                  2E3520322C323820302C3020302E352C372E3320322C342E3520312E352C2D32
                  2E3820322E382C2D31372E3220322E382C2D31372E3220302C30202D322E352C
                  2D31342E32202D312E322C2D31392E3520312E332C2D352E33202D312E372C2D
                  37312E36202D312E372C2D37312E36207A220A2020202020202069643D227061
                  74683431353422202F3E0A202020203C616E696D6174650A2020202020206964
                  3D226672616D6533220A2020202020206174747269627574654E616D653D2264
                  6973706C6179220A20202020202076616C7565733D226E6F6E653B6E6F6E653B
                  696E6C696E653B6E6F6E653B6E6F6E653B6E6F6E653B6E6F6E653B6E6F6E653B
                  6E6F6E65220A2020202020206B657954696D65733D22303B302E3132353B302E
                  32353B302E3337353B302E353B302E3632353B302E37353B302E3837353B3122
                  0A2020202020206475723D223173220A202020202020626567696E3D22307322
                  0A202020202020726570656174436F756E743D22696E646566696E6974652220
                  2F3E0A20203C2F673E0A20203C670A20202020207472616E73666F726D3D2274
                  72616E736C61746528302C2D3739382E313134343329220A202020202069643D
                  226734323039223E0A202020203C706174680A202020202020207374796C653D
                  2266696C6C3A6E6F6E653B7374726F6B653A233238326132633B7374726F6B65
                  2D6D697465726C696D69743A3130220A202020202020207374726F6B652D6D69
                  7465726C696D69743D223130220A20202020202020643D226D2038332E372C38
                  3538206320302C3020372C2D332E3320302C2D31322E3720302C30202D33372C
                  2D32382E3320392C2D343620302C302033332E332C2D312E372032362E372C33
                  312E3320302C30202D302E332C3235202D31312C32352E3320302C30202D3133
                  2C2D332E37202D382E332C372E3320302C302032332E332C31362E372031342C
                  3533206C202D332E342C31312E38206320302C30202D372E372C31382E372031
                  352E372C34322E3320302C302032312E372C32322E3720392E332C32372E3720
                  6C202D34312E372C32342E382032312C31382E36202D33322E372C2D31302063
                  20302C302032362E372C2D34302E332033352C2D3339206C202D33302C2D3230
                  206320302C30202D31322C32362E33202D32302E372C33332E3720302C30202D
                  392E332C32392E33202D372E332C3337206C2032302C31302E35202D32392E33
                  2C31206320302C30202D372E372C2D34322E3220352E332C2D35352E3220302C
                  3020322E392C2D37302E382032382C2D39382E3720302C30202D352E312C2D31
                  332E37202D352E392C2D32352E37202D302E352C2D372E3820302E382C2D3134
                  2E3820362E332C2D3137207A204D2037362C3837362E332036332E332C393034
                  2E36206320302C30202D342E312C3420382E342C31342E37206C20322E392C2D
                  36206320302C30202D342C2D322E37202D352E332C2D3420432036382C393038
                  2038302E312C3839302E312038302E312C3839302E31206D20302C2D31352E38
                  206320302C30202D352E312C2D332E3320322E322C2D3820302C3020352E332C
                  2D302E3820352E332C352E37206C2031382E332C33382E332032322C32332E37
                  206320302C3020382E372C2D352E332031392E372C32312E3320302C30202D33
                  2E372C322E37202D32302E332C2D31322E3320302C30202D32382E372C2D3234
                  202D32392E372C2D3330204C2038302E312C3837342E33205A220A2020202020
                  202069643D22706174683431353622202F3E0A202020203C616E696D6174650A
                  20202020202069643D226672616D6534220A2020202020206174747269627574
                  654E616D653D22646973706C6179220A20202020202076616C7565733D226E6F
                  6E653B6E6F6E653B6E6F6E653B696E6C696E653B6E6F6E653B6E6F6E653B6E6F
                  6E653B6E6F6E653B6E6F6E65220A2020202020206B657954696D65733D22303B
                  302E3132353B302E32353B302E3337353B302E353B302E3632353B302E37353B
                  302E3837353B31220A2020202020206475723D223173220A2020202020206265
                  67696E3D223073220A202020202020726570656174436F756E743D22696E6465
                  66696E69746522202F3E0A20203C2F673E0A20203C670A20202020207472616E
                  73666F726D3D227472616E736C61746528302C2D3130363229220A2020202020
                  69643D226734323132223E0A202020203C706174680A20202020202020737479
                  6C653D2266696C6C3A6E6F6E653B7374726F6B653A233238326132633B737472
                  6F6B652D6D697465726C696D69743A3130220A202020202020207374726F6B65
                  2D6D697465726C696D69743D223130220A20202020202020643D226D20313034
                  2E342C313139312E31206320302C30202D362E392C32392E332032312E352C39
                  32206C2031312E342C32372E332033312E342C2D3131202D32312E332C31392E
                  31202D382E342C30206320302C30202D33322C2D33332E35202D33322E372C2D
                  34362E38204C2037382C31323435206320302C30202D31312E392C3239202D31
                  342E322C3331206C202D34322E322C323520302C31312031342E322C362E3520
                  2D32302E382C30206320302C30202D31322E322C2D32312E39202D352E392C2D
                  32322E39206C2031322E332C2D302E33206320302C302031352E372C2D32352E
                  372032392E332C2D32392E3720302C30202D352E372C2D35302E372032342E37
                  2C2D38392E3320302C30202D332E372C2D31352E38202D342E312C2D32392E37
                  202D302E332C2D392E3420302E392C2D31372E3820352E382C2D323020302C30
                  20372C2D352E33202D312E332C2D31322E3720302C30202D32322E362C2D3134
                  2E3320322C2D33372E3720302C302033352E332C2D31362033372E332C323220
                  302C30202D312E332C32372E37202D31322E372C32362E3720302C30202D3134
                  2E332C2D38202D372E372C3720302C302032332E372C31362E372031322C3531
                  2E33206D202D33372C2D33372E32202D31352E372C33312E37202D312E332C32
                  38206320302C30202D31302E332C342E37202D31362C32322E3720302C30202D
                  312C362E332031302E332C2D332E33206C20392E362C2D31322E3420352C2D33
                  332E332031302E372C2D31392E37206D20322E342C2D31372E37206320302C30
                  202D352E372C2D352E3320322E332C2D382E3720302C3020342E322C302E3320
                  332E312C342E33206C2031362E352C33352032392E332C3239206320302C3020
                  31362E372C2D302E372031332E372C32352E3320302C30202D342C322E33202D
                  342C2D322E3720302C2D35202D31332C2D362E37202D31302E332C2D31362E37
                  206C202D33352E372C2D3330202D31342E392C2D33352E35207A220A20202020
                  20202069643D22706174683431353822202F3E0A202020203C616E696D617465
                  0A20202020202069643D226672616D6535220A20202020202061747472696275
                  74654E616D653D22646973706C6179220A20202020202076616C7565733D226E
                  6F6E653B6E6F6E653B6E6F6E653B6E6F6E653B696E6C696E653B6E6F6E653B6E
                  6F6E653B6E6F6E653B6E6F6E65220A2020202020206B657954696D65733D2230
                  3B302E3132353B302E32353B302E3337353B302E353B302E3632353B302E3735
                  3B302E3837353B31220A2020202020206475723D223173220A20202020202062
                  6567696E3D223073220A202020202020726570656174436F756E743D22696E64
                  6566696E69746522202F3E0A20203C2F673E0A20203C670A2020202020747261
                  6E73666F726D3D227472616E736C61746528302C2D313332362E323432322922
                  0A202020202069643D226734323135223E0A202020203C706174680A20202020
                  2020207374796C653D2266696C6C3A6E6F6E653B7374726F6B653A2332383261
                  32633B7374726F6B652D6D697465726C696D69743A3130220A20202020202020
                  7374726F6B652D6D697465726C696D69743D223130220A20202020202020643D
                  226D203131332E312C313438352E332032322E332C3433202D382E342C34302E
                  372032312E332C31332E35202D32362E332C30206320302C30202D31312E352C
                  2D34312E36202D332C2D35352E35206C202D32342E332C2D3135202D32312C33
                  352E37206320302C3020302E332C3138202D36322C31352E33206C202D372E34
                  2C3138202D332E372C312E3520302C2D32392E352031322E352C332E33206320
                  302C302033332E372C2D31352E332034352E372C2D313220302C3020312C2D36
                  332E332031332C2D37302E37206C2031362C2D32352E33206320302C30202D31
                  382C2D33332E33202D312C2D34342E3720302C3020312E312C2D342E37202D33
                  2C2D392E3420302C30202D32392C2D32372031322C2D34322E3320302C302033
                  332E372C312E332032372E332C323620302C30202D312E372C3239202D31332E
                  372C323620302C30202D31312E332C2D352E37202D372E372C372E3720302C30
                  2032372E332C32312031322E332C36302E33206D202D33342E342C2D34322E32
                  202D372C32322E3520312E382C31372E3120372E332C2D31312E36206320302C
                  30202D302E322C2D31312E3120332C2D31352E36206C202D352E312C2D31322E
                  34207A206D20352E322C2D36206320302C30202D392E312C2D3720332E362C2D
                  31312E3320302C302031302E332C312E3320362E332C31312E3320302C302031
                  332E372C35332E372033312E372C37302E3720302C302031312E352C32302E38
                  20312E362C31392E35206C202D31322E352C2D32342E31206320302C30202D32
                  302E372C2D32362E37202D32312E312C2D33342E39206C202D392E362C2D3331
                  2E32207A220A2020202020202069643D22706174683431363022202F3E0A2020
                  20203C616E696D6174650A20202020202069643D226672616D6536220A202020
                  2020206174747269627574654E616D653D22646973706C6179220A2020202020
                  2076616C7565733D226E6F6E653B6E6F6E653B6E6F6E653B6E6F6E653B6E6F6E
                  653B696E6C696E653B6E6F6E653B6E6F6E653B6E6F6E65220A2020202020206B
                  657954696D65733D22303B302E3132353B302E32353B302E3337353B302E353B
                  302E3632353B302E37353B302E3837353B31220A2020202020206475723D2231
                  73220A202020202020626567696E3D223073220A202020202020726570656174
                  436F756E743D22696E646566696E69746522202F3E0A20203C2F673E0A20203C
                  670A20202020207472616E73666F726D3D227472616E736C61746528302C2D31
                  35393129220A202020202069643D226734323138223E0A202020203C70617468
                  0A202020202020207374796C653D2266696C6C3A6E6F6E653B7374726F6B653A
                  233238326132633B7374726F6B652D6D697465726C696D69743A3130220A2020
                  20202020207374726F6B652D6D697465726C696D69743D223130220A20202020
                  202020643D226D2037382E332C31363630206320302C3020372C2D352E37202D
                  312E372C2D313520302C30202D32372C2D32332E3320342E332C2D34312E3320
                  302C302033302E332C2D362E372033342C31372E3320302C3020302E372C3335
                  202D31302E372C33372E3320302C30202D31352E372C2D37202D392E332C3720
                  302C302033302C31392E3320392E372C37312E33206C202D31332C31392E3920
                  6320302C3020322C35302E35202D31352C35392E38206C202D32302E372C3232
                  2032342E372C392E32202D33312E362C30206320302C302031332E372C2D3433
                  2E382032342C2D34392E3520302C30202D33382E332C2D343420362C2D39362E
                  3320302C30202D31342C2D32372E37202D302E372C2D34312E37207A206D202D
                  302E362C31332E33206320302C30202D372C2D352E3720322E372C2D382E3720
                  302C3020382C312E3720332E332C382E3720302C3020372E332C35382E372031
                  2E352C37392E3720302C3020302E392C32312E33202D332E312C32382E332030
                  2C30202D322E392C33202D332E382C2D372E3720302C30202D372E322C2D3131
                  2E3320312E342C2D32312E3320302C30202D362E332C2D33322E35202D302E37
                  2C2D34322E39206C202D312E332C2D33362E31207A206D2032362C36342E3820
                  31382C35342E35206320302C3020342C3133202D31302E372C31382E37206C20
                  2D33352E332C362031312E372C2D31372E392031382E362C2D322E31202D3135
                  2E382C2D31312E36220A2020202020202069643D22706174683431363222202F
                  3E0A202020203C706F6C796C696E650A202020202020207374796C653D226669
                  6C6C3A6E6F6E653B7374726F6B653A233238326132633B7374726F6B652D6D69
                  7465726C696D69743A3130220A202020202020207374726F6B652D6D69746572
                  6C696D69743D223130220A20202020202020706F696E74733D2235392E342031
                  3831392E332034352E3920313832332E332035312E3320313834302E3520220A
                  2020202020202069643D22706F6C796C696E653431363422202F3E0A20202020
                  3C616E696D6174650A20202020202069643D226672616D6537220A2020202020
                  206174747269627574654E616D653D22646973706C6179220A20202020202076
                  616C7565733D226E6F6E653B6E6F6E653B6E6F6E653B6E6F6E653B6E6F6E653B
                  6E6F6E653B696E6C696E653B6E6F6E653B6E6F6E65220A2020202020206B6579
                  54696D65733D22303B302E3132353B302E32353B302E3337353B302E353B302E
                  3632353B302E37353B302E3837353B31220A2020202020206475723D22317322
                  0A202020202020626567696E3D223073220A202020202020726570656174436F
                  756E743D22696E646566696E69746522202F3E0A20203C2F673E0A20203C670A
                  20202020207472616E73666F726D3D227472616E736C61746528302C2D313835
                  3429220A202020202069643D226734323232223E0A202020203C706174680A20
                  2020202020207374796C653D2266696C6C3A6E6F6E653B7374726F6B653A2332
                  38326132633B7374726F6B652D6D697465726C696D69743A3130220A20202020
                  2020207374726F6B652D6D697465726C696D69743D223130220A202020202020
                  20643D226D2037392E322C313931352E33206320302C3020362E332C2D332E38
                  202D332E342C2D31342E3520302C30202D32352E332C2D32392E3720352E332C
                  2D34322E3720302C302034352E362C2D31312E372033322E362C34332E362030
                  2C30202D312E372C31322E33202D31302E372C31332E3320302C30202D31352E
                  332C2D31312E33202D362E332C3920302C302032372E372C31322E3820362E37
                  2C37322E3220302C30202D302E322C302E34202D302E372C312E31202D342E39
                  2C382E33202D33352E332C35392E38202D33392C36312E36206C202D392E372C
                  34312E342032302C31302E32202D32352C30206320302C30202D31332E372C2D
                  33362E3820332E372C2D35352E3820302C30202D312E362C2D38312E33203237
                  2E332C2D39372E3720302C30202D31382E332C2D33302E32202D302E382C2D34
                  312E37207A206D2032352E352C38322E342032342E332C33372E33206320302C
                  302031352E372C3135202D352E332C32342E33206C202D33342C31392E332032
                  302C3138202D32392E372C2D31302E37206320302C302031392E372C2D33382E
                  332033332E312C2D33382E38206C202D32392C2D31382E37204D2037362E332C
                  31393234206320302C3020322E372C2D313020382E332C2D352E3720302C3020
                  382C332E37202D312E372C382E37206C202D31362E372C33392032352E332C32
                  362E3120352E372C32332E35206320302C30202D342E372C322E37202D362E33
                  2C2D36202D312E362C2D382E37202D382E372C2D3130202D372E372C2D31372E
                  3520312C2D372E35202D312E352C30202D312E352C30204C2035372E392C3139
                  3730206320302C30202D332C2D322E3320302C2D372E37204C2037362E332C31
                  393234205A206D2033322E372C35322E34206320302C302032372E372C31372E
                  362033332E372C33352E3620302C3020302E332C352E33202D352C3020302C30
                  202D32322E332C2D392E35202D31372C2D31342E3320322E352C2D322E332030
                  2E332C2D322E3320302E332C2D322E3320302C30202D372E382C2D35202D3133
                  2E372C2D382E32220A2020202020202069643D22706174683431363622202F3E
                  0A202020203C616E696D6174650A20202020202069643D226672616D6538220A
                  2020202020206174747269627574654E616D653D22646973706C6179220A2020
                  2020202076616C7565733D226E6F6E653B6E6F6E653B6E6F6E653B6E6F6E653B
                  6E6F6E653B6E6F6E653B6E6F6E653B696E6C696E653B6E6F6E65220A20202020
                  20206B657954696D65733D22303B302E3132353B302E32353B302E3337353B30
                  2E353B302E3632353B302E37353B302E3837353B31220A202020202020647572
                  3D223173220A202020202020626567696E3D223073220A202020202020726570
                  656174436F756E743D22696E646566696E69746522202F3E0A20203C2F673E0A
                  20203C670A20202020207472616E73666F726D3D227472616E736C6174652830
                  2C2D3231313929220A202020202069643D226734323235223E0A202020203C70
                  6174680A202020202020207374796C653D2266696C6C3A6E6F6E653B7374726F
                  6B653A233238326132633B7374726F6B652D6D697465726C696D69743A313022
                  0A202020202020207374726F6B652D6D697465726C696D69743D223130220A20
                  202020202020643D226D2037342E362C32313835206320302C3020372E392C2D
                  35202D322E352C2D313320302C30202D31382E342C2D3139202D302E312C2D33
                  342E3720302C302032332E332C2D31382E332033382C352E3720302C3020372E
                  372C3133202D312E372C333420302C30202D322E332C372E38202D392E332C35
                  2E3220302C30202D31342C2D352E36202D372C392E3820302C302033302C3137
                  2E3120382C36382E34206C202D33362E312C36382E39206320302C30202D392E
                  382C31332E33202D31382E312C3135206C202D32362E332C313320302C31322E
                  372031342E322C352E35202D32302E372C30206320302C30202D31332E342C2D
                  32312E35202D342E372C2D32302E38206C20382E372C2D312E33206320302C30
                  2031392E332C2D32362E372033312E332C2D333020302C30202D332E322C2D35
                  342032342E332C2D38382E3720302E312C30202D382E342C2D34352E3420322C
                  2D34392E37207A206D2032352E362C37372E372033342E382C3130342E332032
                  392E342C2D31302E33206320302C3020312E342C32202D312E362C35206C202D
                  31362E362C31332E38202D392E322C30206320302C30202D33302C2D33312E35
                  202D33312C2D34332E35206C202D32382E362C2D32382E36206D202D342E372C
                  2D3130362E31206320302C3020332C2D313320392E332C2D3720302C3020352E
                  372C313020302C31322E37206C202D32302C3331202D372E322C33362E372063
                  20302C30202D322E312C392E37202D382E312C31322E37206C202D31312E312C
                  35206320302C3020342E312C2D31362E3720382E312C2D31382E37206C20382C
                  2D3720312C2D32372E332032302C2D33382E31207A206D2033332C34362E3720
                  6320302C302031392C31362E322032332E332C31362E3420302C302031342C38
                  2E3620392E332C32332E3620302C30202D332C372E34202D352C2D322E392030
                  2C30202D31312E382C2D382E32202D31302E392C2D31352E38206C202D31382E
                  322C2D31362E35220A2020202020202069643D22706174683431363822202F3E
                  0A202020203C616E696D6174650A20202020202069643D226672616D6539220A
                  2020202020206174747269627574654E616D653D22646973706C6179220A2020
                  2020202076616C7565733D226E6F6E653B6E6F6E653B6E6F6E653B6E6F6E653B
                  6E6F6E653B6E6F6E653B6E6F6E653B6E6F6E653B696E6C696E65220A20202020
                  20206B657954696D65733D22303B302E3132353B302E32353B302E3337353B30
                  2E353B302E3632353B302E37353B302E3837353B31220A202020202020647572
                  3D223173220A202020202020626567696E3D223073220A202020202020726570
                  656174436F756E743D22696E646566696E69746522202F3E0A20203C2F673E0A
                  3C2F7376673E}
                Proportional = True
                Stretch = True
                Transparent = True
                Animation.FrameCount = 100
                Animation.Position = 0
                Animation.Animate = True
                OnAnimate = imGalleryLine1StepByStepImageAnimate
              end
              object tbGalleryLine1StepByStepImage: TTrackBar
                AlignWithMargins = True
                Left = 292
                Top = 0
                Width = 25
                Height = 250
                Margins.Left = 0
                Margins.Top = 0
                Margins.Right = 0
                Margins.Bottom = 0
                Align = alRight
                Ctl3D = True
                Max = 100
                Orientation = trVertical
                ParentCtl3D = False
                ShowSelRange = False
                TabOrder = 0
                TickMarks = tmTopLeft
                TickStyle = tsNone
                OnChange = tbGalleryLine1StepByStepImageChange
              end
            end
          end
          object paGalleryLine1ResizeableCaptions: TPanel
            AlignWithMargins = True
            Left = 0
            Top = 585
            Width = 634
            Height = 45
            Margins.Left = 0
            Margins.Top = 10
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 5
            object laGalleryStepByStepDesc: TLabel
              AlignWithMargins = True
              Left = 317
              Top = 0
              Width = 317
              Height = 45
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alClient
              Alignment = taCenter
              Caption = 
                'The image animation may also be fully controlled, allowing to cr' +
                'eate for example step by step progress effects'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 1052688
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentColor = False
              ParentFont = False
              WordWrap = True
              ExplicitWidth = 292
              ExplicitHeight = 26
            end
            object paGalleryLine1ResizeableComponentsCaptions: TPanel
              Left = 0
              Top = 0
              Width = 317
              Height = 45
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 0
              object laGalleryResizeableDesc: TLabel
                AlignWithMargins = True
                Left = 0
                Top = 0
                Width = 317
                Height = 45
                Margins.Left = 0
                Margins.Top = 0
                Margins.Right = 0
                Margins.Bottom = 0
                Align = alClient
                Alignment = taCenter
                Caption = 
                  'Of course, any TImage showing a SVG can be resized in real time ' +
                  'without loss of quality.'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                WordWrap = True
                ExplicitWidth = 314
                ExplicitHeight = 26
              end
            end
          end
        end
        object paGalleryLine2: TPanel
          AlignWithMargins = True
          Left = 5
          Top = 660
          Width = 634
          Height = 150
          Margins.Left = 5
          Margins.Top = 30
          Margins.Right = 5
          Margins.Bottom = 0
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 2
          object paGalleryLine2ComponentsTitle: TPanel
            Left = 0
            Top = 25
            Width = 634
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 0
            object paGalleryTButton: TPanel
              Left = 0
              Top = 0
              Width = 145
              Height = 25
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 0
              object laGalleryTButton: TLabel
                Left = 0
                Top = 0
                Width = 145
                Height = 25
                Align = alClient
                Caption = 'TButton'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -15
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                Layout = tlCenter
                ExplicitWidth = 53
                ExplicitHeight = 18
              end
            end
            object paGalleryTBitBtn: TPanel
              Left = 145
              Top = 0
              Width = 145
              Height = 25
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 1
              object laGalleryTBitBtn: TLabel
                Left = 0
                Top = 0
                Width = 145
                Height = 25
                Align = alClient
                Caption = 'TBitBtn (+ TAction)'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -15
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                Layout = tlCenter
                ExplicitWidth = 130
                ExplicitHeight = 18
              end
            end
            object paGalleryTSpeedBtn: TPanel
              AlignWithMargins = True
              Left = 290
              Top = 0
              Width = 175
              Height = 25
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 2
              object laGalleryTSpeedBtn: TLabel
                Left = 0
                Top = 0
                Width = 175
                Height = 25
                Align = alClient
                Caption = 'TSpeedBtn (+ TAction)'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -15
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                Layout = tlCenter
                ExplicitWidth = 154
                ExplicitHeight = 18
              end
            end
            object paGalleryTPopupMenu: TPanel
              AlignWithMargins = True
              Left = 465
              Top = 0
              Width = 175
              Height = 25
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 3
              object laGalleryTPopupMenu: TLabel
                Left = 0
                Top = 0
                Width = 175
                Height = 25
                Align = alClient
                Caption = 'TPopupMenu'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -15
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                Layout = tlCenter
                ExplicitWidth = 86
                ExplicitHeight = 18
              end
            end
          end
          object paGalleryLine2Components: TPanel
            Left = 0
            Top = 50
            Width = 634
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 1
            object btGalleryTSpeedBtn: TSpeedButton
              AlignWithMargins = True
              Left = 290
              Top = 0
              Width = 95
              Height = 25
              Margins.Left = 70
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Action = acDemoTSpeedBtn
              Align = alLeft
            end
            object btGalleryTButton: TButton
              Left = 0
              Top = 0
              Width = 75
              Height = 25
              Align = alLeft
              Caption = 'TButton'
              HotImageIndex = 2
              ImageIndex = 1
              Images = ilImages
              PressedImageIndex = 3
              TabOrder = 0
            end
            object btGalleryTBitBtn: TBitBtn
              AlignWithMargins = True
              Left = 145
              Top = 0
              Width = 75
              Height = 25
              Margins.Left = 70
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Action = acDemoTBitBtn
              Align = alLeft
              Caption = 'TBitBtn'
              TabOrder = 1
            end
            object btGalleryPopupMenuBtn: TButton
              AlignWithMargins = True
              Left = 465
              Top = 0
              Width = 75
              Height = 25
              Margins.Left = 80
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Caption = 'Click it!'
              PopupMenu = pmPopup
              TabOrder = 2
              OnClick = btGalleryPopupMenuBtnClick
            end
          end
          object paGalleryLine2Header: TPanel
            Left = 0
            Top = 0
            Width = 634
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 2
            object laGalleryLine2Title: TLabel
              Left = 0
              Top = 0
              Width = 588
              Height = 25
              Align = alLeft
              Alignment = taCenter
              AutoSize = False
              Caption = 'Image lists'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 1052688
              Font.Height = -20
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentColor = False
              ParentFont = False
              Layout = tlCenter
            end
          end
          object paGalleryLine2ComponentsDesc: TPanel
            AlignWithMargins = True
            Left = 0
            Top = 85
            Width = 634
            Height = 65
            Margins.Left = 0
            Margins.Top = 10
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 3
            object paGalleryTButtonDesc: TPanel
              Left = 0
              Top = 0
              Width = 140
              Height = 65
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 0
              object laGalleryTButtonDesc: TLabel
                Left = 0
                Top = 0
                Width = 140
                Height = 65
                Align = alClient
                Caption = 
                  'You can also use a TWSVGImageList to skin several components, li' +
                  'ke e.g. the TButton component.'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                WordWrap = True
                ExplicitHeight = 52
              end
            end
            object paGalleryTBitBtnDesc: TPanel
              AlignWithMargins = True
              Left = 145
              Top = 0
              Width = 140
              Height = 65
              Margins.Left = 5
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 1
              object laGalleryTBitBtnDesc: TLabel
                Left = 0
                Top = 0
                Width = 140
                Height = 65
                Align = alClient
                Caption = 
                  'A TBitBtn may also be skinned, but indirectly, if you link it wi' +
                  'th a TAction component.'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                WordWrap = True
                ExplicitWidth = 139
                ExplicitHeight = 52
              end
            end
            object paGalleryTSpeedBtnDesc: TPanel
              AlignWithMargins = True
              Left = 290
              Top = 0
              Width = 140
              Height = 65
              Margins.Left = 5
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 2
              object laGalleryTSpeedBtnDesc: TLabel
                Left = 0
                Top = 0
                Width = 140
                Height = 65
                Align = alClient
                Caption = 
                  'It'#39's the same situation for the TSpeedBtn, that can also be skin' +
                  'ned indirectly with an action.'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                WordWrap = True
                ExplicitHeight = 52
              end
            end
            object paGalleryTPopupMenuDesc: TPanel
              AlignWithMargins = True
              Left = 465
              Top = 0
              Width = 140
              Height = 65
              Margins.Left = 35
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 3
              object laGalleryTPopupMenuDesc: TLabel
                Left = 0
                Top = 0
                Width = 140
                Height = 65
                Align = alClient
                Caption = 
                  'The TWSVGImageList component may also be used on the popup menu ' +
                  'items.'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 1052688
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                WordWrap = True
                ExplicitWidth = 139
                ExplicitHeight = 39
              end
            end
          end
        end
      end
    end
    object tsBrowser: TTabSheet
      Caption = 'tsBrowser'
      ImageIndex = 2
      object spMainVert: TSplitter
        Left = 349
        Top = 0
        Height = 794
        Align = alRight
        ResizeStyle = rsUpdate
        OnMoved = spMainVertMoved
      end
      object paExplorer: TPanel
        Left = 352
        Top = 0
        Width = 309
        Height = 794
        Align = alRight
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 0
        object lbDir: TListBox
          AlignWithMargins = True
          Left = 0
          Top = 0
          Width = 309
          Height = 734
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbDirClick
        end
        object paExplorerBrowser: TPanel
          Left = 0
          Top = 734
          Width = 309
          Height = 60
          Align = alBottom
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 1
          object paExplorerBrowserTop: TPanel
            Left = 0
            Top = 0
            Width = 309
            Height = 31
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 0
            object btBrowse: TButton
              AlignWithMargins = True
              Left = 281
              Top = 3
              Width = 25
              Height = 25
              Align = alRight
              Caption = '...'
              TabOrder = 3
              OnClick = btBrowseClick
            end
            object btNext: TButton
              AlignWithMargins = True
              Left = 34
              Top = 3
              Width = 25
              Height = 25
              Align = alLeft
              Caption = '>>'
              TabOrder = 1
              OnClick = btNextClick
            end
            object btPrev: TButton
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 25
              Height = 25
              Align = alLeft
              Caption = '<<'
              TabOrder = 0
              OnClick = btPrevClick
            end
            object btSlideshow: TButton
              AlignWithMargins = True
              Left = 200
              Top = 3
              Width = 75
              Height = 25
              Align = alRight
              Caption = 'Slideshow'
              Images = ilImages
              TabOrder = 2
              OnClick = btSlideshowClick
            end
            object tbSlideshowTimer: TTrackBar
              AlignWithMargins = True
              Left = 129
              Top = 5
              Width = 65
              Height = 23
              Margins.Top = 5
              Align = alRight
              Max = 10000
              Min = 100
              Position = 2000
              SelEnd = 10000
              SelStart = 100
              ShowSelRange = False
              TabOrder = 4
              TickStyle = tsNone
              Visible = False
              OnChange = tbSlideshowTimerChange
            end
          end
          object paExplorerBrowserBottom: TPanel
            Left = 0
            Top = 31
            Width = 309
            Height = 31
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 1
            object btChangeBgColor: TButton
              AlignWithMargins = True
              Left = 129
              Top = 3
              Width = 177
              Height = 25
              Align = alRight
              Caption = 'Change Background Color'
              TabOrder = 0
              OnClick = btChangeBgColorClick
            end
          end
        end
      end
      object paViewer: TPanel
        Left = 0
        Top = 0
        Width = 349
        Height = 794
        Align = alClient
        BevelOuter = bvNone
        Color = clWhite
        DoubleBuffered = True
        ParentBackground = False
        ParentDoubleBuffered = False
        TabOrder = 1
        object imViewer: TWSVGImage
          Left = 0
          Top = 0
          Width = 349
          Height = 734
          Align = alClient
          Transparent = True
          Animation.FrameCount = 0
          Animation.Position = 0
          Animation.Animate = True
        end
        object paViewerControls: TPanel
          Left = 0
          Top = 734
          Width = 349
          Height = 60
          Align = alBottom
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 0
          object paViewerControlsTop: TPanel
            Left = 0
            Top = 0
            Width = 349
            Height = 30
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 0
            object ckAnimate: TCheckBox
              Left = 0
              Top = 0
              Width = 100
              Height = 30
              Action = acAnimate
              Align = alLeft
              State = cbChecked
              TabOrder = 0
            end
            object paBrowserAnimSpeed: TPanel
              AlignWithMargins = True
              Left = 110
              Top = 0
              Width = 126
              Height = 30
              Margins.Left = 10
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              AutoSize = True
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 1
              object laBrowserAnimSpeed: TLabel
                AlignWithMargins = True
                Left = 0
                Top = 3
                Width = 64
                Height = 13
                Margins.Left = 0
                Align = alLeft
                Caption = 'Anim. Speed:'
                Layout = tlCenter
              end
              object edBrowserAnimSpeed: TEdit
                AlignWithMargins = True
                Left = 70
                Top = 5
                Width = 40
                Height = 20
                Margins.Top = 5
                Margins.Bottom = 5
                Align = alLeft
                NumbersOnly = True
                TabOrder = 0
                Text = '50'
                OnChange = edBrowserAnimSpeedChange
              end
              object udBrowserAnimSpeed: TUpDown
                Left = 110
                Top = 5
                Width = 16
                Height = 20
                Associate = edBrowserAnimSpeed
                Min = 1
                Max = 1000
                Position = 50
                TabOrder = 1
              end
            end
          end
          object paViewerControlsBottom: TPanel
            Left = 0
            Top = 30
            Width = 349
            Height = 30
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 1
            object ckFitToView: TCheckBox
              Left = 0
              Top = 0
              Width = 100
              Height = 30
              Action = acFitToView
              Align = alLeft
              State = cbChecked
              TabOrder = 0
            end
            object paBrowserZoom: TPanel
              AlignWithMargins = True
              Left = 110
              Top = 0
              Width = 126
              Height = 30
              Margins.Left = 10
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              AutoSize = True
              BevelOuter = bvNone
              TabOrder = 1
              object laBrowserZoom: TLabel
                AlignWithMargins = True
                Left = 12
                Top = 3
                Width = 52
                Height = 13
                Margins.Left = 12
                Align = alLeft
                Caption = 'Zoom (%):'
                Layout = tlCenter
              end
              object edBrowserZoom: TEdit
                AlignWithMargins = True
                Left = 70
                Top = 5
                Width = 40
                Height = 20
                Margins.Top = 5
                Margins.Bottom = 5
                Align = alLeft
                Enabled = False
                NumbersOnly = True
                TabOrder = 0
                Text = '100'
                OnChange = edBrowserZoomChange
              end
              object udBrowserZoom: TUpDown
                Left = 110
                Top = 5
                Width = 16
                Height = 20
                Associate = edBrowserZoom
                Enabled = False
                Min = 1
                Max = 5000
                Increment = 10
                Position = 100
                TabOrder = 1
              end
            end
          end
        end
      end
    end
    object tsBanking: TTabSheet
      Caption = 'tsBanking'
      ImageIndex = 1
      object sbBanking: TScrollBox
        Left = 0
        Top = 0
        Width = 661
        Height = 794
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Constraints.MaxWidth = 661
        Constraints.MinWidth = 600
        Color = clWhite
        ParentColor = False
        TabOrder = 0
        object blBankingHeaderSeparator: TBevel
          AlignWithMargins = True
          Left = 3
          Top = 204
          Width = 638
          Height = 1
          Align = alTop
          Shape = bsTopLine
        end
        object blBankingSummarySeparator: TBevel
          AlignWithMargins = True
          Left = 3
          Top = 360
          Width = 638
          Height = 1
          Align = alTop
          Shape = bsTopLine
        end
        object blBankingPaymentSeparator: TBevel
          AlignWithMargins = True
          Left = 3
          Top = 444
          Width = 638
          Height = 1
          Align = alTop
          Shape = bsTopLine
        end
        object blBankingAboutSeparator: TBevel
          AlignWithMargins = True
          Left = 3
          Top = 823
          Width = 638
          Height = 1
          Align = alTop
          Shape = bsTopLine
        end
        object blBankingPersonalInfoSeparator: TBevel
          AlignWithMargins = True
          Left = 3
          Top = 679
          Width = 638
          Height = 1
          Align = alTop
          Shape = bsTopLine
        end
        object paBankingHeader: TPanel
          Left = 0
          Top = 0
          Width = 644
          Height = 201
          Align = alTop
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 0
          object shBankingHeaderBackground: TShape
            Left = 0
            Top = 51
            Width = 644
            Height = 150
            Align = alBottom
            Brush.Color = 15790320
            Pen.Style = psClear
            Shape = stRoundRect
          end
          object imBankingHeaderClipart: TWSVGImage
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 638
            Height = 100
            Align = alTop
            Center = True
            Picture.Data = {
              0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
              203F3E3C73766720656E61626C652D6261636B67726F756E643D226E65772030
              20302035313220353132222069643D224C617965725F31222076657273696F6E
              3D22312E31222076696577426F783D2230203020353132203531322220786D6C
              3A73706163653D2270726573657276652220786D6C6E733D22687474703A2F2F
              7777772E77332E6F72672F323030302F7376672220786D6C6E733A786C696E6B
              3D22687474703A2F2F7777772E77332E6F72672F313939392F786C696E6B223E
              3C673E3C673E3C6C696E6561724772616469656E74206772616469656E74556E
              6974733D227573657253706163654F6E557365222069643D2253564749445F31
              5F222078313D222D302E30303030303237222078323D22353132222079313D22
              323536222079323D22323536223E3C73746F70206F66667365743D2230222073
              74796C653D2273746F702D636F6C6F723A23333342343944222F3E3C73746F70
              206F66667365743D223122207374796C653D2273746F702D636F6C6F723A2330
              3041313835222F3E3C2F6C696E6561724772616469656E743E3C636972636C65
              2063783D22323536222063793D22323536222066696C6C3D2275726C28235356
              4749445F315F292220723D22323536222F3E3C6C696E6561724772616469656E
              74206772616469656E74556E6974733D227573657253706163654F6E55736522
              2069643D2253564749445F325F222078313D2234322E36363636363431222078
              323D223436392E33333333343335222079313D223235362E3030303531383822
              2079323D223235362E30303035313838223E3C73746F70206F66667365743D22
              3022207374796C653D2273746F702D636F6C6F723A23303041313835222F3E3C
              73746F70206F66667365743D223122207374796C653D2273746F702D636F6C6F
              723A23333342343944222F3E3C2F6C696E6561724772616469656E743E3C7061
              746820643D224D3235362C3436392E33333338363233632D3131372E36333135
              3030322C302D3231332E333333333433352D39352E373032333932362D323133
              2E333333333433352D3231332E333333333433352020202063302D3131372E36
              3331343534352C39352E373031383335362D3231332E3333333331332C323133
              2E333333333433352D3231332E333333333133633131372E363335373131372C
              302C3231332E333333333433352C39352E373031383636312C3231332E333333
              333433352C3231332E33333333313320202020433436392E333333333433352C
              3337332E363331343639372C3337332E363335373131372C3436392E33333338
              3632332C3235362C3436392E333333383632337A222066696C6C3D2275726C28
              2353564749445F325F29222F3E3C2F673E3C673E3C7265637420686569676874
              3D2232382E3734393836363522206F7061636974793D22302E33222077696474
              683D223234302220783D223134342220793D223231362E35343435343034222F
              3E3C7061746820643D224D3338332E393234373734322C3230362E3534343534
              3034632D302E343033313938322D31332E343534343036372D31312E33373238
              3032372D32342E323535343737392D32342E393234383034372D32342E323535
              34373739682D31393020202020632D31332E3535323030322C302D32342E3532
              32313235322C31302E383031303731322D32342E393235333038322C32342E32
              353534373739483338332E393234373734327A22206F7061636974793D22302E
              33222F3E3C7061746820643D224D3134342C3235352E32393434303331763635
              2E3431353938353163302C31332E383037343634362C31312E31393235303439
              2C32352C32342E393939393639352C323568313930202020206331332E383036
              393435382C302C32352E303030303330352D31312E313932353335342C32352E
              303030303330352D3235762D36352E34313539383531483134347A204D333233
              2E303335313536332C3238392E31363135393036683130763130682D31305632
              38392E313631353930367A20202020204D3239382E303335313536332C323839
              2E31363135393036683130763130682D3130563238392E313631353930367A20
              4D3335382E303335313536332C3332322E35323130353731483235352E353335
              34333039762D3130683130322E34393937323533563332322E35323130353731
              7A20202020204D3335382E303335313536332C3239392E31363135393036682D
              3130762D3130683130563239392E313631353930367A22206F7061636974793D
              22302E33222F3E3C2F673E3C673E3C726563742066696C6C3D22234646464646
              4622206865696768743D2232382E37343938363635222077696474683D223234
              302220783D223133362220793D223230382E35343435343034222F3E3C706174
              6820643D224D3337352E393234373734322C3139382E35343435323531632D30
              2E343033313938322D31332E343534333931352D31312E333732383032372D32
              342E323535343632362D32342E393234383034372D32342E3235353436323668
              2D31393020202020632D31332E3535323030322C302D32342E35323231323532
              2C31302E383031303731322D32342E393235333038322C32342E323535343632
              36483337352E393234373734327A222066696C6C3D2223464646464646222F3E
              3C7061746820643D224D3133362C3234372E323934343033317636352E343135
              3938353163302C31332E383037343634362C31312E313932353034392C32352C
              32342E393939393639352C323568313930202020206331332E38303639343538
              2C302C32352E303030303330352D31312E313932353335342C32352E30303030
              3330352D3235762D36352E34313539383531483133367A204D3331352E303335
              313536332C3238312E31363135393036683130763130682D3130563238312E31
              3631353930367A20202020204D3239302E303335313536332C3238312E313631
              35393036683130763130682D3130563238312E313631353930367A204D333530
              2E303335313536332C3331342E35323130353731483234372E35333534333039
              762D3130683130322E34393937323533563331342E353231303537317A202020
              20204D3335302E303335313536332C3239312E31363135393036682D3130762D
              3130683130563239312E313631353930367A222066696C6C3D22234646464646
              46222F3E3C2F673E3C2F673E3C2F7376673E}
            Proportional = True
            Stretch = True
            Transparent = True
            Animation.FrameCount = 0
            Animation.Position = 0
          end
          object laBankingHeaderTitle: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 109
            Width = 190
            Height = 40
            Align = alTop
            Alignment = taCenter
            Caption = 'Payment info'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGrayText
            Font.Height = -33
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object laBankingHeaderDesc: TLabel
            Left = 0
            Top = 152
            Width = 527
            Height = 19
            Align = alTop
            Alignment = taCenter
            Caption = 
              'Please complete the following informations in order to process t' +
              'o payment'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGrayText
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
        end
        object paBankingSummary: TPanel
          Left = 0
          Top = 208
          Width = 644
          Height = 149
          Align = alTop
          BevelOuter = bvNone
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 1
          object laBankingSummaryTitle: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 134
            Height = 19
            Align = alTop
            Caption = 'Payment Summary'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = 526344
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object laBankingSummaryReview: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 28
            Width = 262
            Height = 13
            Align = alTop
            Caption = 'Please review the following details for this transaction.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = 526344
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object veBankingSummary: TValueListEditor
            AlignWithMargins = True
            Left = 3
            Top = 47
            Width = 638
            Height = 98
            TabStop = False
            Align = alTop
            BorderStyle = bsNone
            DrawingStyle = gdsGradient
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = 526344
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goThumbTracking]
            ParentFont = False
            ScrollBars = ssNone
            Strings.Strings = (
              'Birds collection for iPhone=$10.00'
              'Tic Tac Toe game for iPhone=$5.00'
              ' ='
              'Total=$15.00')
            TabOrder = 0
            TitleCaptions.Strings = (
              'Description'
              'Item price')
            ColWidths = (
              418
              218)
          end
        end
        object paBankingBillingInfo: TPanel
          Left = 0
          Top = 364
          Width = 644
          Height = 77
          Align = alTop
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 2
          object laBankingBillingInfoTitle: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 132
            Height = 19
            Align = alTop
            Caption = 'Billing Information'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = 526344
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object laBankingBillingInfoDesc: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 28
            Width = 165
            Height = 13
            Align = alTop
            Caption = 'Enter your payment details below.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = 526344
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object paBankingBillingPaymentMethods: TPanel
            Left = 0
            Top = 44
            Width = 644
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 0
            object laBankingPaymentMethod: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 138
              Height = 19
              Align = alLeft
              Caption = 'Payment Method'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 526344
              Font.Height = -16
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
              Layout = tlCenter
            end
            object imBankingPaymentMastercard: TWSVGImage
              AlignWithMargins = True
              Left = 296
              Top = 0
              Width = 33
              Height = 25
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D2269736F2D383835392D31223F3E0D0A3C212D2D2047
                656E657261746F723A2041646F626520496C6C7573747261746F722031392E30
                2E302C20535647204578706F727420506C75672D496E202E2053564720566572
                73696F6E3A20362E3030204275696C6420302920202D2D3E0D0A3C7376672076
                657273696F6E3D22312E31222069643D224C617965725F312220786D6C6E733D
                22687474703A2F2F7777772E77332E6F72672F323030302F7376672220786D6C
                6E733A786C696E6B3D22687474703A2F2F7777772E77332E6F72672F31393939
                2F786C696E6B2220783D223070782220793D22307078220D0A09207669657742
                6F783D22302030203239312E373931203239312E37393122207374796C653D22
                656E61626C652D6261636B67726F756E643A6E657720302030203239312E3739
                31203239312E3739313B2220786D6C3A73706163653D22707265736572766522
                3E0D0A3C673E0D0A093C70617468207374796C653D2266696C6C3A2345323537
                34433B2220643D224D3138322E3239382C3134352E38393563302C35302E3336
                362D34302E3830312C39312E3137362D39312E3134392C39312E31373653302C
                3139362E3235322C302C3134352E3839350D0A09097334302E3831312D39312E
                3137362C39312E3134392D39312E313736533138322E3239382C39352E353338
                2C3138322E3239382C3134352E3839357A222F3E0D0A093C7061746820737479
                6C653D2266696C6C3A234634423435393B2220643D224D3230302E3631362C35
                342E373139632D32302E3434322C302D33392E3236312C362E3831312D35342E
                3436392C31382E3138316C302E3037332C302E3030390D0A090963322E393931
                2C322E38392C362E3239312C342E3932342C382E3833352C382E3235316C2D31
                382E3936352C302E333031632D322E3937322C332D352E36382C362E3236342D
                382E3233332C392E363536483136312E3363322E3534342C332E3035342C342E
                3839362C352E3730382C372E30332C392E3038310D0A0909682D34362E353336
                632D312E3730352C322E3933362D332E3238322C352E3935342D342E3635392C
                392E30396835362E34393363312E3437372C332E3132372C322E3739392C352E
                3438392C332E3932312C382E373939682D36332E37360D0A0909632D312E3031
                322C332E3134362D312E3837382C362E3336342D322E3533352C392E36343668
                36382E39363663302E3637352C332E3135352C312E3139342C362E3037322C31
                2E35352C392E303435682D37312E383834632D302E3330312C332D302E343536
                2C362E3034352D302E3435362C392E3131380D0A09096837322E38353963302C
                332E3232382D302E3232382C362E3231382D302E3535362C392E313138682D37
                312E38343763302E33312C332E3039312C302E3736362C362E3132372C312E33
                36382C392E3131386836382E383536632D302E3731312C322E3935342D312E35
                33322C352E3932362D322E3536322C392E3030380D0A0909682D36332E393639
                63302E3936362C332E3131382C322E3134332C362E3134352C332E3432382C39
                2E3039396835362E363231632D312E3536382C332E3331392D332E3334362C35
                2E3937322D352E3330362C392E303831682D34362E3639310D0A090963312E38
                34322C332E3139312C332E3837352C362E3233362C362E3038312C392E313534
                6C33332E3538392C302E353031632D322E3836332C332E3433372D362E353337
                2C352E3530372D392E3838342C382E35313663302E3138322C302E3134362D35
                2E3335322D302E3031382D31362E3234382D302E3139310D0A09096331362E35
                37362C31372E3130352C33392E3734342C32372E3737322C36352E3434362C32
                372E3737326335302E3335372C302C39312E3137362D34302E38322C39312E31
                37362D39312E313736533235302E3938312C35342E3731392C3230302E363136
                2C35342E3731397A222F3E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C67
                3E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C
                673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A
                3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D
                0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E
                0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C2F7376673E0D0A}
              Proportional = True
              Transparent = True
              Animation.FrameCount = 0
              Animation.Position = 0
            end
            object imBankingPaymentVisa: TWSVGImage
              AlignWithMargins = True
              Left = 260
              Top = 0
              Width = 33
              Height = 25
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C73766720786D6C6E733A64633D2268747470
                3A2F2F7075726C2E6F72672F64632F656C656D656E74732F312E312F2220786D
                6C6E733A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F
                72672F6E73232220786D6C6E733A7264663D22687474703A2F2F7777772E7733
                2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E73232220
                786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F323030
                302F7376672220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F
                323030302F737667222076696577426F783D2230203020323030203132372E30
                343633222076657273696F6E3D22312E31222069643D22737667322220686569
                6768743D223132372E30343633222077696474683D22323030223E0D0A20203C
                646566732069643D226465667334223E0D0A202020203C636C69705061746820
                636C697050617468556E6974733D227573657253706163654F6E557365222069
                643D22636C69705061746834313537223E0D0A2020202020203C706174682064
                3D224D20302C373932203631322C373932203631322C3020302C3020302C3739
                32205A222069643D227061746834313539222F3E0D0A202020203C2F636C6970
                506174683E0D0A20203C2F646566733E0D0A20203C6D65746164617461206964
                3D226D6574616461746137223E0D0A202020203C7264663A5244463E0D0A2020
                202020203C63633A576F726B207264663A61626F75743D22223E0D0A20202020
                202020203C64633A666F726D61743E696D6167652F7376672B786D6C3C2F6463
                3A666F726D61743E0D0A20202020202020203C64633A74797065207264663A72
                65736F757263653D22687474703A2F2F7075726C2E6F72672F64632F64636D69
                747970652F5374696C6C496D616765222F3E0D0A20202020202020203C64633A
                7469746C652F3E0D0A2020202020203C2F63633A576F726B3E0D0A202020203C
                2F7264663A5244463E0D0A20203C2F6D657461646174613E0D0A20203C672074
                72616E73666F726D3D227472616E736C617465282D3137332E35373134332C2D
                3437312E3037323129222069643D226C6179657231223E0D0A202020203C6720
                69643D22673431343522207472616E73666F726D3D226D617472697828302E39
                323539323539342C302C302C2D302E39323539323539342C2D31302E36303931
                33332C3930302E383237363629223E0D0A2020202020203C7061746820643D22
                6D203431342E3931352C3332362E393236202D3231362C3020302C3133372E32
                31203231362C3020302C2D3133372E3231207A22207374796C653D2266696C6C
                3A236666666666663B66696C6C2D6F7061636974793A313B66696C6C2D72756C
                653A6E6F6E7A65726F3B7374726F6B653A6E6F6E65222069643D227061746834
                313437222F3E0D0A2020202020203C7061746820643D226D203230322E353132
                2C3334392E323231203230382E3830322C3020302C2D31382E3732202D323038
                2E3830322C3020302C31382E3732207A22207374796C653D2266696C6C3A2366
                37623630303B66696C6C2D6F7061636974793A313B66696C6C2D72756C653A6E
                6F6E7A65726F3B7374726F6B653A6E6F6E65222069643D227061746834313439
                222F3E0D0A2020202020203C7061746820643D226D203230322E3531322C3434
                312E383137203230382E3830322C3020302C31382E3732202D3230382E383032
                2C3020302C2D31382E3732207A22207374796C653D2266696C6C3A2331613166
                37313B66696C6C2D6F7061636974793A313B66696C6C2D72756C653A6E6F6E7A
                65726F3B7374726F6B653A6E6F6E65222069643D227061746834313531222F3E
                0D0A2020202020203C672069643D226734313533223E0D0A2020202020202020
                3C672069643D2267343135352220636C69702D706174683D2275726C2823636C
                6970506174683431353729223E0D0A202020202020202020203C672069643D22
                673431363122207472616E73666F726D3D227472616E736C617465283330352E
                313132332C3432312E3139323429223E0D0A2020202020202020202020203C70
                61746820643D226D20302C30202D31312E3030362C2D35312E343535202D3133
                2E3331322C30204C202D31332E3331312C3020302C30205A206D2035362E3030
                342C2D33332E32323620372E3030382C31392E33323420342E3033312C2D3139
                2E333234202D31312E3033392C30207A206D2031342E3835392C2D31382E3232
                392031322E3330392C30204C2037322E3431382C302036312E3036332C302063
                202D322E3535392C30202D342E3731352C2D312E343834202D352E36372C2D33
                2E373732206C202D31392E3937352C2D34372E3638332031332E39382C302032
                2E3737362C372E3638342031372E3037362C3020312E3631332C2D372E363834
                207A206D202D33342E3735322C31362E373939206320302E3035392C31332E35
                3739202D31382E3737312C31342E333332202D31382E3634362C32302E333939
                20302E3034312C312E38343420312E3739392C332E38303820352E3634342C34
                2E333120312E3930372C302E32343620372E3136362C302E3434342031332E31
                32392C2D322E333033204C2033382E35372C2D312E33333420432033352E3336
                372C2D302E3137362033312E3234362C302E39342032362E3131392C302E3934
                2031322E3935392C302E393420332E3730312C2D362E303520332E3632372C2D
                31362E3036382063202D302E3038342C2D372E34303820362E3631332C2D3131
                2E3533382031312E3634382C2D31342E30303520352E3139322C2D322E353232
                20362E3933322C2D342E313420362E3930372C2D362E333933202D302E303336
                2C2D332E343532202D342E3134312C2D342E3938202D372E3936312C2D352E30
                3338202D362E3639382C2D302E313034202D31302E3537382C312E3831202D31
                332E3637322C332E323532206C202D322E3431362C2D31312E3238206320332E
                3131352C2D312E34323620382E3835352C2D322E3636372031342E3739392C2D
                322E3733312031332E39392C302032332E3133382C362E3930392032332E3137
                392C31372E363037204D202D31392E3032312C30202D34302E35392C2D35312E
                343535206C202D31342E30372C30202D31302E3631352C34312E303635206320
                2D302E3634332C322E353235202D312E3230342C332E343533202D332E313631
                2C342E3532202D332E3230312C312E373339202D382E3438362C332E33363520
                2D31332E3133322C342E333737204C202D38312E3235342C30202D35382E3630
                322C30206320322E3838352C3020352E3438312C2D312E393220362E3134312C
                2D352E323433204C202D34362E3835342C2D33352E303231202D33332E303036
                2C30202D31392E3032312C30205A22207374796C653D2266696C6C3A23316131
                6637313B66696C6C2D6F7061636974793A313B66696C6C2D72756C653A6E6F6E
                7A65726F3B7374726F6B653A6E6F6E65222069643D227061746834313633222F
                3E0D0A202020202020202020203C2F673E0D0A20202020202020203C2F673E0D
                0A2020202020203C2F673E0D0A202020203C2F673E0D0A20203C2F673E0D0A3C
                2F7376673E}
              Proportional = True
              Transparent = True
              Animation.FrameCount = 0
              Animation.Position = 0
            end
            object imBankingPaymentPayPal: TWSVGImage
              Left = 488
              Top = 0
              Width = 33
              Height = 25
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D227574662D38223F3E0A3C212D2D2047656E65726174
                6F723A2041646F626520496C6C7573747261746F722031362E302E302C205356
                47204578706F727420506C75672D496E202E205356472056657273696F6E3A20
                362E3030204275696C6420302920202D2D3E0A3C21444F435459504520737667
                205055424C494320222D2F2F5733432F2F4454442053564720312E312F2F454E
                222022687474703A2F2F7777772E77332E6F72672F47726170686963732F5356
                472F312E312F4454442F73766731312E647464223E0A3C737667207665727369
                6F6E3D22312E31222069643D224C617965725F312220786D6C6E733A736B6574
                63683D22687474703A2F2F7777772E626F68656D69616E636F64696E672E636F
                6D2F736B657463682F6E73220A0920786D6C6E733D22687474703A2F2F777777
                2E77332E6F72672F323030302F7376672220786D6C6E733A786C696E6B3D2268
                7474703A2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D22
                3070782220793D22307078222077696474683D22373830707822206865696768
                743D223530317078220A092076696577426F783D223020302037383020353031
                2220656E61626C652D6261636B67726F756E643D226E65772030203020373830
                203530312220786D6C3A73706163653D227072657365727665223E0A3C746974
                6C653E616D65782D6F75746C696E653C2F7469746C653E0A3C646573633E4372
                6561746564207769746820536B657463682E3C2F646573633E0A3C7061746820
                66696C6C3D22234646464646462220643D224D3732352C304835354332342E36
                37332C302C302C32342E3637332C302C35357633393163302C33302E3332372C
                32342E3637332C35352C35352C3535683637306333302E3332352C302C35352D
                32342E3637332C35352D35355635350A09433738302C32342E3637332C373535
                2E3332352C302C3732352C307A222F3E0A3C706174682066696C6C3D22233030
                333038372220643D224D3136382E3337392C3136392E383533632D382E333939
                2D352E3737342D31392E3335392D382E3636382D33322E38382D382E36363848
                38332E313533632D342E3134352C302D362E3433352C322E3037332D362E3837
                2C362E3231340A094C35352E3031382C3330302E383833632D302E3232312C31
                2E3331312C302E3130372C322E35312C302E3938312C332E3663302E3836392C
                312E3039322C312E3936322C312E3633352C332E3237312C312E363335683234
                2E38363463342E3336312C302C362E3735382D322E3036382C372E3139382D36
                2E3231350A096C352E3838382D33352E39383663302E3231352D312E3734342C
                302E3938322D332E3136322C322E3239312D342E32353463312E3330382D312E
                30392C322E3934342D312E3830332C342E3930372D322E313363312E3936332D
                302E3332342C332E3831342D302E3438372C352E3536322D302E3438370A0963
                312E3734332C302C332E3831342C302E31312C362E3231372C302E3332376332
                2E3339372C302E3231382C332E3932352C302E3332342C342E35382C302E3332
                346331382E3735362C302C33332E3437382D352E3238352C34342E3136372D31
                352E3836360A096331302E3638342D31302E3537372C31362E3033322D32352E
                3234342C31362E3033322D34342E303034433138302E3937362C3138342E3936
                2C3137362E3737342C3137352E3633362C3136382E3337392C3136392E383533
                7A204D3134312E3338392C3230392E3933330A09632D312E3039342C372E3633
                352D332E3932362C31322E3634392D382E3530362C31352E303439632D342E35
                38312C322E3430332D31312E3132342C332E3539372D31392E3632392C332E35
                39376C2D31302E3739372C302E3332386C352E3536332D33352E3030370A0963
                302E3433342D322E3339372C312E3835312D332E3539372C342E3235322D332E
                35393768362E32313863382E37322C302C31352E3034392C312E3235372C3138
                2E3937352C332E373631433134312E3338392C3139362E3537342C3134322E36
                39382C3230312E3836362C3134312E3338392C3230392E3933337A222F3E0A3C
                706174682066696C6C3D22233030394344452220643D224D3732302E3739342C
                3136312E313835682D32342E323038632D322E3430352C302D332E3832312C31
                2E322D342E3235332C332E3539396C2D32312E3236372C3133362E3039396C2D
                302E3332382C302E3635340A0963302C312E3039362C302E3433372C322E3132
                372C312E3331312C332E31303963302E3836382C302E39382C312E3936332C31
                2E3437312C332E32372C312E3437316832312E35393563342E3133382C302C36
                2E3432392D322E3036382C362E3837312D362E3231356C32312E3236352D3133
                332E383132762D302E3332350A09433732352E3034392C3136322E3731322C37
                32332E3632372C3136312E3138352C3732302E3739342C3136312E3138357A22
                2F3E0A3C706174682066696C6C3D22233030333038372220643D224D3432382E
                33312C3231332E38353763302D312E3038382D302E3433392D322E3132362D31
                2E3330362D332E313036632D302E3837352D302E3938312D312E3835382D312E
                3437342D322E3934352D312E343734682D32352E3139320A09632D322E343034
                2C302D342E3336362C312E3039362D352E3838392C332E3237316C2D33342E36
                37392C35312E30346C2D31342E3339342D34392E303735632D312E3039352D33
                2E3438382D332E3439332D352E3233362D372E3139382D352E323336682D3234
                2E35340A09632D312E3039332C302D322E3037352C302E3439322D322E393432
                2C312E343734632D302E3837352C302E39382D312E3330392C322E3031392D31
                2E3330392C332E31303663302C302E34342C322E3132372C362E3837312C362E
                3337392C31392E3330330A0963342E3235322C31322E3433342C382E3833332C
                32352E3834382C31332E3734312C34302E32343563342E3930382C31342E3339
                332C372E3436382C32322E3033312C372E3638382C32322E383938632D31372E
                3838362C32342E34332D32362E3832372C33372E3531372D32362E3832372C33
                392E3235390A0963302C322E3833382C312E3431372C342E3235342C342E3235
                332C342E3235346832352E31393263322E3339392C302C342E3336312D312E30
                38382C352E3838392D332E32376C38332E3432372D3132302E3339390A094334
                32382E3039322C3231352E3731332C3432382E33312C3231342E3935332C3432
                382E33312C3231332E3835377A222F3E0A3C706174682066696C6C3D22233030
                394344452220643D224D3636322E3838372C3230392E323737682D32342E3836
                36632D332E3035352C302D342E3930342C332E3539392D352E3535382C31302E
                373937632D352E3637372D382E37322D31362E3033312D31332E3038382D3331
                2E3038332D31332E3038380A09632D31352E3730342C302D32392E3036362C35
                2E38392D34302E3037372C31372E363638632D31312E3031362C31312E373739
                2D31362E3532312C32352E3633312D31362E3532312C34312E35353163302C31
                322E3837312C332E3736312C32332E3132312C31312E3238352C33302E373532
                0A0963372E3532352C372E3633392C31372E3631322C31312E3435312C33302E
                3236362C31312E34353163362E3332332C302C31322E3735372D312E3331312C
                31392E332D332E39323663362E3534342D322E3631372C31312E3636352D362E
                3130352C31352E3337392D31302E3436390A0963302C302E3231392D302E3232
                322C312E3139392D302E3635352C322E393433632D302E34342C312E3734382D
                302E3635352C332E3035392D302E3635352C332E39323663302C332E3439342C
                312E3431342C352E3233342C342E3235342C352E3233346832322E3537360A09
                63342E3133382C302C362E3534312D322E3036382C372E3139342D362E323135
                6C31332E3431352D38352E33383963302E3231352D312E3330392D302E313132
                2D322E3530372D302E3938322D332E3539390A09433636352E3238342C323039
                2E3832342C3636342E3139362C3230392E3237372C3636322E3838372C323039
                2E3237377A204D3632302E3139332C3237332E373239632D352E3536322C352E
                3435332D31322E3236382C382E3137382D32302E31322C382E3137380A09632D
                362E3332382C302D31312E3434392D312E3734322D31352E3337372D352E3233
                34632D332E3932372D332E3438342D352E38392D382E3238332D352E38392D31
                342E33393563302D382E3036352C322E3732362D31342E3838352C382E31382D
                32302E3434370A0963352E3434372D352E3536322C31322E3231342D382E3334
                332C32302E3238352D382E33343363362E3130312C302C31312E3137332C312E
                382C31352E3231322C352E33393863342E3033322C332E3539392C362E303534
                2C382E3536332C362E3035342C31342E3838380A09433632382E3533362C3236
                312E3632352C3632352E3735342C3236382E3237392C3632302E3139332C3237
                332E3732397A222F3E0A3C706174682066696C6C3D2223303033303837222064
                3D224D3239312E32332C3230392E323737682D32342E383634632D332E303538
                2C302D342E3930382C332E3539392D352E3536332C31302E373937632D352E38
                38392D382E37322D31362E32352D31332E3038382D33312E3038312D31332E30
                38380A09632D31352E3730342C302D32392E3036352C352E38392D34302E3037
                382C31372E363638632D31312E3031362C31312E3737392D31362E3532312C32
                352E3633312D31362E3532312C34312E35353163302C31322E3837312C332E37
                36332C32332E3132312C31312E3238382C33302E3735320A0963372E3532352C
                372E3633392C31372E36312C31312E3435312C33302E3236322C31312E343531
                63362E3130342C302C31322E3433332D312E3331312C31382E3937352D332E39
                323663362E3534332D322E3631372C31312E3737382D362E3130352C31352E37
                30342D31302E3436390A09632D302E3837352C322E3631372D312E3330392C34
                2E3930382D312E3330392C362E38363963302C332E3439342C312E3431372C35
                2E3233342C342E3235332C352E3233346832322E35373463342E3134312C302C
                362E3534332D322E3036382C372E3139382D362E3231356C31332E3431332D38
                352E3338390A0963302E3231352D312E3330392D302E3131322D322E3530372D
                302E3938312D332E353939433239332E3632372C3230392E3832342C3239322E
                3533382C3230392E3237372C3239312E32332C3230392E3237377A204D323438
                2E3533352C3237332E3839310A09632D352E3536332C352E33352D31322E3338
                322C382E3031362D32302E3434372C382E303136632D362E3332392C302D3131
                2E342D312E3734322D31352E3231342D352E323334632D332E3831392D332E34
                38342D352E3732362D382E3238332D352E3732362D31342E3339350A0963302D
                382E3036352C322E3732352D31342E3838352C382E31382D32302E3434376335
                2E3434392D352E3536322C31322E3231312D382E3334332C32302E3238342D38
                2E33343363362E3130342C302C31312E3137352C312E382C31352E3231342C35
                2E3339380A0963342E3033322C332E3539392C362E3035322C382E3536332C36
                2E3035322C31342E383838433235362E3837382C3236312E3834342C3235342E
                3039372C3236382E3535332C3234382E3533352C3237332E3839317A222F3E0A
                3C706174682066696C6C3D22233030394344452220643D224D3534302E303336
                2C3136392E383533632D382E3339382D352E3737342D31392E3335362D382E36
                36382D33322E3837392D382E363638682D35322E303139632D342E3336352C30
                2D362E3736352C322E3037332D372E3139382C362E3231340A096C2D32312E32
                36352C3133332E343833632D302E3232312C312E3331312C302E3130362C322E
                35312C302E3938312C332E3663302E3836362C312E3039322C312E3936322C31
                2E3633352C332E3237312C312E3633356832362E38323663322E3631372C302C
                342E3336312D312E3431362C352E3233352D342E3235320A096C352E38392D33
                372E39343963302E3231362D312E3734342C302E39382D332E3136322C322E32
                392D342E32353463312E3330392D312E30392C322E3934332D312E3830332C34
                2E3930382D322E313363312E3936322D302E3332342C332E3831332D302E3438
                372C352E3536322D302E3438370A0963312E3734332C302C332E3831342C302E
                31312C362E3231342C302E33323763322E3339392C302E3231382C332E39332C
                302E3332342C342E35382C302E3332346331382E3735392C302C33332E343739
                2D352E3238352C34342E3136382D31352E3836360A096331302E3638372D3130
                2E3537372C31362E3033312D32352E3234342C31362E3033312D34342E303034
                433535322E3633322C3138342E39362C3534382E3433312C3137352E3633362C
                3534302E3033362C3136392E3835337A204D3530362E3530322C3232332E3637
                330A09632D342E3739392C332E3237312D31312E3939372C342E3930362D3231
                2E3539322C342E3930366C2D31302E34372C302E3332386C352E3536332D3335
                2E30303763302E3433322D322E3339372C312E3834392D332E3539372C342E32
                35322D332E35393768352E3838370A0963342E3739372C302C382E3631342C30
                2E3231382C31312E3435342C302E36353363322E3833312C302E34342C352E35
                36312C312E3739392C382E3137382C342E30383963322E3631392C322E323931
                2C332E3932362C352E3631382C332E3932362C392E39380A09433531332E372C
                3231342E3138352C3531312E3239382C3232302E342C3530362E3530322C3232
                332E3637337A222F3E0A3C2F7376673E0A}
              Proportional = True
              Transparent = True
              Animation.FrameCount = 0
              Animation.Position = 0
            end
            object imBankingPaymentAmEx: TWSVGImage
              AlignWithMargins = True
              Left = 332
              Top = 0
              Width = 33
              Height = 25
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                223F3E0A3C7376672077696474683D22373532707822206865696768743D2234
                37317078222076696577426F783D223020302037353220343731222076657273
                696F6E3D22312E312220786D6C6E733D22687474703A2F2F7777772E77332E6F
                72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F
                2F7777772E77332E6F72672F313939392F786C696E6B2220786D6C6E733A736B
                657463683D22687474703A2F2F7777772E626F68656D69616E636F64696E672E
                636F6D2F736B657463682F6E73223E0A202020203C212D2D2047656E65726174
                6F723A20536B6574636820332E332E312028313230303529202D20687474703A
                2F2F7777772E626F68656D69616E636F64696E672E636F6D2F736B6574636820
                2D2D3E0A202020203C7469746C653E536C69636520313C2F7469746C653E0A20
                2020203C646573633E43726561746564207769746820536B657463682E3C2F64
                6573633E0A202020203C646566733E3C2F646566733E0A202020203C67206964
                3D22506167652D3122207374726F6B653D226E6F6E6522207374726F6B652D77
                696474683D2231222066696C6C3D226E6F6E65222066696C6C2D72756C653D22
                6576656E6F64642220736B657463683A747970653D224D5350616765223E0A20
                202020202020203C672069643D22616D65782220736B657463683A747970653D
                224D534C6179657247726F7570223E0A2020202020202020202020203C726563
                742069643D2252656374616E676C652D31222066696C6C3D2223323535374436
                2220736B657463683A747970653D224D53536861706547726F75702220783D22
                312220793D2230222077696474683D2237353022206865696768743D22343731
                222072783D223430223E3C2F726563743E0A2020202020202020202020203C70
                61746820643D224D312E3030323638382C3232312E3138353038204C33372E30
                32363834392C3232312E3138353038204C34352E3134393537392C3230312E36
                37353036204C36332E3333343539362C3230312E3637353036204C37312E3433
                363034322C3232312E3138353038204C3134322E33313633372C3232312E3138
                353038204C3134322E33313633372C3230362E3236393039204C3134382E3634
                3332322C3232312E3234383636204C3138352E34333839342C3232312E323438
                3636204C3139312E37363537392C3230362E3034363534204C3139312E373635
                37392C3232312E3138353038204C3336372E39313730312C3232312E31383530
                38204C3336372E38333435312C3138392E3135393431204C3337312E32343237
                2C3138392E313539343120433337332E36323932342C3138392E323431363120
                3337342E333236332C3138392E3436313434203337342E333236332C3139332E
                3338353136204C3337342E333236332C3232312E3138353038204C3436352E34
                333233322C3232312E3138353038204C3436352E34333233322C3231332E3732
                39373320433437322E37383038322C3231372E36353038203438342E32313036
                342C3232312E3138353038203439392E32353038362C3232312E313835303820
                4C3533372E35373930382C3232312E3138353038204C3534352E37383136332C
                3230312E3637353036204C3536332E39363636342C3230312E3637353036204C
                3537312E39383832382C3232312E3138353038204C3634352E38343834342C32
                32312E3138353038204C3634352E38343834342C3230322E3635323639204C36
                35372E303333352C3232312E3138353038204C3731362E32323036312C323231
                2E3138353038204C3731362E32323036312C39382E3637373839204C3635372E
                36343534332C39382E3637373839204C3635372E36343534332C3131332E3134
                363134204C3634392E34343238382C39382E3637373839204C3538392E333337
                38372C39382E3637373839204C3538392E33333738372C3131332E3134363134
                204C3538312E38303537392C39382E3637373839204C3530302E36313833392C
                39382E363737383920433438372E30323831382C39382E363737383920343735
                2E30383232312C3130302E35363639203436352E34333233322C3130352E3833
                313231204C3436352E34333233322C39382E3637373839204C3430392E343035
                39362C39382E3637373839204C3430392E34303539362C3130352E3833313231
                20433430332E32363533362C3130302E3430353239203339342E38393738362C
                39382E3637373839203338352E35393338332C39382E3637373839204C313830
                2E39303739362C39382E3637373839204C3136372E31373430372C3133302E33
                313934204C3135332E30373033372C39382E3637373839204C38382E35393933
                372C39382E3637373839204C38382E35393933372C3131332E3134363134204C
                38312E3531363932342C39382E3637373839204C32362E3533333531382C3938
                2E3637373839204C302E3939393939372C3135362E3932343435204C302E3939
                393939372C3232312E3138353038204C312E3030323539372C3232312E313835
                3038204C312E3030323638382C3232312E3138353038205A204D3232382E3339
                3932322C3230332E3531343336204C3230362E37383437322C3230332E353134
                3336204C3230362E37303439322C3133342E3732303634204C3137362E313332
                32382C3230332E3531343336204C3135372E36322C3230332E3531343336204C
                3132362E39363735342C3133342E36353937204C3132362E39363735342C3230
                332E3531343336204C38342E3038343432372C3230332E3531343336204C3735
                2E3938323938312C3138332E3932323232204C33322E3038333532342C313833
                2E3932323232204C32332E383939362C3230332E3531343336204C312E303030
                3034372C3230332E3531343336204C33382E3735363234312C3131352E363736
                3932204C37302E30383138332C3131352E3637363932204C3130352E39343130
                332C3139382E3834303836204C3130352E39343130332C3131352E3637363932
                204C3134302E33353238392C3131352E3637363932204C3136372E3934353639
                2C3137352E3236343036204C3139332E32393239372C3131352E363736393220
                4C3232382E33393635372C3131352E3637363932204C3232382E33393635372C
                3230332E3531343336204C3232382E33393935372C3230332E3531343336204C
                3232382E33393932322C3230332E3531343336205A204D36382E373737323134
                2C3136352E3639323837204C35342E3334363236352C3133302E363736303620
                4C33392E3939373739342C3136352E3639323837204C36382E3737373231342C
                3136352E3639323837204C36382E3737373231342C3136352E3639323837205A
                204D3331342E34313934372C3230332E3531343336204C3234332E3938363131
                2C3230332E3531343336204C3234332E39383631312C3131352E363736393220
                4C3331342E34313934372C3131352E3637363932204C3331342E34313934372C
                3133332E3936383231204C3236352E30373131362C3133332E3936383231204C
                3236352E30373131362C3134392E38303039204C3331332E32333535312C3134
                392E38303039204C3331332E32333535312C3136372E3830363036204C323635
                2E30373131362C3136372E3830363036204C3236352E30373131362C3138352E
                3334373539204C3331342E34313934372C3138352E3334373539204C3331342E
                34313934372C3230332E3531343336204C3331342E34313934372C3230332E35
                31343336205A204D3431332E36373532382C3133392E33333332312043343133
                2E36373532382C3135332E3333373832203430342E32383837372C3136302E35
                37333236203339382E38313836332C3136322E373435373520433430332E3433
                3230362C3136342E3439343334203430372E33373233372C3136372E35383335
                31203430392E32343830382C3137302E313432383120433431322E3232353235
                2C3137342E3531313634203431322E37333837352C3137382E34313431362034
                31322E37333837352C3138362E3235383937204C3431322E37333837352C3230
                332E3531343336204C3339312E34373237382C3230332E3531343336204C3339
                312E33393239382C3139322E343337333220433339312E33393239382C313837
                2E31353138203339312E39303131352C3137392E3535303734203338382E3036
                34362C3137352E333234393920433338342E39383336362C3137322E32333538
                31203338302E32383737342C3137312E3536353532203337322E36393731342C
                3137312E3536353532204C3335302E30363336332C3137312E3536353532204C
                3335302E30363336332C3230332E3531343336204C3332382E39383132352C32
                30332E3531343336204C3332382E39383132352C3131352E3637363932204C33
                37372E34373535322C3131352E363736393220433338382E32353038342C3131
                352E3637363932203339362E31383939392C3131352E39363034203430332E30
                303633392C3131392E383834313320433430392E36373634342C3132332E3830
                373836203431332E36373532392C3132392E3533353831203431332E36373532
                392C3133392E3333333231204C3431332E36373532382C3133392E3333333231
                205A204D3338372E30323237372C3135322E333736333220433338342E313235
                342C3135342E3132373536203338302E36393835392C3135342E313835383420
                3337362E35393333332C3135342E3138353834204C3335302E39373939382C31
                35342E3138353834204C3335302E39373939382C3133342E3637353833204C33
                37362E39343138362C3133342E363735383320433338302E36313631312C3133
                342E3637353833203338342E34343939392C3133342E38343031203338362E39
                343032392C3133362E323630313620433338392E36373533362C3133372E3533
                393831203339312E33363734392C3134302E3236333337203339312E33363734
                392C3134342E303235343820433339312E33363734392C3134372E3836343433
                203338392E37353738342C3135302E3935333631203338372E30323237372C31
                35322E3337363332204C3338372E30323237372C3135322E3337363332205A20
                4D3434372E34383930382C3230332E3531343336204C3432352E39373536392C
                3230332E3531343336204C3432352E39373536392C3131352E3637363932204C
                3434372E34383930382C3131352E3637363932204C3434372E34383930382C32
                30332E3531343336204C3434372E34383930382C3230332E3531343336205A20
                4D3639372E32323835362C3230332E3531343336204C3636372E33353033322C
                3230332E3531343336204C3632372E33383538352C3133372E3538373237204C
                3632372E33383538352C3230332E3531343336204C3538342E34343638372C32
                30332E3531343336204C3537362E32343136362C3138332E3932323232204C35
                33322E34343333312C3138332E3932323232204C3532342E34383238372C3230
                332E3531343336204C3439392E38313133372C3230332E353134333620433438
                392E35363238342C3230332E3531343336203437362E35383732322C3230312E
                3235373039203436392E32333837322C3139332E373939303920433436312E38
                323930332C3138362E33343131203435372E39373338362C3137362E32333930
                33203435372E39373338362C3136302E323635393320433435372E3937333836
                2C3134372E3233383935203436302E32373739312C3133352E3333203436392E
                33333938332C3132352E393139343120433437362E31353632312C3131382E39
                30393136203438362E38333034342C3131352E3637363932203530312E333539
                38322C3131352E3637363932204C3532312E37373137342C3131352E36373639
                32204C3532312E37373137342C3133342E3439383039204C3530312E37383831
                382C3133342E343938303920433439342E303933382C3133342E343938303920
                3438392E37343930392C3133352E3633373333203438352E3536342C3133392E
                373031343720433438312E39363935372C3134332E34203437392E3530333232
                2C3135302E3339313731203437392E35303332322C3135392E35393832392043
                3437392E35303332322C3136392E3030383837203438312E33383135382C3137
                352E3739333933203438352E33303036312C3138302E32323633332043343838
                2E353436352C3138332E3730323332203439342E3434352C3138342E37353637
                37203439392E39393439352C3138342E3735363737204C3530392E3436333933
                2C3138342E3735363737204C3533392E31373938372C3131352E363739353720
                4C3537302E37373135322C3131352E3637393537204C3630362E34363834332C
                3139382E3736313338204C3630362E34363834332C3131352E3637393537204C
                3633382E353730392C3131352E3637393537204C3637352E363332372C313736
                2E3835333638204C3637352E363332372C3131352E3637393537204C3639372E
                32323835362C3131352E3637393537204C3639372E32323835362C3230332E35
                31343336204C3639372E32323835362C3230332E3531343336205A204D353639
                2E30373035312C3136352E3639323837204C3535342E34373939332C3133302E
                3637363036204C3533392E39363931362C3136352E3639323837204C3536392E
                30373035312C3136352E3639323837204C3536392E30373035312C3136352E36
                39323837205A222069643D2250617468222066696C6C3D222346464646464622
                20736B657463683A747970653D224D53536861706547726F7570223E3C2F7061
                74683E0A2020202020202020202020203C7061746820643D224D3735302E3935
                3634342C3334332E373637313620433734352E38333438352C3335312E323235
                3136203733352E38353530342C3335352E3030353832203732322E3334343634
                2C3335352E3030353832204C3638312E36323732332C3335352E303035383220
                4C3638312E36323732332C3333362E31363631204C3732322E31373936392C33
                33362E3136363120433732362E32303234382C3333362E31363631203732392E
                30313733362C3333352E3633383837203733302E37313231352C3333332E3939
                30393620433733322E31383037392C3333322E3633313833203733332E323035
                312C3333302E3635383034203733332E323035312C3332382E32363033362043
                3733332E323035312C3332352E3730313037203733322E31383037392C333233
                2E3636383939203733302E36323936372C3332322E343530323820433732392E
                30393938342C3332312E3130393639203732362E38373239342C3332302E3530
                303333203732332E32303133352C3332302E353030333320433730332E343034
                30322C3331392E3833303035203637382E37303539322C3332312E3130393639
                203637382E37303539322C3239332E333037313420433637382E37303539322C
                3238302E3536333633203638362E38333133312C3236372E3134393833203730
                382E39353636342C3236372E3134393833204C3735302E39353337392C323637
                2E3134393833204C3735302E39353337392C3234392E3636393235204C373131
                2E39333338322C3234392E363639323520433730302E31353831322C3234392E
                3636393235203639312E36303433382C3235322E3437373539203638352E3534
                3632362C3235362E3834333735204C3638352E35343632362C3234392E363639
                3235204C3632372E38333034342C3234392E363639323520433631382E363030
                39312C3234392E3636393235203630372E37363730362C3235312E3934373731
                203630322E36343237392C3235362E3834333735204C3630322E36343237392C
                3234392E3636393235204C3439392E35373735312C3234392E3636393235204C
                3439392E35373735312C3235362E383433373520433439312E33373439362C32
                35302E3935313534203437372E35333436362C3234392E363639323520343731
                2E31343636332C3234392E3636393235204C3430332E31363336362C3234392E
                3636393235204C3430332E31363336362C3235362E383433373520433339362E
                36373435322C3235302E3538353933203338322E32343335372C3234392E3636
                393235203337332E34343737322C3234392E3636393235204C3239372E333633
                332C3234392E3636393235204C3237392E39353235322C3236382E3433323133
                204C3236332E36343538362C3234392E3636393235204C3134392E3939313439
                2C3234392E3636393235204C3134392E39393134392C3337322E323631323120
                4C3236312E35303637362C3337322E3236313231204C3237392E3434372C3335
                332E3230313539204C3239362E33343639372C3337322E3236313231204C3336
                352E30383535342C3337322E3332323131204C3336352E30383535342C333433
                2E3438333634204C3337312E38343333392C3334332E34383336342043333830
                2E39363338342C3334332E3632343035203339312E37323035342C3334332E32
                35383435203430312E32313037392C3333392E3137333131204C3430312E3231
                3037392C3337322E3235383532204C3435372E39303736322C3337322E323538
                3532204C3435372E39303736322C3334302E3330373034204C3436302E363432
                36382C3334302E333037303420433436342E31333333362C3334302E33303730
                34203436342E34373635372C3334302E3435303131203436342E34373635372C
                3334332E3932333434204C3436342E34373635372C3337322E3235353837204C
                3633362E37313134342C3337322E323535383720433634372E36343633392C33
                37322E3235353837203635392E30373632312C3336392E343638373320363635
                2E34303537312C3336342E3431313037204C3636352E34303537312C3337322E
                3235353837204C3732302E30333739322C3337322E323535383720433733312E
                34303635362C3337322E3235353837203734322E35303931332C3337302E3636
                383839203735302E39353634342C3336362E3630343735204C3735302E393536
                34342C3334332E3736373132204C3735302E39353634342C3334332E37363731
                36205A204D3430392E34353330312C3239362E363132363620433430392E3435
                3330312C3332312E3031383732203339312E31363638392C3332362E30353738
                34203337322E373337312C3332362E3035373834204C3334362E34323933352C
                3332362E3035373834204C3334362E34323933352C3335352E3532363835204C
                3330352E34343835352C3335352E3532363835204C3237392E34383636372C33
                32362E3434313939204C3235322E353035382C3335352E3532363835204C3136
                382E393930342C3335352E3532363835204C3136382E393930342C3236372E36
                36383232204C3235332E37393038362C3236372E3636383232204C3237392E37
                333134342C3239362E3436363934204C3330362E35353030322C3236372E3636
                383232204C3337332E39323130362C3236372E363638323220433339302E3635
                33342C3236372E3636383232203430392E34353330312C3237322E3238303738
                203430392E34353330312C3239362E3631323636204C3430392E34353330312C
                3239362E3631323636205A204D3234312E38323738312C3333372E3034363535
                204C3138392E393839322C3333372E3034363535204C3138392E393839322C33
                31392E3536353936204C3233362E32373738352C3331392E3536353936204C32
                33362E32373738352C3330312E3634303238204C3138392E393839322C333031
                2E3634303238204C3138392E393839322C3238352E3636373138204C3234322E
                38343934372C3238352E3636373138204C3236352E39313133322C3331312E32
                37303737204C3234312E38323738312C3333372E3034363535204C3234312E38
                323738312C3333372E3034363535205A204D3332352E333534352C3334372E31
                30363638204C3239322E393833332C3331312E33313839204C3332352E333534
                352C3237362E36363737204C3332352E333534352C3334372E3130363638204C
                3332352E333534352C3334372E3130363638205A204D3337332E323237322C33
                30382E3034313137204C3334352E39383032372C3330382E3034313137204C33
                34352E39383032372C3238352E3636373138204C3337332E34373139372C3238
                352E363637313820433338312E30383338382C3238352E363637313820333836
                2E33363737372C3238382E3735363336203338362E33363737372C3239362E34
                3339353620433338362E33363737372C3330342E3033373936203338312E3332
                3836352C3330382E3034313137203337332E323237322C3330382E3034313137
                204C3337332E323237322C3330382E3034313137205A204D3531352E39373035
                332C3236372E3636383232204C3538362E33343030342C3236372E3636383232
                204C3538362E33343030342C3238352E3833373634204C3533362E3936373738
                2C3238352E3833373634204C3533362E39363737382C3330312E383130373420
                4C3538352E313334382C3330312E3831303734204C3538352E313334382C3331
                392E3733363432204C3533362E39363737382C3331392E3733363432204C3533
                362E39363737382C3333372E3231373031204C3538362E33343030342C333337
                2E3239363431204C3538362E33343030342C3335352E3532363738204C353135
                2E39373035332C3335352E3532363738204C3531352E39373035332C3236372E
                3636383135204C3531352E39373035332C3236372E3636383232205A204D3438
                382E39313732342C3331342E3639373320433439332E36313034392C3331362E
                3432323035203439372E34343730332C3331392E3531333837203439392E3234
                3535392C3332322E303733313720433530322E32323237362C3332362E333632
                3531203530322E36353337382C3333302E3336353731203530322E3733383931
                2C3333382E3130393835204C3530322E37333839312C3335352E353236383520
                4C3438312E353731342C3335352E3532363835204C3438312E353731342C3334
                342E353334353820433438312E353731342C3333392E3234393038203438322E
                30383232332C3333312E3432323832203437382E313633322C3332372E333337
                343820433437352E30383232362C3332342E3139303032203437302E33383633
                352C3332332E34333736203436322E36393436332C3332332E34333736204C34
                34302E31363232332C3332332E34333736204C3434302E31363232332C333535
                2E3532363835204C3431382E39373630392C3335352E3532363835204C343138
                2E39373630392C3236372E3636383232204C3436372E36353339332C3236372E
                363638323220433437382E33323831362C3236372E3636383232203438362E31
                303233362C3236382E3133373136203439332E30323235312C3237312E383134
                343920433439392E363736362C3237352E38313737203530332E38363136382C
                3238312E3330313931203530332E38363136382C3239312E3332343520433530
                332E38353836382C3330352E3334373635203439342E34363731392C3331322E
                3530333632203438382E39313732342C3331342E36393733204C3438382E3931
                3732342C3331342E36393733205A204D3437362E39393839392C3330332E3539
                30323220433437342E31373837392C3330352E3235363638203437302E363930
                37372C3330352E3339393735203436362E35383831372C3330352E3339393735
                204C3434302E39373438332C3330352E3339393735204C3434302E3937343833
                2C3238352E3636373138204C3436362E393336372C3238352E36363731382043
                3437302E36393037372C3238352E3636373138203437342E343437352C323835
                2E3734363538203437362E39393839392C3238372E323534313620433437392E
                373331342C3238382E3637363837203438312E33363439392C3239312E333937
                3739203438312E33363439392C3239352E313537323520433438312E33363439
                392C3239382E3931363732203437392E373331342C3330312E39343439362034
                37362E39393839392C3330332E3539303232204C3437362E39393839392C3330
                332E3539303232205A204D3636372E33333533392C3330392E31383636204336
                37312E34343036372C3331332E3431373636203637332E36343039352C333138
                2E37353838203637332E36343039352C3332372E383031313220433637332E36
                343039352C3334362E3730313738203636312E37383237382C3335352E353234
                32203634302E35313934382C3335352E35323432204C3539392E34353335332C
                3335352E35323432204C3539392E34353335332C3333362E3638343439204C36
                34302E33353435332C3333362E363834343920433634342E33353333372C3333
                362E3638343439203634372E31383935342C3333362E3135373236203634382E
                393636382C3333342E353039333420433635302E34313638312C3333332E3135
                303231203635312E34353730392C3333312E3137363433203635312E34353730
                392C3332382E373738373520433635312E34353730392C3332362E3231393434
                203635302E33333136372C3332342E3138373338203634382E38383433332C33
                32322E393638363620433634372E32373230312C3332312E3632383037203634
                352E30343737382C3332312E3031383732203634312E33373631392C3332312E
                303138373220433632312E36353836382C3332302E3334383433203539362E39
                3635392C3332312E3632383037203539362E393635392C3239332E3832353531
                20433539362E393635392C3238312E3038323031203630352E30303631352C32
                36372E3636383232203632372E31313031392C3236372E3636383232204C3636
                392E33373837322C3236372E3636383232204C3636392E33373837322C323836
                2E3336373532204C3633302E37303139362C3238362E33363735322043363236
                2E38363830392C3238362E3336373532203632342E33373531322C3238362E35
                31303539203632322E32353436342C3238372E3935343520433631392E393435
                32372C3238392E3337373231203631392E30383835362C3239312E3438383736
                203631392E30383835362C3239342E3237353920433631392E30383835362C32
                39372E3539303238203632312E30343934312C3239392E38343439203632332E
                3730322C3330302E383139383720433632352E39323632342C3330312E353930
                3834203632382E33313534332C3330312E3831363033203633312E393037322C
                3330312E3831363033204C3634332E32353732322C3330322E31323037312043
                3635342E3730332C3330322E3339383839203636322E35353936372C3330342E
                3337303033203636372E33333533392C3330392E31383636204C3636372E3333
                3533392C3330392E31383636205A204D3735312C3238352E3636373138204C37
                31322E35373333352C3238352E363637313820433730382E373336382C323835
                2E3636373138203730362E31383739372C3238352E3831303235203730342E30
                343038382C3238372E323534313620433730312E38313636352C3238382E3637
                363837203730302E39353939352C3239302E3738383433203730302E39353939
                352C3239332E353735353820433730302E39353939352C3239362E3838393934
                203730322E38333833312C3239392E3134343536203730352E35373037312C33
                30302E313139353320433730372E37393439352C3330302E3839303520373130
                2E31383431352C3330312E31313537203731332E363936312C3330312E313135
                37204C3732352E31323332372C3330312E343230333820433733362E36353431
                392C3330312E3730333837203734342E33353132332C3330332E363737363520
                3734392E30343434382C3330382E343931353720433734392E38393835322C33
                30392E3136313836203735302E34313230322C3330392E393134323820373531
                2C3331302E36363637204C3735312C3238352E3636373138204C3735312C3238
                352E3636373138205A222069643D22706174683133222066696C6C3D22234646
                464646462220736B657463683A747970653D224D53536861706547726F757022
                3E3C2F706174683E0A20202020202020203C2F673E0A202020203C2F673E0A3C
                2F7376673E}
              Proportional = True
              Transparent = True
              Animation.FrameCount = 0
              Animation.Position = 0
            end
            object imBankingPaymentDiners: TWSVGImage
              AlignWithMargins = True
              Left = 368
              Top = 0
              Width = 33
              Height = 25
              Margins.Top = 0
              Margins.Right = 20
              Margins.Bottom = 0
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C7376672077696474683D2237353022206865
                696768743D22343731222076696577426F783D22302030203735302034373122
                20786D6C6E733D22687474703A2F2F7777772E77332E6F72672F323030302F73
                7667223E3C7469746C653E64696E6572733C2F7469746C653E3C672066696C6C
                3D226E6F6E65222066696C6C2D72756C653D226576656E6F6464223E3C726563
                742066696C6C3D2223303037394245222077696474683D223735302220686569
                6768743D22343731222072783D223430222F3E3C7061746820643D224D353834
                2E393334203233372E39343763302D39392E3431352D38322E39382D3136382E
                3133332D3137332E3839352D3136382E31682D37382E323433632D39322E3030
                332D2E3033332D3136372E37332036382E3730352D3136372E3733203136382E
                3120302039302E39332037352E373237203136352E3634203136372E37332031
                36352E3230346837382E3234326339302E3931332E343337203137332E383934
                2D37342E323932203137332E3839342D3136352E3230337A222066696C6C3D22
                23464646222F3E3C7061746820643D224D3333332E32382038332E3933632D38
                342E30372E3032372D3135322E3139342036382E3330382D3135322E32313420
                3135322E35382E30322038342E3235382036382E313434203135322E35333320
                3135322E323134203135322E35362038342E30392D2E303237203135322E3232
                382D36382E333032203135322E32342D3135322E35362D2E3031322D38342E32
                37322D36382E31352D3135322E3535332D3135322E32342D3135322E35387A22
                2066696C6C3D2223303037394245222F3E3C7061746820643D224D3233372E30
                3636203233362E303938632E30382D34312E31382032352E3734362D37362E32
                39362036312E39342D39302E3235763138302E3438632D33362E3139342D3133
                2E3934372D36312E38362D34392E3034342D36312E39342D39302E32337A6D31
                33312039302E323735563134352E3834376333362E3230372031332E39322036
                312E3931342034392E3035372036312E39382039302E3235372D2E3036362034
                312E3231322D32352E3737332037362E3332322D36312E39382039302E32377A
                222066696C6C3D2223464646222F3E3C2F673E3C2F7376673E}
              Proportional = True
              Transparent = True
              Animation.FrameCount = 0
              Animation.Position = 0
            end
            object imBankingPaymentAsterisk: TWSVGImage
              AlignWithMargins = True
              Left = 144
              Top = 3
              Width = 7
              Height = 7
              Margins.Left = 0
              Margins.Right = 20
              Margins.Bottom = 15
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                223F3E0A3C212D2D2043726561746564207769746820496E6B73636170652028
                687474703A2F2F7777772E696E6B73636170652E6F72672F29202D2D3E0A0A3C
                7376670A202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F7267
                2F64632F656C656D656E74732F312E312F220A202020786D6C6E733A63633D22
                687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A
                202020786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F
                313939392F30322F32322D7264662D73796E7461782D6E7323220A202020786D
                6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F323030302F
                737667220A202020786D6C6E733D22687474703A2F2F7777772E77332E6F7267
                2F323030302F737667220A202020786D6C6E733A736F6469706F64693D226874
                74703A2F2F736F6469706F64692E736F75726365666F7267652E6E65742F4454
                442F736F6469706F64692D302E647464220A202020786D6C6E733A696E6B7363
                6170653D22687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D
                657370616365732F696E6B7363617065220A20202077696474683D223332220A
                2020206865696768743D223332220A20202076696577426F783D223020302033
                322033322E303030303031220A20202069643D2273766732220A202020766572
                73696F6E3D22312E31220A202020696E6B73636170653A76657273696F6E3D22
                302E393120723133373235220A202020736F6469706F64693A646F636E616D65
                3D22617374657269736B2E737667223E0A20203C646566730A20202020206964
                3D22646566733422202F3E0A20203C736F6469706F64693A6E616D6564766965
                770A202020202069643D2262617365220A202020202070616765636F6C6F723D
                2223666666666666220A2020202020626F72646572636F6C6F723D2223363636
                363636220A2020202020626F726465726F7061636974793D22312E30220A2020
                202020696E6B73636170653A706167656F7061636974793D22302E30220A2020
                202020696E6B73636170653A70616765736861646F773D2232220A2020202020
                696E6B73636170653A7A6F6F6D3D2232322E34220A2020202020696E6B736361
                70653A63783D2231362E373333383639220A2020202020696E6B73636170653A
                63793D2231352E373436313036220A2020202020696E6B73636170653A646F63
                756D656E742D756E6974733D227078220A2020202020696E6B73636170653A63
                757272656E742D6C617965723D226C6179657231220A202020202073686F7767
                7269643D2266616C7365220A2020202020756E6974733D227078220A20202020
                20696E6B73636170653A73686F7770616765736861646F773D2266616C736522
                0A2020202020696E6B73636170653A77696E646F772D77696474683D22313932
                30220A2020202020696E6B73636170653A77696E646F772D6865696768743D22
                31303238220A2020202020696E6B73636170653A77696E646F772D783D222D38
                220A2020202020696E6B73636170653A77696E646F772D793D222D38220A2020
                202020696E6B73636170653A77696E646F772D6D6178696D697A65643D223122
                202F3E0A20203C6D657461646174610A202020202069643D226D657461646174
                6137223E0A202020203C7264663A5244463E0A2020202020203C63633A576F72
                6B0A2020202020202020207264663A61626F75743D22223E0A20202020202020
                203C64633A666F726D61743E696D6167652F7376672B786D6C3C2F64633A666F
                726D61743E0A20202020202020203C64633A747970650A202020202020202020
                20207264663A7265736F757263653D22687474703A2F2F7075726C2E6F72672F
                64632F64636D69747970652F5374696C6C496D61676522202F3E0A2020202020
                2020203C64633A7469746C653E3C2F64633A7469746C653E0A2020202020203C
                2F63633A576F726B3E0A202020203C2F7264663A5244463E0A20203C2F6D6574
                61646174613E0A20203C670A2020202020696E6B73636170653A6C6162656C3D
                224C617965722031220A2020202020696E6B73636170653A67726F75706D6F64
                653D226C61796572220A202020202069643D226C6179657231220A2020202020
                7472616E73666F726D3D227472616E736C61746528302C2D313032302E333632
                3229223E0A202020203C706174680A20202020202020736F6469706F64693A74
                7970653D2273746172220A202020202020207374796C653D226F706163697479
                3A313B66696C6C3A236537313931323B66696C6C2D6F7061636974793A313B66
                696C6C2D72756C653A6E6F6E7A65726F3B7374726F6B653A236633303630303B
                7374726F6B652D77696474683A302E34363739353438373B7374726F6B652D6C
                696E656361703A726F756E643B7374726F6B652D6D697465726C696D69743A34
                3B7374726F6B652D6461736861727261793A6E6F6E653B7374726F6B652D6461
                73686F66667365743A303B7374726F6B652D6F7061636974793A31220A202020
                2020202069643D227061746834313437220A20202020202020736F6469706F64
                693A73696465733D2236220A20202020202020736F6469706F64693A63783D22
                3136220A20202020202020736F6469706F64693A63793D22313033362E333632
                32220A20202020202020736F6469706F64693A72313D2231352E323238323634
                220A20202020202020736F6469706F64693A72323D22372E3631343133313922
                0A20202020202020736F6469706F64693A617267313D22302E35323335393837
                38220A20202020202020736F6469706F64693A617267323D22312E3034373139
                3736220A20202020202020696E6B73636170653A666C617473696465643D2266
                616C7365220A20202020202020696E6B73636170653A726F756E6465643D2230
                220A20202020202020696E6B73636170653A72616E646F6D697A65643D223022
                0A20202020202020643D226D2032392E3138383036332C313034332E39373633
                202D392E3338303939372C2D312E30323031202D332E3830373036362C382E36
                333432202D332E3830373036362C2D382E36333432202D392E33383039393734
                2C312E3032303120352E353733393331352C2D372E36313431202D352E353733
                393331342C2D372E3631343120392E333830393937332C312E3032303120332E
                3830373036362C2D382E3633343320332E3830373036362C382E363334332039
                2E3338303939372C2D312E30323031202D352E3537333933312C372E36313431
                207A220A20202020202020696E6B73636170653A7472616E73666F726D2D6365
                6E7465722D793D222D332E3830373035333822202F3E0A20203C2F673E0A3C2F
                7376673E0A}
              Proportional = True
              Transparent = True
              Animation.FrameCount = 0
              Animation.Position = 0
            end
            object rbBankingPaymentMethodCreditCard: TRadioButton
              AlignWithMargins = True
              Left = 174
              Top = 3
              Width = 80
              Height = 19
              Align = alLeft
              Caption = 'Credit card'
              Checked = True
              Color = clWhite
              ParentColor = False
              TabOrder = 0
              TabStop = True
            end
            object rbBankingPaymentMethodPayPal: TRadioButton
              AlignWithMargins = True
              Left = 424
              Top = 3
              Width = 61
              Height = 19
              Align = alLeft
              Caption = 'PayPal'
              Color = clWhite
              ParentColor = False
              TabOrder = 1
            end
          end
        end
        object paBankingPersonalInfo: TPanel
          Left = 0
          Top = 448
          Width = 644
          Height = 228
          Align = alTop
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 3
          object laBankingPersonalInfoTitle: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 149
            Height = 19
            Align = alTop
            Caption = 'Personal Information'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = 526344
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object laBankingPersonalInfoDesc: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 28
            Width = 187
            Height = 13
            Align = alTop
            Caption = 'Enter your personal information below.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = 526344
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object paBankingPersonalInfoClientName: TPanel
            Left = 0
            Top = 44
            Width = 644
            Height = 60
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 0
            object paBankingPersonalInfoFirstName: TPanel
              AlignWithMargins = True
              Left = 204
              Top = 0
              Width = 184
              Height = 60
              Margins.Left = 20
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 0
              object edBankingPersonalInfoFirstName: TEdit
                Left = 0
                Top = 23
                Width = 184
                Height = 21
                Align = alTop
                TabOrder = 0
              end
              object paBankingPersonalInfoFirstNameTitle: TPanel
                AlignWithMargins = True
                Left = 3
                Top = 7
                Width = 178
                Height = 13
                Margins.Top = 7
                Align = alTop
                BevelOuter = bvNone
                Color = clWhite
                ParentBackground = False
                TabOrder = 1
                object laBankingPersonalInfoFirstName: TLabel
                  Left = 0
                  Top = 0
                  Width = 49
                  Height = 13
                  Margins.Top = 15
                  Align = alLeft
                  Caption = 'Last name'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = 526344
                  Font.Height = -11
                  Font.Name = 'Tahoma'
                  Font.Style = []
                  ParentFont = False
                end
                object imBankingPersonalInfoFirstName: TWSVGImage
                  AlignWithMargins = True
                  Left = 49
                  Top = 2
                  Width = 7
                  Height = 7
                  Margins.Left = 0
                  Margins.Top = 2
                  Margins.Bottom = 4
                  Align = alLeft
                  Center = True
                  Picture.Data = {
                    0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                    20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                    223F3E0A3C212D2D2043726561746564207769746820496E6B73636170652028
                    687474703A2F2F7777772E696E6B73636170652E6F72672F29202D2D3E0A0A3C
                    7376670A202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F7267
                    2F64632F656C656D656E74732F312E312F220A202020786D6C6E733A63633D22
                    687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A
                    202020786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F
                    313939392F30322F32322D7264662D73796E7461782D6E7323220A202020786D
                    6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F323030302F
                    737667220A202020786D6C6E733D22687474703A2F2F7777772E77332E6F7267
                    2F323030302F737667220A202020786D6C6E733A736F6469706F64693D226874
                    74703A2F2F736F6469706F64692E736F75726365666F7267652E6E65742F4454
                    442F736F6469706F64692D302E647464220A202020786D6C6E733A696E6B7363
                    6170653D22687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D
                    657370616365732F696E6B7363617065220A20202077696474683D223332220A
                    2020206865696768743D223332220A20202076696577426F783D223020302033
                    322033322E303030303031220A20202069643D2273766732220A202020766572
                    73696F6E3D22312E31220A202020696E6B73636170653A76657273696F6E3D22
                    302E393120723133373235220A202020736F6469706F64693A646F636E616D65
                    3D22617374657269736B2E737667223E0A20203C646566730A20202020206964
                    3D22646566733422202F3E0A20203C736F6469706F64693A6E616D6564766965
                    770A202020202069643D2262617365220A202020202070616765636F6C6F723D
                    2223666666666666220A2020202020626F72646572636F6C6F723D2223363636
                    363636220A2020202020626F726465726F7061636974793D22312E30220A2020
                    202020696E6B73636170653A706167656F7061636974793D22302E30220A2020
                    202020696E6B73636170653A70616765736861646F773D2232220A2020202020
                    696E6B73636170653A7A6F6F6D3D2232322E34220A2020202020696E6B736361
                    70653A63783D2231362E373333383639220A2020202020696E6B73636170653A
                    63793D2231352E373436313036220A2020202020696E6B73636170653A646F63
                    756D656E742D756E6974733D227078220A2020202020696E6B73636170653A63
                    757272656E742D6C617965723D226C6179657231220A202020202073686F7767
                    7269643D2266616C7365220A2020202020756E6974733D227078220A20202020
                    20696E6B73636170653A73686F7770616765736861646F773D2266616C736522
                    0A2020202020696E6B73636170653A77696E646F772D77696474683D22313932
                    30220A2020202020696E6B73636170653A77696E646F772D6865696768743D22
                    31303238220A2020202020696E6B73636170653A77696E646F772D783D222D38
                    220A2020202020696E6B73636170653A77696E646F772D793D222D38220A2020
                    202020696E6B73636170653A77696E646F772D6D6178696D697A65643D223122
                    202F3E0A20203C6D657461646174610A202020202069643D226D657461646174
                    6137223E0A202020203C7264663A5244463E0A2020202020203C63633A576F72
                    6B0A2020202020202020207264663A61626F75743D22223E0A20202020202020
                    203C64633A666F726D61743E696D6167652F7376672B786D6C3C2F64633A666F
                    726D61743E0A20202020202020203C64633A747970650A202020202020202020
                    20207264663A7265736F757263653D22687474703A2F2F7075726C2E6F72672F
                    64632F64636D69747970652F5374696C6C496D61676522202F3E0A2020202020
                    2020203C64633A7469746C653E3C2F64633A7469746C653E0A2020202020203C
                    2F63633A576F726B3E0A202020203C2F7264663A5244463E0A20203C2F6D6574
                    61646174613E0A20203C670A2020202020696E6B73636170653A6C6162656C3D
                    224C617965722031220A2020202020696E6B73636170653A67726F75706D6F64
                    653D226C61796572220A202020202069643D226C6179657231220A2020202020
                    7472616E73666F726D3D227472616E736C61746528302C2D313032302E333632
                    3229223E0A202020203C706174680A20202020202020736F6469706F64693A74
                    7970653D2273746172220A202020202020207374796C653D226F706163697479
                    3A313B66696C6C3A236537313931323B66696C6C2D6F7061636974793A313B66
                    696C6C2D72756C653A6E6F6E7A65726F3B7374726F6B653A236633303630303B
                    7374726F6B652D77696474683A302E34363739353438373B7374726F6B652D6C
                    696E656361703A726F756E643B7374726F6B652D6D697465726C696D69743A34
                    3B7374726F6B652D6461736861727261793A6E6F6E653B7374726F6B652D6461
                    73686F66667365743A303B7374726F6B652D6F7061636974793A31220A202020
                    2020202069643D227061746834313437220A20202020202020736F6469706F64
                    693A73696465733D2236220A20202020202020736F6469706F64693A63783D22
                    3136220A20202020202020736F6469706F64693A63793D22313033362E333632
                    32220A20202020202020736F6469706F64693A72313D2231352E323238323634
                    220A20202020202020736F6469706F64693A72323D22372E3631343133313922
                    0A20202020202020736F6469706F64693A617267313D22302E35323335393837
                    38220A20202020202020736F6469706F64693A617267323D22312E3034373139
                    3736220A20202020202020696E6B73636170653A666C617473696465643D2266
                    616C7365220A20202020202020696E6B73636170653A726F756E6465643D2230
                    220A20202020202020696E6B73636170653A72616E646F6D697A65643D223022
                    0A20202020202020643D226D2032392E3138383036332C313034332E39373633
                    202D392E3338303939372C2D312E30323031202D332E3830373036362C382E36
                    333432202D332E3830373036362C2D382E36333432202D392E33383039393734
                    2C312E3032303120352E353733393331352C2D372E36313431202D352E353733
                    393331342C2D372E3631343120392E333830393937332C312E3032303120332E
                    3830373036362C2D382E3633343320332E3830373036362C382E363334332039
                    2E3338303939372C2D312E30323031202D352E3537333933312C372E36313431
                    207A220A20202020202020696E6B73636170653A7472616E73666F726D2D6365
                    6E7465722D793D222D332E3830373035333822202F3E0A20203C2F673E0A3C2F
                    7376673E0A}
                  Proportional = True
                  Transparent = True
                  Animation.FrameCount = 0
                  Animation.Position = 0
                end
              end
            end
            object paBankingPersonalInfoLastName: TPanel
              Left = 0
              Top = 0
              Width = 184
              Height = 60
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 1
              object edBankingPersonalInfoLastName: TEdit
                Left = 0
                Top = 23
                Width = 184
                Height = 21
                Align = alTop
                TabOrder = 0
              end
              object paBankingPersonalInfoLastNameTitla: TPanel
                AlignWithMargins = True
                Left = 3
                Top = 7
                Width = 178
                Height = 13
                Margins.Top = 7
                Align = alTop
                BevelOuter = bvNone
                Color = clWhite
                ParentBackground = False
                TabOrder = 1
                object laBankingPersonalInfoLastName: TLabel
                  Left = 0
                  Top = 0
                  Width = 50
                  Height = 13
                  Margins.Top = 15
                  Align = alLeft
                  Caption = 'First name'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = 526344
                  Font.Height = -11
                  Font.Name = 'Tahoma'
                  Font.Style = []
                  ParentFont = False
                end
                object imBankingPersonalInfoLastName: TWSVGImage
                  AlignWithMargins = True
                  Left = 50
                  Top = 2
                  Width = 7
                  Height = 7
                  Margins.Left = 0
                  Margins.Top = 2
                  Margins.Bottom = 4
                  Align = alLeft
                  Center = True
                  Picture.Data = {
                    0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                    20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                    223F3E0A3C212D2D2043726561746564207769746820496E6B73636170652028
                    687474703A2F2F7777772E696E6B73636170652E6F72672F29202D2D3E0A0A3C
                    7376670A202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F7267
                    2F64632F656C656D656E74732F312E312F220A202020786D6C6E733A63633D22
                    687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A
                    202020786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F
                    313939392F30322F32322D7264662D73796E7461782D6E7323220A202020786D
                    6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F323030302F
                    737667220A202020786D6C6E733D22687474703A2F2F7777772E77332E6F7267
                    2F323030302F737667220A202020786D6C6E733A736F6469706F64693D226874
                    74703A2F2F736F6469706F64692E736F75726365666F7267652E6E65742F4454
                    442F736F6469706F64692D302E647464220A202020786D6C6E733A696E6B7363
                    6170653D22687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D
                    657370616365732F696E6B7363617065220A20202077696474683D223332220A
                    2020206865696768743D223332220A20202076696577426F783D223020302033
                    322033322E303030303031220A20202069643D2273766732220A202020766572
                    73696F6E3D22312E31220A202020696E6B73636170653A76657273696F6E3D22
                    302E393120723133373235220A202020736F6469706F64693A646F636E616D65
                    3D22617374657269736B2E737667223E0A20203C646566730A20202020206964
                    3D22646566733422202F3E0A20203C736F6469706F64693A6E616D6564766965
                    770A202020202069643D2262617365220A202020202070616765636F6C6F723D
                    2223666666666666220A2020202020626F72646572636F6C6F723D2223363636
                    363636220A2020202020626F726465726F7061636974793D22312E30220A2020
                    202020696E6B73636170653A706167656F7061636974793D22302E30220A2020
                    202020696E6B73636170653A70616765736861646F773D2232220A2020202020
                    696E6B73636170653A7A6F6F6D3D2232322E34220A2020202020696E6B736361
                    70653A63783D2231362E373333383639220A2020202020696E6B73636170653A
                    63793D2231352E373436313036220A2020202020696E6B73636170653A646F63
                    756D656E742D756E6974733D227078220A2020202020696E6B73636170653A63
                    757272656E742D6C617965723D226C6179657231220A202020202073686F7767
                    7269643D2266616C7365220A2020202020756E6974733D227078220A20202020
                    20696E6B73636170653A73686F7770616765736861646F773D2266616C736522
                    0A2020202020696E6B73636170653A77696E646F772D77696474683D22313932
                    30220A2020202020696E6B73636170653A77696E646F772D6865696768743D22
                    31303238220A2020202020696E6B73636170653A77696E646F772D783D222D38
                    220A2020202020696E6B73636170653A77696E646F772D793D222D38220A2020
                    202020696E6B73636170653A77696E646F772D6D6178696D697A65643D223122
                    202F3E0A20203C6D657461646174610A202020202069643D226D657461646174
                    6137223E0A202020203C7264663A5244463E0A2020202020203C63633A576F72
                    6B0A2020202020202020207264663A61626F75743D22223E0A20202020202020
                    203C64633A666F726D61743E696D6167652F7376672B786D6C3C2F64633A666F
                    726D61743E0A20202020202020203C64633A747970650A202020202020202020
                    20207264663A7265736F757263653D22687474703A2F2F7075726C2E6F72672F
                    64632F64636D69747970652F5374696C6C496D61676522202F3E0A2020202020
                    2020203C64633A7469746C653E3C2F64633A7469746C653E0A2020202020203C
                    2F63633A576F726B3E0A202020203C2F7264663A5244463E0A20203C2F6D6574
                    61646174613E0A20203C670A2020202020696E6B73636170653A6C6162656C3D
                    224C617965722031220A2020202020696E6B73636170653A67726F75706D6F64
                    653D226C61796572220A202020202069643D226C6179657231220A2020202020
                    7472616E73666F726D3D227472616E736C61746528302C2D313032302E333632
                    3229223E0A202020203C706174680A20202020202020736F6469706F64693A74
                    7970653D2273746172220A202020202020207374796C653D226F706163697479
                    3A313B66696C6C3A236537313931323B66696C6C2D6F7061636974793A313B66
                    696C6C2D72756C653A6E6F6E7A65726F3B7374726F6B653A236633303630303B
                    7374726F6B652D77696474683A302E34363739353438373B7374726F6B652D6C
                    696E656361703A726F756E643B7374726F6B652D6D697465726C696D69743A34
                    3B7374726F6B652D6461736861727261793A6E6F6E653B7374726F6B652D6461
                    73686F66667365743A303B7374726F6B652D6F7061636974793A31220A202020
                    2020202069643D227061746834313437220A20202020202020736F6469706F64
                    693A73696465733D2236220A20202020202020736F6469706F64693A63783D22
                    3136220A20202020202020736F6469706F64693A63793D22313033362E333632
                    32220A20202020202020736F6469706F64693A72313D2231352E323238323634
                    220A20202020202020736F6469706F64693A72323D22372E3631343133313922
                    0A20202020202020736F6469706F64693A617267313D22302E35323335393837
                    38220A20202020202020736F6469706F64693A617267323D22312E3034373139
                    3736220A20202020202020696E6B73636170653A666C617473696465643D2266
                    616C7365220A20202020202020696E6B73636170653A726F756E6465643D2230
                    220A20202020202020696E6B73636170653A72616E646F6D697A65643D223022
                    0A20202020202020643D226D2032392E3138383036332C313034332E39373633
                    202D392E3338303939372C2D312E30323031202D332E3830373036362C382E36
                    333432202D332E3830373036362C2D382E36333432202D392E33383039393734
                    2C312E3032303120352E353733393331352C2D372E36313431202D352E353733
                    393331342C2D372E3631343120392E333830393937332C312E3032303120332E
                    3830373036362C2D382E3633343320332E3830373036362C382E363334332039
                    2E3338303939372C2D312E30323031202D352E3537333933312C372E36313431
                    207A220A20202020202020696E6B73636170653A7472616E73666F726D2D6365
                    6E7465722D793D222D332E3830373035333822202F3E0A20203C2F673E0A3C2F
                    7376673E0A}
                  Proportional = True
                  Transparent = True
                  Animation.FrameCount = 0
                  Animation.Position = 0
                end
              end
            end
            object paBankingPersonalInfoGender: TPanel
              AlignWithMargins = True
              Left = 408
              Top = 0
              Width = 129
              Height = 60
              Margins.Left = 20
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 2
              object laBankingPersonalInfoGenderTitle: TLabel
                AlignWithMargins = True
                Left = 3
                Top = 7
                Width = 35
                Height = 13
                Margins.Top = 7
                Align = alTop
                Caption = 'Gender'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 526344
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
              end
              object rbBankingPersonalInfoGenderMale: TRadioButton
                Left = 0
                Top = 23
                Width = 129
                Height = 17
                Align = alTop
                Caption = 'Male'
                TabOrder = 0
              end
              object rbBankingPersonalInfoGenderFemale: TRadioButton
                Left = 0
                Top = 40
                Width = 129
                Height = 17
                Align = alTop
                Caption = 'Female'
                TabOrder = 1
              end
            end
          end
          object paBankingPersonalInfoCreditCard: TPanel
            Left = 0
            Top = 104
            Width = 644
            Height = 60
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 1
            object paBankingPersonalInfoCreditCardInfo: TPanel
              Left = 0
              Top = 0
              Width = 388
              Height = 60
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 0
              object edBankingPersonalInfoCreditCard: TEdit
                Left = 0
                Top = 23
                Width = 388
                Height = 21
                Align = alTop
                TabOrder = 0
              end
              object paBankingPersonalInfoCreditCardTitle: TPanel
                AlignWithMargins = True
                Left = 3
                Top = 7
                Width = 382
                Height = 13
                Margins.Top = 7
                Align = alTop
                BevelOuter = bvNone
                Color = clWhite
                ParentBackground = False
                TabOrder = 1
                object laBankingPersonalInfoCreditCardTitle: TLabel
                  Left = 0
                  Top = 0
                  Width = 53
                  Height = 13
                  Margins.Top = 15
                  Align = alLeft
                  Caption = 'Credit card'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = 526344
                  Font.Height = -11
                  Font.Name = 'Tahoma'
                  Font.Style = []
                  ParentFont = False
                end
                object imBankingPersonalInfoCreditCardTitleImage: TWSVGImage
                  AlignWithMargins = True
                  Left = 53
                  Top = 2
                  Width = 7
                  Height = 7
                  Margins.Left = 0
                  Margins.Top = 2
                  Margins.Bottom = 4
                  Align = alLeft
                  Center = True
                  Picture.Data = {
                    0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                    20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                    223F3E0A3C212D2D2043726561746564207769746820496E6B73636170652028
                    687474703A2F2F7777772E696E6B73636170652E6F72672F29202D2D3E0A0A3C
                    7376670A202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F7267
                    2F64632F656C656D656E74732F312E312F220A202020786D6C6E733A63633D22
                    687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A
                    202020786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F
                    313939392F30322F32322D7264662D73796E7461782D6E7323220A202020786D
                    6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F323030302F
                    737667220A202020786D6C6E733D22687474703A2F2F7777772E77332E6F7267
                    2F323030302F737667220A202020786D6C6E733A736F6469706F64693D226874
                    74703A2F2F736F6469706F64692E736F75726365666F7267652E6E65742F4454
                    442F736F6469706F64692D302E647464220A202020786D6C6E733A696E6B7363
                    6170653D22687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D
                    657370616365732F696E6B7363617065220A20202077696474683D223332220A
                    2020206865696768743D223332220A20202076696577426F783D223020302033
                    322033322E303030303031220A20202069643D2273766732220A202020766572
                    73696F6E3D22312E31220A202020696E6B73636170653A76657273696F6E3D22
                    302E393120723133373235220A202020736F6469706F64693A646F636E616D65
                    3D22617374657269736B2E737667223E0A20203C646566730A20202020206964
                    3D22646566733422202F3E0A20203C736F6469706F64693A6E616D6564766965
                    770A202020202069643D2262617365220A202020202070616765636F6C6F723D
                    2223666666666666220A2020202020626F72646572636F6C6F723D2223363636
                    363636220A2020202020626F726465726F7061636974793D22312E30220A2020
                    202020696E6B73636170653A706167656F7061636974793D22302E30220A2020
                    202020696E6B73636170653A70616765736861646F773D2232220A2020202020
                    696E6B73636170653A7A6F6F6D3D2232322E34220A2020202020696E6B736361
                    70653A63783D2231362E373333383639220A2020202020696E6B73636170653A
                    63793D2231352E373436313036220A2020202020696E6B73636170653A646F63
                    756D656E742D756E6974733D227078220A2020202020696E6B73636170653A63
                    757272656E742D6C617965723D226C6179657231220A202020202073686F7767
                    7269643D2266616C7365220A2020202020756E6974733D227078220A20202020
                    20696E6B73636170653A73686F7770616765736861646F773D2266616C736522
                    0A2020202020696E6B73636170653A77696E646F772D77696474683D22313932
                    30220A2020202020696E6B73636170653A77696E646F772D6865696768743D22
                    31303238220A2020202020696E6B73636170653A77696E646F772D783D222D38
                    220A2020202020696E6B73636170653A77696E646F772D793D222D38220A2020
                    202020696E6B73636170653A77696E646F772D6D6178696D697A65643D223122
                    202F3E0A20203C6D657461646174610A202020202069643D226D657461646174
                    6137223E0A202020203C7264663A5244463E0A2020202020203C63633A576F72
                    6B0A2020202020202020207264663A61626F75743D22223E0A20202020202020
                    203C64633A666F726D61743E696D6167652F7376672B786D6C3C2F64633A666F
                    726D61743E0A20202020202020203C64633A747970650A202020202020202020
                    20207264663A7265736F757263653D22687474703A2F2F7075726C2E6F72672F
                    64632F64636D69747970652F5374696C6C496D61676522202F3E0A2020202020
                    2020203C64633A7469746C653E3C2F64633A7469746C653E0A2020202020203C
                    2F63633A576F726B3E0A202020203C2F7264663A5244463E0A20203C2F6D6574
                    61646174613E0A20203C670A2020202020696E6B73636170653A6C6162656C3D
                    224C617965722031220A2020202020696E6B73636170653A67726F75706D6F64
                    653D226C61796572220A202020202069643D226C6179657231220A2020202020
                    7472616E73666F726D3D227472616E736C61746528302C2D313032302E333632
                    3229223E0A202020203C706174680A20202020202020736F6469706F64693A74
                    7970653D2273746172220A202020202020207374796C653D226F706163697479
                    3A313B66696C6C3A236537313931323B66696C6C2D6F7061636974793A313B66
                    696C6C2D72756C653A6E6F6E7A65726F3B7374726F6B653A236633303630303B
                    7374726F6B652D77696474683A302E34363739353438373B7374726F6B652D6C
                    696E656361703A726F756E643B7374726F6B652D6D697465726C696D69743A34
                    3B7374726F6B652D6461736861727261793A6E6F6E653B7374726F6B652D6461
                    73686F66667365743A303B7374726F6B652D6F7061636974793A31220A202020
                    2020202069643D227061746834313437220A20202020202020736F6469706F64
                    693A73696465733D2236220A20202020202020736F6469706F64693A63783D22
                    3136220A20202020202020736F6469706F64693A63793D22313033362E333632
                    32220A20202020202020736F6469706F64693A72313D2231352E323238323634
                    220A20202020202020736F6469706F64693A72323D22372E3631343133313922
                    0A20202020202020736F6469706F64693A617267313D22302E35323335393837
                    38220A20202020202020736F6469706F64693A617267323D22312E3034373139
                    3736220A20202020202020696E6B73636170653A666C617473696465643D2266
                    616C7365220A20202020202020696E6B73636170653A726F756E6465643D2230
                    220A20202020202020696E6B73636170653A72616E646F6D697A65643D223022
                    0A20202020202020643D226D2032392E3138383036332C313034332E39373633
                    202D392E3338303939372C2D312E30323031202D332E3830373036362C382E36
                    333432202D332E3830373036362C2D382E36333432202D392E33383039393734
                    2C312E3032303120352E353733393331352C2D372E36313431202D352E353733
                    393331342C2D372E3631343120392E333830393937332C312E3032303120332E
                    3830373036362C2D382E3633343320332E3830373036362C382E363334332039
                    2E3338303939372C2D312E30323031202D352E3537333933312C372E36313431
                    207A220A20202020202020696E6B73636170653A7472616E73666F726D2D6365
                    6E7465722D793D222D332E3830373035333822202F3E0A20203C2F673E0A3C2F
                    7376673E0A}
                  Proportional = True
                  Transparent = True
                  Animation.FrameCount = 0
                  Animation.Position = 0
                end
              end
            end
          end
          object paBankingPersonalInfoCreditCardData: TPanel
            Left = 0
            Top = 164
            Width = 644
            Height = 55
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 2
            object paBankingPersonalInfoCreditCardSecurity: TPanel
              AlignWithMargins = True
              Left = 204
              Top = 0
              Width = 187
              Height = 55
              Margins.Left = 20
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 0
              object paBankingPersonalInfoCreditCardSecurityTitle: TPanel
                AlignWithMargins = True
                Left = 3
                Top = 7
                Width = 181
                Height = 13
                Margins.Top = 7
                Align = alTop
                BevelOuter = bvNone
                Color = clWhite
                ParentBackground = False
                TabOrder = 0
                object laBankingPersonalInfoCreditCardSecurity: TLabel
                  Left = 0
                  Top = 0
                  Width = 65
                  Height = 13
                  Margins.Top = 15
                  Align = alLeft
                  Caption = 'Security code'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = 526344
                  Font.Height = -11
                  Font.Name = 'Tahoma'
                  Font.Style = []
                  ParentFont = False
                end
                object imBankingPersonalInfoCreditCardSecurityTitle: TWSVGImage
                  AlignWithMargins = True
                  Left = 65
                  Top = 2
                  Width = 7
                  Height = 7
                  Margins.Left = 0
                  Margins.Top = 2
                  Margins.Bottom = 4
                  Align = alLeft
                  Center = True
                  Picture.Data = {
                    0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                    20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                    223F3E0A3C212D2D2043726561746564207769746820496E6B73636170652028
                    687474703A2F2F7777772E696E6B73636170652E6F72672F29202D2D3E0A0A3C
                    7376670A202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F7267
                    2F64632F656C656D656E74732F312E312F220A202020786D6C6E733A63633D22
                    687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A
                    202020786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F
                    313939392F30322F32322D7264662D73796E7461782D6E7323220A202020786D
                    6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F323030302F
                    737667220A202020786D6C6E733D22687474703A2F2F7777772E77332E6F7267
                    2F323030302F737667220A202020786D6C6E733A736F6469706F64693D226874
                    74703A2F2F736F6469706F64692E736F75726365666F7267652E6E65742F4454
                    442F736F6469706F64692D302E647464220A202020786D6C6E733A696E6B7363
                    6170653D22687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D
                    657370616365732F696E6B7363617065220A20202077696474683D223332220A
                    2020206865696768743D223332220A20202076696577426F783D223020302033
                    322033322E303030303031220A20202069643D2273766732220A202020766572
                    73696F6E3D22312E31220A202020696E6B73636170653A76657273696F6E3D22
                    302E393120723133373235220A202020736F6469706F64693A646F636E616D65
                    3D22617374657269736B2E737667223E0A20203C646566730A20202020206964
                    3D22646566733422202F3E0A20203C736F6469706F64693A6E616D6564766965
                    770A202020202069643D2262617365220A202020202070616765636F6C6F723D
                    2223666666666666220A2020202020626F72646572636F6C6F723D2223363636
                    363636220A2020202020626F726465726F7061636974793D22312E30220A2020
                    202020696E6B73636170653A706167656F7061636974793D22302E30220A2020
                    202020696E6B73636170653A70616765736861646F773D2232220A2020202020
                    696E6B73636170653A7A6F6F6D3D2232322E34220A2020202020696E6B736361
                    70653A63783D2231362E373333383639220A2020202020696E6B73636170653A
                    63793D2231352E373436313036220A2020202020696E6B73636170653A646F63
                    756D656E742D756E6974733D227078220A2020202020696E6B73636170653A63
                    757272656E742D6C617965723D226C6179657231220A202020202073686F7767
                    7269643D2266616C7365220A2020202020756E6974733D227078220A20202020
                    20696E6B73636170653A73686F7770616765736861646F773D2266616C736522
                    0A2020202020696E6B73636170653A77696E646F772D77696474683D22313932
                    30220A2020202020696E6B73636170653A77696E646F772D6865696768743D22
                    31303238220A2020202020696E6B73636170653A77696E646F772D783D222D38
                    220A2020202020696E6B73636170653A77696E646F772D793D222D38220A2020
                    202020696E6B73636170653A77696E646F772D6D6178696D697A65643D223122
                    202F3E0A20203C6D657461646174610A202020202069643D226D657461646174
                    6137223E0A202020203C7264663A5244463E0A2020202020203C63633A576F72
                    6B0A2020202020202020207264663A61626F75743D22223E0A20202020202020
                    203C64633A666F726D61743E696D6167652F7376672B786D6C3C2F64633A666F
                    726D61743E0A20202020202020203C64633A747970650A202020202020202020
                    20207264663A7265736F757263653D22687474703A2F2F7075726C2E6F72672F
                    64632F64636D69747970652F5374696C6C496D61676522202F3E0A2020202020
                    2020203C64633A7469746C653E3C2F64633A7469746C653E0A2020202020203C
                    2F63633A576F726B3E0A202020203C2F7264663A5244463E0A20203C2F6D6574
                    61646174613E0A20203C670A2020202020696E6B73636170653A6C6162656C3D
                    224C617965722031220A2020202020696E6B73636170653A67726F75706D6F64
                    653D226C61796572220A202020202069643D226C6179657231220A2020202020
                    7472616E73666F726D3D227472616E736C61746528302C2D313032302E333632
                    3229223E0A202020203C706174680A20202020202020736F6469706F64693A74
                    7970653D2273746172220A202020202020207374796C653D226F706163697479
                    3A313B66696C6C3A236537313931323B66696C6C2D6F7061636974793A313B66
                    696C6C2D72756C653A6E6F6E7A65726F3B7374726F6B653A236633303630303B
                    7374726F6B652D77696474683A302E34363739353438373B7374726F6B652D6C
                    696E656361703A726F756E643B7374726F6B652D6D697465726C696D69743A34
                    3B7374726F6B652D6461736861727261793A6E6F6E653B7374726F6B652D6461
                    73686F66667365743A303B7374726F6B652D6F7061636974793A31220A202020
                    2020202069643D227061746834313437220A20202020202020736F6469706F64
                    693A73696465733D2236220A20202020202020736F6469706F64693A63783D22
                    3136220A20202020202020736F6469706F64693A63793D22313033362E333632
                    32220A20202020202020736F6469706F64693A72313D2231352E323238323634
                    220A20202020202020736F6469706F64693A72323D22372E3631343133313922
                    0A20202020202020736F6469706F64693A617267313D22302E35323335393837
                    38220A20202020202020736F6469706F64693A617267323D22312E3034373139
                    3736220A20202020202020696E6B73636170653A666C617473696465643D2266
                    616C7365220A20202020202020696E6B73636170653A726F756E6465643D2230
                    220A20202020202020696E6B73636170653A72616E646F6D697A65643D223022
                    0A20202020202020643D226D2032392E3138383036332C313034332E39373633
                    202D392E3338303939372C2D312E30323031202D332E3830373036362C382E36
                    333432202D332E3830373036362C2D382E36333432202D392E33383039393734
                    2C312E3032303120352E353733393331352C2D372E36313431202D352E353733
                    393331342C2D372E3631343120392E333830393937332C312E3032303120332E
                    3830373036362C2D382E3633343320332E3830373036362C382E363334332039
                    2E3338303939372C2D312E30323031202D352E3537333933312C372E36313431
                    207A220A20202020202020696E6B73636170653A7472616E73666F726D2D6365
                    6E7465722D793D222D332E3830373035333822202F3E0A20203C2F673E0A3C2F
                    7376673E0A}
                  Proportional = True
                  Transparent = True
                  Animation.FrameCount = 0
                  Animation.Position = 0
                end
              end
              object paBankingPersonalInfoCreditCardSecurityInfo: TPanel
                AlignWithMargins = True
                Left = 3
                Top = 30
                Width = 181
                Height = 21
                Margins.Top = 7
                Align = alTop
                BevelOuter = bvNone
                Color = clWhite
                ParentBackground = False
                TabOrder = 1
                object imBankingPersonalInfoCreditCardSecurity: TWSVGImage
                  Left = 0
                  Top = 0
                  Width = 33
                  Height = 21
                  Align = alLeft
                  Center = True
                  Picture.Data = {
                    0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                    20656E636F64696E673D2269736F2D383835392D31223F3E0D0A3C212D2D2047
                    656E657261746F723A2041646F626520496C6C7573747261746F722031392E30
                    2E302C20535647204578706F727420506C75672D496E202E2053564720566572
                    73696F6E3A20362E3030204275696C6420302920202D2D3E0D0A3C7376672076
                    657273696F6E3D22312E31222069643D224C617965725F312220786D6C6E733D
                    22687474703A2F2F7777772E77332E6F72672F323030302F7376672220786D6C
                    6E733A786C696E6B3D22687474703A2F2F7777772E77332E6F72672F31393939
                    2F786C696E6B2220783D223070782220793D22307078220D0A09207669657742
                    6F783D22302030203436302034363022207374796C653D22656E61626C652D62
                    61636B67726F756E643A6E65772030203020343630203436303B2220786D6C3A
                    73706163653D227072657365727665223E0D0A3C672069643D22584D4C49445F
                    38385F223E0D0A093C673E0D0A09093C673E0D0A0909093C7061746820643D22
                    4D3433342E3734332C38322E3132364832352E3235374331312E3330382C3832
                    2E3132362C302C39332E3433352C302C3130372E333833763136342E34396834
                    3630762D3136342E3439433436302C39332E3433352C3434382E3639322C3832
                    2E3132362C3433342E3734332C38322E3132367A0D0A09090909204D3135382E
                    3130322C3139372E3833374C3135382E3130322C3139372E383337632D302E30
                    30312C362E362D352E3430312C31322D31322E3030312C31324835362E363537
                    632D362E362C302D31322D352E342D31322D3132762D34322E31323763302D36
                    2E362C352E342D31322C31322D31326838392E3434350D0A0909090963362E36
                    2C302C31322C352E342C31322C3132563139372E3833377A204D3331332E3531
                    312C3231362E343935632D372E3530342C342E3734332D31362E3338392C372E
                    3439382D32352E3932322C372E343938632D32362E3834322C302D34382E3630
                    312D32312E37362D34382E3630312D34382E3630310D0A0909090963302D3236
                    2E3834322C32312E3735392D34382E3630312C34382E3630312D34382E363031
                    6331302E3132362C302C31392E3532372C332E312C32372E3331312C382E3339
                    38632D382E3833362C31312E3534392D31342E3130322C32352E3936372D3134
                    2E3130322C34312E3539390D0A09090909433330302E3739382C3139312E3537
                    392C3330352E3531392C3230352E3237392C3331332E3531312C3231362E3439
                    357A204D3336392E3339392C3232352E333839632D32362E3834322C302D3438
                    2E3630312D32312E37362D34382E3630312D34382E3630310D0A090909097332
                    312E3735392D34382E3630312C34382E3630312D34382E3630316332362E3834
                    322C302C34382E3630312C32312E3735392C34382E3630312C34382E36303143
                    3431382C3230332E3633312C3339362E3234312C3232352E3338392C3336392E
                    3339392C3232352E3338397A222F3E0D0A0909093C7061746820643D224D302C
                    3335322E36313663302C31332E3934392C31312E3330382C32352E3235372C32
                    352E3235372C32352E323537683430392E3438366331332E3934392C302C3235
                    2E3235372D31312E3330382C32352E3235372D32352E323537762D31322E3734
                    334830563335322E3631367A222F3E0D0A09093C2F673E0D0A093C2F673E0D0A
                    3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D
                    0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E
                    0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C67
                    3E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C
                    673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A
                    3C673E0D0A3C2F673E0D0A3C2F7376673E0D0A}
                  Proportional = True
                  Transparent = True
                  Animation.FrameCount = 0
                  Animation.Position = 0
                end
                object edBankingPersonalInfoCreditCardSecurity: TEdit
                  Left = 33
                  Top = 0
                  Width = 148
                  Height = 21
                  Align = alLeft
                  TabOrder = 0
                end
              end
            end
            object paBankingPersonalInfoCreditCardExpires: TPanel
              Left = 0
              Top = 0
              Width = 184
              Height = 55
              Align = alLeft
              BevelOuter = bvNone
              Color = clWhite
              ParentBackground = False
              TabOrder = 1
              object paBankingPersonalInfoCreditCardExpiresTitle: TPanel
                AlignWithMargins = True
                Left = 3
                Top = 7
                Width = 178
                Height = 13
                Margins.Top = 7
                Align = alTop
                BevelOuter = bvNone
                Color = clWhite
                ParentBackground = False
                TabOrder = 0
                object laBankingPersonalInfoCreditCardExpires: TLabel
                  Left = 0
                  Top = 0
                  Width = 73
                  Height = 13
                  Margins.Top = 15
                  Align = alLeft
                  Caption = 'Expiration date'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = 526344
                  Font.Height = -11
                  Font.Name = 'Tahoma'
                  Font.Style = []
                  ParentFont = False
                end
                object imBankingPersonalInfoCreditCardExpires: TWSVGImage
                  AlignWithMargins = True
                  Left = 73
                  Top = 2
                  Width = 7
                  Height = 7
                  Margins.Left = 0
                  Margins.Top = 2
                  Margins.Bottom = 4
                  Align = alLeft
                  Center = True
                  Picture.Data = {
                    0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                    20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                    223F3E0A3C212D2D2043726561746564207769746820496E6B73636170652028
                    687474703A2F2F7777772E696E6B73636170652E6F72672F29202D2D3E0A0A3C
                    7376670A202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F7267
                    2F64632F656C656D656E74732F312E312F220A202020786D6C6E733A63633D22
                    687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A
                    202020786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F
                    313939392F30322F32322D7264662D73796E7461782D6E7323220A202020786D
                    6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F323030302F
                    737667220A202020786D6C6E733D22687474703A2F2F7777772E77332E6F7267
                    2F323030302F737667220A202020786D6C6E733A736F6469706F64693D226874
                    74703A2F2F736F6469706F64692E736F75726365666F7267652E6E65742F4454
                    442F736F6469706F64692D302E647464220A202020786D6C6E733A696E6B7363
                    6170653D22687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D
                    657370616365732F696E6B7363617065220A20202077696474683D223332220A
                    2020206865696768743D223332220A20202076696577426F783D223020302033
                    322033322E303030303031220A20202069643D2273766732220A202020766572
                    73696F6E3D22312E31220A202020696E6B73636170653A76657273696F6E3D22
                    302E393120723133373235220A202020736F6469706F64693A646F636E616D65
                    3D22617374657269736B2E737667223E0A20203C646566730A20202020206964
                    3D22646566733422202F3E0A20203C736F6469706F64693A6E616D6564766965
                    770A202020202069643D2262617365220A202020202070616765636F6C6F723D
                    2223666666666666220A2020202020626F72646572636F6C6F723D2223363636
                    363636220A2020202020626F726465726F7061636974793D22312E30220A2020
                    202020696E6B73636170653A706167656F7061636974793D22302E30220A2020
                    202020696E6B73636170653A70616765736861646F773D2232220A2020202020
                    696E6B73636170653A7A6F6F6D3D2232322E34220A2020202020696E6B736361
                    70653A63783D2231362E373333383639220A2020202020696E6B73636170653A
                    63793D2231352E373436313036220A2020202020696E6B73636170653A646F63
                    756D656E742D756E6974733D227078220A2020202020696E6B73636170653A63
                    757272656E742D6C617965723D226C6179657231220A202020202073686F7767
                    7269643D2266616C7365220A2020202020756E6974733D227078220A20202020
                    20696E6B73636170653A73686F7770616765736861646F773D2266616C736522
                    0A2020202020696E6B73636170653A77696E646F772D77696474683D22313932
                    30220A2020202020696E6B73636170653A77696E646F772D6865696768743D22
                    31303238220A2020202020696E6B73636170653A77696E646F772D783D222D38
                    220A2020202020696E6B73636170653A77696E646F772D793D222D38220A2020
                    202020696E6B73636170653A77696E646F772D6D6178696D697A65643D223122
                    202F3E0A20203C6D657461646174610A202020202069643D226D657461646174
                    6137223E0A202020203C7264663A5244463E0A2020202020203C63633A576F72
                    6B0A2020202020202020207264663A61626F75743D22223E0A20202020202020
                    203C64633A666F726D61743E696D6167652F7376672B786D6C3C2F64633A666F
                    726D61743E0A20202020202020203C64633A747970650A202020202020202020
                    20207264663A7265736F757263653D22687474703A2F2F7075726C2E6F72672F
                    64632F64636D69747970652F5374696C6C496D61676522202F3E0A2020202020
                    2020203C64633A7469746C653E3C2F64633A7469746C653E0A2020202020203C
                    2F63633A576F726B3E0A202020203C2F7264663A5244463E0A20203C2F6D6574
                    61646174613E0A20203C670A2020202020696E6B73636170653A6C6162656C3D
                    224C617965722031220A2020202020696E6B73636170653A67726F75706D6F64
                    653D226C61796572220A202020202069643D226C6179657231220A2020202020
                    7472616E73666F726D3D227472616E736C61746528302C2D313032302E333632
                    3229223E0A202020203C706174680A20202020202020736F6469706F64693A74
                    7970653D2273746172220A202020202020207374796C653D226F706163697479
                    3A313B66696C6C3A236537313931323B66696C6C2D6F7061636974793A313B66
                    696C6C2D72756C653A6E6F6E7A65726F3B7374726F6B653A236633303630303B
                    7374726F6B652D77696474683A302E34363739353438373B7374726F6B652D6C
                    696E656361703A726F756E643B7374726F6B652D6D697465726C696D69743A34
                    3B7374726F6B652D6461736861727261793A6E6F6E653B7374726F6B652D6461
                    73686F66667365743A303B7374726F6B652D6F7061636974793A31220A202020
                    2020202069643D227061746834313437220A20202020202020736F6469706F64
                    693A73696465733D2236220A20202020202020736F6469706F64693A63783D22
                    3136220A20202020202020736F6469706F64693A63793D22313033362E333632
                    32220A20202020202020736F6469706F64693A72313D2231352E323238323634
                    220A20202020202020736F6469706F64693A72323D22372E3631343133313922
                    0A20202020202020736F6469706F64693A617267313D22302E35323335393837
                    38220A20202020202020736F6469706F64693A617267323D22312E3034373139
                    3736220A20202020202020696E6B73636170653A666C617473696465643D2266
                    616C7365220A20202020202020696E6B73636170653A726F756E6465643D2230
                    220A20202020202020696E6B73636170653A72616E646F6D697A65643D223022
                    0A20202020202020643D226D2032392E3138383036332C313034332E39373633
                    202D392E3338303939372C2D312E30323031202D332E3830373036362C382E36
                    333432202D332E3830373036362C2D382E36333432202D392E33383039393734
                    2C312E3032303120352E353733393331352C2D372E36313431202D352E353733
                    393331342C2D372E3631343120392E333830393937332C312E3032303120332E
                    3830373036362C2D382E3633343320332E3830373036362C382E363334332039
                    2E3338303939372C2D312E30323031202D352E3537333933312C372E36313431
                    207A220A20202020202020696E6B73636170653A7472616E73666F726D2D6365
                    6E7465722D793D222D332E3830373035333822202F3E0A20203C2F673E0A3C2F
                    7376673E0A}
                  Proportional = True
                  Transparent = True
                  Animation.FrameCount = 0
                  Animation.Position = 0
                end
              end
              object paBankingPersonalInfoCreditCardExpiresInfo: TPanel
                AlignWithMargins = True
                Left = 3
                Top = 30
                Width = 178
                Height = 21
                Margins.Top = 7
                Align = alTop
                BevelOuter = bvNone
                Color = clWhite
                ParentBackground = False
                TabOrder = 1
                object dpBankingPersonalInfoCreditCardExpiresInfoDay: TDateTimePicker
                  Left = 0
                  Top = 0
                  Width = 50
                  Height = 21
                  Align = alLeft
                  Date = 42837.000000000000000000
                  Format = 'd'
                  Time = 42837.000000000000000000
                  DateMode = dmUpDown
                  TabOrder = 0
                end
                object dpBankingPersonalInfoCreditCardExpiresInfoMonth: TDateTimePicker
                  AlignWithMargins = True
                  Left = 53
                  Top = 0
                  Width = 50
                  Height = 21
                  Margins.Top = 0
                  Margins.Right = 0
                  Margins.Bottom = 0
                  Align = alLeft
                  Date = 42837.000000000000000000
                  Format = 'M'
                  Time = 42837.000000000000000000
                  DateMode = dmUpDown
                  TabOrder = 1
                end
                object dpBankingPersonalInfoCreditCardExpiresInfoYear: TDateTimePicker
                  AlignWithMargins = True
                  Left = 106
                  Top = 0
                  Width = 55
                  Height = 21
                  Margins.Top = 0
                  Margins.Right = 0
                  Margins.Bottom = 0
                  Align = alLeft
                  Date = 42837.000000000000000000
                  Format = 'yyyy'
                  Time = 42837.000000000000000000
                  DateMode = dmUpDown
                  TabOrder = 2
                end
              end
            end
          end
        end
        object paBankingAboutProduct: TPanel
          Left = 0
          Top = 683
          Width = 644
          Height = 137
          Align = alTop
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 4
          object laBankingAboutProductTitle: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 138
            Height = 19
            Align = alTop
            Caption = 'About our products'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = 526344
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object laBankingAboutProductDesc: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 28
            Width = 330
            Height = 13
            Align = alTop
            Caption = 
              'Please tell us a litttle more about how you experienced our prod' +
              'ucts.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = 526344
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object paBankingAboutProductRight: TPanel
            AlignWithMargins = True
            Left = 292
            Top = 44
            Width = 291
            Height = 93
            Margins.Left = 1
            Margins.Top = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alLeft
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 0
            object ckBankingAboutProductFromAdv: TCheckBox
              Left = 0
              Top = 17
              Width = 291
              Height = 17
              Align = alTop
              Caption = 'From advertising'
              TabOrder = 0
            end
            object ckBankingAboutProductOther: TCheckBox
              Left = 0
              Top = 34
              Width = 291
              Height = 17
              Align = alTop
              Caption = 'Other'
              TabOrder = 1
            end
            object ckBankingAboutProductPrivate: TCheckBox
              Left = 0
              Top = 0
              Width = 291
              Height = 17
              Align = alTop
              Caption = 'For private purposes'
              Checked = True
              State = cbChecked
              TabOrder = 2
            end
          end
          object paBankingAboutProductLeft: TPanel
            Left = 0
            Top = 44
            Width = 291
            Height = 93
            Margins.Left = 0
            Margins.Top = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alLeft
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 1
            object ckBankingAboutProductProfessional: TCheckBox
              Left = 0
              Top = 68
              Width = 291
              Height = 17
              Align = alTop
              Caption = 'For professional purposes'
              TabOrder = 0
            end
            object ckBankingAboutProductAsAGift: TCheckBox
              Left = 0
              Top = 51
              Width = 291
              Height = 17
              Align = alTop
              Caption = 'As a gift'
              TabOrder = 1
            end
            object ckBankingAboutProductByFriends: TCheckBox
              Left = 0
              Top = 34
              Width = 291
              Height = 17
              Align = alTop
              Caption = 'By friends'
              Checked = True
              State = cbChecked
              TabOrder = 2
            end
            object ckBankingAboutProductOnTheInternet: TCheckBox
              Left = 0
              Top = 17
              Width = 291
              Height = 17
              Align = alTop
              Caption = 'On the internet'
              Checked = True
              State = cbChecked
              TabOrder = 3
            end
            object ckBankingAboutProductInAShop: TCheckBox
              Left = 0
              Top = 0
              Width = 291
              Height = 17
              Align = alTop
              Caption = 'In a shop'
              TabOrder = 4
            end
          end
        end
        object paBankingBuy: TPanel
          Left = 0
          Top = 827
          Width = 644
          Height = 100
          Align = alTop
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 5
          object imBankingBuyNowButton: TWSVGImageButton
            AlignWithMargins = True
            Left = 0
            Top = 0
            Width = 644
            Height = 100
            Cursor = crHandPoint
            Margins.Left = 0
            Margins.Top = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alTop
            Center = True
            Picture.Data = {
              0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
              20656E636F64696E673D227574662D38223F3E0D0A3C212D2D2047656E657261
              746F723A2041646F626520496C6C7573747261746F722031392E302E312C2053
              5647204578706F727420506C75672D496E202E205356472056657273696F6E3A
              20362E3030204275696C6420302920202D2D3E0D0A3C7376672076657273696F
              6E3D22312E31222069643D224C617965725F312220786D6C6E733D2268747470
              3A2F2F7777772E77332E6F72672F323030302F7376672220786D6C6E733A786C
              696E6B3D22687474703A2F2F7777772E77332E6F72672F313939392F786C696E
              6B2220783D223070782220793D22307078220D0A092076696577426F783D2230
              203020383020383022207374796C653D22656E61626C652D6261636B67726F75
              6E643A6E6577203020302038302038303B2220786D6C3A73706163653D227072
              657365727665223E0D0A3C7374796C6520747970653D22746578742F63737322
              3E0D0A092E7374307B66696C6C3A233145323332443B7D0D0A3C2F7374796C65
              3E0D0A3C673E0D0A093C7061746820636C6173733D227374302220643D224D37
              352E392C35342E314C35362E372C34322E365632392E3863302D322E362D322E
              322D342E382D342E382D342E38732D342E382C322E322D342E382C342E387632
              386C2D352E322D352E32632D322D322E312D342E382D332E322D372E372D332E
              320D0A0909632D322E392C302D352E372C312E322D372E372C332E326C32302C
              323063332E392C332E372C382E322C352E332C31322E372C352E3363322E342C
              302C342E372D302E352C362E382D312E3363362E352D322E372C31312D392C31
              312D31362E354337372C35382C37362E362C35362C37352E392C35342E317A22
              2F3E0D0A093C673E0D0A09093C7061746820636C6173733D227374302220643D
              224D32352E332C31392E37632D302E342D302E332D312E312D302E352D322D30
              2E35682D3476342E3368342E3163302E392C302C312E352D302E322C322D302E
              3663302E342D302E342C302E372D312C302E372D312E370D0A0909094332362C
              32302E362C32352E382C32302E312C32352E332C31392E377A222F3E0D0A0909
              3C7061746820636C6173733D227374302220643D224D32372E382C3235632D31
              2C302E392D322E352C312E332D342E352C312E33682D352E37682D312E365631
              302E3368312E3668362E3263312E362C302C322E382C302E342C332E362C312E
              3163302E392C302E372C312E332C312E372C312E332C330D0A09090963302C31
              2E332D302E352C322E332D312E352C33632D302E322C302E312D302E332C302E
              322D302E352C302E3363302E342C302E312C302E372C302E332C312C302E3563
              312C302E372C312E352C312E372C312E352C332E314332392E342C32322E392C
              32382E392C32342E322C32372E382C32357A0D0A090909204D34372E312C3230
              2E3363302C322E332D302E352C342D312E352C35632D312C312D322E372C312E
              352D352C312E35632D322E322C302D332E382D302E352D342E392D312E36632D
              312E312D312E312D312E362D322E372D312E362D342E39762D313068312E3768
              312E377631302E310D0A09090963302C312E322C302E332C322C302E382C322E
              3673312E332C302E392C322E342C302E3963312E312C302C312E392D302E332C
              322E342D302E3863302E352D302E352C302E372D312E342C302E372D322E3756
              31302E3368312E3768312E375632302E337A204D37302E392C362E380D0A0909
              09632D332D332D372E312D342E382D31312E372D342E38682D33394331312E31
              2C322C332E372C392E342C332E372C31382E3563302C342E362C312E382C382E
              372C342E382C31312E3763332C332C372E312C342E382C31312E372C342E3868
              32332E35762D356831322E39762D392E376C2D352E362D313068340D0A090909
              6C332E332C376C332E312D3768332E386C2D352E332C313056333563382E372D
              302E342C31352E372D372E362C31352E372D31362E354337352E372C31332E39
              2C37332E392C392E382C37302E392C362E387A222F3E0D0A09093C7061746820
              636C6173733D227374302220643D224D32352C31362E3163302E342D302E332C
              302E362D302E382C302E362D312E3463302D302E352D302E322D312D302E372D
              312E32632D302E342D302E332D312E312D302E342D322D302E34682D332E3776
              332E3568332E380D0A0909094332342C31362E352C32342E362C31362E342C32
              352C31362E317A222F3E0D0A093C2F673E0D0A3C2F673E0D0A3C2F7376673E0D
              0A}
            Proportional = True
            Stretch = True
            Transparent = True
            OnClick = imBankingBuyNowButtonClick
            Animation.FrameCount = 0
            Animation.Position = 0
            HoveredAnimation.FrameCount = 0
            HoveredAnimation.Position = 0
            HoveredAnimation.Animate = True
            HoveredPicture.Data = {
              0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
              20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
              223F3E0A3C212D2D2047656E657261746F723A2041646F626520496C6C757374
              7261746F722031392E302E312C20535647204578706F727420506C75672D496E
              202E205356472056657273696F6E3A20362E3030204275696C6420302920202D
              2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
              6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
              3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
              6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
              2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
              202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
              323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
              332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
              693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
              65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
              696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
              672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
              6E3D22312E31220A20202069643D224C617965725F31220A202020783D223070
              78220A202020793D22307078220A20202076696577426F783D22302030203830
              203830220A2020207374796C653D22656E61626C652D6261636B67726F756E64
              3A6E6577203020302038302038303B220A202020786D6C3A73706163653D2270
              72657365727665220A202020696E6B73636170653A76657273696F6E3D22302E
              393120723133373235220A202020736F6469706F64693A646F636E616D653D22
              637573746F6D5F62757920627574746F6E5F686F7665722E737667223E3C6D65
              7461646174610A202020202069643D226D657461646174613231223E3C726466
              3A5244463E3C63633A576F726B0A2020202020202020207264663A61626F7574
              3D22223E3C64633A666F726D61743E696D6167652F7376672B786D6C3C2F6463
              3A666F726D61743E3C64633A747970650A20202020202020202020207264663A
              7265736F757263653D22687474703A2F2F7075726C2E6F72672F64632F64636D
              69747970652F5374696C6C496D61676522202F3E3C64633A7469746C653E3C2F
              64633A7469746C653E3C2F63633A576F726B3E3C2F7264663A5244463E3C2F6D
              657461646174613E3C646566730A202020202069643D2264656673313922202F
              3E3C736F6469706F64693A6E616D6564766965770A202020202070616765636F
              6C6F723D2223666666666666220A2020202020626F72646572636F6C6F723D22
              23363636363636220A2020202020626F726465726F7061636974793D2231220A
              20202020206F626A656374746F6C6572616E63653D223130220A202020202067
              726964746F6C6572616E63653D223130220A20202020206775696465746F6C65
              72616E63653D223130220A2020202020696E6B73636170653A706167656F7061
              636974793D2230220A2020202020696E6B73636170653A70616765736861646F
              773D2232220A2020202020696E6B73636170653A77696E646F772D7769647468
              3D2231353631220A2020202020696E6B73636170653A77696E646F772D686569
              6768743D22383739220A202020202069643D226E616D6564766965773137220A
              202020202073686F77677269643D2266616C7365220A2020202020696E6B7363
              6170653A7A6F6F6D3D22322E3935220A2020202020696E6B73636170653A6378
              3D223430220A2020202020696E6B73636170653A63793D223430220A20202020
              20696E6B73636170653A77696E646F772D783D2230220A2020202020696E6B73
              636170653A77696E646F772D793D2230220A2020202020696E6B73636170653A
              77696E646F772D6D6178696D697A65643D2230220A2020202020696E6B736361
              70653A63757272656E742D6C617965723D224C617965725F3122202F3E3C7374
              796C650A2020202020747970653D22746578742F637373220A20202020206964
              3D227374796C6533223E0A092E7374307B66696C6C3A233145323332443B7D0A
              3C2F7374796C653E3C670A202020202069643D226735220A2020202020737479
              6C653D2266696C6C3A236530383030613B66696C6C2D6F7061636974793A3122
              3E3C706174680A20202020202020636C6173733D22737430220A202020202020
              20643D224D37352E392C35342E314C35362E372C34322E365632392E3863302D
              322E362D322E322D342E382D342E382D342E38732D342E382C322E322D342E38
              2C342E387632386C2D352E322D352E32632D322D322E312D342E382D332E322D
              372E372D332E32202020632D322E392C302D352E372C312E322D372E372C332E
              326C32302C323063332E392C332E372C382E322C352E332C31322E372C352E33
              63322E342C302C342E372D302E352C362E382D312E3363362E352D322E372C31
              312D392C31312D31362E354337372C35382C37362E362C35362C37352E392C35
              342E317A220A2020202020202069643D227061746837220A2020202020202073
              74796C653D2266696C6C3A236530383030613B66696C6C2D6F7061636974793A
              3122202F3E3C670A2020202020202069643D226739220A202020202020207374
              796C653D2266696C6C3A236530383030613B66696C6C2D6F7061636974793A31
              223E3C706174680A202020202020202020636C6173733D22737430220A202020
              202020202020643D224D32352E332C31392E37632D302E342D302E332D312E31
              2D302E352D322D302E35682D3476342E3368342E3163302E392C302C312E352D
              302E322C322D302E3663302E342D302E342C302E372D312C302E372D312E3720
              2020204332362C32302E362C32352E382C32302E312C32352E332C31392E377A
              220A20202020202020202069643D22706174683131220A202020202020202020
              7374796C653D2266696C6C3A236530383030613B66696C6C2D6F706163697479
              3A3122202F3E3C706174680A202020202020202020636C6173733D2273743022
              0A202020202020202020643D224D32372E382C3235632D312C302E392D322E35
              2C312E332D342E352C312E33682D352E37682D312E365631302E3368312E3668
              362E3263312E362C302C322E382C302E342C332E362C312E3163302E392C302E
              372C312E332C312E372C312E332C332020202063302C312E332D302E352C322E
              332D312E352C33632D302E322C302E312D302E332C302E322D302E352C302E33
              63302E342C302E312C302E372C302E332C312C302E3563312C302E372C312E35
              2C312E372C312E352C332E314332392E342C32322E392C32382E392C32342E32
              2C32372E382C32357A20202020204D34372E312C32302E3363302C322E332D30
              2E352C342D312E352C35632D312C312D322E372C312E352D352C312E35632D32
              2E322C302D332E382D302E352D342E392D312E36632D312E312D312E312D312E
              362D322E372D312E362D342E39762D313068312E3768312E377631302E312020
              202063302C312E322C302E332C322C302E382C322E3673312E332C302E392C32
              2E342C302E3963312E312C302C312E392D302E332C322E342D302E3863302E35
              2D302E352C302E372D312E342C302E372D322E375631302E3368312E3768312E
              375632302E337A204D37302E392C362E3820202020632D332D332D372E312D34
              2E382D31312E372D342E38682D33394331312E312C322C332E372C392E342C33
              2E372C31382E3563302C342E362C312E382C382E372C342E382C31312E376333
              2C332C372E312C342E382C31312E372C342E386832332E35762D356831322E39
              762D392E376C2D352E362D31306834202020206C332E332C376C332E312D3768
              332E386C2D352E332C313056333563382E372D302E342C31352E372D372E362C
              31352E372D31362E354337352E372C31332E392C37332E392C392E382C37302E
              392C362E387A220A20202020202020202069643D22706174683133220A202020
              2020202020207374796C653D2266696C6C3A236530383030613B66696C6C2D6F
              7061636974793A3122202F3E3C706174680A202020202020202020636C617373
              3D22737430220A202020202020202020643D224D32352C31362E3163302E342D
              302E332C302E362D302E382C302E362D312E3463302D302E352D302E322D312D
              302E372D312E32632D302E342D302E332D312E312D302E342D322D302E34682D
              332E3776332E3568332E38202020204332342C31362E352C32342E362C31362E
              342C32352C31362E317A220A20202020202020202069643D2270617468313522
              0A2020202020202020207374796C653D2266696C6C3A236530383030613B6669
              6C6C2D6F7061636974793A3122202F3E3C2F673E3C2F673E3C2F7376673E}
            ClickedAnimation.FrameCount = 0
            ClickedAnimation.Position = 0
            ClickedAnimation.Animate = True
            ClickedPicture.Data = {
              0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
              20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
              223F3E0A3C212D2D2047656E657261746F723A2041646F626520496C6C757374
              7261746F722031392E302E312C20535647204578706F727420506C75672D496E
              202E205356472056657273696F6E3A20362E3030204275696C6420302920202D
              2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
              6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
              3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
              6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
              2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
              202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
              323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
              332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
              693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
              65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
              696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
              672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
              6E3D22312E31220A20202069643D224C617965725F31220A202020783D223070
              78220A202020793D22307078220A20202076696577426F783D22302030203830
              203830220A2020207374796C653D22656E61626C652D6261636B67726F756E64
              3A6E6577203020302038302038303B220A202020786D6C3A73706163653D2270
              72657365727665220A202020696E6B73636170653A76657273696F6E3D22302E
              393120723133373235220A202020736F6469706F64693A646F636E616D653D22
              637573746F6D5F62757920627574746F6E5F636C69636B65642E737667223E3C
              6D657461646174610A202020202069643D226D657461646174613231223E3C72
              64663A5244463E3C63633A576F726B0A2020202020202020207264663A61626F
              75743D22223E3C64633A666F726D61743E696D6167652F7376672B786D6C3C2F
              64633A666F726D61743E3C64633A747970650A20202020202020202020207264
              663A7265736F757263653D22687474703A2F2F7075726C2E6F72672F64632F64
              636D69747970652F5374696C6C496D61676522202F3E3C64633A7469746C653E
              3C2F64633A7469746C653E3C2F63633A576F726B3E3C2F7264663A5244463E3C
              2F6D657461646174613E3C646566730A202020202069643D2264656673313922
              202F3E3C736F6469706F64693A6E616D6564766965770A202020202070616765
              636F6C6F723D2223666666666666220A2020202020626F72646572636F6C6F72
              3D2223363636363636220A2020202020626F726465726F7061636974793D2231
              220A20202020206F626A656374746F6C6572616E63653D223130220A20202020
              2067726964746F6C6572616E63653D223130220A20202020206775696465746F
              6C6572616E63653D223130220A2020202020696E6B73636170653A706167656F
              7061636974793D2230220A2020202020696E6B73636170653A70616765736861
              646F773D2232220A2020202020696E6B73636170653A77696E646F772D776964
              74683D2231353631220A2020202020696E6B73636170653A77696E646F772D68
              65696768743D22383739220A202020202069643D226E616D6564766965773137
              220A202020202073686F77677269643D2266616C7365220A2020202020696E6B
              73636170653A7A6F6F6D3D22322E3935220A2020202020696E6B73636170653A
              63783D223430220A2020202020696E6B73636170653A63793D223430220A2020
              202020696E6B73636170653A77696E646F772D783D2230220A2020202020696E
              6B73636170653A77696E646F772D793D2230220A2020202020696E6B73636170
              653A77696E646F772D6D6178696D697A65643D2230220A2020202020696E6B73
              636170653A63757272656E742D6C617965723D224C617965725F3122202F3E3C
              7374796C650A2020202020747970653D22746578742F637373220A2020202020
              69643D227374796C6533223E0A092E7374307B66696C6C3A233145323332443B
              7D0A3C2F7374796C653E3C670A202020202069643D226735220A202020202073
              74796C653D2266696C6C3A236666633230613B66696C6C2D6F7061636974793A
              31223E3C706174680A20202020202020636C6173733D22737430220A20202020
              202020643D224D37352E392C35342E314C35362E372C34322E365632392E3863
              302D322E362D322E322D342E382D342E382D342E38732D342E382C322E322D34
              2E382C342E387632386C2D352E322D352E32632D322D322E312D342E382D332E
              322D372E372D332E32202020632D322E392C302D352E372C312E322D372E372C
              332E326C32302C323063332E392C332E372C382E322C352E332C31322E372C35
              2E3363322E342C302C342E372D302E352C362E382D312E3363362E352D322E37
              2C31312D392C31312D31362E354337372C35382C37362E362C35362C37352E39
              2C35342E317A220A2020202020202069643D227061746837220A202020202020
              207374796C653D2266696C6C3A236666633230613B66696C6C2D6F7061636974
              793A3122202F3E3C670A2020202020202069643D226739220A20202020202020
              7374796C653D2266696C6C3A236666633230613B66696C6C2D6F706163697479
              3A31223E3C706174680A202020202020202020636C6173733D22737430220A20
              2020202020202020643D224D32352E332C31392E37632D302E342D302E332D31
              2E312D302E352D322D302E35682D3476342E3368342E3163302E392C302C312E
              352D302E322C322D302E3663302E342D302E342C302E372D312C302E372D312E
              37202020204332362C32302E362C32352E382C32302E312C32352E332C31392E
              377A220A20202020202020202069643D22706174683131220A20202020202020
              20207374796C653D2266696C6C3A236666633230613B66696C6C2D6F70616369
              74793A3122202F3E3C706174680A202020202020202020636C6173733D227374
              30220A202020202020202020643D224D32372E382C3235632D312C302E392D32
              2E352C312E332D342E352C312E33682D352E37682D312E365631302E3368312E
              3668362E3263312E362C302C322E382C302E342C332E362C312E3163302E392C
              302E372C312E332C312E372C312E332C332020202063302C312E332D302E352C
              322E332D312E352C33632D302E322C302E312D302E332C302E322D302E352C30
              2E3363302E342C302E312C302E372C302E332C312C302E3563312C302E372C31
              2E352C312E372C312E352C332E314332392E342C32322E392C32382E392C3234
              2E322C32372E382C32357A20202020204D34372E312C32302E3363302C322E33
              2D302E352C342D312E352C35632D312C312D322E372C312E352D352C312E3563
              2D322E322C302D332E382D302E352D342E392D312E36632D312E312D312E312D
              312E362D322E372D312E362D342E39762D313068312E3768312E377631302E31
              2020202063302C312E322C302E332C322C302E382C322E3673312E332C302E39
              2C322E342C302E3963312E312C302C312E392D302E332C322E342D302E386330
              2E352D302E352C302E372D312E342C302E372D322E375631302E3368312E3768
              312E375632302E337A204D37302E392C362E3820202020632D332D332D372E31
              2D342E382D31312E372D342E38682D33394331312E312C322C332E372C392E34
              2C332E372C31382E3563302C342E362C312E382C382E372C342E382C31312E37
              63332C332C372E312C342E382C31312E372C342E386832332E35762D35683132
              2E39762D392E376C2D352E362D31306834202020206C332E332C376C332E312D
              3768332E386C2D352E332C313056333563382E372D302E342C31352E372D372E
              362C31352E372D31362E354337352E372C31332E392C37332E392C392E382C37
              302E392C362E387A220A20202020202020202069643D22706174683133220A20
              20202020202020207374796C653D2266696C6C3A236666633230613B66696C6C
              2D6F7061636974793A3122202F3E3C706174680A202020202020202020636C61
              73733D22737430220A202020202020202020643D224D32352C31362E3163302E
              342D302E332C302E362D302E382C302E362D312E3463302D302E352D302E322D
              312D302E372D312E32632D302E342D302E332D312E312D302E342D322D302E34
              682D332E3776332E3568332E38202020204332342C31362E352C32342E362C31
              362E342C32352C31362E317A220A20202020202020202069643D227061746831
              35220A2020202020202020207374796C653D2266696C6C3A236666633230613B
              66696C6C2D6F7061636974793A3122202F3E3C2F673E3C2F673E3C2F7376673E}
            DisabledAnimation.FrameCount = 200
            DisabledAnimation.Position = 0
            DisabledAnimation.Animate = True
            DisabledPicture.Data = {
              0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
              20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
              223F3E0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
              6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
              3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
              6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
              2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
              202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
              323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
              332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
              693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
              65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
              696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
              672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
              6E3D22312E31220A20202069643D226C6F616465722D31220A202020783D2230
              7078220A202020793D22307078220A20202077696474683D2234307078220A20
              20206865696768743D2234307078220A20202076696577426F783D2230203020
              3430203430220A202020656E61626C652D6261636B67726F756E643D226E6577
              20302030203430203430220A202020786D6C3A73706163653D22707265736572
              7665220A202020696E6B73636170653A76657273696F6E3D22302E3931207231
              33373235220A202020736F6469706F64693A646F636E616D653D22726F746174
              696E672D637572736F722E737667223E3C6D657461646174610A202020202069
              643D226D657461646174613131223E3C7264663A5244463E3C63633A576F726B
              0A2020202020202020207264663A61626F75743D22223E3C64633A666F726D61
              743E696D6167652F7376672B786D6C3C2F64633A666F726D61743E3C64633A74
              7970650A20202020202020202020207264663A7265736F757263653D22687474
              703A2F2F7075726C2E6F72672F64632F64636D69747970652F5374696C6C496D
              61676522202F3E3C2F63633A576F726B3E3C2F7264663A5244463E3C2F6D6574
              61646174613E3C646566730A202020202069643D22646566733922202F3E3C73
              6F6469706F64693A6E616D6564766965770A202020202070616765636F6C6F72
              3D2223666666666666220A2020202020626F72646572636F6C6F723D22233636
              36363636220A2020202020626F726465726F7061636974793D2231220A202020
              20206F626A656374746F6C6572616E63653D223130220A202020202067726964
              746F6C6572616E63653D223130220A20202020206775696465746F6C6572616E
              63653D223130220A2020202020696E6B73636170653A706167656F7061636974
              793D2230220A2020202020696E6B73636170653A70616765736861646F773D22
              32220A2020202020696E6B73636170653A77696E646F772D77696474683D2231
              333430220A2020202020696E6B73636170653A77696E646F772D686569676874
              3D22393437220A202020202069643D226E616D65647669657737220A20202020
              2073686F77677269643D2266616C7365220A2020202020696E6B73636170653A
              7A6F6F6D3D22352E39220A2020202020696E6B73636170653A63783D22323022
              0A2020202020696E6B73636170653A63793D223230220A2020202020696E6B73
              636170653A77696E646F772D783D2230220A2020202020696E6B73636170653A
              77696E646F772D793D2230220A2020202020696E6B73636170653A77696E646F
              772D6D6178696D697A65643D2230220A2020202020696E6B73636170653A6375
              7272656E742D6C617965723D226C6F616465722D3122202F3E3C706174680A20
              202020206F7061636974793D22302E32220A2020202020643D224D32302E3230
              312C352E313639632D382E3235342C302D31342E3934362C362E3639322D3134
              2E3934362C31342E39343663302C382E3235352C362E3639322C31342E393436
              2C31342E3934362C31342E39343620202020207331342E3934362D362E363931
              2C31342E3934362D31342E3934364333352E3134362C31312E3836312C32382E
              3435352C352E3136392C32302E3230312C352E3136397A204D32302E3230312C
              33312E373439632D362E3432352C302D31312E3633342D352E3230382D31312E
              3633342D31312E363334202020202063302D362E3432352C352E3230392D3131
              2E3633342C31312E3633342D31312E36333463362E3432352C302C31312E3633
              332C352E3230392C31312E3633332C31312E3633344333312E3833342C32362E
              3534312C32362E3632362C33312E3734392C32302E3230312C33312E3734397A
              220A202020202069643D227061746833220A20202020207374796C653D226669
              6C6C3A233030356565663B66696C6C2D6F7061636974793A3122202F3E3C7061
              74680A2020202020643D224D32362E3031332C31302E3034376C312E3635342D
              322E383636632D322E3139382D312E3237322D342E3734332D322E3031322D37
              2E3436362D322E303132683076332E333132683020202020204332322E33322C
              382E3438312C32342E3330312C392E3035372C32362E3031332C31302E303437
              7A220A202020202069643D227061746835220A20202020207374796C653D2266
              696C6C3A233064363864313B66696C6C2D6F7061636974793A31223E3C616E69
              6D6174655472616E73666F726D0A202020202020206174747269627574655479
              70653D22786D6C220A202020202020206174747269627574654E616D653D2274
              72616E73666F726D220A20202020202020747970653D22726F74617465220A20
              20202020202066726F6D3D2230203230203230220A20202020202020746F3D22
              333630203230203230220A202020202020206475723D22302E3573220A202020
              20202020726570656174436F756E743D22696E646566696E69746522202F3E3C
              2F706174683E3C2F7376673E}
          end
        end
      end
    end
    object tsFreshBooks: TTabSheet
      Caption = 'tsFreshBooks'
      ImageIndex = 3
      object sbFreshBooks: TScrollBox
        Left = 0
        Top = 0
        Width = 661
        Height = 794
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Constraints.MaxHeight = 794
        Constraints.MaxWidth = 661
        Color = clWhite
        ParentColor = False
        TabOrder = 0
        object imFreshBooksHeaderShadow: TImage
          Left = 0
          Top = 215
          Width = 661
          Height = 5
          Align = alTop
          Center = True
          Picture.Data = {
            0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
            20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
            223F3E0A3C212D2D2043726561746564207769746820496E6B73636170652028
            687474703A2F2F7777772E696E6B73636170652E6F72672F29202D2D3E0A0A3C
            7376670A202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F7267
            2F64632F656C656D656E74732F312E312F220A202020786D6C6E733A63633D22
            687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A
            202020786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F
            313939392F30322F32322D7264662D73796E7461782D6E7323220A202020786D
            6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F323030302F
            737667220A202020786D6C6E733D22687474703A2F2F7777772E77332E6F7267
            2F323030302F737667220A202020786D6C6E733A736F6469706F64693D226874
            74703A2F2F736F6469706F64692E736F75726365666F7267652E6E65742F4454
            442F736F6469706F64692D302E647464220A202020786D6C6E733A696E6B7363
            6170653D22687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D
            657370616365732F696E6B7363617065220A20202077696474683D2232353522
            0A2020206865696768743D2235220A20202076696577426F783D223020302032
            35352035220A20202069643D2273766732220A20202076657273696F6E3D2231
            2E31220A202020696E6B73636170653A76657273696F6E3D22302E3931207231
            33373235220A202020736F6469706F64693A646F636E616D653D22736861646F
            772E737667223E0A20203C646566730A202020202069643D2264656673342220
            2F3E0A20203C736F6469706F64693A6E616D6564766965770A20202020206964
            3D2262617365220A202020202070616765636F6C6F723D222366666666666622
            0A2020202020626F72646572636F6C6F723D2223363636363636220A20202020
            20626F726465726F7061636974793D22312E30220A2020202020696E6B736361
            70653A706167656F7061636974793D22302E30220A2020202020696E6B736361
            70653A70616765736861646F773D2232220A2020202020696E6B73636170653A
            7A6F6F6D3D2232220A2020202020696E6B73636170653A63783D222D38362E30
            3933303231220A2020202020696E6B73636170653A63793D223130382E393539
            3636220A2020202020696E6B73636170653A646F63756D656E742D756E697473
            3D227078220A2020202020696E6B73636170653A63757272656E742D6C617965
            723D226C6179657231220A202020202073686F77677269643D2266616C736522
            0A2020202020756E6974733D227078220A2020202020696E6B73636170653A73
            686F7770616765736861646F773D2266616C7365220A2020202020696E6B7363
            6170653A77696E646F772D77696474683D2231393230220A2020202020696E6B
            73636170653A77696E646F772D6865696768743D2231303238220A2020202020
            696E6B73636170653A77696E646F772D783D222D38220A2020202020696E6B73
            636170653A77696E646F772D793D222D38220A2020202020696E6B7363617065
            3A77696E646F772D6D6178696D697A65643D223122202F3E0A20203C6D657461
            646174610A202020202069643D226D6574616461746137223E0A202020203C72
            64663A5244463E0A2020202020203C63633A576F726B0A202020202020202020
            7264663A61626F75743D22223E0A20202020202020203C64633A666F726D6174
            3E696D6167652F7376672B786D6C3C2F64633A666F726D61743E0A2020202020
            2020203C64633A747970650A20202020202020202020207264663A7265736F75
            7263653D22687474703A2F2F7075726C2E6F72672F64632F64636D6974797065
            2F5374696C6C496D61676522202F3E0A20202020202020203C64633A7469746C
            65202F3E0A2020202020203C2F63633A576F726B3E0A202020203C2F7264663A
            5244463E0A20203C2F6D657461646174613E0A20203C670A2020202020696E6B
            73636170653A6C6162656C3D224C617965722031220A2020202020696E6B7363
            6170653A67726F75706D6F64653D226C61796572220A202020202069643D226C
            6179657231220A20202020207472616E73666F726D3D227472616E736C617465
            28302C2D313034372E3336323229223E0A202020203C706174680A2020202020
            20207374796C653D226F7061636974793A313B66696C6C3A236339633963393B
            66696C6C2D6F7061636974793A313B66696C6C2D72756C653A6E6F6E7A65726F
            3B7374726F6B653A6E6F6E653B7374726F6B652D77696474683A302E32343036
            393830373B7374726F6B652D6C696E656361703A726F756E643B7374726F6B65
            2D6D697465726C696D69743A343B7374726F6B652D6461736861727261793A6E
            6F6E653B7374726F6B652D646173686F66667365743A303B7374726F6B652D6F
            7061636974793A31220A20202020202020643D226D203235302C313034362E35
            31303220302C322E32353239206320302C322E30303333202D322E3130353839
            2C342E303538202D342E35393231312C332E34353138202D31312E3431303635
            2C2D322E37383133202D33302E393936332C2D352E36323539202D39332E3833
            3433352C2D352E38353237206C202D34382E30343039332C302063202D37302E
            35343836362C302E33383537202D38312E3336353336382C312E38373335202D
            39332E3934303438352C352E38353237204320372E31343332392C313035322E
            393920352C313035302E3533393120352C313034382E37363331206C20302C2D
            322E32353239207A220A2020202020202069643D227265637434313336220A20
            202020202020696E6B73636170653A636F6E6E6563746F722D63757276617475
            72653D2230220A20202020202020736F6469706F64693A6E6F64657479706573
            3D2263737363637373636322202F3E0A20203C2F673E0A3C2F7376673E0A}
          Stretch = True
          Transparent = True
        end
        object paFreshBooksHeader: TPanel
          Left = 0
          Top = 0
          Width = 661
          Height = 215
          Align = alTop
          BevelOuter = bvNone
          Color = 16644080
          ParentBackground = False
          TabOrder = 0
          object shFreshBooksHeaderBackground: TShape
            Left = 0
            Top = 0
            Width = 661
            Height = 215
            Align = alBottom
            Brush.Color = 16644080
            Pen.Color = 13224393
          end
          object paFreshBooksHeaderYourCreditCard: TPanel
            AlignWithMargins = True
            Left = 10
            Top = 10
            Width = 641
            Height = 40
            Margins.Left = 10
            Margins.Top = 10
            Margins.Right = 10
            Align = alTop
            BevelOuter = bvNone
            Color = 16644080
            ParentBackground = False
            TabOrder = 0
            object laFreshBooksHeaderYourCreditCard: TLabel
              AlignWithMargins = True
              Left = 40
              Top = 5
              Width = 134
              Height = 18
              Margins.Left = 0
              Margins.Top = 5
              Margins.Right = 0
              Align = alLeft
              Caption = 'Your Credit Card'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 14647812
              Font.Height = -16
              Font.Name = 'Verdana'
              Font.Style = []
              ParentFont = False
            end
            object imFreshBooksHeaderYourCreditCard: TImage
              Left = 0
              Top = 0
              Width = 40
              Height = 40
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                223F3E0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
                6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
                3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
                6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
                2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
                202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
                323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
                332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
                693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
                65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
                696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
                672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
                6E3D22312E31220A20202069643D224C617965725F31220A202020783D223070
                78220A202020793D22307078220A20202076696577426F783D22302030203530
                203530220A2020207374796C653D22656E61626C652D6261636B67726F756E64
                3A6E6577203020302035302035303B220A202020786D6C3A73706163653D2270
                72657365727665220A202020696E6B73636170653A76657273696F6E3D22302E
                393120723133373235220A202020736F6469706F64693A646F636E616D653D22
                626C75655F6C6561662E737667223E3C6D657461646174610A20202020206964
                3D226D657461646174613133223E3C7264663A5244463E3C63633A576F726B0A
                2020202020202020207264663A61626F75743D22223E3C64633A666F726D6174
                3E696D6167652F7376672B786D6C3C2F64633A666F726D61743E3C64633A7479
                70650A20202020202020202020207264663A7265736F757263653D2268747470
                3A2F2F7075726C2E6F72672F64632F64636D69747970652F5374696C6C496D61
                676522202F3E3C64633A7469746C653E417373657420313C2F64633A7469746C
                653E3C2F63633A576F726B3E3C2F7264663A5244463E3C2F6D65746164617461
                3E3C646566730A202020202069643D2264656673313122202F3E3C736F646970
                6F64693A6E616D6564766965770A202020202070616765636F6C6F723D222366
                6666666666220A2020202020626F72646572636F6C6F723D2223363636363636
                220A2020202020626F726465726F7061636974793D2231220A20202020206F62
                6A656374746F6C6572616E63653D223130220A202020202067726964746F6C65
                72616E63653D223130220A20202020206775696465746F6C6572616E63653D22
                3130220A2020202020696E6B73636170653A706167656F7061636974793D2230
                220A2020202020696E6B73636170653A70616765736861646F773D2232220A20
                20202020696E6B73636170653A77696E646F772D77696474683D22363930220A
                2020202020696E6B73636170653A77696E646F772D6865696768743D22343830
                220A202020202069643D226E616D65647669657739220A202020202073686F77
                677269643D2266616C7365220A2020202020696E6B73636170653A7A6F6F6D3D
                22342E3732220A2020202020696E6B73636170653A63783D223235220A202020
                2020696E6B73636170653A63793D223235220A2020202020696E6B7363617065
                3A77696E646F772D783D2230220A2020202020696E6B73636170653A77696E64
                6F772D793D2230220A2020202020696E6B73636170653A77696E646F772D6D61
                78696D697A65643D2230220A2020202020696E6B73636170653A63757272656E
                742D6C617965723D224C617965725F3122202F3E3C7374796C650A2020202020
                747970653D22746578742F637373220A202020202069643D227374796C653322
                3E0A092E7374307B66696C6C3A233030353736373B7D0A3C2F7374796C653E3C
                7469746C650A202020202069643D227469746C6535223E417373657420313C2F
                7469746C653E3C706174680A2020202020636C6173733D22737430220A202020
                2020643D224D33382E332C382E35632D312E362C322E362D352E332C352E362D
                31352E322C38632D31362E392C342E312D31302E362C32352D31302E362C3235
                73302E342D392E372C382D31342E324333302E362C32312E342C33322E322C31
                392C33322E322C31392020732D312E342C332E382D392E342C392E37632D322E
                392C322E332D352E312C352E332D362E332C382E3763342E352D332E312C382E
                372D302E332C31352E362D352E324334302E392C32352E392C33382E332C382E
                352C33382E332C382E357A220A202020202069643D227061746837220A202020
                20207374796C653D2266696C6C3A233034383264663B66696C6C2D6F70616369
                74793A3122202F3E3C2F7376673E}
              Proportional = True
              Transparent = True
            end
          end
          object paFreshBooksHeaderLine1: TPanel
            AlignWithMargins = True
            Left = 10
            Top = 53
            Width = 641
            Height = 30
            Margins.Left = 10
            Margins.Top = 0
            Margins.Right = 10
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = 16644080
            ParentBackground = False
            TabOrder = 1
            object imFreshBooksHeaderCreditCardSecurityCode: TImage
              Left = 294
              Top = 0
              Width = 43
              Height = 30
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D2269736F2D383835392D31223F3E0D0A3C212D2D2047
                656E657261746F723A2041646F626520496C6C7573747261746F722031392E30
                2E302C20535647204578706F727420506C75672D496E202E2053564720566572
                73696F6E3A20362E3030204275696C6420302920202D2D3E0D0A3C7376672076
                657273696F6E3D22312E31222069643D224C617965725F312220786D6C6E733D
                22687474703A2F2F7777772E77332E6F72672F323030302F7376672220786D6C
                6E733A786C696E6B3D22687474703A2F2F7777772E77332E6F72672F31393939
                2F786C696E6B2220783D223070782220793D22307078220D0A09207669657742
                6F783D22302030203531322035313222207374796C653D22656E61626C652D62
                61636B67726F756E643A6E65772030203020353132203531323B2220786D6C3A
                73706163653D227072657365727665223E0D0A3C673E0D0A093C706174682073
                74796C653D2266696C6C3A233641424441303B2220643D224D3437362E36392C
                3434312E3337394833352E3331632D31392E352C302D33352E33312D31352E38
                312D33352E33312D33352E3331563130352E39333163302D31392E352C31352E
                38312D33352E33312C33352E33312D33352E33310D0A0909483437362E363963
                31392E352C302C33352E33312C31352E38312C33352E33312C33352E33317633
                30302E313338433531322C3432352E3536392C3439362E31392C3434312E3337
                392C3437362E36392C3434312E333739222F3E0D0A093C706F6C79676F6E2073
                74796C653D2266696C6C3A233438383537383B2220706F696E74733D22302C31
                39342E323037203531322C3139342E323037203531322C3132332E3538362030
                2C3132332E3538362009222F3E0D0A093C706F6C79676F6E207374796C653D22
                66696C6C3A234630433431393B2220706F696E74733D223330302E3133382C33
                38382E343134203435392E3033342C3338382E343134203435392E3033342C33
                30302E313338203330302E3133382C3330302E3133382009222F3E0D0A093C67
                3E0D0A09093C70617468207374796C653D2266696C6C3A233438383537383B22
                20643D224D3132332E3438332C3236342E3832384834342E313431632D342E38
                38322C302D382E3832382D332E3934362D382E3832382D382E38323873332E39
                34362D382E3832382C382E3832382D382E3832386837392E3334320D0A090909
                63342E3838322C302C382E3832382C332E3934362C382E3832382C382E383238
                533132382E3336352C3236342E3832382C3132332E3438332C3236342E383238
                222F3E0D0A09093C70617468207374796C653D2266696C6C3A23343838353738
                3B2220643D224D3233382E3334352C3236342E383238682D37392E333432632D
                342E3838322C302D382E3832382D332E3934362D382E3832382D382E38323873
                332E3934362D382E3832382C382E3832382D382E3832386837392E3334320D0A
                09090963342E3838322C302C382E3832382C332E3934362C382E3832382C382E
                383238533234332E3232362C3236342E3832382C3233382E3334352C3236342E
                383238222F3E0D0A09093C70617468207374796C653D2266696C6C3A23343838
                3537383B2220643D224D3137362E3535322C3330302E3133384834342E313338
                632D342E3838322C302D382E3832382D332E3934362D382E3832382D382E3832
                3873332E3934362D382E3832382C382E3832382D382E383238683133322E3431
                340D0A09090963342E3838322C302C382E3832382C332E3934362C382E383238
                2C382E383238533138312E3433332C3330302E3133382C3137362E3535322C33
                30302E313338222F3E0D0A09093C70617468207374796C653D2266696C6C3A23
                3438383537383B2220643D224D3233382E3334352C3330302E313338682D3236
                2E343833632D342E3838322C302D382E3832382D332E3934362D382E3832382D
                382E38323873332E3934362D382E3832382C382E3832382D382E383238683236
                2E3438330D0A09090963342E3838322C302C382E3832382C332E3934362C382E
                3832382C382E383238533234332E3232362C3330302E3133382C3233382E3334
                352C3330302E313338222F3E0D0A093C2F673E0D0A3C2F673E0D0A3C673E0D0A
                3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D
                0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E
                0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C67
                3E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C
                673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A3C673E0D0A3C2F673E0D0A
                3C2F7376673E0D0A}
              Proportional = True
              Transparent = True
            end
            object edFreshBooksHeaderCreditCard: TEdit
              AlignWithMargins = True
              Left = 0
              Top = 0
              Width = 217
              Height = 30
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 25
              Margins.Bottom = 0
              Align = alLeft
              PasswordChar = '*'
              TabOrder = 0
              Text = '1234 1234 1234 1324'
            end
            object edFreshBooksHeaderCreditCardSecurityCode: TEdit
              AlignWithMargins = True
              Left = 242
              Top = 0
              Width = 47
              Height = 30
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 5
              Margins.Bottom = 0
              Align = alLeft
              PasswordChar = '*'
              TabOrder = 1
              Text = '123'
            end
          end
          object paFreshBooksHeaderLine1Captions: TPanel
            AlignWithMargins = True
            Left = 10
            Top = 83
            Width = 641
            Height = 21
            Margins.Left = 10
            Margins.Top = 0
            Margins.Right = 10
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = 16644080
            ParentBackground = False
            TabOrder = 2
            object laFreshBooksHeaderCreditCardCaption: TLabel
              AlignWithMargins = True
              Left = 0
              Top = 0
              Width = 84
              Height = 16
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Caption = 'Card Number'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 14647812
              Font.Height = -13
              Font.Name = 'Verdana'
              Font.Style = []
              ParentFont = False
            end
            object laFreshBooksHeaderSecurityCodeCaption: TLabel
              AlignWithMargins = True
              Left = 244
              Top = 0
              Width = 93
              Height = 16
              Margins.Left = 160
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Caption = 'Security Code'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 14647812
              Font.Height = -13
              Font.Name = 'Verdana'
              Font.Style = []
              ParentFont = False
            end
          end
          object paFreshBooksHeaderLine2: TPanel
            AlignWithMargins = True
            Left = 10
            Top = 114
            Width = 641
            Height = 30
            Margins.Left = 10
            Margins.Top = 10
            Margins.Right = 10
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = 16644080
            ParentBackground = False
            TabOrder = 3
            object laFreshBooksHeaderSlash: TLabel
              AlignWithMargins = True
              Left = 298
              Top = 0
              Width = 6
              Height = 16
              Margins.Top = 0
              Margins.Bottom = 0
              Align = alLeft
              Caption = '/'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 14647812
              Font.Height = -13
              Font.Name = 'Verdana'
              Font.Style = []
              ParentFont = False
            end
            object edFreshBooksHeaderName: TEdit
              AlignWithMargins = True
              Left = 0
              Top = 0
              Width = 217
              Height = 30
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 25
              Margins.Bottom = 0
              Align = alLeft
              TabOrder = 0
            end
            object dpFreshBooksHeaderExpDateYear: TDateTimePicker
              AlignWithMargins = True
              Left = 310
              Top = 0
              Width = 55
              Height = 30
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Date = 42837.000000000000000000
              Format = 'yyyy'
              Time = 42837.000000000000000000
              DateMode = dmUpDown
              TabOrder = 1
            end
            object dpFreshBooksHeaderExpDateMonth: TDateTimePicker
              AlignWithMargins = True
              Left = 245
              Top = 0
              Width = 50
              Height = 30
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Date = 42837.000000000000000000
              Format = 'M'
              Time = 42837.000000000000000000
              DateMode = dmUpDown
              TabOrder = 2
            end
          end
          object paFreshBooksHeaderLine2Captions: TPanel
            AlignWithMargins = True
            Left = 10
            Top = 144
            Width = 641
            Height = 21
            Margins.Left = 10
            Margins.Top = 0
            Margins.Right = 10
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = 16644080
            ParentBackground = False
            TabOrder = 4
            object laFreshBooksHeaderFullName: TLabel
              AlignWithMargins = True
              Left = 0
              Top = 0
              Width = 63
              Height = 16
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Caption = 'Full Name'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 14647812
              Font.Height = -13
              Font.Name = 'Verdana'
              Font.Style = []
              ParentFont = False
            end
            object laFreshBooksHeaderExpDate: TLabel
              AlignWithMargins = True
              Left = 244
              Top = 0
              Width = 64
              Height = 16
              Margins.Left = 181
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Caption = 'Exp. Date'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 14647812
              Font.Height = -13
              Font.Name = 'Verdana'
              Font.Style = []
              ParentFont = False
            end
          end
          object paFreshBooksHeaderLine3: TPanel
            AlignWithMargins = True
            Left = 10
            Top = 175
            Width = 641
            Height = 30
            Margins.Left = 10
            Margins.Top = 10
            Margins.Right = 10
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = 16644080
            ParentBackground = False
            TabOrder = 5
            object imFreshBooksHeaderVisa: TImage
              AlignWithMargins = True
              Left = 280
              Top = 0
              Width = 50
              Height = 30
              Margins.Left = 280
              Margins.Top = 0
              Margins.Right = 5
              Margins.Bottom = 0
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                223F3E0A3C212D2D2043726561746564207769746820496E6B73636170652028
                687474703A2F2F7777772E696E6B73636170652E6F72672F29202D2D3E0A0A3C
                7376670A202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F7267
                2F64632F656C656D656E74732F312E312F220A202020786D6C6E733A63633D22
                687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A
                202020786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F
                313939392F30322F32322D7264662D73796E7461782D6E7323220A202020786D
                6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F323030302F
                737667220A202020786D6C6E733D22687474703A2F2F7777772E77332E6F7267
                2F323030302F737667220A202020786D6C6E733A736F6469706F64693D226874
                74703A2F2F736F6469706F64692E736F75726365666F7267652E6E65742F4454
                442F736F6469706F64692D302E647464220A202020786D6C6E733A696E6B7363
                6170653D22687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D
                657370616365732F696E6B7363617065220A20202077696474683D2234333022
                0A2020206865696768743D22333030220A20202076696577426F783D22302030
                20343330203330302E3030303031220A20202069643D2273766734323231220A
                20202076657273696F6E3D22312E31220A202020696E6B73636170653A766572
                73696F6E3D22302E393120723133373235220A202020736F6469706F64693A64
                6F636E616D653D2276697361322E737667223E0A20203C646566730A20202020
                2069643D22646566733432323322202F3E0A20203C736F6469706F64693A6E61
                6D6564766965770A202020202069643D2262617365220A202020202070616765
                636F6C6F723D2223666666666666220A2020202020626F72646572636F6C6F72
                3D2223363636363636220A2020202020626F726465726F7061636974793D2231
                2E30220A2020202020696E6B73636170653A706167656F7061636974793D2230
                2E30220A2020202020696E6B73636170653A70616765736861646F773D223222
                0A2020202020696E6B73636170653A7A6F6F6D3D22312E393739383939220A20
                20202020696E6B73636170653A63783D223230332E3735373537220A20202020
                20696E6B73636170653A63793D223138372E3032333931220A2020202020696E
                6B73636170653A646F63756D656E742D756E6974733D227078220A2020202020
                696E6B73636170653A63757272656E742D6C617965723D226C6179657231220A
                202020202073686F77677269643D2266616C7365220A2020202020756E697473
                3D227078220A2020202020696E6B73636170653A73686F777061676573686164
                6F773D2266616C7365220A2020202020696E6B73636170653A77696E646F772D
                77696474683D2231393230220A2020202020696E6B73636170653A77696E646F
                772D6865696768743D2231303238220A2020202020696E6B73636170653A7769
                6E646F772D783D222D38220A2020202020696E6B73636170653A77696E646F77
                2D793D222D38220A2020202020696E6B73636170653A77696E646F772D6D6178
                696D697A65643D223122202F3E0A20203C6D657461646174610A202020202069
                643D226D6574616461746134323236223E0A202020203C7264663A5244463E0A
                2020202020203C63633A576F726B0A2020202020202020207264663A61626F75
                743D22223E0A20202020202020203C64633A666F726D61743E696D6167652F73
                76672B786D6C3C2F64633A666F726D61743E0A20202020202020203C64633A74
                7970650A20202020202020202020207264663A7265736F757263653D22687474
                703A2F2F7075726C2E6F72672F64632F64636D69747970652F5374696C6C496D
                61676522202F3E0A20202020202020203C64633A7469746C65202F3E0A202020
                2020203C2F63633A576F726B3E0A202020203C2F7264663A5244463E0A20203C
                2F6D657461646174613E0A20203C670A2020202020696E6B73636170653A6C61
                62656C3D224C617965722031220A2020202020696E6B73636170653A67726F75
                706D6F64653D226C61796572220A202020202069643D226C6179657231220A20
                202020207472616E73666F726D3D227472616E736C61746528302C2D3735322E
                3336323229223E0A202020203C726563740A202020202020207374796C653D22
                6F7061636974793A313B66696C6C3A236666666666663B66696C6C2D6F706163
                6974793A302E38303932343835363B66696C6C2D72756C653A6E6F6E7A65726F
                3B7374726F6B653A236339633963393B7374726F6B652D77696474683A342E37
                393036303734353B7374726F6B652D6C696E656361703A726F756E643B737472
                6F6B652D6D697465726C696D69743A343B7374726F6B652D6461736861727261
                793A6E6F6E653B7374726F6B652D646173686F66667365743A303B7374726F6B
                652D6F7061636974793A31220A2020202020202069643D227265637434383037
                220A2020202020202077696474683D223431352E3230393338220A2020202020
                20206865696768743D223238352E3230393338220A20202020202020783D2237
                2E33393533303337220A20202020202020793D223735392E3735373531220A20
                20202020202072783D22342E39373032393738220A2020202020202072793D22
                352E3230313238363822202F3E0A202020203C670A2020202020202069643D22
                6734373939220A202020202020207472616E73666F726D3D226D617472697828
                312E303336343130382C302C302C312E303336343130382C2D32372E33363439
                38392C2D3134342E333436313729223E0A2020202020203C706174680A202020
                202020202020643D226D203236342E34313533312C3937352E38313037392063
                2031362E38332C2D362E34362033352E39322C2D352E39372035322E38342C2D
                302E3035202D312E33332C382E3031202D322E37332C31362E3031202D342E30
                382C32342E3032202D392E31392C2D342E3435202D31392E36362C2D362E3436
                202D32392E38332C2D352E3138202D342E34332C302E3632202D392E34332C32
                2E3332202D31312E35312C362E3634303031202D312E32372C322E383420302E
                33392C362E303320322E36372C372E373920382E332C362E35342031392E3034
                2C382E39382032372E31392C31352E373720362E35342C352031312E35312C31
                322E37332031312E31332C32312E323220302E34362C31302E3733202D352E30
                392C32312E3232202D31332E37382C32372E3339202D31312E312C372E393720
                2D32352E31322C31302E3333202D33382E35312C31302E3437202D31302E3936
                2C2D302E3631202D32322E32312C2D312E3632202D33322E34332C2D352E3932
                20312C2D382E323820322E37342C2D31362E343720342E30342C2D32342E3732
                2031312E34312C352E34392032342E32312C382E38332033362E39312C372E30
                3820332E39312C2D302E3720382E30332C2D322E30322031302E36392C2D352E
                313420322E31332C2D322E3420322E35322C2D362E323920302E35392C2D382E
                3931202D322E38372C2D34202D372E35332C2D362E3035202D31312E37382C2D
                382E32202D392E30392C2D342E3232202D31382E36322C2D392E3035202D3234
                2E33382C2D31372E36202D352E33382C2D372E3735202D342E38362C2D31382E
                3239202D302E38332C2D32362E353230303120342E31342C2D382E3635203132
                2E33342C2D31342E36342032312E30372C2D31382E3134206C20302C3020302C
                30206320302C3020302C3020302C30207A220A20202020202020202069643D22
                7376675F34220A202020202020202020696E6B73636170653A636F6E6E656374
                6F722D6375727661747572653D2230220A2020202020202020207374796C653D
                2266696C6C3A2330303434383922202F3E0A2020202020203C706174680A2020
                20202020202020643D226D2035312E3533353331342C3937332E323330373920
                632031352E36382C302E30322033312E33362C302E30312034372E30342C2D30
                2E303120332E3935393939362C2D302E303320382E3331393939362C302E3635
                2031312E3230393939362C332E363120322E34352C322E343320332E312C352E
                393620332E37362C392E323120332E31312C31352E353830303120362E35352C
                33312E313130303120392E36322C34362E3730303031202D372E38342C2D3139
                2E3239202D32322E37372C2D33352E3239303031202D34312E3035393939362C
                2D34352E3132303031202D392E362C2D352E33202D31392E39352C2D392E3136
                202D33302E35382C2D31312E383220302C2D302E363420302E30312C2D312E39
                3320302E30312C2D322E3537207A220A20202020202020202069643D22737667
                5F35220A202020202020202020696E6B73636170653A636F6E6E6563746F722D
                6375727661747572653D2230220A2020202020202020207374796C653D226669
                6C6C3A2366666161303022202F3E0A2020202020203C706174680A2020202020
                20202020643D226D203135342E39373533312C3937332E313830373920632031
                302E35392C2D302E31382032312E31372C2D302E30332033312E37362C2D302E
                3038202D31352E33362C33362E3335303031202D33312E32332C37322E343830
                3031202D34362E36342C3130382E3831303031202D31302E35382C302E313820
                2D32312E31372C302E3034202D33312E37362C302E3038202D382E3733393939
                362C2D33312E3436202D31372E3638393939362C2D36322E3836202D32362E32
                32393939362C2D39342E33373030312031382E3238393939362C392E38332033
                332E3231393939362C32352E38333030312034312E3035393939362C34352E31
                32303031206C20302E31332C302E3235206320302E39372C342E3620312E3934
                2C392E323120322E37362C31332E383520392E37382C2D32342E352031392E32
                332C2D34392E31333030312032382E39322C2D37332E3636303031206C20302C
                3020302C3020302C30206320302C3020302C3020302C30207A220A2020202020
                2020202069643D227376675F36220A202020202020202020696E6B7363617065
                3A636F6E6E6563746F722D6375727661747572653D2230220A20202020202020
                20207374796C653D2266696C6C3A2330303434383922202F3E0A202020202020
                3C706174680A202020202020202020643D226D203139382E33383533312C3937
                332E3039303739206320392E39352C302031392E39312C302E30352032392E38
                362C2D302E3032202D362C33362E3333303031202D31322E332C37322E363130
                3031202D31382E352C3130382E3931303031202D392E39392C302E3032202D31
                392E39382C30202D32392E39372C302E303120362E32322C2D33362E33203132
                2E332C2D37322E36322031382E36312C2D3130382E3930303031206C20302C30
                20302C3020302C30207A220A20202020202020202069643D227376675F37220A
                202020202020202020696E6B73636170653A636F6E6E6563746F722D63757276
                61747572653D2230220A2020202020202020207374796C653D2266696C6C3A23
                30303434383922202F3E0A2020202020203C706174680A202020202020202020
                69643D227061746834313434220A202020202020202020643D226D203339332E
                31373533312C3937332E31353037392063202D382E34392C2D302E3031202D31
                362E39392C2D302E3231202D32352E34372C302E3038202D352E36322C302E32
                202D31312E312C332E3338202D31332E312C382E38202D31342E38322C33332E
                3337303031202D32392E35352C36362E3739303031202D34342E32382C313030
                2E32303030312031302E35312C302E30352032312E30312C2D302E3031203331
                2E35322C302E303320322E30342C2D352E343320342E31392C2D31302E383120
                362E31352C2D31362E32372031322E36322C2D302E31352032352E32362C2D30
                2E32352033372E38382C302E303620312E30382C352E343120322E33352C3130
                2E373920332E35322C31362E313920392E32362C302E30312031382E35322C30
                2E30322032372E37382C2D302E3031202D372E38332C2D33362E34202D31362E
                30312C2D37322E3732202D32342C2D3130392E3038303031207A206D202D3336
                2E33342C36392E3930303031206320352E31352C2D31332E31332031302E332C
                2D32362E32362031352E32362C2D33392E343720332E31312C31332E31322035
                2E38322C32362E333420382E372C33392E3532202D372E39392C2D302E303220
                2D31352E39382C302E3035202D32332E39362C2D302E3035207A220A20202020
                2020202020696E6B73636170653A636F6E6E6563746F722D6375727661747572
                653D2230220A2020202020202020207374796C653D2266696C6C3A2330303434
                383922202F3E0A2020202020203C706174680A202020202020202020643D224D
                2035302E3532353331342C3933352E3939303739220A20202020202020202069
                643D227376675F3131220A202020202020202020696E6B73636170653A636F6E
                6E6563746F722D6375727661747572653D2230220A2020202020202020207374
                796C653D2266696C6C3A2366666666666622202F3E0A202020203C2F673E0A20
                203C2F673E0A3C2F7376673E0A}
              Proportional = True
              Transparent = True
            end
            object imFreshBooksHeaderMasterCard: TImage
              AlignWithMargins = True
              Left = 335
              Top = 0
              Width = 43
              Height = 30
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                223F3E0A3C212D2D2043726561746564207769746820496E6B73636170652028
                687474703A2F2F7777772E696E6B73636170652E6F72672F29202D2D3E0A0A3C
                7376670A202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F7267
                2F64632F656C656D656E74732F312E312F220A202020786D6C6E733A63633D22
                687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A
                202020786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F
                313939392F30322F32322D7264662D73796E7461782D6E7323220A202020786D
                6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F323030302F
                737667220A202020786D6C6E733D22687474703A2F2F7777772E77332E6F7267
                2F323030302F737667220A202020786D6C6E733A736F6469706F64693D226874
                74703A2F2F736F6469706F64692E736F75726365666F7267652E6E65742F4454
                442F736F6469706F64692D302E647464220A202020786D6C6E733A696E6B7363
                6170653D22687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D
                657370616365732F696E6B7363617065220A20202077696474683D2234333022
                0A2020206865696768743D22333030220A20202076696577426F783D22302030
                20343330203330302E3030303031220A20202069643D2273766734323231220A
                20202076657273696F6E3D22312E31220A202020696E6B73636170653A766572
                73696F6E3D22302E393120723133373235220A202020736F6469706F64693A64
                6F636E616D653D226D617374657263617264322E737667223E0A20203C646566
                730A202020202069643D22646566733432323322202F3E0A20203C736F646970
                6F64693A6E616D6564766965770A202020202069643D2262617365220A202020
                202070616765636F6C6F723D2223666666666666220A2020202020626F726465
                72636F6C6F723D2223363636363636220A2020202020626F726465726F706163
                6974793D22312E30220A2020202020696E6B73636170653A706167656F706163
                6974793D22302E30220A2020202020696E6B73636170653A7061676573686164
                6F773D2232220A2020202020696E6B73636170653A7A6F6F6D3D22312E393739
                383939220A2020202020696E6B73636170653A63783D223230332E3735373537
                220A2020202020696E6B73636170653A63793D223136362E3832303836220A20
                20202020696E6B73636170653A646F63756D656E742D756E6974733D22707822
                0A2020202020696E6B73636170653A63757272656E742D6C617965723D226C61
                79657231220A202020202073686F77677269643D2266616C7365220A20202020
                20756E6974733D227078220A2020202020696E6B73636170653A73686F777061
                6765736861646F773D2266616C7365220A2020202020696E6B73636170653A77
                696E646F772D77696474683D2231393230220A2020202020696E6B7363617065
                3A77696E646F772D6865696768743D2231303238220A2020202020696E6B7363
                6170653A77696E646F772D783D222D38220A2020202020696E6B73636170653A
                77696E646F772D793D222D38220A2020202020696E6B73636170653A77696E64
                6F772D6D6178696D697A65643D223122202F3E0A20203C6D657461646174610A
                202020202069643D226D6574616461746134323236223E0A202020203C726466
                3A5244463E0A2020202020203C63633A576F726B0A2020202020202020207264
                663A61626F75743D22223E0A20202020202020203C64633A666F726D61743E69
                6D6167652F7376672B786D6C3C2F64633A666F726D61743E0A20202020202020
                203C64633A747970650A20202020202020202020207264663A7265736F757263
                653D22687474703A2F2F7075726C2E6F72672F64632F64636D69747970652F53
                74696C6C496D61676522202F3E0A20202020202020203C64633A7469746C6520
                2F3E0A2020202020203C2F63633A576F726B3E0A202020203C2F7264663A5244
                463E0A20203C2F6D657461646174613E0A20203C670A2020202020696E6B7363
                6170653A6C6162656C3D224C617965722031220A2020202020696E6B73636170
                653A67726F75706D6F64653D226C61796572220A202020202069643D226C6179
                657231220A20202020207472616E73666F726D3D227472616E736C6174652830
                2C2D3735322E3336323229223E0A202020203C726563740A2020202020202073
                74796C653D226F7061636974793A313B66696C6C3A236666666666663B66696C
                6C2D6F7061636974793A302E38303738343331353B66696C6C2D72756C653A6E
                6F6E7A65726F3B7374726F6B653A236339633963393B7374726F6B652D776964
                74683A342E37393036303834313B7374726F6B652D6C696E656361703A726F75
                6E643B7374726F6B652D6D697465726C696D69743A343B7374726F6B652D6461
                736861727261793A6E6F6E653B7374726F6B652D646173686F66667365743A30
                3B7374726F6B652D6F7061636974793A31220A2020202020202069643D227265
                637434383037220A2020202020202077696474683D223431352E323039333822
                0A202020202020206865696768743D223238352E3230393338220A2020202020
                2020783D22372E33393533303432220A20202020202020793D223735392E3735
                373531220A2020202020202072783D22342E39373032393738220A2020202020
                202072793D22352E3230313238373722202F3E0A202020203C670A2020202020
                202069643D226734383738220A202020202020207472616E73666F726D3D226D
                617472697828312E323739393935332C302C302C312E323739393935332C3139
                382E31393131322C2D3138372E333531333529223E0A2020202020203C706174
                680A202020202020202020643D226D202D36352E3333303939342C3736342E34
                33303732206320372E34372C2D312E37332031342E35392C2D312E3935203232
                2E32322C2D312E38362032302E3539393939352C2D302E32322034312E313339
                393935312C392E31392035362E3337393939352C32322E37332033372E392C33
                332E31372032392E30382C33392E34372032372E39352C35342E323420302E32
                322C302E333920302E32372C312E323720302E32392C322E3136202D352E3333
                2C31322E3132202D352E36392C32382E3133202D302E36352C32382E32362031
                2E34312C33382E3131202D32362E30392C34352E3838202D32372E36322C3437
                2E3331202D31342E30323939393939312C31322E3437202D33312E39392C3231
                2E3032202D35302E3832393939352C32322E3238202D31312E34312C312E3035
                202D31392E33392C302E3737202D32382E38312C2D312E3635202D32342E3339
                2C2D352E3437202D34352E3933393939362C2D32312E3933202D35372E393239
                3939362C2D34332E3832202D31352E38342C2D32382E3135202D31342E34382C
                2D36352E303820332E37332C2D39312E38322031322E362C2D31392E31322033
                322E3834393939362C2D33332E30362035352E3236393939362C2D33372E3833
                207A220A20202020202020202069643D227376675F32220A2020202020202020
                20696E6B73636170653A636F6E6E6563746F722D6375727661747572653D2230
                220A2020202020202020207374796C653D2266696C6C3A236666303030342220
                2F3E0A2020202020203C706174680A202020202020202020643D226D2034392E
                3537393030312C3833332E36383037322037322E3939393939392C2D342E3720
                2D322E312C35312E35202D37302E3839393939392C302063202D31342E392C2D
                31302E38202D31362E3639392C2D33362E3620302C2D34362E38207A220A2020
                2020202020202069643D227376675F3236220A202020202020202020696E6B73
                636170653A636F6E6E6563746F722D6375727661747572653D2230220A202020
                2020202020207374796C653D2266696C6C3A2366666666666622202F3E0A2020
                202020203C706174680A202020202020202020643D226D2035312E3837393030
                312C3736342E3738303732206320372E38392C2D322E31312031342E39382C2D
                322E30342032332E30342C2D322E313920342E36322C2D302E30352031302E38
                372C302E36392031352E34322C312E36322031392E3436393939392C332E3939
                2033372E3535393939392C31342E36342035302E3334393939392C32392E3837
                2031322E35332C31342E38342032302E32332C33332E38342032302E37372C35
                332E333220302E39342C31382E3437202D342E31312C33372E3134202D31342E
                33352C35322E3535202D31332E34322C32302E3833202D33362E30332C33352E
                3534202D36302E3534393939392C33392E3139202D382E36372C302E3435202D
                31362E31382C312E3534202D32362E30352C302E3138202D31372E35342C2D32
                202D33342E32392C2D31302E3035202D34372E32372C2D32312E393520312E35
                332C2D312E343320332E33332C2D332E3120342E38332C2D342E3536206C202D
                392E313239393939392C2D302E3033202D342E39352C2D352E35322031382E33
                3539393939392C302E313920342E30332C2D352E3234202D32362E3531393939
                3939312C2D302E3432202D332E32333939393939392C2D342E35372033322E32
                3839393939392C2D302E313320332E3432322C2D352E36202D33382E39343139
                3939392C302E32202D322E342C2D342E393920632031332E38372C2D302E3238
                2032392E353532393939392C302034332E343232393939392C2D302E31206C20
                322E3431372C2D352E3433202D34372E35372C302E3034202D322E30382C2D35
                2E31352035312E34362C2D302E313520322E31322C2D352E38352063202D352E
                30342C2D302E3133202D31302E33382C2D302E3134202D31352E34312C2D302E
                303320302E32392C2D312E363620302E35352C2D332E333320302E38322C2D35
                20342E32372C302E303120382E35352C302E30312031322E38342C302E303220
                342E322C382E36332031352E38392C31312E30372032332E39382C362E383620
                302E37372C2D322E303320312E30372C2D342E313920312E372C2D362E323720
                6C20302E34322C2D312E3934206320302E30352C342E353320312E37322C382E
                303520342E36312C392E343220342E35372C322E323820392E39382C302E3937
                2031342E322C2D312E3520302E322C302E353920302E36312C312E373620302E
                38312C322E333520322E34352C2D302E303420342E39312C2D302E303620372E
                33372C2D302E303620312E33382C2D372E3620322E39382C2D31352E31352034
                2E33352C2D32322E373420312E31322C2D342E3339202D322E35312C2D382E33
                34202D362E35332C2D392E3433202D352E34312C2D312E3339202D31312E3338
                2C2D312E3238202D31362E36382C302E35202D312E36342C322E3137202D312E
                392C352E3032202D322E37352C372E353320342E35382C2D302E383420392E33
                312C2D312E37352031332E39372C2D302E393720312E39332C302E323420322E
                30392C322E323520322E32382C332E3935202D352E35322C302E3534202D3130
                2E34352C2D302E3138202D31352E34392C322E3631202D332E31322C312E3620
                2D342E37382C332E3336202D352E38372C38202D322E38372C312E3136202D35
                2E36392C322E3934202D382E39322C322E3534202D342E33322C2D302E303220
                2D382E37352C2D322E3933202D392E32362C2D372E3436202D302E35322C2D35
                2E353720312E32382C2D31312E393720352E38392C2D31352E343620342E3632
                2C2D332E32352031302E34372C2D312E35352031352E332C302E323320302E34
                312C2D322E383220302E37392C2D352E363420312E30392C2D382E3438202D39
                2E32392C2D332E33202D32322E32342C2D312E3832202D32382E30372C372E31
                37206C202D302E31392C2D322E32362063202D31382E35372C2D302E3037202D
                33372E313439393939392C302E3033202D35352E37322C2D302E303920302E32
                362C2D312E373520302E35332C2D332E353120302E37392C2D352E3236206C20
                35342E35332C302E3335202D312E34352C2D352E3735202D35322E39312C2D30
                2E313320312E39372C2D352E30392034392E37342C2D302E3035202D322E3237
                352C2D352E35392063202D31332E37352C302E3037202D33312E393034393939
                392C302E3037202D34352E363534393939392C302E3034206C20332E3032362C
                2D342E393320632031332E31362C2D302E32342032372E313433393939392C30
                2E30372034302E333233393939392C2D302E31206C202D332E37342C2D352E35
                322063202D31312E30312C2D302E31202D32322E393239393939392C302E3138
                202D33332E393339393939392C2D302E3036206C20332E36343939393939392C
                2D342E38372032372E38323939393939312C2D302E3035202D342E3536362C2D
                352E3736202D32302E303733393939392C2D302E3220332E37372C2D352E3135
                2031322E363839393939392C302E31382063202D312E392C2D322E3135202D33
                2E38332C2D342E3237202D352E39342C2D362E32312031302E38352C2D31302E
                32332032342E31352C2D31372E31382033382E36312C2D32302E3632206C2030
                2C3020302C3020302C3020302C30207A220A20202020202020202069643D2273
                76675F33220A202020202020202020696E6B73636170653A636F6E6E6563746F
                722D6375727661747572653D2230220A2020202020202020207374796C653D22
                66696C6C3A2366666161303022202F3E0A2020202020203C706174680A202020
                202020202020643D226D202D3132372E30333039392C3837332E373630373220
                6320322E35382C2D31332E313920342E33392C2D32362E353320372E312C2D33
                392E363920342E36342C302E3120392E32372C302E30382031332E39312C302E
                303120302E39362C372E353120312E30342C31352E303820312E33332C32322E
                363320332E35342C2D372E383820362E3436393939362C2D31352E3136203130
                2E3034393939362C2D32322E353820342E39332C302E303420392E38372C2D30
                2E31352031342E382C302E3139202D322E33382C31332E3136202D342E35322C
                32362E3337202D372E31312C33392E35202D322E37332C2D302E3035202D352E
                34362C2D302E3035202D382E31382C2D302E303420312E37362C2D392E382033
                2E36342C2D31392E353820352E322C2D32392E3431202D342E35382C392E3620
                2D382E31312C31392E3636202D31322E3338393939362C32392E34206C202D39
                2E33372C302E3035202D302E32342C2D332E33372063202D302E33342C2D382E
                3735202D302E392C2D31372E3439202D312E34352C2D32362E3233206C202D35
                2E32312C32392E3631202D382E34342C2D302E3037207A220A20202020202020
                202069643D227376675F3132220A202020202020202020696E6B73636170653A
                636F6E6E6563746F722D6375727661747572653D2230220A2020202020202020
                207374796C653D2266696C6C3A2366666666666622202F3E0A2020202020203C
                706174680A202020202020202020643D226D203134302E3736392C3833342E38
                33303732206320322E37362C2D302E333320352E35352C2D302E313420382E33
                332C2D302E3033202D322E37372C31322E3935202D342E34362C32362E31202D
                372E31312C33392E3038202D322E36382C2D302E3032202D352E35362C2D302E
                3032202D382E32322C302E303120302E30332C2D302E3820302E34392C2D322E
                343120302E35322C2D332E3231202D322E342C322E3033202D352E33372C342E
                3031202D382E372C332E3538202D372E31382C2D302E35202D31312E32332C2D
                362E3631202D31302E38372C2D31322E3732202D302E37372C2D392E31312035
                2E38312C2D32302E322031352E38372C2D31392E383320332E342C302E303220
                352E35362C322E303320382E30332C332E3820302E37342C2D332E353720312E
                33342C2D372E303320322E31352C2D31302E3638207A220A2020202020202020
                2069643D227376675F3133220A202020202020202020696E6B73636170653A63
                6F6E6E6563746F722D6375727661747572653D2230220A202020202020202020
                7374796C653D2266696C6C3A2366666666666622202F3E0A2020202020203C70
                6174680A202020202020202020643D226D202D37352E3537303939342C383432
                2E3238303732206320362E36352C2D312E36332031342E36332C2D322E353820
                32302E35322C312E363620332E33352C322E333820322E33392C362E39322031
                2E39322C31302E34202D312E32392C362E3438202D322E33392C31332E303120
                2D332E38382C31392E3436202D322E33352C2D302E3033202D342E37312C2D30
                2E3031202D372E30352C302E3035202D302E30342C2D302E3732202D302E3131
                2C2D322E3137202D302E31342C2D322E39202D342E33362C332E3036202D3130
                2E36382C342E3732202D31352E34312C312E3538202D342E38392C2D332E3537
                202D342E32312C2D31312E3420302C2D31352E323420342E37322C2D342E3638
                2031312E35362C2D332E38392031372E36392C2D342E313120302E32342C2D31
                2E353920302E32312C2D342E3036202D312E38312C2D342E3531202D342E3639
                2C2D312E3535202D392E36342C302E3332202D31342E33392C302E373420302E
                382C2D322E333820312E32362C2D342E393420322E35352C2D372E3133207A22
                0A20202020202020202069643D227376675F3134220A20202020202020202069
                6E6B73636170653A636F6E6E6563746F722D6375727661747572653D2230220A
                2020202020202020207374796C653D2266696C6C3A2366666666666622202F3E
                0A2020202020203C706174680A202020202020202020643D226D202D34332E35
                32303939342C3834312E3635303732206320352E30382C2D312E32322031302E
                3632393939352C2D312E30392031352E3734393939352C2D302E3232206C202D
                312E32342C362E38362063202D332E38362C2D302E3038202D382E3335393939
                352C2D312E3733202D31312E3732393939352C302E3736202D312E32392C312E
                343720302E30332C332E353820312E34372C342E323720322E39392C312E3620
                362E3930393939352C312E393220382E3930393939352C3520322E38382C352E
                3436202D302E30362C31332E3236202D362E3234393939352C31342E3831202D
                352E37382C312E3337202D31312E39342C302E3939202D31372E36392C2D302E
                333820302E33382C2D322E333320302E38372C2D342E363420312E33372C2D36
                2E393520342E30312C302E393520382E32372C312E39312031322E33372C302E
                393220312E362C2D302E373420322E32362C2D322E323620312E36312C2D332E
                3931202D332E30312C2D322E3932202D372E39362C2D322E3934202D31302E35
                2C2D362E3437202D322E39382C2D352E3233202D302E33322C2D31332E342035
                2E39332C2D31342E3639206C20302C30207A220A20202020202020202069643D
                227376675F3135220A202020202020202020696E6B73636170653A636F6E6E65
                63746F722D6375727661747572653D2230220A2020202020202020207374796C
                653D2266696C6C3A2366666666666622202F3E0A2020202020203C706174680A
                202020202020202020643D226D203130382E3833392C3834362E333830373220
                6320322E322C2D322E393620352E33312C2D352E333520392E32322C2D342E39
                202D302E35352C322E3338202D312E30372C342E3736202D312E35352C372E31
                34202D322E34372C302E3534202D352E32362C302E3831202D372E31312C322E
                3734202D312E39362C312E3734202D322E33392C342E3438202D322E38362C36
                2E3932202D302E39392C352E3131202D312E34332C31302E3333202D322E3839
                2C31352E3334202D322E37332C302E3235202D352E3437393939392C302E3136
                202D382E3230393939392C302E313820312E36322C2D31302E373620332E3531
                2C2D32312E343920352E3532393939392C2D33322E313820322E37332C302E30
                3220352E34372C302E303120382E32312C2D302E3032202D302E31332C312E35
                39202D302E32342C332E3138202D302E33342C342E3738206C20302C3020302C
                30206320302C3020302C3020302C30207A220A20202020202020202069643D22
                7376675F3138220A202020202020202020696E6B73636170653A636F6E6E6563
                746F722D6375727661747572653D2230220A2020202020202020207374796C65
                3D2266696C6C3A2366666666666622202F3E0A2020202020203C706174680A20
                2020202020202020643D226D203132372E3137392C3835312E30323037322063
                20312E35392C2D322E323220342E36382C2D322E373720372E31342C2D312E37
                3420362E32342C342E3033202D312E35332C31382E3638202D352E39312C3137
                2E38202D382E33312C2D302E3139202D352E31392C2D31312E3632202D312E32
                332C2D31362E3036207A220A20202020202020202069643D227376675F323022
                0A202020202020202020696E6B73636170653A636F6E6E6563746F722D637572
                7661747572653D2230220A2020202020202020207374796C653D2266696C6C3A
                2366666161303022202F3E0A2020202020203C706174680A2020202020202020
                20643D226D202D31372E3431303939392C3835352E3233303732206320302E30
                342C2D302E323520302E31332C2D302E373720302E31372C2D312E3033206C20
                352E34392C302E31322063202D302E32312C312E3633202D302E33352C332E32
                37202D302E34322C342E3931206C202D342E35362C302E3032206320302C3020
                2D302E36382C2D342E3032202D302E36382C2D342E3032207A220A2020202020
                2020202069643D227376675F3231220A202020202020202020696E6B73636170
                653A636F6E6E6563746F722D6375727661747572653D2230220A202020202020
                2020207374796C653D2266696C6C3A2366666161303022202F3E0A2020202020
                203C706174680A202020202020202020643D226D2032372E3936393030312C38
                35342E3133303732206320332E32362C302E303720362E35322C302E30392039
                2E37392C302E31202D302E312C312E3635202D302E31342C332E3331202D302E
                312C342E3937202D332E35322C2D302E3035202D372E30342C2D302E3037202D
                31302E35362C2D302E303620302E32362C2D312E363820302E35332C2D332E33
                3520302E38372C2D352E3031207A220A20202020202020202069643D22737667
                5F3232220A202020202020202020696E6B73636170653A636F6E6E6563746F72
                2D6375727661747572653D2230220A2020202020202020207374796C653D2266
                696C6C3A2366666161303022202F3E0A2020202020203C706174680A20202020
                2020202020643D226D202D37332E3238303939342C3836322E33373037322063
                20322E32352C2D332E343920362E382C2D332E38322031302E35362C2D332E36
                38202D302E33392C322E34202D302E36382C352E3134202D322E37312C362E37
                38202D322C322E3033202D362E33322C322E3831202D372E35372C312E353320
                2D302E39332C2D302E39202D312E34392C2D332E3238202D302E32382C2D342E
                3633206C20302C3020302C30206320302C3020302C3020302C30207A220A2020
                2020202020202069643D227376675F3233220A202020202020202020696E6B73
                636170653A636F6E6E6563746F722D6375727661747572653D2230220A202020
                2020202020207374796C653D2266696C6C3A2366663030303422202F3E0A2020
                202020203C706174680A202020202020202020643D226D2037352E3936393030
                312C3836312E3736303732206320322E33352C2D332E313720362E36322C2D32
                2E39392031302E31342C2D332E3037202D302E32382C322E34202D302E34382C
                352E3139202D322E36312C362E3736202D312E39362C322E3131202D362E3537
                2C322E3439202D372E36322C312E3631202D322E32362C2D312E3136202D312E
                35352C2D332E383320302E30392C2D352E33207A220A20202020202020202069
                643D227376675F3234220A202020202020202020696E6B73636170653A636F6E
                6E6563746F722D6375727661747572653D2230220A2020202020202020207374
                796C653D2266696C6C3A2366666161303022202F3E0A2020202020203C706174
                680A202020202020202020643D226D2032372E3039393030312C3835392E3134
                303732206320332E35322C2D302E303120372E30342C302E30312031302E3536
                2C302E303620302E32352C312E393920302E37312C332E393520312E33352C35
                2E3835202D342E32392C2D302E3031202D382E35372C2D302E3031202D31322E
                38342C2D302E303220302E33312C2D312E393620302E36342C2D332E39332030
                2E39332C2D352E3839207A220A20202020202020202069643D227376675F3235
                220A202020202020202020696E6B73636170653A636F6E6E6563746F722D6375
                727661747572653D2230220A2020202020202020207374796C653D2266696C6C
                3A2366663030303422202F3E0A2020202020203C670A20202020202020202074
                72616E73666F726D3D227472616E736C617465282D3534312E3532312C373631
                2E333830373229220A20202020202020202069643D227376675F3331223E0A20
                202020202020203C706174680A2020202020202020202020643D226D20353337
                2E34332C38312E36206320352E34382C2D322E38342031322E39392C2D332E31
                342031372E39342C302E393420342E342C342E313520332E32332C31302E3639
                20322E322C31352E3939202D362E36322C2D302E3031202D31332E32342C2D30
                2E31202D31392E38362C302E303420302E38342C322E343620312E39342C352E
                313520342E34392C362E323920342E37382C312E37312031302E30352C302E36
                362031342E35372C2D312E3338202D302E35322C322E3538202D312E30332C35
                2E3136202D312E35322C372E3735202D362E35342C312E3538202D31342E3031
                2C322E31202D32302E30322C2D312E3437202D342E31352C2D322E3338202D35
                2E38332C2D372E3336202D352E38382C2D31312E393120302E30372C2D312E36
                3420302E32312C2D332E323820302E34322C2D342E393120312E31362C2D342E
                343820332E33382C2D392E3120372E36362C2D31312E3334207A220A20202020
                2020202020202069643D227376675F3136220A2020202020202020202020696E
                6B73636170653A636F6E6E6563746F722D6375727661747572653D2230220A20
                202020202020202020207374796C653D2266696C6C3A2366666666666622202F
                3E0A20202020202020203C706174680A2020202020202020202020643D226D20
                3533392E30332C39322E3338206320312E31392C2D322E383620332E332C2D36
                2E363420372E30322C2D362E303120332E30392C302E3120342E31372C332E35
                20342E37312C362E3031202D332E39312C302E3134202D372E38322C302E3135
                202D31312E37332C30207A220A202020202020202020202069643D227376675F
                3139220A2020202020202020202020696E6B73636170653A636F6E6E6563746F
                722D6375727661747572653D2230220A20202020202020202020207374796C65
                3D2266696C6C3A2366663030303422202F3E0A2020202020203C2F673E0A2020
                202020203C706174680A202020202020202020643D226D202D32312E39353039
                39392C3833342E3037303732206320322E37352C302E303520352E352C302E30
                3420382E32342C302E3232202D302E32362C312E3735202D302E35332C332E35
                31202D302E37392C352E3236202D302E30362C302E35202D302E322C312E3531
                202D302E32372C322E303120312E36362C302E303920332E33342C302E313320
                352E303130303030312C302E3138202D302E333930303030312C322E3135202D
                302E383030303030312C342E3331202D312E323030303030312C362E3436202D
                312E36392C2D302E3033202D332E33392C2D302E3037202D352E30362C2D302E
                3131202D302E34352C322E3033202D302E38372C342E3036202D312E32322C36
                2E3131202D302E30342C302E3236202D302E31332C302E3738202D302E31372C
                312E3033202D302E36372C332E3835202D322E34342C31302E3539202D312E37
                332C31312E353920312E33322C312E333620342E34332C302E323320362E3636
                2C302E3238206C202D312E38312C352E36382063202D332E32322C312E333620
                2D362E39352C312E3334202D31302E32392C302E3439202D322E32312C2D302E
                34202D332E39372C2D322E3536202D332E332C2D342E383420312E37342C2D31
                312E343920332E39342C2D32322E393120352E39332C2D33342E3336206C2030
                2C3020302C3020302C3020302C30207A220A20202020202020202069643D2273
                76675F3130220A202020202020202020696E6B73636170653A636F6E6E656374
                6F722D6375727661747572653D2230220A2020202020202020207374796C653D
                2266696C6C3A2366666666666622202F3E0A2020202020203C706174680A2020
                20202020202020643D226D2032312E3633393030312C3834312E383330373220
                6320322E38352C2D302E333420352E37342C2D302E313820382E36312C2D302E
                3137202D302E31382C312E3034202D302E35352C332E3133202D302E37342C34
                2E313720322E36372C2D322E393420362E31392C2D342E372031302E31372C2D
                352E3031202D312E312C322E3835202D322E31362C352E3731202D332E30362C
                382E3633202D332E34392C302E31202D372C312E3338202D382E36352C342E36
                38202D302E33342C312E3636202D302E36312C332E3333202D302E38372C352E
                3031202D302E32392C312E3936202D302E36322C332E3933202D302E39332C35
                2E3839202D302E32372C312E3637202D302E35332C332E3334202D302E38322C
                35202D302E32322C312E32202D302E34392C322E3339202D302E38322C332E35
                36202D322E38382C302E3239202D352E37372C302E3138202D382E36352C302E
                313920322E312C2D31302E363120332E35312C2D32312E333720352E37362C2D
                33312E3935206C20302C3020302C3020302C3020302C30207A220A2020202020
                2020202069643D227376675F3137220A202020202020202020696E6B73636170
                653A636F6E6E6563746F722D6375727661747572653D2230220A202020202020
                2020207374796C653D2266696C6C3A2366666666666622202F3E0A202020203C
                2F673E0A20203C2F673E0A3C2F7376673E0A}
              Proportional = True
              Transparent = True
            end
          end
        end
        object paFreshBookAddress: TPanel
          AlignWithMargins = True
          Left = 0
          Top = 235
          Width = 661
          Height = 168
          Margins.Left = 0
          Margins.Top = 15
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 1
          object paFreshBookAddressLine2: TPanel
            AlignWithMargins = True
            Left = 10
            Top = 50
            Width = 641
            Height = 30
            Margins.Left = 10
            Margins.Top = 10
            Margins.Right = 10
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 0
            object edFreshBookAddress: TEdit
              AlignWithMargins = True
              Left = 0
              Top = 0
              Width = 378
              Height = 30
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 25
              Margins.Bottom = 0
              Align = alLeft
              TabOrder = 0
            end
          end
          object paFreshBookAddressLine1: TPanel
            AlignWithMargins = True
            Left = 10
            Top = 0
            Width = 641
            Height = 40
            Margins.Left = 10
            Margins.Top = 0
            Margins.Right = 10
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 1
            object imFreshBookAddressTitle: TWSVGImage
              AlignWithMargins = True
              Left = 0
              Top = 0
              Width = 40
              Height = 40
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C212D2D2042792053616D2048657262657274
                2028407368657262292C20666F722065766572796F6E652E204D6F7265204020
                687474703A2F2F676F6F2E676C2F37414A7A624C202D2D3E0A3C212D2D20546F
                646F3A2061646420656173696E67202D2D3E0A3C7376672077696474683D2235
                3722206865696768743D223537222076696577426F783D223020302035372035
                372220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F32303030
                2F73766722207374726F6B653D2223393939223E0A202020203C672066696C6C
                3D226E6F6E65222066696C6C2D72756C653D226576656E6F6464223E0A202020
                20202020203C67207472616E73666F726D3D227472616E736C61746528312031
                2922207374726F6B652D77696474683D2232223E0A2020202020202020202020
                203C636972636C652063783D2235222063793D2235302220723D2235223E0A20
                2020202020202020202020202020203C616E696D617465206174747269627574
                654E616D653D226379220A202020202020202020202020202020202020202020
                626567696E3D22307322206475723D22322E3273220A20202020202020202020
                202020202020202020202076616C7565733D2235303B353B35303B3530220A20
                202020202020202020202020202020202020202063616C634D6F64653D226C69
                6E656172220A2020202020202020202020202020202020202020207265706561
                74436F756E743D22696E646566696E69746522202F3E0A202020202020202020
                202020202020203C616E696D617465206174747269627574654E616D653D2263
                78220A202020202020202020202020202020202020202020626567696E3D2230
                7322206475723D22322E3273220A202020202020202020202020202020202020
                20202076616C7565733D22353B32373B34393B35220A20202020202020202020
                202020202020202020202063616C634D6F64653D226C696E656172220A202020
                202020202020202020202020202020202020726570656174436F756E743D2269
                6E646566696E69746522202F3E0A2020202020202020202020203C2F63697263
                6C653E0A2020202020202020202020203C636972636C652063783D2232372220
                63793D22352220723D2235223E0A202020202020202020202020202020203C61
                6E696D617465206174747269627574654E616D653D226379220A202020202020
                202020202020202020202020202020626567696E3D22307322206475723D2232
                2E3273220A20202020202020202020202020202020202020202066726F6D3D22
                352220746F3D2235220A20202020202020202020202020202020202020202076
                616C7565733D22353B35303B35303B35220A2020202020202020202020202020
                2020202020202063616C634D6F64653D226C696E656172220A20202020202020
                2020202020202020202020202020726570656174436F756E743D22696E646566
                696E69746522202F3E0A202020202020202020202020202020203C616E696D61
                7465206174747269627574654E616D653D226378220A20202020202020202020
                2020202020202020202020626567696E3D22307322206475723D22322E327322
                0A20202020202020202020202020202020202020202066726F6D3D2232372220
                746F3D223237220A20202020202020202020202020202020202020202076616C
                7565733D2232373B34393B353B3237220A202020202020202020202020202020
                20202020202063616C634D6F64653D226C696E656172220A2020202020202020
                20202020202020202020202020726570656174436F756E743D22696E64656669
                6E69746522202F3E0A2020202020202020202020203C2F636972636C653E0A20
                20202020202020202020203C636972636C652063783D223439222063793D2235
                302220723D2235223E0A202020202020202020202020202020203C616E696D61
                7465206174747269627574654E616D653D226379220A20202020202020202020
                2020202020202020202020626567696E3D22307322206475723D22322E327322
                0A20202020202020202020202020202020202020202076616C7565733D223530
                3B35303B353B3530220A20202020202020202020202020202020202020202063
                616C634D6F64653D226C696E656172220A202020202020202020202020202020
                202020202020726570656174436F756E743D22696E646566696E69746522202F
                3E0A202020202020202020202020202020203C616E696D617465206174747269
                627574654E616D653D226378220A202020202020202020202020202020202020
                20202066726F6D3D2234392220746F3D223439220A2020202020202020202020
                20202020202020202020626567696E3D22307322206475723D22322E3273220A
                20202020202020202020202020202020202020202076616C7565733D2234393B
                353B32373B3439220A2020202020202020202020202020202020202020206361
                6C634D6F64653D226C696E656172220A20202020202020202020202020202020
                2020202020726570656174436F756E743D22696E646566696E69746522202F3E
                0A2020202020202020202020203C2F636972636C653E0A20202020202020203C
                2F673E0A202020203C2F673E0A3C2F7376673E}
              Proportional = True
              Transparent = True
              Animation.FrameCount = 45
              Animation.Position = 0
              Animation.Animate = True
            end
            object laFreshBookAddressCaption: TLabel
              AlignWithMargins = True
              Left = 45
              Top = 0
              Width = 338
              Height = 40
              Margins.Left = 5
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              AutoSize = False
              Caption = 'Add the billing address of your card below'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 12237498
              Font.Height = -13
              Font.Name = 'Verdana'
              Font.Style = []
              ParentFont = False
              Layout = tlCenter
            end
          end
          object paFreshBookAddressLine2Captions: TPanel
            AlignWithMargins = True
            Left = 10
            Top = 80
            Width = 641
            Height = 21
            Margins.Left = 10
            Margins.Top = 0
            Margins.Right = 10
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentBackground = False
            ParentFont = False
            TabOrder = 2
            object laFreshBooksStreetAddressCaption: TLabel
              AlignWithMargins = True
              Left = 0
              Top = 0
              Width = 98
              Height = 16
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Caption = 'Street address'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 4539717
              Font.Height = -13
              Font.Name = 'Verdana'
              Font.Style = []
              ParentFont = False
            end
          end
          object paFreshBookAddressLine3: TPanel
            AlignWithMargins = True
            Left = 10
            Top = 111
            Width = 641
            Height = 30
            Margins.Left = 10
            Margins.Top = 10
            Margins.Right = 10
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 3
            object edFreshBookCity: TEdit
              AlignWithMargins = True
              Left = 0
              Top = 0
              Width = 217
              Height = 30
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 25
              Margins.Bottom = 0
              Align = alLeft
              TabOrder = 0
            end
            object edFreshBooksCountry: TEdit
              AlignWithMargins = True
              Left = 242
              Top = 0
              Width = 136
              Height = 30
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 25
              Margins.Bottom = 0
              Align = alLeft
              TabOrder = 1
            end
          end
          object paFreshBookAddressLine3Captions: TPanel
            AlignWithMargins = True
            Left = 10
            Top = 141
            Width = 641
            Height = 21
            Margins.Left = 10
            Margins.Top = 0
            Margins.Right = 10
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentBackground = False
            ParentFont = False
            TabOrder = 4
            object laFreshBookCityCaption: TLabel
              AlignWithMargins = True
              Left = 0
              Top = 0
              Width = 26
              Height = 16
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Caption = 'City'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 4539717
              Font.Height = -13
              Font.Name = 'Verdana'
              Font.Style = []
              ParentFont = False
            end
            object laFreshBooksCountryCaption: TLabel
              AlignWithMargins = True
              Left = 244
              Top = 0
              Width = 52
              Height = 16
              Margins.Left = 218
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Caption = 'Country'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 4539717
              Font.Height = -13
              Font.Name = 'Verdana'
              Font.Style = []
              ParentFont = False
            end
          end
        end
        object paFreshBooksPaymentOptions: TPanel
          AlignWithMargins = True
          Left = 0
          Top = 418
          Width = 661
          Height = 87
          Margins.Left = 0
          Margins.Top = 15
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 2
          object paFreshBooksPaymentOptionsRight: TPanel
            AlignWithMargins = True
            Left = 219
            Top = 50
            Width = 189
            Height = 37
            Margins.Left = 10
            Margins.Top = 10
            Margins.Right = 10
            Margins.Bottom = 0
            Align = alLeft
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 0
            object ckFreshBooksPaymentOptionsOther: TCheckBox
              Left = 0
              Top = 17
              Width = 189
              Height = 17
              Align = alTop
              Caption = 'Other'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 4539717
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
            end
            object ckFreshBooksPaymentOptionsGift: TCheckBox
              Left = 0
              Top = 0
              Width = 189
              Height = 17
              Align = alTop
              Caption = 'Gift'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 4539717
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
            end
          end
          object paFreshBooksPaymentOptionsHeader: TPanel
            AlignWithMargins = True
            Left = 10
            Top = 0
            Width = 641
            Height = 40
            Margins.Left = 10
            Margins.Top = 0
            Margins.Right = 10
            Margins.Bottom = 0
            Align = alTop
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 1
            object imFreshBooksPaymentOptionsHeader: TWSVGImage
              AlignWithMargins = True
              Left = 0
              Top = 0
              Width = 40
              Height = 40
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C212D2D2042792053616D2048657262657274
                2028407368657262292C20666F722065766572796F6E652E204D6F7265204020
                687474703A2F2F676F6F2E676C2F37414A7A624C202D2D3E0A3C212D2D20546F
                646F3A2061646420656173696E67202D2D3E0A3C7376672077696474683D2235
                3722206865696768743D223537222076696577426F783D223020302035372035
                372220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F32303030
                2F73766722207374726F6B653D2223393939223E0A202020203C672066696C6C
                3D226E6F6E65222066696C6C2D72756C653D226576656E6F6464223E0A202020
                20202020203C67207472616E73666F726D3D227472616E736C61746528312031
                2922207374726F6B652D77696474683D2232223E0A2020202020202020202020
                203C636972636C652063783D2235222063793D2235302220723D2235223E0A20
                2020202020202020202020202020203C616E696D617465206174747269627574
                654E616D653D226379220A202020202020202020202020202020202020202020
                626567696E3D22307322206475723D22322E3273220A20202020202020202020
                202020202020202020202076616C7565733D2235303B353B35303B3530220A20
                202020202020202020202020202020202020202063616C634D6F64653D226C69
                6E656172220A2020202020202020202020202020202020202020207265706561
                74436F756E743D22696E646566696E69746522202F3E0A202020202020202020
                202020202020203C616E696D617465206174747269627574654E616D653D2263
                78220A202020202020202020202020202020202020202020626567696E3D2230
                7322206475723D22322E3273220A202020202020202020202020202020202020
                20202076616C7565733D22353B32373B34393B35220A20202020202020202020
                202020202020202020202063616C634D6F64653D226C696E656172220A202020
                202020202020202020202020202020202020726570656174436F756E743D2269
                6E646566696E69746522202F3E0A2020202020202020202020203C2F63697263
                6C653E0A2020202020202020202020203C636972636C652063783D2232372220
                63793D22352220723D2235223E0A202020202020202020202020202020203C61
                6E696D617465206174747269627574654E616D653D226379220A202020202020
                202020202020202020202020202020626567696E3D22307322206475723D2232
                2E3273220A20202020202020202020202020202020202020202066726F6D3D22
                352220746F3D2235220A20202020202020202020202020202020202020202076
                616C7565733D22353B35303B35303B35220A2020202020202020202020202020
                2020202020202063616C634D6F64653D226C696E656172220A20202020202020
                2020202020202020202020202020726570656174436F756E743D22696E646566
                696E69746522202F3E0A202020202020202020202020202020203C616E696D61
                7465206174747269627574654E616D653D226378220A20202020202020202020
                2020202020202020202020626567696E3D22307322206475723D22322E327322
                0A20202020202020202020202020202020202020202066726F6D3D2232372220
                746F3D223237220A20202020202020202020202020202020202020202076616C
                7565733D2232373B34393B353B3237220A202020202020202020202020202020
                20202020202063616C634D6F64653D226C696E656172220A2020202020202020
                20202020202020202020202020726570656174436F756E743D22696E64656669
                6E69746522202F3E0A2020202020202020202020203C2F636972636C653E0A20
                20202020202020202020203C636972636C652063783D223439222063793D2235
                302220723D2235223E0A202020202020202020202020202020203C616E696D61
                7465206174747269627574654E616D653D226379220A20202020202020202020
                2020202020202020202020626567696E3D22307322206475723D22322E327322
                0A20202020202020202020202020202020202020202076616C7565733D223530
                3B35303B353B3530220A20202020202020202020202020202020202020202063
                616C634D6F64653D226C696E656172220A202020202020202020202020202020
                202020202020726570656174436F756E743D22696E646566696E69746522202F
                3E0A202020202020202020202020202020203C616E696D617465206174747269
                627574654E616D653D226378220A202020202020202020202020202020202020
                20202066726F6D3D2234392220746F3D223439220A2020202020202020202020
                20202020202020202020626567696E3D22307322206475723D22322E3273220A
                20202020202020202020202020202020202020202076616C7565733D2234393B
                353B32373B3439220A2020202020202020202020202020202020202020206361
                6C634D6F64653D226C696E656172220A20202020202020202020202020202020
                2020202020726570656174436F756E743D22696E646566696E69746522202F3E
                0A2020202020202020202020203C2F636972636C653E0A20202020202020203C
                2F673E0A202020203C2F673E0A3C2F7376673E}
              Proportional = True
              Transparent = True
              Animation.FrameCount = 45
              Animation.Position = 0
              Animation.Animate = True
            end
            object laFreshBooksPaymentOptionsHeader: TLabel
              AlignWithMargins = True
              Left = 45
              Top = 0
              Width = 338
              Height = 40
              Margins.Left = 5
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alLeft
              AutoSize = False
              Caption = 'Please select the payment options below'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 12237498
              Font.Height = -13
              Font.Name = 'Verdana'
              Font.Style = []
              ParentFont = False
              Layout = tlCenter
            end
          end
          object paFreshBooksPaymentOptionsLeft: TPanel
            AlignWithMargins = True
            Left = 10
            Top = 50
            Width = 189
            Height = 37
            Margins.Left = 10
            Margins.Top = 10
            Margins.Right = 10
            Margins.Bottom = 0
            Align = alLeft
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 2
            object ckFreshBooksPaymentOptionsForAFriend: TCheckBox
              Left = 0
              Top = 17
              Width = 189
              Height = 17
              Align = alTop
              Caption = 'Buy for a friend'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 4539717
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
            end
            object ckFreshBooksPaymentOptionsForMe: TCheckBox
              Left = 0
              Top = 0
              Width = 189
              Height = 17
              Align = alTop
              Caption = 'Buy for me'
              Checked = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 4539717
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              State = cbChecked
              TabOrder = 1
            end
          end
        end
        object paFreshBooksPayNow: TPanel
          AlignWithMargins = True
          Left = 0
          Top = 515
          Width = 661
          Height = 90
          Margins.Left = 0
          Margins.Top = 10
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 3
          object paFreshBooksPayNowLine1: TPanel
            AlignWithMargins = True
            Left = 0
            Top = 10
            Width = 388
            Height = 80
            Margins.Left = 0
            Margins.Top = 10
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alLeft
            BevelOuter = bvNone
            Color = clWhite
            ParentBackground = False
            TabOrder = 0
            object imFreshBooksPayNow: TWSVGImageButton
              AlignWithMargins = True
              Left = 0
              Top = 0
              Width = 388
              Height = 40
              Cursor = crHandPoint
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alTop
              Center = True
              Picture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                223F3E0A3C212D2D2043726561746564207769746820496E6B73636170652028
                687474703A2F2F7777772E696E6B73636170652E6F72672F29202D2D3E0A0A3C
                7376670A202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F7267
                2F64632F656C656D656E74732F312E312F220A202020786D6C6E733A63633D22
                687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A
                202020786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F
                313939392F30322F32322D7264662D73796E7461782D6E7323220A202020786D
                6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F323030302F
                737667220A202020786D6C6E733D22687474703A2F2F7777772E77332E6F7267
                2F323030302F737667220A202020786D6C6E733A736F6469706F64693D226874
                74703A2F2F736F6469706F64692E736F75726365666F7267652E6E65742F4454
                442F736F6469706F64692D302E647464220A202020786D6C6E733A696E6B7363
                6170653D22687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D
                657370616365732F696E6B7363617065220A20202077696474683D2233303022
                0A2020206865696768743D223430220A20202076696577426F783D2230203020
                3330302E30303030312033392E393939393939220A20202069643D2273766735
                323734220A20202076657273696F6E3D22312E31220A202020696E6B73636170
                653A76657273696F6E3D22302E393120723133373235220A202020736F646970
                6F64693A646F636E616D653D226672657368626F6F6B735F7061796E6F775F64
                656661756C742E737667223E0A20203C646566730A202020202069643D226465
                66733532373622202F3E0A20203C736F6469706F64693A6E616D656476696577
                0A202020202069643D2262617365220A202020202070616765636F6C6F723D22
                23666666666666220A2020202020626F72646572636F6C6F723D222336363636
                3636220A2020202020626F726465726F7061636974793D22312E30220A202020
                2020696E6B73636170653A706167656F7061636974793D22302E30220A202020
                2020696E6B73636170653A70616765736861646F773D2232220A202020202069
                6E6B73636170653A7A6F6F6D3D22332E393539373938220A2020202020696E6B
                73636170653A63783D223130362E3839393831220A2020202020696E6B736361
                70653A63793D2232362E373030333332220A2020202020696E6B73636170653A
                646F63756D656E742D756E6974733D227078220A2020202020696E6B73636170
                653A63757272656E742D6C617965723D226C6179657231220A20202020207368
                6F77677269643D2266616C7365220A2020202020756E6974733D227078220A20
                20202020696E6B73636170653A73686F7770616765736861646F773D2266616C
                7365220A2020202020696E6B73636170653A77696E646F772D77696474683D22
                31393230220A2020202020696E6B73636170653A77696E646F772D6865696768
                743D2231303238220A2020202020696E6B73636170653A77696E646F772D783D
                222D38220A2020202020696E6B73636170653A77696E646F772D793D222D3822
                0A2020202020696E6B73636170653A77696E646F772D6D6178696D697A65643D
                223122202F3E0A20203C6D657461646174610A202020202069643D226D657461
                6461746135323739223E0A202020203C7264663A5244463E0A2020202020203C
                63633A576F726B0A2020202020202020207264663A61626F75743D22223E0A20
                202020202020203C64633A666F726D61743E696D6167652F7376672B786D6C3C
                2F64633A666F726D61743E0A20202020202020203C64633A747970650A202020
                20202020202020207264663A7265736F757263653D22687474703A2F2F707572
                6C2E6F72672F64632F64636D69747970652F5374696C6C496D61676522202F3E
                0A20202020202020203C64633A7469746C65202F3E0A2020202020203C2F6363
                3A576F726B3E0A202020203C2F7264663A5244463E0A20203C2F6D6574616461
                74613E0A20203C670A2020202020696E6B73636170653A6C6162656C3D224C61
                7965722031220A2020202020696E6B73636170653A67726F75706D6F64653D22
                6C61796572220A202020202069643D226C6179657231220A2020202020747261
                6E73666F726D3D227472616E736C61746528302C2D313031322E333632322922
                3E0A202020203C726563740A202020202020207374796C653D226F7061636974
                793A313B66696C6C3A233833633730303B66696C6C2D6F7061636974793A302E
                38303738343331343B66696C6C2D72756C653A6E6F6E7A65726F3B7374726F6B
                653A233738626330303B7374726F6B652D77696474683A302E39383539373431
                393B7374726F6B652D6C696E656361703A726F756E643B7374726F6B652D6D69
                7465726C696D69743A343B7374726F6B652D6461736861727261793A6E6F6E65
                3B7374726F6B652D646173686F66667365743A303B7374726F6B652D6F706163
                6974793A31220A2020202020202069643D227265637435323832220A20202020
                20202077696474683D223239392E3031343034220A2020202020202068656967
                68743D2233392E303134303237220A20202020202020783D22302E3439323938
                3731220A20202020202020793D22313031322E38353532220A20202020202020
                72783D22342E39353336363533220A2020202020202072793D22352E30373330
                383222202F3E0A202020203C746578740A20202020202020786D6C3A73706163
                653D227072657365727665220A20202020202020666F6E742D73697A653D2232
                35220A20202020202020783D2239352E323735383739220A2020202020202079
                3D22313033382E39333537220A2020202020202069643D227465787435323834
                220A20202020202020736F6469706F64693A6C696E6573706163696E673D2231
                323525220A202020202020207374796C653D22666F6E742D73697A653A323570
                783B6C696E652D6865696768743A313235253B666F6E742D66616D696C793A56
                657264616E613B66696C6C3A23666666666666223E506179204E6F773C2F7465
                78743E0A20203C2F673E0A3C2F7376673E0A}
              Proportional = True
              Stretch = True
              Transparent = True
              OnClick = imFreshBooksPayNowClick
              Animation.FrameCount = 0
              Animation.Position = 0
              HoveredAnimation.FrameCount = 0
              HoveredAnimation.Position = 0
              HoveredAnimation.Animate = True
              HoveredPicture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                223F3E0A3C212D2D2043726561746564207769746820496E6B73636170652028
                687474703A2F2F7777772E696E6B73636170652E6F72672F29202D2D3E0A0A3C
                7376670A202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F7267
                2F64632F656C656D656E74732F312E312F220A202020786D6C6E733A63633D22
                687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A
                202020786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F
                313939392F30322F32322D7264662D73796E7461782D6E7323220A202020786D
                6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F323030302F
                737667220A202020786D6C6E733D22687474703A2F2F7777772E77332E6F7267
                2F323030302F737667220A202020786D6C6E733A736F6469706F64693D226874
                74703A2F2F736F6469706F64692E736F75726365666F7267652E6E65742F4454
                442F736F6469706F64692D302E647464220A202020786D6C6E733A696E6B7363
                6170653D22687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D
                657370616365732F696E6B7363617065220A20202077696474683D2233303022
                0A2020206865696768743D223430220A20202076696577426F783D2230203020
                3330302E30303030312033392E393939393939220A20202069643D2273766735
                323734220A20202076657273696F6E3D22312E31220A202020696E6B73636170
                653A76657273696F6E3D22302E393120723133373235220A202020736F646970
                6F64693A646F636E616D653D226672657368626F6F6B735F7061796E6F775F68
                6F7665722E737667223E0A20203C646566730A202020202069643D2264656673
                3532373622202F3E0A20203C736F6469706F64693A6E616D6564766965770A20
                2020202069643D2262617365220A202020202070616765636F6C6F723D222366
                6666666666220A2020202020626F72646572636F6C6F723D2223363636363636
                220A2020202020626F726465726F7061636974793D22312E30220A2020202020
                696E6B73636170653A706167656F7061636974793D22302E30220A2020202020
                696E6B73636170653A70616765736861646F773D2232220A2020202020696E6B
                73636170653A7A6F6F6D3D22332E393539373938220A2020202020696E6B7363
                6170653A63783D223130362E3839393831220A2020202020696E6B7363617065
                3A63793D2232362E373030333332220A2020202020696E6B73636170653A646F
                63756D656E742D756E6974733D227078220A2020202020696E6B73636170653A
                63757272656E742D6C617965723D226C6179657231220A202020202073686F77
                677269643D2266616C7365220A2020202020756E6974733D227078220A202020
                2020696E6B73636170653A73686F7770616765736861646F773D2266616C7365
                220A2020202020696E6B73636170653A77696E646F772D77696474683D223139
                3230220A2020202020696E6B73636170653A77696E646F772D6865696768743D
                2231303238220A2020202020696E6B73636170653A77696E646F772D783D222D
                38220A2020202020696E6B73636170653A77696E646F772D793D222D38220A20
                20202020696E6B73636170653A77696E646F772D6D6178696D697A65643D2231
                22202F3E0A20203C6D657461646174610A202020202069643D226D6574616461
                746135323739223E0A202020203C7264663A5244463E0A2020202020203C6363
                3A576F726B0A2020202020202020207264663A61626F75743D22223E0A202020
                20202020203C64633A666F726D61743E696D6167652F7376672B786D6C3C2F64
                633A666F726D61743E0A20202020202020203C64633A747970650A2020202020
                2020202020207264663A7265736F757263653D22687474703A2F2F7075726C2E
                6F72672F64632F64636D69747970652F5374696C6C496D61676522202F3E0A20
                202020202020203C64633A7469746C653E3C2F64633A7469746C653E0A202020
                2020203C2F63633A576F726B3E0A202020203C2F7264663A5244463E0A20203C
                2F6D657461646174613E0A20203C670A2020202020696E6B73636170653A6C61
                62656C3D224C617965722031220A2020202020696E6B73636170653A67726F75
                706D6F64653D226C61796572220A202020202069643D226C6179657231220A20
                202020207472616E73666F726D3D227472616E736C61746528302C2D31303132
                2E3336323229223E0A202020203C726563740A202020202020207374796C653D
                226F7061636974793A313B66696C6C3A233161623830613B66696C6C2D6F7061
                636974793A302E38303738343331353B66696C6C2D72756C653A6E6F6E7A6572
                6F3B7374726F6B653A233135623230353B7374726F6B652D77696474683A302E
                39383539373431393B7374726F6B652D6C696E656361703A726F756E643B7374
                726F6B652D6D697465726C696D69743A343B7374726F6B652D64617368617272
                61793A6E6F6E653B7374726F6B652D646173686F66667365743A303B7374726F
                6B652D6F7061636974793A31220A2020202020202069643D2272656374353238
                32220A2020202020202077696474683D223239392E3031343034220A20202020
                2020206865696768743D2233392E303134303237220A20202020202020783D22
                302E34393239383731220A20202020202020793D22313031322E38353532220A
                2020202020202072783D22342E39353336363533220A2020202020202072793D
                22352E30373330383222202F3E0A202020203C746578740A2020202020202078
                6D6C3A73706163653D227072657365727665220A20202020202020666F6E742D
                73697A653D223235220A20202020202020783D2239352E323735383739220A20
                202020202020793D22313033382E39333537220A2020202020202069643D2274
                65787435323834220A20202020202020736F6469706F64693A6C696E65737061
                63696E673D2231323525220A202020202020207374796C653D22666F6E742D73
                697A653A323570783B6C696E652D6865696768743A313235253B666F6E742D66
                616D696C793A56657264616E613B66696C6C3A23666666666666223E50617920
                4E6F773C2F746578743E0A20203C2F673E0A3C2F7376673E0A}
              ClickedAnimation.FrameCount = 0
              ClickedAnimation.Position = 0
              ClickedAnimation.Animate = True
              ClickedPicture.Data = {
                0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
                20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
                223F3E0A3C212D2D2043726561746564207769746820496E6B73636170652028
                687474703A2F2F7777772E696E6B73636170652E6F72672F29202D2D3E0A0A3C
                7376670A202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F7267
                2F64632F656C656D656E74732F312E312F220A202020786D6C6E733A63633D22
                687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A
                202020786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F
                313939392F30322F32322D7264662D73796E7461782D6E7323220A202020786D
                6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F323030302F
                737667220A202020786D6C6E733D22687474703A2F2F7777772E77332E6F7267
                2F323030302F737667220A202020786D6C6E733A736F6469706F64693D226874
                74703A2F2F736F6469706F64692E736F75726365666F7267652E6E65742F4454
                442F736F6469706F64692D302E647464220A202020786D6C6E733A696E6B7363
                6170653D22687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D
                657370616365732F696E6B7363617065220A20202077696474683D2233303022
                0A2020206865696768743D223430220A20202076696577426F783D2230203020
                3330302E30303030312033392E393939393939220A20202069643D2273766735
                323734220A20202076657273696F6E3D22312E31220A202020696E6B73636170
                653A76657273696F6E3D22302E393120723133373235220A202020736F646970
                6F64693A646F636E616D653D226672657368626F6F6B735F7061796E6F775F63
                6C69636B65642E737667223E0A20203C646566730A202020202069643D226465
                66733532373622202F3E0A20203C736F6469706F64693A6E616D656476696577
                0A202020202069643D2262617365220A202020202070616765636F6C6F723D22
                23666666666666220A2020202020626F72646572636F6C6F723D222336363636
                3636220A2020202020626F726465726F7061636974793D22312E30220A202020
                2020696E6B73636170653A706167656F7061636974793D22302E30220A202020
                2020696E6B73636170653A70616765736861646F773D2232220A202020202069
                6E6B73636170653A7A6F6F6D3D22332E393539373938220A2020202020696E6B
                73636170653A63783D223130362E3839393831220A2020202020696E6B736361
                70653A63793D2232362E373030333332220A2020202020696E6B73636170653A
                646F63756D656E742D756E6974733D227078220A2020202020696E6B73636170
                653A63757272656E742D6C617965723D226C6179657231220A20202020207368
                6F77677269643D2266616C7365220A2020202020756E6974733D227078220A20
                20202020696E6B73636170653A73686F7770616765736861646F773D2266616C
                7365220A2020202020696E6B73636170653A77696E646F772D77696474683D22
                31393230220A2020202020696E6B73636170653A77696E646F772D6865696768
                743D2231303238220A2020202020696E6B73636170653A77696E646F772D783D
                222D38220A2020202020696E6B73636170653A77696E646F772D793D222D3822
                0A2020202020696E6B73636170653A77696E646F772D6D6178696D697A65643D
                223122202F3E0A20203C6D657461646174610A202020202069643D226D657461
                6461746135323739223E0A202020203C7264663A5244463E0A2020202020203C
                63633A576F726B0A2020202020202020207264663A61626F75743D22223E0A20
                202020202020203C64633A666F726D61743E696D6167652F7376672B786D6C3C
                2F64633A666F726D61743E0A20202020202020203C64633A747970650A202020
                20202020202020207264663A7265736F757263653D22687474703A2F2F707572
                6C2E6F72672F64632F64636D69747970652F5374696C6C496D61676522202F3E
                0A20202020202020203C64633A7469746C653E3C2F64633A7469746C653E0A20
                20202020203C2F63633A576F726B3E0A202020203C2F7264663A5244463E0A20
                203C2F6D657461646174613E0A20203C670A2020202020696E6B73636170653A
                6C6162656C3D224C617965722031220A2020202020696E6B73636170653A6772
                6F75706D6F64653D226C61796572220A202020202069643D226C617965723122
                0A20202020207472616E73666F726D3D227472616E736C61746528302C2D3130
                31322E3336323229223E0A202020203C726563740A202020202020207374796C
                653D226F7061636974793A313B66696C6C3A236561366430613B66696C6C2D6F
                7061636974793A302E38303738343331353B66696C6C2D72756C653A6E6F6E7A
                65726F3B7374726F6B653A236637356630353B7374726F6B652D77696474683A
                302E39383539373431393B7374726F6B652D6C696E656361703A726F756E643B
                7374726F6B652D6D697465726C696D69743A343B7374726F6B652D6461736861
                727261793A6E6F6E653B7374726F6B652D646173686F66667365743A303B7374
                726F6B652D6F7061636974793A31220A2020202020202069643D227265637435
                323832220A2020202020202077696474683D223239392E3031343034220A2020
                20202020206865696768743D2233392E303134303237220A2020202020202078
                3D22302E34393239383731220A20202020202020793D22313031322E38353532
                220A2020202020202072783D22342E39353336363533220A2020202020202072
                793D22352E30373330383222202F3E0A202020203C746578740A202020202020
                20786D6C3A73706163653D227072657365727665220A20202020202020666F6E
                742D73697A653D223235220A20202020202020783D2239352E32373538373922
                0A20202020202020793D22313033382E39333537220A2020202020202069643D
                227465787435323834220A20202020202020736F6469706F64693A6C696E6573
                706163696E673D2231323525220A202020202020207374796C653D22666F6E74
                2D73697A653A323570783B6C696E652D6865696768743A313235253B666F6E74
                2D66616D696C793A56657264616E613B66696C6C3A23666666666666223E5061
                79204E6F773C2F746578743E0A20203C2F673E0A3C2F7376673E0A}
              DisabledAnimation.FrameCount = 77
              DisabledAnimation.Position = 0
              DisabledAnimation.Animate = True
              DisabledPicture.Data = {
                0C5457535647477261706869633C212D2D2042792053616D2048657262657274
                2028407368657262292C20666F722065766572796F6E652E204D6F7265204020
                687474703A2F2F676F6F2E676C2F37414A7A624C202D2D3E0A3C737667207769
                6474683D22353822206865696768743D223538222076696577426F783D223020
                302035382035382220786D6C6E733D22687474703A2F2F7777772E77332E6F72
                672F323030302F737667223E0A202020203C672066696C6C3D226E6F6E652220
                66696C6C2D72756C653D226576656E6F6464223E0A20202020202020203C6720
                7472616E73666F726D3D227472616E736C617465283220312922207374726F6B
                653D222363636322207374726F6B652D77696474683D22312E35223E0A202020
                2020202020202020203C636972636C652063783D2234322E363031222063793D
                2231312E3436322220723D2235222066696C6C2D6F7061636974793D22312220
                66696C6C3D2223393939223E0A202020202020202020202020202020203C616E
                696D617465206174747269627574654E616D653D2266696C6C2D6F7061636974
                79220A202020202020202020202020202020202020202020626567696E3D2230
                7322206475723D22312E3373220A202020202020202020202020202020202020
                20202076616C7565733D22313B303B303B303B303B303B303B30222063616C63
                4D6F64653D226C696E656172220A202020202020202020202020202020202020
                202020726570656174436F756E743D22696E646566696E69746522202F3E0A20
                20202020202020202020203C2F636972636C653E0A2020202020202020202020
                203C636972636C652063783D2234392E303633222063793D2232372E30363322
                20723D2235222066696C6C2D6F7061636974793D2230222066696C6C3D222339
                3939223E0A202020202020202020202020202020203C616E696D617465206174
                747269627574654E616D653D2266696C6C2D6F706163697479220A2020202020
                20202020202020202020202020202020626567696E3D22307322206475723D22
                312E3373220A20202020202020202020202020202020202020202076616C7565
                733D22303B313B303B303B303B303B303B30222063616C634D6F64653D226C69
                6E656172220A2020202020202020202020202020202020202020207265706561
                74436F756E743D22696E646566696E69746522202F3E0A202020202020202020
                2020203C2F636972636C653E0A2020202020202020202020203C636972636C65
                2063783D2234322E363031222063793D2234322E3636332220723D2235222066
                696C6C2D6F7061636974793D2230222066696C6C3D2223393939223E0A202020
                202020202020202020202020203C616E696D617465206174747269627574654E
                616D653D2266696C6C2D6F706163697479220A20202020202020202020202020
                2020202020202020626567696E3D22307322206475723D22312E3373220A2020
                2020202020202020202020202020202020202076616C7565733D22303B303B31
                3B303B303B303B303B30222063616C634D6F64653D226C696E656172220A2020
                20202020202020202020202020202020202020726570656174436F756E743D22
                696E646566696E69746522202F3E0A2020202020202020202020203C2F636972
                636C653E0A2020202020202020202020203C636972636C652063783D22323722
                2063793D2234392E3132352220723D2235222066696C6C2D6F7061636974793D
                2230222066696C6C3D2223393939223E0A202020202020202020202020202020
                203C616E696D617465206174747269627574654E616D653D2266696C6C2D6F70
                6163697479220A20202020202020202020202020202020202020202062656769
                6E3D22307322206475723D22312E3373220A2020202020202020202020202020
                2020202020202076616C7565733D22303B303B303B313B303B303B303B302220
                63616C634D6F64653D226C696E656172220A2020202020202020202020202020
                20202020202020726570656174436F756E743D22696E646566696E6974652220
                2F3E0A2020202020202020202020203C2F636972636C653E0A20202020202020
                20202020203C636972636C652063783D2231312E333939222063793D2234322E
                3636332220723D2235222066696C6C2D6F7061636974793D2230222066696C6C
                3D2223393939223E0A202020202020202020202020202020203C616E696D6174
                65206174747269627574654E616D653D2266696C6C2D6F706163697479220A20
                2020202020202020202020202020202020202020626567696E3D223073222064
                75723D22312E3373220A20202020202020202020202020202020202020202076
                616C7565733D22303B303B303B303B313B303B303B30222063616C634D6F6465
                3D226C696E656172220A20202020202020202020202020202020202020202072
                6570656174436F756E743D22696E646566696E69746522202F3E0A2020202020
                202020202020203C2F636972636C653E0A2020202020202020202020203C6369
                72636C652063783D22342E393338222063793D2232372E3036332220723D2235
                222066696C6C2D6F7061636974793D2230222066696C6C3D2223393939223E0A
                202020202020202020202020202020203C616E696D6174652061747472696275
                74654E616D653D2266696C6C2D6F706163697479220A20202020202020202020
                2020202020202020202020626567696E3D22307322206475723D22312E337322
                0A20202020202020202020202020202020202020202076616C7565733D22303B
                303B303B303B303B313B303B30222063616C634D6F64653D226C696E65617222
                0A202020202020202020202020202020202020202020726570656174436F756E
                743D22696E646566696E69746522202F3E0A2020202020202020202020203C2F
                636972636C653E0A2020202020202020202020203C636972636C652063783D22
                31312E333939222063793D2231312E3436322220723D2235222066696C6C2D6F
                7061636974793D2230222066696C6C3D2223393939223E0A2020202020202020
                20202020202020203C616E696D617465206174747269627574654E616D653D22
                66696C6C2D6F706163697479220A202020202020202020202020202020202020
                202020626567696E3D22307322206475723D22312E3373220A20202020202020
                202020202020202020202020202076616C7565733D22303B303B303B303B303B
                303B313B30222063616C634D6F64653D226C696E656172220A20202020202020
                2020202020202020202020202020726570656174436F756E743D22696E646566
                696E69746522202F3E0A2020202020202020202020203C2F636972636C653E0A
                2020202020202020202020203C636972636C652063783D223237222063793D22
                352220723D2235222066696C6C2D6F7061636974793D2230222066696C6C3D22
                23393939223E0A202020202020202020202020202020203C616E696D61746520
                6174747269627574654E616D653D2266696C6C2D6F706163697479220A202020
                202020202020202020202020202020202020626567696E3D2230732220647572
                3D22312E3373220A20202020202020202020202020202020202020202076616C
                7565733D22303B303B303B303B303B303B303B31222063616C634D6F64653D22
                6C696E656172220A202020202020202020202020202020202020202020726570
                656174436F756E743D22696E646566696E69746522202F3E0A20202020202020
                20202020203C2F636972636C653E0A20202020202020203C2F673E0A20202020
                3C2F673E0A3C2F7376673E}
            end
            object paFreshBooksPayNowLine1Captions: TPanel
              AlignWithMargins = True
              Left = 10
              Top = 50
              Width = 368
              Height = 21
              Margins.Left = 10
              Margins.Top = 10
              Margins.Right = 10
              Margins.Bottom = 0
              Align = alTop
              BevelOuter = bvNone
              Color = clWhite
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentBackground = False
              ParentFont = False
              TabOrder = 0
              object laFreshBooksPayNowCaption: TLabel
                AlignWithMargins = True
                Left = 26
                Top = 0
                Width = 101
                Height = 16
                Margins.Left = 0
                Margins.Top = 0
                Margins.Right = 0
                Margins.Bottom = 0
                Align = alLeft
                Caption = 'Read about our'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 4539717
                Font.Height = -13
                Font.Name = 'Verdana'
                Font.Style = []
                ParentFont = False
                Layout = tlCenter
              end
              object imFreshBooksPayNowCaption: TImage
                AlignWithMargins = True
                Left = 0
                Top = 0
                Width = 26
                Height = 21
                Margins.Left = 0
                Margins.Top = 0
                Margins.Right = 0
                Margins.Bottom = 0
                Align = alLeft
                Center = True
                Picture.Data = {
                  0C5457535647477261706869633C7376672076657273696F6E3D22312E312220
                  69643D224C617965725F312220786D6C6E733D22687474703A2F2F7777772E77
                  332E6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474
                  703A2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D223070
                  782220793D22307078222077696474683D22353132707822206865696768743D
                  223531327078222076696577426F783D2230203020353132203531322220656E
                  61626C652D6261636B67726F756E643D226E6577203020302035313220353132
                  2220786D6C3A73706163653D227072657365727665222066696C6C3D22236363
                  63223E0D0A202020203C7061746820643D224D3338342C3234332E3556313539
                  63302D37302E3638382D35372E3331332D3132382D3132382D31323853313238
                  2C38382E3331332C3132382C3135397638342E35632D31302E37352C332E352D
                  32312E352C372E3132352D33322C31312E35763139322020633130322E343036
                  2C34322E3635362C3231372E3539342C34322E3635362C3332302C3056323535
                  433430352E352C3235302E3632352C3339342E37352C3234372C3338342C3234
                  332E357A204D3238382C343135682D36346C31312E3933382D37312E36323520
                  20433232382E3831332C3333372E352C3232342C3332382E3933382C3232342C
                  33313963302D31372E3638382C31342E3331332D33322C33322D33327333322C
                  31342E3331332C33322C333263302C392E3933382D342E3831332C31382E352D
                  31312E3933382C32342E3337354C3238382C3431357A2020204D3335322C3233
                  342E373139632D36332E3135362D31352D3132382E3831332D31352D3139322C
                  305631353963302D35322E3933382C34332E3036332D39362C39362D39367339
                  362C34332E3036332C39362C3936563233342E3731397A222F3E0D0A3C2F7376
                  673E}
                Proportional = True
                Transparent = True
              end
              object laFreshBooksPayNowLink: TLabel
                AlignWithMargins = True
                Left = 132
                Top = 0
                Width = 135
                Height = 16
                Cursor = crHandPoint
                Margins.Left = 5
                Margins.Top = 0
                Margins.Right = 0
                Margins.Bottom = 0
                Align = alLeft
                Caption = 'security safeguards.'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = 4539717
                Font.Height = -13
                Font.Name = 'Verdana'
                Font.Style = [fsUnderline]
                ParentFont = False
                Layout = tlCenter
              end
            end
          end
        end
      end
    end
  end
  object paNavigation: TPanel
    Left = 0
    Top = 0
    Width = 50
    Height = 822
    Align = alLeft
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    object ibBankingForm: TWSVGImageButton
      AlignWithMargins = True
      Left = 3
      Top = 103
      Width = 44
      Height = 44
      Hint = 
        'Banking demo, demonstrates how the SVG images may be used to imp' +
        'rove the visual of a form.'
      Align = alTop
      ParentShowHint = False
      Picture.Data = {
        0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
        20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
        223F3E0A3C212D2D2047656E657261746F723A2041646F626520496C6C757374
        7261746F722031392E302E302C20535647204578706F727420506C75672D496E
        202E205356472056657273696F6E3A20362E3030204275696C6420302920202D
        2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
        6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
        3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
        6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
        2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
        202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
        323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
        332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
        693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
        65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
        696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
        672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
        6E3D22312E31220A20202069643D224C617965725F31220A202020783D223070
        78220A202020793D22307078220A20202076696577426F783D22302030203435
        342E3934203435342E3934220A2020207374796C653D22656E61626C652D6261
        636B67726F756E643A6E657720302030203435342E3934203435342E39343B22
        0A202020786D6C3A73706163653D227072657365727665220A202020696E6B73
        636170653A76657273696F6E3D22302E393120723133373235220A202020736F
        6469706F64693A646F636E616D653D2262616E6B696E675F627574746F6E5F64
        656661756C742E737667223E3C6D657461646174610A202020202069643D226D
        657461646174613438223E3C7264663A5244463E3C63633A576F726B0A202020
        2020202020207264663A61626F75743D22223E3C64633A666F726D61743E696D
        6167652F7376672B786D6C3C2F64633A666F726D61743E3C64633A747970650A
        20202020202020202020207264663A7265736F757263653D22687474703A2F2F
        7075726C2E6F72672F64632F64636D69747970652F5374696C6C496D61676522
        202F3E3C2F63633A576F726B3E3C2F7264663A5244463E3C2F6D657461646174
        613E3C646566730A202020202069643D2264656673343622202F3E3C736F6469
        706F64693A6E616D6564766965770A202020202070616765636F6C6F723D2223
        666666666666220A2020202020626F72646572636F6C6F723D22233636363636
        36220A2020202020626F726465726F7061636974793D2231220A20202020206F
        626A656374746F6C6572616E63653D223130220A202020202067726964746F6C
        6572616E63653D223130220A20202020206775696465746F6C6572616E63653D
        223130220A2020202020696E6B73636170653A706167656F7061636974793D22
        30220A2020202020696E6B73636170653A70616765736861646F773D2232220A
        2020202020696E6B73636170653A77696E646F772D77696474683D2236393022
        0A2020202020696E6B73636170653A77696E646F772D6865696768743D223438
        30220A202020202069643D226E616D6564766965773434220A20202020207368
        6F77677269643D2266616C7365220A2020202020696E6B73636170653A7A6F6F
        6D3D22302E3531383734393732220A2020202020696E6B73636170653A63783D
        223232372E3437220A2020202020696E6B73636170653A63793D223232372E34
        37220A2020202020696E6B73636170653A77696E646F772D783D2230220A2020
        202020696E6B73636170653A77696E646F772D793D2230220A2020202020696E
        6B73636170653A77696E646F772D6D6178696D697A65643D2230220A20202020
        20696E6B73636170653A63757272656E742D6C617965723D224C617965725F31
        22202F3E3C670A202020202069643D22584D4C49445F3838345F220A20202020
        207374796C653D2266696C6C3A233462346234623B66696C6C2D6F7061636974
        793A31223E3C670A2020202020202069643D226734220A202020202020207374
        796C653D2266696C6C3A233462346234623B66696C6C2D6F7061636974793A31
        223E3C670A20202020202020202069643D226736220A20202020202020202073
        74796C653D2266696C6C3A233462346234623B66696C6C2D6F7061636974793A
        31223E3C706174680A2020202020202020202020643D224D3338322E3938372C
        3139322E3530357639352E3433386834382E3837316331322E3734382C302C32
        332E3038312D31302E3333342C32332E3038312D32332E303832563135312E33
        3339483336342E36372020202020433337352E3931322C3136312E3530332C33
        38322E3938372C3137362E3139332C3338322E3938372C3139322E3530357A22
        0A202020202020202020202069643D227061746838220A202020202020202020
        20207374796C653D2266696C6C3A233462346234623B66696C6C2D6F70616369
        74793A3122202F3E3C706174680A2020202020202020202020643D224D343331
        2E3835382C36302E393938483132352E303334632D31322E3734382C302D3233
        2E3038312C31302E3333342D32332E3038312C32332E3038327631352E303739
        483435342E3934762D31352E30382020202020433435342E39342C37312E3333
        312C3434342E3630362C36302E3939382C3433312E3835382C36302E3939387A
        220A202020202020202020202069643D22706174683130220A20202020202020
        202020207374796C653D2266696C6C3A233462346234623B66696C6C2D6F7061
        636974793A3122202F3E3C706174680A2020202020202020202020643D224D33
        32372E3437392C3136362E3939374832352E3530384331312E34322C3136362E
        3939372C302C3137382E3431382C302C3139322E353035763137352E39323963
        302C31342E3038382C31312E34322C32352E3530382C32352E3530382C32352E
        353038683330312E39373120202020206331342E3038382C302C32352E353038
        2D31312E34322C32352E3530382D32352E353038563139322E35303543333532
        2E3938372C3137382E3431382C3334312E3536372C3136362E3939372C333237
        2E3437392C3136362E3939377A204D33342E3236382C3232362E323535202020
        202063302D362E362C352E342D31322C31322D31326836332E30353363362E36
        2C302C31322C352E342C31322C31327632362E37343363302C362E362D352E34
        2C31322D31322C31324834362E323638632D362E362C302D31322D352E342D31
        322D3132563232362E3235357A204D3137362E3439332C3334352E3933312020
        20202063302C322E3238352D312E3835332C342E3133392D342E3133382C342E
        3133394833382E343036632D322E3238362C302D342E3133392D312E3835332D
        342E3133392D342E31333863302D372E3538362C302D31342E3133372C302D32
        312E373233202020202063302D322E3238362C312E3835332D342E3133382C34
        2E3133392D342E313338683133332E39343863322E3238352C302C342E313339
        2C312E3835332C342E3133392C342E313338433137362E3439332C3333312E37
        39332C3137362E3439332C3333382E3334352C3137362E3439332C3334352E39
        33317A2020202020204D3233362E34372C3237322E333536632D342E3739342C
        322E3234342D31302E3134312C332E3530352D31352E3738352C332E35303563
        2D32302E3539372C302D33372E3239352D31362E3639382D33372E3239352D33
        372E3239357331362E3639372D33372E3239352C33372E3239352D33372E3239
        35202020202063362E3039322C302C31312E3833382C312E3437312C31362E39
        31372C342E303631632D372E3137372C392E35372D31312E3433342C32312E34
        352D31312E3433342C33342E333035433232362E3136382C3235312E3739352C
        3232392E3938362C3236332E3037322C3233362E34372C3237322E3335367A20
        20202020204D3238332E3436332C3237362E393331632D32302E3539372C302D
        33372E3239352D31362E3639372D33372E3239352D33372E32393463302D3230
        2E3539382C31362E3639372D33372E3239352C33372E3239352D33372E323935
        20202020206332302E3539372C302C33372E3239352C31362E3639372C33372E
        3239352C33372E323935433332302E3735382C3236302E3233342C3330342E30
        36312C3237362E3933312C3238332E3436332C3237362E3933317A220A202020
        202020202020202069643D22706174683132220A202020202020202020202073
        74796C653D2266696C6C3A233462346234623B66696C6C2D6F7061636974793A
        3122202F3E3C2F673E3C2F673E3C2F673E3C670A202020202069643D22673134
        22202F3E3C670A202020202069643D2267313622202F3E3C670A202020202069
        643D2267313822202F3E3C670A202020202069643D2267323022202F3E3C670A
        202020202069643D2267323222202F3E3C670A202020202069643D2267323422
        202F3E3C670A202020202069643D2267323622202F3E3C670A20202020206964
        3D2267323822202F3E3C670A202020202069643D2267333022202F3E3C670A20
        2020202069643D2267333222202F3E3C670A202020202069643D226733342220
        2F3E3C670A202020202069643D2267333622202F3E3C670A202020202069643D
        2267333822202F3E3C670A202020202069643D2267343022202F3E3C670A2020
        20202069643D2267343222202F3E3C2F7376673E}
      Proportional = True
      ShowHint = True
      Stretch = True
      Transparent = True
      OnClick = ibBankingFormClick
      Animation.FrameCount = 0
      Animation.Position = 0
      HoveredAnimation.FrameCount = 0
      HoveredAnimation.Position = 0
      HoveredPicture.Data = {
        0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
        20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
        223F3E0A3C212D2D2047656E657261746F723A2041646F626520496C6C757374
        7261746F722031392E302E302C20535647204578706F727420506C75672D496E
        202E205356472056657273696F6E3A20362E3030204275696C6420302920202D
        2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
        6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
        3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
        6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
        2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
        202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
        323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
        332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
        693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
        65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
        696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
        672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
        6E3D22312E31220A20202069643D224C617965725F31220A202020783D223070
        78220A202020793D22307078220A20202076696577426F783D22302030203435
        342E3934203435342E3934220A2020207374796C653D22656E61626C652D6261
        636B67726F756E643A6E657720302030203435342E3934203435342E39343B22
        0A202020786D6C3A73706163653D227072657365727665220A202020696E6B73
        636170653A76657273696F6E3D22302E393120723133373235220A202020736F
        6469706F64693A646F636E616D653D2262616E6B696E675F627574746F6E5F68
        6F76657265642E737667223E3C6D657461646174610A202020202069643D226D
        657461646174613438223E3C7264663A5244463E3C63633A576F726B0A202020
        2020202020207264663A61626F75743D22223E3C64633A666F726D61743E696D
        6167652F7376672B786D6C3C2F64633A666F726D61743E3C64633A747970650A
        20202020202020202020207264663A7265736F757263653D22687474703A2F2F
        7075726C2E6F72672F64632F64636D69747970652F5374696C6C496D61676522
        202F3E3C64633A7469746C653E3C2F64633A7469746C653E3C2F63633A576F72
        6B3E3C2F7264663A5244463E3C2F6D657461646174613E3C646566730A202020
        202069643D2264656673343622202F3E3C736F6469706F64693A6E616D656476
        6965770A202020202070616765636F6C6F723D2223666666666666220A202020
        2020626F72646572636F6C6F723D2223363636363636220A2020202020626F72
        6465726F7061636974793D2231220A20202020206F626A656374746F6C657261
        6E63653D223130220A202020202067726964746F6C6572616E63653D22313022
        0A20202020206775696465746F6C6572616E63653D223130220A202020202069
        6E6B73636170653A706167656F7061636974793D2230220A2020202020696E6B
        73636170653A70616765736861646F773D2232220A2020202020696E6B736361
        70653A77696E646F772D77696474683D22363930220A2020202020696E6B7363
        6170653A77696E646F772D6865696768743D22343830220A202020202069643D
        226E616D6564766965773434220A202020202073686F77677269643D2266616C
        7365220A2020202020696E6B73636170653A7A6F6F6D3D22302E353138373439
        3732220A2020202020696E6B73636170653A63783D223232372E3437220A2020
        202020696E6B73636170653A63793D223232372E3437220A2020202020696E6B
        73636170653A77696E646F772D783D2230220A2020202020696E6B7363617065
        3A77696E646F772D793D2230220A2020202020696E6B73636170653A77696E64
        6F772D6D6178696D697A65643D2230220A2020202020696E6B73636170653A63
        757272656E742D6C617965723D224C617965725F3122202F3E3C670A20202020
        2069643D22584D4C49445F3838345F220A20202020207374796C653D2266696C
        6C3A233063356530613B66696C6C2D6F7061636974793A31223E3C670A202020
        2020202069643D226734220A202020202020207374796C653D2266696C6C3A23
        3063356530613B66696C6C2D6F7061636974793A31223E3C670A202020202020
        20202069643D226736220A2020202020202020207374796C653D2266696C6C3A
        233063356530613B66696C6C2D6F7061636974793A31223E3C706174680A2020
        202020202020202020643D224D3338322E3938372C3139322E3530357639352E
        3433386834382E3837316331322E3734382C302C32332E3038312D31302E3333
        342C32332E3038312D32332E303832563135312E333339483336342E36372020
        202020433337352E3931322C3136312E3530332C3338322E3938372C3137362E
        3139332C3338322E3938372C3139322E3530357A220A20202020202020202020
        2069643D227061746838220A20202020202020202020207374796C653D226669
        6C6C3A233063356530613B66696C6C2D6F7061636974793A3122202F3E3C7061
        74680A2020202020202020202020643D224D3433312E3835382C36302E393938
        483132352E303334632D31322E3734382C302D32332E3038312C31302E333334
        2D32332E3038312C32332E3038327631352E303739483435342E3934762D3135
        2E30382020202020433435342E39342C37312E3333312C3434342E3630362C36
        302E3939382C3433312E3835382C36302E3939387A220A202020202020202020
        202069643D22706174683130220A20202020202020202020207374796C653D22
        66696C6C3A233063356530613B66696C6C2D6F7061636974793A3122202F3E3C
        706174680A2020202020202020202020643D224D3332372E3437392C3136362E
        3939374832352E3530384331312E34322C3136362E3939372C302C3137382E34
        31382C302C3139322E353035763137352E39323963302C31342E3038382C3131
        2E34322C32352E3530382C32352E3530382C32352E353038683330312E393731
        20202020206331342E3038382C302C32352E3530382D31312E34322C32352E35
        30382D32352E353038563139322E353035433335322E3938372C3137382E3431
        382C3334312E3536372C3136362E3939372C3332372E3437392C3136362E3939
        377A204D33342E3236382C3232362E323535202020202063302D362E362C352E
        342D31322C31322D31326836332E30353363362E362C302C31322C352E342C31
        322C31327632362E37343363302C362E362D352E342C31322D31322C31324834
        362E323638632D362E362C302D31322D352E342D31322D3132563232362E3235
        357A204D3137362E3439332C3334352E393331202020202063302C322E323835
        2D312E3835332C342E3133392D342E3133382C342E3133394833382E34303663
        2D322E3238362C302D342E3133392D312E3835332D342E3133392D342E313338
        63302D372E3538362C302D31342E3133372C302D32312E373233202020202063
        302D322E3238362C312E3835332D342E3133382C342E3133392D342E31333868
        3133332E39343863322E3238352C302C342E3133392C312E3835332C342E3133
        392C342E313338433137362E3439332C3333312E3739332C3137362E3439332C
        3333382E3334352C3137362E3439332C3334352E3933317A2020202020204D32
        33362E34372C3237322E333536632D342E3739342C322E3234342D31302E3134
        312C332E3530352D31352E3738352C332E353035632D32302E3539372C302D33
        372E3239352D31362E3639382D33372E3239352D33372E3239357331362E3639
        372D33372E3239352C33372E3239352D33372E323935202020202063362E3039
        322C302C31312E3833382C312E3437312C31362E3931372C342E303631632D37
        2E3137372C392E35372D31312E3433342C32312E34352D31312E3433342C3334
        2E333035433232362E3136382C3235312E3739352C3232392E3938362C323633
        2E3037322C3233362E34372C3237322E3335367A2020202020204D3238332E34
        36332C3237362E393331632D32302E3539372C302D33372E3239352D31362E36
        39372D33372E3239352D33372E32393463302D32302E3539382C31362E363937
        2D33372E3239352C33372E3239352D33372E32393520202020206332302E3539
        372C302C33372E3239352C31362E3639372C33372E3239352C33372E32393543
        3332302E3735382C3236302E3233342C3330342E3036312C3237362E3933312C
        3238332E3436332C3237362E3933317A220A202020202020202020202069643D
        22706174683132220A20202020202020202020207374796C653D2266696C6C3A
        233063356530613B66696C6C2D6F7061636974793A3122202F3E3C2F673E3C2F
        673E3C2F673E3C670A202020202069643D2267313422202F3E3C670A20202020
        2069643D2267313622202F3E3C670A202020202069643D2267313822202F3E3C
        670A202020202069643D2267323022202F3E3C670A202020202069643D226732
        3222202F3E3C670A202020202069643D2267323422202F3E3C670A2020202020
        69643D2267323622202F3E3C670A202020202069643D2267323822202F3E3C67
        0A202020202069643D2267333022202F3E3C670A202020202069643D22673332
        22202F3E3C670A202020202069643D2267333422202F3E3C670A202020202069
        643D2267333622202F3E3C670A202020202069643D2267333822202F3E3C670A
        202020202069643D2267343022202F3E3C670A202020202069643D2267343222
        202F3E3C2F7376673E}
      ClickedAnimation.FrameCount = 0
      ClickedAnimation.Position = 0
      ClickedPicture.Data = {
        0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
        20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
        223F3E0A3C212D2D2047656E657261746F723A2041646F626520496C6C757374
        7261746F722031392E302E302C20535647204578706F727420506C75672D496E
        202E205356472056657273696F6E3A20362E3030204275696C6420302920202D
        2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
        6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
        3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
        6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
        2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
        202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
        323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
        332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
        693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
        65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
        696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
        672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
        6E3D22312E31220A20202069643D224C617965725F31220A202020783D223070
        78220A202020793D22307078220A20202076696577426F783D22302030203435
        342E3934203435342E3934220A2020207374796C653D22656E61626C652D6261
        636B67726F756E643A6E657720302030203435342E3934203435342E39343B22
        0A202020786D6C3A73706163653D227072657365727665220A202020696E6B73
        636170653A76657273696F6E3D22302E393120723133373235220A202020736F
        6469706F64693A646F636E616D653D2262616E6B696E675F627574746F6E5F63
        6C69636B65642E737667223E3C6D657461646174610A202020202069643D226D
        657461646174613438223E3C7264663A5244463E3C63633A576F726B0A202020
        2020202020207264663A61626F75743D22223E3C64633A666F726D61743E696D
        6167652F7376672B786D6C3C2F64633A666F726D61743E3C64633A747970650A
        20202020202020202020207264663A7265736F757263653D22687474703A2F2F
        7075726C2E6F72672F64632F64636D69747970652F5374696C6C496D61676522
        202F3E3C64633A7469746C653E3C2F64633A7469746C653E3C2F63633A576F72
        6B3E3C2F7264663A5244463E3C2F6D657461646174613E3C646566730A202020
        202069643D2264656673343622202F3E3C736F6469706F64693A6E616D656476
        6965770A202020202070616765636F6C6F723D2223666666666666220A202020
        2020626F72646572636F6C6F723D2223363636363636220A2020202020626F72
        6465726F7061636974793D2231220A20202020206F626A656374746F6C657261
        6E63653D223130220A202020202067726964746F6C6572616E63653D22313022
        0A20202020206775696465746F6C6572616E63653D223130220A202020202069
        6E6B73636170653A706167656F7061636974793D2230220A2020202020696E6B
        73636170653A70616765736861646F773D2232220A2020202020696E6B736361
        70653A77696E646F772D77696474683D22363930220A2020202020696E6B7363
        6170653A77696E646F772D6865696768743D22343830220A202020202069643D
        226E616D6564766965773434220A202020202073686F77677269643D2266616C
        7365220A2020202020696E6B73636170653A7A6F6F6D3D22302E353138373439
        3732220A2020202020696E6B73636170653A63783D223232372E3437220A2020
        202020696E6B73636170653A63793D223232372E3437220A2020202020696E6B
        73636170653A77696E646F772D783D2230220A2020202020696E6B7363617065
        3A77696E646F772D793D2230220A2020202020696E6B73636170653A77696E64
        6F772D6D6178696D697A65643D2230220A2020202020696E6B73636170653A63
        757272656E742D6C617965723D224C617965725F3122202F3E3C670A20202020
        2069643D22584D4C49445F3838345F220A20202020207374796C653D2266696C
        6C3A233063623030613B66696C6C2D6F7061636974793A31223E3C670A202020
        2020202069643D226734220A202020202020207374796C653D2266696C6C3A23
        3063623030613B66696C6C2D6F7061636974793A31223E3C670A202020202020
        20202069643D226736220A2020202020202020207374796C653D2266696C6C3A
        233063623030613B66696C6C2D6F7061636974793A31223E3C706174680A2020
        202020202020202020643D224D3338322E3938372C3139322E3530357639352E
        3433386834382E3837316331322E3734382C302C32332E3038312D31302E3333
        342C32332E3038312D32332E303832563135312E333339483336342E36372020
        202020433337352E3931322C3136312E3530332C3338322E3938372C3137362E
        3139332C3338322E3938372C3139322E3530357A220A20202020202020202020
        2069643D227061746838220A20202020202020202020207374796C653D226669
        6C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C7061
        74680A2020202020202020202020643D224D3433312E3835382C36302E393938
        483132352E303334632D31322E3734382C302D32332E3038312C31302E333334
        2D32332E3038312C32332E3038327631352E303739483435342E3934762D3135
        2E30382020202020433435342E39342C37312E3333312C3434342E3630362C36
        302E3939382C3433312E3835382C36302E3939387A220A202020202020202020
        202069643D22706174683130220A20202020202020202020207374796C653D22
        66696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C
        706174680A2020202020202020202020643D224D3332372E3437392C3136362E
        3939374832352E3530384331312E34322C3136362E3939372C302C3137382E34
        31382C302C3139322E353035763137352E39323963302C31342E3038382C3131
        2E34322C32352E3530382C32352E3530382C32352E353038683330312E393731
        20202020206331342E3038382C302C32352E3530382D31312E34322C32352E35
        30382D32352E353038563139322E353035433335322E3938372C3137382E3431
        382C3334312E3536372C3136362E3939372C3332372E3437392C3136362E3939
        377A204D33342E3236382C3232362E323535202020202063302D362E362C352E
        342D31322C31322D31326836332E30353363362E362C302C31322C352E342C31
        322C31327632362E37343363302C362E362D352E342C31322D31322C31324834
        362E323638632D362E362C302D31322D352E342D31322D3132563232362E3235
        357A204D3137362E3439332C3334352E393331202020202063302C322E323835
        2D312E3835332C342E3133392D342E3133382C342E3133394833382E34303663
        2D322E3238362C302D342E3133392D312E3835332D342E3133392D342E313338
        63302D372E3538362C302D31342E3133372C302D32312E373233202020202063
        302D322E3238362C312E3835332D342E3133382C342E3133392D342E31333868
        3133332E39343863322E3238352C302C342E3133392C312E3835332C342E3133
        392C342E313338433137362E3439332C3333312E3739332C3137362E3439332C
        3333382E3334352C3137362E3439332C3334352E3933317A2020202020204D32
        33362E34372C3237322E333536632D342E3739342C322E3234342D31302E3134
        312C332E3530352D31352E3738352C332E353035632D32302E3539372C302D33
        372E3239352D31362E3639382D33372E3239352D33372E3239357331362E3639
        372D33372E3239352C33372E3239352D33372E323935202020202063362E3039
        322C302C31312E3833382C312E3437312C31362E3931372C342E303631632D37
        2E3137372C392E35372D31312E3433342C32312E34352D31312E3433342C3334
        2E333035433232362E3136382C3235312E3739352C3232392E3938362C323633
        2E3037322C3233362E34372C3237322E3335367A2020202020204D3238332E34
        36332C3237362E393331632D32302E3539372C302D33372E3239352D31362E36
        39372D33372E3239352D33372E32393463302D32302E3539382C31362E363937
        2D33372E3239352C33372E3239352D33372E32393520202020206332302E3539
        372C302C33372E3239352C31362E3639372C33372E3239352C33372E32393543
        3332302E3735382C3236302E3233342C3330342E3036312C3237362E3933312C
        3238332E3436332C3237362E3933317A220A202020202020202020202069643D
        22706174683132220A20202020202020202020207374796C653D2266696C6C3A
        233063623030613B66696C6C2D6F7061636974793A3122202F3E3C2F673E3C2F
        673E3C2F673E3C670A202020202069643D2267313422202F3E3C670A20202020
        2069643D2267313622202F3E3C670A202020202069643D2267313822202F3E3C
        670A202020202069643D2267323022202F3E3C670A202020202069643D226732
        3222202F3E3C670A202020202069643D2267323422202F3E3C670A2020202020
        69643D2267323622202F3E3C670A202020202069643D2267323822202F3E3C67
        0A202020202069643D2267333022202F3E3C670A202020202069643D22673332
        22202F3E3C670A202020202069643D2267333422202F3E3C670A202020202069
        643D2267333622202F3E3C670A202020202069643D2267333822202F3E3C670A
        202020202069643D2267343022202F3E3C670A202020202069643D2267343222
        202F3E3C2F7376673E}
      DisabledAnimation.FrameCount = 0
      DisabledAnimation.Position = 0
    end
    object ibBrowserForm: TWSVGImageButton
      AlignWithMargins = True
      Left = 3
      Top = 53
      Width = 44
      Height = 44
      Hint = 'SVG image browser. You can use it to test your own images.'
      Align = alTop
      ParentShowHint = False
      Picture.Data = {
        0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
        20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
        223F3E0A3C212D2D2047656E657261746F723A2041646F626520496C6C757374
        7261746F722031392E302E302C20535647204578706F727420506C75672D496E
        202E205356472056657273696F6E3A20362E3030204275696C6420302920202D
        2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
        6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
        3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
        6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
        2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
        202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
        323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
        332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
        693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
        65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
        696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
        672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
        6E3D22312E31220A20202069643D224C617965725F31220A202020783D223070
        78220A202020793D22307078220A20202076696577426F783D22302030203531
        3220353132220A2020207374796C653D22656E61626C652D6261636B67726F75
        6E643A6E65772030203020353132203531323B220A202020786D6C3A73706163
        653D227072657365727665220A202020696E6B73636170653A76657273696F6E
        3D22302E393120723133373235220A202020736F6469706F64693A646F636E61
        6D653D2262726F777365725F627574746F6E5F64656661756C742E737667223E
        3C6D657461646174610A202020202069643D226D657461646174613439223E3C
        7264663A5244463E3C63633A576F726B0A2020202020202020207264663A6162
        6F75743D22223E3C64633A666F726D61743E696D6167652F7376672B786D6C3C
        2F64633A666F726D61743E3C64633A747970650A202020202020202020202072
        64663A7265736F757263653D22687474703A2F2F7075726C2E6F72672F64632F
        64636D69747970652F5374696C6C496D61676522202F3E3C2F63633A576F726B
        3E3C2F7264663A5244463E3C2F6D657461646174613E3C646566730A20202020
        2069643D2264656673343722202F3E3C736F6469706F64693A6E616D65647669
        65770A202020202070616765636F6C6F723D2223666666666666220A20202020
        20626F72646572636F6C6F723D2223363636363636220A2020202020626F7264
        65726F7061636974793D2231220A20202020206F626A656374746F6C6572616E
        63653D223130220A202020202067726964746F6C6572616E63653D223130220A
        20202020206775696465746F6C6572616E63653D223130220A2020202020696E
        6B73636170653A706167656F7061636974793D2230220A2020202020696E6B73
        636170653A70616765736861646F773D2232220A2020202020696E6B73636170
        653A77696E646F772D77696474683D22363930220A2020202020696E6B736361
        70653A77696E646F772D6865696768743D22343830220A202020202069643D22
        6E616D6564766965773435220A202020202073686F77677269643D2266616C73
        65220A2020202020696E6B73636170653A7A6F6F6D3D22302E34363039333735
        220A2020202020696E6B73636170653A63783D22323536220A2020202020696E
        6B73636170653A63793D22323536220A2020202020696E6B73636170653A7769
        6E646F772D783D2230220A2020202020696E6B73636170653A77696E646F772D
        793D2230220A2020202020696E6B73636170653A77696E646F772D6D6178696D
        697A65643D2230220A2020202020696E6B73636170653A63757272656E742D6C
        617965723D224C617965725F3122202F3E3C670A202020202069643D22673322
        0A20202020207374796C653D2266696C6C3A233462346234623B66696C6C2D6F
        7061636974793A31223E3C670A2020202020202069643D226735220A20202020
        2020207374796C653D2266696C6C3A233462346234623B66696C6C2D6F706163
        6974793A31223E3C670A20202020202020202069643D226737220A2020202020
        202020207374796C653D2266696C6C3A233462346234623B66696C6C2D6F7061
        636974793A31223E3C706174680A2020202020202020202020643D224D343732
        2E3137382C33342E36324833392E3832324331372E3832392C33342E36322C30
        2C35322E3434392C302C37342E3434327634352E32363363302C332E3134312C
        322E3534372C352E3638392C352E3638392C352E363839683530302E36323220
        2020202063332E3134312C302C352E3638392D322E3534372C352E3638392D35
        2E3638395637342E343432433531322C35322E3434392C3439342E3137312C33
        342E36322C3437322E3137382C33342E36327A204D37312E3830352C39322E38
        34342020202020632D372E30392C302D31322E3833362D352E3734372D31322E
        3833362D31322E38333673352E3734372D31322E3833362C31322E3833362D31
        322E38333663372E3038392C302C31322E3833362C352E3734372C31322E3833
        362C31322E38333620202020204338342E3634322C38372E3039372C37382E38
        39352C39322E3834342C37312E3830352C39322E3834347A204D3131322E3935
        372C39322E383434632D372E30392C302D31322E3833362D352E3734372D3132
        2E3833362D31322E38333673352E3734372D31322E3833362C31322E3833362D
        31322E38333620202020207331322E3833362C352E3734372C31322E3833362C
        31322E383336433132352E3739342C38372E3039372C3132302E3034362C3932
        2E3834342C3131322E3935372C39322E3834347A204D3135342E3130392C3932
        2E3834342020202020632D372E3038392C302D31322E3833362D352E3734372D
        31322E3833362D31322E38333673352E3734372D31322E3833362C31322E3833
        362D31322E38333663372E30392C302C31322E3833362C352E3734372C31322E
        3833362C31322E3833362020202020433136362E3934352C38372E3039372C31
        36312E3139382C39322E3834342C3135342E3130392C39322E3834347A220A20
        2020202020202020202069643D227061746839220A2020202020202020202020
        7374796C653D2266696C6C3A233462346234623B66696C6C2D6F706163697479
        3A3122202F3E3C706174680A2020202020202020202020643D224D3530362E33
        31312C3134382E313548352E363839632D332E3134312C302D352E3638392C32
        2E3534372D352E3638392C352E363839763238332E37313963302C32312E3939
        332C31372E3832392C33392E3832322C33392E3832322C33392E383232683433
        322E33353620202020206332312E3939332C302C33392E3832322D31372E3832
        392C33392E3832322D33392E383232563135332E383339433531322C3135302E
        3639372C3530392E3435342C3134382E31352C3530362E3331312C3134382E31
        357A204D3335372E3333352C3431342E312020202020632D342E3434332C342E
        3434332D31312E3634372C342E3434332D31362E3039322C306C2D37372E3030
        352D37372E303035632D32372E3839382C32302E3134382D36372E3035332C31
        372E3539342D39322E3034322D372E3339362020202020632D32372E3737332D
        32372E3737332D32372E3737332D37322E3936352D302E3030312D3130302E37
        33386332372E3737342D32372E3737332C37322E3936352D32372E3737332C31
        30302E3733382C3068302E30303120202020206332352E3033352C32352E3033
        352C32372E3439322C36342E3231362C372E3339362C39322E3034326C37372E
        3030352C37372E303035433336312E3737382C3430322E3435312C3336312E37
        37382C3430392E3635362C3335372E3333352C3431342E317A220A2020202020
        20202020202069643D22706174683131220A2020202020202020202020737479
        6C653D2266696C6C3A233462346234623B66696C6C2D6F7061636974793A3122
        202F3E3C706174680A2020202020202020202020643D224D3138382E3238362C
        3234352E303532632D31382E3930312C31382E3930312D31382E3930312C3439
        2E3635362C302E3030312C36382E3535366331382E392C31382E3930312C3439
        2E36352C31382E3930312C36382E3535322C302E303033202020202063302E30
        30312D302E3030312C302E3030322D302E3030322C302E3030332D302E303033
        63302E3030312D302E3030312C302E3030322D302E3030322C302E3030352D30
        2E3030336331382E3839372D31382E3930312C31382E3839362D34392E363533
        2D302E3030352D36382E3535332020202020433233372E3934352C3232362E31
        35332C3230372E3138382C3232362E31352C3138382E3238362C3234352E3035
        327A220A202020202020202020202069643D22706174683133220A2020202020
        2020202020207374796C653D2266696C6C3A233462346234623B66696C6C2D6F
        7061636974793A3122202F3E3C2F673E3C2F673E3C2F673E3C670A2020202020
        69643D2267313522202F3E3C670A202020202069643D2267313722202F3E3C67
        0A202020202069643D2267313922202F3E3C670A202020202069643D22673231
        22202F3E3C670A202020202069643D2267323322202F3E3C670A202020202069
        643D2267323522202F3E3C670A202020202069643D2267323722202F3E3C670A
        202020202069643D2267323922202F3E3C670A202020202069643D2267333122
        202F3E3C670A202020202069643D2267333322202F3E3C670A20202020206964
        3D2267333522202F3E3C670A202020202069643D2267333722202F3E3C670A20
        2020202069643D2267333922202F3E3C670A202020202069643D226734312220
        2F3E3C670A202020202069643D2267343322202F3E3C2F7376673E}
      Proportional = True
      ShowHint = True
      Stretch = True
      Transparent = True
      OnClick = ibBrowserFormClick
      Animation.FrameCount = 0
      Animation.Position = 0
      HoveredAnimation.FrameCount = 0
      HoveredAnimation.Position = 0
      HoveredPicture.Data = {
        0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
        20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
        223F3E0A3C212D2D2047656E657261746F723A2041646F626520496C6C757374
        7261746F722031392E302E302C20535647204578706F727420506C75672D496E
        202E205356472056657273696F6E3A20362E3030204275696C6420302920202D
        2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
        6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
        3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
        6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
        2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
        202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
        323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
        332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
        693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
        65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
        696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
        672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
        6E3D22312E31220A20202069643D224C617965725F31220A202020783D223070
        78220A202020793D22307078220A20202076696577426F783D22302030203531
        3220353132220A2020207374796C653D22656E61626C652D6261636B67726F75
        6E643A6E65772030203020353132203531323B220A202020786D6C3A73706163
        653D227072657365727665220A202020696E6B73636170653A76657273696F6E
        3D22302E393120723133373235220A202020736F6469706F64693A646F636E61
        6D653D2262726F777365725F627574746F6E5F686F76657265642E737667223E
        3C6D657461646174610A202020202069643D226D657461646174613439223E3C
        7264663A5244463E3C63633A576F726B0A2020202020202020207264663A6162
        6F75743D22223E3C64633A666F726D61743E696D6167652F7376672B786D6C3C
        2F64633A666F726D61743E3C64633A747970650A202020202020202020202072
        64663A7265736F757263653D22687474703A2F2F7075726C2E6F72672F64632F
        64636D69747970652F5374696C6C496D61676522202F3E3C64633A7469746C65
        3E3C2F64633A7469746C653E3C2F63633A576F726B3E3C2F7264663A5244463E
        3C2F6D657461646174613E3C646566730A202020202069643D22646566733437
        22202F3E3C736F6469706F64693A6E616D6564766965770A2020202020706167
        65636F6C6F723D2223666666666666220A2020202020626F72646572636F6C6F
        723D2223363636363636220A2020202020626F726465726F7061636974793D22
        31220A20202020206F626A656374746F6C6572616E63653D223130220A202020
        202067726964746F6C6572616E63653D223130220A2020202020677569646574
        6F6C6572616E63653D223130220A2020202020696E6B73636170653A70616765
        6F7061636974793D2230220A2020202020696E6B73636170653A706167657368
        61646F773D2232220A2020202020696E6B73636170653A77696E646F772D7769
        6474683D22363930220A2020202020696E6B73636170653A77696E646F772D68
        65696768743D22343830220A202020202069643D226E616D6564766965773435
        220A202020202073686F77677269643D2266616C7365220A2020202020696E6B
        73636170653A7A6F6F6D3D22302E34363039333735220A2020202020696E6B73
        636170653A63783D22323536220A2020202020696E6B73636170653A63793D22
        323536220A2020202020696E6B73636170653A77696E646F772D783D2230220A
        2020202020696E6B73636170653A77696E646F772D793D2230220A2020202020
        696E6B73636170653A77696E646F772D6D6178696D697A65643D2230220A2020
        202020696E6B73636170653A63757272656E742D6C617965723D224C61796572
        5F3122202F3E3C670A202020202069643D226733220A20202020207374796C65
        3D2266696C6C3A233063356530613B66696C6C2D6F7061636974793A31223E3C
        670A2020202020202069643D226735220A202020202020207374796C653D2266
        696C6C3A233063356530613B66696C6C2D6F7061636974793A31223E3C670A20
        202020202020202069643D226737220A2020202020202020207374796C653D22
        66696C6C3A233063356530613B66696C6C2D6F7061636974793A31223E3C7061
        74680A2020202020202020202020643D224D3437322E3137382C33342E363248
        33392E3832324331372E3832392C33342E36322C302C35322E3434392C302C37
        342E3434327634352E32363363302C332E3134312C322E3534372C352E363839
        2C352E3638392C352E363839683530302E363232202020202063332E3134312C
        302C352E3638392D322E3534372C352E3638392D352E3638395637342E343432
        433531322C35322E3434392C3439342E3137312C33342E36322C3437322E3137
        382C33342E36327A204D37312E3830352C39322E3834342020202020632D372E
        30392C302D31322E3833362D352E3734372D31322E3833362D31322E38333673
        352E3734372D31322E3833362C31322E3833362D31322E38333663372E303839
        2C302C31322E3833362C352E3734372C31322E3833362C31322E383336202020
        20204338342E3634322C38372E3039372C37382E3839352C39322E3834342C37
        312E3830352C39322E3834347A204D3131322E3935372C39322E383434632D37
        2E30392C302D31322E3833362D352E3734372D31322E3833362D31322E383336
        73352E3734372D31322E3833362C31322E3833362D31322E3833362020202020
        7331322E3833362C352E3734372C31322E3833362C31322E383336433132352E
        3739342C38372E3039372C3132302E3034362C39322E3834342C3131322E3935
        372C39322E3834347A204D3135342E3130392C39322E3834342020202020632D
        372E3038392C302D31322E3833362D352E3734372D31322E3833362D31322E38
        333673352E3734372D31322E3833362C31322E3833362D31322E38333663372E
        30392C302C31322E3833362C352E3734372C31322E3833362C31322E38333620
        20202020433136362E3934352C38372E3039372C3136312E3139382C39322E38
        34342C3135342E3130392C39322E3834347A220A202020202020202020202069
        643D227061746839220A20202020202020202020207374796C653D2266696C6C
        3A233063356530613B66696C6C2D6F7061636974793A3122202F3E3C70617468
        0A2020202020202020202020643D224D3530362E3331312C3134382E31354835
        2E363839632D332E3134312C302D352E3638392C322E3534372D352E3638392C
        352E363839763238332E37313963302C32312E3939332C31372E3832392C3339
        2E3832322C33392E3832322C33392E383232683433322E333536202020202063
        32312E3939332C302C33392E3832322D31372E3832392C33392E3832322D3339
        2E383232563135332E383339433531322C3135302E3639372C3530392E343534
        2C3134382E31352C3530362E3331312C3134382E31357A204D3335372E333335
        2C3431342E312020202020632D342E3434332C342E3434332D31312E3634372C
        342E3434332D31362E3039322C306C2D37372E3030352D37372E303035632D32
        372E3839382C32302E3134382D36372E3035332C31372E3539342D39322E3034
        322D372E3339362020202020632D32372E3737332D32372E3737332D32372E37
        37332D37322E3936352D302E3030312D3130302E3733386332372E3737342D32
        372E3737332C37322E3936352D32372E3737332C3130302E3733382C3068302E
        30303120202020206332352E3033352C32352E3033352C32372E3439322C3634
        2E3231362C372E3339362C39322E3034326C37372E3030352C37372E30303543
        3336312E3737382C3430322E3435312C3336312E3737382C3430392E3635362C
        3335372E3333352C3431342E317A220A202020202020202020202069643D2270
        6174683131220A20202020202020202020207374796C653D2266696C6C3A2330
        63356530613B66696C6C2D6F7061636974793A3122202F3E3C706174680A2020
        202020202020202020643D224D3138382E3238362C3234352E303532632D3138
        2E3930312C31382E3930312D31382E3930312C34392E3635362C302E3030312C
        36382E3535366331382E392C31382E3930312C34392E36352C31382E3930312C
        36382E3535322C302E303033202020202063302E3030312D302E3030312C302E
        3030322D302E3030322C302E3030332D302E30303363302E3030312D302E3030
        312C302E3030322D302E3030322C302E3030352D302E3030336331382E383937
        2D31382E3930312C31382E3839362D34392E3635332D302E3030352D36382E35
        35332020202020433233372E3934352C3232362E3135332C3230372E3138382C
        3232362E31352C3138382E3238362C3234352E3035327A220A20202020202020
        2020202069643D22706174683133220A20202020202020202020207374796C65
        3D2266696C6C3A233063356530613B66696C6C2D6F7061636974793A3122202F
        3E3C2F673E3C2F673E3C2F673E3C670A202020202069643D2267313522202F3E
        3C670A202020202069643D2267313722202F3E3C670A202020202069643D2267
        313922202F3E3C670A202020202069643D2267323122202F3E3C670A20202020
        2069643D2267323322202F3E3C670A202020202069643D2267323522202F3E3C
        670A202020202069643D2267323722202F3E3C670A202020202069643D226732
        3922202F3E3C670A202020202069643D2267333122202F3E3C670A2020202020
        69643D2267333322202F3E3C670A202020202069643D2267333522202F3E3C67
        0A202020202069643D2267333722202F3E3C670A202020202069643D22673339
        22202F3E3C670A202020202069643D2267343122202F3E3C670A202020202069
        643D2267343322202F3E3C2F7376673E}
      ClickedAnimation.FrameCount = 0
      ClickedAnimation.Position = 0
      ClickedPicture.Data = {
        0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
        20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
        223F3E0A3C212D2D2047656E657261746F723A2041646F626520496C6C757374
        7261746F722031392E302E302C20535647204578706F727420506C75672D496E
        202E205356472056657273696F6E3A20362E3030204275696C6420302920202D
        2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
        6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
        3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
        6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
        2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
        202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
        323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
        332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
        693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
        65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
        696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
        672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
        6E3D22312E31220A20202069643D224C617965725F31220A202020783D223070
        78220A202020793D22307078220A20202076696577426F783D22302030203531
        3220353132220A2020207374796C653D22656E61626C652D6261636B67726F75
        6E643A6E65772030203020353132203531323B220A202020786D6C3A73706163
        653D227072657365727665220A202020696E6B73636170653A76657273696F6E
        3D22302E393120723133373235220A202020736F6469706F64693A646F636E61
        6D653D2262726F777365725F627574746F6E5F636C69636B65642E737667223E
        3C6D657461646174610A202020202069643D226D657461646174613439223E3C
        7264663A5244463E3C63633A576F726B0A2020202020202020207264663A6162
        6F75743D22223E3C64633A666F726D61743E696D6167652F7376672B786D6C3C
        2F64633A666F726D61743E3C64633A747970650A202020202020202020202072
        64663A7265736F757263653D22687474703A2F2F7075726C2E6F72672F64632F
        64636D69747970652F5374696C6C496D61676522202F3E3C64633A7469746C65
        3E3C2F64633A7469746C653E3C2F63633A576F726B3E3C2F7264663A5244463E
        3C2F6D657461646174613E3C646566730A202020202069643D22646566733437
        22202F3E3C736F6469706F64693A6E616D6564766965770A2020202020706167
        65636F6C6F723D2223666666666666220A2020202020626F72646572636F6C6F
        723D2223363636363636220A2020202020626F726465726F7061636974793D22
        31220A20202020206F626A656374746F6C6572616E63653D223130220A202020
        202067726964746F6C6572616E63653D223130220A2020202020677569646574
        6F6C6572616E63653D223130220A2020202020696E6B73636170653A70616765
        6F7061636974793D2230220A2020202020696E6B73636170653A706167657368
        61646F773D2232220A2020202020696E6B73636170653A77696E646F772D7769
        6474683D22363930220A2020202020696E6B73636170653A77696E646F772D68
        65696768743D22343830220A202020202069643D226E616D6564766965773435
        220A202020202073686F77677269643D2266616C7365220A2020202020696E6B
        73636170653A7A6F6F6D3D22302E34363039333735220A2020202020696E6B73
        636170653A63783D22323536220A2020202020696E6B73636170653A63793D22
        323536220A2020202020696E6B73636170653A77696E646F772D783D2230220A
        2020202020696E6B73636170653A77696E646F772D793D2230220A2020202020
        696E6B73636170653A77696E646F772D6D6178696D697A65643D2230220A2020
        202020696E6B73636170653A63757272656E742D6C617965723D224C61796572
        5F3122202F3E3C670A202020202069643D226733220A20202020207374796C65
        3D2266696C6C3A233063623030613B66696C6C2D6F7061636974793A31223E3C
        670A2020202020202069643D226735220A202020202020207374796C653D2266
        696C6C3A233063623030613B66696C6C2D6F7061636974793A31223E3C670A20
        202020202020202069643D226737220A2020202020202020207374796C653D22
        66696C6C3A233063623030613B66696C6C2D6F7061636974793A31223E3C7061
        74680A2020202020202020202020643D224D3437322E3137382C33342E363248
        33392E3832324331372E3832392C33342E36322C302C35322E3434392C302C37
        342E3434327634352E32363363302C332E3134312C322E3534372C352E363839
        2C352E3638392C352E363839683530302E363232202020202063332E3134312C
        302C352E3638392D322E3534372C352E3638392D352E3638395637342E343432
        433531322C35322E3434392C3439342E3137312C33342E36322C3437322E3137
        382C33342E36327A204D37312E3830352C39322E3834342020202020632D372E
        30392C302D31322E3833362D352E3734372D31322E3833362D31322E38333673
        352E3734372D31322E3833362C31322E3833362D31322E38333663372E303839
        2C302C31322E3833362C352E3734372C31322E3833362C31322E383336202020
        20204338342E3634322C38372E3039372C37382E3839352C39322E3834342C37
        312E3830352C39322E3834347A204D3131322E3935372C39322E383434632D37
        2E30392C302D31322E3833362D352E3734372D31322E3833362D31322E383336
        73352E3734372D31322E3833362C31322E3833362D31322E3833362020202020
        7331322E3833362C352E3734372C31322E3833362C31322E383336433132352E
        3739342C38372E3039372C3132302E3034362C39322E3834342C3131322E3935
        372C39322E3834347A204D3135342E3130392C39322E3834342020202020632D
        372E3038392C302D31322E3833362D352E3734372D31322E3833362D31322E38
        333673352E3734372D31322E3833362C31322E3833362D31322E38333663372E
        30392C302C31322E3833362C352E3734372C31322E3833362C31322E38333620
        20202020433136362E3934352C38372E3039372C3136312E3139382C39322E38
        34342C3135342E3130392C39322E3834347A220A202020202020202020202069
        643D227061746839220A20202020202020202020207374796C653D2266696C6C
        3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C70617468
        0A2020202020202020202020643D224D3530362E3331312C3134382E31354835
        2E363839632D332E3134312C302D352E3638392C322E3534372D352E3638392C
        352E363839763238332E37313963302C32312E3939332C31372E3832392C3339
        2E3832322C33392E3832322C33392E383232683433322E333536202020202063
        32312E3939332C302C33392E3832322D31372E3832392C33392E3832322D3339
        2E383232563135332E383339433531322C3135302E3639372C3530392E343534
        2C3134382E31352C3530362E3331312C3134382E31357A204D3335372E333335
        2C3431342E312020202020632D342E3434332C342E3434332D31312E3634372C
        342E3434332D31362E3039322C306C2D37372E3030352D37372E303035632D32
        372E3839382C32302E3134382D36372E3035332C31372E3539342D39322E3034
        322D372E3339362020202020632D32372E3737332D32372E3737332D32372E37
        37332D37322E3936352D302E3030312D3130302E3733386332372E3737342D32
        372E3737332C37322E3936352D32372E3737332C3130302E3733382C3068302E
        30303120202020206332352E3033352C32352E3033352C32372E3439322C3634
        2E3231362C372E3339362C39322E3034326C37372E3030352C37372E30303543
        3336312E3737382C3430322E3435312C3336312E3737382C3430392E3635362C
        3335372E3333352C3431342E317A220A202020202020202020202069643D2270
        6174683131220A20202020202020202020207374796C653D2266696C6C3A2330
        63623030613B66696C6C2D6F7061636974793A3122202F3E3C706174680A2020
        202020202020202020643D224D3138382E3238362C3234352E303532632D3138
        2E3930312C31382E3930312D31382E3930312C34392E3635362C302E3030312C
        36382E3535366331382E392C31382E3930312C34392E36352C31382E3930312C
        36382E3535322C302E303033202020202063302E3030312D302E3030312C302E
        3030322D302E3030322C302E3030332D302E30303363302E3030312D302E3030
        312C302E3030322D302E3030322C302E3030352D302E3030336331382E383937
        2D31382E3930312C31382E3839362D34392E3635332D302E3030352D36382E35
        35332020202020433233372E3934352C3232362E3135332C3230372E3138382C
        3232362E31352C3138382E3238362C3234352E3035327A220A20202020202020
        2020202069643D22706174683133220A20202020202020202020207374796C65
        3D2266696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F
        3E3C2F673E3C2F673E3C2F673E3C670A202020202069643D2267313522202F3E
        3C670A202020202069643D2267313722202F3E3C670A202020202069643D2267
        313922202F3E3C670A202020202069643D2267323122202F3E3C670A20202020
        2069643D2267323322202F3E3C670A202020202069643D2267323522202F3E3C
        670A202020202069643D2267323722202F3E3C670A202020202069643D226732
        3922202F3E3C670A202020202069643D2267333122202F3E3C670A2020202020
        69643D2267333322202F3E3C670A202020202069643D2267333522202F3E3C67
        0A202020202069643D2267333722202F3E3C670A202020202069643D22673339
        22202F3E3C670A202020202069643D2267343122202F3E3C670A202020202069
        643D2267343322202F3E3C2F7376673E}
      DisabledAnimation.FrameCount = 0
      DisabledAnimation.Position = 0
    end
    object ibFreshBooksForm: TWSVGImageButton
      AlignWithMargins = True
      Left = 3
      Top = 153
      Width = 44
      Height = 44
      Hint = 
        'FreshBooks like page demo, demonstrates how the SVG images may b' +
        'e used to improve the visual of a form.'
      Align = alTop
      ParentShowHint = False
      Picture.Data = {
        0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
        20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
        223F3E0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
        6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
        3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
        6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
        2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
        202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
        323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
        332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
        693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
        65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
        696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
        672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
        6E3D22312E31220A20202069643D224C617965725F31220A202020783D223070
        78220A202020793D22307078220A20202076696577426F783D22302030203530
        203530220A2020207374796C653D22656E61626C652D6261636B67726F756E64
        3A6E6577203020302035302035303B220A202020786D6C3A73706163653D2270
        72657365727665220A202020696E6B73636170653A76657273696F6E3D22302E
        393120723133373235220A202020736F6469706F64693A646F636E616D653D22
        626C75655F6C6561665F627574746F6E5F64656661756C742E737667223E3C6D
        657461646174610A202020202069643D226D657461646174613133223E3C7264
        663A5244463E3C63633A576F726B0A2020202020202020207264663A61626F75
        743D22223E3C64633A666F726D61743E696D6167652F7376672B786D6C3C2F64
        633A666F726D61743E3C64633A747970650A2020202020202020202020726466
        3A7265736F757263653D22687474703A2F2F7075726C2E6F72672F64632F6463
        6D69747970652F5374696C6C496D61676522202F3E3C64633A7469746C653E41
        7373657420313C2F64633A7469746C653E3C2F63633A576F726B3E3C2F726466
        3A5244463E3C2F6D657461646174613E3C646566730A202020202069643D2264
        656673313122202F3E3C736F6469706F64693A6E616D6564766965770A202020
        202070616765636F6C6F723D2223666666666666220A2020202020626F726465
        72636F6C6F723D2223363636363636220A2020202020626F726465726F706163
        6974793D2231220A20202020206F626A656374746F6C6572616E63653D223130
        220A202020202067726964746F6C6572616E63653D223130220A202020202067
        75696465746F6C6572616E63653D223130220A2020202020696E6B7363617065
        3A706167656F7061636974793D2230220A2020202020696E6B73636170653A70
        616765736861646F773D2232220A2020202020696E6B73636170653A77696E64
        6F772D77696474683D22363930220A2020202020696E6B73636170653A77696E
        646F772D6865696768743D22343830220A202020202069643D226E616D656476
        69657739220A202020202073686F77677269643D2266616C7365220A20202020
        20696E6B73636170653A7A6F6F6D3D22342E3732220A2020202020696E6B7363
        6170653A63783D222D362E36373337323838220A2020202020696E6B73636170
        653A63793D223235220A2020202020696E6B73636170653A77696E646F772D78
        3D2230220A2020202020696E6B73636170653A77696E646F772D793D2230220A
        2020202020696E6B73636170653A77696E646F772D6D6178696D697A65643D22
        30220A2020202020696E6B73636170653A63757272656E742D6C617965723D22
        4C617965725F3122202F3E3C7374796C650A2020202020747970653D22746578
        742F637373220A202020202069643D227374796C6533223E0A092E7374307B66
        696C6C3A233030353736373B7D0A3C2F7374796C653E3C7469746C650A202020
        202069643D227469746C6535223E417373657420313C2F7469746C653E3C7061
        74680A2020202020636C6173733D22737430220A2020202020643D224D33382E
        332C382E35632D312E362C322E362D352E332C352E362D31352E322C38632D31
        362E392C342E312D31302E362C32352D31302E362C323573302E342D392E372C
        382D31342E324333302E362C32312E342C33322E322C31392C33322E322C3139
        2020732D312E342C332E382D392E342C392E37632D322E392C322E332D352E31
        2C352E332D362E332C382E3763342E352D332E312C382E372D302E332C31352E
        362D352E324334302E392C32352E392C33382E332C382E352C33382E332C382E
        357A220A202020202069643D227061746837220A20202020207374796C653D22
        66696C6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C
        2F7376673E}
      Proportional = True
      ShowHint = True
      Stretch = True
      Transparent = True
      OnClick = ibFreshBooksFormClick
      Animation.FrameCount = 0
      Animation.Position = 0
      HoveredAnimation.FrameCount = 0
      HoveredAnimation.Position = 0
      HoveredPicture.Data = {
        0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
        20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
        223F3E0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
        6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
        3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
        6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
        2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
        202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
        323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
        332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
        693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
        65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
        696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
        672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
        6E3D22312E31220A20202069643D224C617965725F31220A202020783D223070
        78220A202020793D22307078220A20202076696577426F783D22302030203530
        203530220A2020207374796C653D22656E61626C652D6261636B67726F756E64
        3A6E6577203020302035302035303B220A202020786D6C3A73706163653D2270
        72657365727665220A202020696E6B73636170653A76657273696F6E3D22302E
        393120723133373235220A202020736F6469706F64693A646F636E616D653D22
        626C75655F6C6561665F627574746F6E5F686F76657265642E737667223E3C6D
        657461646174610A202020202069643D226D657461646174613133223E3C7264
        663A5244463E3C63633A576F726B0A2020202020202020207264663A61626F75
        743D22223E3C64633A666F726D61743E696D6167652F7376672B786D6C3C2F64
        633A666F726D61743E3C64633A747970650A2020202020202020202020726466
        3A7265736F757263653D22687474703A2F2F7075726C2E6F72672F64632F6463
        6D69747970652F5374696C6C496D61676522202F3E3C64633A7469746C653E41
        7373657420313C2F64633A7469746C653E3C2F63633A576F726B3E3C2F726466
        3A5244463E3C2F6D657461646174613E3C646566730A202020202069643D2264
        656673313122202F3E3C736F6469706F64693A6E616D6564766965770A202020
        202070616765636F6C6F723D2223666666666666220A2020202020626F726465
        72636F6C6F723D2223363636363636220A2020202020626F726465726F706163
        6974793D2231220A20202020206F626A656374746F6C6572616E63653D223130
        220A202020202067726964746F6C6572616E63653D223130220A202020202067
        75696465746F6C6572616E63653D223130220A2020202020696E6B7363617065
        3A706167656F7061636974793D2230220A2020202020696E6B73636170653A70
        616765736861646F773D2232220A2020202020696E6B73636170653A77696E64
        6F772D77696474683D22363930220A2020202020696E6B73636170653A77696E
        646F772D6865696768743D22343830220A202020202069643D226E616D656476
        69657739220A202020202073686F77677269643D2266616C7365220A20202020
        20696E6B73636170653A7A6F6F6D3D22342E3732220A2020202020696E6B7363
        6170653A63783D222D362E36373337323838220A2020202020696E6B73636170
        653A63793D223235220A2020202020696E6B73636170653A77696E646F772D78
        3D2230220A2020202020696E6B73636170653A77696E646F772D793D2230220A
        2020202020696E6B73636170653A77696E646F772D6D6178696D697A65643D22
        30220A2020202020696E6B73636170653A63757272656E742D6C617965723D22
        4C617965725F3122202F3E3C7374796C650A2020202020747970653D22746578
        742F637373220A202020202069643D227374796C6533223E0A092E7374307B66
        696C6C3A233030353736373B7D0A3C2F7374796C653E3C7469746C650A202020
        202069643D227469746C6535223E417373657420313C2F7469746C653E3C7061
        74680A2020202020636C6173733D22737430220A2020202020643D224D33382E
        332C382E35632D312E362C322E362D352E332C352E362D31352E322C38632D31
        362E392C342E312D31302E362C32352D31302E362C323573302E342D392E372C
        382D31342E324333302E362C32312E342C33322E322C31392C33322E322C3139
        2020732D312E342C332E382D392E342C392E37632D322E392C322E332D352E31
        2C352E332D362E332C382E3763342E352D332E312C382E372D302E332C31352E
        362D352E324334302E392C32352E392C33382E332C382E352C33382E332C382E
        357A220A202020202069643D227061746837220A20202020207374796C653D22
        66696C6C3A233063356530613B66696C6C2D6F7061636974793A3122202F3E3C
        2F7376673E}
      ClickedAnimation.FrameCount = 0
      ClickedAnimation.Position = 0
      ClickedPicture.Data = {
        0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
        20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
        223F3E0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
        6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
        3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
        6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
        2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
        202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
        323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
        332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
        693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
        65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
        696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
        672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
        6E3D22312E31220A20202069643D224C617965725F31220A202020783D223070
        78220A202020793D22307078220A20202076696577426F783D22302030203530
        203530220A2020207374796C653D22656E61626C652D6261636B67726F756E64
        3A6E6577203020302035302035303B220A202020786D6C3A73706163653D2270
        72657365727665220A202020696E6B73636170653A76657273696F6E3D22302E
        393120723133373235220A202020736F6469706F64693A646F636E616D653D22
        626C75655F6C6561665F627574746F6E5F636C69636B65642E737667223E3C6D
        657461646174610A202020202069643D226D657461646174613133223E3C7264
        663A5244463E3C63633A576F726B0A2020202020202020207264663A61626F75
        743D22223E3C64633A666F726D61743E696D6167652F7376672B786D6C3C2F64
        633A666F726D61743E3C64633A747970650A2020202020202020202020726466
        3A7265736F757263653D22687474703A2F2F7075726C2E6F72672F64632F6463
        6D69747970652F5374696C6C496D61676522202F3E3C64633A7469746C653E41
        7373657420313C2F64633A7469746C653E3C2F63633A576F726B3E3C2F726466
        3A5244463E3C2F6D657461646174613E3C646566730A202020202069643D2264
        656673313122202F3E3C736F6469706F64693A6E616D6564766965770A202020
        202070616765636F6C6F723D2223666666666666220A2020202020626F726465
        72636F6C6F723D2223363636363636220A2020202020626F726465726F706163
        6974793D2231220A20202020206F626A656374746F6C6572616E63653D223130
        220A202020202067726964746F6C6572616E63653D223130220A202020202067
        75696465746F6C6572616E63653D223130220A2020202020696E6B7363617065
        3A706167656F7061636974793D2230220A2020202020696E6B73636170653A70
        616765736861646F773D2232220A2020202020696E6B73636170653A77696E64
        6F772D77696474683D22363930220A2020202020696E6B73636170653A77696E
        646F772D6865696768743D22343830220A202020202069643D226E616D656476
        69657739220A202020202073686F77677269643D2266616C7365220A20202020
        20696E6B73636170653A7A6F6F6D3D22342E3732220A2020202020696E6B7363
        6170653A63783D222D362E36373337323838220A2020202020696E6B73636170
        653A63793D223235220A2020202020696E6B73636170653A77696E646F772D78
        3D2230220A2020202020696E6B73636170653A77696E646F772D793D2230220A
        2020202020696E6B73636170653A77696E646F772D6D6178696D697A65643D22
        30220A2020202020696E6B73636170653A63757272656E742D6C617965723D22
        4C617965725F3122202F3E3C7374796C650A2020202020747970653D22746578
        742F637373220A202020202069643D227374796C6533223E0A092E7374307B66
        696C6C3A233030353736373B7D0A3C2F7374796C653E3C7469746C650A202020
        202069643D227469746C6535223E417373657420313C2F7469746C653E3C7061
        74680A2020202020636C6173733D22737430220A2020202020643D224D33382E
        332C382E35632D312E362C322E362D352E332C352E362D31352E322C38632D31
        362E392C342E312D31302E362C32352D31302E362C323573302E342D392E372C
        382D31342E324333302E362C32312E342C33322E322C31392C33322E322C3139
        2020732D312E342C332E382D392E342C392E37632D322E392C322E332D352E31
        2C352E332D362E332C382E3763342E352D332E312C382E372D302E332C31352E
        362D352E324334302E392C32352E392C33382E332C382E352C33382E332C382E
        357A220A202020202069643D227061746837220A20202020207374796C653D22
        66696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C
        2F7376673E}
      DisabledAnimation.FrameCount = 0
      DisabledAnimation.Position = 0
    end
    object ibGalleryForm: TWSVGImageButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 44
      Height = 44
      Hint = 'This is the gallery. You will see here the library capabilities.'
      Align = alTop
      Center = True
      ParentShowHint = False
      Picture.Data = {
        0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
        20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
        223F3E0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
        6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
        3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
        6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
        2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
        202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
        323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
        332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
        693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
        65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
        696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
        672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
        6E3D22312E31220A20202077696474683D22353736220A202020686569676874
        3D22353132220A20202076696577426F783D223020302035373620353132220A
        20202069643D2273766732220A202020696E6B73636170653A76657273696F6E
        3D22302E393120723133373235220A202020736F6469706F64693A646F636E61
        6D653D22627574746F6E5F67616C6C6572795F64656661756C742E737667223E
        0A20203C6D657461646174610A202020202069643D226D657461646174613135
        223E0A202020203C7264663A5244463E0A2020202020203C63633A576F726B0A
        2020202020202020207264663A61626F75743D22223E0A20202020202020203C
        64633A666F726D61743E696D6167652F7376672B786D6C3C2F64633A666F726D
        61743E0A20202020202020203C64633A747970650A2020202020202020202020
        7264663A7265736F757263653D22687474703A2F2F7075726C2E6F72672F6463
        2F64636D69747970652F5374696C6C496D61676522202F3E0A2020202020203C
        2F63633A576F726B3E0A202020203C2F7264663A5244463E0A20203C2F6D6574
        61646174613E0A20203C646566730A202020202069643D226465667331332220
        2F3E0A20203C736F6469706F64693A6E616D6564766965770A20202020207061
        6765636F6C6F723D2223666666666666220A2020202020626F72646572636F6C
        6F723D2223363636363636220A2020202020626F726465726F7061636974793D
        2231220A20202020206F626A656374746F6C6572616E63653D223130220A2020
        20202067726964746F6C6572616E63653D223130220A20202020206775696465
        746F6C6572616E63653D223130220A2020202020696E6B73636170653A706167
        656F7061636974793D2230220A2020202020696E6B73636170653A7061676573
        6861646F773D2232220A2020202020696E6B73636170653A77696E646F772D77
        696474683D2231303236220A2020202020696E6B73636170653A77696E646F77
        2D6865696768743D22383431220A202020202069643D226E616D656476696577
        3131220A202020202073686F77677269643D2266616C7365220A202020202069
        6E6B73636170653A7A6F6F6D3D22302E34363039333735220A2020202020696E
        6B73636170653A63783D223332312E3930383631220A2020202020696E6B7363
        6170653A63793D22323536220A2020202020696E6B73636170653A77696E646F
        772D783D2230220A2020202020696E6B73636170653A77696E646F772D793D22
        30220A2020202020696E6B73636170653A77696E646F772D6D6178696D697A65
        643D2230220A2020202020696E6B73636170653A63757272656E742D6C617965
        723D227376673222202F3E0A20203C670A202020202069643D2269636F6D6F6F
        6E2D69676E6F726522202F3E0A20203C706174680A2020202020643D224D3534
        34203634682D3332762D333263302D31372E362D31342E342D33322D33322D33
        32682D343438632D31372E3620302D33322031342E342D333220333276333834
        63302031372E362031342E342033322033322033326833327633326330203137
        2E362031342E34203332203332203332683434386331372E3620302033322D31
        342E342033322D3332762D33383463302D31372E362D31342E342D33322D3332
        2D33327A4D363420393676333230682D33312E393433632D302E3032302D302E
        3031372D302E3034312D302E3033382D302E3035372D302E303538762D333833
        2E38383563302E3031372D302E30323020302E3033382D302E30343120302E30
        35372D302E303537683434372E38383563302E30323020302E30313720302E30
        343120302E30333820302E30353820302E3035387633312E393432682D333834
        632D31372E3620302D33322031342E342D333220333276307A4D353434203437
        392E393432632D302E30313720302E3032302D302E30333820302E3034312D30
        2E30353820302E303538682D3434372E383835632D302E3032302D302E303137
        2D302E3034312D302E3033382D302E3035372D302E303538762D3338332E3838
        3563302E3031372D302E30323020302E3033382D302E30343120302E3035372D
        302E303537683434372E38383563302E30323020302E30313720302E30343120
        302E30333820302E30353820302E303538763338332E3838347A220A20202020
        2069643D227061746835220A20202020207374796C653D2266696C6C3A233462
        346234623B66696C6C2D6F7061636974793A302E393634373035383822202F3E
        0A20203C706174680A2020202020643D224D3438302031373663302032362E35
        312D32312E34392034382D3438203438732D34382D32312E34392D34382D3438
        2032312E34392D34382034382D34382034382032312E34392034382034387A22
        0A202020202069643D227061746837220A20202020207374796C653D2266696C
        6C3A233462346234623B66696C6C2D6F7061636974793A302E39363437303538
        3822202F3E0A20203C706174680A2020202020643D224D35313220343438682D
        333834762D36346C3131322D31393220313238203136306833326C3131322D39
        367A220A202020202069643D227061746839220A20202020207374796C653D22
        66696C6C3A233462346234623B66696C6C2D6F7061636974793A302E39363437
        3035383822202F3E0A3C2F7376673E0A}
      Proportional = True
      ShowHint = True
      Stretch = True
      Transparent = True
      OnClick = ibGalleryFormClick
      Animation.FrameCount = 0
      Animation.Position = 0
      HoveredAnimation.FrameCount = 0
      HoveredAnimation.Position = 0
      HoveredPicture.Data = {
        0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
        20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
        223F3E0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
        6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
        3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
        6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
        2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
        202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
        323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
        332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
        693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
        65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
        696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
        672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
        6E3D22312E31220A20202077696474683D22353736220A202020686569676874
        3D22353132220A20202076696577426F783D223020302035373620353132220A
        20202069643D2273766732220A202020696E6B73636170653A76657273696F6E
        3D22302E393120723133373235220A202020736F6469706F64693A646F636E61
        6D653D22627574746F6E5F67616C6C6572795F686F76657265642E737667223E
        0A20203C6D657461646174610A202020202069643D226D657461646174613135
        223E0A202020203C7264663A5244463E0A2020202020203C63633A576F726B0A
        2020202020202020207264663A61626F75743D22223E0A20202020202020203C
        64633A666F726D61743E696D6167652F7376672B786D6C3C2F64633A666F726D
        61743E0A20202020202020203C64633A747970650A2020202020202020202020
        7264663A7265736F757263653D22687474703A2F2F7075726C2E6F72672F6463
        2F64636D69747970652F5374696C6C496D61676522202F3E0A20202020202020
        203C64633A7469746C653E3C2F64633A7469746C653E0A2020202020203C2F63
        633A576F726B3E0A202020203C2F7264663A5244463E0A20203C2F6D65746164
        6174613E0A20203C646566730A202020202069643D2264656673313322202F3E
        0A20203C736F6469706F64693A6E616D6564766965770A202020202070616765
        636F6C6F723D2223666666666666220A2020202020626F72646572636F6C6F72
        3D2223363636363636220A2020202020626F726465726F7061636974793D2231
        220A20202020206F626A656374746F6C6572616E63653D223130220A20202020
        2067726964746F6C6572616E63653D223130220A20202020206775696465746F
        6C6572616E63653D223130220A2020202020696E6B73636170653A706167656F
        7061636974793D2230220A2020202020696E6B73636170653A70616765736861
        646F773D2232220A2020202020696E6B73636170653A77696E646F772D776964
        74683D2231303236220A2020202020696E6B73636170653A77696E646F772D68
        65696768743D22383431220A202020202069643D226E616D6564766965773131
        220A202020202073686F77677269643D2266616C7365220A2020202020696E6B
        73636170653A7A6F6F6D3D22302E34363039333735220A2020202020696E6B73
        636170653A63783D223332312E3930383631220A2020202020696E6B73636170
        653A63793D22323536220A2020202020696E6B73636170653A77696E646F772D
        783D2230220A2020202020696E6B73636170653A77696E646F772D793D223022
        0A2020202020696E6B73636170653A77696E646F772D6D6178696D697A65643D
        2230220A2020202020696E6B73636170653A63757272656E742D6C617965723D
        227376673222202F3E0A20203C670A202020202069643D2269636F6D6F6F6E2D
        69676E6F726522202F3E0A20203C706174680A2020202020643D224D35343420
        3634682D3332762D333263302D31372E362D31342E342D33322D33322D333268
        2D343438632D31372E3620302D33322031342E342D3332203332763338346330
        2031372E362031342E3420333220333220333268333276333263302031372E36
        2031342E34203332203332203332683434386331372E3620302033322D31342E
        342033322D3332762D33383463302D31372E362D31342E342D33322D33322D33
        327A4D363420393676333230682D33312E393433632D302E3032302D302E3031
        372D302E3034312D302E3033382D302E3035372D302E303538762D3338332E38
        383563302E3031372D302E30323020302E3033382D302E30343120302E303537
        2D302E303537683434372E38383563302E30323020302E30313720302E303431
        20302E30333820302E30353820302E3035387633312E393432682D333834632D
        31372E3620302D33322031342E342D333220333276307A4D353434203437392E
        393432632D302E30313720302E3032302D302E30333820302E3034312D302E30
        353820302E303538682D3434372E383835632D302E3032302D302E3031372D30
        2E3034312D302E3033382D302E3035372D302E303538762D3338332E38383563
        302E3031372D302E30323020302E3033382D302E30343120302E3035372D302E
        303537683434372E38383563302E30323020302E30313720302E30343120302E
        30333820302E30353820302E303538763338332E3838347A220A202020202069
        643D227061746835220A20202020207374796C653D2266696C6C3A2330633565
        30613B66696C6C2D6F7061636974793A302E393634373035383822202F3E0A20
        203C706174680A2020202020643D224D3438302031373663302032362E35312D
        32312E34392034382D3438203438732D34382D32312E34392D34382D34382032
        312E34392D34382034382D34382034382032312E34392034382034387A220A20
        2020202069643D227061746837220A20202020207374796C653D2266696C6C3A
        233063356530613B66696C6C2D6F7061636974793A302E393634373035383822
        202F3E0A20203C706174680A2020202020643D224D35313220343438682D3338
        34762D36346C3131322D31393220313238203136306833326C3131322D39367A
        220A202020202069643D227061746839220A20202020207374796C653D226669
        6C6C3A233063356530613B66696C6C2D6F7061636974793A302E393634373035
        383822202F3E0A3C2F7376673E0A}
      ClickedAnimation.FrameCount = 0
      ClickedAnimation.Position = 0
      ClickedPicture.Data = {
        0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
        20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
        223F3E0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
        6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
        3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
        6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
        2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
        202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
        323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
        332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
        693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
        65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
        696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
        672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
        6E3D22312E31220A20202077696474683D22353736220A202020686569676874
        3D22353132220A20202076696577426F783D223020302035373620353132220A
        20202069643D2273766732220A202020696E6B73636170653A76657273696F6E
        3D22302E393120723133373235220A202020736F6469706F64693A646F636E61
        6D653D22627574746F6E5F67616C6C6572795F636C69636B65642E737667223E
        0A20203C6D657461646174610A202020202069643D226D657461646174613135
        223E0A202020203C7264663A5244463E0A2020202020203C63633A576F726B0A
        2020202020202020207264663A61626F75743D22223E0A20202020202020203C
        64633A666F726D61743E696D6167652F7376672B786D6C3C2F64633A666F726D
        61743E0A20202020202020203C64633A747970650A2020202020202020202020
        7264663A7265736F757263653D22687474703A2F2F7075726C2E6F72672F6463
        2F64636D69747970652F5374696C6C496D61676522202F3E0A20202020202020
        203C64633A7469746C653E3C2F64633A7469746C653E0A2020202020203C2F63
        633A576F726B3E0A202020203C2F7264663A5244463E0A20203C2F6D65746164
        6174613E0A20203C646566730A202020202069643D2264656673313322202F3E
        0A20203C736F6469706F64693A6E616D6564766965770A202020202070616765
        636F6C6F723D2223666666666666220A2020202020626F72646572636F6C6F72
        3D2223363636363636220A2020202020626F726465726F7061636974793D2231
        220A20202020206F626A656374746F6C6572616E63653D223130220A20202020
        2067726964746F6C6572616E63653D223130220A20202020206775696465746F
        6C6572616E63653D223130220A2020202020696E6B73636170653A706167656F
        7061636974793D2230220A2020202020696E6B73636170653A70616765736861
        646F773D2232220A2020202020696E6B73636170653A77696E646F772D776964
        74683D2231303236220A2020202020696E6B73636170653A77696E646F772D68
        65696768743D22383431220A202020202069643D226E616D6564766965773131
        220A202020202073686F77677269643D2266616C7365220A2020202020696E6B
        73636170653A7A6F6F6D3D22302E34363039333735220A2020202020696E6B73
        636170653A63783D223332312E3930383631220A2020202020696E6B73636170
        653A63793D22323536220A2020202020696E6B73636170653A77696E646F772D
        783D2230220A2020202020696E6B73636170653A77696E646F772D793D223022
        0A2020202020696E6B73636170653A77696E646F772D6D6178696D697A65643D
        2230220A2020202020696E6B73636170653A63757272656E742D6C617965723D
        227376673222202F3E0A20203C670A202020202069643D2269636F6D6F6F6E2D
        69676E6F726522202F3E0A20203C706174680A2020202020643D224D35343420
        3634682D3332762D333263302D31372E362D31342E342D33322D33322D333268
        2D343438632D31372E3620302D33322031342E342D3332203332763338346330
        2031372E362031342E3420333220333220333268333276333263302031372E36
        2031342E34203332203332203332683434386331372E3620302033322D31342E
        342033322D3332762D33383463302D31372E362D31342E342D33322D33322D33
        327A4D363420393676333230682D33312E393433632D302E3032302D302E3031
        372D302E3034312D302E3033382D302E3035372D302E303538762D3338332E38
        383563302E3031372D302E30323020302E3033382D302E30343120302E303537
        2D302E303537683434372E38383563302E30323020302E30313720302E303431
        20302E30333820302E30353820302E3035387633312E393432682D333834632D
        31372E3620302D33322031342E342D333220333276307A4D353434203437392E
        393432632D302E30313720302E3032302D302E30333820302E3034312D302E30
        353820302E303538682D3434372E383835632D302E3032302D302E3031372D30
        2E3034312D302E3033382D302E3035372D302E303538762D3338332E38383563
        302E3031372D302E30323020302E3033382D302E30343120302E3035372D302E
        303537683434372E38383563302E30323020302E30313720302E30343120302E
        30333820302E30353820302E303538763338332E3838347A220A202020202069
        643D227061746835220A20202020207374796C653D2266696C6C3A2330636230
        30613B66696C6C2D6F7061636974793A302E393634373035383822202F3E0A20
        203C706174680A2020202020643D224D3438302031373663302032362E35312D
        32312E34392034382D3438203438732D34382D32312E34392D34382D34382032
        312E34392D34382034382D34382034382032312E34392034382034387A220A20
        2020202069643D227061746837220A20202020207374796C653D2266696C6C3A
        233063623030613B66696C6C2D6F7061636974793A302E393634373035383822
        202F3E0A20203C706174680A2020202020643D224D35313220343438682D3338
        34762D36346C3131322D31393220313238203136306833326C3131322D39367A
        220A202020202069643D227061746839220A20202020207374796C653D226669
        6C6C3A233063623030613B66696C6C2D6F7061636974793A302E393634373035
        383822202F3E0A3C2F7376673E0A}
      DisabledAnimation.FrameCount = 0
      DisabledAnimation.Position = 0
    end
  end
  object rsBankingStyle: TWSVGRadioButtonStyle
    UncheckedGlyph.Data = {
      0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
      20656E636F64696E673D227574662D38223F3E0D0A3C212D2D2047656E657261
      746F723A2041646F626520496C6C7573747261746F722031372E312E302C2053
      5647204578706F727420506C75672D496E202E205356472056657273696F6E3A
      20362E3030204275696C6420302920202D2D3E0D0A3C21444F43545950452073
      7667205055424C494320222D2F2F5733432F2F4454442053564720312E312F2F
      454E222022687474703A2F2F7777772E77332E6F72672F47726170686963732F
      5356472F312E312F4454442F73766731312E647464223E0D0A3C737667207665
      7273696F6E3D22312E31222069643D2243616C7175655F312220786D6C6E733D
      22687474703A2F2F7777772E77332E6F72672F323030302F7376672220786D6C
      6E733A786C696E6B3D22687474703A2F2F7777772E77332E6F72672F31393939
      2F786C696E6B2220783D223070782220793D22307078220D0A09207769647468
      3D223136707822206865696768743D2231367078222076696577426F783D2230
      20302031362031362220656E61626C652D6261636B67726F756E643D226E6577
      203020302031362031362220786D6C3A73706163653D22707265736572766522
      3E0D0A3C706174682066696C6C3D22234233423342332220643D224D382C312E
      3543342E342C312E352C312E352C342E342C312E352C3863302C332E362C322E
      392C362E352C362E352C362E3573362E352D322E392C362E352D362E35433134
      2E352C342E342C31312E362C312E352C382C312E357A204D382C31312E390D0A
      09632D322E322C302D332E392D312E372D332E392D332E3963302D322E322C31
      2E372D332E392C332E392D332E3963322E322C302C332E392C312E372C332E39
      2C332E394331312E392C31302E322C31302E322C31312E392C382C31312E397A
      222F3E0D0A3C2F7376673E0D0A}
    CheckedGlyph.Data = {
      0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
      20656E636F64696E673D227574662D38223F3E0D0A3C212D2D2047656E657261
      746F723A2041646F626520496C6C7573747261746F722031372E312E302C2053
      5647204578706F727420506C75672D496E202E205356472056657273696F6E3A
      20362E3030204275696C6420302920202D2D3E0D0A3C21444F43545950452073
      7667205055424C494320222D2F2F5733432F2F4454442053564720312E312F2F
      454E222022687474703A2F2F7777772E77332E6F72672F47726170686963732F
      5356472F312E312F4454442F73766731312E647464223E0D0A3C737667207665
      7273696F6E3D22312E31222069643D2243616C7175655F312220786D6C6E733D
      22687474703A2F2F7777772E77332E6F72672F323030302F7376672220786D6C
      6E733A786C696E6B3D22687474703A2F2F7777772E77332E6F72672F31393939
      2F786C696E6B2220783D223070782220793D22307078220D0A09207769647468
      3D223136707822206865696768743D2231367078222076696577426F783D2230
      20302031362031362220656E61626C652D6261636B67726F756E643D226E6577
      203020302031362031362220786D6C3A73706163653D22707265736572766522
      3E0D0A3C706174682066696C6C3D22233030373142432220643D224D382C312E
      3543342E342C312E352C312E352C342E342C312E352C3863302C332E362C322E
      392C362E352C362E352C362E3573362E352D322E392C362E352D362E35433134
      2E352C342E342C31312E362C312E352C382C312E357A204D382C31312E390D0A
      09632D322E322C302D332E392D312E372D332E392D332E3963302D322E322C31
      2E372D332E392C332E392D332E3963322E322C302C332E392C312E372C332E39
      2C332E394331312E392C31302E322C31302E322C31312E392C382C31312E397A
      204D382C3643362E392C362C362E312C362E392C362E312C380D0A0963302C31
      2E312C302E392C322C312E392C3273312E392D302E392C312E392D3243392E39
      2C362E392C392E312C362C382C367A222F3E0D0A3C2F7376673E0D0A}
    DisabledUncheckedGlyph.Data = {
      0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
      20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
      223F3E0A3C212D2D2047656E657261746F723A2041646F626520496C6C757374
      7261746F722031372E312E302C20535647204578706F727420506C75672D496E
      202E205356472056657273696F6E3A20362E3030204275696C6420302920202D
      2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
      6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
      3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
      6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
      2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
      202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
      323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
      332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
      693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
      65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
      696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
      672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
      6E3D22312E31220A20202069643D2243616C7175655F31220A202020783D2230
      7078220A202020793D22307078220A20202077696474683D2231367078220A20
      20206865696768743D2231367078220A20202076696577426F783D2230203020
      3136203136220A202020656E61626C652D6261636B67726F756E643D226E6577
      20302030203136203136220A202020786D6C3A73706163653D22707265736572
      7665220A202020696E6B73636170653A76657273696F6E3D22302E3931207231
      33373235220A202020736F6469706F64693A646F636E616D653D2269636F6E2D
      726164696F2D64656661756C742D64697361626C65642E737667223E3C6D6574
      61646174610A202020202069643D226D6574616461746139223E3C7264663A52
      44463E3C63633A576F726B0A2020202020202020207264663A61626F75743D22
      223E3C64633A666F726D61743E696D6167652F7376672B786D6C3C2F64633A66
      6F726D61743E3C64633A747970650A20202020202020202020207264663A7265
      736F757263653D22687474703A2F2F7075726C2E6F72672F64632F64636D6974
      7970652F5374696C6C496D61676522202F3E3C2F63633A576F726B3E3C2F7264
      663A5244463E3C2F6D657461646174613E3C646566730A202020202069643D22
      646566733722202F3E3C736F6469706F64693A6E616D6564766965770A202020
      202070616765636F6C6F723D2223666666666666220A2020202020626F726465
      72636F6C6F723D2223363636363636220A2020202020626F726465726F706163
      6974793D2231220A20202020206F626A656374746F6C6572616E63653D223130
      220A202020202067726964746F6C6572616E63653D223130220A202020202067
      75696465746F6C6572616E63653D223130220A2020202020696E6B7363617065
      3A706167656F7061636974793D2230220A2020202020696E6B73636170653A70
      616765736861646F773D2232220A2020202020696E6B73636170653A77696E64
      6F772D77696474683D22363930220A2020202020696E6B73636170653A77696E
      646F772D6865696768743D22343830220A202020202069643D226E616D656476
      69657735220A202020202073686F77677269643D2266616C7365220A20202020
      20696E6B73636170653A7A6F6F6D3D2231342E3735220A2020202020696E6B73
      636170653A63783D2238220A2020202020696E6B73636170653A63793D223822
      0A2020202020696E6B73636170653A77696E646F772D783D2230220A20202020
      20696E6B73636170653A77696E646F772D793D2230220A2020202020696E6B73
      636170653A77696E646F772D6D6178696D697A65643D2230220A202020202069
      6E6B73636170653A63757272656E742D6C617965723D2243616C7175655F3122
      202F3E3C706174680A202020202066696C6C3D2223423342334233220A202020
      2020643D224D382C312E3543342E342C312E352C312E352C342E342C312E352C
      3863302C332E362C322E392C362E352C362E352C362E3573362E352D322E392C
      362E352D362E354331342E352C342E342C31312E362C312E352C382C312E357A
      204D382C31312E392020632D322E322C302D332E392D312E372D332E392D332E
      3963302D322E322C312E372D332E392C332E392D332E3963322E322C302C332E
      392C312E372C332E392C332E394331312E392C31302E322C31302E322C31312E
      392C382C31312E397A220A202020202069643D227061746833220A2020202020
      7374796C653D2266696C6C3A236362636263623B66696C6C2D6F706163697479
      3A3122202F3E3C2F7376673E}
    DisabledCheckedGlyph.Data = {
      0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
      20656E636F64696E673D227574662D38223F3E0D0A3C212D2D2047656E657261
      746F723A2041646F626520496C6C7573747261746F722031372E312E302C2053
      5647204578706F727420506C75672D496E202E205356472056657273696F6E3A
      20362E3030204275696C6420302920202D2D3E0D0A3C21444F43545950452073
      7667205055424C494320222D2F2F5733432F2F4454442053564720312E312F2F
      454E222022687474703A2F2F7777772E77332E6F72672F47726170686963732F
      5356472F312E312F4454442F73766731312E647464223E0D0A3C737667207665
      7273696F6E3D22312E31222069643D2243616C7175655F312220786D6C6E733D
      22687474703A2F2F7777772E77332E6F72672F323030302F7376672220786D6C
      6E733A786C696E6B3D22687474703A2F2F7777772E77332E6F72672F31393939
      2F786C696E6B2220783D223070782220793D22307078220D0A09207769647468
      3D223136707822206865696768743D2231367078222076696577426F783D2230
      20302031362031362220656E61626C652D6261636B67726F756E643D226E6577
      203020302031362031362220786D6C3A73706163653D22707265736572766522
      3E0D0A3C706174682066696C6C3D22234343434343432220643D224D382C312E
      3543342E342C312E352C312E352C342E342C312E352C3863302C332E362C322E
      392C362E352C362E352C362E3573362E352D322E392C362E352D362E35433134
      2E352C342E342C31312E362C312E352C382C312E357A204D382C31312E390D0A
      09632D322E322C302D332E392D312E372D332E392D332E3963302D322E322C31
      2E372D332E392C332E392D332E3963322E322C302C332E392C312E372C332E39
      2C332E394331312E392C31302E322C31302E322C31312E392C382C31312E397A
      204D382C3643362E392C362C362E312C362E392C362E312C380D0A0963302C31
      2E312C302E392C322C312E392C3273312E392D302E392C312E392D3243392E39
      2C362E392C392E312C362C382C367A222F3E0D0A3C2F7376673E0D0A}
    Targets = <
      item
        RadioButton = rbBankingPaymentMethodCreditCard
      end
      item
        RadioButton = rbBankingPaymentMethodPayPal
      end
      item
        RadioButton = rbBankingPersonalInfoGenderFemale
      end
      item
        RadioButton = rbBankingPersonalInfoGenderMale
      end
      item
        RadioButton = rbGalleryStyledRBChecked
      end
      item
        RadioButton = rbGalleryStyledRBDefault
      end
      item
        RadioButton = rbGalleryStyledRBDisabled
      end
      item
        RadioButton = rbGalleryStyledRBDisabledChecked
      end>
    Left = 332
    Top = 32
  end
  object csBankingStyle: TWSVGCheckBoxStyle
    UncheckedGlyph.Data = {
      0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
      20656E636F64696E673D227574662D38223F3E0D0A3C212D2D2047656E657261
      746F723A2041646F626520496C6C7573747261746F722031372E312E302C2053
      5647204578706F727420506C75672D496E202E205356472056657273696F6E3A
      20362E3030204275696C6420302920202D2D3E0D0A3C21444F43545950452073
      7667205055424C494320222D2F2F5733432F2F4454442053564720312E312F2F
      454E222022687474703A2F2F7777772E77332E6F72672F47726170686963732F
      5356472F312E312F4454442F73766731312E647464223E0D0A3C737667207665
      7273696F6E3D22312E31222069643D2243616C7175655F312220786D6C6E733D
      22687474703A2F2F7777772E77332E6F72672F323030302F7376672220786D6C
      6E733A786C696E6B3D22687474703A2F2F7777772E77332E6F72672F31393939
      2F786C696E6B2220783D223070782220793D22307078220D0A09207769647468
      3D223136707822206865696768743D2231367078222076696577426F783D2230
      20302031362031362220656E61626C652D6261636B67726F756E643D226E6577
      203020302031362031362220786D6C3A73706163653D22707265736572766522
      3E0D0A3C706174682066696C6C3D22234233423342332220643D224D342E382C
      312E3568362E3563312E382C302C332E322C312E352C332E322C332E3276362E
      3563302C312E382D312E352C332E322D332E322C332E3248342E38632D312E38
      2C302D332E322D312E352D332E322D332E3256342E370D0A0943312E352C332C
      332C312E352C342E382C312E357A222F3E0D0A3C2F7376673E0D0A}
    CheckedGlyph.Data = {
      0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
      20656E636F64696E673D227574662D38223F3E0D0A3C212D2D2047656E657261
      746F723A2041646F626520496C6C7573747261746F722031372E312E302C2053
      5647204578706F727420506C75672D496E202E205356472056657273696F6E3A
      20362E3030204275696C6420302920202D2D3E0D0A3C21444F43545950452073
      7667205055424C494320222D2F2F5733432F2F4454442053564720312E312F2F
      454E222022687474703A2F2F7777772E77332E6F72672F47726170686963732F
      5356472F312E312F4454442F73766731312E647464223E0D0A3C737667207665
      7273696F6E3D22312E31222069643D2243616C7175655F312220786D6C6E733D
      22687474703A2F2F7777772E77332E6F72672F323030302F7376672220786D6C
      6E733A786C696E6B3D22687474703A2F2F7777772E77332E6F72672F31393939
      2F786C696E6B2220783D223070782220793D22307078220D0A09207769647468
      3D223136707822206865696768743D2231367078222076696577426F783D2230
      20302031362031362220656E61626C652D6261636B67726F756E643D226E6577
      203020302031362031362220786D6C3A73706163653D22707265736572766522
      3E0D0A0D0A3C706174682066696C6C3D22233030373142432220643D224D3131
      2E322C312E3548342E3843332C312E352C312E352C332C312E352C342E377636
      2E3563302C312E382C312E352C332E322C332E322C332E3268362E3563312E38
      2C302C332E322D312E352C332E322D332E3256342E370D0A094331342E352C33
      2C31332C312E352C31312E322C312E357A204D31322E322C362E376C2D342E37
      2C342E37632D302E312C302E312D302E332C302E312D302E332C306C2D302E37
      2D302E37632D302E312D302E312D302E332D302E332D302E332D302E334C342E
      362C382E360D0A0963302C302D302E312D302E312D302E312D302E3263302D30
      2E312C302D302E312C302E312D302E326C302E372D302E3663302C302C302E31
      2D302E312C302E322D302E3163302E312C302C302E312C302C302E322C302E31
      6C312E362C312E3663302E312C302E312C302E322C302E312C302E332C300D0A
      096C332E362D332E3663302E312D302E312C302E322D302E312C302E332C306C
      302E372C302E374331322E332C362E342C31322E332C362E362C31322E322C36
      2E377A222F3E0D0A3C2F7376673E0D0A}
    GrayedGlyph.Data = {
      0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
      20656E636F64696E673D227574662D38223F3E0D0A3C212D2D2047656E657261
      746F723A2041646F626520496C6C7573747261746F722031372E312E302C2053
      5647204578706F727420506C75672D496E202E205356472056657273696F6E3A
      20362E3030204275696C6420302920202D2D3E0D0A3C21444F43545950452073
      7667205055424C494320222D2F2F5733432F2F4454442053564720312E312F2F
      454E222022687474703A2F2F7777772E77332E6F72672F47726170686963732F
      5356472F312E312F4454442F73766731312E647464223E0D0A3C737667207665
      7273696F6E3D22312E31222069643D2243616C7175655F312220786D6C6E733D
      22687474703A2F2F7777772E77332E6F72672F323030302F7376672220786D6C
      6E733A786C696E6B3D22687474703A2F2F7777772E77332E6F72672F31393939
      2F786C696E6B2220783D223070782220793D22307078220D0A09207769647468
      3D223136707822206865696768743D2231367078222076696577426F783D2230
      20302031362031362220656E61626C652D6261636B67726F756E643D226E6577
      203020302031362031362220786D6C3A73706163653D22707265736572766522
      3E0D0A3C706174682066696C6C3D22233030373142432220643D224D31312E32
      2C312E3548342E3843332C312E352C312E352C332C312E352C342E3776362E35
      63302C312E382C312E352C332E322C332E322C332E3268362E3563312E382C30
      2C332E322D312E352C332E322D332E3256342E370D0A094331342E352C332C31
      332C312E352C31312E322C312E357A222F3E0D0A3C2F7376673E0D0A}
    DisabledUncheckedGlyph.Data = {
      0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
      20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
      223F3E0A3C212D2D2047656E657261746F723A2041646F626520496C6C757374
      7261746F722031372E312E302C20535647204578706F727420506C75672D496E
      202E205356472056657273696F6E3A20362E3030204275696C6420302920202D
      2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
      6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
      3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
      6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
      2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
      202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
      323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
      332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
      693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
      65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
      696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
      672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
      6E3D22312E31220A20202069643D2243616C7175655F31220A202020783D2230
      7078220A202020793D22307078220A20202077696474683D2231367078220A20
      20206865696768743D2231367078220A20202076696577426F783D2230203020
      3136203136220A202020656E61626C652D6261636B67726F756E643D226E6577
      20302030203136203136220A202020786D6C3A73706163653D22707265736572
      7665220A202020696E6B73636170653A76657273696F6E3D22302E3931207231
      33373235220A202020736F6469706F64693A646F636E616D653D2269636F6E2D
      636865636B626F782D64656661756C742D64697361626C65642E737667223E3C
      6D657461646174610A202020202069643D226D6574616461746139223E3C7264
      663A5244463E3C63633A576F726B0A2020202020202020207264663A61626F75
      743D22223E3C64633A666F726D61743E696D6167652F7376672B786D6C3C2F64
      633A666F726D61743E3C64633A747970650A2020202020202020202020726466
      3A7265736F757263653D22687474703A2F2F7075726C2E6F72672F64632F6463
      6D69747970652F5374696C6C496D61676522202F3E3C2F63633A576F726B3E3C
      2F7264663A5244463E3C2F6D657461646174613E3C646566730A202020202069
      643D22646566733722202F3E3C736F6469706F64693A6E616D6564766965770A
      202020202070616765636F6C6F723D2223666666666666220A2020202020626F
      72646572636F6C6F723D2223363636363636220A2020202020626F726465726F
      7061636974793D2231220A20202020206F626A656374746F6C6572616E63653D
      223130220A202020202067726964746F6C6572616E63653D223130220A202020
      20206775696465746F6C6572616E63653D223130220A2020202020696E6B7363
      6170653A706167656F7061636974793D2230220A2020202020696E6B73636170
      653A70616765736861646F773D2232220A2020202020696E6B73636170653A77
      696E646F772D77696474683D2231323039220A2020202020696E6B7363617065
      3A77696E646F772D6865696768743D22373431220A202020202069643D226E61
      6D65647669657735220A202020202073686F77677269643D2266616C7365220A
      2020202020696E6B73636170653A7A6F6F6D3D2231342E3735220A2020202020
      696E6B73636170653A63783D222D31322E323731313836220A2020202020696E
      6B73636170653A63793D2238220A2020202020696E6B73636170653A77696E64
      6F772D783D22343733220A2020202020696E6B73636170653A77696E646F772D
      793D22313132220A2020202020696E6B73636170653A77696E646F772D6D6178
      696D697A65643D2230220A2020202020696E6B73636170653A63757272656E74
      2D6C617965723D2243616C7175655F3122202F3E3C706174680A202020202066
      696C6C3D2223423342334233220A2020202020643D224D342E382C312E356836
      2E3563312E382C302C332E322C312E352C332E322C332E3276362E3563302C31
      2E382D312E352C332E322D332E322C332E3248342E38632D312E382C302D332E
      322D312E352D332E322D332E3256342E37202043312E352C332C332C312E352C
      342E382C312E357A220A202020202069643D227061746833220A202020202073
      74796C653D2266696C6C3A236363636363633B66696C6C2D6F7061636974793A
      3122202F3E3C2F7376673E}
    DisabledCheckedGlyph.Data = {
      0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
      20656E636F64696E673D227574662D38223F3E0D0A3C212D2D2047656E657261
      746F723A2041646F626520496C6C7573747261746F722031372E312E302C2053
      5647204578706F727420506C75672D496E202E205356472056657273696F6E3A
      20362E3030204275696C6420302920202D2D3E0D0A3C21444F43545950452073
      7667205055424C494320222D2F2F5733432F2F4454442053564720312E312F2F
      454E222022687474703A2F2F7777772E77332E6F72672F47726170686963732F
      5356472F312E312F4454442F73766731312E647464223E0D0A3C737667207665
      7273696F6E3D22312E31222069643D2243616C7175655F312220786D6C6E733D
      22687474703A2F2F7777772E77332E6F72672F323030302F7376672220786D6C
      6E733A786C696E6B3D22687474703A2F2F7777772E77332E6F72672F31393939
      2F786C696E6B2220783D223070782220793D22307078220D0A09207769647468
      3D223136707822206865696768743D2231367078222076696577426F783D2230
      20302031362031362220656E61626C652D6261636B67726F756E643D226E6577
      203020302031362031362220786D6C3A73706163653D22707265736572766522
      3E0D0A3C706174682066696C6C3D22234343434343432220643D224D31312E32
      2C312E3548342E3843332C312E352C312E352C332C312E352C342E3776362E35
      63302C312E382C312E352C332E322C332E322C332E3268362E3563312E382C30
      2C332E322D312E352C332E322D332E3256342E370D0A094331342E352C332C31
      332C312E352C31312E322C312E357A204D31322E322C362E376C2D342E372C34
      2E37632D302E312C302E312D302E332C302E312D302E332C306C2D302E372D30
      2E37632D302E312D302E312D302E332D302E332D302E332D302E334C342E362C
      382E360D0A0963302C302D302E312D302E312D302E312D302E3263302D302E31
      2C302D302E312C302E312D302E326C302E372D302E3663302C302C302E312D30
      2E312C302E322D302E3163302E312C302C302E312C302C302E322C302E316C31
      2E362C312E3663302E312C302E312C302E322C302E312C302E332C300D0A096C
      332E362D332E3663302E312D302E312C302E322D302E312C302E332C306C302E
      372C302E374331322E332C362E342C31322E332C362E362C31322E322C362E37
      7A222F3E0D0A0D0A3C2F7376673E0D0A}
    DisabledGrayedGlyph.Data = {
      0C5457535647477261706869633C3F786D6C2076657273696F6E3D22312E3022
      20656E636F64696E673D225554462D3822207374616E64616C6F6E653D226E6F
      223F3E0A3C212D2D2047656E657261746F723A2041646F626520496C6C757374
      7261746F722031372E312E302C20535647204578706F727420506C75672D496E
      202E205356472056657273696F6E3A20362E3030204275696C6420302920202D
      2D3E0A0A3C7376670A202020786D6C6E733A64633D22687474703A2F2F707572
      6C2E6F72672F64632F656C656D656E74732F312E312F220A202020786D6C6E73
      3A63633D22687474703A2F2F6372656174697665636F6D6D6F6E732E6F72672F
      6E7323220A202020786D6C6E733A7264663D22687474703A2F2F7777772E7733
      2E6F72672F313939392F30322F32322D7264662D73796E7461782D6E7323220A
      202020786D6C6E733A7376673D22687474703A2F2F7777772E77332E6F72672F
      323030302F737667220A202020786D6C6E733D22687474703A2F2F7777772E77
      332E6F72672F323030302F737667220A202020786D6C6E733A736F6469706F64
      693D22687474703A2F2F736F6469706F64692E736F75726365666F7267652E6E
      65742F4454442F736F6469706F64692D302E647464220A202020786D6C6E733A
      696E6B73636170653D22687474703A2F2F7777772E696E6B73636170652E6F72
      672F6E616D657370616365732F696E6B7363617065220A20202076657273696F
      6E3D22312E31220A20202069643D2243616C7175655F31220A202020783D2230
      7078220A202020793D22307078220A20202077696474683D2231367078220A20
      20206865696768743D2231367078220A20202076696577426F783D2230203020
      3136203136220A202020656E61626C652D6261636B67726F756E643D226E6577
      20302030203136203136220A202020786D6C3A73706163653D22707265736572
      7665220A202020696E6B73636170653A76657273696F6E3D22302E3931207231
      33373235220A202020736F6469706F64693A646F636E616D653D2269636F6E2D
      636865636B626F782D64656661756C742D64697361626C65642E737667223E3C
      6D657461646174610A202020202069643D226D6574616461746139223E3C7264
      663A5244463E3C63633A576F726B0A2020202020202020207264663A61626F75
      743D22223E3C64633A666F726D61743E696D6167652F7376672B786D6C3C2F64
      633A666F726D61743E3C64633A747970650A2020202020202020202020726466
      3A7265736F757263653D22687474703A2F2F7075726C2E6F72672F64632F6463
      6D69747970652F5374696C6C496D61676522202F3E3C2F63633A576F726B3E3C
      2F7264663A5244463E3C2F6D657461646174613E3C646566730A202020202069
      643D22646566733722202F3E3C736F6469706F64693A6E616D6564766965770A
      202020202070616765636F6C6F723D2223666666666666220A2020202020626F
      72646572636F6C6F723D2223363636363636220A2020202020626F726465726F
      7061636974793D2231220A20202020206F626A656374746F6C6572616E63653D
      223130220A202020202067726964746F6C6572616E63653D223130220A202020
      20206775696465746F6C6572616E63653D223130220A2020202020696E6B7363
      6170653A706167656F7061636974793D2230220A2020202020696E6B73636170
      653A70616765736861646F773D2232220A2020202020696E6B73636170653A77
      696E646F772D77696474683D2231323039220A2020202020696E6B7363617065
      3A77696E646F772D6865696768743D22373431220A202020202069643D226E61
      6D65647669657735220A202020202073686F77677269643D2266616C7365220A
      2020202020696E6B73636170653A7A6F6F6D3D2231342E3735220A2020202020
      696E6B73636170653A63783D222D31322E323731313836220A2020202020696E
      6B73636170653A63793D2238220A2020202020696E6B73636170653A77696E64
      6F772D783D22343733220A2020202020696E6B73636170653A77696E646F772D
      793D22313132220A2020202020696E6B73636170653A77696E646F772D6D6178
      696D697A65643D2230220A2020202020696E6B73636170653A63757272656E74
      2D6C617965723D2243616C7175655F3122202F3E3C706174680A202020202066
      696C6C3D2223423342334233220A2020202020643D224D342E382C312E356836
      2E3563312E382C302C332E322C312E352C332E322C332E3276362E3563302C31
      2E382D312E352C332E322D332E322C332E3248342E38632D312E382C302D332E
      322D312E352D332E322D332E3256342E37202043312E352C332C332C312E352C
      342E382C312E357A220A202020202069643D227061746833220A202020202073
      74796C653D2266696C6C3A236363636363633B66696C6C2D6F7061636974793A
      3122202F3E3C2F7376673E}
    Targets = <
      item
        CheckBox = ckBankingAboutProductInAShop
      end
      item
        CheckBox = ckBankingAboutProductAsAGift
      end
      item
        CheckBox = ckBankingAboutProductByFriends
      end
      item
        CheckBox = ckBankingAboutProductFromAdv
      end
      item
        CheckBox = ckBankingAboutProductOnTheInternet
      end
      item
        CheckBox = ckBankingAboutProductOther
      end
      item
        CheckBox = ckBankingAboutProductPrivate
      end
      item
        CheckBox = ckBankingAboutProductProfessional
      end
      item
        CheckBox = ckFreshBooksPaymentOptionsForAFriend
      end
      item
        CheckBox = ckFreshBooksPaymentOptionsForMe
      end
      item
        CheckBox = ckFreshBooksPaymentOptionsGift
      end
      item
        CheckBox = ckFreshBooksPaymentOptionsOther
      end
      item
        CheckBox = ckGalleryStyledCBChecked
      end
      item
        CheckBox = ckGalleryStyledCBDefault
      end
      item
        CheckBox = ckGalleryStyledCBDisabled
      end
      item
        CheckBox = ckGalleryStyledCBDisabledChecked
      end
      item
        CheckBox = ckGalleryStyledCBDisabledGrayed
      end
      item
        CheckBox = ckGalleryStyledCBGrayed
      end>
    Left = 284
    Top = 32
  end
  object odOpen: TOpenDialog
    DefaultExt = 'svg'
    Filter = 'Scalable Vector Graphic|*.svg|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open a SVG file'
    Left = 238
    Top = 32
  end
  object cdBgColor: TColorDialog
    Left = 192
    Top = 32
  end
  object tiSlideshow: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = tiSlideshowTimer
    Left = 95
    Top = 32
  end
  object ilImages: TWSVGImageList
    DPIScale = True
    Left = 143
    Top = 32
    Bitmap = {
      494C010104000900040010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002F2F2F3834343440343434403434344007070708000000000F0F0F103434
      3440000000000000000000000000000000000000000000000000000000000000
      00002C302C3830363040303630403036304007070708000000000F0F0F103036
      3040000000000000000000000000000000000000000000000000000000000000
      00002C342C38303B3040303B3040303B304007070708000000000F0F0F10303B
      3040000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004646
      46604A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF1D1D1D2000000000343434404A4A
      4AFF535353800000000000000000000000000000000000000000000000003D49
      3D60095D0BFF095D0BFF095D0BFF095D0BFF1C1D1C200000000030363040095D
      0BFF425743800000000000000000000000000000000000000000000000003D54
      3D6009B00BFF09B00BFF09B00BFF09B00BFF1C1E1C2000000000303B304009B0
      0BFF426C43800000000000000000000000000000000000000000000000000000
      00000000000012131214435647650D0D0D0E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005858
      58974E4E4EF7545454875656568F4A4A4AFF1D1D1D2000000000343434404A4A
      4AFF5A5A5AB7000000000000000000000000000000000000000000000000405E
      4197115F13F7415A4287415C428F095D0BFF1C1D1C200000000030363040095D
      0BFF386439B7000000000000000000000000000000000000000000000000407B
      419711AD13F7417142874176428F09B00BFF1C1E1C2000000000303B304009B0
      0BFF388E39B70000000000000000000000000000000000000000000000000000
      00001819181B478953BD3E944ED8498354B00D0D0D0E00000000000000000000
      0000000000000000000000000000000000000000000000000000000000005959
      59C7555555DF00000000000000004A4A4AFF1D1D1D2000000000343434404A4A
      4AFF535353E70000000000000000000000000000000000000000000000003165
      32C7236325DF0000000000000000095D0BFF1C1D1C200000000030363040095D
      0BFF1D621FE70000000000000000000000000000000000000000000000003197
      32C723A225DF000000000000000009B00BFF1C1E1C2000000000303B304009B0
      0BFF1DA61FE70000000000000000000000000000000000000000000000001819
      181B478953BD3E944ED83E944ED83E944ED8498354B00D0D0D0E000000000000
      0000000000000000000000000000000000000000000000000000000000005050
      50EF4E4E4EF73E3E3E50464646604A4A4AFF5A5A5AAF53535380595959BF4A4A
      4AFF4A4A4AFF0F0F0F1000000000000000000000000000000000000000001761
      19EF115F13F7374038503D493D60095D0BFF3B633CAF42574380356536BF095D
      0BFF095D0BFF0F0F0F10000000000000000000000000000000000000000017A9
      19EF11AD13F7374838503D543D6009B00BFF3B8A3CAF426C4380359236BF09B0
      0BFF09B00BFF0F0F0F10000000000000000000000000000000001819181B4789
      53BD3E944ED83E944ED83E944ED83E944ED83E944ED8498354B00D0D0D0E0000
      0000000000000000000000000000000000000000000000000000161616184A4A
      4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A
      4AFF4A4A4AFF2F2F2F380000000000000000000000000000000015161518095D
      0BFF095D0BFF095D0BFF095D0BFF095D0BFF095D0BFF095D0BFF095D0BFF095D
      0BFF095D0BFF2C302C38000000000000000000000000000000001517151809B0
      0BFF09B00BFF09B00BFF09B00BFF09B00BFF09B00BFF09B00BFF09B00BFF09B0
      0BFF09B00BFF2C342C380000000000000000000000001819181B478953BD3E94
      4ED83E944ED83E944ED83E944ED83E944ED83E944ED83E944ED8498354B00D0D
      0D0E0000000000000000000000000000000000000000000000000F0F0F104A4A
      4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A
      4AFF4A4A4AFF29292930000000000000000000000000000000000F0F0F10095D
      0BFF095D0BFF095D0BFF095D0BFF095D0BFF095D0BFF095D0BFF095D0BFF095D
      0BFF095D0BFF272A2730000000000000000000000000000000000F0F0F1009B0
      0BFF09B00BFF09B00BFF09B00BFF09B00BFF09B00BFF09B00BFF09B00BFF09B0
      0BFF09B00BFF272D2730000000000000000000000000498354B03E944ED83E94
      4ED83E944ED8478953BD343D3644438F51CB3E944ED83E944ED83E944ED84983
      54B00D0D0D0E00000000000000000000000023232328505050EF464646605858
      58974A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A
      4AFF5A5A5AB739393948505050EF3434344022242228176119EF3D493D60405E
      4197095D0BFF095D0BFF095D0BFF095D0BFF095D0BFF095D0BFF095D0BFF095D
      0BFF386439B7343B3448176119EF303630402226222817A919EF3D543D60407B
      419709B00BFF09B00BFF09B00BFF09B00BFF09B00BFF09B00BFF09B00BFF09B0
      0BFF388E39B73441344817A919EF303B3040000000004A6850803E944ED83E94
      4ED8478953BD1819181B0000000023262429438F51CB3E944ED83E944ED83E94
      4ED8498354B00D0D0D0E000000000000000007070708585858CF4A4A4AFF4040
      40555A5A5AB74A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF5858
      58CF3232323D505050EF505050EF1D1D1D20070707082D642ECF095D0BFF3943
      3A55386439B7095D0BFF095D0BFF095D0BFF095D0BFF095D0BFF095D0BFF2D64
      2ECF2F332F3D176119EF176119EF1C1D1C20070707082D9B2ECF09B00BFF394C
      3A55388E39B709B00BFF09B00BFF09B00BFF09B00BFF09B00BFF09B00BFF2D9B
      2ECF2F382F3D17A919EF17A919EF1C1E1C20000000000000000049644E7A4983
      54B01819181B00000000000000000000000023262429438F51CB3E944ED83E94
      4ED83E944ED8498354B00D0D0D0E00000000000000001D1D1D20555555DF4E4E
      4EF736363644565656D74A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF535353E73131
      313C535353E74E4E4EF72F2F2F3800000000000000001C1D1C20236325DF115F
      13F732383244286329D7095D0BFF095D0BFF095D0BFF095D0BFF1D621FE72E33
      2E3C1D621FE7115F13F72C302C3800000000000000001C1E1C2023A225DF11AD
      13F7323E3244289F29D709B00BFF09B00BFF09B00BFF09B00BFF1DA61FE72E37
      2E3C1DA61FE711AD13F72C342C38000000000000000000000000000000000000
      0000000000000000000000000000000000000000000023262429438F51CB3E94
      4ED83E944ED83E944ED84A7F55A9000000000000000000000000292929305050
      50EF505050EF36363643535353E74A4A4AFF4A4A4AFF4E4E4EF73232323D5555
      55DF4A4A4AFF3939394800000000000000000000000000000000272A27301761
      19EF176119EF323832431D621FE7095D0BFF095D0BFF115F13F72F332F3D2363
      25DF095D0BFF343B344800000000000000000000000000000000272D273017A9
      19EF17A919EF323D32431DA61FE709B00BFF09B00BFF11AD13F72F382F3D23A2
      25DF09B00BFF3441344800000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000023262429438F
      51CB3E944ED83E944ED8458C52C4060606070000000000000000000000003434
      34404E4E4EF7555555DF3B3B3B4B4E4E4EF74A4A4AFF40404055585858CF4A4A
      4AFF464646600000000000000000000000000000000000000000000000003036
      3040115F13F7236325DF353D364B115F13F7095D0BFF394339552D642ECF095D
      0BFF3D493D60000000000000000000000000000000000000000000000000303B
      304011AD13F723A225DF3544364B11AD13F709B00BFF394C39552D9B2ECF09B0
      0BFF3D543D600000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002326
      2429438F51CB438F51CB23262429000000000000000000000000000000000000
      0000424242584A4A4AFF585858CF3232323D2D2D2D365A5A5AB74A4A4AFF5050
      5078000000000000000000000000000000000000000000000000000000000000
      00003A443B58095D0BFF2D642ECF2F332F3D2A2E2A36386439B7095D0BFF4254
      4278000000000000000000000000000000000000000000000000000000000000
      00003A4E3B5809B00BFF2D9B2ECF2F382F3D2A322A36388E39B709B00BFF4266
      4278000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000121312140D0D0D0E00000000000000000000000000000000000000000000
      0000000000004D4D4D704A4A4AFF535353E7555555DF4A4A4AFF5656568F0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000040504170095D0BFF1D621FE7236325DF095D0BFF415C428F0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004060417009B00BFF1DA61FE723A225DF09B00BFF4176428F0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000046464660585858CF565656D753535380000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000003D493D602D642ECF286329D742574380000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000003D543D602D9B2ECF289F29D7426C4380000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
    Pictures = {
      040000000C000000000000005457535647477261706869633B07000000000000
      3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
      462D3822207374616E64616C6F6E653D226E6F223F3E0A3C7376670A20202078
      6D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F656C656D
      656E74732F312E312F220A202020786D6C6E733A63633D22687474703A2F2F63
      72656174697665636F6D6D6F6E732E6F72672F6E7323220A202020786D6C6E73
      3A7264663D22687474703A2F2F7777772E77332E6F72672F313939392F30322F
      32322D7264662D73796E7461782D6E7323220A202020786D6C6E733A7376673D
      22687474703A2F2F7777772E77332E6F72672F323030302F737667220A202020
      786D6C6E733D22687474703A2F2F7777772E77332E6F72672F323030302F7376
      67220A202020786D6C6E733A736F6469706F64693D22687474703A2F2F736F64
      69706F64692E736F75726365666F7267652E6E65742F4454442F736F6469706F
      64692D302E647464220A202020786D6C6E733A696E6B73636170653D22687474
      703A2F2F7777772E696E6B73636170652E6F72672F6E616D657370616365732F
      696E6B7363617065220A20202077696474683D223136220A2020206865696768
      743D223136220A20202076696577426F783D22302030203136203136220A2020
      2069643D2273766734313336220A20202076657273696F6E3D22312E31220A20
      2020696E6B73636170653A76657273696F6E3D22302E39312072313337323522
      0A202020736F6469706F64693A646F636E616D653D2269636F6E2D7375636365
      73732E737667223E0A20203C6D657461646174610A202020202069643D226D65
      74616461746134313434223E0A202020203C7264663A5244463E0A2020202020
      203C63633A576F726B0A2020202020202020207264663A61626F75743D22223E
      0A20202020202020203C64633A666F726D61743E696D6167652F7376672B786D
      6C3C2F64633A666F726D61743E0A20202020202020203C64633A747970650A20
      202020202020202020207264663A7265736F757263653D22687474703A2F2F70
      75726C2E6F72672F64632F64636D69747970652F5374696C6C496D6167652220
      2F3E0A2020202020203C2F63633A576F726B3E0A202020203C2F7264663A5244
      463E0A20203C2F6D657461646174613E0A20203C646566730A20202020206964
      3D22646566733431343222202F3E0A20203C736F6469706F64693A6E616D6564
      766965770A202020202070616765636F6C6F723D2223666666666666220A2020
      202020626F72646572636F6C6F723D2223363636363636220A2020202020626F
      726465726F7061636974793D2231220A20202020206F626A656374746F6C6572
      616E63653D223130220A202020202067726964746F6C6572616E63653D223130
      220A20202020206775696465746F6C6572616E63653D223130220A2020202020
      696E6B73636170653A706167656F7061636974793D2230220A2020202020696E
      6B73636170653A70616765736861646F773D2232220A2020202020696E6B7363
      6170653A77696E646F772D77696474683D2231313730220A2020202020696E6B
      73636170653A77696E646F772D6865696768743D22373931220A202020202069
      643D226E616D65647669657734313430220A202020202073686F77677269643D
      2266616C7365220A2020202020696E6B73636170653A7A6F6F6D3D2232362E39
      30363235220A2020202020696E6B73636170653A63783D22352E383931323336
      32220A2020202020696E6B73636170653A63793D22382E37343134323034220A
      2020202020696E6B73636170653A77696E646F772D783D22333034220A202020
      2020696E6B73636170653A77696E646F772D793D223932220A2020202020696E
      6B73636170653A77696E646F772D6D6178696D697A65643D2230220A20202020
      20696E6B73636170653A63757272656E742D6C617965723D2273766734313336
      22202F3E0A20203C706174680A2020202020643D224D31352E3520342E343937
      6330202E3235382D2E30392E3437382D2E3237312E3635386C2D382E33323220
      382E333232632D2E3138312E3138312D2E342E3237312D2E3635382E32373173
      2D2E3437382D2E30392D2E3635382D2E3237316C2D342E38322D342E38313963
      2D2E3138312D2E3138312D2E3237312D2E342D2E3237312D2E363538732E3039
      2D2E3437382E3237312D2E3635386C312E3331362D312E333136632E3138312D
      2E3138312E342D2E3237312E3635382D2E323731732E3437382E30392E363538
      2E3237316C322E38343520322E38353520362E3334382D362E333538632E3138
      312D2E3138312E342D2E3237312E3635382D2E323731732E3437382E30392E36
      35382E3237316C312E33313620312E333136632E3138322E3138312E3237322E
      342E3237322E3635387A220A202020202069643D227061746834313338220A20
      202020207374796C653D2266696C6C3A233430613132393B66696C6C2D6F7061
      636974793A302E373035383832333722202F3E0A3C2F7376673E0A0400000000
      000000000000000C00000000000000545753564747726170686963790E000000
      0000003C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D
      225554462D3822207374616E64616C6F6E653D226E6F223F3E0A3C212D2D2053
      766720566563746F722049636F6E73203A20687474703A2F2F7777772E6F6E6C
      696E65776562666F6E74732E636F6D2F69636F6E202D2D3E0A0A3C7376670A20
      2020786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
      6C656D656E74732F312E312F220A202020786D6C6E733A63633D22687474703A
      2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A202020786D
      6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F313939392F
      30322F32322D7264662D73796E7461782D6E7323220A202020786D6C6E733A73
      76673D22687474703A2F2F7777772E77332E6F72672F323030302F737667220A
      202020786D6C6E733D22687474703A2F2F7777772E77332E6F72672F32303030
      2F737667220A202020786D6C6E733A736F6469706F64693D22687474703A2F2F
      736F6469706F64692E736F75726365666F7267652E6E65742F4454442F736F64
      69706F64692D302E647464220A202020786D6C6E733A696E6B73636170653D22
      687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D6573706163
      65732F696E6B7363617065220A20202076657273696F6E3D22312E31220A2020
      20783D22307078220A202020793D22307078220A20202076696577426F783D22
      30203020313030302031303030220A202020656E61626C652D6261636B67726F
      756E643D226E65772030203020313030302031303030220A202020786D6C3A73
      706163653D227072657365727665220A20202069643D2273766732220A202020
      696E6B73636170653A76657273696F6E3D22302E393120723133373235220A20
      2020736F6469706F64693A646F636E616D653D22627574746F6E5F686F6D655F
      64656661756C742E737667223E3C646566730A202020202069643D2264656673
      343822202F3E3C736F6469706F64693A6E616D6564766965770A202020202070
      616765636F6C6F723D2223666666666666220A2020202020626F72646572636F
      6C6F723D2223363636363636220A2020202020626F726465726F706163697479
      3D2231220A20202020206F626A656374746F6C6572616E63653D223130220A20
      2020202067726964746F6C6572616E63653D223130220A202020202067756964
      65746F6C6572616E63653D223130220A2020202020696E6B73636170653A7061
      67656F7061636974793D2230220A2020202020696E6B73636170653A70616765
      736861646F773D2232220A2020202020696E6B73636170653A77696E646F772D
      77696474683D2231393230220A2020202020696E6B73636170653A77696E646F
      772D6865696768743D2231303238220A202020202069643D226E616D65647669
      65773436220A202020202073686F77677269643D2266616C7365220A20202020
      20696E6B73636170653A7A6F6F6D3D22302E323336220A2020202020696E6B73
      636170653A63783D22353030220A2020202020696E6B73636170653A63793D22
      353030220A2020202020696E6B73636170653A77696E646F772D783D222D3822
      0A2020202020696E6B73636170653A77696E646F772D793D222D38220A202020
      2020696E6B73636170653A77696E646F772D6D6178696D697A65643D2231220A
      2020202020696E6B73636170653A63757272656E742D6C617965723D22737667
      3222202F3E3C6D657461646174610A202020202069643D226D65746164617461
      34223E2053766720566563746F722049636F6E73203A20687474703A2F2F7777
      772E6F6E6C696E65776562666F6E74732E636F6D2F69636F6E203C7264663A52
      44463E3C63633A576F726B0A20202020207264663A61626F75743D22223E3C64
      633A666F726D61743E696D6167652F7376672B786D6C3C2F64633A666F726D61
      743E3C64633A747970650A202020202020207264663A7265736F757263653D22
      687474703A2F2F7075726C2E6F72672F64632F64636D69747970652F5374696C
      6C496D61676522202F3E3C2F63633A576F726B3E3C2F7264663A5244463E3C2F
      6D657461646174613E3C670A202020202069643D226736220A20202020207374
      796C653D2266696C6C3A233462346234623B66696C6C2D6F7061636974793A31
      223E3C670A2020202020202069643D226738220A202020202020207374796C65
      3D2266696C6C3A233462346234623B66696C6C2D6F7061636974793A31223E3C
      670A20202020202020202069643D22673130220A202020202020202020737479
      6C653D2266696C6C3A233462346234623B66696C6C2D6F7061636974793A3122
      3E3C706174680A2020202020202020202020643D224D3534372E362C3231382E
      32632D31322E342D31342E352D33302E362D32322E392D34392E372D32322E39
      632D31392E312C302D33372E332C382E342D34392E372C32322E394C3136392E
      342C353435632D31372E332C32302E332D32342E382C34372E322D32302E332C
      37332E356C35302E362C3330312E3963352E332C33312E352C33322E352C3534
      2E332C36342E342C35342E33683234332E32632D302E312D312E312C302E312D
      312E362C302E312D322E365637353363302D31342E322C31312E332D32352E39
      2C32352E362D32352E39683131372E326331342E322C302C32352E392C31312E
      372C32352E392C32352E39763232312E386835352E386333312E392C302C3539
      2E322D32322E392C36342E342D35342E336C35302E362D33303263342E342D32
      362E332D332D35332E312D32302E342D37332E344C3534372E362C3231382E32
      7A204D3433312E372C3833302E3863302C31342E322D31312E352C32352E382D
      32352E382C32352E38682D38392E35632D31342E322C302D32352E382D31312E
      352D32352E382D32352E38762D38392E3563302D31342E322C31312E352D3235
      2E382C32352E382D32352E386838392E356331342E322C302C32352E382C3131
      2E352C32352E382C32352E38563833302E387A220A2020202020202020202020
      69643D22706174683132220A20202020202020202020207374796C653D226669
      6C6C3A233462346234623B66696C6C2D6F7061636974793A3122202F3E3C7061
      74680A2020202020202020202020643D224D3937362E312C3436394C3632392E
      372C38332E38433539362E322C34362E352C3534382C32342E362C3439372E39
      2C32352E33632D35302E322C302E312D39382E312C32312E372D3133312E352C
      35392E324C32332E372C3436392E32632D31392E392C32322E342D31372E392C
      35362E372C342E342C37362E366331302E342C392E322C32332E322C31332E37
      2C33362E312C31332E376331342E392C302C32392E382D362E312C34302E352D
      31382E326C3334322E372D3338342E366331322E392D31342E352C33312E342D
      32322E362C35302E372D32322E3663302E312C302C302E312C302C302E322C30
      6331392E332C302C33372E382C382C35302E372C32322E346C3334362E342C33
      38352E326332302C32322E332C35342E342C32342E312C37362E362C34433939
      342E332C3532352E372C3939362E312C3439312E332C3937362E312C3436397A
      220A202020202020202020202069643D22706174683134220A20202020202020
      202020207374796C653D2266696C6C3A233462346234623B66696C6C2D6F7061
      636974793A3122202F3E3C2F673E3C2F673E3C670A2020202020202069643D22
      673136220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673138220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673230220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673232220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673234220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673236220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673238220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673330220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673332220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673334220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673336220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673338220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673430220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673432220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C670A2020202020202069643D22
      673434220A202020202020207374796C653D2266696C6C3A233462346234623B
      66696C6C2D6F7061636974793A3122202F3E3C2F673E3C2F7376673E04000000
      00000000000000000C000000000000005457535647477261706869638E0E0000
      000000003C3F786D6C2076657273696F6E3D22312E302220656E636F64696E67
      3D225554462D3822207374616E64616C6F6E653D226E6F223F3E0A3C212D2D20
      53766720566563746F722049636F6E73203A20687474703A2F2F7777772E6F6E
      6C696E65776562666F6E74732E636F6D2F69636F6E202D2D3E0A0A3C7376670A
      202020786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F
      656C656D656E74732F312E312F220A202020786D6C6E733A63633D2268747470
      3A2F2F6372656174697665636F6D6D6F6E732E6F72672F6E7323220A20202078
      6D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31393939
      2F30322F32322D7264662D73796E7461782D6E7323220A202020786D6C6E733A
      7376673D22687474703A2F2F7777772E77332E6F72672F323030302F73766722
      0A202020786D6C6E733D22687474703A2F2F7777772E77332E6F72672F323030
      302F737667220A202020786D6C6E733A736F6469706F64693D22687474703A2F
      2F736F6469706F64692E736F75726365666F7267652E6E65742F4454442F736F
      6469706F64692D302E647464220A202020786D6C6E733A696E6B73636170653D
      22687474703A2F2F7777772E696E6B73636170652E6F72672F6E616D65737061
      6365732F696E6B7363617065220A20202076657273696F6E3D22312E31220A20
      2020783D22307078220A202020793D22307078220A20202076696577426F783D
      2230203020313030302031303030220A202020656E61626C652D6261636B6772
      6F756E643D226E65772030203020313030302031303030220A202020786D6C3A
      73706163653D227072657365727665220A20202069643D2273766732220A2020
      20696E6B73636170653A76657273696F6E3D22302E393120723133373235220A
      202020736F6469706F64693A646F636E616D653D22627574746F6E5F686F6D65
      5F686F76657265642E737667223E3C646566730A202020202069643D22646566
      73343822202F3E3C736F6469706F64693A6E616D6564766965770A2020202020
      70616765636F6C6F723D2223666666666666220A2020202020626F7264657263
      6F6C6F723D2223363636363636220A2020202020626F726465726F7061636974
      793D2231220A20202020206F626A656374746F6C6572616E63653D223130220A
      202020202067726964746F6C6572616E63653D223130220A2020202020677569
      6465746F6C6572616E63653D223130220A2020202020696E6B73636170653A70
      6167656F7061636974793D2230220A2020202020696E6B73636170653A706167
      65736861646F773D2232220A2020202020696E6B73636170653A77696E646F77
      2D77696474683D2231393230220A2020202020696E6B73636170653A77696E64
      6F772D6865696768743D2231303238220A202020202069643D226E616D656476
      6965773436220A202020202073686F77677269643D2266616C7365220A202020
      2020696E6B73636170653A7A6F6F6D3D22302E323336220A2020202020696E6B
      73636170653A63783D22353030220A2020202020696E6B73636170653A63793D
      22353030220A2020202020696E6B73636170653A77696E646F772D783D222D38
      220A2020202020696E6B73636170653A77696E646F772D793D222D38220A2020
      202020696E6B73636170653A77696E646F772D6D6178696D697A65643D223122
      0A2020202020696E6B73636170653A63757272656E742D6C617965723D227376
      673222202F3E3C6D657461646174610A202020202069643D226D657461646174
      6134223E2053766720566563746F722049636F6E73203A20687474703A2F2F77
      77772E6F6E6C696E65776562666F6E74732E636F6D2F69636F6E203C7264663A
      5244463E3C63633A576F726B0A20202020207264663A61626F75743D22223E3C
      64633A666F726D61743E696D6167652F7376672B786D6C3C2F64633A666F726D
      61743E3C64633A747970650A202020202020207264663A7265736F757263653D
      22687474703A2F2F7075726C2E6F72672F64632F64636D69747970652F537469
      6C6C496D61676522202F3E3C64633A7469746C653E3C2F64633A7469746C653E
      3C2F63633A576F726B3E3C2F7264663A5244463E3C2F6D657461646174613E3C
      670A202020202069643D226736220A20202020207374796C653D2266696C6C3A
      233063356530613B66696C6C2D6F7061636974793A31223E3C670A2020202020
      202069643D226738220A202020202020207374796C653D2266696C6C3A233063
      356530613B66696C6C2D6F7061636974793A31223E3C670A2020202020202020
      2069643D22673130220A2020202020202020207374796C653D2266696C6C3A23
      3063356530613B66696C6C2D6F7061636974793A31223E3C706174680A202020
      2020202020202020643D224D3534372E362C3231382E32632D31322E342D3134
      2E352D33302E362D32322E392D34392E372D32322E39632D31392E312C302D33
      372E332C382E342D34392E372C32322E394C3136392E342C353435632D31372E
      332C32302E332D32342E382C34372E322D32302E332C37332E356C35302E362C
      3330312E3963352E332C33312E352C33322E352C35342E332C36342E342C3534
      2E33683234332E32632D302E312D312E312C302E312D312E362C302E312D322E
      365637353363302D31342E322C31312E332D32352E392C32352E362D32352E39
      683131372E326331342E322C302C32352E392C31312E372C32352E392C32352E
      39763232312E386835352E386333312E392C302C35392E322D32322E392C3634
      2E342D35342E336C35302E362D33303263342E342D32362E332D332D35332E31
      2D32302E342D37332E344C3534372E362C3231382E327A204D3433312E372C38
      33302E3863302C31342E322D31312E352C32352E382D32352E382C32352E3868
      2D38392E35632D31342E322C302D32352E382D31312E352D32352E382D32352E
      38762D38392E3563302D31342E322C31312E352D32352E382C32352E382D3235
      2E386838392E356331342E322C302C32352E382C31312E352C32352E382C3235
      2E38563833302E387A220A202020202020202020202069643D22706174683132
      220A20202020202020202020207374796C653D2266696C6C3A23306335653061
      3B66696C6C2D6F7061636974793A3122202F3E3C706174680A20202020202020
      20202020643D224D3937362E312C3436394C3632392E372C38332E3843353936
      2E322C34362E352C3534382C32342E362C3439372E392C32352E33632D35302E
      322C302E312D39382E312C32312E372D3133312E352C35392E324C32332E372C
      3436392E32632D31392E392C32322E342D31372E392C35362E372C342E342C37
      362E366331302E342C392E322C32332E322C31332E372C33362E312C31332E37
      6331342E392C302C32392E382D362E312C34302E352D31382E326C3334322E37
      2D3338342E366331322E392D31342E352C33312E342D32322E362C35302E372D
      32322E3663302E312C302C302E312C302C302E322C306331392E332C302C3337
      2E382C382C35302E372C32322E346C3334362E342C3338352E326332302C3232
      2E332C35342E342C32342E312C37362E362C34433939342E332C3532352E372C
      3939362E312C3439312E332C3937362E312C3436397A220A2020202020202020
      20202069643D22706174683134220A20202020202020202020207374796C653D
      2266696C6C3A233063356530613B66696C6C2D6F7061636974793A3122202F3E
      3C2F673E3C2F673E3C670A2020202020202069643D22673136220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673138220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673230220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673232220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673234220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673236220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673238220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673330220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673332220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673334220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673336220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673338220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673430220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673432220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C670A2020202020202069643D22673434220A2020202020
      20207374796C653D2266696C6C3A233063356530613B66696C6C2D6F70616369
      74793A3122202F3E3C2F673E3C2F7376673E0400000000000000000000000C00
      0000000000005457535647477261706869638E0E0000000000003C3F786D6C20
      76657273696F6E3D22312E302220656E636F64696E673D225554462D38222073
      74616E64616C6F6E653D226E6F223F3E0A3C212D2D2053766720566563746F72
      2049636F6E73203A20687474703A2F2F7777772E6F6E6C696E65776562666F6E
      74732E636F6D2F69636F6E202D2D3E0A0A3C7376670A202020786D6C6E733A64
      633D22687474703A2F2F7075726C2E6F72672F64632F656C656D656E74732F31
      2E312F220A202020786D6C6E733A63633D22687474703A2F2F63726561746976
      65636F6D6D6F6E732E6F72672F6E7323220A202020786D6C6E733A7264663D22
      687474703A2F2F7777772E77332E6F72672F313939392F30322F32322D726466
      2D73796E7461782D6E7323220A202020786D6C6E733A7376673D22687474703A
      2F2F7777772E77332E6F72672F323030302F737667220A202020786D6C6E733D
      22687474703A2F2F7777772E77332E6F72672F323030302F737667220A202020
      786D6C6E733A736F6469706F64693D22687474703A2F2F736F6469706F64692E
      736F75726365666F7267652E6E65742F4454442F736F6469706F64692D302E64
      7464220A202020786D6C6E733A696E6B73636170653D22687474703A2F2F7777
      772E696E6B73636170652E6F72672F6E616D657370616365732F696E6B736361
      7065220A20202076657273696F6E3D22312E31220A202020783D22307078220A
      202020793D22307078220A20202076696577426F783D22302030203130303020
      31303030220A202020656E61626C652D6261636B67726F756E643D226E657720
      30203020313030302031303030220A202020786D6C3A73706163653D22707265
      7365727665220A20202069643D2273766732220A202020696E6B73636170653A
      76657273696F6E3D22302E393120723133373235220A202020736F6469706F64
      693A646F636E616D653D22627574746F6E5F686F6D655F636C69636B65642E73
      7667223E3C646566730A202020202069643D2264656673343822202F3E3C736F
      6469706F64693A6E616D6564766965770A202020202070616765636F6C6F723D
      2223666666666666220A2020202020626F72646572636F6C6F723D2223363636
      363636220A2020202020626F726465726F7061636974793D2231220A20202020
      206F626A656374746F6C6572616E63653D223130220A20202020206772696474
      6F6C6572616E63653D223130220A20202020206775696465746F6C6572616E63
      653D223130220A2020202020696E6B73636170653A706167656F706163697479
      3D2230220A2020202020696E6B73636170653A70616765736861646F773D2232
      220A2020202020696E6B73636170653A77696E646F772D77696474683D223139
      3230220A2020202020696E6B73636170653A77696E646F772D6865696768743D
      2231303238220A202020202069643D226E616D6564766965773436220A202020
      202073686F77677269643D2266616C7365220A2020202020696E6B7363617065
      3A7A6F6F6D3D22302E323336220A2020202020696E6B73636170653A63783D22
      353030220A2020202020696E6B73636170653A63793D22353030220A20202020
      20696E6B73636170653A77696E646F772D783D222D38220A2020202020696E6B
      73636170653A77696E646F772D793D222D38220A2020202020696E6B73636170
      653A77696E646F772D6D6178696D697A65643D2231220A2020202020696E6B73
      636170653A63757272656E742D6C617965723D227376673222202F3E3C6D6574
      61646174610A202020202069643D226D6574616461746134223E205376672056
      6563746F722049636F6E73203A20687474703A2F2F7777772E6F6E6C696E6577
      6562666F6E74732E636F6D2F69636F6E203C7264663A5244463E3C63633A576F
      726B0A20202020207264663A61626F75743D22223E3C64633A666F726D61743E
      696D6167652F7376672B786D6C3C2F64633A666F726D61743E3C64633A747970
      650A202020202020207264663A7265736F757263653D22687474703A2F2F7075
      726C2E6F72672F64632F64636D69747970652F5374696C6C496D61676522202F
      3E3C64633A7469746C653E3C2F64633A7469746C653E3C2F63633A576F726B3E
      3C2F7264663A5244463E3C2F6D657461646174613E3C670A202020202069643D
      226736220A20202020207374796C653D2266696C6C3A233063623030613B6669
      6C6C2D6F7061636974793A31223E3C670A2020202020202069643D226738220A
      202020202020207374796C653D2266696C6C3A233063623030613B66696C6C2D
      6F7061636974793A31223E3C670A20202020202020202069643D22673130220A
      2020202020202020207374796C653D2266696C6C3A233063623030613B66696C
      6C2D6F7061636974793A31223E3C706174680A2020202020202020202020643D
      224D3534372E362C3231382E32632D31322E342D31342E352D33302E362D3232
      2E392D34392E372D32322E39632D31392E312C302D33372E332C382E342D3439
      2E372C32322E394C3136392E342C353435632D31372E332C32302E332D32342E
      382C34372E322D32302E332C37332E356C35302E362C3330312E3963352E332C
      33312E352C33322E352C35342E332C36342E342C35342E33683234332E32632D
      302E312D312E312C302E312D312E362C302E312D322E365637353363302D3134
      2E322C31312E332D32352E392C32352E362D32352E39683131372E326331342E
      322C302C32352E392C31312E372C32352E392C32352E39763232312E38683535
      2E386333312E392C302C35392E322D32322E392C36342E342D35342E336C3530
      2E362D33303263342E342D32362E332D332D35332E312D32302E342D37332E34
      4C3534372E362C3231382E327A204D3433312E372C3833302E3863302C31342E
      322D31312E352C32352E382D32352E382C32352E38682D38392E35632D31342E
      322C302D32352E382D31312E352D32352E382D32352E38762D38392E3563302D
      31342E322C31312E352D32352E382C32352E382D32352E386838392E35633134
      2E322C302C32352E382C31312E352C32352E382C32352E38563833302E387A22
      0A202020202020202020202069643D22706174683132220A2020202020202020
      2020207374796C653D2266696C6C3A233063623030613B66696C6C2D6F706163
      6974793A3122202F3E3C706174680A2020202020202020202020643D224D3937
      362E312C3436394C3632392E372C38332E38433539362E322C34362E352C3534
      382C32342E362C3439372E392C32352E33632D35302E322C302E312D39382E31
      2C32312E372D3133312E352C35392E324C32332E372C3436392E32632D31392E
      392C32322E342D31372E392C35362E372C342E342C37362E366331302E342C39
      2E322C32332E322C31332E372C33362E312C31332E376331342E392C302C3239
      2E382D362E312C34302E352D31382E326C3334322E372D3338342E366331322E
      392D31342E352C33312E342D32322E362C35302E372D32322E3663302E312C30
      2C302E312C302C302E322C306331392E332C302C33372E382C382C35302E372C
      32322E346C3334362E342C3338352E326332302C32322E332C35342E342C3234
      2E312C37362E362C34433939342E332C3532352E372C3939362E312C3439312E
      332C3937362E312C3436397A220A202020202020202020202069643D22706174
      683134220A20202020202020202020207374796C653D2266696C6C3A23306362
      3030613B66696C6C2D6F7061636974793A3122202F3E3C2F673E3C2F673E3C67
      0A2020202020202069643D22673136220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673138220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673230220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673232220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673234220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673236220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673238220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673330220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673332220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673334220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673336220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673338220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673430220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673432220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C67
      0A2020202020202069643D22673434220A202020202020207374796C653D2266
      696C6C3A233063623030613B66696C6C2D6F7061636974793A3122202F3E3C2F
      673E3C2F7376673E040000000000000000000000}
  end
  object alActions: TActionList
    Images = ilImages
    Left = 372
    Top = 32
    object acAnimate: TAction
      Category = 'Browser'
      AutoCheck = True
      Caption = 'Animate'
      Checked = True
      OnExecute = acAnimateExecute
    end
    object acFitToView: TAction
      Category = 'Browser'
      AutoCheck = True
      Caption = 'Fit SVG to view'
      Checked = True
      OnExecute = acFitToViewExecute
    end
    object acDemoTBitBtn: TAction
      Category = 'Gallery'
      Caption = 'TBitBtn'
      ImageIndex = 1
      OnExecute = acDemoTBitBtnExecute
    end
    object acDemoTSpeedBtn: TAction
      Category = 'Gallery'
      Caption = 'TSpeedBtn'
      ImageIndex = 1
      OnExecute = acDemoTSpeedBtnExecute
    end
  end
  object pmPopup: TPopupMenu
    Images = ilImages
    Left = 420
    Top = 32
    object miGreyHouse: TMenuItem
      Caption = 'Grey house'
      ImageIndex = 1
    end
    object miDarkGreenHouse: TMenuItem
      Caption = 'Dark green house'
      ImageIndex = 2
    end
    object miLightGreenHouse: TMenuItem
      Caption = 'Light green house'
      ImageIndex = 3
    end
  end
  object tiResize: TTimer
    Interval = 20
    OnTimer = tiResizeTimer
    Left = 59
    Top = 32
  end
  object pmBrowserSettings: TPopupMenu
    Images = ilImages
    Left = 454
    Top = 32
    object miAnimate: TMenuItem
      Action = acAnimate
      AutoCheck = True
    end
    object miBrowserSettingsSep1: TMenuItem
      Caption = '-'
    end
    object miFitToView: TMenuItem
      Action = acFitToView
      AutoCheck = True
    end
    object miBrowserSettingsSep2: TMenuItem
      Caption = '-'
    end
    object miChangeBgColor: TMenuItem
      Caption = 'Change background color'
      ImageIndex = 4
    end
  end
  object aeEvents: TApplicationEvents
    OnMessage = aeEventsMessage
    Left = 499
    Top = 32
  end
end
