unit Main;

interface

// for Delphi XE3 and higher
{$IF CompilerVersion >= 24.0}
    {$LEGACYIFEND ON}
{$IFEND}

uses
    System.Classes,
    System.Math,
    System.SysUtils,
    System.Variants,
    System.Actions,
    Winapi.Windows,
    Winapi.Messages,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.ComCtrls,
    Vcl.ExtCtrls,
    Vcl.StdCtrls,
    Vcl.Buttons,
    Vcl.Menus,
    Vcl.Grids,
    Vcl.ValEdit,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.AppEvnts,
    Vcl.ActnList,
    Vcl.ImgList,
    UTWSmartPointer,
    UTWSVGGraphic,
    UTWSVGCheckBoxStyle,
    UTWSVGComponentStyle,
    UTWSVGRadioButtonStyle,
    UTWSVGImageButton,
    UTWSVGImage,
    UTWSVGImageList,
    UTWSVGAnimationDescriptor, System.ImageList;

type
    {**
     Demo main form
     @author JMR
    }
    TMainForm = class(TForm)
        published
            pcMain: TPageControl;
            tsComponentGallery: TTabSheet;
            sbGallery: TScrollBox;
            paGalleryLine3: TPanel;
            paGalleryLine3Captions: TPanel;
            paGalleryStyledCheckBox: TPanel;
            laGalleryStyledCheckBox: TLabel;
            paGalleryNormalCheckBox: TPanel;
            laGalleryNormalCheckBox: TLabel;
            paGalleryStyledRadioButton: TPanel;
            laGalleryStyledRadioButton: TLabel;
            paGalleryNormalRadioButton: TPanel;
            laGalleryNormalRadioButton: TLabel;
            paGalleryLine3Components: TPanel;
            blGalleryLine3ComponentsSeparator: TBevel;
            paGalleryLine3ComponentsStyledCB: TPanel;
            ckGalleryStyledCBDisabledGrayed: TCheckBox;
            ckGalleryStyledCBDisabledChecked: TCheckBox;
            ckGalleryStyledCBDisabled: TCheckBox;
            ckGalleryStyledCBGrayed: TCheckBox;
            ckGalleryStyledCBChecked: TCheckBox;
            ckGalleryStyledCBDefault: TCheckBox;
            paGalleryLine2ComponentsUnstyledCB: TPanel;
            ckGalleryUnstyledCBDisabledGrayed: TCheckBox;
            ckGalleryUnstyledCBDisabledChecked: TCheckBox;
            ckGalleryUnstyledCBDisabled: TCheckBox;
            ckGalleryUnstyledCBGrayed: TCheckBox;
            ckGalleryUnstyledCBChecked: TCheckBox;
            ckGalleryUnstyledCBDefault: TCheckBox;
            paGalleryStyledRB: TPanel;
            rbGalleryStyledRBDefault: TRadioButton;
            rbGalleryStyledRBChecked: TRadioButton;
            paGalleryStyledRBDisabled: TPanel;
            rbGalleryStyledRBDisabled: TRadioButton;
            rbGalleryStyledRBDisabledChecked: TRadioButton;
            paGalleryUnstyledRB: TPanel;
            rbGalleryUnstyledRBDefault: TRadioButton;
            rbGalleryUnstyledRBChecked: TRadioButton;
            paGalleryUnstyledRBDisabled: TPanel;
            rbGalleryUnstyledRBDisabled: TRadioButton;
            rbGalleryUnstyledRBDisabledChecked: TRadioButton;
            paGalleryLine3Header: TPanel;
            laGalleryLine3Title: TLabel;
            paGalleryLine3Desc: TPanel;
            paGalleryCheckBoxDesc: TPanel;
            laGalleryCheckBoxDesc: TLabel;
            paGalleryLine1: TPanel;
            paGalleryLine1Captions: TPanel;
            paGalleryTImage: TPanel;
            laGalleryTImage: TLabel;
            paGalleryTWSVGImage: TPanel;
            laGalleryTWSVGImage: TLabel;
            paGalleryTWSVGImageButton: TPanel;
            laGalleryTWSVGImageButton: TLabel;
            paGalleryLine1Components: TPanel;
            imGalleryTImage: TImage;
            imGalleryTWSVGImage: TWSVGImage;
            imGalleryTWSVGImageButton: TWSVGImageButton;
            paGalleryLine1Header: TPanel;
            laGalleryLine1Title: TLabel;
            paGalleryLine1Desc: TPanel;
            paGalleryTImageDesc: TPanel;
            laGalleryTImageDesc: TLabel;
            paGalleryTWSVGImageDesc: TPanel;
            laGalleryTWSVGImageDesc: TLabel;
            paGalleryTWSVGImageButtonDesc: TPanel;
            laGalleryTWSVGImageButtonDesc: TLabel;
            paGalleryLine1Resizeable: TPanel;
            paGalleryLine1ResizeableLeft: TPanel;
            paGalleryLine1ResizeableImageControls: TPanel;
            tbGalleryLine1ResizeableImage: TTrackBar;
            paGalleryLine1ResizeableImageView: TPanel;
            imGalleryLine1ResizeableImage: TImage;
            paGalleryLine1ResizeableRight: TPanel;
            imGalleryLine1StepByStepImage: TWSVGImage;
            tbGalleryLine1StepByStepImage: TTrackBar;
            paGalleryLine1ResizeableCaptions: TPanel;
            laGalleryStepByStepDesc: TLabel;
            paGalleryLine1ResizeableComponentsCaptions: TPanel;
            laGalleryResizeableDesc: TLabel;
            paGalleryLine2: TPanel;
            paGalleryLine2ComponentsTitle: TPanel;
            paGalleryTButton: TPanel;
            laGalleryTButton: TLabel;
            paGalleryTBitBtn: TPanel;
            laGalleryTBitBtn: TLabel;
            paGalleryTSpeedBtn: TPanel;
            laGalleryTSpeedBtn: TLabel;
            paGalleryTPopupMenu: TPanel;
            laGalleryTPopupMenu: TLabel;
            paGalleryLine2Components: TPanel;
            btGalleryTSpeedBtn: TSpeedButton;
            btGalleryTButton: TButton;
            btGalleryTBitBtn: TBitBtn;
            btGalleryPopupMenuBtn: TButton;
            paGalleryLine2Header: TPanel;
            laGalleryLine2Title: TLabel;
            paGalleryLine2ComponentsDesc: TPanel;
            paGalleryTButtonDesc: TPanel;
            laGalleryTButtonDesc: TLabel;
            paGalleryTBitBtnDesc: TPanel;
            laGalleryTBitBtnDesc: TLabel;
            paGalleryTSpeedBtnDesc: TPanel;
            laGalleryTSpeedBtnDesc: TLabel;
            paGalleryTPopupMenuDesc: TPanel;
            laGalleryTPopupMenuDesc: TLabel;
            tsBrowser: TTabSheet;
            spMainVert: TSplitter;
            paExplorer: TPanel;
            lbDir: TListBox;
            paExplorerBrowser: TPanel;
            paExplorerBrowserTop: TPanel;
            btBrowse: TButton;
            btNext: TButton;
            btPrev: TButton;
            btSlideshow: TButton;
            tbSlideshowTimer: TTrackBar;
            paExplorerBrowserBottom: TPanel;
            btChangeBgColor: TButton;
            paViewer: TPanel;
            imViewer: TWSVGImage;
            paViewerControls: TPanel;
            paViewerControlsTop: TPanel;
            ckAnimate: TCheckBox;
            paBrowserAnimSpeed: TPanel;
            laBrowserAnimSpeed: TLabel;
            edBrowserAnimSpeed: TEdit;
            udBrowserAnimSpeed: TUpDown;
            paViewerControlsBottom: TPanel;
            ckFitToView: TCheckBox;
            paBrowserZoom: TPanel;
            laBrowserZoom: TLabel;
            edBrowserZoom: TEdit;
            udBrowserZoom: TUpDown;
            tsBanking: TTabSheet;
            sbBanking: TScrollBox;
            blBankingHeaderSeparator: TBevel;
            blBankingSummarySeparator: TBevel;
            blBankingPaymentSeparator: TBevel;
            blBankingAboutSeparator: TBevel;
            blBankingPersonalInfoSeparator: TBevel;
            paBankingHeader: TPanel;
            shBankingHeaderBackground: TShape;
            imBankingHeaderClipart: TWSVGImage;
            laBankingHeaderTitle: TLabel;
            laBankingHeaderDesc: TLabel;
            paBankingSummary: TPanel;
            laBankingSummaryTitle: TLabel;
            laBankingSummaryReview: TLabel;
            veBankingSummary: TValueListEditor;
            paBankingBillingInfo: TPanel;
            laBankingBillingInfoTitle: TLabel;
            laBankingBillingInfoDesc: TLabel;
            paBankingBillingPaymentMethods: TPanel;
            laBankingPaymentMethod: TLabel;
            imBankingPaymentMastercard: TWSVGImage;
            imBankingPaymentVisa: TWSVGImage;
            imBankingPaymentPayPal: TWSVGImage;
            imBankingPaymentAmEx: TWSVGImage;
            imBankingPaymentDiners: TWSVGImage;
            imBankingPaymentAsterisk: TWSVGImage;
            rbBankingPaymentMethodCreditCard: TRadioButton;
            rbBankingPaymentMethodPayPal: TRadioButton;
            paBankingPersonalInfo: TPanel;
            laBankingPersonalInfoTitle: TLabel;
            laBankingPersonalInfoDesc: TLabel;
            paBankingPersonalInfoClientName: TPanel;
            paBankingPersonalInfoFirstName: TPanel;
            edBankingPersonalInfoFirstName: TEdit;
            paBankingPersonalInfoFirstNameTitle: TPanel;
            laBankingPersonalInfoFirstName: TLabel;
            imBankingPersonalInfoFirstName: TWSVGImage;
            paBankingPersonalInfoLastName: TPanel;
            edBankingPersonalInfoLastName: TEdit;
            paBankingPersonalInfoLastNameTitla: TPanel;
            laBankingPersonalInfoLastName: TLabel;
            imBankingPersonalInfoLastName: TWSVGImage;
            paBankingPersonalInfoGender: TPanel;
            laBankingPersonalInfoGenderTitle: TLabel;
            rbBankingPersonalInfoGenderMale: TRadioButton;
            rbBankingPersonalInfoGenderFemale: TRadioButton;
            paBankingPersonalInfoCreditCard: TPanel;
            paBankingPersonalInfoCreditCardInfo: TPanel;
            edBankingPersonalInfoCreditCard: TEdit;
            paBankingPersonalInfoCreditCardTitle: TPanel;
            laBankingPersonalInfoCreditCardTitle: TLabel;
            imBankingPersonalInfoCreditCardTitleImage: TWSVGImage;
            paBankingPersonalInfoCreditCardData: TPanel;
            paBankingPersonalInfoCreditCardSecurity: TPanel;
            paBankingPersonalInfoCreditCardSecurityTitle: TPanel;
            laBankingPersonalInfoCreditCardSecurity: TLabel;
            imBankingPersonalInfoCreditCardSecurityTitle: TWSVGImage;
            paBankingPersonalInfoCreditCardSecurityInfo: TPanel;
            imBankingPersonalInfoCreditCardSecurity: TWSVGImage;
            edBankingPersonalInfoCreditCardSecurity: TEdit;
            paBankingPersonalInfoCreditCardExpires: TPanel;
            paBankingPersonalInfoCreditCardExpiresTitle: TPanel;
            laBankingPersonalInfoCreditCardExpires: TLabel;
            imBankingPersonalInfoCreditCardExpires: TWSVGImage;
            paBankingPersonalInfoCreditCardExpiresInfo: TPanel;
            dpBankingPersonalInfoCreditCardExpiresInfoDay: TDateTimePicker;
            dpBankingPersonalInfoCreditCardExpiresInfoMonth: TDateTimePicker;
            dpBankingPersonalInfoCreditCardExpiresInfoYear: TDateTimePicker;
            paBankingAboutProduct: TPanel;
            laBankingAboutProductTitle: TLabel;
            laBankingAboutProductDesc: TLabel;
            paBankingAboutProductRight: TPanel;
            ckBankingAboutProductFromAdv: TCheckBox;
            ckBankingAboutProductOther: TCheckBox;
            ckBankingAboutProductPrivate: TCheckBox;
            paBankingAboutProductLeft: TPanel;
            ckBankingAboutProductProfessional: TCheckBox;
            ckBankingAboutProductAsAGift: TCheckBox;
            ckBankingAboutProductByFriends: TCheckBox;
            ckBankingAboutProductOnTheInternet: TCheckBox;
            ckBankingAboutProductInAShop: TCheckBox;
            paBankingBuy: TPanel;
            imBankingBuyNowButton: TWSVGImageButton;
            tsFreshBooks: TTabSheet;
            sbFreshBooks: TScrollBox;
            imFreshBooksHeaderShadow: TImage;
            paFreshBooksHeader: TPanel;
            shFreshBooksHeaderBackground: TShape;
            paFreshBooksHeaderYourCreditCard: TPanel;
            laFreshBooksHeaderYourCreditCard: TLabel;
            imFreshBooksHeaderYourCreditCard: TImage;
            paFreshBooksHeaderLine1: TPanel;
            imFreshBooksHeaderCreditCardSecurityCode: TImage;
            edFreshBooksHeaderCreditCard: TEdit;
            edFreshBooksHeaderCreditCardSecurityCode: TEdit;
            paFreshBooksHeaderLine1Captions: TPanel;
            laFreshBooksHeaderCreditCardCaption: TLabel;
            laFreshBooksHeaderSecurityCodeCaption: TLabel;
            paFreshBooksHeaderLine2: TPanel;
            laFreshBooksHeaderSlash: TLabel;
            edFreshBooksHeaderName: TEdit;
            dpFreshBooksHeaderExpDateYear: TDateTimePicker;
            dpFreshBooksHeaderExpDateMonth: TDateTimePicker;
            paFreshBooksHeaderLine2Captions: TPanel;
            laFreshBooksHeaderFullName: TLabel;
            laFreshBooksHeaderExpDate: TLabel;
            paFreshBooksHeaderLine3: TPanel;
            imFreshBooksHeaderVisa: TImage;
            imFreshBooksHeaderMasterCard: TImage;
            paFreshBookAddress: TPanel;
            paFreshBookAddressLine2: TPanel;
            edFreshBookAddress: TEdit;
            paFreshBookAddressLine1: TPanel;
            imFreshBookAddressTitle: TWSVGImage;
            laFreshBookAddressCaption: TLabel;
            paFreshBookAddressLine2Captions: TPanel;
            laFreshBooksStreetAddressCaption: TLabel;
            paFreshBookAddressLine3: TPanel;
            edFreshBookCity: TEdit;
            edFreshBooksCountry: TEdit;
            paFreshBookAddressLine3Captions: TPanel;
            laFreshBookCityCaption: TLabel;
            laFreshBooksCountryCaption: TLabel;
            paFreshBooksPaymentOptions: TPanel;
            paFreshBooksPaymentOptionsRight: TPanel;
            ckFreshBooksPaymentOptionsOther: TCheckBox;
            ckFreshBooksPaymentOptionsGift: TCheckBox;
            paFreshBooksPaymentOptionsHeader: TPanel;
            imFreshBooksPaymentOptionsHeader: TWSVGImage;
            laFreshBooksPaymentOptionsHeader: TLabel;
            paFreshBooksPaymentOptionsLeft: TPanel;
            ckFreshBooksPaymentOptionsForAFriend: TCheckBox;
            ckFreshBooksPaymentOptionsForMe: TCheckBox;
            paFreshBooksPayNow: TPanel;
            paFreshBooksPayNowLine1: TPanel;
            imFreshBooksPayNow: TWSVGImageButton;
            paFreshBooksPayNowLine1Captions: TPanel;
            laFreshBooksPayNowCaption: TLabel;
            imFreshBooksPayNowCaption: TImage;
            laFreshBooksPayNowLink: TLabel;
            paNavigation: TPanel;
            ibBankingForm: TWSVGImageButton;
            ibBrowserForm: TWSVGImageButton;
            ibFreshBooksForm: TWSVGImageButton;
            ibGalleryForm: TWSVGImageButton;
            rsBankingStyle: TWSVGRadioButtonStyle;
            csBankingStyle: TWSVGCheckBoxStyle;
            odOpen: TOpenDialog;
            cdBgColor: TColorDialog;
            tiSlideshow: TTimer;
            ilImages: TWSVGImageList;
            alActions: TActionList;
            acAnimate: TAction;
            acFitToView: TAction;
            acDemoTBitBtn: TAction;
            acDemoTSpeedBtn: TAction;
            pmPopup: TPopupMenu;
            miGreyHouse: TMenuItem;
            miDarkGreenHouse: TMenuItem;
            miLightGreenHouse: TMenuItem;
            tiResize: TTimer;
            pmBrowserSettings: TPopupMenu;
            miAnimate: TMenuItem;
            miBrowserSettingsSep1: TMenuItem;
            miFitToView: TMenuItem;
            miBrowserSettingsSep2: TMenuItem;
            miChangeBgColor: TMenuItem;
            aeEvents: TApplicationEvents;

            procedure FormResize(pSender: TObject);
            procedure pcMainChange(pSender: TObject);
            procedure ibGalleryFormClick(pSender: TObject);
            procedure ibBrowserFormClick(pSender: TObject);
            procedure ibBankingFormClick(pSender: TObject);
            procedure ibFreshBooksFormClick(pSender: TObject);
            procedure lbDirClick(pSender: TObject);
            procedure btGalleryPopupMenuBtnClick(pSender: TObject);
            procedure btPrevClick(pSender: TObject);
            procedure btNextClick(pSender: TObject);
            procedure btSlideshowClick(pSender: TObject);
            procedure btBrowseClick(pSender: TObject);
            procedure btChangeBgColorClick(pSender: TObject);
            procedure edBrowserAnimSpeedChange(pSender: TObject);
            procedure edBrowserZoomChange(pSender: TObject);
            procedure tbGalleryLine1ResizeableImageChange(pSender: TObject);
            procedure tbGalleryLine1StepByStepImageChange(pSender: TObject);
            procedure tbSlideshowTimerChange(pSender: TObject);
            procedure tiSlideshowTimer(pSender: TObject);
            procedure tiResizeTimer(pSender: TObject);
            procedure spMainVertMoved(pSender: TObject);
            function imGalleryLine1StepByStepImageAnimate(pSender: TObject;
                    pAnimDesc: TWSVGAnimationDescriptor; pCustomData: Pointer): Boolean;
            procedure imBankingBuyNowButtonClick(pSender: TObject);
            procedure imFreshBooksPayNowClick(pSender: TObject);
            procedure acDemoTBitBtnExecute(pSender: TObject);
            procedure acDemoTSpeedBtnExecute(pSender: TObject);
            procedure acAnimateExecute(pSender: TObject);
            procedure acFitToViewExecute(pSender: TObject);
            procedure aeEventsMessage(var msg: tagMSG; var handled: Boolean);

        private
            m_CurrentDir:                       UnicodeString;
            m_CurrentSelection:                 Integer;
            m_ResizeDirection:                  Integer;
            m_ResizeableTrackbarWndProc_Backup: TWndMethod;
            m_StepByStepTrackbarWndProc_Backup: TWndMethod;

            {**
             Open the browser demo page
            }
            procedure OpenBrowserPage;

            {**
             Get the default image directory
             @returns(Default image directory)
            }
            function Browser_GetDefaultImgDir: UnicodeString;

            {**
             Clear the browser view
            }
            procedure Browser_ClearView;

            {**
             Open a SVG file and show it on the view
             @param(fileName SVG file name to open)
            }
            procedure Browser_OpenSVG(fileName: UnicodeString);

            {**
             Called when browser content should be resized
            }
            procedure Browser_OnResize;

            {**
             Called when animation is running
             @param(pSender Event sender)
             @param(pAnimDesc Animation description)
             @param(pCustomData Custom data)
             @returns(@true if animation can continue, otherwise @false)
            }
            function Browser_OnAnimate(pSender: TObject; pAnimDesc: TWSVGAnimationDescriptor;
                    pCustomData: Pointer): Boolean;

            {**
             Make a control with a rounded background
             @param(pControl Control to make rounded)
            }
            procedure MakeRounded(pControl: TWinControl);

        protected
            {**
             Called after DPI changed because app was moved on another monitor
             @param(pSender Event sender)
             @param(oldDPI Previous DPI value)
             @param(newDPI New DPI value)
            }
            procedure OnAfterMonitorDpiChangedHandler(pSender: TObject; oldDPI, newDPI: Integer); virtual;

            {**
             Resizeable image trackbar hooked Windows procedure
             @param(message Windows message)
            }
            procedure ResizeableTrackbarWndProc(var message: TMessage);

            {**
             Step-by-step animated image trackbar hooked Windows procedure
             @param(message Windows message)
            }
            procedure StepByStepTrackbarWndProc(var message: TMessage);

        public
            {**
             Constructor
             @param(pOwner Form owner)
            }
            constructor Create(pOwner: TComponent); override;

            {**
             Destructor
            }
            destructor Destroy; override;
    end;

var
    MainForm: TMainForm;

implementation
//---------------------------------------------------------------------------
{$R *.dfm}
//---------------------------------------------------------------------------
constructor TMainForm.Create(pOwner: TComponent);
var
    count, i: Integer;
begin
    inherited Create(pOwner);

    // this property was removed since 10.3 Rio compilers, for earlier version set it dynamically
    {$if CompilerVersion < 33}
        ilImages.ColorDepth := cd32Bit;
    {$ifend}

    m_CurrentSelection                 := -1;
    m_ResizeDirection                  := -10;
    m_ResizeableTrackbarWndProc_Backup :=  nil;
    m_StepByStepTrackbarWndProc_Backup :=  nil;

    {$ifndef _DEBUG}
        count := pcMain.PageCount;

        // hide page control tabs
        for i := 0 to count - 1 do
            pcMain.Pages[i].TabVisible := False;
    {$endif}

    // hook the resizeable trackbar windows procedure
    m_ResizeableTrackbarWndProc_Backup       := tbGalleryLine1ResizeableImage.WindowProc;
    tbGalleryLine1ResizeableImage.WindowProc := ResizeableTrackbarWndProc;

    // hook the step-by-step trackbar windows procedure
    m_StepByStepTrackbarWndProc_Backup       := tbGalleryLine1StepByStepImage.WindowProc;
    tbGalleryLine1StepByStepImage.WindowProc := StepByStepTrackbarWndProc;

    // be notified if the per-monitor DPI changes
    {$if CompilerVersion >= 30}
        OnAfterMonitorDpiChanged := OnAfterMonitorDpiChangedHandler;
    {$ifend}

    ibGalleryFormClick(nil);
end;
//---------------------------------------------------------------------------
destructor TMainForm.Destroy;
begin
    if (Assigned(m_ResizeableTrackbarWndProc_Backup)) then
        tbGalleryLine1ResizeableImage.WindowProc := m_ResizeableTrackbarWndProc_Backup;

    if (Assigned(m_StepByStepTrackbarWndProc_Backup)) then
        tbGalleryLine1StepByStepImage.WindowProc := m_StepByStepTrackbarWndProc_Backup;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TMainForm.FormResize(pSender: TObject);
begin
    Browser_OnResize();
end;
//---------------------------------------------------------------------------
procedure TMainForm.OnAfterMonitorDpiChangedHandler(pSender: TObject; oldDPI, newDPI: Integer);
begin
    // TBitBtn doesn't refresh its glyph automatically, so force it to refresh
    acDemoTBitBtn.ImageIndex := -1;
    acDemoTBitBtn.ImageIndex :=  1;

    // TSpeedBtn doesn't refresh its glyph automatically, so force it to refresh
    acDemoTSpeedBtn.ImageIndex := -1;
    acDemoTSpeedBtn.ImageIndex :=  1;
end;
//---------------------------------------------------------------------------
procedure TMainForm.pcMainChange(pSender: TObject);
begin
    // search for active page
    if (pcMain.ActivePage = tsBrowser) then
        OpenBrowserPage();
end;
//---------------------------------------------------------------------------
procedure TMainForm.ibGalleryFormClick(pSender: TObject);
begin
    pcMain.ActivePage := tsComponentGallery;

    // the TPageControl will not call it so force the OnChange event manually
    pcMainChange(pcMain);
end;
//---------------------------------------------------------------------------
procedure TMainForm.ibBrowserFormClick(pSender: TObject);
begin
    pcMain.ActivePage := tsBrowser;

    // the TPageControl will not call it so force the OnChange event manually
    pcMainChange(pcMain);
end;
//---------------------------------------------------------------------------
procedure TMainForm.ibBankingFormClick(pSender: TObject);
begin
    pcMain.ActivePage := tsBanking;

    // the TPageControl will not call it so force the OnChange event manually
    pcMainChange(pcMain);
end;
//---------------------------------------------------------------------------
procedure TMainForm.ibFreshBooksFormClick(pSender: TObject);
begin
    pcMain.ActivePage := tsFreshBooks;

    // the TPageControl will not call it so force the OnChange event manually
    pcMainChange(pcMain);
end;
//---------------------------------------------------------------------------
procedure TMainForm.lbDirClick(pSender: TObject);
begin
    // is file list empty?
    if (lbDir.Count = 0) then
        Exit;

    // is file list selection valid?
    if ((lbDir.ItemIndex < 0) or (lbDir.ItemIndex >= lbDir.Count)) then
        Exit;

    // nothing changed since last selection?
    if (lbDir.ItemIndex = m_CurrentSelection) then
        Exit;

    // open SVG from selection
    Browser_OpenSVG(m_CurrentDir + lbDir.Items.Strings[lbDir.ItemIndex]);
end;
//---------------------------------------------------------------------------
procedure TMainForm.btGalleryPopupMenuBtnClick(pSender: TObject);
var
    mousePos: TPoint;
begin
    mousePos := Mouse.CursorPos;
    pmPopup.Popup(mousePos.X, mousePos.Y);
end;
//---------------------------------------------------------------------------
procedure TMainForm.btPrevClick(pSender: TObject);
begin
    // is file list empty?
    if (lbDir.Count = 0) then
        Exit;

    // calculate prev valid index to show
    if (lbDir.ItemIndex - 1 < 0) then
        lbDir.ItemIndex := lbDir.Count - 1
    else
        lbDir.ItemIndex := lbDir.ItemIndex - 1;

    // open SVG from selection
    Browser_OpenSVG(m_CurrentDir + lbDir.Items.Strings[lbDir.ItemIndex]);
end;
//---------------------------------------------------------------------------
procedure TMainForm.btNextClick(pSender: TObject);
begin
    // is file list empty?
    if (lbDir.Count = 0) then
        Exit;

    // calculate next valid index to show
    if (lbDir.ItemIndex + 1 >= lbDir.Count) then
        lbDir.ItemIndex := 0
    else
        lbDir.ItemIndex := lbDir.ItemIndex + 1;

    // open SVG from selection
    Browser_OpenSVG(m_CurrentDir + lbDir.Items.Strings[lbDir.ItemIndex]);
end;
//---------------------------------------------------------------------------
procedure TMainForm.btSlideshowClick(pSender: TObject);
begin
    // slideshow enabled?
    tiSlideshow.Enabled      := not tiSlideshow.Enabled;
    tbSlideshowTimer.Visible :=     tiSlideshow.Enabled;

    if (ilImages.Count > 0) then
        if (tiSlideshow.Enabled) then
            btSlideshow.ImageIndex := 0
        else
            btSlideshow.ImageIndex := -1;
end;
//---------------------------------------------------------------------------
procedure TMainForm.btBrowseClick(pSender: TObject);
begin
    // let the user select the file to show
    if (not odOpen.Execute) then
        Exit;

    Browser_OpenSVG(odOpen.FileName);
end;
//---------------------------------------------------------------------------
procedure TMainForm.btChangeBgColorClick(pSender: TObject);
begin
    cdBgColor.Color := paViewer.Color;

    if (not cdBgColor.Execute(Handle)) then
        Exit;

    paViewer.Color := cdBgColor.Color;
end;
//---------------------------------------------------------------------------
procedure TMainForm.edBrowserAnimSpeedChange(pSender: TObject);
var
    frameCount: Integer;
begin
    // don't try to convert empty text
    if (Length(edBrowserAnimSpeed.Text) = 0) then
        Exit;

    // ignore too long texts
    if (Length(edBrowserAnimSpeed.Text) > 5) then
        Exit;

    // get the new FPS value to apply
    frameCount := Min(Max(StrToInt(StringReplace(edBrowserAnimSpeed.Text, #27, '',
            [rfReplaceAll, rfIgnoreCase])), udBrowserAnimSpeed.Min), udBrowserAnimSpeed.Max);

    // apply new frame count
    imViewer.Animation.FrameCount := frameCount;
end;
//---------------------------------------------------------------------------
procedure TMainForm.edBrowserZoomChange(pSender: TObject);
var
    pSVGGraphic:                  TWSVGGraphic;
    svgSize:                      TSize;
    zoomFactor:                   Integer;
    zoomFactorF, widthF, heightF: Single;
begin
    if (acFitToView.Checked) then
        Exit;

    // graphic must be of SVG type
    if (not(imViewer.Picture.Graphic is TWSVGGraphic)) then
        Exit;

    // get the SVG graphic
    pSVGGraphic := imViewer.Picture.Graphic as TWSVGGraphic;

    if (not Assigned(pSVGGraphic)) then
        Exit;

    // get the original SVG size
    svgSize := pSVGGraphic.Rasterizer.GetSize(pSVGGraphic.Native);

    // don't try to convert empty text
    if (Length(edBrowserZoom.Text) = 0) then
    begin
        // restore original size
        pSVGGraphic.Width  := svgSize.Width;
        pSVGGraphic.Height := svgSize.Height;
        Exit;
    end;

    // ignore too long texts
    if (Length(edBrowserZoom.Text) > 5) then
    begin
        // restore original size
        pSVGGraphic.Width  := svgSize.Width;
        pSVGGraphic.Height := svgSize.Height;
        Exit;
    end;

    // get the new zoom fator to apply
    zoomFactor := Min(Max(StrToInt(StringReplace(edBrowserZoom.Text, #27, '',
            [rfReplaceAll, rfIgnoreCase])), udBrowserZoom.Min), udBrowserZoom.Max);

    zoomFactorF := zoomFactor;
    widthF      := svgSize.Width;
    heightF     := svgSize.Height;

    // apply the size with the zoom factor
    pSVGGraphic.Width  := Round(widthF  * (zoomFactorF / 100.0));
    pSVGGraphic.Height := Round(heightF * (zoomFactorF / 100.0));
end;
//---------------------------------------------------------------------------
procedure TMainForm.tbGalleryLine1ResizeableImageChange(pSender: TObject);
var
    position: Single;
begin
    position := tbGalleryLine1ResizeableImage.Position;

    // update the image size
    imGalleryLine1ResizeableImage.Left   := 125 - Round(position / 2.3);
    imGalleryLine1ResizeableImage.Top    := 125 - Round(position / 3.0);
    imGalleryLine1ResizeableImage.Width  := tbGalleryLine1ResizeableImage.Position;
    imGalleryLine1ResizeableImage.Height := tbGalleryLine1ResizeableImage.Position;
end;
//---------------------------------------------------------------------------
procedure TMainForm.tbGalleryLine1StepByStepImageChange(pSender: TObject);
begin
    // update the image frame number
    imGalleryLine1StepByStepImage.Animation.Position := tbGalleryLine1StepByStepImage.Position;
end;
//---------------------------------------------------------------------------
procedure TMainForm.tbSlideshowTimerChange(pSender: TObject);
begin
    tiSlideshow.Interval := tbSlideshowTimer.Position;
end;
//---------------------------------------------------------------------------
procedure TMainForm.tiSlideshowTimer(pSender: TObject);
begin
    // the currently visible page isn't the browser page?
    if (pcMain.ActivePage <> tsBrowser) then
        Exit;

    btNextClick(btNext);
end;
//---------------------------------------------------------------------------
procedure TMainForm.tiResizeTimer(pSender: TObject);
begin
    // the currently visible page isn't the demo page?
    if (pcMain.ActivePage <> tsComponentGallery) then
        Exit;

    // update the trackbar position
    tbGalleryLine1ResizeableImage.Position := tbGalleryLine1ResizeableImage.Position + m_ResizeDirection;

    // check if direction should be inverted
    if (imGalleryLine1ResizeableImage.Height <= tbGalleryLine1ResizeableImage.Min) then
    begin
        imGalleryLine1ResizeableImage.Width  :=  tbGalleryLine1ResizeableImage.Min;
        imGalleryLine1ResizeableImage.Height :=  tbGalleryLine1ResizeableImage.Min;
        m_ResizeDirection                    := -m_ResizeDirection;
    end
    else
    if (imGalleryLine1ResizeableImage.Height >= tbGalleryLine1ResizeableImage.Max) then
    begin
        imGalleryLine1ResizeableImage.Width  :=  tbGalleryLine1ResizeableImage.Max;
        imGalleryLine1ResizeableImage.Height :=  tbGalleryLine1ResizeableImage.Max;
        m_ResizeDirection                    := -m_ResizeDirection;
    end;
end;
//---------------------------------------------------------------------------
procedure TMainForm.spMainVertMoved(pSender: TObject);
begin
    Browser_OnResize();
end;
//---------------------------------------------------------------------------
function TMainForm.imGalleryLine1StepByStepImageAnimate(pSender: TObject;
        pAnimDesc: TWSVGAnimationDescriptor; pCustomData: Pointer): Boolean;
begin
    tbGalleryLine1StepByStepImage.Position := imGalleryLine1StepByStepImage.Animation.Position;
    Result                                 := True;
end;
//---------------------------------------------------------------------------
procedure TMainForm.imBankingBuyNowButtonClick(pSender: TObject);
begin
    imBankingBuyNowButton.Enabled := False;
end;
//---------------------------------------------------------------------------
procedure TMainForm.imFreshBooksPayNowClick(pSender: TObject);
begin
    imFreshBooksPayNow.Enabled := false;
end;
//---------------------------------------------------------------------------
procedure TMainForm.acDemoTBitBtnExecute(pSender: TObject);
begin
    // dummy stupid code, otherwise RAD Studio will disable the action
end;
//---------------------------------------------------------------------------
procedure TMainForm.acDemoTSpeedBtnExecute(pSender: TObject);
begin
    // dummy stupid code, otherwise RAD Studio will disable the action
end;
//---------------------------------------------------------------------------
procedure TMainForm.acAnimateExecute(pSender: TObject);
begin
    imViewer.Animation.Animate := acAnimate.Checked;
    edBrowserAnimSpeed.Enabled := acAnimate.Checked;
    udBrowserAnimSpeed.Enabled := acAnimate.Checked;
end;
//---------------------------------------------------------------------------
procedure TMainForm.acFitToViewExecute(pSender: TObject);
begin
    edBrowserZoom.Enabled := not acFitToView.Checked;
    udBrowserZoom.Enabled := not acFitToView.Checked;

    if (acFitToView.Checked) then
    begin
        // set SVG size to fit the view
        imViewer.Picture.Graphic.Width  := imViewer.Width;
        imViewer.Picture.Graphic.Height := imViewer.Height;
    end
    else
        // apply the zoom factor
        edBrowserZoomChange(nil);
end;
//---------------------------------------------------------------------------
procedure TMainForm.aeEventsMessage(var msg: tagMSG; var handled: Boolean);
var
    delta:                        SmallInt;
    distance, i:                  Integer;
    lineDirection, pageDirection: WORD;
    mousePos:                     TPoint;
    pTarget:                      TControl;
    pWinControl:                  TWinControl;
    info:                         tagWINDOWINFO;
begin
    // dispatch messages
    case msg.message of
        WM_MOUSEWHEEL:
        begin
            // search for currently visible page
            if (pcMain.ActivePage = tsBrowser) then
            begin
                // do nothing if the browser zoom isn't accessible
                if (not edBrowserZoom.Enabled) then
                    Exit;

                // get zoom direction (positive up, negative down)
                delta := SmallInt(HIWORD(msg.wParam));

                if (delta = 0) then
                    Exit;

                // compute zoom distance
                distance := delta div WHEEL_DELTA;

                // apply the new zoom value
                udBrowserZoom.Position := udBrowserZoom.Position + (distance * 5);

                handled := True;
                Exit;
            end
            else
            begin
                delta := SmallInt(HIWORD(msg.wParam));

                if (delta = 0) then
                    Exit;

                // get scroll direction (positive up, negative down)
                if (delta > 0) then
                begin
                    lineDirection := SB_LINEUP;
                    pageDirection := SB_PAGEUP;
                end
                else
                begin
                    lineDirection := SB_LINEDOWN;
                    pageDirection := SB_PAGEDOWN;
                end;

                mousePos := TPoint.Create(msg.pt.x, msg.pt.y);

                // find control under cursor
                pTarget := FindDragTarget(mousePos, True);

                if (not Assigned(pTarget)) then
                    Exit;

                if (not(pTarget is TWinControl)) then
                    pWinControl := pTarget.Parent
                else
                    // check first scrolling window parent and scroll it
                    pWinControl := pTarget as TWinControl;

                FillChar(info, SizeOf(tagWindowInfo), $00);
                info.cbSize := SizeOf(info);

                // find first window control in child's parent hieararchy with scrollbars and scroll it
                while (Assigned(pWinControl)) do
                begin
                    // poll window info for vertical scrollbar
                    if (not GetWindowInfo(pWinControl.Handle, info)) then
                    begin
                        pWinControl := pWinControl.Parent;
                        continue;
                    end;

                    if ((info.dwStyle and WS_VSCROLL) = 0) then
                    begin
                        pWinControl := pWinControl.Parent;
                        continue;
                    end;

                    // has vertical scrollbars, scroll window
                    if (delta < 0) then
                        delta := delta * -1;

                    // compute scroll distance
                    distance := delta div WHEEL_DELTA;

                    // if distance < 5 send distance time SB_LINEUP/DOWN messages otherwise one SB_PAGEUP/DOWN
                    if (distance < 5) then
                    begin
                        // mutiply actual distance for better effect (more scrolling)
                        distance := distance * 5;

                        for i := 0 to distance - 1 do
                            SendMessage(pWinControl.Handle, WM_VSCROLL, MAKEWPARAM(lineDirection, 0), 0);
                    end
                    else
                        SendMessage(pWinControl.Handle, WM_VSCROLL, MAKEWPARAM(pageDirection, 0), 0);

                    handled := True;
                    Exit;
                end;
            end;
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TMainForm.OpenBrowserPage;
var
    fileRec: TSearchRec;
begin
    Browser_ClearView();

    m_CurrentDir := Browser_GetDefaultImgDir();

    try
        lbDir.Items.BeginUpdate;
        lbDir.Items.Clear;

        // also list the other SVG in the dir to facilitate the browsing
        if (FindFirst(m_CurrentDir + '*.svg', faNormal, fileRec) = 0) then
        begin
            repeat
                // add SVG file name to list
                lbDir.Items.Add(fileRec.Name);
            until (FindNext(fileRec) <> 0);

            System.SysUtils.FindClose(fileRec);
        end;
    finally
        lbDir.Items.EndUpdate;
        lbDir.Invalidate;
    end;

    // get the default image directory
    odOpen.InitialDir := m_CurrentDir;

    // configure the viewer events
    imViewer.OnAnimate := Browser_OnAnimate;

    // configure the slideshow timer position
    tbSlideshowTimer.Position := tiSlideshow.Interval;

    // open the first image if available
    if (lbDir.Count > 0) then
    begin
        lbDir.ItemIndex := 0;

        // open SVG from selection
        Browser_OpenSVG(m_CurrentDir + lbDir.Items.Strings[lbDir.ItemIndex]);
    end;
end;
//---------------------------------------------------------------------------
function TMainForm.Browser_GetDefaultImgDir: UnicodeString;
var
    imgDir: UnicodeString;
begin
    // get the image directory. Start from current application dir
    imgDir := IncludeTrailingPathDelimiter(GetCurrentDir);

    // check if image dir is closer to application location
    if (DirectoryExists(imgDir + 'Images')) then
        imgDir := imgDir + 'Images'
    else
    begin
        // image dir isn't next to app location, probably app is running from compiler. So browse to
        // image directory. From the current app path, it's should be located at: ".\\..\\..\\Images"
        imgDir := ExtractFilePath(ExcludeTrailingPathDelimiter(imgDir));
        imgDir := ExtractFilePath(ExcludeTrailingPathDelimiter(imgDir));
        imgDir := ExtractFilePath(ExcludeTrailingPathDelimiter(imgDir));
        imgDir := imgDir + 'Images';
    end;

    // check if dir exists. If not, use the current application dir
    if (DirectoryExists(imgDir)) then
        Exit (IncludeTrailingPathDelimiter(imgDir));

    Result := IncludeTrailingPathDelimiter(GetCurrentDir);
end;
//---------------------------------------------------------------------------
procedure TMainForm.Browser_ClearView;
begin
    // clear previous view
    acAnimate.Enabled          := False;
    edBrowserAnimSpeed.Enabled := False;
    udBrowserAnimSpeed.Enabled := False;
    edBrowserAnimSpeed.Text    := '0';
    imViewer.Picture.Assign(nil);
end;
//---------------------------------------------------------------------------
procedure TMainForm.Browser_OpenSVG(fileName: UnicodeString);
var
    svgName: UnicodeString;
    fileRec: TSearchRec;
    pSvg:    IWSmartPointer<TWSVGGraphic>;
    svgSize: TSize;
begin
    Browser_ClearView();

    // save the current directory
    m_CurrentDir := IncludeTrailingPathDelimiter(ExtractFilePath(fileName));

    // get the SVG file name to open
    svgName := ExtractFileName(fileName);

    try
        lbDir.Items.BeginUpdate;
        lbDir.Clear;

        // also list the other SVG in the dir to facilitate the browsing
        if (FindFirst(m_CurrentDir + '*.svg', faNormal, fileRec) = 0) then
        begin
            repeat
                // add SVG file name to list
                lbDir.Items.Add(fileRec.Name);

                // found currently opened SVG?
                if (fileRec.Name = svgName) then
                begin
                    // select it
                    lbDir.ItemIndex    := lbDir.Count - 1;
                    m_CurrentSelection := lbDir.ItemIndex;
                end;
            until (FindNext(fileRec) <> 0);

            System.SysUtils.FindClose(fileRec);
        end;
    finally
        lbDir.Items.EndUpdate;
        lbDir.Invalidate;
    end;

    // load and configure the SVG file
    pSvg              := TWSmartPointer<TWSVGGraphic>.Create();
    pSvg.LoadFromFile(fileName);
    pSvg.Proportional := true;

    if (acFitToView.Checked) then
    begin
        // set SVG size to fit the view
        pSvg.Width  := imViewer.Width;
        pSvg.Height := imViewer.Height;
    end
    else
    begin
        // get original SVG size
        svgSize := pSvg.Rasterizer.GetSize(pSvg.Native);

        // reset the zoom control
        edBrowserZoom.Text     := '100';
        udBrowserZoom.Position :=  100;

        // set SVG size
        pSvg.Width  := svgSize.Width;
        pSvg.Height := svgSize.Height;
    end;

    // show it in viewer
    imViewer.Picture.Assign(pSvg);
end;
//---------------------------------------------------------------------------
procedure TMainForm.Browser_OnResize;
begin
    // is browser visible and a picture is currently shown?
    if ((not acFitToView.Checked) or (pcMain.ActivePage <> tsBrowser) or (not Assigned(imViewer.Picture.Graphic))) then
        Exit;

    // resize it
    imViewer.Picture.Graphic.Width  := imViewer.ClientWidth;
    imViewer.Picture.Graphic.Height := imViewer.ClientHeight;
end;
//---------------------------------------------------------------------------
function TMainForm.Browser_OnAnimate(pSender: TObject; pAnimDesc: TWSVGAnimationDescriptor;
        pCustomData: Pointer): Boolean;
begin
    // this event is called only if SVG supports animation, so enable the animation interface
    acAnimate.Enabled          := True;
    edBrowserAnimSpeed.Enabled := True;
    udBrowserAnimSpeed.Enabled := True;
    edBrowserAnimSpeed.Text    := IntToStr(imViewer.Animation.FrameCount);

    // notify that animation is allowed to run
    Result := True;
end;
//---------------------------------------------------------------------------
procedure TMainForm.MakeRounded(pControl: TWinControl);
var
    rect:    TRect;
    hRegion: HRGN;
begin
    rect    := pControl.ClientRect;
    hRegion := CreateRoundRectRgn(rect.Left, rect.Top, rect.Right + 5, rect.Bottom + 5, 5, 5);
    pControl.Perform(EM_GETRECT, 0, LPARAM(@rect));
    InflateRect(rect, -5, -5);
    pControl.Perform(EM_SETRECTNP, 0, LPARAM(@rect));
    SetWindowRgn(pControl.Handle, hRegion, true);
    pControl.Invalidate;
end;
//---------------------------------------------------------------------------
procedure TMainForm.ResizeableTrackbarWndProc(var message: TMessage);
begin
    case (message.Msg) of
        CM_MOUSEENTER: tiResize.Enabled := False;
        CM_MOUSELEAVE: tiResize.Enabled := True;
    end;

    if (Assigned(m_ResizeableTrackbarWndProc_Backup)) then
        m_ResizeableTrackbarWndProc_Backup(message);
end;
//---------------------------------------------------------------------------
procedure TMainForm.StepByStepTrackbarWndProc(var message: TMessage);
begin
    case (message.Msg) of
        CM_MOUSEENTER: imGalleryLine1StepByStepImage.Animation.Animate := False;
        CM_MOUSELEAVE: imGalleryLine1StepByStepImage.Animation.Animate := True;
    end;

    if (Assigned(m_StepByStepTrackbarWndProc_Backup)) then
        m_StepByStepTrackbarWndProc_Backup(message);
end;
//---------------------------------------------------------------------------

end.
