unit UTWSVG_Register;

interface

uses DesignIntf,
     System.Classes,
     System.TypInfo,
     UTWSVGGraphic,
     UTWSVGImage,
     UTWSVGImageButton,
     UTWSVGImageList,
     UTWSVGCheckBoxStyle,
     UTWSVGRadioButtonStyle,
     UTWSVG_Editors;

{**
 Main register procedure
}
procedure Register;

implementation
//---------------------------------------------------------------------------
// Main register procedure
//---------------------------------------------------------------------------
procedure Register;
begin
    // register all components to show in the designer toolbar
    RegisterComponents('TWSVGControls', [TWSVGImage, TWSVGImageButton, TWSVGImageList,
            TWSVGCheckBoxStyle, TWSVGRadioButtonStyle]);

    // register SVG image list component editor
    RegisterComponentEditor(TWSVGImageList, TWSVGImageListComponentEditor);

    // hide properties that should be omitted in TWSVGImageList
    UnlistPublishedProperty(TWSVGImageList, 'ColorDepth');

    // force IDE to register SVG graphic immediately, before opening project in designer for the first
    // time, otherwise TImage may loose their content while opened in XE8 and newer
    ForceDemandLoadState(dlDisable);
end;
//---------------------------------------------------------------------------

end.
