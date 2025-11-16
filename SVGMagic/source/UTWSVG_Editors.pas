{**
 @abstract(@name provides the editors required to edit the advanced TWSVG properties.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVG_Editors;

interface

uses DesignEditors,
     System.Classes,
     Vcl.Controls,
     Vcl.Dialogs,
     UTWSmartPointer,
     UTWSVGImageList,
     UTWSVGImageListEditorDlg;

type
    {**
     component editor for the SVG image list, changes the component behavior itself to custom
     respond to double-click, right click, ...
    }
    TWSVGImageListComponentEditor = class(TComponentEditor)
        public
            {**
             Called when component is double-clicked and property should be edited
            }
            procedure Edit; override;

            {**
             Called when user selected custom entry in component popup menu
             @param(index Custom entry index)
            }
            procedure ExecuteVerb(index: Integer); override;

            {**
             Get custom popup menu item caption to show
             @param(index Custom item index)
             @returns(Caption to show)
            }
            function GetVerb(index: Integer): UnicodeString; override;

            {**
             Get custom item count
             @returns(Count)
            }
            function GetVerbCount: Integer; override;
    end;

implementation
//---------------------------------------------------------------------------
procedure TWSVGImageListComponentEditor.Edit;
var
    pComponent: TComponent;
    pImageList: TWSVGImageList;
    pDialog:    IWSmartPointer<TWSVGImageListEditorDlg>;
begin
    // get component containing property to edit
    pComponent := GetComponent;

    // found it?
    if (not Assigned(pComponent) or not (pComponent is TWSVGImageList)) then
    begin
        MessageDlg('SVG image list not found', mtError, [mbOK], 0);
        Exit;
    end;

    // get the image list to edit
    pImageList := pComponent as TWSVGImageList;

    // create and populate image list editor dialog
    pDialog := TWSmartPointer<TWSVGImageListEditorDlg>.Create
            (TWSVGImageListEditorDlg.CreateDesigner(nil, Designer));
    pDialog.Caption   := 'Editing ' + pImageList.Name;
    pDialog.ImageList := pImageList;

    // show dialog
    if (pDialog.ShowModal = mrOk) then
        // reassign image list on success
        pImageList.Assign(pDialog.ImageList);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListComponentEditor.ExecuteVerb(index: Integer);
begin
    // edit images property
    if (index = 0) then
        Edit;
end;
//---------------------------------------------------------------------------
function TWSVGImageListComponentEditor.GetVerb(index: Integer): UnicodeString;
begin
    if (index = 0) then
        Exit('Edit images...');

    Result := '';
end;
//---------------------------------------------------------------------------
function TWSVGImageListComponentEditor.GetVerbCount: Integer;
begin
    Result := 1;
end;
//---------------------------------------------------------------------------

end.
