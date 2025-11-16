{**
 @abstract(@name provides an editor dialog box to deal with the SVG images contained inside a SVG
           image list.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGImageListEditorDlg;

interface

uses DesignIntf,
     System.SysUtils,
     System.Variants,
     System.Classes,
     System.Win.Registry,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.Forms,
     Vcl.Dialogs,
     Vcl.StdCtrls,
     Vcl.ExtCtrls,
     Vcl.ComCtrls,
     Vcl.Grids,
     Winapi.Messages,
     Winapi.Windows,
     Winapi.Shellapi,
     UTWColor,
     UTWSmartPointer,
     UTWSVGGraphic,
     UTWSVGImageList;

type
    TWSVGImageListEditorDlg = class(TForm)
        published
            paButtons: TPanel;
            btCancel: TButton;
            btOk: TButton;
            paEditor: TPanel;
            dgImageGrid: TDrawGrid;
            paEditorButtons: TPanel;
            btAdd: TButton;
            btSaveToFile: TButton;
            btEdit: TButton;
            btDelete: TButton;
            btDeleteAll: TButton;
            btMoveBefore: TButton;
            btMoveAfter: TButton;
            odOpenDlg: TOpenDialog;
            sdSaveDlg: TSaveDialog;
            cbColorDlg: TColorBox;
            cdColorDlg: TColorDialog;
            paColorKey: TPanel;
            btColorKey: TButton;
            paColorKeySample: TPanel;
            tbOpacity: TTrackBar;
            laOpacityTitle: TLabel;

            procedure FormCreate(pSender: TObject);
            procedure FormClose(pSender: TObject; var action: TCloseAction);
            procedure FormShow(pSender: TObject);
            procedure dgImageGridMouseDown(pSender: TObject; button: TMouseButton; shift: TShiftState;
                    x, y: Integer);
            procedure dgImageGridClick(pSender: TObject);
            procedure dgImageGridDragOver(pSender, pSource: TObject; x, y: Integer; state: TDragState;
                    var accept: Boolean);
            procedure dgImageGridDragDrop(pSender, pSource: TObject; x, y: Integer);
            procedure dgImageGridDrawCell(pSender: TObject; col, row: Integer; rect: TRect;
                    state: TGridDrawState);
            procedure btAddClick(pSender: TObject);
            procedure btEditClick(pSender: TObject);
            procedure btMoveBeforeClick(pSender: TObject);
            procedure btMoveAfterClick(pSender: TObject);
            procedure btDeleteClick(pSender: TObject);
            procedure btSaveToFileClick(pSender: TObject);
            procedure btDeleteAllClick(pSender: TObject);
            procedure btColorKeyClick(pSender: TObject);
            procedure btOkClick(pSender: TObject);
            procedure tbOpacityChange(pSender: TObject);

        private
            m_pImageList:              TWSVGImageList;
            m_pDesigner:               IDesigner;
            m_RegKey:                  UnicodeString;
            m_Modified:                Boolean;
            m_ImageGridWndProc_Backup: TWndMethod;

        protected
            {**
             Read the form position from registry
            }
            procedure ReadPosFromRegistry; virtual;

            {**
             Write the form position to registry
            }
            procedure WritePosToRegistry; virtual;

            {**
             Configure the image list grid in relation to the number of cells to show inside the client rect
             @param(cellCount Cell count to show in the grid)
            }
            procedure ConfigGrid(cellCount: Integer); virtual;

            {**
             Set image list
             @param(pImageList Image list)
            }
            procedure SetImageList(pImageList: TWSVGImageList); virtual;

            {**
             Rebuild the view image list
            }
            procedure RebuildList; virtual;

            {**
             Add image from file
             @param(fileName Image file name to add)
            }
            procedure AddImage(fileName: UnicodeString); virtual;

            {**
             Selected index in grid
             @param(index Index to select in grid)
            }
            procedure Select(index: Integer); virtual;

            {**
             Get selected index
             @returns(Selected index, -1 if no selection)
            }
            function GetSelectedIndex: Integer; virtual;

            {**
             Get cell index from a mouse coordinate
             @param(x Mouse coordinate on the x axis in pixels, from the view left and top client rect)
             @param(y Mouse coordinate on the y axis in pixels, from the view left and top client rect)
             @returns(Cell index above mouse cursor, -1 if no cell above)
            }
            function GetIndexFromMouseCoords(x, y: Integer): Integer; virtual;

            {**
             Get cell index from column and row indices
             @param(col column index)
             @param(row Row index)
             @returns(Cell at column and row, -1 if no cell was found)
            }
            function GetIndexFromColRow(col, row: Longint): Integer; virtual;

            {**
             Set image list content as modified
            }
            procedure Touch; virtual;

            {**
             Called when image list has changed
            }
            procedure OnChange; virtual;

            {**
             Image grid hooked Windows procedure
             @param(message @bold([in, out]) Windows message, may contains result on function ends)
            }
            procedure ImageGridWndProc(var message: TMessage); virtual;

        public
            {**
             Constructor
             @param(pOwner Dialog box owner)
             @param(pDesigner Embarcadero designer)
            }
            constructor CreateDesigner(pOwner: TComponent; pDesigner: IDesigner); virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

        public
            {**
             Get or set the image list to edit
            }
            property ImageList: TWSVGImageList read m_pImageList write SetImageList;
    end;

var
    WSVGImageListEditorDlg: TWSVGImageListEditorDlg;

implementation
//---------------------------------------------------------------------------
// Global resources
//---------------------------------------------------------------------------
{$R *.dfm}
//---------------------------------------------------------------------------
// TWSVGImageListEditorDlg
//---------------------------------------------------------------------------
constructor TWSVGImageListEditorDlg.CreateDesigner(pOwner: TComponent; pDesigner: IDesigner);
begin
    inherited Create(pOwner);

    m_pImageList := TWSVGImageList.Create(Self);
    m_pDesigner  := pDesigner;
    m_RegKey     := '\Software\SVGMagic\ImgListEditor';
    m_Modified   := False;

    // hook the image grid windows procedure
    m_ImageGridWndProc_Backup := dgImageGrid.WindowProc;
    dgImageGrid.WindowProc    := ImageGridWndProc;
end;
//---------------------------------------------------------------------------
destructor TWSVGImageListEditorDlg.Destroy;
begin
    // prohibit view to receive external files
    DragAcceptFiles(dgImageGrid.Handle, False);

    // restore the previously defined message loop in image grid
    dgImageGrid.WindowProc := m_ImageGridWndProc_Backup;

    FreeAndNil(m_pImageList);

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.FormCreate(pSender: TObject);
begin
    ReadPosFromRegistry;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.FormClose(pSender: TObject; var action: TCloseAction);
begin
    WritePosToRegistry;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.FormShow(pSender: TObject);
begin
    // allow the external files to be dragged onto the view. NOTE as explained here:
    // https://www.experts-exchange.com/questions/23538464/Unable-to-get-Drag-and-Drop-FIles-working.html
    // the DragAcceptFiles() function should not be called from the constructor, because it's happen
    // too early and prevent the external drag to be recognized
    DragAcceptFiles(dgImageGrid.Handle, True);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.dgImageGridMouseDown(pSender: TObject; button: TMouseButton;
        shift: TShiftState; x, y: Integer);
begin
    dgImageGrid.BeginDrag(false, 10);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.dgImageGridClick(pSender: TObject);
begin
    OnChange;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.dgImageGridDragOver(pSender, pSource: TObject; x, y: Integer;
        state: TDragState; var accept: Boolean);
var
    index: Integer;
begin
    // get matching item index
    index := GetIndexFromMouseCoords(x, y);

    // drag can only be accepted if mouse is hovering a valid cell
    accept := (index >= 0) and (index < m_pImageList.Count);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.dgImageGridDragDrop(pSender, pSource: TObject; x, y: Integer);
var
    pSVG:                           TWSVGGraphic;
    colorKey:                       TWColor;
    srcIndex, dstIndex, imageCount: Integer;
begin
    // get matching source and destination item indices
    srcIndex := GetSelectedIndex;
    dstIndex := GetIndexFromMouseCoords(x, y);

    imageCount := m_pImageList.Count;

    // are indices valid?
    if ((srcIndex < 0) or (srcIndex >= imageCount) or (dstIndex < 0) or (dstIndex >= imageCount)) then
        Exit;

    pSVG := TWSVGGraphic.Create;

    try
        // keep the current destination image in a temporary image
        colorKey := m_pImageList.GetSVGColorKey(dstIndex);
        pSVG.Assign(m_pImageList.GetSVG(dstIndex));

        // copy the source image in the destination
        m_pImageList.ReplaceSVG(dstIndex, m_pImageList.GetSVG(srcIndex));
        m_pImageList.SetSVGColorKey(dstIndex, m_pImageList.GetSVGColorKey(srcIndex));

        // copy the temporary kept image in the source
        m_pImageList.ReplaceSVG(srcIndex, pSVG);
        m_pImageList.SetSVGColorKey(srcIndex, colorKey);
    finally
        pSVG.Free;
    end;

    OnChange;

    dgImageGrid.Invalidate;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.dgImageGridDrawCell(pSender: TObject; col, row: Integer;
        rect: TRect; state: TGridDrawState);
var
    pSVG:                TWSVGGraphic;
    text:                UnicodeString;
    textRect, imageRect: TRect;
    index:               Integer;
    prevAnimate:         Boolean;
begin
    // calculate the current index in grid
    index := (row * dgImageGrid.ColCount) + col;

    // select background color to use
    if (gdSelected in state) then
        dgImageGrid.Canvas.Brush.Color := clSkyBlue
    else
        dgImageGrid.Canvas.Brush.Color := clWhite;

    // draw item background
    dgImageGrid.Canvas.FillRect(rect);

    // is index out of bounds?
    if (index >= m_pImageList.Count) then
    begin
        // fill item background with grid color
        dgImageGrid.Canvas.Brush.Color := dgImageGrid.Color;
        dgImageGrid.Canvas.Brush.Style := bsSolid;
        dgImageGrid.Canvas.FillRect(rect);
        Exit;
    end;

    // calculate text rectangle
    textRect := TRect.Create(rect.Left, rect.Bottom - 15, rect.Right, rect.Bottom);

    // get picture as SVG graphic
    pSVG := m_pImageList.GetSVG(index);

    // found it?
    if (not Assigned(pSVG)) then
    begin
        // draw item background
        dgImageGrid.Canvas.Brush.Color := clRed;
        dgImageGrid.Canvas.FillRect(rect);

        DrawText(dgImageGrid.Canvas.Handle, '#ERROR', 6, textRect, DT_SINGLELINE or DT_LEFT
                or DT_VCENTER or DT_END_ELLIPSIS);

        Exit;
    end;

    // calculate image rectangle
    imageRect := TRect.Create(rect.Left + 7, rect.Top + 2, rect.Right - 7, rect.Bottom - 17);

    // keep previous SVG value
    prevAnimate := pSVG.Animate;

    try
        // configure SVG
        pSVG.Animate      := False;
        pSVG.Proportional := True;

        // draw image
        dgImageGrid.Canvas.StretchDraw(imageRect, pSVG);
    finally
        // revert SVG to previous value
        pSVG.Animate := prevAnimate;
    end;

    // get text to draw
    text := IntToStr(index);

    // draw text
    DrawTextW(dgImageGrid.Canvas.Handle, PWideChar(text), Length(text), textRect, DT_SINGLELINE
            or DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.btAddClick(pSender: TObject);
var
    fileName: UnicodeString;
begin
    // open dialog box and check user result
    if (not odOpenDlg.Execute) then
        Exit;

    // only one file selected?
    if (odOpenDlg.Files.Count > 1) then
    begin
        // import images
        for fileName in odOpenDlg.Files do
            AddImage(fileName);
    end
    else
        // import image
        AddImage(odOpenDlg.FileName);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.btEditClick(pSender: TObject);
var
    pSVG:  IWSmartPointer<TWSVGGraphic>;
    index: NativeInt;
begin
    try
        // get selected item index
        index := GetSelectedIndex;

        // is index valid?
        if ((index < 0) or (index >= m_pImageList.Count)) then
        begin
            MessageDlg('Index is out of bounds', mtError, [mbOK], 0);
            Exit;
        end;

        // open dialog box and check user result
        if (not odOpenDlg.Execute) then
            Exit;

        // load SVG image
        pSVG := TWSmartPointer<TWSVGGraphic>.Create();
        pSVG.LoadFromFile(odOpenDlg.FileName);
        m_pImageList.ReplaceSVG(index, pSVG);

        RebuildList;

        // restore selection
        Select(index);

        Touch;

        // notify that image list has changed
        OnChange;
    except
        MessageDlg('Could not modify item', mtError, [mbOK], 0);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.btMoveBeforeClick(pSender: TObject);
var
    pSrcSVG, pDstSVG:       TWSVGGraphic;
    pSrcTmpSvg, pDstTmpSvg: IWSmartPointer<TWSVGGraphic>;
    imageCount, index:      NativeInt;
begin
    try
        // get image count
        imageCount := m_pImageList.Count;

        // not enough items to swap?
        if (imageCount <= 1) then
            Exit;

        // get selected item index
        index := GetSelectedIndex;

        // is index valid?
        if ((index < 1) or (index >= imageCount)) then
            Exit;

        // swap images
        pSrcTmpSvg := TWSmartPointer<TWSVGGraphic>.Create();
        pDstTmpSvg := TWSmartPointer<TWSVGGraphic>.Create();
        pSrcSVG    := m_pImageList.GetSVG(index - 1);
        pDstSVG    := m_pImageList.GetSVG(index);
        pSrcTmpSvg.Assign(pSrcSVG);
        pDstTmpSvg.Assign(pDstSVG);
        m_pImageList.ReplaceSVG(index - 1, pDstTmpSvg);
        m_pImageList.ReplaceSVG(index,     pSrcTmpSvg);

        // update index to new position
        Select(index - 1);

        Touch;

        // notify that image list has changed
        OnChange;
    Except
        MessageDlg('Swap failed', mtError, [mbOK], 0);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.btMoveAfterClick(pSender: TObject);
var
    pSrcSVG, pDstSVG:       TWSVGGraphic;
    pSrcTmpSvg, pDstTmpSvg: IWSmartPointer<TWSVGGraphic>;
    imageCount, index:      NativeInt;
begin
    try
        // get image count
        imageCount := m_pImageList.Count;

        // not enough items to swap?
        if (imageCount <= 1) then
            Exit;

        // get selected item index
        index := GetSelectedIndex;

        // is index valid?
        if ((index < 0) or (index >= imageCount - 1)) then
            Exit;

        // swap images
        pSrcTmpSvg := TWSmartPointer<TWSVGGraphic>.Create();
        pDstTmpSvg := TWSmartPointer<TWSVGGraphic>.Create();
        pSrcSVG    := m_pImageList.GetSVG(index);
        pDstSVG    := m_pImageList.GetSVG(index + 1);
        pSrcTmpSvg.Assign(pSrcSVG);
        pDstTmpSvg.Assign(pDstSVG);
        m_pImageList.ReplaceSVG(index,     pDstTmpSvg);
        m_pImageList.ReplaceSVG(index + 1, pSrcTmpSvg);

        // update index to new position
        Select(index + 1);

        Touch;

        // notify that image list has changed
        OnChange;
    Except
        MessageDlg('Swap failed', mtError, [mbOK], 0);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.btDeleteClick(pSender: TObject);
var
    index: NativeInt;
begin
    try
        // get selected item index
        index := GetSelectedIndex;

        // is index valid?
        if ((index < 0) or (index >= m_pImageList.Count)) then
        begin
            MessageDlg('Index is out of bounds', mtError, [mbOK], 0);
            Exit;
        end;

        // delete image at index
        m_pImageList.DeleteSVG(index);

        RebuildList;

        // any item remaining in list?
        if (m_pImageList.Count > 0) then
            // restore selection
            if (index >= m_pImageList.Count) then
                Select(m_pImageList.Count - 1)
            else
                Select(index);

        Touch;

        // notify that image list has changed
        OnChange;
    except
        MessageDlg('Delete failed', mtError, [mbOK], 0);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.btSaveToFileClick(pSender: TObject);
var
    pSVG:  TWSVGGraphic;
    index: NativeInt;
begin
    // open dialog box and check user result
    if (not sdSaveDlg.Execute) then
        Exit;

    try
        // get selected item index
        index := GetSelectedIndex;

        // is index valid?
        if ((index < 0) or (index >= m_pImageList.Count)) then
        begin
            MessageDlg('Index is out of bounds', mtError, [mbOK], 0);
            Exit;
        end;

        // get item
        pSVG := m_pImageList.GetSVG(index);

        // found it?
        if (not Assigned(pSVG)) then
        begin
            MessageDlg('Invalid item', mtError, [mbOK], 0);
            Exit;
        end;

        // save to file
        pSVG.SaveToFile(sdSaveDlg.FileName);
    except
        MessageDlg('Could not save file - ' + sdSaveDlg.FileName, mtError, [mbOK], 0);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.btDeleteAllClick(pSender: TObject);
begin
    // clear image list
    m_pImageList.Clear;

    // clear shown image grid
    dgImageGrid.ColCount := 0;
    dgImageGrid.RowCount := 0;

    Touch;

    // notify that image list has changed
    OnChange;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.btColorKeyClick(pSender: TObject);
var
    index: NativeInt;
    color: TWColor;
begin
    // get selected item index
    index := GetSelectedIndex;

    // is index valid?
    if ((index < 0) or (index >= m_pImageList.Count)) then
    begin
        MessageDlg('Index is out of bounds', mtError, [mbOK], 0);
        Exit;
    end;

    // set the current color key as default color
    color            := m_pImageList.GetSVGColorKey(index);
    cdColorDlg.Color := color.GetColor;

    // open dialog box and check user result
    if (not cdColorDlg.Execute) then
        Exit;

    // update color key
    color := TWColor.Create(cdColorDlg.Color, 255);
    m_pImageList.SetSVGColorKey(index, color);

    Touch;

    // notify that image list has changed
    OnChange;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.btOkClick(pSender: TObject);
begin
    // modification occurred?
    if (m_Modified) then
        // notify designer
        m_pDesigner.Modified;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.tbOpacityChange(pSender: TObject);
var
    index: NativeInt;
    color: TWColor;
begin
    // get selected item index
    index := GetSelectedIndex;

    // is index valid?
    if ((index < 0) or (index >= m_pImageList.Count)) then
    begin
        MessageDlg('Index is out of bounds', mtError, [mbOK], 0);
        Exit;
    end;

    // get color key
    color            := m_pImageList.GetSVGColorKey(index);
    cdColorDlg.Color := color.GetColor;

    // update color key
    color.SetOpacity(tbOpacity.Position);
    m_pImageList.SetSVGColorKey(index, color);

    Touch;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.ReadPosFromRegistry;
var
    pRegistry:   TRegistry;
    desktopRect: TRect;
begin
    // open registry instance
    pRegistry := TRegistry.Create;

    // succeeded?
    if (not Assigned(pRegistry)) then
    begin
        Position := poDefaultPosOnly;
        Exit;
    end;

    try
        // set current user as root key
        pRegistry.RootKey := HKEY_CURRENT_USER;

        // open registry key
        if (pRegistry.OpenKey(m_RegKey, True)) then
        begin
            // read the left value, if exists
            if (pRegistry.ValueExists('left')) then
                Left := pRegistry.ReadInteger('left')
            else
            begin
                Position := poDefaultPosOnly;
                Exit;
            end;

            // read the top value, if exists
            if (pRegistry.ValueExists('top')) then
                Top := pRegistry.ReadInteger('top')
            else
            begin
                Position := poDefaultPosOnly;
                Exit;
            end;

            // read the width, if exists
            if (pRegistry.ValueExists('width')) then
                Width := pRegistry.ReadInteger('width')
            else
            begin
                Position := poDefaultPosOnly;
                Exit;
            end;

            // read the height, if exists
            if (pRegistry.ValueExists('height')) then
                Height := pRegistry.ReadInteger('height')
            else
            begin
                Position := poDefaultPosOnly;
                Exit;
            end;
        end
        else
        begin
            Position := poDefaultPosOnly;
            exit;
        end;

        // get work area rect
        desktopRect := Screen.DesktopRect;

        // is form left out of screen?
        if (Left < desktopRect.Left) then
            Left := desktopRect.Left;

        // is form top out of screen?
        if (Top < desktopRect.Top) then
            Top := desktopRect.Top;

        // is form right out of screen?
        if ((Left + Width) > desktopRect.Right) then
            Left := desktopRect.Right - Width;

        // is form bottom out of screen?
        if ((Top + Height) > desktopRect.Bottom) then
            Top := desktopRect.Bottom - Height;
    Finally
        pRegistry.CloseKey;
        pRegistry.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.WritePosToRegistry;
var
pRegistry : TRegistry;
begin
    // open registry instance
    pRegistry := TRegistry.Create;

    // succeeded?
    if (not Assigned(pRegistry)) then
    begin
        Position := poDefaultPosOnly;
        Exit;
    end;

    try
        // set current user as root key
        pRegistry.RootKey := HKEY_CURRENT_USER;

        // open registry key
        if pRegistry.OpenKey(m_RegKey, True) then
        begin
            // write the form left, top, width and height in registry
            pRegistry.WriteInteger('left',   Left);
            pRegistry.WriteInteger('top',    Top);
            pRegistry.WriteInteger('height', Height);
            pRegistry.WriteInteger('width',  Width);
        end;
    Finally
        pRegistry.CloseKey;
        pRegistry.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.ConfigGrid(cellCount: Integer);
begin
    // clear the grid if nothing to show
    if ((cellCount = 0) or (dgImageGrid.ClientWidth = 0)) then
    begin
        dgImageGrid.ColCount := 0;
        dgImageGrid.RowCount := 0;
        Exit;
    end;

    // grid cell must have a defined width and height
    Assert(dgImageGrid.DefaultColWidth  > 0);
    Assert(dgImageGrid.DefaultRowHeight > 0);

    // calculate the number of needed columns and rows
    dgImageGrid.ColCount := dgImageGrid.ClientWidth div dgImageGrid.DefaultColWidth;
    dgImageGrid.RowCount := cellCount div dgImageGrid.ColCount;

    // check if the last row is partially filled by cells, add it if yes
    if ((cellCount mod dgImageGrid.ColCount) <> 0) then
        dgImageGrid.RowCount := dgImageGrid.RowCount + 1;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.SetImageList(pImageList: TWSVGImageList);
begin
    m_pImageList.Clear;

    if (not Assigned(pImageList)) then
        Exit;

    m_pImageList.Assign(pImageList);

    RebuildList;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.RebuildList;
begin
    // configure grid to show images
    ConfigGrid(m_pImageList.Count);

    // unselect all cells by default
    Select(-1);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.AddImage(fileName: UnicodeString);
var
    pSVG: IWSmartPointer<TWSVGGraphic>;
begin
    try
        // load SVG and add it to image list
        pSVG := TWSmartPointer<TWSVGGraphic>.Create();
        pSVG.LoadFromFile(fileName);
        m_pImageList.AddSVG(pSVG);

        RebuildList;

        Touch;

        // notify that image list has changed
        OnChange;
    except
        MessageDlg('Could not load file - ' + fileName, mtError, [mbOK], 0);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.Select(index: Integer);
const
    noSelRect: TGridRect = (Left: -1; Top: -1; Right: -1; Bottom: -1);
    defRect:   TGridRect = (Left:  0; Top:  0; Right:  1; Bottom:  1);
begin
    // is index valid?
    if ((index < 0) or (index >= m_pImageList.Count)) then
        dgImageGrid.Selection := noSelRect
    else
    begin
        // dummy code, completely stupid, but if nothing is selected while the Col and Row are
        // modified, an "out of range" exception will be raised
        dgImageGrid.Selection := defRect;

        // calculate and select column and row matching with index
        dgImageGrid.Col := index mod dgImageGrid.ColCount;
        dgImageGrid.Row := index div dgImageGrid.ColCount;
    end;

    OnChange;
end;
//---------------------------------------------------------------------------
function TWSVGImageListEditorDlg.GetSelectedIndex: Integer;
begin
    // nothing selected?
    if ((dgImageGrid.Col < 0) or (dgImageGrid.Row < 0)) then
        Exit(-1);

    Result := GetIndexFromColRow(dgImageGrid.Col, dgImageGrid.Row);
end;
//---------------------------------------------------------------------------
function TWSVGImageListEditorDlg.GetIndexFromMouseCoords(x, y: Integer): Integer;
var
    col, row: Integer;
begin
    // get the row and column matching with current mouse position
    dgImageGrid.MouseToCell(x, y, col, row);

    // get matching item index
    Result := GetIndexFromColRow(col, row);
end;
//---------------------------------------------------------------------------
function TWSVGImageListEditorDlg.GetIndexFromColRow(col, row: Longint): Integer;
begin
    // no column or row?
    if ((dgImageGrid.ColCount <= 0) or (dgImageGrid.RowCount <= 0)) then
        Exit(-1);

    // calculate matching item index
    Result := (row * dgImageGrid.ColCount) + col;

    // is index valid?
    if ((Result < 0) or (Result >= m_pImageList.Count)) then
        Exit(-1);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.Touch;
begin
    m_Modified := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.OnChange;
var
    imageCount, index: NativeInt;
    actionEnabled:     Boolean;
    color:             TWColor;
begin
    // get image count
    imageCount := m_pImageList.Count;

    // get selected item index
    index := GetSelectedIndex;

    // are actions enabled?
    actionEnabled := ((index >= 0) and (index < imageCount));

    btSaveToFile.Enabled     := actionEnabled;
    btEdit.Enabled           := actionEnabled;
    btDelete.Enabled         := actionEnabled;
    btColorKey.Enabled       := actionEnabled;
    tbOpacity.Enabled        := actionEnabled;
    paColorKeySample.Enabled := actionEnabled;

    // are move enabled?
    btMoveBefore.Enabled := actionEnabled and (imageCount > 1) and (index >= 1) and (index < imageCount);
    btMoveAfter.Enabled  := actionEnabled and (imageCount > 1) and (index >= 0) and (index < imageCount - 1);

    // can delete all items?
    btDeleteAll.Enabled := (imageCount > 0);

    // update the color key
    if (actionEnabled) then
    begin
        color                  := m_pImageList.GetSVGColorKey(index);
        paColorKeySample.Color := color.GetColor;
        tbOpacity.Position     := color.GetOpacity;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageListEditorDlg.ImageGridWndProc(var message: TMessage);
var
    hDropHandle:          HDROP;
    bufferSize, count, i: Cardinal;
    fileName:             string;
begin
    case (message.Msg) of
        WM_WINDOWPOSCHANGED:
        begin
            ConfigGrid(m_pImageList.Count);
            dgImageGrid.Invalidate;
        end;

        WM_MOUSEWHEEL:
        begin
            // set the focus on view in case the mouse wheel was performed above it, otherwise the
            // selection will remain frozen on the first cell
            if (not dgImageGrid.Focused and dgImageGrid.CanFocus) then
                dgImageGrid.SetFocus;

            // ignore mouse wheel on empty view, because Embarcadero developers didn't foresee that
            // may happen, and doing that an "out of range" exception is raised
            if (m_pImageList.Count = 0) then
            begin
                message.Result := 0;
                Exit;
            end;

            // certify that a cell is always selected before processing the scroll, otherwise the
            // view may raise an "out of range" exception
            if ((m_pImageList.Count > 0) and ((dgImageGrid.Col < 0) or (dgImageGrid.Row < 0))) then
            begin
                Select(0);
                message.Result := 0;
                Exit;
            end;
        end;

        WM_DROPFILES:
        begin
            hDropHandle := HDROP(message.WParam);

            try
                // get dropped files count
                count := DragQueryFile(hDropHandle, $FFFFFFFF, nil, 0);

                // iterate through dropped file names
                for i := 0 to count - 1 do
                begin
                    // get file name length and initialize string which will receives it
                    bufferSize := DragQueryFile(hDropHandle, i, nil, 0);
                    SetLength(FileName, bufferSize);
                    FillChar(fileName, $0, bufferSize);

                    // get filename for each drop
                    DragQueryFile(hDropHandle, i, PChar(fileName), bufferSize + 1);

                    // ignore dirs
                    if (DirectoryExists(fileName)) then
                        continue;

                    // also ignore unknown files
                    if (LowerCase(ExtractFileExt(fileName)) <> '.svg') then
                        continue;

                    // add the file
                    AddImage(fileName);
                end;
            finally
                DragFinish(hDropHandle);
            end;

            OnChange;

            dgImageGrid.Invalidate;
        end;
    end;

    if (Assigned(m_ImageGridWndProc_Backup)) then
        m_ImageGridWndProc_Backup(message);
end;
//---------------------------------------------------------------------------

end.
