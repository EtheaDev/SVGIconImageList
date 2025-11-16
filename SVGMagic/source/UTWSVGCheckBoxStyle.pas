{**
 @abstract(@name provides a class that allows the checkbox glyphs to be overriden by SVG.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGCheckBoxStyle;

interface

uses System.Classes,
     System.SysUtils,
     {$if CompilerVersion <= 24}
        Vcl.ActnList,
     {$else}
        System.Actions,
     {$ifend}
     System.Generics.Collections,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.StdCtrls,
     Winapi.Messages,
     Winapi.Windows,
     UTWColor,
     UTWHelpers,
     UTWSVGGraphic,
     UTWSVGComponentStyle;

type
    {**
     Style to apply to checkbox
    }
    TWSVGCheckBoxStyle = class(TWSVGComponentStyle)
        public type
            {**
             Collection changed messages
            }
            IECollectionChangedMsg =
            (
                IE_CC_ItemAdded,
                IE_CC_ItemDeleting,
                IE_CC_ItemExtracting,
                IE_CC_ItemValueChanged
            );

            {**
             Called when collection changed
             @param(pSender Event sender)
             @param(pItem Item that changed in the collection)
             @param(message Message about action currently applying to item)
            }
            ITfCollectionChangedEvent = procedure (pSender: TObject; pItem: TCollectionItem;
                    message: IECollectionChangedMsg) of object;

            {**
             Checkbox collection item
            }
            IWCheckBoxCollectionItem = class (TCollectionItem)
                private
                    m_pCheckBox: TCheckBox;

                protected
                    {**
                     Set the checkbox owned by the item
                     @param(pCheckBox Checkbox to set)
                    }
                    procedure SetCheckBox(const pCheckBox: TCheckBox); virtual;

                    {**
                     Get the name to show in the collection
                     @returns(The display name)
                    }
                    function GetDisplayName: string; override;

                published
                    {**
                     Get or set the checkbox owned by the item
                    }
                    property CheckBox: TCheckBox read m_pCheckBox write SetCheckBox;
            end;

            {**
             Checkbox collection (can be shown in design time)
            }
            IWCheckBoxCollection = class (TCollection)
                private
                    m_pCollectionOwner: TComponent;
                    m_fOnChanged:       ITfCollectionChangedEvent;

                protected
                    {**
                     Get the collection owner
                     @returns(The collection owner)
                    }
                    function GetOwner: TPersistent; override;

                    {**
                     Called when something important (e.g. adding or removing an item) happened in the collection
                     @param(pItem Item that raised the notification)
                     @param(action Action that the item is currently applying)
                    }
                    procedure Notify(pItem: TCollectionItem; action: System.Classes.TCollectionNotification); override;

                    {**
                     Notify that collection content has changed
                     @param(pItem Item that raised the notification)
                     @param(message Message about what changed in the collection)
                    }
                    procedure CollectionChanged(pItem: TCollectionItem; message: IECollectionChangedMsg); virtual;

                public
                    {**
                     Constructor
                     @param(pCollectionOwner Collection owner)
                    }
                    constructor Create(pCollectionOwner: TComponent); virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                public
                    {**
                     Get or set the OnChanged event
                    }
                    property OnChanged: ITfCollectionChangedEvent read m_fOnChanged write m_fOnChanged;
            end;

        private
            m_pUncheckedGlyph:         TWSVGComponentStyle.IWGlyph;
            m_pCheckedGlyph:           TWSVGComponentStyle.IWGlyph;
            m_pGrayedGlyph:            TWSVGComponentStyle.IWGlyph;
            m_pDisabledUncheckedGlyph: TWSVGComponentStyle.IWGlyph;
            m_pDisabledCheckedGlyph:   TWSVGComponentStyle.IWGlyph;
            m_pDisabledGrayedGlyph:    TWSVGComponentStyle.IWGlyph;
            m_pTargetsCollection:      IWCheckBoxCollection;

        protected
            {**
             Get the unchecked state glyph
             @returns(The glyph picture)
            }
            function GetUncheckedGlyph: TPicture; virtual;

            {**
             Set the unchecked state glyph
             @param(pPicture Picture representing the state)
            }
            procedure SetUncheckedGlyph(pPicture: TPicture); virtual;

            {**
             Get the checked state glyph
             @returns(The glyph picture)
            }
            function GetCheckedGlyph: TPicture; virtual;

            {**
             Set the checked state glyph
             @param(pPicture Picture representing the state)
            }
            procedure SetCheckedGlyph(pPicture: TPicture); virtual;

            {**
             Get the grayed state glyph
             @returns(The glyph picture)
            }
            function GetGrayedGlyph: TPicture; virtual;

            {**
             Set the grayed state glyph
             @param(pPicture Picture representing the state)
            }
            procedure SetGrayedGlyph(pPicture: TPicture); virtual;

            {**
             Get the disabled unchecked state glyph
             @returns(The glyph picture)
            }
            function GetDisabledUncheckedGlyph: TPicture; virtual;

            {**
             Set the disabled unchecked state glyph
             @param(pPicture Picture representing the state)
            }
            procedure SetDisabledUncheckedGlyph(pPicture: TPicture); virtual;

            {**
             Get the disabled checked state glyph
             @returns(The glyph picture)
            }
            function GetDisabledCheckedGlyph: TPicture; virtual;

            {**
             Set the disabled checked state glyph
             @param(pPicture Picture representing the state)
            }
            procedure SetDisabledCheckedGlyph(pPicture: TPicture); virtual;

            {**
             Get the disabled grayed state glyph
             @returns(The glyph picture)
            }
            function GetDisabledGrayedGlyph: TPicture; virtual;

            {**
             Set the disabled grayed state glyph
             @param(pPicture Picture representing the state)
            }
            procedure SetDisabledGrayedGlyph(pPicture: TPicture); virtual;

            {**
             Set the target collection
             @param(pCollection New target collection to set)
            }
            procedure SetTargetsCollection(pCollection: IWCheckBoxCollection); virtual;

            {**
             Enable or disable if all components mathcing with style are skinned
             @param(value If @true, all components matching with style will be skinned)
            }
            procedure SetAll(value: Boolean); override;

            {**
             Called after component was completely loaded from DFM
            }
            procedure Loaded; override;

            {**
             Hook all the controls contained in the collection
             @param(allowCleanup If @true, the empty items are allowed to be cleaned from collection)
             @param(pItemToIgnore Item that should not be hooked e.g. because deleting, ignored if @nil)
            }
            procedure HookControlsFromCollection(allowCleanup: Boolean = True;
                    pItemToIgnore: TCollectionItem = nil); virtual;

            {**
             Called when original component size should be saved (before any DPI change was applied)
            }
            procedure DoSaveOriginalSize; override;

            {**
             Called when size should be recalculated due to a DPI change
             @param(oldDPI Previous pixels per inch value)
             @param(newDPI New pixels per inch value)
            }
            procedure DoApplyDPIChange(oldDPI, newDPI: Integer); override;

            {**
             Check if advanced paint should be processed
             @param(pTarget Target component to paint)
             @returns(@true if advanced paint should be done, otherwise @false)
            }
            function DoProcessAdvancedPaint(pTarget: TWinControl): Boolean; override;

            {**
             Check if a newly added control should be added to skin engine, and add it if yes
             @param(pControl Newly added control)
            }
            procedure DoAddTarget(pControl: TControl); override;

            {**
             Notify that the animation status has changed and should be updated
            }
            procedure DoUpdateAnimStatus; override;

            {**
             Draw the advanced paint on the target component
             @param(hDC Device context to paint on)
             @param(pTarget Target control to paint)
            }
            procedure Draw(hDC: THandle; pTarget: TWinControl); override;

            {**
             Called when a target is freed
             @param(pTarget Target about to be freed)
            }
            procedure OnFreeTarget(pTarget: TWinControl); override;

            {**
             Called when collection changed
             @param(pSender Event sender)
             @param(pItem Item that changed in the collection)
             @param(message Message about action currently applying to item)
            }
            procedure OnCollectionChanged(pSender: TObject; pItem: TCollectionItem;
                    message: IECollectionChangedMsg); virtual;

        public
            {**
             Constructor
             @param(pOwner Component owner)
            }
            constructor Create(pOwner: TComponent); override;

            {**
             Destructor
            }
            destructor Destroy; override;

        published
            {**
             Get or set the unchecked state glyph content
            }
            property UncheckedGlyph: TPicture read GetUncheckedGlyph write SetUncheckedGlyph;

            {**
             Get or set the checked state glyph content
            }
            property CheckedGlyph: TPicture read GetCheckedGlyph write SetCheckedGlyph;

            {**
             Get or set the grayed state glyph content
            }
            property GrayedGlyph: TPicture read GetGrayedGlyph write SetGrayedGlyph;

            {**
             Get or set the disabled unchecked state glyph content
            }
            property DisabledUncheckedGlyph: TPicture read GetDisabledUncheckedGlyph write SetDisabledUncheckedGlyph;

            {**
             Get or set the disabled checked state glyph content
            }
            property DisabledCheckedGlyph: TPicture read GetDisabledCheckedGlyph write SetDisabledCheckedGlyph;

            {**
             Get or set the disabled grayed state glyph content
            }
            property DisabledGrayedGlyph: TPicture read GetDisabledGrayedGlyph write SetDisabledGrayedGlyph;

            {**
             Get or set the target component collection
            }
            property Targets: IWCheckBoxCollection read m_pTargetsCollection write SetTargetsCollection;
    end;

implementation
//---------------------------------------------------------------------------
// TWSVGCheckBoxStyle.IWCheckBoxCollectionItem
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.IWCheckBoxCollectionItem.SetCheckBox(const pCheckBox: TCheckBox);
var
    pItem:               TCollectionItem;
    pCheckBoxItem:       IWCheckBoxCollectionItem;
    pCheckBoxCollection: IWCheckBoxCollection;
    pPrevCheckBox:       TCheckBox;
begin
    // iterate through collection items
    for pItem in Collection do
    begin
        // found itself?
        if (pItem = Self) then
            continue;

        // is a checkbox collection item?
        if (not (pItem is IWCheckBoxCollectionItem)) then
            continue;

        // get checkbox collection item
        pCheckBoxItem := pItem as IWCheckBoxCollectionItem;

        // another collection item with the same value already exists?
        if (pCheckBoxItem.m_pCheckBox = pCheckBox) then
            raise Exception.Create('This item already exists in the list.');
    end;

    pPrevCheckBox := m_pCheckBox;
    m_pCheckBox   := pCheckBox;

    // notify that collection has changed
    if (Collection is IWCheckBoxCollection) then
    begin
        pCheckBoxCollection := Collection as IWCheckBoxCollection;
        pCheckBoxCollection.CollectionChanged(Self, IE_CC_ItemValueChanged);
    end;

    // update previous checkbox to paint it without the removed style
    if (Assigned(pPrevCheckBox)) then
        pPrevCheckBox.Invalidate;
end;
//---------------------------------------------------------------------------
function TWSVGCheckBoxStyle.IWCheckBoxCollectionItem.GetDisplayName: string;
begin
    if (not Assigned(m_pCheckBox)) then
        Exit(inherited);

    Result := m_pCheckBox.Name;
end;
//---------------------------------------------------------------------------
// TWSVGCheckBoxStyle.IWCheckBoxCollection
//---------------------------------------------------------------------------
constructor TWSVGCheckBoxStyle.IWCheckBoxCollection.Create(pCollectionOwner: TComponent);
begin
    inherited Create(IWCheckBoxCollectionItem);

    m_pCollectionOwner := pCollectionOwner;
    m_fOnChanged       := nil;
end;
//---------------------------------------------------------------------------
destructor TWSVGCheckBoxStyle.IWCheckBoxCollection.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGCheckBoxStyle.IWCheckBoxCollection.GetOwner: TPersistent;
begin
    Result := m_pCollectionOwner;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.IWCheckBoxCollection.Notify(pItem: TCollectionItem;
        action: System.Classes.TCollectionNotification);
begin
    inherited Notify(pItem, action);

    case (action) of
        // for an unknown reason RAD Studio Rio changed the cnXXX location, breaking thus the
        // compatibility with prev compilers
        {$if CompilerVersion < 33}
            System.Classes.cnAdded:      CollectionChanged(pItem, IE_CC_ItemAdded);
            System.Classes.cnExtracting: CollectionChanged(pItem, IE_CC_ItemExtracting);
            System.Classes.cnDeleting:   CollectionChanged(pItem, IE_CC_ItemDeleting);
        {$else}
            System.Generics.Collections.cnAdded:      CollectionChanged(pItem, IE_CC_ItemAdded);
            System.Generics.Collections.cnExtracting: CollectionChanged(pItem, IE_CC_ItemExtracting);
            System.Generics.Collections.cnDeleting:   CollectionChanged(pItem, IE_CC_ItemDeleting);
        {$ifend}
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.IWCheckBoxCollection.CollectionChanged(pItem: TCollectionItem;
        message: TWSVGCheckBoxStyle.IECollectionChangedMsg);
begin
    if (Assigned(m_fOnChanged)) then
        m_fOnChanged(Self, pItem, message);
end;
//---------------------------------------------------------------------------
// TWSVGCheckBoxStyle
//---------------------------------------------------------------------------
constructor TWSVGCheckBoxStyle.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_pUncheckedGlyph         := IWGlyph.Create(Self);
    m_pCheckedGlyph           := IWGlyph.Create(Self);
    m_pGrayedGlyph            := IWGlyph.Create(Self);
    m_pDisabledUncheckedGlyph := IWGlyph.Create(Self);
    m_pDisabledCheckedGlyph   := IWGlyph.Create(Self);
    m_pDisabledGrayedGlyph    := IWGlyph.Create(Self);
    m_pTargetsCollection      := IWCheckBoxCollection.Create(Self);

    // set internal events
    m_pTargetsCollection.OnChanged := OnCollectionChanged;
end;
//---------------------------------------------------------------------------
destructor TWSVGCheckBoxStyle.Destroy;
begin
    FreeAndNil(m_pUncheckedGlyph);
    FreeAndNil(m_pCheckedGlyph);
    FreeAndNil(m_pGrayedGlyph);
    FreeAndNil(m_pDisabledUncheckedGlyph);
    FreeAndNil(m_pDisabledCheckedGlyph);
    FreeAndNil(m_pDisabledGrayedGlyph);
    FreeAndNil(m_pTargetsCollection);

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGCheckBoxStyle.GetUncheckedGlyph: TPicture;
begin
    Result := m_pUncheckedGlyph.Picture;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.SetUncheckedGlyph(pPicture: TPicture);
begin
    m_pUncheckedGlyph.Picture := pPicture;
end;
//---------------------------------------------------------------------------
function TWSVGCheckBoxStyle.GetCheckedGlyph: TPicture;
begin
    Result := m_pCheckedGlyph.Picture;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.SetCheckedGlyph(pPicture: TPicture);
begin
    m_pCheckedGlyph.Picture := pPicture;
end;
//---------------------------------------------------------------------------
function TWSVGCheckBoxStyle.GetGrayedGlyph: TPicture;
begin
    Result := m_pGrayedGlyph.Picture;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.SetGrayedGlyph(pPicture: TPicture);
begin
    m_pGrayedGlyph.Picture := pPicture;
end;
//---------------------------------------------------------------------------
function TWSVGCheckBoxStyle.GetDisabledUncheckedGlyph: TPicture;
begin
    Result := m_pDisabledUncheckedGlyph.Picture;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.SetDisabledUncheckedGlyph(pPicture: TPicture);
begin
    m_pDisabledUncheckedGlyph.Picture := pPicture;
end;
//---------------------------------------------------------------------------
function TWSVGCheckBoxStyle.GetDisabledCheckedGlyph: TPicture;
begin
    Result := m_pDisabledCheckedGlyph.Picture;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.SetDisabledCheckedGlyph(pPicture: TPicture);
begin
    m_pDisabledCheckedGlyph.Picture := pPicture;
end;
//---------------------------------------------------------------------------
function TWSVGCheckBoxStyle.GetDisabledGrayedGlyph: TPicture;
begin
    Result := m_pDisabledGrayedGlyph.Picture;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.SetDisabledGrayedGlyph(pPicture: TPicture);
begin
    m_pDisabledGrayedGlyph.Picture := pPicture;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.SetTargetsCollection(pCollection: IWCheckBoxCollection);
begin
    m_pTargetsCollection.Assign(pCollection);

    // hook all controls existing in the collection
    HookControlsFromCollection;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.SetAll(value: Boolean);
var
    prevAll: Boolean;
begin
    prevAll := All;

    SetAll(value, TWSVGCheckBoxStyle, TCheckBox);

    // hook all controls existing in the collection
    if (prevAll <> All) then
        HookControlsFromCollection;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.Loaded;
begin
    inherited Loaded;

    // hook all controls existing in the collection
    HookControlsFromCollection;

    // update the animation status
    DoUpdateAnimStatus;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.HookControlsFromCollection(allowCleanup: Boolean;
        pItemToIgnore: TCollectionItem);
var
    pCheckBoxCollectionItem: IWCheckBoxCollectionItem;
    count, i:                NativeInt;
begin
    // don't do that if the "All" property is enabled
    if (All) then
        Exit;

    // unhook and clear all previous targets
    m_pHooks.Clear;

    // get checkbox item count
    count := m_pTargetsCollection.Count;

    // revert iterate through collection items
    for i := count - 1 downto 0 do
    begin
        // is a checkbox item?
        if (not (m_pTargetsCollection.Items[i] is IWCheckBoxCollectionItem)) then
            continue;

        // get checkbox item
        pCheckBoxCollectionItem := m_pTargetsCollection.Items[i] as IWCheckBoxCollectionItem;
        Assert(Assigned(pCheckBoxCollectionItem));

        // is item empty and not currently removing?
        if (allowCleanup and not Assigned(pCheckBoxCollectionItem.m_pCheckBox)) then
        begin
            // remove it from list
            m_pTargetsCollection.Delete(i);
            continue;
        end;

        // hook the target
        if (not Assigned(pItemToIgnore) or (m_pTargetsCollection.Items[i] <> pItemToIgnore)) then
            AddTarget(pCheckBoxCollectionItem.m_pCheckBox);
    end;

    InvalidateAllTargets;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.DoSaveOriginalSize;
begin
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.DoApplyDPIChange(oldDPI, newDPI: Integer);
begin
end;
//---------------------------------------------------------------------------
function TWSVGCheckBoxStyle.DoProcessAdvancedPaint(pTarget: TWinControl): Boolean;
var
    pCheckBox: TCheckBox;
begin
    // target should be a checkbox
    if (not (pTarget is TCheckBox)) then
        Exit(False);

    // get target checkbox
    pCheckBox := pTarget as TCheckBox;

    // is target checkbox disabled?
    {$if CompilerVersion <= 24}
        // not really correct, but Enabled property doesn't exist in base classes for this compiler version
        if (not pCheckBox.Enabled or (Assigned(pCheckBox.Action) and (pCheckBox.Action is TAction)
                and not (pCheckBox.Action as TAction).Enabled))
    {$else}
        if (not pCheckBox.Enabled or (Assigned(pCheckBox.Action) and (pCheckBox.Action is TContainedAction)
                and not (pCheckBox.Action as TContainedAction).Enabled))
    {$ifend}
    then
    begin
        // search for checkbox state
        case (pCheckBox.State) of
            cbUnchecked: Exit(not m_pDisabledUncheckedGlyph.IsEmpty);
            cbChecked:   Exit(not m_pDisabledCheckedGlyph.IsEmpty);
            cbGrayed:    Exit(not m_pDisabledGrayedGlyph.IsEmpty);
        end;

        Exit(False);
    end;

    // search for checkbox state
    case (pCheckBox.State) of
        cbUnchecked: Exit(not m_pUncheckedGlyph.IsEmpty);
        cbChecked:   Exit(not m_pCheckedGlyph.IsEmpty);
        cbGrayed:    Exit(not m_pGrayedGlyph.IsEmpty);
    end;

    Result := False;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.DoAddTarget(pControl: TControl);
begin
    // is a checkbox?
    if (not(pControl is TCheckBox)) then
        Exit;

    // add it to the skin engine
    AddTarget(pControl as TWinControl);
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.DoUpdateAnimStatus;
begin
    if (not Animate) then
        Exit;

    m_pUncheckedGlyph.UpdateAnimStatus(Animate);
    m_pCheckedGlyph.UpdateAnimStatus(Animate);
    m_pGrayedGlyph.UpdateAnimStatus(Animate);
    m_pDisabledUncheckedGlyph.UpdateAnimStatus(Animate);
    m_pDisabledCheckedGlyph.UpdateAnimStatus(Animate);
    m_pDisabledGrayedGlyph.UpdateAnimStatus(Animate);
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.Draw(hDC: THandle; pTarget: TWinControl);
var
    pCheckBox:      TCheckBox;
    bgRect, cbRect: TRect;
begin
    // target should be a checkbox
    if (not Assigned(pTarget) or not (pTarget is TCheckBox)) then
        Exit;

    if (not Assigned(m_pCanvas)) then
        Exit;

    // get target checkbox and prepare the target canvas
    pCheckBox        := pTarget as TCheckBox;
    m_pCanvas.Handle := hDC;

    // calculate the checkbox rects
    CalculateCheckGlyphRects(pCheckBox, pCheckBox.Alignment, bgRect, cbRect);

    // is target checkbox disabled?
    {$if CompilerVersion <= 24}
        // not really correct, but Enabled property doesn't exist in base classes for this compiler version
        if (not pCheckBox.Enabled or (Assigned(pCheckBox.Action) and (pCheckBox.Action is TAction)
                and not (pCheckBox.Action as TAction).Enabled))
    {$else}
        if (not pCheckBox.Enabled or (Assigned(pCheckBox.Action) and (pCheckBox.Action is TContainedAction)
                and not (pCheckBox.Action as TContainedAction).Enabled))
    {$ifend}
    then
    begin
        // search for checkbox state
        case (pCheckBox.State) of
            cbUnchecked:
            begin
                // checkbox is unchecked, paint the unchecked glyph
                if (m_pDisabledUncheckedGlyph.IsEmpty) then
                    Exit;

                FillBg(bgRect, pCheckBox, m_pCanvas);
                m_pCanvas.StretchDraw(cbRect, m_pDisabledUncheckedGlyph.Picture.Graphic);
            end;

            cbChecked:
            begin
                // checkbox is checked, paint the checked glyph
                if (m_pDisabledCheckedGlyph.IsEmpty) then
                    Exit;

                FillBg(bgRect, pCheckBox, m_pCanvas);
                m_pCanvas.StretchDraw(cbRect, m_pDisabledCheckedGlyph.Picture.Graphic);
            end;

            cbGrayed:
            begin
                // checkbox is grayed, paint the grayed glyph
                if (m_pDisabledGrayedGlyph.IsEmpty) then
                    Exit;

                FillBg(bgRect, pCheckBox, m_pCanvas);
                m_pCanvas.StretchDraw(cbRect, m_pDisabledGrayedGlyph.Picture.Graphic);
            end;
        end;

        Exit;
    end;

    // search for checkbox state
    case (pCheckBox.State) of
        cbUnchecked:
        begin
            // checkbox is unchecked, paint the unchecked glyph
            if (m_pUncheckedGlyph.IsEmpty) then
                Exit;

            FillBg(bgRect, pCheckBox, m_pCanvas);
            m_pCanvas.StretchDraw(cbRect, m_pUncheckedGlyph.Picture.Graphic);
        end;

        cbChecked:
        begin
            // checkbox is checked, paint the checked glyph
            if (m_pCheckedGlyph.IsEmpty) then
                Exit;

            FillBg(bgRect, pCheckBox, m_pCanvas);
            m_pCanvas.StretchDraw(cbRect, m_pCheckedGlyph.Picture.Graphic);
        end;

        cbGrayed:
        begin
            // checkbox is grayed, paint the grayed glyph
            if (m_pGrayedGlyph.IsEmpty) then
                Exit;

            FillBg(bgRect, pCheckBox, m_pCanvas);
            m_pCanvas.StretchDraw(cbRect, m_pGrayedGlyph.Picture.Graphic);
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.OnFreeTarget(pTarget: TWinControl);
var
    pCheckBoxCollectionItem: IWCheckBoxCollectionItem;
    count, i:                NativeInt;
begin
    // get checkbox item count
    count := m_pTargetsCollection.Count;

    // revert iterate through collection items
    for i := count - 1 downto 0 do
    begin
        // is a checkbox item?
        if (not (m_pTargetsCollection.Items[i] is IWCheckBoxCollectionItem)) then
            continue;

        // get checkbox item
        pCheckBoxCollectionItem := m_pTargetsCollection.Items[i] as IWCheckBoxCollectionItem;
        Assert(Assigned(pCheckBoxCollectionItem));

        // item contains the freeing target?
        if (pCheckBoxCollectionItem.m_pCheckBox = pTarget) then
            // delete it from list
            m_pTargetsCollection.Delete(i);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGCheckBoxStyle.OnCollectionChanged(pSender: TObject; pItem: TCollectionItem;
        message: IECollectionChangedMsg);
var
    pCheckBoxCollectionItem: IWCheckBoxCollectionItem;
begin
    if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then
        Exit;

    // hook all controls existing in the collection. NOTE the collection should never be cleaned
    // while collection is changing, and the deleting item should not be hooked
    case (message) of
        IE_CC_ItemDeleting,
        IE_CC_ItemExtracting:
        begin
            HookControlsFromCollection(False, pItem);

            // is item assigned and is a checkbox item?
            if (Assigned(pItem) and (pItem is IWCheckBoxCollectionItem)) then
            begin
                // get checkbox item
                pCheckBoxCollectionItem := pItem as IWCheckBoxCollectionItem;

                // invalidate the deleting checkbox if assigned
                if (Assigned(pCheckBoxCollectionItem.m_pCheckBox)) then
                    pCheckBoxCollectionItem.m_pCheckBox.Invalidate;
            end;
        end;

        IE_CC_ItemValueChanged: HookControlsFromCollection(False);
    end;
end;
//---------------------------------------------------------------------------

end.
