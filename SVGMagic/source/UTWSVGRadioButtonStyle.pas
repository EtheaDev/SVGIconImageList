{**
 @abstract(@name provides a class that allows the radiobutton glyphs to be overriden by SVG.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGRadioButtonStyle;

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
     Style to apply to radiobutton
    }
    TWSVGRadioButtonStyle = class(TWSVGComponentStyle)
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
             Radiobutton collection item
            }
            IWRadioButtonCollectionItem = class (TCollectionItem)
                private
                    m_pRadioButton: TRadioButton;

                protected
                    {**
                     Set the radiobutton owned by the item
                     @param(pRadioButton Radiobutton to set)
                    }
                    procedure SetRadioButton(const pRadioButton: TRadioButton); virtual;

                    {**
                     Get the name to show in the collection
                     @returns(The display name)
                    }
                    function GetDisplayName: string; override;

                published
                    {**
                     Get or set the radiobutton owned by the item
                    }
                    property RadioButton: TRadioButton read m_pRadioButton write SetRadioButton;
            end;

            {**
             Radiobutton collection (can be shown in design time)
            }
            IWRadioButtonCollection = class (TCollection)
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
            m_pDisabledUncheckedGlyph: TWSVGComponentStyle.IWGlyph;
            m_pDisabledCheckedGlyph:   TWSVGComponentStyle.IWGlyph;
            m_pTargetsCollection:      IWRadioButtonCollection;

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
             Set the target collection
             @param(pCollection New target collection to set)
            }
            procedure SetTargetsCollection(pCollection: IWRadioButtonCollection); virtual;

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
             Get or set the disabled unchecked state glyph content
            }
            property DisabledUncheckedGlyph: TPicture read GetDisabledUncheckedGlyph write SetDisabledUncheckedGlyph;

            {**
             Get or set the disabled checked state glyph content
            }
            property DisabledCheckedGlyph: TPicture read GetDisabledCheckedGlyph write SetDisabledCheckedGlyph;

            {**
             Get or set the target component collection
            }
            property Targets: IWRadioButtonCollection read m_pTargetsCollection write SetTargetsCollection;
    end;

implementation
//---------------------------------------------------------------------------
// TWSVGRadioButtonStyle.IWRadioButtonCollectionItem
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.IWRadioButtonCollectionItem.SetRadioButton(const pRadioButton: TRadioButton);
var
    pItem:                  TCollectionItem;
    pRadioButtonItem:       IWRadioButtonCollectionItem;
    pRadioButtonCollection: IWRadioButtonCollection;
    pPrevRadioButton:       TRadioButton;
begin
    // iterate through collection items
    for pItem in Collection do
    begin
        // found itself?
        if (pItem = Self) then
            continue;

        // is a radiobutton collection item?
        if (not (pItem is IWRadioButtonCollectionItem)) then
            continue;

        // get radiobutton collection item
        pRadioButtonItem := pItem as IWRadioButtonCollectionItem;

        // another collection item with the same value already exists?
        if (pRadioButtonItem.m_pRadioButton = pRadioButton) then
            raise Exception.Create('This item already exists in the list.');
    end;

    pPrevRadioButton := m_pRadioButton;
    m_pRadioButton   := pRadioButton;

    // notify that collection has changed
    if (Collection is IWRadioButtonCollection) then
    begin
        pRadioButtonCollection := Collection as IWRadioButtonCollection;
        pRadioButtonCollection.CollectionChanged(Self, IE_CC_ItemValueChanged);
    end;

    // update previous radiobutton to paint it without the removed style
    if (Assigned(pPrevRadioButton)) then
        pPrevRadioButton.Invalidate;
end;
//---------------------------------------------------------------------------
function TWSVGRadioButtonStyle.IWRadioButtonCollectionItem.GetDisplayName: string;
begin
    if (not Assigned(m_pRadioButton)) then
        Exit(inherited);

    Result := m_pRadioButton.Name;
end;
//---------------------------------------------------------------------------
// TWSVGRadioButtonStyle.IWRadioButtonCollection
//---------------------------------------------------------------------------
constructor TWSVGRadioButtonStyle.IWRadioButtonCollection.Create(pCollectionOwner: TComponent);
begin
    inherited Create(IWRadioButtonCollectionItem);

    m_pCollectionOwner := pCollectionOwner;
    m_fOnChanged       := nil;
end;
//---------------------------------------------------------------------------
destructor TWSVGRadioButtonStyle.IWRadioButtonCollection.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGRadioButtonStyle.IWRadioButtonCollection.GetOwner: TPersistent;
begin
    Result := m_pCollectionOwner;
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.IWRadioButtonCollection.Notify(pItem: TCollectionItem;
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
procedure TWSVGRadioButtonStyle.IWRadioButtonCollection.CollectionChanged(pItem: TCollectionItem;
        message: TWSVGRadioButtonStyle.IECollectionChangedMsg);
begin
    if (Assigned(m_fOnChanged)) then
        m_fOnChanged(Self, pItem, message);
end;
//---------------------------------------------------------------------------
// TWSVGRadioButtonStyle
//---------------------------------------------------------------------------
constructor TWSVGRadioButtonStyle.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_pUncheckedGlyph         := IWGlyph.Create(Self);
    m_pCheckedGlyph           := IWGlyph.Create(Self);
    m_pDisabledUncheckedGlyph := IWGlyph.Create(Self);
    m_pDisabledCheckedGlyph   := IWGlyph.Create(Self);
    m_pTargetsCollection      := IWRadioButtonCollection.Create(Self);

    // set internal events
    m_pTargetsCollection.OnChanged := OnCollectionChanged;
end;
//---------------------------------------------------------------------------
destructor TWSVGRadioButtonStyle.Destroy;
begin
    FreeAndNil(m_pUncheckedGlyph);
    FreeAndNil(m_pCheckedGlyph);
    FreeAndNil(m_pDisabledUncheckedGlyph);
    FreeAndNil(m_pDisabledCheckedGlyph);
    FreeAndNil(m_pTargetsCollection);

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGRadioButtonStyle.GetUncheckedGlyph: TPicture;
begin
    Result := m_pUncheckedGlyph.Picture;
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.SetUncheckedGlyph(pPicture: TPicture);
begin
    m_pUncheckedGlyph.Picture := pPicture;
end;
//---------------------------------------------------------------------------
function TWSVGRadioButtonStyle.GetCheckedGlyph: TPicture;
begin
    Result := m_pCheckedGlyph.Picture;
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.SetCheckedGlyph(pPicture: TPicture);
begin
    m_pCheckedGlyph.Picture := pPicture;
end;
//---------------------------------------------------------------------------
function TWSVGRadioButtonStyle.GetDisabledUncheckedGlyph: TPicture;
begin
    Result := m_pDisabledUncheckedGlyph.Picture;
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.SetDisabledUncheckedGlyph(pPicture: TPicture);
begin
    m_pDisabledUncheckedGlyph.Picture := pPicture;
end;
//---------------------------------------------------------------------------
function TWSVGRadioButtonStyle.GetDisabledCheckedGlyph: TPicture;
begin
    Result := m_pDisabledCheckedGlyph.Picture;
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.SetDisabledCheckedGlyph(pPicture: TPicture);
begin
    m_pDisabledCheckedGlyph.Picture := pPicture;
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.SetTargetsCollection(pCollection: IWRadioButtonCollection);
begin
    m_pTargetsCollection.Assign(pCollection);

    // hook all controls existing in the collection
    HookControlsFromCollection;
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.SetAll(value: Boolean);
var
    prevAll: Boolean;
begin
    prevAll := All;

    SetAll(value, TWSVGRadioButtonStyle, TRadioButton);

    // hook all controls existing in the collection
    if (prevAll <> All) then
        HookControlsFromCollection;
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.Loaded;
begin
    inherited Loaded;

    // hook all controls existing in the collection
    HookControlsFromCollection;

    // update the animation status
    DoUpdateAnimStatus;
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.HookControlsFromCollection(allowCleanup: Boolean;
        pItemToIgnore: TCollectionItem);
var
    pRadioButtonCollectionItem: IWRadioButtonCollectionItem;
    count, i:                   NativeInt;
begin
    // don't do that if the "All" property is enabled
    if (All) then
        Exit;

    // unhook and clear all previous targets
    m_pHooks.Clear;

    // get radiobutton item count
    count := m_pTargetsCollection.Count;

    // revert iterate through collection items
    for i := count - 1 downto 0 do
    begin
        // is a radiobutton item?
        if (not (m_pTargetsCollection.Items[i] is IWRadioButtonCollectionItem)) then
            continue;

        // get radiobutton item
        pRadioButtonCollectionItem := m_pTargetsCollection.Items[i] as IWRadioButtonCollectionItem;
        Assert(Assigned(pRadioButtonCollectionItem));

        // is item empty and not currently removing?
        if (allowCleanup and not Assigned(pRadioButtonCollectionItem.m_pRadioButton)) then
        begin
            // remove it from list
            m_pTargetsCollection.Delete(i);
            continue;
        end;

        // hook the target
        if (not Assigned(pItemToIgnore) or (m_pTargetsCollection.Items[i] <> pItemToIgnore)) then
            AddTarget(pRadioButtonCollectionItem.m_pRadioButton);
    end;

    InvalidateAllTargets;
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.DoSaveOriginalSize;
begin
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.DoApplyDPIChange(oldDPI, newDPI: Integer);
begin
end;
//---------------------------------------------------------------------------
function TWSVGRadioButtonStyle.DoProcessAdvancedPaint(pTarget: TWinControl): Boolean;
var
    pRadioButton: TRadioButton;
begin
    // target should be a radiobutton
    if (not (pTarget is TRadioButton)) then
        Exit(False);

    // get target radiobutton
    pRadioButton := pTarget as TRadioButton;

    // is target radiobutton disabled?
    {$if CompilerVersion <= 24}
        // not really correct, but Enabled property doesn't exist in base classes for this compiler version
        if (not pRadioButton.Enabled or (Assigned(pRadioButton.Action) and (pRadioButton.Action is TAction)
                and not (pRadioButton.Action as TAction).Enabled))
    {$else}
        if (not pRadioButton.Enabled or (Assigned(pRadioButton.Action) and (pRadioButton.Action is TContainedAction)
                and not (pRadioButton.Action as TContainedAction).Enabled))
    {$ifend}
    then
    begin
        // search for radiobutton state
        if (pRadioButton.Checked) then
            Exit(not m_pDisabledCheckedGlyph.IsEmpty);

        Exit(not m_pDisabledUncheckedGlyph.IsEmpty);
    end;

    // search for radiobutton state
    if (pRadioButton.Checked) then
        Exit(not m_pCheckedGlyph.IsEmpty);

    Exit(not m_pUncheckedGlyph.IsEmpty);
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.DoAddTarget(pControl: TControl);
begin
    // is a radio button?
    if (not(pControl is TRadioButton)) then
        Exit;

    // add it to the skin engine
    AddTarget(pControl as TWinControl);
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.DoUpdateAnimStatus;
begin
    if (not Animate) then
        Exit;

    m_pUncheckedGlyph.UpdateAnimStatus(Animate);
    m_pCheckedGlyph.UpdateAnimStatus(Animate);
    m_pDisabledUncheckedGlyph.UpdateAnimStatus(Animate);
    m_pDisabledCheckedGlyph.UpdateAnimStatus(Animate);
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.Draw(hDC: THandle; pTarget: TWinControl);
var
    pRadioButton:   TRadioButton;
    bgRect, cbRect: TRect;
begin
    // target should be a radiobutton
    if (not Assigned(pTarget) or not (pTarget is TRadioButton)) then
        Exit;

    if (not Assigned(m_pCanvas)) then
        Exit;

    // get target radiobutton and prepare the target canvas
    pRadioButton     := pTarget as TRadioButton;
    m_pCanvas.Handle := hDC;

    // calculate the radiobutton rects
    CalculateCheckGlyphRects(pRadioButton, pRadioButton.Alignment, bgRect, cbRect);

    // is target radiobutton disabled?
    {$if CompilerVersion <= 24}
        // not really correct, but Enabled property doesn't exist in base classes for this compiler version
        if (not pRadioButton.Enabled or (Assigned(pRadioButton.Action) and (pRadioButton.Action is TAction)
                and not (pRadioButton.Action as TAction).Enabled))
    {$else}
        if (not pRadioButton.Enabled or (Assigned(pRadioButton.Action) and (pRadioButton.Action is TContainedAction)
                and not (pRadioButton.Action as TContainedAction).Enabled))
    {$ifend}
    then
    begin
        // search for radiobutton state
        if (pRadioButton.Checked) then
        begin
            // radiobutton is checked, paint the checked glyph
            if (m_pDisabledCheckedGlyph.IsEmpty) then
                Exit;

            FillBg(bgRect, pRadioButton, m_pCanvas);
            m_pCanvas.StretchDraw(cbRect, m_pDisabledCheckedGlyph.Picture.Graphic);
        end
        else
        begin
            // radiobutton is unchecked, paint the unchecked glyph
            if (m_pDisabledUncheckedGlyph.IsEmpty) then
                Exit;

            FillBg(bgRect, pRadioButton, m_pCanvas);
            m_pCanvas.StretchDraw(cbRect, m_pDisabledUncheckedGlyph.Picture.Graphic);
        end;

        Exit;
    end;

    // search for radiobutton state
    if (pRadioButton.Checked) then
    begin
        // radiobutton is checked, paint the checked glyph
        if (m_pCheckedGlyph.IsEmpty) then
            Exit;

        FillBg(bgRect, pRadioButton, m_pCanvas);
        m_pCanvas.StretchDraw(cbRect, m_pCheckedGlyph.Picture.Graphic);
    end
    else
    begin
        // radiobutton is unchecked, paint the unchecked glyph
        if (m_pUncheckedGlyph.IsEmpty) then
            Exit;

        FillBg(bgRect, pRadioButton, m_pCanvas);
        m_pCanvas.StretchDraw(cbRect, m_pUncheckedGlyph.Picture.Graphic);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.OnFreeTarget(pTarget: TWinControl);
var
    pRadioButtonCollectionItem: IWRadioButtonCollectionItem;
    count, i:                   NativeInt;
begin
    // get radiobutton item count
    count := m_pTargetsCollection.Count;

    // revert iterate through collection items
    for i := count - 1 downto 0 do
    begin
        // is a radiobutton item?
        if (not (m_pTargetsCollection.Items[i] is IWRadioButtonCollectionItem)) then
            continue;

        // get radiobutton item
        pRadioButtonCollectionItem := m_pTargetsCollection.Items[i] as IWRadioButtonCollectionItem;
        Assert(Assigned(pRadioButtonCollectionItem));

        // item contains the freeing target?
        if (pRadioButtonCollectionItem.m_pRadioButton = pTarget) then
            // delete it from list
            m_pTargetsCollection.Delete(i);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGRadioButtonStyle.OnCollectionChanged(pSender: TObject; pItem: TCollectionItem;
        message: IECollectionChangedMsg);
var
    pRadioButtonCollectionItem: IWRadioButtonCollectionItem;
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

            // is item assigned and is a radiobutton item?
            if (Assigned(pItem) and (pItem is IWRadioButtonCollectionItem)) then
            begin
                // get radiobutton item
                pRadioButtonCollectionItem := pItem as IWRadioButtonCollectionItem;

                // invalidate the deleting radiobutton if assigned
                if (Assigned(pRadioButtonCollectionItem.m_pRadioButton)) then
                    pRadioButtonCollectionItem.m_pRadioButton.Invalidate;
            end;
        end;

        IE_CC_ItemValueChanged: HookControlsFromCollection(False);
    end;
end;
//---------------------------------------------------------------------------

end.
