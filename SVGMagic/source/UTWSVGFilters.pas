{**
 @abstract(@name contains the Scalable Vector Graphics (SVG) effects. An effect is a kind of
                 filter that can be applied while the image is rasterized.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGFilters;
{$I SVGMagic.inc}

interface

uses System.SysUtils,
     System.Generics.Collections,
     {$ifdef USE_VERYSIMPLEXML}
         Xml.VerySimple,
     {$else}
         Xml.XMLIntf,
     {$endif}
     UTWHelpers,
     UTWSVGTags,
     UTWSVGCommon,
     UTWSVGItems,
     UTWSVGAttribute,
     UTWSVGMeasure,
     UTWSVGProperties,
     UTWSVGAnimation;

type
    {**
     SVG filter effect, may be a gaussian blur, a lighting, ...
    }
    TWSVGEffect = class(TWSVGElement)
        public type
            {**
             Filter effect type enumeration
             @value(IE_ET_Unknown Unknown effect)
             @value(IE_ET_Blend Filter for compose two objects together ruled by a certain blending mode)
             @value(IE_ET_ColorMatrix Filter for change colors based on a transformation matrix)
             @value(IE_ET_ComponentTransfer Filter for perform color-component-wise remapping of data
                                            for each pixel)
             @value(IE_ET_Composite Filter for perform the combination of two input images pixel-wise
                                           in image space using one of the Porter-Duff compositing operations)
             @value(IE_ET_ConvolveMatrix Filter for apply a matrix convolution filter effect)
             @value(IE_ET_DiffuseLighting Filter for light an image using the alpha channel as a bump map)
             @value(IE_ET_DisplacementMap Filter for use the pixel values from the image from in2
                                          to spatially displace the image from in)
             @value(IE_ET_Flood Filter for fill the filter subregion with the color and opacity
                                defined by flood-color and flood-opacity)
             @value(IE_ET_GaussianBlur Filter for blur the input image by the amount specified in
                                       stdDeviation, which defines the bell-curve)
             @value(IE_ET_Image Filter for fetch image data from an external source and provides the
                                pixel data as output (meaning if the external source is an SVG image,
                                it is rasterized))
             @value(IE_ET_Merge Filter for allow filter effects to be applied concurrently instead of sequentially)
             @value(IE_ET_Morphology Filter for erode or dilate the input image)
             @value(IE_ET_Offset Filter for offset the input image)
             @value(IE_ET_SpecularLighting Filter for light a source graphic using the alpha channel
                                           as a bump map)
             @value(IE_ET_Tile Filter to fill a target rectangle with a repeated, tiled pattern of
                               an input image)
             @value(IE_ET_Turbulence Filter to creates an image using the Perlin turbulence function)
             @value(IE_ET_DistantLight Filter to define a distant light source that can be used
                                       within a lighting filter primitive)
             @value(IE_ET_PointLight Filter to define a light source which allows to create a point light effect)
             @value(IE_ET_SpotLight Filter to define a light source which allows to create a spotlight effect)
            }
            IEType =
            (
                IE_ET_Unknown,
                IE_ET_Blend,
                IE_ET_ColorMatrix,
                IE_ET_ComponentTransfer,
                IE_ET_Composite,
                IE_ET_ConvolveMatrix,
                IE_ET_DiffuseLighting,
                IE_ET_DisplacementMap,
                IE_ET_Flood,
                IE_ET_GaussianBlur,
                IE_ET_Image,
                IE_ET_Merge,
                IE_ET_Morphology,
                IE_ET_Offset,
                IE_ET_SpecularLighting,
                IE_ET_Tile,
                IE_ET_Turbulence,
                IE_ET_DistantLight,
                IE_ET_PointLight,
                IE_ET_SpotLight
            );

        private
            m_Type: IEType;

        public
            {**
             Constructor
             @param(pParent Parent item, orphan or root if @nil)
             @param(pOptions SVG options)
            }
            constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions); override;

            {**
             Constructor
             @param(pParent Parent item, orphan or root if @nil)
             @param(effectType Effect type)
             @param(pOptions SVG options)
            }
            constructor CreateType(pParent: TWSVGItem; effectType: IEType; pOptions: PWSVGOptions); virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Assign (i.e. copy) content from another item
             @param(pOther Other item to copy from)
            }
            procedure Assign(const pOther: TWSVGItem); override;

            {**
             Clear
            }
            procedure Clear; override;

            {**
             Read SVG element from xml
             @param(pNode Xml node containing SVG element to read)
             @returns(@true on success, otherwise @false)
            }
            {$ifdef USE_VERYSIMPLEXML}
                function Read(const pNode: TXMLNode): Boolean; override;
            {$else}
                function Read(const pNode: IXMLNode): Boolean; override;
            {$endif}
    end;

    {**
     Scalable Vector Graphics (SVG) gaussian blur effect
    }
    TWSVGGaussianBlur = class(TWSVGEffect)
        public
            {**
             Constructor
             @param(pParent Parent item, orphan or root if @nil)
             @param(pOptions SVG options)
            }
            constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions); override;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Assign (i.e. copy) content from another item
             @param(pOther Other item to copy from)
            }
            procedure Assign(const pOther: TWSVGItem); override;

            {**
             Create new element instance
             @param(pParent Parent item, orphan or root if @nil)
             @returns(Element instance)
            }
            function CreateInstance(pParent: TWSVGItem): TWSVGElement; override;

            {**
             Read SVG element from xml
             @param(pNode Xml node containing SVG element to read)
             @returns(@true on success, otherwise @false)
            }
            {$ifdef USE_VERYSIMPLEXML}
                function Read(const pNode: TXMLNode): Boolean; override;
            {$else}
                function Read(const pNode: IXMLNode): Boolean; override;
            {$endif}
    end;

    {**
     SVG filter, describes a filter to apply to finalize a drawing
    }
    TWSVGFilter = class(TWSVGElement)
        public type
            IEffects = TObjectList<TWSVGElement>;

        private
            {**
             Delete and clear all data
            }
            procedure DelAndClear;

        protected
            m_pEffects: IEffects;

            {**
             Get effect at index
             @param(index Index at which effect should be get)
             @returns(Effect)
             @raises(Exception if index is out of bounds)
            }
            function GetEffect(index: Integer): TWSVGEffect; virtual;

            {**
             Get effect count
             @returns(Effect count)
            }
            function GetEffectCount: NativeUInt; virtual;

        public
            {**
             Constructor
             @param(pParent Parent item, orphan or root if @nil)
             @param(pOptions SVG options)
            }
            constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions); override;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Assign (i.e. copy) content from another item
             @param(pOther Other item to copy from)
            }
            procedure Assign(const pOther: TWSVGItem); override;

            {**
             Clear
            }
            procedure Clear; override;

            {**
             Create new element instance
             @param(pParent Parent item, orphan or root if @nil)
             @returns(Element instance)
            }
            function CreateInstance(pParent: TWSVGItem): TWSVGElement; override;

            {**
             Read SVG element from xml
             @param(pNode Xml node containing SVG element to read)
             @returns(@true on success, otherwise @false)
            }
            {$ifdef USE_VERYSIMPLEXML}
                function Read(const pNode: TXMLNode): Boolean; override;
            {$else}
                function Read(const pNode: IXMLNode): Boolean; override;
            {$endif}

            {**
             Log content
             @param(margin Margin length in chars)
            }
            procedure Log(margin: Cardinal); override;

            {**
             Print content to string
             @param(margin Margin length in chars)
             @returns(Content)
            }
            function Print(margin: Cardinal): UnicodeString; override;

            {**
             Get xml formatted string
             @returns(String)
            }
            function ToXml: UnicodeString; override;

        public
            {**
             Get effect at index. Example: effect := Effects[0];
             @br @bold(NOTE) An exception will be raised if index is out of bounds
            }
            property Effects[index: Integer]: TWSVGEffect read GetEffect;

            {**
             Get gradient stop count
            }
            property EffectCount: NativeUInt read GetEffectCount;
    end;

implementation
//---------------------------------------------------------------------------
// TWSVGEffect
//---------------------------------------------------------------------------
constructor TWSVGEffect.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Type := IE_ET_Unknown;
end;
//---------------------------------------------------------------------------
constructor TWSVGEffect.CreateType(pParent: TWSVGItem; effectType: IEType; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_Type := effectType;
end;
//---------------------------------------------------------------------------
destructor TWSVGEffect.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGEffect.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGEffect;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGEffect)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGEffect;

    // copy data from source
    m_Type := pSource.m_Type;
end;
//---------------------------------------------------------------------------
procedure TWSVGEffect.Clear;
begin
    inherited Clear;

    m_Type := IE_ET_Unknown;
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGEffect.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGEffect.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pID:  TWSVGPropText;
begin
    // no element?
    if (not Assigned(pNode)) then
        Exit(False);

    pID := nil;

    try
        pID := TWSVGPropText.Create(Self, m_pOptions);

        // read identifier
        if (pID.Read(C_SVG_Prop_ID, pNode)) then
        begin
            // expose the identifier
            ItemID := pID.Value;

            m_pProperties.Add(pID);
            pID := nil;
        end;
    finally
        pID.Free;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
// TWSVGGaussianBlur
//---------------------------------------------------------------------------
constructor TWSVGGaussianBlur.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited CreateType(pParent, TWSVGEffect.IEType.IE_ET_GaussianBlur, pOptions);

    ItemName := C_SVG_Filter_Gaussian_Blur;
end;
//---------------------------------------------------------------------------
destructor TWSVGGaussianBlur.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGGaussianBlur.Assign(const pOther: TWSVGItem);
begin
    inherited Assign(pOther);
end;
//---------------------------------------------------------------------------
function TWSVGGaussianBlur.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGGaussianBlur.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGGaussianBlur.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGGaussianBlur.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    pStdDeviation:  TWSVGAttribute<Single>;
begin
    // no element?
    if (not Assigned(pNode)) then
        Exit(False);

    if (not inherited Read(pNode)) then
        Exit(False);

    pStdDeviation := nil;

    try
        pStdDeviation := TWSVGAttribute<Single>.Create(Self, m_pOptions);

        // read std deviation (optional)
        if (pStdDeviation.Read(C_SVG_Filter_STD_Deviation, pNode)) then
        begin
            m_pProperties.Add(pStdDeviation);
            pStdDeviation := nil;
        end;
    finally
        pStdDeviation.Free;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
// TWSVGFilter
//---------------------------------------------------------------------------
constructor TWSVGFilter.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    ItemName   := C_SVG_Tag_Filter;
    m_pEffects := IEffects.Create;
end;
//---------------------------------------------------------------------------
destructor TWSVGFilter.Destroy;
begin
    DelAndClear;

    m_pEffects.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGFilter.DelAndClear;
begin
    // clear all effects. NOTE as a TObjectList is used, the list will take care of freeing all
    // objects it contains, for that an explicit Free() on each item isn't required
    m_pEffects.Clear;
end;
//---------------------------------------------------------------------------
function TWSVGFilter.GetEffect(index: Integer): TWSVGEffect;
begin
    if (index >= m_pEffects.Count) then
        raise Exception.Create('Index is out of bounds');

    Assert(m_pEffects[index] is TWSVGEffect);

    Result := m_pEffects[index] as TWSVGEffect;
end;
//---------------------------------------------------------------------------
function TWSVGFilter.GetEffectCount: NativeUInt;
begin
    Result := m_pEffects.Count;
end;
//---------------------------------------------------------------------------
procedure TWSVGFilter.Assign(const pOther: TWSVGItem);
var
    pSource:             TWSVGFilter;
    pEffect, pNewEffect: TWSVGElement;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGFilter)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGFilter;

    // iterate through properties to copy
    for pEffect in pSource.m_pEffects do
    begin
        pNewEffect := nil;

        try
            // copy property from source
            pNewEffect := pEffect.CreateInstance(Self);
            pNewEffect.Assign(pEffect);
            m_pEffects.Add(pNewEffect);
            pNewEffect := nil;
        finally
            pNewEffect.Free;
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGFilter.Clear;
begin
    inherited Clear;

    DelAndClear;
end;
//---------------------------------------------------------------------------
function TWSVGFilter.CreateInstance(pParent: TWSVGItem): TWSVGElement;
begin
    Result := TWSVGFilter.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    function TWSVGFilter.Read(const pNode: TXMLNode): Boolean;
{$else}
    function TWSVGFilter.Read(const pNode: IXMLNode): Boolean;
{$endif}
var
    {$ifdef USE_VERYSIMPLEXML}
        pChild:                  TXMLNode;
    {$else}
        pChild:                  IXMLNode;
    {$endif}
    pID:                         TWSVGPropText;
    pX, pY, pWidth, pHeight:     TWSVGMeasure<Single>;
    pFilterUnit, pPrimitiveUnit: TWSVGPropUnit;
    pHRef, pXLinkHRef:           TWSVGPropLink;
    //pAnimation:                  TWSVGAnimation;
    pGaussianBlur:               TWSVGGaussianBlur;
    name:                        UnicodeString;
    i:                           NativeUInt;
    success:                     Boolean;
begin
    // no element?
    if (not Assigned(pNode)) then
        Exit(False);

    pID := nil;

    try
        pID := TWSVGPropText.Create(Self, m_pOptions);

        // read identifier
        if (pID.Read(C_SVG_Prop_ID, pNode)) then
        begin
            // expose the identifier
            ItemID := pID.Value;

            m_pProperties.Add(pID);
            pID := nil;
        end;
    finally
        pID.Free;
    end;

    // normally a filter should always contain an identifier, otherwise it cannot be get and used
    if (Length(ItemID) = 0) then
    begin
        TWLogHelper.LogToCompiler('Read filter - FAILED - identifier is empty');
        Exit(False);
    End;

    pX := nil;

    try
        pX := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read x position
        if (not pX.Read(C_SVG_Prop_X, pNode)) then
        begin
            // set default value if position was omitted
            pX.ItemName    := C_SVG_Prop_X;
            pX.MeasureUnit := IEUnit.IE_UN_Percent;
            pX.Value       := 0.0;
        end;

        m_pProperties.Add(pX);
        pX := nil;
    finally
        pX.Free;
    end;

    pY := nil;

    try
        pY := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read y position
        if (not pY.Read(C_SVG_Prop_Y, pNode)) then
        begin
            // set default value if position was omitted
            pY.ItemName    := C_SVG_Prop_Y;
            pY.MeasureUnit := IEUnit.IE_UN_Percent;
            pY.Value       := 0.0;
        end;

        m_pProperties.Add(pY);
        pY := nil;
    finally
        pY.Free;
    end;

    pWidth := nil;

    try
        pWidth := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read width
        if (not pWidth.Read(C_SVG_Prop_Width, pNode)) then
        begin
            // set default value if position was omitted
            pWidth.ItemName    := C_SVG_Prop_Width;
            pWidth.MeasureUnit := IEUnit.IE_UN_Percent;
            pWidth.Value       := 0.0;
        end;

        m_pProperties.Add(pWidth);
        pWidth := nil;
    finally
        pWidth.Free;
    end;

    pHeight := nil;

    try
        pHeight := TWSVGMeasure<Single>.Create(Self, m_pOptions, False);

        // read height
        if (not pHeight.Read(C_SVG_Prop_Height, pNode)) then
        begin
            // set default value if position was omitted
            pHeight.ItemName    := C_SVG_Prop_Height;
            pHeight.MeasureUnit := IEUnit.IE_UN_Percent;
            pHeight.Value       := 0.0;
        end;

        m_pProperties.Add(pHeight);
        pHeight := nil;
    finally
        pHeight.Free;
    end;

    pFilterUnit := nil;

    try
        pFilterUnit := TWSVGPropUnit.Create(Self, m_pOptions);

        // read filter unit (optional)
        if (not pFilterUnit.Read(C_SVG_Filter_STD_Deviation, pNode)) then
        begin
            // set default value if filter unit was omitted
            pFilterUnit.ItemName := C_SVG_Filter_Units;
            pFilterUnit.UnitType := TWSVGPropUnit.IEType.IE_UT_ObjectBoundingBox;
        end;

        m_pProperties.Add(pFilterUnit);
        pFilterUnit := nil;
    finally
        pFilterUnit.Free;
    end;

    pPrimitiveUnit := nil;

    try
        pPrimitiveUnit := TWSVGPropUnit.Create(Self, m_pOptions);

        // read primitive unit (optional)
        if (not pPrimitiveUnit.Read(C_SVG_Filter_STD_Deviation, pNode)) then
        begin
            // set default value if primitive unit was omitted
            pPrimitiveUnit.ItemName := C_SVG_Primitive_Units;
            pPrimitiveUnit.UnitType := TWSVGPropUnit.IEType.IE_UT_UserSpaceOnUse;
        end;

        m_pProperties.Add(pPrimitiveUnit);
        pPrimitiveUnit := nil;
    finally
        pPrimitiveUnit.Free;
    end;

    pHRef := nil;

    try
        pHRef := TWSVGPropLink.Create(Self, m_pOptions);

        // read external reference to another object (optional)
        if (pHRef.Read(C_SVG_Prop_HRef, pNode)) then
        begin
            m_pProperties.Add(pHRef);
            pHRef := nil;
        end;
    finally
        pHRef.Free;
    end;

    pXLinkHRef := nil;

    try
        pXLinkHRef := TWSVGPropLink.Create(Self, m_pOptions);

        // read external link/reference to another object (optional). NOTE this kind of links are
        // deprecated in the SVG2 standards, but may still appear in several SVG files
        if (pXLinkHRef.Read(C_SVG_Prop_XLink_HRef, pNode)) then
        begin
            m_pProperties.Add(pXLinkHRef);
            pXLinkHRef := nil;
        end;
    finally
        pXLinkHRef.Free;
    end;

    Result := True;

    for i := 0 to pNode.ChildNodes.Count - 1 do
    begin
        pChild := pNode.ChildNodes.Get(i);

        if (not Assigned(pChild)) then
            continue;

        // get element name
        name := pChild.NodeName;

        // todo FIXME -cfeature -oJean: Effects are theorically animatable. Search for a concrete
        //                              example, and show how implemented in TWSVGContainer
        {
        // is an animation?
        if ((name = C_SVG_Tag_Animate) or (name = C_SVG_Tag_Animate_Color)
                or (name = C_SVG_Tag_Animate_Transform) or (name = C_SVG_Tag_Animate_Motion))
        then
        begin
            pAnimation := TWSVGAnimation.Create(Self, m_pOptions);

            try
                // read child element as animation
                if (pAnimation.Read(pChild)) then
                begin
                    // add newly read animation to list
                    m_Animations.Add(pAnimation);
                    pAnimation := nil;
                end;
            finally
                pAnimation.Free;
            end;

            continue;
        end;
        }

        // search for effect to read
        if (name = C_SVG_Filter_Gaussian_Blur) then
        begin
            pGaussianBlur := nil;

            try
                // read gaussian blur
                pGaussianBlur := TWSVGGaussianBlur.Create(Self, m_pOptions);
                success       := pGaussianBlur.Read(pChild);

                if (Result) then
                begin
                    m_pEffects.Add(pGaussianBlur);
                    pGaussianBlur := nil;
                end;
            finally
                pGaussianBlur.Free;
            end;

            Result := success and Result;
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGFilter.Log(margin: Cardinal);
var
    pEffect: TWSVGElement;
begin
    inherited Log(margin);

    // iterate through effects to log
    for pEffect in m_pEffects do
        // log child effect
        pEffect.Log(margin);
end;
//---------------------------------------------------------------------------
function TWSVGFilter.Print(margin: Cardinal): UnicodeString;
var
    pEffect: TWSVGElement;
begin
    Result := inherited Print(margin);

    // iterate through effects to print
    for pEffect in m_pEffects do
        // print child effect
        Result := Result + pEffect.Print(margin);
end;
//---------------------------------------------------------------------------
function TWSVGFilter.ToXml: UnicodeString;
var
    pEffect: TWSVGElement;
begin
    Result := inherited ToXml;

    // iterate through effects to convert to xml
    for pEffect in m_pEffects do
        // write child effect to xml
        Result := Result + pEffect.ToXml();
end;
//---------------------------------------------------------------------------

end.
