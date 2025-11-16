{**
 @abstract(@name provides classes to describe the animations.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGAnimationDescriptor;

interface

uses System.SysUtils,
     System.Math,
     UTWColor,
     UTWVector,
     UTWMatrix,
     UTWDateTime,
     UTWGeometryTools,
     UTWHelpers,
     UTWSVGAnimation;

type
    {**
     SVG animation descriptor, keeps the animation data and provides functions to process them
    }
    TWSVGAnimationDescriptor = class
        public type
            IKeyList = array of Double;

        private
            m_pAnimation:       TWSVGAnimation;
            m_pBegin:           TWSimpleTime;
            m_pEnd:             TWSimpleTime;
            m_pDuration:        TWSimpleTime;
            m_KeySplines:       IKeyList;
            m_KeyTimes:         IKeyList;
            m_GroupCount:       NativeUInt;
            m_ValPerGroupCount: NativeUInt;
            m_RepeatCount:      NativeUInt;
            m_PartialCount:     NativeUInt;
            m_DoLoop:           Boolean;
            m_NegativeBegin:    Boolean;
            m_NegativeEnd:      Boolean;
            m_NegativeDuration: Boolean;
            m_CalcMode:         TWSVGAnimation.IPropCalcMode.IECalcModeType;
            m_AdditiveMode:     TWSVGAnimation.IPropAdditiveMode.IEType;

        protected
            {**
             Build Bezier control points from key splines
             @param(index Group index (a group is a set of 4 values, keySplines count is always a multiple of 4))
             @param(control1 @bold([out]) First Bezier curve control point)
             @param(control2 @bold([out]) Second Bezier curve control point)
             @raises(Exception if index is out of bounds)
            }
            procedure GetBezierControlPointsFromKeySplines(index: NativeUInt; out control1: TWVector2;
                    out control2: TWVector2); virtual;

            {**
             Get progression in time based on a Bezier curve
             @param(index Group index (a group is a set of 4 values, keySplines count is always a multiple of 4))
             @param(position Current position in percent (from 0.0 to 1.0))
             @returns(Progression in percent (from 0.0 to 1.0))
            }
            function GetBezierProgression(index: NativeUInt; position: Double): Double; virtual;

        public
            {**
             Constructor
            }
            constructor Create; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Add animation key spline
             @param(key Key to add)
            }
            procedure AddKeySpline(key: Double); virtual;

            {**
             Add animation key time
             @param(key Key to add)
            }
            procedure AddKeyTime(key: Double); virtual;

        public
            {**
             Get or set the SVG animation descriptor
            }
            property Animation: TWSVGAnimation read m_pAnimation write m_pAnimation;

            {**
             Get begin time
            }
            property BeginTime: TWSimpleTime read m_pBegin;

            {**
             Get end time
            }
            property EndTime: TWSimpleTime read m_pEnd;

            {**
             Get duration
            }
            property Duration: TWSimpleTime read m_pDuration;

            {**
             Get key splines
            }
            property KeySplines: IKeyList read m_KeySplines;

            {**
             Get key times
            }
            property KeyTimes: IKeyList read m_KeyTimes;

            {**
             Get or set the group count
            }
            property GroupCount: NativeUInt read m_GroupCount write m_GroupCount;

            {**
             Get or set the values per groups count
            }
            property ValuePerGroupCount: NativeUInt read m_ValPerGroupCount write m_ValPerGroupCount;

            {**
             Get or set the repeat count
            }
            property RepeatCount: NativeUInt read m_RepeatCount write m_RepeatCount;

            {**
             Get or set the partial count
            }
            property PartialCount: NativeUInt read m_PartialCount write m_PartialCount;

            {**
             Get or set the do loop value
            }
            property DoLoop: Boolean read m_DoLoop write m_DoLoop;

            {**
             Get or set the negative begin time value
            }
            property NegativeBegin: Boolean read m_NegativeBegin write m_NegativeBegin;

            {**
             Get or set the negative end time value
            }
            property NegativeEnd: Boolean read m_NegativeEnd write m_NegativeEnd;

            {**
             Get or set the negative duration value
            }
            property NegativeDuration: Boolean read m_NegativeDuration write m_NegativeDuration;

            {**
             Get or set the calculation mode
            }
            property CalcMode: TWSVGAnimation.IPropCalcMode.IECalcModeType read m_CalcMode write m_CalcMode;

            {**
             Get or set the additive mode
            }
            property AdditiveMode: TWSVGAnimation.IPropAdditiveMode.IEType read m_AdditiveMode write m_AdditiveMode;
    end;

    {**
     SVG animation descriptor to use when the animation must be applied to values
    }
    TWSVGValueAnimDesc = class(TWSVGAnimationDescriptor)
        public type
            IValues = array of Double;

        private
            m_From:   IValues;
            m_To:     IValues;
            m_By:     IValues;
            m_Values: IValues;

        public
            {**
             Constructor
            }
            constructor Create; override;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Get animated value at position
             @param(position Animation position in percent (between 0.0 and 1.0))
             @returns(Animated value at position, the unit is dependent of animation type)
             @raises(Exception on error)
            }
            function GetValueAt(position: Double): Double; overload; virtual;

            {**
             Get animated values at position
             @param(position Animation position in percent (between 0.0 and 1.0))
             @param(values Value array to get from)
             @param(frameCount - the number of values the frame should contain, in case the animation contains no key list)
             @returns(Animated values at position, the unit is dependent of animation type)
             @raises(Exception on error)
             @br @bold(NOTE) If animation uses key times, value array length may be equal to
                             or a multiple of key time array length
            }
            function GetValuesAt(position: Double; values: IValues; frameCount: NativeUInt): IValues; overload; virtual;

            {**
             Add "from" animation value to list
             @param(value Value to add)
            }
            procedure AddFrom(value: Double); virtual;

            {**
             Add "to" animation value to list
             @param(value Value to add)
            }
            procedure AddTo(value: Double); virtual;

            {**
             Add "by" animation value to list
             @param(value Value to add)
            }
            procedure AddBy(value: Double); virtual;

            {**
             Add animation value to list
             @param(value Value to add)
            }
            procedure AddValue(value: Double); virtual;

        public
            {**
             Get the from values
            }
            property FromList: IValues read m_From;

            {**
             Get to values
            }
            property ToList: IValues read m_To;

            {**
             Get by values
            }
            property ByList: IValues read m_By;

            {**
             Get values
            }
            property Values: IValues read m_Values;
    end;

    {**
     SVG animation descriptor to use when the animation must be applied to a color
    }
    TWSVGColorAnimDesc = class(TWSVGAnimationDescriptor)
        public type
            IValues = array of TWColor;

        private
            m_From:   IValues;
            m_To:     IValues;
            m_By:     IValues;
            m_Values: IValues;

        public
            {**
             Constructor
            }
            constructor Create; override;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Get animated value at position
             @param(position Animation position in percent (between 0.0 and 1.0))
             @returns(Animated value at position)
             @raises(Exception on error)
            }
            function GetValueAt(position: Double): TWColor; virtual;

            {**
             Get animated values at position
             @param(position Animation position in percent (between 0.0 and 1.0))
             @param(values Value array to get from)
             @param(frameCount - the number of values the frame should contain, in case the animation contains no key list)
             @returns(Animated values at position, the unit is dependent of animation type)
             @raises(Exception on error)
             @br @bold(NOTE) If animation uses key times, value array length may be equal to
                             or a multiple of key time array length
            }
            function GetValuesAt(position: Double; values: IValues; frameCount: NativeUInt): IValues; overload; virtual;

            {**
             Add "from" animation value to list
             @param(pValue Value to add)
            }
            procedure AddFrom(const pValue: PWColor); virtual;

            {**
             Add "to" animation value to list
             @param(pValue Value to add)
            }
            procedure AddTo(const pValue: PWColor); virtual;

            {**
             Add "by" animation value to list
             @param(pValue Value to add)
            }
            procedure AddBy(const pValue: PWColor); virtual;

            {**
             Add animation value to list
             @param(pValue Value to add)
            }
            procedure AddValue(const pValue: PWColor); virtual;

        public
            {**
             Get the from values
            }
            property FromList: IValues read m_From;

            {**
             Get to values
            }
            property ToList: IValues read m_To;

            {**
             Get by values
            }
            property ByList: IValues read m_By;

            {**
             Get values
            }
            property Values: IValues read m_Values;
    end;

    {**
     SVG animation descriptor to use when the animation must be applied to a matrix
     @br @bold(NOTE) This descriptor also uses double as values, instead of WMatrix3x3, as it could
                     be expected. The reason is that the data are written as follow in the SVG:
                     <animateTransform
                         attributeType="xml"
                         attributeName="transform"
                         type="rotate"
                         from="0 20 20"
                         to="360 20 20"
                         dur="0.5s"
                         repeatCount="indefinite"/>
                     As seen in the above example, a matrix animation is described as a type, that
                     indicates which kind of matrix should be built, and several value list, that
                     contain the data to apply to the matrix. But there are no matrix list, it's
                     the reason why double type is used instead of matrix
    }
    TWSVGMatrixAnimDesc = class(TWSVGValueAnimDesc)
        private
            m_TransformType: TWSVGAnimation.IPropAnimTransformType.IETransformType;

        public
            {**
             Constructor
            }
            constructor Create; override;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Combine the animation values with a matrix
             @param(position Animation position in percent (between 0.0 and 1.0))
             @param(matrix @bold([in, out]) Matrix in which animation values will be combined)
            }
            procedure Combine(position: Double; var matrix: TWMatrix3x3); virtual;

        public
            {**
             Get transform type
            }
            property TransformType: TWSVGAnimation.IPropAnimTransformType.IETransformType read m_TransformType write m_TransformType;
    end;

    {**
     SVG animation descriptor to use when the animation must be applied to an enumerated value
    }
    TWSVGEnumAnimDesc = class(TWSVGAnimationDescriptor)
        public type
            IValues = array of Integer;

        private
            m_From:   IValues;
            m_To:     IValues;
            m_By:     IValues;
            m_Values: IValues;

        public
            {**
             Constructor
            }
            constructor Create; override;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Get animated value at position
             @param(position Animation position in percent (between 0.0 and 1.0))
             @returns(Animated value at position)
             @raises(Exception on error)
            }
            function GetValueAt(position: Double): Integer; virtual;

            {**
             Add "from" animation value to list
             @param(value Value to add)
            }
            procedure AddFrom(value: Integer); virtual;

            {**
             Add "to" animation value to list
             @param(value Value to add)
            }
            procedure AddTo(value: Integer); virtual;

            {**
             Add "by" animation value to list
             @param(value Value to add)
            }
            procedure AddBy(value: Integer); virtual;

            {**
             Add animation value to list
             @param(value Value to add)
            }
            procedure AddValue(value: Integer); virtual;

        public
            {**
             Get the from values
            }
            property FromList: IValues read m_From;

            {**
             Get to values
            }
            property ToList: IValues read m_To;

            {**
             Get by values
            }
            property ByList: IValues read m_By;

            {**
             Get values
            }
            property Values: IValues read m_Values;
    end;

implementation
//---------------------------------------------------------------------------
// TWSVGAnimationDescriptor
//---------------------------------------------------------------------------
constructor TWSVGAnimationDescriptor.Create;
begin
    inherited Create;

    m_pAnimation       := nil;
    m_pBegin           := TWSimpleTime.Create;
    m_pEnd             := TWSimpleTime.Create;
    m_pDuration        := TWSimpleTime.Create;
    m_GroupCount       := 0;
    m_ValPerGroupCount := 0;
    m_RepeatCount      := 0;
    m_PartialCount     := 0;
    m_DoLoop           := False;
    m_NegativeBegin    := False;
    m_NegativeEnd      := False;
    m_NegativeDuration := False;
    m_CalcMode         := TWSVGAnimation.IPropCalcMode.IECalcModeType.IE_CT_Linear;
    m_AdditiveMode     := TWSVGAnimation.IPropAdditiveMode.IEType.IE_AT_Replace;
end;
//---------------------------------------------------------------------------
destructor TWSVGAnimationDescriptor.Destroy;
begin
    m_pBegin.Free;
    m_pEnd.Free;
    m_pDuration.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimationDescriptor.GetBezierControlPointsFromKeySplines(index: NativeUInt;
        out control1: TWVector2; out control2: TWVector2);
var
    keySplineCount, keyIndex: NativeUInt;
begin
    keySplineCount := Length(m_KeySplines);
    keyIndex       := (index * 4);

    if (keyIndex >= keySplineCount) then
        raise Exception.Create('Index is out of bounds');

    control1.X := m_KeySplines[keyIndex];
    control1.Y := m_KeySplines[keyIndex + 1];
    control2.X := m_KeySplines[keyIndex + 2];
    control2.Y := m_KeySplines[keyIndex + 3];
end;
//---------------------------------------------------------------------------
function TWSVGAnimationDescriptor.GetBezierProgression(index: NativeUInt; position: Double): Double;
var
    startPoint, endPoint, control1, control2: TWVector2;
begin
    startPoint := Default(TWVector2);
    endPoint   := TWVector2.Create(1.0, 1.0);

    GetBezierControlPointsFromKeySplines(index, control1, control2);

    // using a Bezier curve for a time value, the abscissa represents the elapsed time, and the
    // ordinate represents the progression during this time. As the time may be considered as
    // elapsing regularly, the resulting value can be found on the ordinate
    Result := TWGeometryTools.GetCubicBezierPoint(startPoint, endPoint, control1, control2, position).Y;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimationDescriptor.AddKeySpline(key: Double);
begin
    SetLength(m_KeySplines, Length(m_KeySplines) + 1);
    m_KeySplines[Length(m_KeySplines) - 1] := key;
end;
//---------------------------------------------------------------------------
procedure TWSVGAnimationDescriptor.AddKeyTime(key: Double);
begin
    SetLength(m_KeyTimes, Length(m_KeyTimes) + 1);
    m_KeyTimes[Length(m_KeyTimes) - 1] := key;
end;
//---------------------------------------------------------------------------
// TWSVGValueAnimDesc
//---------------------------------------------------------------------------
constructor TWSVGValueAnimDesc.Create;
begin
    inherited Create;
end;
//---------------------------------------------------------------------------
destructor TWSVGValueAnimDesc.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGValueAnimDesc.GetValueAt(position: Double): Double;
var
    valueCount:     NativeUInt;
    fromPos, toPos: Double;
    values:         IValues;
begin
    valueCount := Length(m_Values);

    // value list defined? (value list take precedence over from/to/by values, see:
    // https://www.w3.org/TR/2001/REC-smil-animation-20010904/#ValuesAttribute)
    if (valueCount > 0) then
    begin
        values := GetValuesAt(position, m_Values, 1);

        // for single values, the result should contain only one item
        if (Length(values) <> 1) then
            raise Exception.CreateFmt('Incorrect value count - %d', [Integer(Length(values))]);

        Exit(values[0]);
    end;

    // in case this happen:
    // https://www.w3.org/TR/2001/REC-smil-animation-20010904/#ByAttribute
    if (Length(m_By) > 0) then
        raise Exception.Create('Unsupported by animation');

    values := GetValuesAt(position, m_From, 1);

    // for single values, the result should contain only one item
    if (Length(values) <> 1) then
        raise Exception.CreateFmt('Incorrect from value count - %d', [Integer(Length(values))]);

    fromPos := values[0];
    values  := GetValuesAt(position, m_To, 1);

    // for single values, the result should contain only one item
    if (Length(values) <> 1) then
        raise Exception.CreateFmt('Incorrect to value count - %d', [Integer(Length(values))]);

    toPos := values[0];

    // calculate animation position
    Result := fromPos + (position * (toPos - fromPos));
end;
//---------------------------------------------------------------------------
function TWSVGValueAnimDesc.GetValuesAt(position: Double; values: IValues; frameCount: NativeUInt): IValues;
var
    keyTimeCount,
    valueCount,
    resultCount,
    i,
    j,
    index,
    startIndex,
    endIndex,
    frameIndex,
    curStart,
    curEnd:       NativeUInt;
    curPos,
    indexCount,
    progression,
    relativePos,
    deltaTime,
    posBetween,
    timePerFrame: Double;
begin
    valueCount := Length(values);

    // value list contains no values, or only one value?
    if (valueCount = 0) then
        Exit
    else
    if (valueCount = 1) then
    begin
        SetLength(Result, 1);
        Result[0] := values[0];
        Exit;
    end;

    keyTimeCount := Length(m_KeyTimes);

    // key time are used?
    if (keyTimeCount > 0) then
    begin
        // check if the key time count is a multiplier of the value count and find
        // how many values should be get by key time
        if (not TWMathHelper.CheckAndGetMuliplier(keyTimeCount, valueCount, resultCount)) then
        begin
            TWLogHelper.LogToCompiler('Malformed animation - the value count does not match with the key time count - value count - '
                    + IntToStr(valueCount) + ' - key time count - ' + IntToStr(keyTimeCount));
            Exit;
        end;

        curPos := Min(position, 1.0);

        // iterate through animation keys
        for i := 0 to keyTimeCount - 2 do
            // found current key?
            if ((curPos >= m_KeyTimes[i]) and (curPos <= m_KeyTimes[i + 1])) then
            begin
                // calculate start and end indexes to interpolate
                startIndex := i;
                endIndex   := i + 1;

                // animations governed by time keys are in fact divided into several segments.
                // Each values in the key time list represent the time where the animation
                // should start and end. These values are paired with the values list, that
                // represent the start and end positions the animated segment should reach. As
                // the received position is relative to the whole animation, it must be
                // converted to indicate which percent of the segment is currently processed.
                // For example, a segment beginning on 22% of the total time and ending on 44%
                // is 50% processed if the received position is equal to 33%
                //
                // --------------------------------------¦------------------------------------------------------------------
                // |                                     ¦    Total time = 100%                                            |
                // |-------------------------------------¦-----------------------------------------------------------------|
                // | Seg. 1 from 0% to 22% | Seg. 2 from 22% to 44% | Seg. 3 from 44% to 100%                              |
                // |                       |             ¦          |                                                      |
                // |-----------------------|-------------¦----------|------------------------------------------------------|
                //                                       ¦
                //                                       ¦ position = 33%, pos in segment2 = 50%
                //
                deltaTime  := (m_KeyTimes[endIndex] - m_KeyTimes[startIndex]);
                posBetween := (curPos - m_KeyTimes[startIndex]) / deltaTime;

                // search for calculation mode
                case (m_CalcMode) of
                    TWSVGAnimation.IPropCalcMode.IECalcModeType.IE_CT_Discrete: progression := 1.0;
                    TWSVGAnimation.IPropCalcMode.IECalcModeType.IE_CT_Linear:   progression := posBetween;
                    TWSVGAnimation.IPropCalcMode.IECalcModeType.IE_CT_Spline:   progression := GetBezierProgression(startIndex, posBetween);
                else
                    raise Exception.CreateFmt('Unknown calculation mode - %d', [Integer(m_CalcMode)]);
                end;

                SetLength(Result, resultCount);

                for j := 0 to resultCount - 1 do
                begin
                    curStart := (startIndex * resultCount) + j;
                    curEnd   := (endIndex   * resultCount) + j;

                    // calculate relative animation position (i.e. animation between key times)
                    relativePos := (values[curEnd] - values[curStart]) * progression;

                    // calculate animation length between keys
                    Result[j] := (values[curStart] + relativePos);
                end;

                Exit;
            end;

        // should never happen because current position should always be found between key times
        TWLogHelper.LogToCompiler('Malformed animation - the key time could not be found - position - '
                + FloatToStr(position) + ' - key time count - ' + IntToStr(keyTimeCount));
        Exit;
    end;

    // check if value count matches the frame count
    if ((valueCount = 0) or ((valueCount mod frameCount) <> 0)) then
    begin
        TWLogHelper.LogToCompiler('malformed animation - the number of values must match the number of frame values - '
                + IntToStr(valueCount) + ' - ' + IntToStr(frameCount));
        Exit;
    end;

    SetLength(Result, frameCount);

    // calculate the index count and the current index
    indexCount := (valueCount - 1) div frameCount;
    index      := Floor(position * indexCount);
    frameIndex := (index * frameCount);

    // calculate the animation position inside the frame
    timePerFrame := 1.0 / indexCount;

    if (timePerFrame > 0.0) then
        posBetween := TWMathHelper.ExtMod(position, timePerFrame) / timePerFrame
    else
        posBetween := 0.0;

    // search for calculation mode
    case (m_CalcMode) of
        TWSVGAnimation.IPropCalcMode.IECalcModeType.IE_CT_Discrete: progression := 1.0;
        TWSVGAnimation.IPropCalcMode.IECalcModeType.IE_CT_Linear:   progression := posBetween;
        TWSVGAnimation.IPropCalcMode.IECalcModeType.IE_CT_Spline:   progression := GetBezierProgression(frameIndex, posBetween);
    else
        raise Exception.CreateFmt('Unknown calculation mode - %d', [Integer(m_CalcMode)]);
    end;

    for i := 0 to frameCount - 1 do
    begin
        // calculate the from and to indexes for each x and y values
        startIndex := (frameIndex + i)          mod valueCount;
        endIndex   := (startIndex + frameCount) mod valueCount;

        // calculate frame value
        Result[i] := values[startIndex] + (progression * (values[endIndex] - values[startIndex]));
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGValueAnimDesc.AddFrom(value: Double);
begin
    SetLength(m_From, Length(m_From) + 1);
    m_From[Length(m_From) - 1] := value;
end;
//---------------------------------------------------------------------------
procedure TWSVGValueAnimDesc.AddTo(value: Double);
begin
    SetLength(m_To, Length(m_To) + 1);
    m_To[Length(m_To) - 1] := value;
end;
//---------------------------------------------------------------------------
procedure TWSVGValueAnimDesc.AddBy(value: Double);
begin
    SetLength(m_By, Length(m_By) + 1);
    m_By[Length(m_By) - 1] := value;
end;
//---------------------------------------------------------------------------
procedure TWSVGValueAnimDesc.AddValue(value: Double);
begin
    SetLength(m_Values, Length(m_Values) + 1);
    m_Values[Length(m_Values) - 1] := value;
end;
//---------------------------------------------------------------------------
// TWSVGColorAnimDesc
//---------------------------------------------------------------------------
constructor TWSVGColorAnimDesc.Create;
begin
    inherited Create;
end;
//---------------------------------------------------------------------------
destructor TWSVGColorAnimDesc.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGColorAnimDesc.GetValueAt(position: Double): TWColor;
var
    valueCount:         NativeUInt;
    fromColor, toColor: TWColor;
    values:             IValues;
begin
    valueCount := Length(m_Values);

    // value list defined? (value list take precedence over from/to/by values, see:
    // https://www.w3.org/TR/2001/REC-smil-animation-20010904/#ValuesAttribute)
    if (valueCount > 0) then
    begin
        values := GetValuesAt(position, m_Values, 1);

        // for single values, the result should contain only one item
        if (Length(values) <> 1) then
            raise Exception.CreateFmt('Incorrect value count - %d', [Integer(Length(values))]);

        Exit(values[0]);
    end;

    // in case this happen:
    // https://www.w3.org/TR/2001/REC-smil-animation-20010904/#ByAttribute
    if (Length(m_By) > 0) then
        raise Exception.Create('Unsupported by animation');

    values := GetValuesAt(position, m_From, 1);

    // for single values, the result should contain only one item
    if (Length(values) <> 1) then
        raise Exception.CreateFmt('Incorrect from value count - %d', [Integer(Length(values))]);

    fromColor := values[0];
    values  := GetValuesAt(position, m_To, 1);

    // for single values, the result should contain only one item
    if (Length(values) <> 1) then
        raise Exception.CreateFmt('Incorrect to value count - %d', [Integer(Length(values))]);

    toColor := values[0];

    // calculate animation position
    Result := fromColor.Blend(toColor, position)
end;
//---------------------------------------------------------------------------
function TWSVGColorAnimDesc.GetValuesAt(position: Double; values: IValues; frameCount: NativeUInt): IValues;
var
    keyTimeCount,
    valueCount,
    resultCount,
    i,
    j,
    index,
    startIndex,
    endIndex,
    frameIndex,
    curStart,
    curEnd:       NativeUInt;
    curPos,
    indexCount,
    progression,
    deltaTime,
    posBetween,
    timePerFrame: Double;
begin
    valueCount := Length(values);

    // value list contains no values, or only one value?
    if (valueCount = 0) then
        Exit
    else
    if (valueCount = 1) then
    begin
        SetLength(Result, 1);
        Result[0] := values[0];
        Exit;
    end;

    keyTimeCount := Length(m_KeyTimes);

    // key time are used?
    if (keyTimeCount > 0) then
    begin
        // check if the key time count is a multiplier of the value count and find
        // how many values should be get by key time
        if (not TWMathHelper.CheckAndGetMuliplier(keyTimeCount, valueCount, resultCount)) then
        begin
            TWLogHelper.LogToCompiler('Malformed animation - the value count does not match with the key time count - value count - '
                    + IntToStr(valueCount) + ' - key time count - ' + IntToStr(keyTimeCount));
            Exit;
        end;

        curPos := Min(position, 1.0);

        // iterate through animation keys
        for i := 0 to keyTimeCount - 2 do
            // found current key?
            if ((curPos >= m_KeyTimes[i]) and (curPos <= m_KeyTimes[i + 1])) then
            begin
                // calculate start and end indexes to interpolate
                startIndex := i;
                endIndex   := i + 1;

                // animations governed by time keys are in fact divided into several segments.
                // Each values in the key time list represent the time where the animation
                // should start and end. These values are paired with the values list, that
                // represent the start and end positions the animated segment should reach. As
                // the received position is relative to the whole animation, it must be
                // converted to indicate which percent of the segment is currently processed.
                // For example, a segment beginning on 22% of the total time and ending on 44%
                // is 50% processed if the received position is equal to 33%
                //
                // --------------------------------------¦------------------------------------------------------------------
                // |                                     ¦    Total time = 100%                                            |
                // |-------------------------------------¦-----------------------------------------------------------------|
                // | Seg. 1 from 0% to 22% | Seg. 2 from 22% to 44% | Seg. 3 from 44% to 100%                              |
                // |                       |             ¦          |                                                      |
                // |-----------------------|-------------¦----------|------------------------------------------------------|
                //                                       ¦
                //                                       ¦ position = 33%, pos in segment2 = 50%
                //
                deltaTime  := (m_KeyTimes[endIndex] - m_KeyTimes[startIndex]);
                posBetween := (curPos - m_KeyTimes[startIndex]) / deltaTime;

                // search for calculation mode
                case (m_CalcMode) of
                    TWSVGAnimation.IPropCalcMode.IECalcModeType.IE_CT_Discrete: progression := 1.0;
                    TWSVGAnimation.IPropCalcMode.IECalcModeType.IE_CT_Linear:   progression := posBetween;
                    TWSVGAnimation.IPropCalcMode.IECalcModeType.IE_CT_Spline:   progression := GetBezierProgression(startIndex, posBetween);
                else
                    raise Exception.CreateFmt('Unknown calculation mode - %d', [Integer(m_CalcMode)]);
                end;

                SetLength(Result, resultCount);

                for j := 0 to resultCount - 1 do
                begin
                    curStart := (startIndex * resultCount) + j;
                    curEnd   := (endIndex   * resultCount) + j;

                    // calculate relative animation color (i.e. between key times)
                    Result[j] := values[curStart].Blend(values[curEnd], progression);
                end;

                Exit;
            end;

        // should never happen because current position should always be found between key times
        TWLogHelper.LogToCompiler('Malformed animation - the key time could not be found - position - '
                + FloatToStr(position) + ' - key time count - ' + IntToStr(keyTimeCount));
        Exit;
    end;

    // check if value count matches the frame count
    if ((valueCount = 0) or ((valueCount mod frameCount) <> 0)) then
    begin
        TWLogHelper.LogToCompiler('malformed animation - the number of values must match the number of frame values - '
                + IntToStr(valueCount) + ' - ' + IntToStr(frameCount));
        Exit;
    end;

    SetLength(Result, frameCount);

    // calculate the index count and the current index
    indexCount := (valueCount - 1) div frameCount;
    index      := Floor(position * indexCount);
    frameIndex := (index * frameCount);

    // calculate the animation position inside the frame
    timePerFrame := 1.0 / indexCount;

    if (timePerFrame > 0.0) then
        posBetween := TWMathHelper.ExtMod(position, timePerFrame) / timePerFrame
    else
        posBetween := 0.0;

    // search for calculation mode
    case (m_CalcMode) of
        TWSVGAnimation.IPropCalcMode.IECalcModeType.IE_CT_Discrete: progression := 1.0;
        TWSVGAnimation.IPropCalcMode.IECalcModeType.IE_CT_Linear:   progression := posBetween;
        TWSVGAnimation.IPropCalcMode.IECalcModeType.IE_CT_Spline:   progression := GetBezierProgression(frameIndex, posBetween);
    else
        raise Exception.CreateFmt('Unknown calculation mode - %d', [Integer(m_CalcMode)]);
    end;

    for i := 0 to frameCount - 1 do
    begin
        // calculate the from and to indexes for each x and y values
        startIndex := (frameIndex + i)          mod valueCount;
        endIndex   := (startIndex + frameCount) mod valueCount;

        // calculate final color value
        Result[i] := values[startIndex].Blend(values[endIndex], progression)
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGColorAnimDesc.AddFrom(const pValue: PWColor);
begin
    if (not Assigned(pValue)) then
        Exit;

    SetLength(m_From, Length(m_From) + 1);
    m_From[Length(m_From) - 1].Assign(pValue^);
end;
//---------------------------------------------------------------------------
procedure TWSVGColorAnimDesc.AddTo(const pValue: PWColor);
begin
    if (not Assigned(pValue)) then
        Exit;

    SetLength(m_To, Length(m_To) + 1);
    m_To[Length(m_To) - 1].Assign(pValue^);
end;
//---------------------------------------------------------------------------
procedure TWSVGColorAnimDesc.AddBy(const pValue: PWColor);
begin
    if (not Assigned(pValue)) then
        Exit;

    SetLength(m_By, Length(m_By) + 1);
    m_By[Length(m_By) - 1].Assign(pValue^);
end;
//---------------------------------------------------------------------------
procedure TWSVGColorAnimDesc.AddValue(const pValue: PWColor);
begin
    if (not Assigned(pValue)) then
        Exit;

    SetLength(m_Values, Length(m_Values) + 1);
    m_Values[Length(m_Values) - 1].Assign(pValue^);
end;
//---------------------------------------------------------------------------
// TWSVGMatrixAnimDesc
//---------------------------------------------------------------------------
constructor TWSVGMatrixAnimDesc.Create;
begin
    m_TransformType := TWSVGAnimation.IPropAnimTransformType.IETransformType.IE_TT_Unknown;

    inherited Create;
end;
//---------------------------------------------------------------------------
destructor TWSVGMatrixAnimDesc.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGMatrixAnimDesc.Combine(position: Double; var matrix: TWMatrix3x3);
var
    xMat, yMat:            Double;
    i, fromCount, toCount: NativeUInt;
    rotationCenter, skew:  TWVector2;
    frameValues:           IValues;
begin
    if (Length(m_Values) > 0) then
        frameValues := GetValuesAt(position, m_Values, m_ValPerGroupCount)
    else
    begin
        // in case this happen:
        // https://www.w3.org/TR/2001/REC-smil-animation-20010904/#ByAttribute
        if (Length(m_By) > 0) then
            raise Exception.Create('Unsupported by animation');

        fromCount := Length(m_From);
        toCount   := Length(m_To);

        // animation from and to properties should always contain the same number of values
        if (fromCount <> toCount) then
            raise Exception.CreateFmt('Malformed animation - from count - %d - to count - %d',
                    [fromCount, toCount]);

        SetLength(frameValues, fromCount);

        for i := 0 to fromCount - 1 do
            frameValues[i] := m_From[i] + ((m_To[i] - m_From[i]) * position);
    end;

    // search for transformation type
    case (m_TransformType) of
        TWSVGAnimation.IPropAnimTransformType.IETransformType.IE_TT_Translate:
        begin
            case (Length(frameValues)) of
                1:
                begin
                    xMat := frameValues[0];
                    yMat := 0.0;
                end;

                2:
                begin
                    xMat := frameValues[0];
                    yMat := frameValues[1];
                end;
            else
                raise Exception.CreateFmt('Unsupported value count - %d',
                        [Integer(Length(frameValues))]);
            end;

            matrix.Translate(TWVector2.Create(xMat, yMat));
        end;

        TWSVGAnimation.IPropAnimTransformType.IETransformType.IE_TT_Rotate:
        begin
            case (Length(frameValues)) of
                1: rotationCenter := TWVector2.Create(0.0, 0.0);
                2: rotationCenter := TWVector2.Create(frameValues[1], 0.0);
                3: rotationCenter := TWVector2.Create(frameValues[1], frameValues[2]);
            else
                raise Exception.CreateFmt('Unsupported value count - %d',
                        [Integer(Length(frameValues))]);
            end;

            matrix.RotateCenter(TWGeometryTools.DegToRad(frameValues[0]), rotationCenter);
        end;

        TWSVGAnimation.IPropAnimTransformType.IETransformType.IE_TT_Scale:
        begin
            case (Length(frameValues)) of
                1:
                begin
                    xMat := frameValues[0];
                    yMat := 0.0;
                end;

                2:
                begin
                    xMat := frameValues[0];
                    yMat := frameValues[1];
                end;
            else
                raise Exception.CreateFmt('Unsupported value count - %d',
                        [Integer(Length(frameValues))]);
            end;

            matrix.Scale(TWVector2.Create(xMat, yMat));
        end;

        TWSVGAnimation.IPropAnimTransformType.IETransformType.IE_TT_SkewX:
        begin
            if (Length(frameValues) = 0) then
                raise Exception.CreateFmt('Unsupported value count - %d', [Integer(Length(frameValues))]);

            skew := TWVector2.Create(Tan(TWGeometryTools.DegToRad(frameValues[0])), 0.0);
            matrix.Shear(skew);
        end;

        TWSVGAnimation.IPropAnimTransformType.IETransformType.IE_TT_SkewY:
        begin
            if (Length(frameValues) = 0) then
                raise Exception.CreateFmt('Unsupported value count - %d', [Integer(Length(frameValues))]);

            skew := TWVector2.Create(0.0, Tan(TWGeometryTools.DegToRad(frameValues[0])));
            matrix.Shear(skew);
        end;
    else
        raise Exception.CreateFmt('Unknown animation transform type - %d',
                [Integer(m_TransformType)]);
    end;
end;
//---------------------------------------------------------------------------
// TWSVGEnumAnimDesc
//---------------------------------------------------------------------------
constructor TWSVGEnumAnimDesc.Create;
begin
    inherited Create;
end;
//---------------------------------------------------------------------------
destructor TWSVGEnumAnimDesc.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGEnumAnimDesc.GetValueAt(position: Double): Integer;
var
    keyTimeCount, i: NativeUInt;
    curPos:          Double;
begin
    keyTimeCount := Length(m_KeyTimes);

    // key time are used?
    if (keyTimeCount > 0) then
    begin
        curPos := Min(position, 1.0);

        // iterate through animation keys
        for i := 0 to keyTimeCount - 2 do
            // found current key?
            if ((curPos >= m_KeyTimes[i]) and (curPos <= m_KeyTimes[i + 1])) then
                Exit(m_Values[i]);

        // should never happen because current position should always be found between key times
        TWLogHelper.LogToCompiler('Malformed animation - the key time could not be found - position - '
                + FloatToStr(position) + ' - key time count - ' + IntToStr(keyTimeCount));
    end;

    // list of values?
    if (Length(m_Values) > 0) then
        // calculate the index to get relatively to the animation position
        Exit(m_Values[Trunc(position * (Length(m_Values) - 1))]);

    raise Exception.Create('NOT IMPLEMENTED');
end;
//---------------------------------------------------------------------------
procedure TWSVGEnumAnimDesc.AddFrom(value: Integer);
begin
    SetLength(m_From, Length(m_From) + 1);
    m_From[Length(m_From) - 1] := value;
end;
//---------------------------------------------------------------------------
procedure TWSVGEnumAnimDesc.AddTo(value: Integer);
begin
    SetLength(m_To, Length(m_To) + 1);
    m_To[Length(m_To) - 1] := value;
end;
//---------------------------------------------------------------------------
procedure TWSVGEnumAnimDesc.AddBy(value: Integer);
begin
    SetLength(m_By, Length(m_By) + 1);
    m_By[Length(m_By) - 1] := value;
end;
//---------------------------------------------------------------------------
procedure TWSVGEnumAnimDesc.AddValue(value: Integer);
begin
    SetLength(m_Values, Length(m_Values) + 1);
    m_Values[Length(m_Values) - 1] := value;
end;
//---------------------------------------------------------------------------

end.
