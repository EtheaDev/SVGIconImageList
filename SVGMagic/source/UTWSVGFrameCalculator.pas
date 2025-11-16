{**
 @abstract(@name provides a class to calculate the frame count and time interval required to process
           an animation.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGFrameCalculator;

interface

uses System.SysUtils,
     System.Math,
     Winapi.Windows,
     UTWHelpers;

const
    //---------------------------------------------------------------------------
    // Global macros
    //---------------------------------------------------------------------------
    C_Time_Epsilon: Double = 0.0116; // represents +/- 1 frame or interval per 24 hours
    //---------------------------------------------------------------------------

type
    {**
     Calculate the frame count and time interval required to process an animation
    }
    TWSVGFrameCalculator = class
        public type
            {**
             Frame info
            }
            IInfo = record
                m_FrameCountSinceLastPaint: NativeUInt;
                m_InterpolationFactor:      Double;
                m_FrameCount:               Double;
                m_Interval:                 Double;
                m_ElapsedTime:              Double;
            end;

        private
            m_FrameCount:          Double;
            m_Interval:            Double;
            m_InterpolationFactor: Double;
            m_Time:                NativeUInt;

            {**
             Calculate and update frame count
            }
            procedure CalculateAndUpdateFrameCount; inline;

            {**
             Calculate and update time interval
            }
            procedure CalculateAndUpdateInterval; inline;

        public
            {**
             Constructor
            }
            constructor Create; overload; virtual;

            {**
             Constructor
            *@param(frameCount Frame count)
            }
            constructor Create(frameCount: Double); overload; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Start time measurement
            }
            procedure StartTimer; virtual;

            {**
             Get frame count
             @returns(The frame count)
             @br @bold(NOTE) Be careful, this is not identical to the FPS. The frame count is used to
                             determine how many frames, in an ideal situation, should be rendered by
                             seconds, and thus allows to calculate the time interval between each frames.
                             Instead, the FPS represents the number of frames per seconds a system can
                             effectively process
            }
            function GetFrameCount: Double; virtual;

            {**
             Set frame count
             @param(frameCount Frame count)
             @br @bold(NOTE) Be careful, this is not identical to the FPS. The frame count is used to
                             determine how many frames, in an ideal situation, should be rendered by
                             seconds, and thus allows to calculate the time interval between each frames.
                             Instead, the FPS represents the number of frames per seconds a system can
                             effectively process
            }
            procedure SetFrameCount(frameCount: Double); virtual;

            {**
             Set time interval after which frame count should be incremented
             @param(interval Time interval)
            }
            procedure SetInterval(interval: Double); virtual;

            {**
             Calculate frame count and time interval to respect a total animation duration
             @param(duration Duration to set in milliseconds)
             @param(frameCount Number of frames the animation contains)
             @raises(Exception if duration is equal to 0)
             @br @bold(NOTE) The duration is based on the fact that 100% of an animation cycle must
                             be performed during this time. E.g. for an animation that should execute
                             100 frames during 6 seconds, the frame count will be equal to 17
            }
            procedure SetDuration(duration: Double; frameCount: NativeUInt); virtual;

            {**
             Get frame info
             @param(info @bold([out]) Frame info to populate)
             @raises(Exception if frame count is equal to 0)
            }
            procedure GetInfo(out info: IInfo); virtual;

            {**
             Check and return an index that is always within the range delimited by start and end
             @param(index Index to check)
             @param(startIndex Range start)
             @param(endIndex Range end)
             @returns(An index that is always within the range delimited by start and end)
            }
            class function ValidateIndex(index, startIndex, endIndex: NativeUInt): NativeUInt; static;

        public
            {**
             Get or set the number of frame to render per seconds
             @br @bold(NOTE) Be careful, this is not identical to the FPS. The frame count is used to
                             determine how many frames, in an ideal situation, should be rendered by
                             seconds, and thus allows to calculate the time interval between each frames.
                             Instead, the FPS represents the number of frames per seconds a system can
                             effectively process
            }
            property FrameCount: Double read m_FrameCount write SetFrameCount;

            {**
             Get or set the interval
            }
            property Interval: Double read m_Interval write SetInterval;
    end;

implementation
//---------------------------------------------------------------------------
constructor TWSVGFrameCalculator.Create;
begin
    inherited Create;

    m_InterpolationFactor := 0.0;

    SetFrameCount(C_Time_Epsilon);
end;
//---------------------------------------------------------------------------
constructor TWSVGFrameCalculator.Create(frameCount: Double);
begin
    inherited Create;

    m_InterpolationFactor := 0.0;

    SetFrameCount(frameCount);
end;
//---------------------------------------------------------------------------
destructor TWSVGFrameCalculator.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGFrameCalculator.CalculateAndUpdateFrameCount;
begin
    // calculate frame per seconds
    if (m_Interval > 0.0) then
        m_FrameCount := 1000.0 / m_Interval
    else
        m_FrameCount := 0.0;
end;
//---------------------------------------------------------------------------
procedure TWSVGFrameCalculator.CalculateAndUpdateInterval;
begin
    // calculate time interval between 2 frames
    if (m_FrameCount > 0.0) then
        m_Interval := 1000.0 / m_FrameCount
    else
        m_Interval := 0.0;
end;
//---------------------------------------------------------------------------
procedure TWSVGFrameCalculator.StartTimer;
begin
    m_Time := GetTickCount;
end;
//---------------------------------------------------------------------------
function TWSVGFrameCalculator.GetFrameCount: Double;
begin
    Result := m_FrameCount;
end;
//---------------------------------------------------------------------------
procedure TWSVGFrameCalculator.SetFrameCount(frameCount: Double);
begin
    // is frame count out of bounds?
    if (frameCount = 0.0) then
        m_FrameCount := C_Time_Epsilon
    else
        m_FrameCount := frameCount;

    CalculateAndUpdateInterval;
end;
//---------------------------------------------------------------------------
procedure TWSVGFrameCalculator.SetInterval(interval: Double);
begin
    // is interval out of bounds?
    if (interval = 0.0) then
        m_Interval := C_Time_Epsilon
    else
        m_Interval := interval;

    CalculateAndUpdateFrameCount;
end;
//---------------------------------------------------------------------------
procedure TWSVGFrameCalculator.SetDuration(duration: Double; frameCount: NativeUInt);
begin
    if (duration = 0.0) then
        raise Exception.Create('Duration value is equal to 0');

    SetFrameCount(frameCount * (1000.0 / duration));
end;
//---------------------------------------------------------------------------
procedure TWSVGFrameCalculator.GetInfo(out info: IInfo);
var
    frameCountSinceLastPaint: NativeUInt;
begin
    // no frame per seconds or interval?
    if ((m_FrameCount = 0.0) or (m_Interval = 0.0)) then
        Exit;

    // populate frame info
    info.m_FrameCount  := m_FrameCount;
    info.m_Interval    := m_Interval;
    info.m_ElapsedTime := GetTickCount - m_Time;

    // stamp current time for next calculation
    m_Time := GetTickCount;

    // calculate how many frames must be incremented since the last rendering
    m_InterpolationFactor := m_InterpolationFactor + (info.m_ElapsedTime / m_Interval);

    // should increment one frame or more?
    if (m_InterpolationFactor >= 1.0) then
    begin
        // calculate number of frames to increment
        frameCountSinceLastPaint := Floor(m_InterpolationFactor);

        // calculate interpolation factor, i.e. the remaining part between 2 frames (NOTE should
        // always be between 0 and 1)
        m_InterpolationFactor := m_InterpolationFactor - frameCountSinceLastPaint;

        info.m_FrameCountSinceLastPaint := frameCountSinceLastPaint;
    end
    else
        info.m_FrameCountSinceLastPaint := 0;

    info.m_InterpolationFactor := m_InterpolationFactor;
end;
//------------------------------------------------------------------------------
class function TWSVGFrameCalculator.ValidateIndex(index, startIndex, endIndex: NativeUInt): NativeUInt;
var
    range: NativeUInt;
begin
    // is index out of bounds?
    if ((index >= startIndex) and (index <= endIndex)) then
        Exit(index);

    // calculate range
    range := (endIndex - startIndex) + 1;

    // calculate and return index within the range delimited by start and end
    Exit(startIndex + NativeUInt(TWMathHelper.IntMod(index - startIndex, range)));
end;
//------------------------------------------------------------------------------

end.
