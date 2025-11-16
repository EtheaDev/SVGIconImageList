{**
 @abstract(@name provides a global animation timer based on the VCL TTimer control.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWAnimationTimer;

interface

uses System.Classes,
     System.SysUtils,
     Vcl.ExtCtrls,
     Winapi.Windows,
     UTWDesignPatterns;

type
    {**
     VCL animation timer message info
    }
    TWAnimationTimerMsgInfo = record
        {**
         Elapsed time since last draw in milliseconds
        }
        m_ElapsedTime: Double;
    end;

    {**
     Global animation timer based on the VCL TTimer control
    }
    TWAnimationTimer = class sealed (TInterfacedObject, IWSubject)
        public type
            {**
             Animation messages that can be sent to observers
             @value(IE_AM_Animate Message notifying that a new animation frame should be generated)
             @value(IE_AM_Destroying Message notifying that the animation timer is being destroyed)
             @br @bold(NOTE) These values begin on 0 to not interfere with other messages. The
                             allowed range for a new animation timer message is between 0 and 99
            }
            EWAnimationTimerMessages =
            (
                IE_AM_Animate = 0,
                IE_AM_Destroying
            );

        private
            class var m_pInstance:    IWSubject;
                      m_pTimer:       TTimer;
                      m_pObservers:   TList;
                      m_PreviousTime: Double;
                      m_Info:         TWAnimationTimerMsgInfo;

            {**
             Called when animation should be rendered
             @param(pSender Event sender)
            }
            procedure OnAnimate(pSender: TObject);

        public
            {**
             Constructor
            }
            constructor Create;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Gets animation timer instance, creates one if still not created
             @return(Animation timer instance)
            }
            class function GetInstance: IWSubject; static;

            {**
             Attaches observer
             @param(pObserver Observer to attach)
            }
            procedure Attach(pObserver: IWObserver);

            {**
             Detaches observer
             @param(pObserver Observer to detach)
            }
            procedure Detach(pObserver: IWObserver);

            {**
             Notifies all observers about an occurred event
             @param(message Notification message)
            }
            procedure Notify(message: TWMessage);
    end;

implementation

uses
  System.Types;

//---------------------------------------------------------------------------
// TWAnimationTimer
//---------------------------------------------------------------------------
constructor TWAnimationTimer.Create;
begin
    // singleton was already initialized?
    if (Assigned(m_pInstance)) then
        raise Exception.Create('Cannot create many instances of a singleton class');

    inherited Create;

    // configure internal variables
    m_pObservers   := TList.Create;
    m_PreviousTime := GetTickCount;

    // configure animation timer (an interval of 20 means ~50 fps)
    m_pTimer          := TTimer.Create(nil);
    m_pTimer.Interval := 20;
    m_pTimer.OnTimer  := OnAnimate;
    m_pTimer.Enabled  := True;
end;
//---------------------------------------------------------------------------
destructor TWAnimationTimer.Destroy;
var
    message: TWMessage;
begin
    // configure destruction message
    message.m_Type  := NativeUInt(IE_AM_Destroying);
    message.m_pInfo := nil;

    // notify all observers about destruction
    Notify(message);

    // clear memory
    m_pTimer.Free;
    m_pObservers.Free;

    inherited Destroy;

    m_pInstance := nil;
end;
//---------------------------------------------------------------------------
procedure TWAnimationTimer.OnAnimate(pSender: TObject);
var
    now:     NativeUInt;
    message: TWMessage;
begin
    // calculate time interval
    now                  :=  GetTickCount;
    m_Info.m_ElapsedTime := (now - m_PreviousTime);
    m_PreviousTime       :=  now;

    // configure animation message
    message.m_Type  := NativeUInt(IE_AM_Animate);
    message.m_pInfo := @m_Info;

    // notify all observers about animation
    Notify(message);
end;
//---------------------------------------------------------------------------
class function TWAnimationTimer.GetInstance: IWSubject;
begin
    // is singleton instance already initialized?
    if (Assigned(m_pInstance)) then
        // get it
        Exit(m_pInstance);

    // create new singleton instance
    m_pInstance := TWAnimationTimer.Create;
    Result      := m_pInstance;
end;
//---------------------------------------------------------------------------
procedure TWAnimationTimer.Attach(pObserver: IWObserver);
begin
    // observer already exists in observers list?
    if (m_pObservers.IndexOf(Pointer(pObserver)) <> -1) then
        Exit;

    // add observer to observers list
    m_pObservers.Add(Pointer(pObserver));
end;
//---------------------------------------------------------------------------
procedure TWAnimationTimer.Detach(pObserver: IWObserver);
begin
    // remove observer from observers list. NOTE observer list will check if observer exists before
    // trying to remove it, so this check isn't necessary here
    m_pObservers.Remove(Pointer(pObserver));
end;
//---------------------------------------------------------------------------
procedure TWAnimationTimer.Notify(message: TWMessage);
var
    pObject: Pointer;
    pItem:   IWObserver;
begin
    // iterate through observers to notify
    for pObject in m_pObservers do
    begin
        // get observer
        pItem := IWObserver(pObject);

        // found it?
        if (not Assigned(pItem)) then
            continue;

        // notify observer about message
        pItem.OnNotified(message);
    end;
end;
//---------------------------------------------------------------------------

end.
