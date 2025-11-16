{**
 @abstract(@name provides some classic ready-to-use design patterns classes)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWDesignPatterns;

interface

type
    {**
     Generic message, e.g. sent from a subject to an observer
    }
    TWMessage = record
        {**
         Generic type, meaning is left to discretion of the child classes implementation
        }
        m_Type: NativeUInt;

        {**
         Generic info, in case a subject should send additional specific info to its observer
        }
        m_pInfo: Pointer
    end;

    {**
     Generic observer
    }
    IWObserver = interface['{BEC3651E-9E18-44A0-8410-D3C6E3F3B82C}']
        {**
         Called when subject has sent a notification to the observer
         @param(message Notification message)
        }
        procedure OnNotified(message: TWMessage);
    end;

    {**
     Generic subject
    }
    IWSubject = interface['{D4A8A2CB-EA4F-4D28-8761-0ACEC8B8CFF0}']
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

end.
