{**
 @abstract(@name provides a smart pointer. A smart pointer is a class that auto-deletes an object
           instance when it becomes out of scope (similar to std::auto_ptr).)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSmartPointer;

interface

uses System.SysUtils;

type
    {**
     This works as an interface containing an anonymous function. This allow the compiler to call
     the Invoke function anonymously while the smart pointer is used, allowing thus to write
     pMyClass.MyProperty instead of pMyClass.Invoke.MyProperty
    }
    IWSmartPointer<T> = reference to function: T;

    {**
     Smart pointer
     @br @bold(NOTE) Smart pointer should be used as follow:
                     @longCode(#
                               procedure TMyClass.MyFunction;
                               var
                                   // NOTE THAT the interface is declared here and NOT the smar pointer type
                                   pAnotherClass: IWSmartPointer<TAnotherClass>;
                                   ...
                               begin
                                   pAnotherClass := TWSmartPointer<TAnotherClass>.Create;
                               end;
                               #)
     @br @bold(NOTE) Unlike their C++ counterparts, and unfortunately, I could not find a correct
                     way to implement the Release() function. This was not possible because the
                     smart pointer must be declared using its interface, e.g.
                     pClass: IWSmartPointer<TMyClass>, to work as expected. Doing that, any direct
                     call to pClass invokes internally the owned pointer, so there is no further
                     conversion needed, but in other hand, any conversion like
                     TWSmartPointer<TMyClass>(pClass) result to an access violation, because - I
                     think - the conversion is done on the owned pointer instead of the smart pointer
                     itself. For that reason, calling a function like TWSmartPointer<TMyClass>(pClass).
                     Release is impossible, so a smart pointer may be used only for instances that
                     will be always deleted on function ends, like e.g. option forms, but in case
                     the pointer is needed to be released, a try...finally block should be used instead
    }
    TWSmartPointer<T: class, constructor> = class(TInterfacedObject, IWSmartPointer<T>)
    private
        m_pInstance: T;

    public
        {**
         Constructor
        }
        constructor Create; overload; virtual;

        {**
         Constructor
         @param(pInstance New instance to own)
        }
        constructor Create(pInstance: T); overload; virtual;

        {**
         Destructor
        }
        destructor Destroy; override;

        {**
         Invoke the owned class (needed to be used by the IWSmartPointer interface)
         @returns(Owned class)
        }
        function Invoke: T; virtual;
    end;

implementation
//---------------------------------------------------------------------------
constructor TWSmartPointer<T>.Create;
begin
    inherited Create;

    m_pInstance := T.Create;
end;
//---------------------------------------------------------------------------
constructor TWSmartPointer<T>.Create(pInstance: T);
begin
    inherited Create;

    m_pInstance := pInstance;
end;
//---------------------------------------------------------------------------
destructor TWSmartPointer<T>.Destroy;
begin
    FreeAndNil(m_pInstance);

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSmartPointer<T>.Invoke: T;
begin
    Result := m_pInstance;
end;
//---------------------------------------------------------------------------

end.
