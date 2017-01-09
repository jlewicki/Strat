namespace Strat.UI

open System
open System.Collections.Generic
open System.ComponentModel
open System.Reactive.Linq
open System.Reactive.Subjects

/// <summary>
/// An observable that represents a value (that is, a 'property') that changes over time, and raises property
/// change events as it does so.
/// </summary>
/// <remarks>
/// This is class is very closely related to the BehaviorSubject class included in Rx.Net. However, since 
/// BehaviorSubject does not implement <see cref="INotifyPropertyChanged"/>, it is not useful in data-binding scenarios.
/// <see cref="ReadOnlyProperty"/> is intended to adapt <see cref="IObservable{T}"/> and make it compatible with data-
/// binding.
/// </remarks>
type ReadOnlyProperty<'T>
    (initialValue: 'T, futureValues: IObservable<'T>, equality: IEqualityComparer<'T>) as this =

    static let valuePropChangedArgs = new PropertyChangedEventArgs("Value")
    do if isNull futureValues then nullArg "futureValues"
    do if isNull equality then nullArg "equality"
     
    let valueSubject = new BehaviorSubject<'T>(initialValue)
    let propChangedEvent = Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()

    do
        valueSubject
            // Note that we concat the initial value in the subject with the future values, so that DistinctUntilChanged
            // will compare the initial value with the first future value. Otherwise we are only comparing values in
            // the futureValues stream.
            .Take(1).Concat(futureValues)
            .DistinctUntilChanged(equality)
            .Subscribe(valueSubject)
            |> ignore

        // Raise property changed whenever we get a new value.
        valueSubject
            .Subscribe(
                (fun v -> propChangedEvent.Trigger(this, valuePropChangedArgs)),
                // Note that we explicity add a no-op error handler, since the default handler would rethrow the 
                // exception, which is not what we want (this subcription is just an implementation detail). 
                // valueSubject will cache the error and rethrow it in its Value getter.
                (fun (ex: exn) -> ())) 
            |> ignore

    /// Initializes a new ReadOnlyProperty with the specified initial value and future values, using a default 
    /// comparer.
    new (initialValue: 'T, values: IObservable<'T>) = 
        new ReadOnlyProperty<'T>(initialValue, values, EqualityComparer.Default)

    /// Gets (synchronously) the current value of this property
    member this.Value
        // Note that BehaviorSubject.Value will rethrow exceptions received from futureValues stream.  This is 
        // good since we don't have to do that explicitly.
        with get() = valueSubject.Value

    /// Raised when the value of the Value property changes.
    [<CLIEvent>] 
    member this.PropertyChanged =
        propChangedEvent.Publish 

    interface IObservableProperty<'T> with
        member this.Value = 
            this.Value

    interface IObservable<'T> with
        member this.Subscribe (observer: IObserver<'T>) = 
            valueSubject.Subscribe(observer)

    interface INotifyPropertyChanged with
        [<CLIEvent>] 
        member this.PropertyChanged =
            this.PropertyChanged


/// Functions for creating and composing ObservableProperty instances.
module ObservableProperty = 
   
   /// Creates a new ObservableProperty instance.
   let create initValue values : IObservableProperty<_>= 
      upcast (new ReadOnlyProperty<_>(initValue, values))

   /// Creates a new ObservableProperty instance that always yields that specified value.
   let always initValue : IObservableProperty<_> =
      upcast (new ReadOnlyProperty<_>(initValue, Observable.Never()))