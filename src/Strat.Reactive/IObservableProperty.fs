namespace Strat.Reactive
open System
open System.ComponentModel


/// Describes an observable property. An observable property is an observable that provides synchronous access to
/// its current value, and raised property change notifications when the current value changes.
type IObservableProperty<'T> = 
   inherit IObservable<'T>
   inherit INotifyPropertyChanged
   /// Gets (synchronously) the current value of this observable property.
   abstract member Value: 'T