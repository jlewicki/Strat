namespace Strat.Mobile.Android

open System
open System.Reactive.Linq
open System.Reactive.Subjects
open Android.App


type ICurrentActivityProvider =
   abstract CurrentActivity: option<Activity>
   abstract CurrentActivityObservable : IObservable<Activity>


// Keeps track of the current activity as activities start and resume.
type CurrentActivityActivityLifecycleCallbacks ()= 
   inherit ActivityLifecycleCallbacksBase()
   
   let _navigationActivitySubject = new BehaviorSubject<option<Activity>> (None)
   let _navigationActivity = 
      _navigationActivitySubject
         .Where(Option.isSome)
         .Select(Option.get)
   
   interface ICurrentActivityProvider with
      member this.CurrentActivity : option<Activity> = 
         _navigationActivitySubject.Value
      member this.CurrentActivityObservable : IObservable<Activity>  = 
         _navigationActivity

   //override this.OnActivityCreated (activity, _) =
   //   _navigationActivitySubject.OnNext (Some activity)
   override this.OnActivityStarted activity =
      _navigationActivitySubject.OnNext (Some activity)
   override this.OnActivityResumed activity =
      _navigationActivitySubject.OnNext (Some activity)
