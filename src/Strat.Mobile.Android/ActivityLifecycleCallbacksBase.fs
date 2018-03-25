namespace Strat.Mobile.Android

open Android.App
open Android.OS


/// Provides an empty implementation of the Application.IActivityLifecycleCallbacks interface. Subclasses can override 
[<AbstractClass>]
type ActivityLifecycleCallbacksBase() =
   inherit Java.Lang.Object()

   abstract OnActivityCreated: Activity * Bundle -> unit
   default this.OnActivityCreated (activity, savedInstanceState) = ()
   
   abstract OnActivityStarted: Activity -> unit
   default this.OnActivityStarted (activity) = ()
   
   abstract OnActivityResumed: Activity -> unit
   default this.OnActivityResumed (activity) = ()
   
   abstract OnActivityPaused: Activity -> unit
   default this.OnActivityPaused (activity) = ()
   
   abstract OnActivityStopped: Activity -> unit 
   default this.OnActivityStopped (activity) = ()

   abstract OnActivityDestroyed: Activity -> unit
   default this.OnActivityDestroyed (activity) = ()

   abstract OnActivitySaveInstanceState: Activity * Bundle -> unit
   default this.OnActivitySaveInstanceState (activity, outState) = ()
   
   interface Application.IActivityLifecycleCallbacks with
      member this.OnActivityCreated (activity, savedInstanceState) = this.OnActivityCreated(activity, savedInstanceState)
      member this.OnActivityStarted (activity) = this.OnActivityStarted activity
      member this.OnActivityResumed (activity) = this.OnActivityResumed activity
      member this.OnActivityPaused (activity) = this.OnActivityPaused activity
      member this.OnActivityStopped (activity) = this.OnActivityStopped activity
      member this.OnActivityDestroyed (activity) = this.OnActivityDestroyed activity
      member this.OnActivitySaveInstanceState (activity, outState) = this.OnActivitySaveInstanceState(activity, outState)
 
      