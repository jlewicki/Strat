namespace Strat.Mobile.Android.Navigation

open System
open System.Reactive.Linq
open System.Reactive.Subjects
open System.Threading.Tasks
open System.Reactive.Threading.Tasks
open Android.App
open Android.Content
open Strat.Mobile.Android


type CurrentActivityViewNavigator (currentActivityProvider: ICurrentActivityProvider) =

   interface IViewNavigator with
      member this.Navigate navInfo : Task<obj> =
         match navInfo with
         | Activity activityInfo -> this.NavigateActivity activityInfo
         | Fragment fragmentInfo -> this.NavigateFragment fragmentInfo
         
   //member private this.NavigateActivity (navInfo: ActivityViewInfo) : Task<obj> =
   //   let activity =  currentActivityProvider.CurrentActivity.Value
   //   let intent = (new Intent(activity, navInfo.ActivityType)).AddFlags(navInfo.Flags)
   //   let bundle = ActivityOptions.MakeCustomAnimation(activity, 0, 0).ToBundle()
   //   // Create a task that will complete when the activity identified by the ActivityNavInfo has started (that is,
   //   // its OnStart method  has been called).
   //   let taskSource = new TaskCompletionSource<obj>()
   //   currentActivityProvider.CurrentActivityObservable
   //      .Where(fun a -> navInfo.ActivityType.IsAssignableFrom(a.GetType()))
   //      .FirstAsync()
   //      .Subscribe(
   //         (fun activity -> taskSource.SetResult activity),
   //         (fun (ex: exn) -> taskSource.SetException ex))
   //   |> ignore
   //   // Now tell android to start the activity.
   //   activity.StartActivity(intent, bundle)
   //   taskSource.Task

   member private this.NavigateActivity (navInfo: ActivityViewInfo) : Task<obj> =
      async {
         let activity = currentActivityProvider.CurrentActivity.Value
         let intent = (new Intent(activity, navInfo.ActivityType)).AddFlags(navInfo.Flags)
         let bundle = ActivityOptions.MakeCustomAnimation(activity, 0, 0).ToBundle()
         do! Async.SwitchToContext Application.SynchronizationContext
         activity.StartActivity(intent, bundle)
         do! Async.SwitchToThreadPool()
         let! startedActivity = 
            currentActivityProvider.CurrentActivityObservable
               .Where(fun a -> navInfo.ActivityType.IsAssignableFrom(a.GetType()))
               .Select(fun a -> a :> obj)
               .FirstAsync()
               .ToTask()
            |> Async.AwaitTask
         return startedActivity
      }
      |> Async.StartAsTask
     

   member private this.NavigateFragment (viewInfo: FragmentViewInfo) : Task<obj> =
      async {
         do! Async.SwitchToContext Application.SynchronizationContext
         let activity = currentActivityProvider.CurrentActivity.Value
         let fragment = Activator.CreateInstance viewInfo.FragmentType :?> Fragment
         let fragmentManager = viewInfo.ResolveFragmentManager(activity.FragmentManager)
         fragmentManager
            .BeginTransaction()
            .Replace(viewInfo.ContainerViewId, fragment, ("Fragment_" + viewInfo.StateId))
            .Commit() 
         |> ignore
         // TODO: do we need to use FragmentLifecycleCallbacks to determine when fragment has started?
         return fragment :> obj
      }
      |> Async.StartAsTask