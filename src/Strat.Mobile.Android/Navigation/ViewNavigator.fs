﻿namespace Strat.Mobile.Android.Navigation

open System
open System.Reactive.Linq
open System.Reactive.Subjects
open System.Threading.Tasks
open Android.App
open Android.Content
open Strat.Mobile.Android


type CurrentActivityViewNavigator (currentActivityProvider: ICurrentActivityProvider) =

   interface IViewNavigator with
      member this.Navigate navInfo =
         // TODO: Current activity will be None when app starts. Do we need a better way to handle this? 
         if currentActivityProvider.CurrentActivity.IsSome then
            match navInfo with
            | Activity activityInfo -> this.NavigateActivity activityInfo
            | Fragment fragmentInfo -> this.NavigateFragment fragmentInfo
         else
            Task.CompletedTask
         
   member private this.NavigateActivity (navInfo: ActivityViewInfo) : Task =
      let activity =  currentActivityProvider.CurrentActivity.Value
      let intent = (new Intent(activity, navInfo.ActivityType)).AddFlags(navInfo.Flags)
      let bundle = ActivityOptions.MakeCustomAnimation(activity, 0, 0).ToBundle()
      // Create a task that will complete when the activity identified by the ActivityNavInfo has started (that is,
      // its OnStart method  has been called).
      let taskSource = new TaskCompletionSource<unit>()
      currentActivityProvider.CurrentActivityObservable
         .Where(fun a -> navInfo.ActivityType.IsAssignableFrom(a.GetType()))
         .FirstAsync()
         .Subscribe(
            (fun _ -> taskSource.SetResult(())),
            (fun (ex: exn) -> taskSource.SetException ex))
      |> ignore
      // Now tell android to start the activity.
      activity.StartActivity(intent, bundle)
      taskSource.Task :> Task

   member private this.NavigateFragment (viewInfo: FragmentViewInfo) : Task =
      async {
         do! Async.SwitchToContext Application.SynchronizationContext
         let activity = currentActivityProvider.CurrentActivity.Value
         let fragment = Activator.CreateInstance viewInfo.FragmentType :?> Fragment
         let fragmentManager = viewInfo.ResolveFragmentManager(activity.FragmentManager)
         return fragmentManager
            .BeginTransaction()
            .Replace(viewInfo.ContainerViewId, fragment, ("Fragment_" + viewInfo.StateId))
            .Commit() 
         |> ignore
      } 
      // TODO: FragmentManager.FragmentLifecycleCallbacks be used to help figure out when the 
      // fragment has started?
      |> Async.StartAsTask :> Task