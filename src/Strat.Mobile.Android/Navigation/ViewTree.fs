namespace Strat.Mobile.Android.Navigation

open System
open System.Threading.Tasks
open Android.App
open Android.Content
open Strat.StateMachine
open Strat.StateMachine.Definition


type ActivityViewInfo (stateId: StateId, activityType: Type, flags: ActivityFlags) = 
   member this.StateId = stateId
   member this.ActivityType = activityType
   member this.Flags = flags


type FragmentViewInfo (stateId: StateId, fragmentType: Type, containerViewId: int, resolveFragManager: FragmentManager -> FragmentManager) = 
   member this.StateId = stateId
   member this.FragmentType = fragmentType
   member this.ContainerViewId = containerViewId
   member this.ResolveFragmentManager(activityFragmentManager: FragmentManager) = resolveFragManager activityFragmentManager


type ViewInfo =
   | Activity of ActivityViewInfo
   | Fragment of FragmentViewInfo
with
   member this.StateId =
      match this with
      | Activity activityInfo -> activityInfo.StateId
      | Fragment fragmentInfo -> fragmentInfo.StateId


type IViewNavigator =
   abstract Navigate: ViewInfo -> Task<obj>


[<Sealed>]
type FragmentStateView(viewInfo: FragmentViewInfo, childViews: list<FragmentStateView>) =
   member this.ViewInfo = viewInfo
   member this.ChildViews = childViews


[<Sealed>]
type ActivityStateView(viewInfo: ActivityViewInfo, childViews: list<FragmentStateView>) =
   member this.ViewInfo = viewInfo
   member this.ChildViews = childViews


type StateViewTree = list<ActivityStateView>


type CreateFragmentStateView = 
   | CreateFragmentStateView of ((FragmentManager->FragmentManager) -> int -> FragmentStateView)


module ViewTree =
   let navigateOnEnter (navigator: IViewNavigator) (viewInfo: ViewInfo) onEnter = 
      TransitionHandler.Async (fun transCtx ->
         async {
            // We first call the inner onEnter handler, if available. The expectation is that this handler will 
            // ensure that the TargetData of the transtition context is updated to a value that can be fed into
            // the dataToModel mapping function
            let! nextData = Machine.Run.transitionHandler onEnter transCtx
            // Open the view corresponding to this statemachine state. Note that it is important to wait until the
            // navigation is complete before yielding. Consider the following situation: 
            // If the state that we are entering is represented by a fragment, then it will likely have a parent
            // sttate that is represented by an activity.  We can't display (navigate to) the fragment unless the 
            // parent activity is available, since we need to place the fragment in the activity's FragmentManager.
            // So it is important for the view of a parent state to be fully ready before entering the child state.
            let! view = navigator.Navigate viewInfo |> Async.AwaitTask
            return nextData
         })

   let activity<'TActivity when 'TActivity :> Activity> 
      (stateId: StateId)
      (flags: option<ActivityFlags>)  =
      
      let _flags = defaultArg flags (ActivityFlags.NewTask ||| ActivityFlags.ClearTask)
      let navInfo = ActivityViewInfo (stateId, typeof<'TActivity>, _flags)
      ActivityStateView (navInfo, List.empty)

   let activityWithFragments<'TActivity when 'TActivity :> Activity> 
      (stateId: StateId)
      (flags: option<ActivityFlags>)
      (containerViewId: int)
      (childFragmentCreators: list<CreateFragmentStateView>) =
      
      let _flags = defaultArg flags (ActivityFlags.NewTask ||| ActivityFlags.ClearTask)
      let navInfo = ActivityViewInfo (stateId, typeof<'TActivity>, _flags)
      let childFragmentViews = 
         childFragmentCreators 
         |> List.map (fun (CreateFragmentStateView creator) -> creator id containerViewId)
      ActivityStateView (navInfo, childFragmentViews)

   let fragment<'TFragment when 'TFragment :> Fragment and 'TFragment : (new : unit -> 'TFragment)> 
      (stateId: StateId) =
      CreateFragmentStateView(fun (resolveFragManager: FragmentManager->FragmentManager) containerViewId ->
         let navInfo = FragmentViewInfo (stateId, typeof<'TFragment>, containerViewId, resolveFragManager)
         FragmentStateView (navInfo, List.empty))

   let fragmentWithFragments<'TFragment when 'TFragment :> Fragment and 'TFragment : (new : unit -> 'TFragment)> 
      (stateId: StateId)
      (containerViewId: int)
      (childFragmentCreators: list<CreateFragmentStateView>) =
      CreateFragmentStateView(fun (resolveFragManager: FragmentManager->FragmentManager) parentContainerViewId ->
         let navInfo = FragmentViewInfo (stateId, typeof<'TFragment>, parentContainerViewId, resolveFragManager)
         // Returns the fragment manager of fragment associated with this fragmen state view. Child fragments
         // for the child fragment state views can be placed in this fragment manager.
         let resolveFragManagerForChildren fragManager = 
            let fragManager = resolveFragManager fragManager
            let fragment = fragManager.FindFragmentByTag ("Fragment_" + stateId)
            if isNull fragment then
               invalidOp <| "Cant find fragment in fragment manager for state " + stateId
            fragment.ChildFragmentManager
         let childFragmentViews = 
            childFragmentCreators
            |> List.map (fun (CreateFragmentStateView creator) -> creator resolveFragManagerForChildren containerViewId)
         FragmentStateView (navInfo, childFragmentViews))

   let mixinViewTree
      (navigator: IViewNavigator) 
      (viewTree: StateViewTree) 
      (stateTree: StateTree<'D,'M>) : StateTree<'D,'M> = 

      // Descend the tree of fragment views, and add an OnEnter handler to the corresponding state that
      // will display the fragment when the state is entered.  
      let rec mixinFragmentViews (fragmentViews: list<FragmentStateView>) (stateTree: StateTree<'D,'M>) =
         fragmentViews
         |> List.fold (fun stateTree fragmentStateView -> 
            let viewInfo = fragmentStateView.ViewInfo
            stateTree 
            |> StateTree.Wrap.onEnter viewInfo.StateId (navigateOnEnter navigator (ViewInfo.Fragment viewInfo))
            |> mixinFragmentViews fragmentStateView.ChildViews
         ) stateTree

      viewTree 
      |> List.fold (fun stateTree activityStateView ->
         let viewInfo = activityStateView.ViewInfo
         stateTree 
         |> StateTree.Wrap.onEnter viewInfo.StateId (navigateOnEnter navigator (ViewInfo.Activity viewInfo))
         |> mixinFragmentViews activityStateView.ChildViews
      ) stateTree