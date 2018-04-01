namespace Strat.Mobile.Android.Navigation

open System
open System.Reactive.Subjects
open System.Threading
open System.Threading.Tasks
open Android.App
open Android.Content
open Strat.StateMachine
open Strat.StateMachine.Definition
open Strat.UI


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


module ViewTree =

   type IViewModelFactory =
      abstract CreateViewModel: BehaviorSubject<'Model>-> 'ViewModel when 'ViewModel: not struct

   let navigateOnEnter 
      (navigator: IViewNavigator) 
      (viewInfo: ViewInfo) 
      (onEnter: TransitionHandler<_,_>) 
      (transCtx: TransitionContext<_,_>) = 
      async {
         // We first call the inner onEnter handler, if available. The expectation is that this handler will 
         // ensure that the TargetData of the transtition context is updated to a value that can be fed into
         // the dataToModel mapping function
         let! nextData = Machine.Run.transitionHandler onEnter transCtx
         // Open the view corresponding to this statemachine state. Note that it is important to wait until the
         // navigation is complete before yielding. Consider the following situation: 
         // If the state that we are entering is represented by a fragment, then it will likely have a parent
         // state that is represented by an activity.  We can't display (navigate to) the fragment unless the 
         // parent activity is available, since we need to place the fragment in the activity's FragmentManager.
         // So it is important for the view of a parent state to be fully ready before entering the child state.
         let! view = navigator.Navigate viewInfo |> Async.AwaitTask
         return view, nextData
      }

   let simpleNavigationHandler (navigator: IViewNavigator) (viewInfo: ViewInfo) (handler: StateHandler<'D,'M>) = 
      { handler with
         OnEnter = TransitionHandler.Async (fun transCtx -> async {
            let! _, nextData = navigateOnEnter navigator viewInfo handler.OnEnter transCtx 
            return nextData
         } ) }

   let mvvmNavigationHandler<'D, 'M, 'View, 'ViewModel when 'View :> IView<'ViewModel> and 'ViewModel: not struct>
      (uiSyncContext: SynchronizationContext) 
      (viewNavigator: IViewNavigator) 
      (viewModelFactory: IViewModelFactory)
      (viewInfo: ViewInfo)
      (handler: StateHandler<'D,'M>) =
         let mutable model : BehaviorSubject<'D> = null
         { OnEnter = TransitionHandler.Async(fun transCtx -> 
            async {
               let! oView, nextData = navigateOnEnter viewNavigator viewInfo handler.OnEnter transCtx
               // Convert state machine context data to a model. This model should be the 'root data' for the view
               // model. The view model may expose values derived from this root state, but user commands can only
               // affect this root data.
               //let initModel = nextData |> dataToModel
               model <- new BehaviorSubject<'D> (nextData)
               // Build a view model for the view, handing it the root state
               let viewModel = viewModelFactory.CreateViewModel model
               // Hand the view model to the view, doing so on the UI thread.
               do! Async.SwitchToContext uiSyncContext
               (oView :?> 'View).SetViewModel viewModel
               do! Async.SwitchToThreadPool ()
               return nextData
            })
           OnMessage = MessageHandler.Async (fun ctx -> 
            async {
               let! msgResult = Machine.Run.messageHandler handler.OnMessage ctx
               match msgResult with
               | InternalTransition(nextData) -> 
                  // If an internal transition was performed, yield a new 'root data' value. The view model should observe
                  // this new value, and UI appropriately.
                  do! Async.SwitchToContext uiSyncContext
                  //model.OnNext (nextData |> dataToModel)
                  model.OnNext nextData
                  do! Async.SwitchToThreadPool ()
               | _ -> ()
               return msgResult 
           })
           OnExit = TransitionHandler.Async (fun ctx -> async {
               // We no longer need the subject that yields the model, since we are exiting the state.
               model.OnCompleted()
               model.Dispose()
               return! Machine.Run.transitionHandler handler.OnExit ctx
           })
         }


   [<AbstractClass>]
   type FragmentStateView(viewInfo: FragmentViewInfo, childViews: list<FragmentStateView>) =
      member this.ViewInfo = viewInfo
      member this.ChildViews = childViews
      abstract CreateHandlerWrapper: 
         SynchronizationContext * IViewNavigator * IViewModelFactory -> (StateHandler<'D,'M> -> StateHandler<'D,'M>)


   type SimpleFragmentStateView (viewInfo: FragmentViewInfo, childViews: list<FragmentStateView>) =
      inherit FragmentStateView(viewInfo, childViews)
      override this.CreateHandlerWrapper (_, navigator, _) = 
        simpleNavigationHandler navigator (ViewInfo.Fragment viewInfo)


   type MvvmFragmentStateView (viewInfo: FragmentViewInfo, childViews: list<FragmentStateView>) =
      inherit FragmentStateView(viewInfo, childViews)
      override this.CreateHandlerWrapper (uiSyncContext, navigator, viewModelFactory) = 
        mvvmNavigationHandler uiSyncContext navigator viewModelFactory (ViewInfo.Fragment viewInfo)


   [<AbstractClass>]
   type ActivityStateView(viewInfo: ActivityViewInfo, childViews: list<FragmentStateView>) =
      member this.ViewInfo = viewInfo
      member this.ChildViews = childViews
      abstract CreateHandlerWrapper: 
         SynchronizationContext * IViewNavigator * IViewModelFactory -> (StateHandler<'D,'M> -> StateHandler<'D,'M>)


   type SimpleActivityStateView (viewInfo: ActivityViewInfo, childViews: list<FragmentStateView>) =
      inherit ActivityStateView(viewInfo, childViews)
      override this.CreateHandlerWrapper (_, navigator, _) = 
         simpleNavigationHandler navigator (ViewInfo.Activity viewInfo)


   type MvvmActivityStateView (viewInfo: ActivityViewInfo, childViews: list<FragmentStateView>) =
      inherit ActivityStateView(viewInfo, childViews)
      override this.CreateHandlerWrapper (uiSyncContext, navigator, viewModelFactory) = 
        mvvmNavigationHandler uiSyncContext navigator viewModelFactory (ViewInfo.Activity viewInfo)


   type CreateFragmentStateView = 
      | CreateFragmentStateView of ((FragmentManager->FragmentManager) -> int -> FragmentStateView)


   type StateViewTree = list<ActivityStateView>


   let newActivityInfo<'Activity when 'Activity :> Activity>
      (stateId: StateId)
      (flags: option<ActivityFlags>) =
         let _flags = defaultArg flags (ActivityFlags.NewTask ||| ActivityFlags.ClearTask)
         ActivityViewInfo (stateId, typeof<'Activity>, _flags)


   let activityWithFragmentsCore<'TActivity when 'TActivity :> Activity> 
      (createStateView: ActivityViewInfo * list<FragmentStateView> -> ActivityStateView)
      (stateId: StateId)
      (flags: option<ActivityFlags>)
      (containerViewId: int)
      (childFragmentCreators: list<CreateFragmentStateView>) =
         let navInfo = newActivityInfo<'TActivity> stateId flags
         let childFragmentViews = 
            childFragmentCreators 
            |> List.map (fun (CreateFragmentStateView creator) -> creator id containerViewId)
         createStateView (navInfo, childFragmentViews) 


   let fragmentWithFragmentsCore<'TFragment when 'TFragment :> Fragment and 'TFragment : (new : unit -> 'TFragment)> 
      (createStateView: FragmentViewInfo * list<FragmentStateView> -> FragmentStateView)
      (stateId: StateId)
      (containerViewId: int)
      (childFragmentCreators: list<CreateFragmentStateView>) =
      CreateFragmentStateView (fun resolveFragManager parentContainerViewId ->
         let navInfo = FragmentViewInfo (stateId, typeof<'TFragment>, parentContainerViewId, resolveFragManager)
         // Returns the fragment manager of fragment associated with this fragment state view. Child fragments
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
         createStateView (navInfo, childFragmentViews))


   let activity<'TActivity when 'TActivity :> Activity> (stateId: StateId) (flags: option<ActivityFlags>) =
      let navInfo = newActivityInfo<'TActivity> stateId flags
      SimpleActivityStateView (navInfo, List.empty) :> ActivityStateView


   let mvvmActivity<'Model, 'Activity, 'ViewModel when 'Activity :> Activity and 'Activity :> IView<'ViewModel> and 'ViewModel: not struct>
      (stateId: StateId)
      (flags: option<ActivityFlags>) =
      let navInfo = newActivityInfo<'Activity> stateId flags
      MvvmActivityStateView (navInfo, List.empty) :> ActivityStateView


   let activityWithFragments<'TActivity when 'TActivity :> Activity> 
      (stateId: StateId)
      (flags: option<ActivityFlags>)
      (containerViewId: int)
      (childFragmentCreators: list<CreateFragmentStateView>) =
         let createStateView (avi, fsvs) = SimpleActivityStateView (avi, fsvs) :> ActivityStateView
         activityWithFragmentsCore createStateView stateId flags containerViewId childFragmentCreators


   let mvvmActivityWithFragments<'Model, 'Activity, 'ViewModel when 'Activity :> Activity and 'Activity :> IView<'ViewModel> and 'ViewModel: not struct>
      (stateId: StateId)
      (flags: option<ActivityFlags>)
      (containerViewId: int)
      (childFragmentCreators: list<CreateFragmentStateView>) =
         let createStateView (avi, fsvs) = MvvmActivityStateView (avi, fsvs) :> ActivityStateView
         activityWithFragmentsCore createStateView stateId flags containerViewId childFragmentCreators
   

   let fragment<'TFragment when 'TFragment :> Fragment and 'TFragment : (new : unit -> 'TFragment)> 
     (stateId: StateId) =
     CreateFragmentStateView (fun resolveFragManager containerViewId ->
        let navInfo = FragmentViewInfo (stateId, typeof<'TFragment>, containerViewId, resolveFragManager)
        SimpleFragmentStateView (navInfo, List.empty) :> FragmentStateView)


   let mvvmFragment<'Model, 'Fragment, 'ViewModel when 'Fragment :> Fragment and 'Fragment : (new : unit -> 'Fragment) and 'Fragment :> IView<'ViewModel> and 'ViewModel: not struct> 
      (stateId: StateId) =
      CreateFragmentStateView (fun resolveFragManager containerViewId ->
         let navInfo = FragmentViewInfo (stateId, typeof<'Fragment>, containerViewId, resolveFragManager)
         MvvmFragmentStateView (navInfo, List.empty) :> FragmentStateView)


   let fragmentWithFragments<'TFragment when 'TFragment :> Fragment and 'TFragment : (new : unit -> 'TFragment)> 
      (stateId: StateId)
      (containerViewId: int)
      (childFragmentCreators: list<CreateFragmentStateView>) =
         let createStateView (fvi, fsvs) = SimpleFragmentStateView (fvi, fsvs) :> FragmentStateView
         fragmentWithFragmentsCore createStateView stateId containerViewId childFragmentCreators
  

   let mvvmFragmentWithFragments<'Model, 'Fragment, 'ViewModel when 'Fragment :> Fragment and 'Fragment : (new : unit -> 'Fragment) and 'Fragment :> IView<'ViewModel> and 'ViewModel: not struct> 
      (stateId: StateId)
      (containerViewId: int)
      (childFragmentCreators: list<CreateFragmentStateView>) =
         let createStateView (fvi, fsvs) = MvvmFragmentStateView (fvi, fsvs) :> FragmentStateView
         fragmentWithFragmentsCore createStateView stateId containerViewId childFragmentCreators


   let mixinViewTree
      (uiSyncContext: SynchronizationContext)
      (navigator: IViewNavigator) 
      (viewModelFactory: IViewModelFactory) 
      (viewTree: StateViewTree) 
      (stateTree: StateTree<'D,'M>) : StateTree<'D,'M> = 

      // Descend the tree of fragment views, and add an OnEnter handler to the corresponding state that
      // will display the fragment when the state is entered.  
      let rec mixinFragmentViews (fragmentViews: list<FragmentStateView>) (stateTree: StateTree<'D,'M>) =
         fragmentViews
         |> List.fold (fun stateTree fragmentStateView -> 
            let viewInfo = fragmentStateView.ViewInfo
            let handlerWrapper = fragmentStateView.CreateHandlerWrapper(uiSyncContext, navigator, viewModelFactory)
            stateTree 
            |> StateTree.Wrap.handler viewInfo.StateId handlerWrapper
            |> mixinFragmentViews fragmentStateView.ChildViews
         ) stateTree

      viewTree 
      |> List.fold (fun stateTree activityStateView ->
         let viewInfo = activityStateView.ViewInfo
         let handlerWrapper = activityStateView.CreateHandlerWrapper(uiSyncContext, navigator, viewModelFactory)
         stateTree 
         |> StateTree.Wrap.handler viewInfo.StateId handlerWrapper
         |> mixinFragmentViews activityStateView.ChildViews
      ) stateTree