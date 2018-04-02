namespace Strat.Mobile.Android.Navigation

open System
open System.Threading.Tasks
open Android.App
open Android.Content
open Strat.StateMachine
open Strat.UI


/// Describes the activity should be started when a state has been entered.
[<Class>]
 type internal ActivityViewInfo = 
   member StateId: StateId
   member ActivityType: Type 
   member Flags: ActivityFlags


/// Describes the fragment should be displayed when a state has been entered.
[<Class>]
type internal FragmentViewInfo =
   member StateId: StateId
   member FragmentType: Type
   member ContainerViewId: int
   member ResolveFragmentManager: parentFragmentManager:FragmentManager -> FragmentManager


/// Describes a view (activity or fragment) that can be used to display a state view.
type ViewInfo =
   | Activity of ActivityViewInfo
   | Fragment of FragmentViewInfo
with
   member StateId: StateId


/// Defines methods to navigate to a view (that is, an activity or fragment) that provides a visualization of a state 
/// in a state tree.
type IViewNavigator =
   /// Navigates to the activity or fragment represented by the specified view info. The returned task completes when 
   /// the associated view has been displayed (that is, the activity has fully started, or the fragment transaction has
   /// been committed). The object that is returned from that task is the activity or fragment that was displayed/
   abstract Navigate: ViewInfo -> Task<obj>


/// <summary>
///   Defines functions for mapping the states in a state tree to the activities and fragments that provide
///   visualizations of the states.</summary>
/// <remarks>
///   Each state can be visualized with an activity or a fragment. The activities and fragments can be thought of as
///   forming a forest, since each activity can contain 0 or more fragments, and each fragment can contain 0 or more
///   child fragments. Thus each tree in the forest is rooted by an activity, and the descendants of the root are 
///   fragments.
///   <para>
///   This forest of activities and fragents can be superimposed on a state tree. In other words, states in the state 
///   tree are mapped to activities and fragments in the view forest, such that the hierachies in each tree are 
///   isomorphic. In other words if state B is a descendent of state A in the state tree, then the view mapped to 
///   state B must be a descendent of the view mapped to state A in the view tree.
/// </remarks>
module ViewTree =

   open System.Threading
   open System.Reactive.Subjects

   type IViewModelFactory<'Model> =
      abstract CreateViewModel: BehaviorSubject<'Model>-> 'ViewModel when 'ViewModel: not struct

   [<AbstractClass>]
   type ActivityStateView<'D,'M> = 
      abstract CreateHandlerWrapper: 
         SynchronizationContext * IViewNavigator * IViewModelFactory<'D> -> (StateHandler<'D,'M> -> StateHandler<'D,'M>)

   type CreateFragmentStateView<'D,'M>

   /// Represent the complete set of mappings between states in a state tree, and the activities and fragments that are shown 
   /// as states are entered.
   type StateViewTree<'D,'M> = list<ActivityStateView<'D,'M>>

   [<Class>]
   type Builder<'D, 'M> = 

      /// Creates an ActivityStateView indicating that an activity of the specified type should be started when the state
      /// with the specified ID is entered.
      /// <param name="stateId">Id of the state that, when entered, the activity should be started.</param>
      /// <param name="flags">Activity flags indicating how the activity should be started</param>
      member Activity<'Activity when 'Activity :> Activity> : 
         stateId: StateId *
         flags: option<ActivityFlags> -> ActivityStateView<'D,'M>

      /// Creates an ActivityStateView indicating that an activity of the specified type should be started when the state
      /// with the specified ID is entered. The activity will host fragments as descendent states are entered.
      /// <param name="stateId">Id of the state that, when entered, the activity should be started.</param>
      /// <param name="flags">Activity flags indicating how the activity should be started</param>
      /// <param name="containerViewId">The id of the view, in the activity's view, that will host the fragments 
      ///   corresponding to descendent states.</param>
      /// <param name="childFragmentCreators">List of functions indicating the mappings between the fragments to host, and
      ///   descendent states.</param>
      member ActivityWithFragments<'Activity when 'Activity :> Activity> :
         stateId: StateId *
         flags: option<ActivityFlags> *
         containerViewId: int *
         childFragmentCreators: list<CreateFragmentStateView<'D,'M>> -> ActivityStateView<'D,'M>

      /// <summary>
      ///   Creates an CreateFragmentStateView indicating that a fragment of the specified type should be displayed
      ///   when the state with the specified ID is entered.</summary>
      /// <remarks>The fragment is hosted in an activity or fragment that has been displayed in response to an ancestor
      ///   state being entered.</remarks>
      /// <param name="stateId">
      ///   Id of the state that, when entered, the the fragment should be displayed.</param>
      member Fragment<'Fragment when 'Fragment :> Fragment and 'Fragment : (new : unit -> 'Fragment)> :
         stateId: StateId -> CreateFragmentStateView<'D,'M>
      
      /// <summary>
      ///   Creates an CreateFragmentStateView indicating that a fragment of the specified type should be displayed 
      ///   when the state with the specified ID is entered. The fragment will host fragments as descendent states are
      ///   entered.</summary>
      /// <remarks>
      ///   The fragment is hosted in an activity or fragment that has been displayed in response to an ancestor
      ///   state being entered.</remarks>
      /// <param name="stateId">
      ///   Id of the state that, when entered, the the fragment should be displayed.</param>
      /// <param name="containerViewId">
      ///   The id of the view, in the fragments's view, that will host the fragments corresponding to descendent 
      ///   states.</param>
      /// <param name="childFragmentCreators">
      ///   List of functions indicating the mappings between the fragments to host, and descendent states.</param>
      member FragmentWithFragments<'Fragment when 'Fragment :> Fragment and 'Fragment : (new : unit -> 'Fragment)> :
         stateId: StateId *
         containerViewId: int * 
         childFragmentCreators: list<CreateFragmentStateView<'D,'M>> -> CreateFragmentStateView<'D,'M>

      /// <summary>
      ///   Creates an ActivityStateView indicating that an activity of the specified type should be started when the
      ///   state with the specified ID is entered. Additionally, when the state is entered, a view model will be
      ///   created and initialized with the current data of the statew machine, and assigned to the activity.</summary>
      /// <param name="stateId">
      ///   Id of the state that, when entered, the activity should be started.</param>
      /// <param name="flags">
      ///   Activity flags indicating how the activity should be started</param>
      member MvvmActivity<'Activity, 'ViewModel when 'Activity :> Activity and 'Activity :> IView<'ViewModel> and 'ViewModel: not struct> :
         stateId: StateId *
         flags: option<ActivityFlags> -> ActivityStateView<'D,'M>
      
      member MvvmActivityWithFragments<'Activity, 'ViewModel when 'Activity :> Activity and 'Activity :> IView<'ViewModel> and 'ViewModel: not struct> :
         stateId: StateId *
         flags: option<ActivityFlags> * 
         containerViewId: int *
         childFragmentCreators: list<CreateFragmentStateView<'D,'M>> -> ActivityStateView<'D,'M>
      
      /// <summary>
      ///   Creates an CreateFragmentStateView indicating that a fragment of the specified type should be displayed
      ///   when the state with the specified ID is entered. Additionally, when the state is entered, a view model will
      ///   be created and initialized with the current data of the statew machine, and assigned to the 
      ///   fragment.</summary>
      /// <remarks>The fragment is hosted in an activity or fragment that has been displayed in response to an ancestor
      ///   state being entered.</remarks>
      /// <param name="stateId">
      ///   Id of the state that, when entered, the the fragment should be displayed.</param>
      member MvvmFragment<'Fragment, 'ViewModel when 'Fragment :> Fragment and 'Fragment : (new : unit -> 'Fragment) and 'Fragment :> IView<'ViewModel> and 'ViewModel: not struct> :
         stateId: StateId -> CreateFragmentStateView<'D,'M>
      
      member MvvmFragmentWithFragments<'Fragment, 'ViewModel when 'Fragment :> Fragment and 'Fragment : (new : unit -> 'Fragment) and 'Fragment :> IView<'ViewModel> and 'ViewModel: not struct> :
         stateId: StateId *
         containerViewId: int * 
         childFragmentCreators: list<CreateFragmentStateView<'D,'M>> -> CreateFragmentStateView<'D,'M>
  

   [<GeneralizableValue>]
   val Build<'D,'M> : Builder<'D, 'M>


   /// Returns a new state tree by adding transition handlers to states in the tree that will start activities and
   /// fragments as the starts are entered.
   val mixinViewTree:
      uiSyncContext: SynchronizationContext ->
      navigator: IViewNavigator ->
      viewModelFactory: IViewModelFactory<'D> ->
      viewTree: StateViewTree<'D,'M> ->
      stateTree: StateTree<'D,'M> -> StateTree<'D,'M>
