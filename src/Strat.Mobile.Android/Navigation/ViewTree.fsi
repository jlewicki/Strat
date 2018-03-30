namespace Strat.Mobile.Android.Navigation

open System
open System.Threading.Tasks
open Android.App
open Android.Content
open Strat.StateMachine


/// Indicates that an activity should be started when a state has been entered.
 type ActivityViewInfo = 
   new: stateId: StateId * activityType: Type * flags: ActivityFlags -> ActivityViewInfo
   member StateId: StateId
   member ActivityType: Type 
   member Flags: ActivityFlags


/// Indicates that a fragment should be displayed when a state has been entered.
type FragmentViewInfo =
   new: stateId: StateId * fragmentType: Type * containerViewId: int * resolveFragManager: (FragmentManager->FragmentManager) -> FragmentViewInfo
   member StateId: StateId
   member FragmentType: Type
   member ContainerViewId: int
   member ResolveFragmentManager: parentFragmentManager:FragmentManager -> FragmentManager


type ActivityStateView
type CreateFragmentStateView


/// Represent the complete set of mappings between states in a state tree, and the activities and fragments that are shown 
/// as states are entered.
type StateViewTree = list<ActivityStateView>


/// Describes a view (activity or fragment) that can be used to display a state view.
type ViewInfo =
   | Activity of ActivityViewInfo
   | Fragment of FragmentViewInfo
with
   member StateId: StateId


/// Defines methods to navigate to a fragment of view 
type IViewNavigator =
   /// Navigates to the activity or fragment represented by the specified view info. The returned task completes when 
   /// the associated view has been displayed (that is, the activity has fully started, or the fragment transaction has
   /// been committed). The object that is returned from that task is the activity or fragment that was displayed/
   abstract Navigate: ViewInfo -> Task<obj>


module ViewTree = 

   /// Creates an ActivityStateView indicating that an activity of the specified type should be started when the state
   /// with the specified ID is entered.
   /// <param name="stateId">Id of the state that, when entered, the activity should be started.</param>
   /// <param name="flags">Activity flags indicating how the activity should be started</param>
   val activity<'TActivity when 'TActivity :> Activity> :
      stateId: StateId ->
      flags: option<ActivityFlags> -> ActivityStateView
  
   /// Creates an ActivityStateView indicating that an activity of the specified type should be started when the state
   /// with the specfified ID is entered. The activity will host fragments as descendent states are entered.
   /// <param name="stateId">Id of the state that, when entered, the activity should be started.</param>
   /// <param name="flags">Activity flags indicating how the activity should be started</param>
   /// <param name="containerViewId">The id of the view, in the activity's view, that will host the fragments 
   ///   corresponding to descendent states.</param>
   /// <param name="childFragmentCreators">List of functions indicating the mappings between the fragments to host, and
   ///   descendent states.</param>
   val activityWithFragments<'TActivity when 'TActivity :> Activity> :
      stateId: StateId ->
      flags: option<ActivityFlags> ->
      containerViewId: int -> 
      childFragmentCreators: list<CreateFragmentStateView> -> ActivityStateView
   
   /// Creates an CreateFragmentStateView indicating that a fragment of the specified type should be displayed when the
   /// state with the specified ID is entered. 
   /// <remarks>The fragment is hosted in an activity or fragment that has been displayed in response to an ancestor
   ///   state being entered.</remarks>
   /// <param name="stateId">Id of the state that, when entered, the the fragment should be displayed.</param>
   val fragment<'TFragment when 'TFragment :> Fragment and 'TFragment : (new : unit -> 'TFragment)> :
      stateId: StateId -> CreateFragmentStateView

   /// Creates an CreateFragmentStateView indicating that a fragment of the specified type should be displayed when the
   /// state with the specified ID is entered. The fragment will host fragments as descendent states are entered.
   /// <remarks>The fragment is hosted in an activity or fragment that has been displayed in response to an ancestor
   ///   state being entered.</remarks>
   /// <param name="stateId">Id of the state that, when entered, the the fragment should be displayed.</param>
   /// <param name="containerViewId">The id of the view, in the fragments's view, that will host the fragments 
   ///   corresponding to descendent states.</param>
   /// <param name="childFragmentCreators">List of functions indicating the mappings between the fragments to host, and
   ///   descendent states.</param>
   val fragmentWithFragments<'TFragment when 'TFragment :> Fragment and 'TFragment : (new : unit -> 'TFragment)> :
      stateId: StateId -> 
      containerViewId: int -> 
      childFragmentCreators: list<CreateFragmentStateView> -> CreateFragmentStateView

   /// Returns a new state tree by adding transition handlers to states in the tree that will start activities and
   /// fragments as the starts are entered.
   val mixinViewTree:
      navigator: IViewNavigator ->
      viewTree: StateViewTree ->
      stateTree: StateTree<'D,'M> -> StateTree<'D,'M>