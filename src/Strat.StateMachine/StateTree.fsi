namespace Strat.StateMachine

/// Represents a hierarchical tree of state-machine states.
[<Sealed>]
type StateTree<'D,'M>


/// Funtions for working with StateTree<'D, 'M> instances.
module StateTree = 

   /// Returns the root state for the state tree.
   val rootState: stateTree: StateTree<'D,'M> -> State<'D, 'M>

   /// Returns the parent state of the specified state, if available. The root state for the state tree will not have
   /// a parent.
   val parentState: state: State<'D, 'M> -> stateTree: StateTree<'D,'M> -> option<State<'D, 'M>>

   /// Returns the parent state of the state with the specified, if available. The root state for the state tree will
   /// not have a parent.
   val parentStateById: stateId: StateId -> stateTree: StateTree<'D,'M> -> option<State<'D, 'M>>

   /// Returns a list of all the ancestor states (in upwards order) of the state with the specified ID.
   val ancestorStatesById: stateId: StateId -> stateTree: StateTree<'D,'M> -> list<State<'D, 'M>>

   /// Returns a list of containing the state with the specified id, followed by all of its ancestor states (in upwards
   /// order).
   val selfAndAncestorStatesById: stateId: StateId -> stateTree: StateTree<'D,'M> -> list<State<'D, 'M>>
      
   /// Returns a value indicating if the specified target state is the same state, or an ancestor state, of the state
   /// with the specified id.
   val isSelfOrAncestor: stateId: StateId -> targetStateId: StateId -> stateTree: StateTree<'D,'M> -> bool

   /// Returns the state with the specified ID, throwing an error if the state does not exist.
   val findState: stateId: StateId -> stateTree: StateTree<'D,'M> -> State<'D,'M>

   /// Returns the state with the specified ID, or None if the state does not exist. 
   val tryFindState: stateId: StateId -> stateTree: StateTree<'D,'M> -> option<State<'D,'M>>


   /// Provides functions for creating and adding states to a state tree. Note that the Strat.StateMachine.Definition
   /// namespace contains more convenient ways of definining the states and creating the tree.
   module Build =

      /// Creates a new tree containing a single root state
      val newTree: rootId:StateId * rootHandler: StateHandler<'D,'M> * initialTransition: InitialTransition<'D> -> StateTree<'D,'M>

      /// Adds the specified state a to the state tree
      val addState: state: State<'D,'M> -> stateTree: StateTree<'D,'M> -> StateTree<'D,'M>
  

   /// Provides methods for modifying the behavior of states in a state tree, by wrapping one or more of thir handler 
   /// functions.
   module Wrap =
  
      /// Updates the state handler of the specified state by applying the specfified function, and returns an updated
      /// state tree. The new handler should invoke the original handler as part of its execution.
      val handler: stateId: StateId -> wrap: (StateHandler<'D,'M> -> StateHandler<'D,'M>) -> stateTree: StateTree<'D,'M> -> StateTree<'D,'M> 

      /// Updates the OnEnter handler of the specified state by applying the specfified function, and returns an updated
      /// state tree. The new handler should invoke the original handler as part of its execution.
      val onEnter: stateId: StateId -> wrap:(TransitionHandler<'D,'M> -> TransitionHandler<'D,'M>) -> stateTree: StateTree<'D,'M> -> StateTree<'D,'M> 

      /// Updates the OnExit handler of the specified state by applying the specfified function, and returns an updated
      /// state tree. The new handler should invoke the original handler as part of its execution.
      val onExit: stateId: StateId -> wrap:(TransitionHandler<'D,'M> -> TransitionHandler<'D,'M>) -> stateTree: StateTree<'D,'M> -> StateTree<'D,'M>