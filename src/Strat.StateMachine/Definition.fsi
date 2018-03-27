namespace Strat.StateMachine.Definition

open Strat.StateMachine


/// Defines functions for defining the initial transition funcion for a state.
[<Sealed>]
type Start =
   /// Returns an InitialTransition that enters the specified child state when an parent state is entered.
   static member With: stateName: StateId -> InitialTransition<'D>


/// Defines methods to create a state handler from synchronoes and asynchronous handling functions.
[<Sealed>]
type Handle = 
   /// Returns a StateHandler with the specified handler functions. An empty no-op version of a handler will be 
   /// included if any of the handlers are not provided.
   static member With: 
      ?onMessage: MessageHandler<'D,'M> * 
      ?onEnter: TransitionHandler<'D,'M> * 
      ?onExit: TransitionHandler<'D,'M> -> StateHandler<'D,'M>


/// Provides methods for defining the states in a state tree
module StateTree =

   type CreateChildState<'D,'M>

   /// Constructs a new state tree with rooted at a state with the specified name, initial transition, state handler
   /// handlers, and child states. 
   val fromRoot: 
      id: StateId -> 
      handler: StateHandler<'D,'M> -> 
      initialTransition: InitialTransition<'D> -> 
      childStates: seq<CreateChildState<'D,'M>> -> StateTree<'D,'M>

   /// Constructs a new 'flat' state tree with a default 'no-op' root state, serving as a parent for the specified set
   /// of leaf states. The resulting tree effectively will behave as a 'traditional' state machine (that is, 
   /// non-hierarchical) 
   val fromLeaves: initialStateId: StateId -> states: seq<CreateChildState<'D,'M>> -> StateTree<'D,'M> 

   /// Defines a leaf state in a state tree that processes messages using the specified handler functions. 
   val leaf: id: StateId -> handler:StateHandler<'D,'M> -> CreateChildState<'D,'M>

   /// Defines an interior state in a state tree that processes messages using the specified handler functions. 
   val interior: 
      id: StateId -> 
      handler: StateHandler<'D,'M> -> 
      initialTransition: InitialTransition<'D> -> 
      childStates: seq<CreateChildState<'D,'M>> -> CreateChildState<'D,'M>