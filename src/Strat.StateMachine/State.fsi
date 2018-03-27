namespace Strat.StateMachine

/// Type that unique identifies a state, within the tree of states for a state machine.
type StateId = string


/// Represents a state, within the tree of states for a state machine, that handles messages of the specified type
/// and process data of the specified type.
type State<'Data,'Message> =
   /// Root state of a state tree
   | Root of 
      Id: StateId * 
      Handlers: StateHandler<'Data,'Message> * 
      InitialTransition: InitialTransition<'Data>
   /// Interior state of a state tree (that is, neither a root or a leaf state)
   | Interior of 
      Id: StateId * 
      ParentId: StateId * 
      Handlers: StateHandler<'Data,'Message> * 
      InitialTransition: InitialTransition<'Data>
   /// Leaf state in a state tree
   | Leaf of 
      Id: StateId * 
      ParentId: StateId * 
      Handlers: StateHandler<'Data,'Message>
   /// A terminal state. A state machine cannot transition out of this state. 
   | Terminal of 
      Parent: StateId * 
      FromState: StateId * 
      Reason: option<StopReason>

   static member TerminalStateId: StateId
   
   /// Gets the ID of this state. The ID must be unique within the tree of states for a state machine.
   member Id: StateId
   
   /// Gets the handler functions for this state.
   member Handler: StateHandler<'Data, 'Message>


/// The handling functions the define the behavior of a state.
and [<NoComparison; NoEquality>] 
   StateHandler<'D,'M> = { 
      /// Function that handles messages for the state.
      OnMessage: MessageHandler<'D,'M>
      /// Function that is invoked when the state is entered.
      OnEnter: TransitionHandler<'D,'M>
      /// Function that is invoked when the state is exited.
      OnExit: TransitionHandler<'D,'M> 
   }


/// Describes the function that is called to process a message, in both synchronous and asynchronous variations. The
/// function is passed a message context describing the message that arrived, and yields a message result describing
/// what state transition should occur (if any). 
and [<NoComparison; NoEquality>] 
   MessageHandler<'D,'M> =
   | Sync of Handler: (MessageContext<'D,'M> -> MessageResult<'D,'M>) 
   | Async of Handler: (MessageContext<'D,'M> -> Async<MessageResult<'D,'M>>)


/// Describes the function that is called on entering or leaving a state, in both synchronous and asynchronous
/// variations. The function is passed a transition context describing the transition taking place, and yields an
/// updated data instance. 
and [<NoComparison; NoEquality>] 
   TransitionHandler<'D,'M> =
   | Sync of Handler: (TransitionContext<'D,'M> -> 'D) 
   | Async of Handler: (TransitionContext<'D,'M> -> Async<'D>)


/// Describes the function that is called when an interior state is being entered, in both synchronous and asynchronous
/// variations. The function is passed the current data instabnce and yields an updated data instance and the child
/// state that should become the current state. 
and [<NoComparison; NoEquality>] 
   InitialTransition<'D> =
   | Sync of Handler: ('D -> struct ('D * StateId))
   | Async of Handler: ('D -> Async<struct ('D * StateId)>)


/// Describes the contextual information available to states as they handle messages.
and [<Struct; NoComparison; NoEquality>] MessageContext<'D,'M> =
   new: message:'M * data:'D -> MessageContext<'D,'M>
   /// The message to be processed b 
   member Message: 'M
   member Data: 'D

   /// Returns a message result indicating that a transition to the specified state should occur, optionally updating
   /// the state data.
   member GoTo: nextState: StateId * ?nextData:'D * ?action: TransitionHandler<'D,'M> -> MessageResult<'D,'M>

   /// Returns a message result indicating that an internal transition should occur, optionally updating the state data.
   /// An internal transition means that the current state will not change, and no entry and exit handlers will be 
   /// called. 
   member Stay: ?nextData:'D -> MessageResult<'D,'M>

   /// Returns a message result indicating that a self-transition should occur, optionally updating the state data. A 
   /// self-transition means that the current state is exited and re-entered, calling the handler functions for the 
   /// state. By current, we mean the current state of the state machine, not necessarily the state (which could be an
   /// ancestor state of the current state) that calls this method.
   member GoToSelf: ?nextData:'D * ?action: TransitionHandler<'D,'M> -> MessageResult<'D,'M>

   /// Returns a message result indicating the message could not be handled by a state, and that any ancestor states 
   /// should be given an opportunity to handle the message.
   member Unhandled: unit -> MessageResult<'D,'M>

   /// Returns a message result indicating that the state machine should transition to the terminal state, and 
   /// stop all further message processing.
   member Stop: ?reason: string * ?code: int -> MessageResult<'D,'M>


/// Describes a state transition from a source state and data to a target state and data. 
and [<Struct; NoComparison; NoEquality>] 
   TransitionContext<'D,'M> = { 
      /// The source state for the transition.
      SourceState: State<'D,'M>
      /// The state machine data when the transition started.
      SourceData: 'D
      /// The target state for the transition.
      TargetState: State<'D,'M> 
      /// The state machine data when the transition is complete.
      TargetData: 'D
      /// The state that is currently handling the transition. That is, the state for which OnEnter or OnExit is being
      /// called. Note that in the case of a transition action, which is called after exiting, and before entering, the
      /// value of HandlingState will be the least-common-ancestor state for the overall state transition.
      HandlingState: State<'D,'M>
   }


/// Describes the possible results when OnMessage is called on a state.
and [<NoComparison; NoEquality>] 
   MessageResult<'D,'M> = 
   /// A transition to the specified state should occur, optionally updating the state data.
   | Transition of NextState:StateId * NextData:'D * Action:option<TransitionHandler<'D,'M>>
   /// An internal transition should occur, optionally updating the state data. An internal transition means that the 
   /// current state will not change, and no entry and exit handlers will be called. 
   | InternalTransition of NextData:'D
   /// A self-transition should occur, optionally updating the state data. A self-transition means that the current
   /// state is exited and re-entered, calling the handler functions for the state.
   | SelfTransition of NextData:'D * Action:option<TransitionHandler<'D,'M>>
   /// The message could not be handled by a state, and that any ancestor states should be given an opportunity to
   /// handle the message.
   | Unhandled
   // The state machine should transition to the terminal state, and stop all further message processing.
   | Stop of Reason: option<StopReason>
   | InvalidMessage of Reason:string * Code:option<int>


/// Describes why the state machine was stopped.
and [<Sealed>] StopReason =
   new: reason: string * code: int -> StopReason
   member Reason: string
   member Code: int


/// Functional operations for State<'Data, 'Message>.
module State = 
   
   /// Returns the ID of the state.
   val inline id: state: State<'D,'M> -> StateId

   /// Returns the handler for the state.
   val inline handler: state: State<'D,'M> -> StateHandler<'D,'M>

   /// Returns the initial transition for the state, if available.
   val initialTransition: state: State<'D,'M> -> option<InitialTransition<'D>>


/// Defines default message handling functions
module Handlers =
  
   /// A no-op message handler that always yields an Unhandled message result.
   [<GeneralizableValue>]
   val emptyMessageHandler: MessageHandler<'D,'M>

   /// A no-op transition handler
   [<GeneralizableValue>]
   val emptyTransitionHandler: TransitionHandler<'D,'M>

   /// A state handler with no-op message and transition handlers.
   [<GeneralizableValue>]
   val emptyHandler: StateHandler<'D,'M>
   