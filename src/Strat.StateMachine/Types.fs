namespace Strat.StateMachine

open System


/// Identifies a state in a state tree. The name must be unique within the tree.
type StateId = StateId of Name: string


/// Represents a state in a state machine that handles messages of the specified type and process data of the specified
/// type.
[<NoComparison; ReferenceEquality>]
type State<'Data,'Message> = 
   /// Root state of a state tree
   | Root of
      Id: StateId *
      Handlers: StateHandler<'Data,'Message> * 
      InitialTransition: InitialTransition<'Data>
   /// Interior state of a state tree (that is, neither a root or a leaf state)
   | Interior of
      Id: StateId * 
      Parent: State<'Data,'Message> * 
      Handlers: StateHandler<'Data,'Message> * 
      InitialTransition: InitialTransition<'Data>
   /// Leaf state in a state tree
   | Leaf of 
      Id: StateId * 
      Parent: State<'Data,'Message> * 
      Handlers: StateHandler<'Data,'Message>
   /// A terminal state. The state machine cannot transition out of this state. 
   | Terminal of 
      Parent: State<'Data,'Message> * 
      FromState: StateId *
      Reason: option<StopReason>
with 
   /// Gets the descriptive text associated with the ID of this state.
   member this.Name = 
      match this with
      | Root (StateId(name), _, _) -> name
      | Interior (StateId(name), _, _, _) -> name
      | Leaf (StateId(name), _, _) -> name
      | Terminal _ -> "TerminalState"
   member this.Id = 
      match this with
      | Root (id, _, _) -> id
      | Interior (id, _, _, _) -> id
      | Leaf (id, _, _) -> id
      | Terminal _ -> StateId "TerminalState"


/// Describes the contextual information available to states as they handle messages. 
and [<Struct; NoComparison; NoEquality>] MessageContext<'D,'M> = {
   /// The message that is being processed.
   Message: 'M
   /// The current state machine context.
   Data: 'D
} with 
   /// Returns a message result indicating that a transition to the specified state should occur, optionally updating
   /// the state data.
   member this.GoTo( nextState: StateId, ?nextData:'D, ?action: TransitionHandler<'D,'M>) =  
      let _nextData = defaultArg nextData this.Data
      MessageResult.Transition(nextState, _nextData, action)

   /// Returns a message result indicating that an internal transition should occur, optionally updating the state data.
   /// An internal transition means that the current state will not change, and no entry and exit handlers will be 
   /// called. 
   member this.Stay( ?nextData:'D ) = 
      let _nextData = defaultArg nextData this.Data
      MessageResult.InternalTransition(_nextData)

   /// Returns a message result indicating that an self-transition should occur, optionally updating the state data. A 
   /// self-transition means that the current state is exited and re-entered, calling the handler functions for the 
   /// state. By current, we mean the current state of the state machine, not necessarily the state (which could be an
   /// ancestor state of the current state) that calls this method.
   member this.GoToSelf(?nextData:'D, ?action: TransitionHandler<'D,'M>) =
      let _nextData = defaultArg nextData this.Data
      MessageResult.SelfTransition(_nextData, action)

   /// Returns a message result indicating the message could not be handled by a state, and that any ancestor states 
   /// should be given an opportunity to handle the message.
   member this.Unhandled() = 
      MessageResult.Unhandled

   /// Returns a message result indicating that the state machine should tranmsition to the terminated state, and 
   /// stop all further message processing.
   member this.Stop(?reason: string, ?code: int) = 
      let stopReason = 
         if reason.IsSome || code.IsSome then 
            let _reason = defaultArg reason ""
            let _code = defaultArg code -1
            Some({Reason = _reason; Code = _code})
         else None
      MessageResult.Stop(stopReason)


/// Describes the set of functions that define the behavior of a state.
and [<NoComparison; NoEquality>] StateHandler<'D,'M> = {
   /// Called when a message arrives that should be processed by the state.
   OnMessage: MessageHandler<'D,'M>
   /// Called when the state is being entered.
   OnEnter: TransitionHandler<'D,'M>
   /// Called when the state is being exited.
   OnExit: TransitionHandler<'D,'M>
}


/// Describes the function that is called to process a message, in both synchronous and asynchronous variations. The
/// function is passed a message context describing the message that arrived, and yields a message result describing
/// what state transition should occur (if any). 
and [<NoComparison; NoEquality>] MessageHandler<'D,'M> =
   | Sync of Handler: (MessageContext<'D,'M> -> MessageResult<'D,'M>) 
   | Async of Handler: (MessageContext<'D,'M> -> Async<MessageResult<'D,'M>>)


/// Describes the function that is called on entering or leaving a state, in both synchronous and asynchronous 
/// variations. The function is passed a transition context describing the transition taking place, and yields an 
/// updated data instance.
and [<NoComparison; NoEquality>] TransitionHandler<'D,'M> =
   | Sync of Handler: (TransitionContext<'D,'M> -> 'D) 
   | Async of Handler: (TransitionContext<'D,'M> -> Async<'D>)


/// Describes the function that is called when a composite state is being entered, in both synchronous and asynchronous 
/// variations. This function is passed the current data instabnce and yields an updated data instance and the child 
/// state that should become the current state.
and [<NoComparison; NoEquality>] InitialTransition<'D> =
   | Sync of Handler: ('D -> struct ('D * StateId))
   | Async of Handler: ('D -> Async<struct ('D * StateId)>)


/// Describes a state transition from a source state and data to a target state and data.
and [<Struct; NoComparison; NoEquality>] TransitionContext<'D,'M> = { 
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
and [<NoComparison; NoEquality>] MessageResult<'D,'M> = 
   | Transition of NextState:StateId * NextData:'D * Action:option<TransitionHandler<'D,'M>>
   | InternalTransition of NextData:'D
   | SelfTransition of NextData:'D * Action:option<TransitionHandler<'D,'M>>
   | Unhandled
   | Stop of Reason: option<StopReason>
   | InvalidMessage of Reason:string * Code:option<int>


/// Describes why state machine was stopped.
and [<NoComparison>] StopReason = {
   Reason: string
   Code: int
}


/// Describes a collection of states forming a hierarchical state tree.
[<NoComparison; NoEquality>]
type StateTree<'D,'M> = { 
   /// The root state in the state tree.
   Root: State<'D,'M> 
   /// Map of all states in the tree, keyed by state name.
   States: Map<StateId, Lazy<State<'D,'M>>>
}

/// Describes the state and data that is being managed by a state machine.
[<NoComparison>]
type StateMachineContext<'D,'M> = { 
   /// The current data for the state machine.
   Data: 'D
   /// The current state that will process incoming messages.
   State: State<'D,'M>
   /// The complete tree of states known to the state machine.
   StateTree: StateTree<'D,'M> 
}


/// Describes how a message was successfully handled by a state machine.
[<NoComparison>]
type MessageHandled<'D,'M> = {
   /// The message that was handled.
   Message: 'M
   /// The machine context before the message was handled.
   PrevContext: StateMachineContext<'D,'M>
   /// The machine context after the message was handled.
   NextContext: StateMachineContext<'D,'M>
   /// The states that were entered when the message was processed, in the order they were entered.
   EnteredStates: List<State<'D,'M>>
   /// The states that were exited when the message was processed, in the order they were exited.
   ExitedStates: List<State<'D,'M>>
}


/// Describes how a message was processed (possibly unsuccessfully) by a state machine.
[<NoComparison>]
type MessageProcessed<'D,'M> = 
   /// A message was successfully handled.
   | HandledMessage of MessageHandled<'D,'M>
   /// A message was not handled by the current state, or any of its ancestor states.
   | UnhandledMessage of Message: 'M
   /// A message was recognized by the current state, or an ancestor state, but the message was not considered valid.
   | InvalidMessage of Reason: string * Code:option<int> * Message: 'M