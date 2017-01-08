namespace Strat.StateMachine

open System


/// Names a state. This name must be unique within a state tree.
type StateName = StateName of Name:string
with
   static member Terminal = StateName "Terminal"


/// Represents a state in a state machine that handles messages of the specified type and process data of the specified
/// type.
type State<'Data,'Message> = 
   /// Root state of a state tree
   | Root of
      Name: StateName *
      Handlers: StateHandler<'Data,'Message> * 
      InitialTransition: InitialTransition<'Data>
   /// Intermediate state of a state tree (that is, neither a root or a leaf state)
   | Intermediate of
      Name: StateName * 
      Parent: State<'Data,'Message> * 
      Handlers: StateHandler<'Data,'Message> * 
      InitialTransition: InitialTransition<'Data>
   /// Leaf state in a state tree
   | Leaf of 
      Name: StateName * 
      Parent: State<'Data,'Message> * 
      Handlers: StateHandler<'Data,'Message>
   /// A terminal state. 
   | Terminal of 
      Parent: State<'Data,'Message> * 
      FromState: StateName *
      Reason: option<StopReason>
with 
   /// Gets the name of this state. The name is unique within the state tree managed by a <c>StateMachine<_,_></c>.
   member this.Name = 
      match this with
      | Root(name,_,_) -> name
      | Intermediate(name,_,_,_) -> name
      | Leaf(name,_,_) -> name
      | Terminal(_) -> StateName.Terminal


/// Describes the set of functions that define the behavior of a state.
and StateHandler<'D,'M> = {
   /// Called when a message arrives that should be processed by the state.
   OnMessage: MessageHandler<'D,'M>
   /// Called when the state is being entered.
   OnEnter: TransitionHandler<'D,'M>
   /// Called when the state is being exited.
   OnExit: TransitionHandler<'D,'M>
}


/// Describes the function that is called when a state handles a message.
and MessageHandler<'D,'M> = 
   MessageContext<'D,'M> -> Async<MessageResult<'D,'M>>


/// Describes functions that are called when a transition occurs between two states. These function can transform the context
/// data as the transition progresses.
and TransitionHandler<'D,'M> = 
   TransitionContext<'D,'M> -> Async<'D> 


/// Describes the contextual information available to states as they handle messages. 
and MessageContext<'D,'M> = {
   /// The message that is being processed.
   Message: 'M
   /// The current state machine context.
   Data: 'D
}
with 
   /// Returns a message result indicating that a transition to the specified state should occur, optionally updating
   /// the state data.
   member this.GoTo( nextState: StateName, ?nextData:'D, ?action: TransitionHandler<'D,'M>) =  
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
   /// state.
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


/// Describes a state transition from a source state and data to a target state and data.
and TransitionContext<'D,'M> = { 
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


/// Describes a function that is called when a composite state is being entered. This function determines the data and
/// child state that should become the current state.
and InitialTransition<'D> = 
   'D -> Async<'D * StateName>


/// Describes the possible results when OnMessage is called on a state.
and MessageResult<'D,'M> = 
   | Transition of NextState:StateName * NextData:'D * Action:option<TransitionHandler<'D,'M>>
   | InternalTransition of NextData:'D
   | SelfTransition of NextData:'D * Action:option<TransitionHandler<'D,'M>>
   | Unhandled
   | Stop of Reason: option<StopReason>
   | InvalidMessage of Reason:string * Code:option<int>


/// Describes why state machine was stopped.
and StopReason = {
   Reason: string
   Code: int
}


/// Describes a collection of states forming a hierarchical state tree.
type StateTree<'D,'M> = { 
   /// The root state in the state tree.
   Root: State<'D,'M> 
   /// Map of all states in the tree, keyed by state name.
   States: Map<StateName, Lazy<State<'D,'M>>>
}


/// Describes the state and data that is being managed by a state machine.
type StateMachineContext<'D,'M> = { 
   /// The current data for the state machine.
   Data: 'D
   /// The current state that will process incoming messages.
   State: State<'D,'M>
   /// The complete tree of states known to the state machine.
   StateTree: StateTree<'D,'M> 
}


/// Describes how a message was successfully handled by a state machine.
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
type MessageProcessed<'D,'M> = 
   /// A message was successfully handled.
   | HandledMessage of MessageHandled<'D,'M>
   /// A message was not handled by the current state, or any of its ancestor states.
   | UnhandledMessage of Message: 'M
   /// A message was recognized by the current state, or an ancestor state, but the message was not considered valid.
   | InvalidMessage of Reason: string * Code:option<int> * Message: 'M
 