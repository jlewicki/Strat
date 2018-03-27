namespace Strat.StateMachine

/// Describes the state and data that is being managed by a state machine.

[<NoEquality; NoComparison>]
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
with
   /// The state after the message was processed
   member NextState: State<'D,'M>
   /// The data after the message was processed
   member NextData: 'D



/// Describes how a message was processed (possibly unsuccessfully) by a state machine.
[<NoComparison>]
type MessageProcessed<'D,'M> = 
   /// A message was successfully handled.
   | HandledMessage of MessageHandled<'D,'M>
   /// A message was not handled by the current state, or any of its ancestor states.
   | UnhandledMessage of Message: 'M
   /// A message was recognized by the current state, or an ancestor state, but the message was not considered valid.
   | InvalidMessage of Reason: string * Code:option<int> * Message: 'M


/// Defines functions for processing messages with a hierarhical tree of states
module Machine =

   /// Yields a new state machine context resulting from entering the specified initial state (or root state if not 
   /// provided) with the specified initial data.
   val initializeContext: 
      stateTree: StateTree<'D,'M> -> 
      initialData:'D -> 
      initialStateId: option<StateId> ->  Async<StateMachineContext<'D,'M>>

   /// Yields the result of processing the message within the specified machine context.
   val processMessage:
      message: 'M ->
      context:StateMachineContext<'D,'M> -> Async<MessageProcessed<'D,'M>>

   /// <summary> 
   /// Yields the result of transitionining the specified state machine context to a stopped (terminal) state, 
   /// optionally providing a reason the machine is being stopped.
   /// </summary> 
   /// <exception cref="System.ArgumentException">If the context is already in a terminal state.</exception> 
   val stop: 
      smContext: StateMachineContext<'D,'M> -> 
      reason: option<StopReason> ->  Async<StateMachineContext<'D,'M>>


   /// Defines helper functions to run state handlers.
   module Run =
   
      /// Invokes the transition handler, and returns an async of the result.
      val inline transitionHandler: handler: TransitionHandler<'D,'M> -> transCtx: TransitionContext<'D,'M> -> Async<'D>

      /// Invokes the message handler, and returns an async of the result.
      val inline messageHandler: handler: MessageHandler<'D,'M> -> msgCtx: MessageContext<'D,'M> -> Async<MessageResult<'D,'M>>

      /// Invokes the initial transition function, and returns an async of the result.
      val inline initialTransition: initTransition: InitialTransition<'D> -> data: 'D -> Async<struct ('D * StateId)>