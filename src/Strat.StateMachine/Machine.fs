namespace Strat.StateMachine

open System


type [<NoEquality; NoComparison>] StateMachineContext<'D,'M> = { 
   Data: 'D
   State: State<'D,'M>
   StateTree: StateTree<'D,'M> 
}


[<NoComparison>]
type MessageHandled<'D,'M> = {
   Message: 'M
   PrevContext: StateMachineContext<'D,'M>
   NextContext: StateMachineContext<'D,'M>
   EnteredStates: List<State<'D,'M>>
   ExitedStates: List<State<'D,'M>>
} with
   member this.NextState = this.NextContext.State
   member this.NextData = this.NextContext.Data


[<NoComparison>]
type MessageProcessed<'D,'M> = 
   | HandledMessage of MessageHandled<'D,'M>
   | UnhandledMessage of Message: 'M
   | InvalidMessage of Reason: string * Code:option<int> * Message: 'M


module Machine =

   module Run =
      let inline transitionHandler (handler: TransitionHandler<_,_>) (transCtx: TransitionContext<_,_>) = 
         match handler with 
         | TransitionHandler.Sync handler -> async.Return (handler transCtx) 
         | TransitionHandler.Async handler -> handler transCtx 

      let inline messageHandler (handler: MessageHandler<_,_>) (msgCtx: MessageContext<_,_>) = 
         match handler with 
         | MessageHandler.Sync handler -> async.Return (handler msgCtx) 
         | MessageHandler.Async handler -> handler msgCtx 

      let inline initialTransition (initTransition: InitialTransition<'D>) (data: 'D) = 
         match initTransition with 
         | InitialTransition.Sync handler -> async.Return (handler data) 
         | InitialTransition.Async handler -> handler data 


   /// Describes the target state of a transition
   type TransitionTarget<'D,'M> = 
      /// Transition is an internal.  The current state will not change, and no entry or exit handlers will be called.
      | CurrentState
      /// Transition is to a different state, or a self transition (i.e the current state is exited and re-entered).
      | OtherState of State<'D,'M>


   // Helper function to construct TransitionContext record
   let mkTransitionContext sourceData sourceState targetData transTarget handlingState: TransitionContext<_,_> = 
      let targetState = 
         match transTarget with 
         | CurrentState -> sourceState 
         | OtherState targetState -> targetState
      { SourceData = sourceData 
        SourceState = sourceState
        TargetData = targetData
        TargetState = targetState
        HandlingState = handlingState }


   // Helper function to construct MessageProcessed record
   let private mkMessageProcessed msgResult message prevMachineContext nextMachineContext exitingStates enteringStates = 
      match msgResult with
      | MessageResult.Unhandled -> 
         MessageProcessed.UnhandledMessage message
      | MessageResult.InvalidMessage (reason, code) -> 
         MessageProcessed.InvalidMessage (reason, code, message)
      | _ ->
         MessageProcessed.HandledMessage 
            { Message = message;
              PrevContext = prevMachineContext
              NextContext = nextMachineContext
              ExitedStates = List.ofSeq exitingStates 
              EnteredStates = List.ofSeq enteringStates }


   let inline withTransContextTransform transform transHandler = 
      match transHandler with
      | TransitionHandler.Sync handle -> TransitionHandler.Sync (transform >> handle)
      | TransitionHandler.Async handle -> TransitionHandler.Async (transform >> handle)

   let inline withHandlingState state  = 
      fun transCtx -> { transCtx with HandlingState = state }
 
     
   let onEnterWithHandlingState state : TransitionHandler<_,_> =
      let onEnter = (State.handler state).OnEnter
      onEnter |> withTransContextTransform (withHandlingState state)


   let onExitWithHandlingState state : TransitionHandler<_,_> =
      let onExit = (State.handler state).OnExit
      onExit |> withTransContextTransform (withHandlingState state)

   
   // Determines the path between the two states, and returns the list of states exited, the least common ancestor
   // state, and the list of states entered 
   let statePath (state1: State<'D,'M>) (state2: State<'D,'M>) (stateTree: StateTree<'D, 'M>) = 
      let ancestors1 = StateTree.ancestorStatesById state1.Id stateTree
      let ancestors2 =
         match state2 with
         | Terminal (parentId, _, _) -> 
            // Terminal state is 'transient' and not representd in the state tree, but its parent is,
            // so we that to calculate the ancestor path.
            StateTree.selfAndAncestorStatesById parentId stateTree
         | _ -> 
            StateTree.ancestorStatesById state2.Id stateTree
      let lca = 
         Seq.zip (ancestors1 |> List.rev) (ancestors2 |> List.rev)
         |> Seq.takeWhile (Object.Equals)
         |> Seq.last
         |> fst
      let notLca s = not (Object.Equals (s,lca))
      let exitingStates = (state1::ancestors1) |> List.takeWhile notLca
      let enteringStates = (state2::ancestors2) |> List.takeWhile notLca |> List.rev
      struct (exitingStates, lca, enteringStates)


   // Compose the list of transition handlers into a single handler that will invoke each in turn.
   let composeHandlers (actions: List<TransitionHandler<'D,'M>>) : TransitionHandler<'D,'M> = 
      actions 
      |> List.fold (fun prevResult action ->       
         match prevResult, action with
         | TransitionHandler.Sync prevHandler, TransitionHandler.Sync handler ->
            TransitionHandler.Sync (fun transCtx -> 
               let nextData = prevHandler transCtx
               handler { transCtx with TargetData = nextData })
         | TransitionHandler.Sync prevHandler, TransitionHandler.Async handler ->
            TransitionHandler.Async (fun transCtx -> 
               let nextData = prevHandler transCtx
               handler { transCtx with TargetData = nextData })
         | TransitionHandler.Async prevHandler, TransitionHandler.Sync handler ->
            TransitionHandler.Async (fun transCtx ->
               async { 
                  let! nextData = prevHandler transCtx
                  return handler { transCtx with TargetData = nextData }
               })
         | TransitionHandler.Async prevHandler, TransitionHandler.Async handler ->
            TransitionHandler.Async (fun transCtx -> 
               async {
                  let! nextData = prevHandler transCtx
                  return! handler { transCtx with TargetData = nextData } 
               })
      ) Handlers.emptyTransitionHandler


   // Returns an async yielding the result of entering the initial state (recursively descending the state tree)
   // of the state in the specified context.
   let rec enterInitialState 
      (fromRoot: bool) 
      (stateTree: StateTree<'D,'M>)
      (transCtx:TransitionContext<'D,'M>) = 

      // Recursively enters each state indicated by running the initialTransition function.
      let rec enterInitialStateAcc stateTree transCtx = 
         async {
            match transCtx.TargetState |> State.initialTransition with 
            | Some handler ->
               let! struct (nextData, nextStateId) = Run.initialTransition handler transCtx.TargetData
               let nextState = 
                  match stateTree |> StateTree.tryFindState nextStateId with
                  | Some nextState -> nextState
                  | _ -> 
                     let msg = 
                        sprintf "State %s returned state %s as an initial child state, but that state does not exist" 
                                 transCtx.TargetState.Id 
                                 nextStateId
                     invalidOp msg
               let transCtx = {transCtx with TargetData = nextData; TargetState = nextState; HandlingState = nextState }
               let! nextData = Run.transitionHandler nextState.Handler.OnEnter transCtx
               return! enterInitialStateAcc stateTree { transCtx with TargetData = nextData }
            | None -> 
               return transCtx
         }

      async {
         // States from root to the next state specified in the context (inclusive)
         let enteringStates = 
            if fromRoot then stateTree |> StateTree.selfAndAncestorStatesById transCtx.TargetState.Id |> List.rev 
            else List.empty
         // Enter ancestor states
         let onEnterHandler = enteringStates |> List.map onEnterWithHandlingState |> composeHandlers
         let! nextData = transCtx |> Run.transitionHandler onEnterHandler
         let transCtx = { transCtx with TargetData = nextData } 
         // Descend into initial states, entering each one
         return! enterInitialStateAcc stateTree transCtx 
      }

   // Handles a message by invoking OnMessage on the state and each of its ancestors, until the message has been
   // handled. Returns an async yielding the message result, next state machine state, the next data, and a 
   // transition action that should be invoked during the transition to the next state.
   let handleMessage 
      (message: 'M)
      (machineCtx:StateMachineContext<'D,'M>) : Async<struct (MessageResult<'D,'M> * TransitionTarget<'D,'M> * 'D * TransitionHandler<'D,'M>)> =
      
      let originalState = machineCtx.State
      let tree = machineCtx.StateTree
      let rec handleMessageAcc (msgCtx: MessageContext<'D,'M>) state = 
         async {
            let handlers = state |> State.handler
            let! msgResult = Run.messageHandler handlers.OnMessage msgCtx
            match msgResult with 
            | Transition (stateName, nextData, optAction) -> 
               let action = defaultArg optAction Handlers.emptyTransitionHandler
               return struct (msgResult, OtherState (machineCtx.StateTree |> StateTree.findState stateName), nextData, action)
            | SelfTransition (nextData, optAction) -> 
               let action = defaultArg optAction Handlers.emptyTransitionHandler
               // Note that for a self-transition, we transition to the current state for the state machine, not the 
               // handling state. Perhaps that is slightly arbitrary, but I believe that is the most semantically 
               // consistent behavior.
               return struct (msgResult, OtherState originalState, nextData, action)
            | InternalTransition nextData -> 
               return struct (msgResult, CurrentState, nextData, Handlers.emptyTransitionHandler)
            | MessageResult.Stop optReason ->
               let terminalState = Terminal(tree |> StateTree.rootState |> State.id, state.Id, optReason)
               return struct (msgResult, OtherState terminalState, msgCtx.Data, Handlers.emptyTransitionHandler)
            | Unhandled ->
               match StateTree.parentState state tree with
               | Some parent -> 
                  // Let parent state try and handle the message
                  return! handleMessageAcc msgCtx parent
               | None -> 
                  // No more parents available, so just stay in current state
                  return struct (msgResult, CurrentState, msgCtx.Data, Handlers.emptyTransitionHandler)
            | MessageResult.InvalidMessage(_) ->
               // A state has indicated the message was not appropriate, so just stay in current state.
               return struct (msgResult, CurrentState, msgCtx.Data, Handlers.emptyTransitionHandler)
         }
      handleMessageAcc (MessageContext<'D,'M> ( message, machineCtx.Data)) machineCtx.State


   // Creates an action that is the composition of the various actions (exiting, transition, entering, etc.) that must
   // be invoked when transitioning between the specified states. Returns the states to be exited, the states to be
   // entered, and the composite action.
   let buildTransition 
      (machineCtx:StateMachineContext<'D,'M>) 
      (targetState:TransitionTarget<'D,'M>) 
      (transitionAction: TransitionHandler<'D,'M>) =

      match targetState with
      | CurrentState ->
         // Internal transition, but we still need to invoke the provided transitionAction.
         let compositeAction transCtx = async {
            let! nextData = Run.transitionHandler transitionAction transCtx
            let nextTransCtx  = { transCtx with TargetData = nextData } 
            return nextTransCtx 
         }
         List.empty, List.empty, compositeAction
      | OtherState nextState ->
         let state = machineCtx.State
         let tree = machineCtx.StateTree
         let struct (exitingStates, lca, enteringStates) = statePath state nextState tree
         let exitingHandler = exitingStates |> List.map onExitWithHandlingState |> composeHandlers
         let enteringHandler = enteringStates |> List.map onEnterWithHandlingState |> composeHandlers
         let transitionAtLcaHandler = transitionAction |> withTransContextTransform (withHandlingState lca)
         let transitionHandler = composeHandlers [exitingHandler; transitionAtLcaHandler; enteringHandler]
         let compositeAction transCtx = async {
            let! nextData = Run.transitionHandler transitionHandler transCtx
            let nextTransCtx  = { transCtx with TargetData = nextData } 
            return! enterInitialState false machineCtx.StateTree nextTransCtx 
         } 
         exitingStates, enteringStates, compositeAction


   // Returns an async that transitions between the previous and next states, performing all relevant actions (exit, 
   // transition, and enter actions). Yields the states that were exited, the states that were entered, and the final 
   // transition context.
   let doTransition 
      (machineCtx:StateMachineContext<'D,'M>) 
      (nextData: 'D)
      (nextState: TransitionTarget<'D,'M>)
      (transitionAction: TransitionHandler<'D,'M>) = 
      async {
         // Build a function that encapsulates the chain of functions that need to be called during the 
         // transition to the new state
         let exiting, entering, transition = buildTransition machineCtx nextState transitionAction
         // Invoke the transition function, which yields the final context and state
         let! transCtx = transition (mkTransitionContext machineCtx.Data machineCtx.State nextData nextState machineCtx.State)
         return struct (exiting, entering, transCtx)
      }


   let initializeContext 
      (stateTree: StateTree<'D,'M>) 
      (initialData:'D) 
      (initialStateId: option<StateId>) : Async<StateMachineContext<'D,'M>> = 
      async { 
         let root = stateTree |> StateTree.rootState
         let _initialStateId = defaultArg initialStateId root.Id 
         let initialState = stateTree |> StateTree.findState _initialStateId 
         let transCtx = mkTransitionContext initialData root initialData (OtherState initialState) initialState 
         // Descend into the initial state, if the state machine was initialized with a composite state.  We're 
         // starting from scratch, so descend from the root state.
         let! transCtx = enterInitialState true stateTree transCtx
         return { State = transCtx.TargetState; Data = transCtx.TargetData; StateTree = stateTree }   
      }
   

   let processMessage (message: 'M) (smContext:StateMachineContext<'D,'M>) : Async<MessageProcessed<'D,'M>> = 
      async {
         // Let the current state handle the message
         let! struct (msgResult, nextState, nextData, transitionAction) = handleMessage message smContext
         // Perform the transition to the new state
         let! struct (exited, entered, transCtx) = doTransition smContext nextData nextState transitionAction
         // Return record describing how the message was handled
         let nextSmCtx = { smContext with State = transCtx.TargetState; Data = transCtx.TargetData }
         return mkMessageProcessed msgResult message smContext nextSmCtx exited entered
      }


   let stop 
      (smContext: StateMachineContext<'D,'M>) 
      (reason: option<StopReason>) : Async<StateMachineContext<'D,'M>> = 
      async {
         match smContext.State with
         | Terminal(_) ->
            raise <| invalidArg "smContext" "smContext is already in a terminal state."
            return smContext
         | _ ->
            // Transition to terminal state, so that current state is exited before state machine is stopped. 
            let root = smContext.StateTree |> StateTree.rootState
            let terminalState = Terminal(root.Id, smContext.State.Id, reason) 
            let handler = Handlers.emptyTransitionHandler
            let! struct (_, _, transCtx) = doTransition smContext smContext.Data (OtherState terminalState) handler
            return { smContext with State = transCtx.TargetState; Data = transCtx.TargetData }
      }