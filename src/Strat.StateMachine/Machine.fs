namespace Strat.StateMachine


/// Defines functions for processing messages with a state machine.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StateMachine =
   open State

   /// Describes the target state of a transition
   type private TransitionTarget<'D,'M> = 
      /// Transition is an internal.  The current state will not change, and no entry or exit handlers will be called.
      | Internal
      /// Transition is to a different state, or a self transition (i.e the current state is exited and re-entered).
      | State of State<'D,'M>


   /// Describes a function that transforms a transition context as a state machine transitions between states.
   type private Transition<'D,'M> = 
      TransitionContext<'D,'M> -> Async<TransitionContext<'D,'M>> 


   // No-op handler functions
   let private emptyTransAction: TransitionHandler<'D,'M> = fun transCtx -> async.Return transCtx.TargetData


   /// Helper functions to construct records
   let private mkTransitionContext sourceData sourceState targetData transTarget handlingState: TransitionContext<_,_> = 
      let targetState = match transTarget with | Internal -> sourceState | State(targetState) -> targetState
      { SourceData = sourceData; SourceState = sourceState; TargetData = targetData; TargetState = targetState; HandlingState = handlingState }

   let private mkMessageProcessed msgResult message prevMachineContext nextMachineContext exitingStates enteringStates = 
      match msgResult with
      | MessageResult.Unhandled -> 
         MessageProcessed.UnhandledMessage message
      | MessageResult.InvalidMessage(reason, code) -> 
         MessageProcessed.InvalidMessage(reason, code, message)
      | _ ->
         MessageProcessed.HandledMessage { 
            Message = message;
            PrevContext = prevMachineContext
            NextContext = nextMachineContext
            ExitedStates = List.ofSeq exitingStates 
            EnteredStates = List.ofSeq enteringStates 
         }


   /// Returns the state with the specified name, or throws an exception.
   let private findState stateName (stateTree: StateTree<'D,'M>) =
      match stateTree.States |> Map.tryFind stateName with
      | Some(state) -> state.Value
      | None -> invalidOp <| sprintf "Unable to find a state with name %A in the state tree." name


   /// Throws an exception if parent is not the parent state of child. 
   let private ensureChild parent child = 
      match State.parent child with 
      | Some(parentOfChild) ->
         if not (equals parentOfChild parent) then
            invalidOp <| sprintf "State %A is not a child state of %A" child parent
      | None -> 
         invalidOp <| sprintf "State %A is missing a parent state" child


   /// Throws an exception of the root ancestor of the state is not the specified root state.
   let private ensureRoot state (rootState: State<'D,'M>) = 
      let stateRoot = selfAndAncestors state |> Seq.last
      if not (equals stateRoot rootState) then 
         invalidOp <| sprintf "State %A has a different root state that than expected root %A" (name state) (name rootState)


   /// Repeatedly 'evolves' a state value by threading it through calls to f, which returns the evolved state, and a
   /// value indicating if the evolution should continue. An async yielding the final evolved state is returned.
   let rec private generate (f: 'State -> Async<'State*bool>) (state: 'State) = async {
      let! nextState, shouldContinue = f state
      if shouldContinue then return! generate f nextState
      else return state
   }


   /// Returns an async representing the result of applying the function fFold to each element of the list, 
   /// threading an accumulator argument through the computation.
   let private foldAsync fFold state (items: List<'A>) : Async<'B> = async {
      return! items |> List.fold (fun stateAsync item -> async {
         let! state = stateAsync
         return! fFold state item
      })  (async.Return state)
   }


   /// Returns an action that invokes each of the actions in sequence
   let private composeActions (actions:seq<TransitionHandler<'D,'M>>) : TransitionHandler<'D,'M> = 
      actions 
      |> Seq.fold (fun prevResult action -> 
         fun transCtx -> async {
            let! nextData = prevResult transCtx
            return! action { transCtx with TargetData = nextData }
         }) emptyTransAction


   /// Returns an async yielding the result of entering the initial state (recursively descending the state tree)
   /// of the state in the specified context.
   let rec private enterInitialState (fromRoot: bool) stateTree (transCtx:TransitionContext<'D,'M>) = async {
      // States from root to the next state specified in the context (inclusive)
      let statesToEnter = 
         if fromRoot then transCtx.TargetState |> selfAndAncestors |> List.rev 
         else []

      // Enter ancestor states
      let! transCtx = 
         statesToEnter
         |> foldAsync (fun transCtx (state: State<'D,'M>) -> async {
            let handlers = handlers state
            let! nextData = handlers.OnEnter { transCtx with HandlingState = state }
            return { transCtx with TargetData = nextData }
         }) transCtx

      // Descend into initial states, entering each one
      return!  
         transCtx
         |> generate (fun (transCtx:TransitionContext<'D,'M>) -> async {
            match initialTransition transCtx.TargetState with 
            | Some(initTransition) ->
               let! nextData, nextStateName = initTransition transCtx.TargetData
               let nextState = findState nextStateName stateTree
               let handlers = handlers nextState
               ensureChild transCtx.TargetState nextState 
               let transCtx = { transCtx with TargetData = nextData; TargetState = nextState; HandlingState = nextState }
               let! nextData = handlers.OnEnter transCtx
               return { transCtx with TargetData = nextData }, true
            | None -> 
               return transCtx, false
         } ) 
   } 


   /// Handles a message by invoking OnMessage on the state and each of its ancestors, until the message has been
   /// handled. Returns an async yielding the message result, next state machine state, the next data, and a 
   /// transition action that should be invoked during the transition to the next state.
   let private handleMessage 
      (message: 'M)
      (machineCtx:StateMachineContext<'D,'M>) : Async<MessageResult<'D,'M> * TransitionTarget<'D,'M> * 'D * TransitionHandler<'D,'M>> =
      
      let rec handleMessageAcc msgCtx state = 
         async {
            let handlers = handlers state
            let! msgResult = handlers.OnMessage msgCtx
            match msgResult with 
            | Transition(stateName, nextData, optAction) -> 
               let action = defaultArg optAction emptyTransAction
               return msgResult, State(findState stateName machineCtx.StateTree), nextData, action
            | SelfTransition(nextData, optAction) -> 
               let action = defaultArg optAction emptyTransAction
               return msgResult, State(state), nextData, action
            | InternalTransition(nextContext) -> 
               return msgResult, Internal, nextContext, emptyTransAction
            | MessageResult.Stop(optReason) ->
               let terminalState = Terminal(machineCtx.StateTree.Root, state.Name, optReason)
               return msgResult, State(terminalState), msgCtx.Data, emptyTransAction
            | Unhandled ->
               match State.parent state with
               | Some(parent) -> 
                  // Let parent state try and handle the message
                  return! handleMessageAcc msgCtx parent
               | None -> 
                  // No more parents available, so just stay in current state
                  return msgResult, Internal, msgCtx.Data, emptyTransAction
            | MessageResult.InvalidMessage(_) ->
               // A state has indicated the message was not appropriate, so just stay in current state.
               return msgResult, Internal, msgCtx.Data, emptyTransAction
         }
      handleMessageAcc { Message=message; Data=machineCtx.Data} machineCtx.State


   /// Returns an action that is the composition of the various actions (exiting, transition, entering, etc.) that
   /// must be invoked when transitioning between the specified states.
   let private buildTransition 
      (machineCtx:StateMachineContext<'D,'M>) 
      (targetState:TransitionTarget<'D,'M>) 
      (transitionAction: TransitionHandler<'D,'M>) : List<State<'D,'M>> * List<State<'D,'M>> * Transition<'D,'M> = 
      
      let toInitialTransition transitionAction (transCtx: TransitionContext<_,_>) = async {
         let! nextData = transitionAction transCtx
         return { transCtx with TargetData = nextData }
      }
      let toOnEnterTransitionAction state : TransitionHandler<_,_> = 
         fun transCtx -> (handlers state).OnEnter { transCtx with HandlingState = state }
      let toOnExitTransitionAction state : TransitionHandler<_,_> = 
         fun transCtx -> (handlers state).OnExit { transCtx with HandlingState = state }
      let transitionActionWithHandlingState (state:State<_,_>) = 
         fun transCtx -> transitionAction { transCtx with HandlingState = state }

      match targetState with
      | Internal ->
         // Internal transition, but we still need to invoke the associated action.
         List.empty, List.empty, toInitialTransition transitionAction
      | State(nextState) ->
         let state = machineCtx.State
         let lca = leastCommonAncestor state nextState
         let notLca state = not (equals state lca)
         let exitingStates = state |> selfAndAncestors |> List.takeWhile notLca
         let enteringStates = nextState |> selfAndAncestors |> List.takeWhile notLca |> List.rev
         let exitingAction = exitingStates |> Seq.map toOnExitTransitionAction |> composeActions
         let enteringAction = enteringStates |> Seq.map toOnEnterTransitionAction |> composeActions
         let actions = composeActions [exitingAction; (transitionActionWithHandlingState lca); enteringAction]
         let action = fun transCtx -> async {
            let! nextData = actions transCtx
            let nextTransCtx  = { transCtx with TargetData = nextData } 
            return! enterInitialState false machineCtx.StateTree nextTransCtx 
         }
         exitingStates, enteringStates, action


   /// Returns an async that transitions between the previous and next states, performing all relevant actions (exit, 
   /// transition, and enter actions). Yields the states that were exited, the states that were entered, and the final 
   /// transition context.
   let private doTransition (machineCtx:StateMachineContext<'D,'M>) nextData nextState transitionAction =  async {
      // Build a function that encapsulates the chain of functions that need to be called during the 
      // transition to the new state
      let exiting, entering, transition = buildTransition machineCtx nextState transitionAction

      // Invoke the transition function, which yields the final context and state
      let! transCtx = transition (mkTransitionContext machineCtx.Data machineCtx.State nextData nextState machineCtx.State)
      ensureRoot transCtx.TargetState machineCtx.StateTree.Root
      return exiting, entering, transCtx
   }


   /// Yields a new state machine context resulting from entering the specified initial state (or root state if not 
   /// provided) with the specified initial data.
   let initializeContext 
      (stateTree: StateTree<'D,'M>) 
      (initialData:'D) 
      (initialStateName: option<StateName>) : Async<StateMachineContext<'D,'M>> = async { 
      
      let initialStateName = defaultArg initialStateName stateTree.Root.Name 
      let initialState = findState initialStateName stateTree

      // Descend into the initial state, if the state machine was initialized with a composite state.  We're 
      // starting from scratch, so descend from the root state.
      let transCtx = mkTransitionContext initialData stateTree.Root initialData (TransitionTarget.State(initialState)) initialState 
      let! transCtx = enterInitialState true stateTree transCtx 
      ensureRoot transCtx.TargetState stateTree.Root 
      
      return { State = transCtx.TargetState; Data = transCtx.TargetData; StateTree = stateTree }   
   } 


   /// <summary> 
   /// Yields the result of transitionining the specified state machine context to a stopped (terminal) state, 
   /// optionally providing a reason the machine is being stopped.
   /// </summary> 
   /// <exception cref="System.ArgumentException">If the context is already in a terminal state.</exception> 
   let stop 
      (smContext: StateMachineContext<'D,'M>) 
      (reason: option<StopReason>) : Async<StateMachineContext<'D,'M>> = async {

      match smContext.State with
      | Terminal(_) ->
         raise <| invalidArg "smContext" "smContext is already in a terminal state."
         return smContext
      | _ ->
         // Transition to terminal state, so that current state is exited before state machine is stopped. 
         let terminalState = Terminal(smContext.StateTree.Root, smContext.State.Name, reason) 
         let! _, _, transCtx = doTransition smContext smContext.Data (State(terminalState)) emptyTransAction
         return { smContext with State = transCtx.TargetState; Data = transCtx.TargetData }
   }


   /// Yields the result of processing the message within the specified machine context.
   let processMessage (message: 'M) (smContext:StateMachineContext<'D,'M>) : Async<MessageProcessed<'D,'M>> = async {
      // Let the current state handle the message
      let! msgResult, nextState, nextData, transitionAction = handleMessage message smContext
                 
      // Perform the transition to the new state
      let! exited, entered, transCtx = doTransition smContext nextData nextState transitionAction

      // Return record describing how the message was handled
      let nextSmCtx = { smContext with State = transCtx.TargetState; Data = transCtx.TargetData }
      return mkMessageProcessed msgResult message smContext nextSmCtx exited entered
   }
