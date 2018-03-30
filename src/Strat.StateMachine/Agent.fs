namespace Strat.StateMachine

open System
open System.Threading.Tasks

/// Describes the lifecycle of a state machine ahent
type AgentLifecycle<'D,'M> =  
   /// State machine has been created, but Start has not been called. 
   | New 
   /// State machine has been started, and can process messages 
   | Started of StateMachineContext<'D,'M>  
   /// The state machine was stopped (either internal or external). This is the terminal state, and the state machine 
   /// may not be restarted. 
   | Stopped of FromState: StateId *  Type: StopType * FinalData: 'D * Reason: option<StopReason> 


/// Describes how a state machine agent was stopped.
and StopType =  
   /// The agent was stopped in response to processing a message.
   | Internal = 1 
   /// The agent was explicitly stopped by client code calling one of the Stop methods.
   | External = 2 


/// Describes types of messages that can be to sent the state machine agent 
type internal SMMessage<'M> = 
   | Start 
   | DispatchMessage of 'M 
   | GetLifecycle 
   | Stop of Reason:option<StopReason> 


/// Describes the replies that the state machine agent can send in response to an SMMessage 
type internal SMReply<'D,'M> =  
   | StartReply of DidStart: bool * Lifecycle: AgentLifecycle<'D,'M> 
   | GetLifecycleReply of AgentLifecycle<'D,'M> 
   | StopReply of DidStop: bool * Lifecycle: AgentLifecycle<'D,'M> 
   | DispatchMessageReply of option<MessageProcessed<'D,'M>> * Lifecycle: AgentLifecycle<'D,'M> 
   | ErrorReply of Error: Exception * Lifecycle: AgentLifecycle<'D,'M> 
with  
   member this.Lifecyle =  
      match this with 
         | StartReply(_, lifecycle) -> lifecycle 
         | StopReply(_, lifecycle) -> lifecycle 
         | GetLifecycleReply(lifecycle) -> lifecycle 
         | DispatchMessageReply(_,lifecycle) -> lifecycle 
         | ErrorReply(_,lifecycle)-> lifecycle 


/// Describes the message type that parameterizes the state machine agent.  
type internal SMReplyableMessage<'D,'M> = SMMessage<'M> * AsyncReplyChannel<SMReply<'D,'M>> 


/// A function that maps a state to a state handler. Primarily useful for unit testing. 
type HandlerMapping<'D,'M> =  
   State<'D,'M> -> StateHandler<'D,'M> -> StateHandler<'D,'M> 


///Describes the notfications that can be emitted from the state machine agent.         
type internal AgentNotification<'D,'M> = 
   | Started of StateMachineContext<'D,'M>
   | MessageProcessed of MessageProcessed<'D,'M>
   | Transitioned of MessageHandled<'D,'M>
   | Stopped of option<StopReason>


/// <summary>
/// 
/// </summary>
/// <remarks>
/// StateMachineAgent implements IDisposable. The implementation of the Dispose method synchronously stops the agent,
/// as if the Stop method was called. Callers should recognize therefore that Dispose is a blocking method. 
/// </remarks>
[<Sealed>] 
type StateMachineAgent<'D,'M>
   ( stateTree: StateTree<'D,'M>, 
     initialData: 'D, 
     ?initialStateName: StateId, 
     ?handlerMapping: HandlerMapping<'D,'M> ) =  

   // Materialize the state tree 
   let rootState = stateTree |> StateTree.rootState
     
   let _handlerMapping = defaultArg handlerMapping (fun _ handler -> handler) 
   let _initialStateName = defaultArg initialStateName (rootState.Id)
   let currentLifecycle = ref AgentLifecycle.New 
   let notifications = Event<AgentNotification<'D,'M>>()
   let errored = Event<exn>()

   // Validate initial state 
   do if not (stateTree |> StateTree.tryFindState _initialStateName |> Option.isSome) then  
         invalidArg "initialStateName" (sprintf "Unable to find initial state %A in the state tree" _initialStateName) 


   /// Converts timespan to a millisecond representation. 
   let timespanInMillis (ts: Option<TimeSpan>) =  
      ts |> Option.map (fun ts -> int ts.TotalMilliseconds ) 


   // Record creation functions
   let mkSmContext data state : StateMachineContext<_,_>=  
      { State = state; Data = data; StateTree = stateTree } 

   let mkStopReason optReason optCode =  
      if Option.isSome optReason || Option.isSome optCode then 
         Some (StopReason( defaultArg optReason "", defaultArg optCode -1)) 
      else  
         None 


   // Helper functions to validate current lifecycle 
   let ensureStarted() = 
      match !currentLifecycle with 
      | AgentLifecycle.Started(_) -> () 
      | _ -> invalidOp "Lifecycle is not Started." 


   /// Converts a MessageProcessed to an AgentLifecycle
   let msgProcessedToLifecycle (prevSmContext: StateMachineContext<'D,'M>) (msgProcessed: MessageProcessed<'D,'M>) = 
      match msgProcessed with
      | HandledMessage( { NextContext = { State = Terminal(_, fromState, reason); Data = data } } ) ->
         AgentLifecycle.Stopped (fromState, StopType.Internal, data, reason)
      | HandledMessage( handledMessage ) ->
         AgentLifecycle.Started handledMessage.NextContext 
      | _ -> 
         AgentLifecycle.Started prevSmContext
      
              
   /// Converts an AgentLifecycle to a SMContext 
   let lifecycleToSMContext (lifecycle: AgentLifecycle<'D,'M>) =  
      match lifecycle with 
      | AgentLifecycle.Started(smContext) ->  
         smContext
      | AgentLifecycle.Stopped(fromState, _, data, reason) -> 
         let terminalState = Terminal(rootState.Id, fromState, reason) 
         mkSmContext data terminalState
      | _ as lifecyle ->  
         invalidOp <| sprintf "Unexpected lifecycle %A" lifecyle  
   

   /// Converts a SMReply to a SMContext 
   let replytoSMContext (smReply: SMReply<_,_>) =  
      lifecycleToSMContext smReply.Lifecyle 


   // Returns a function that raises interesting events describing how a message was processed.
   let mkPostReplyAction (msgProcessed: MessageProcessed<_,_>) (nextLifecycle: AgentLifecycle<_,_>) = 
      fun() -> 
         notifications.Trigger (AgentNotification.MessageProcessed(msgProcessed))
         match msgProcessed with 
         | MessageProcessed.HandledMessage(msgHandled) when not msgHandled.ExitedStates.IsEmpty -> 
            notifications.Trigger (AgentNotification.Transitioned(msgHandled)) 
         | _ -> ()
         match nextLifecycle with 
         | AgentLifecycle.Stopped(_, _, _, optReason) -> 
            notifications.Trigger (AgentNotification.Stopped optReason)
         | _ -> ()


   /// The main message processing loop for the state machine agent. 
   let rec smAgent = new MailboxProcessor<SMReplyableMessage<'D,'M>>( fun inbox ->   
      let rec loop (lifecycle: AgentLifecycle<'D,'M>) = 
         async { 
            // Wait for next message 
            let! smMessage, replyChannel = inbox.Receive() 
   
            // Helper methods to update current lifecycle and to send a reply before looping 
            let replyAndLoopWithAction replyMsg nextLifecycle (postReplyAction: option<unit->unit>) =  
               currentLifecycle := nextLifecycle 
               replyChannel.Reply replyMsg 
               // Call action (which will raise events) after reply has been sent. Its hard to say which approach is 
               // more intuitive (raise events before or aftre the reply message), so this is slightly arbitrary.
               if Option.isSome postReplyAction then postReplyAction.Value() 
               loop <| nextLifecycle  
            let replyAndLoop replyMsg nextLifecycle =  
               replyAndLoopWithAction replyMsg nextLifecycle None 
                
            try 
               match lifecycle, smMessage with 
               | AgentLifecycle.New, Start -> 
                  // Enter the initial of the state machine.  This is either the initial state provided in the ctor, 
                  // or the initial state for the root state 
                  let! smCtx = Machine.initializeContext stateTree initialData initialStateName
                  let nextLifecycle = AgentLifecycle.Started(smCtx) 
                  return! replyAndLoop (StartReply(true, nextLifecycle)) nextLifecycle
                
               | AgentLifecycle.Started(_), GetLifecycle -> 
                  // Just reply with the current lifecycle. 
                  return! replyAndLoop (GetLifecycleReply(lifecycle)) lifecycle
   
               | AgentLifecycle.Started(smContext), Stop(reason) -> 
                  // Transition to terminal state, so that current state is exited before state machine is stopped. 
                  let! nextSmContext = Machine.stop smContext reason

                  // Enter stopped lifecycle state. 
                  let nextLifecycle = (AgentLifecycle.Stopped(smContext.State.Id, StopType.External, nextSmContext.Data, reason)) 
                  return! replyAndLoop (StopReply(true, nextLifecycle)) nextLifecycle
                
               | AgentLifecycle.Started(smContext), DispatchMessage(message) -> 

                  let! msgProcessed = Machine.processMessage message smContext
   
                  // Bookeeping 
                  let nextLifecycle = msgProcessed |> msgProcessedToLifecycle smContext
                  let postReplyAction = mkPostReplyAction msgProcessed nextLifecycle
                  return! 
                     replyAndLoopWithAction 
                        (DispatchMessageReply(Some(msgProcessed), nextLifecycle)) 
                        nextLifecycle 
                        (Some(postReplyAction))
   
               | AgentLifecycle.Stopped(_), DispatchMessage(_) -> 
                  // Nothing to do if stopped, but make to send a reply message 
                  return! replyAndLoop (SMReply.DispatchMessageReply(None, lifecycle)) lifecycle
                
               | AgentLifecycle.Stopped(_), Stop(_) -> 
                  // We'll only get here if two threads perform external stops, which would be unsual, but not invalid. 
                  // Just ignore stops after the first 
                  return! replyAndLoop (SMReply.StopReply(false, lifecycle)) lifecycle
                
               | lifecycle, message ->  
                  invalidOp <| sprintf "Invalid state machine message %A for lifecycle %A" message lifecycle 

            with ex -> 
               // Catch exceptions and send them on the reply channel, to make them more explicit to clients. The  
               // default F# agent behavior of sending the exception to the Error event and never sending a reply 
               // makes life difficult for unit testing (unless a timeout is specified, calls will simply never 
               // return). 
               let postReplyAction() = errored.Trigger ex
               return! replyAndLoopWithAction (SMReply.ErrorReply(ex, lifecycle)) lifecycle (Some postReplyAction)
         }   
      loop AgentLifecycle.New ) 


   /// Helper method for starting the state machine. 
   let doStart fSendStartMsg =  
      lock currentLifecycle (fun() -> 
         match !currentLifecycle with 
         | AgentLifecycle.New -> 
            smAgent.Start() 
            let dispatcher reply = SMMessage.Start, reply 
            fSendStartMsg dispatcher 
         | AgentLifecycle.Started(_) -> invalidOp "State machine is already started." 
         | AgentLifecycle.Stopped(_) -> invalidOp "State machine has been stopped." ) 


   /// Unwraps and throws if the choice carries an exception. 
   let valueOrThrow (smReply: SMReply<_,_>) =  
      match smReply with  
      | ErrorReply(ex,_) ->  
         let msg = "An error occurred while processing a message. See the inner excption for details." 
         raise <| new InvalidOperationException(msg, ex) 
      | _ -> smReply


   /// Gets the current lifecycle state of the state machine. 
   member this.Lifecycle : AgentLifecycle<'D,'M> =  
      !currentLifecycle 


   /// <summary>Gets the current context for the state machine.</summary> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not Started.</exception> 
   member this.Context : StateMachineContext<'D,'M> =
      match !currentLifecycle with 
      | AgentLifecycle.Started(smContext) -> smContext
      | _ -> invalidOp "Lifecycle must be running" 


   /// <summary>
   /// Starts the state machine so that it can process messages. The calling thread is blocked until the state 
   /// machine has fully started, or until the timeout expires. 
   /// </summary>
   /// <param name="timeout">Optional timeout indicating how long to wait for the state machine to start.</param>
   /// <exception cref="System.TimeoutException">If the state machine has not started before the timeout expires.</exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not New.</exception> 
   member this.Start (?timeout: TimeSpan) : unit =
      doStart (fun dispatcher -> 
         smAgent.PostAndReply (dispatcher, ?timeout = timespanInMillis timeout) |> valueOrThrow |> ignore  ) 


   /// <summary>
   /// Asynchronously starts the state machine so that it can process messages.
   /// </summary>
   /// <param name="timeout">How long to wait for the state machine to start.</param>
   /// <exception cref="System.TimeoutException">If the state machine has not started before the timeout expires.</exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine has already been started or stopped.</exception>
   member this.StartAsync (?timeout: TimeSpan) : Task = 
      doStart (fun dispatcher -> 
         async { 
            let! reply = smAgent.PostAndAsyncReply (dispatcher, ?timeout = timespanInMillis timeout) 
            return reply |> valueOrThrow |> ignore }
         |> Async.StartAsTask :> Task)


   /// <summary> 
   /// Stops the state machine. The calling thread is blocked until the state machine has fully stopped. 
   /// </summary> 
   /// <param name="timeout">
   /// Optional timout indicating how long to wait for the state machine to stop.</param>
   /// <param name="message">
   /// Optional reason describing why the state machine is being stopped. 
   /// </param>
   /// <param name="code">
   /// Optional application-specific code indicating the reason the state machine is being stopped.
   /// </param>
   /// <exception cref="System.InvalidOperationException">If the state machine has not been started.</exception> 
   member this.Stop (?message:string, ?code: int, ?timeout: TimeSpan) : unit = 
      lock currentLifecycle (fun () -> 
         match !currentLifecycle with 
         | AgentLifecycle.Started(_)  -> 
            let stopReason = mkStopReason message code
            let dispatcher reply = SMMessage.Stop(stopReason), reply 
            match (smAgent.PostAndReply (dispatcher, ?timeout = timespanInMillis timeout) |> valueOrThrow) with 
            | StopReply(true, _)-> 
               (smAgent :> IDisposable).Dispose() 
               notifications.Trigger( AgentNotification.Stopped stopReason )
            | _ -> () // Already stopped, nothing to do. 
         | _ -> ())  // Already stopped, nothing to do. 


   /// <summary> 
   /// Asynchronously stops the state machine. The calling thread is blocked until the state machine has fully stopped. 
   /// </summary> 
   /// <param name="timeout">
   /// Optional timout indicating how long to wait for the state machine to stop.</param>
   /// <param name="message">
   /// Optional reason describing why the state machine is being stopped. 
   /// </param>
   /// <param name="code">
   /// Optional application-specific code indicating the reason the state machine is being stopped.
   /// </param>
   /// <exception cref="System.InvalidOperationException">If the state machine has not been started.</exception> 
   member this.StopAsync(?message:string, ?code: int, ?timeout: TimeSpan) : Task = 
      lock currentLifecycle (fun () -> 
         match !currentLifecycle with 
         | AgentLifecycle.Started(_)  -> 
            let stopReason = mkStopReason message code
            let dispatcher reply = SMMessage.Stop(stopReason), reply 
            async { 
               let! reply = smAgent.PostAndAsyncReply (dispatcher, ?timeout = timespanInMillis timeout) 
               return  
                  match reply |> valueOrThrow with 
                  | StopReply(true, _)-> 
                     (smAgent :> IDisposable).Dispose() 
                     notifications.Trigger( AgentNotification.Stopped stopReason )
                  | _ -> () // Already stopped, nothing to do. 
            } 
         | _ -> async.Return ()) // Already stopped, nothing to do.
      |> Async.StartAsTask :> Task


   /// <summary>
   /// Sends a message to the state machine. The calling thread is blocked until the message is processed, or the 
   /// timeout expires.
   /// </summary>
   /// <param name="message">The message to send to the state machine for processing.</param>
   /// <param name="timeout">
   /// Optional timeout indicating how long to wait for the state machine to process the message.
   /// </param>
   /// <exception cref="System.TimeoutException">
   /// If the state machine has not processed the message before the timeout expires.
   /// </exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not Started.</exception>
   member this.SendMessage(message: 'M, ?timeout: TimeSpan) : StateMachineContext<'D,'M> = 
         lock currentLifecycle (fun () -> 
         ensureStarted() 
         let dispatcher reply = SMMessage.DispatchMessage(message), reply 
         let reply = smAgent.PostAndReply (dispatcher, ?timeout = timespanInMillis timeout) 
         reply
         |> valueOrThrow 
         |> replytoSMContext)


   /// <summary>
   /// Asynchronously sends a message to the state machine.
   /// </summary>
   /// <param name="message">The message to send to the state machine for processing.</param>
   /// <param name="timeout">
   /// Optional timeout indicating how long to wait for the state machine to process the message.
   /// </param>
   /// <exception cref="System.TimeoutException">
   /// If the state machine has not processed the message before the timeout expires.
   /// </exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not Started.</exception>
   member this.SendMessageAsync(message: 'M, ?timeout: TimeSpan) : Task<StateMachineContext<'D,'M>> = 
      lock currentLifecycle (fun () -> 
         ensureStarted() 
         let dispatcher reply = SMMessage.DispatchMessage(message), reply 
         async { 
            let! reply = smAgent.PostAndAsyncReply (dispatcher, ?timeout = timespanInMillis timeout) 
            return reply |> valueOrThrow |> replytoSMContext 
         }
         |> Async.StartAsTask)


   /// Observable that publishes a notification each time a message was processed by this state machine.
   member val MessageProcessed : IObservable<MessageProcessed<'D,'M>> = 
      notifications.Publish |> Observable.choose(function | MessageProcessed m -> Some m | _ -> None )
      with get


    /// Observable that publishes a notification each time a state transition has occurred in this state machine.
   member val Transitioned : IObservable<MessageHandled<'D,'M>> =
      notifications.Publish |> Observable.choose(function | Transitioned t  -> Some t | _ -> None )
      with get


   /// Observable that publishes a notification when this state machine has been started.
   member val Started : IObservable<StateMachineContext<'D,'M>> = 
      notifications.Publish |> Observable.choose( function | Started smCtx -> Some smCtx | _ -> None )
      with get
      

   /// Observable that publishes a notification when the state machine is stopped, optionally including a description
   /// of why the machine was stopped.
   member val Stopped : IObservable<option<StopReason>> = 
      notifications.Publish |> Observable.choose( function | Stopped reason -> Some(reason) | _ -> None )
      with get


   /// Observable that publishes errors that taht occur while the state machine is processing messages.
   member this.Error : IObservable<exn> =
      upcast errored.Publish


   interface IDisposable with 
      member this.Dispose() = this.Stop()


/// Provides functions for working with <c>StateMachineAgent<_, _></c> instances.
module StateMachineAgent =

   /// Creates a new <c>StateMachineAgent<_, _></c> agent instance.
   [<CompiledName "NewAgent">]
   let newAgent (stateTree: StateTree<'D,'M>) (initialData: 'D) =
      new StateMachineAgent<'D,'M>( stateTree, initialData)


   /// Creates a new StateMachineAgent instance in the specified initial state.
   [<CompiledName "NewAgentIn">]
   let newAgentIn (initialState: StateId) (stateTree: StateTree<'D,'M>) (initialData: 'D) =
      new StateMachineAgent<'D,'M>( stateTree, initialData, initialState )


   /// Creates and starts a new StateMachineAgent agent instance.
   [<CompiledName "StartNewAgent">]
   let startNewAgent (stateTree: StateTree<'D,'M>) (initialData: 'D) =
      let agent = newAgent stateTree initialData
      agent.Start()
      agent


   /// Creates and starts a new StateMachineAgent instance in the specified initial state.
   [<CompiledName "StartNewAgentIn">]
   let startNewAgentIn (initialState: StateId) (stateTree: StateTree<'D,'M>) (initialData: 'D) =
      let agent = newAgentIn initialState stateTree initialData
      agent.Start()
      agent


   /// Returns a value indicating if the StateMachineAgent is in the AgentLifecycle.New lifecycle state.
   let inline isNew (agent: StateMachineAgent<_,_>) : bool = 
      match agent.Lifecycle with | New -> true | _ -> false


   /// Returns a value indicating if the StateMachineAgent is in the AgentLifecycle.Started lifecycle state.
   let inline isStarted (agent: StateMachineAgent<_,_>) : bool = 
      match agent.Lifecycle with | AgentLifecycle.Started(_) -> true | _ -> false


   /// Returns a value indicating if the StateMachineAgent is in the AgentLifecycle.Stopped lifecycle state.
   let inline isStopped (agent: StateMachineAgent<_,_>) : bool = 
      match agent.Lifecycle with | AgentLifecycle.Stopped(_) -> true | _ -> false

