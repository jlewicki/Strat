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
   | Stopped of FromState: StateName *  Type: StopType * FinalData: 'D * Reason: option<StopReason> 


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


/// Provides data for the Transitioned event of StateMachineAgent.
type TransitionedEventArgs<'D,'M>( msgHandled : MessageHandled<'D,'M>) = 
   inherit EventArgs()
   /// The states that were entered during the transition, in the order they were entered (root-most to leaf)
   member this.EnteredStates = msgHandled.EnteredStates :> seq<_>
   /// The states that were exited during the transition, in the order they were exited (leaf to root-most)
   member this.ExitedStates = msgHandled.ExitedStates :> seq<_>
   /// The initial source state for the transition.
   member this.SourceState = msgHandled.PrevContext.State
   /// The final target state for the transition.
   member this.TargetState = msgHandled.NextContext.State


/// Handler type for the Transitioned event of StateMachineAgent.
type TransitionedEventHandler<'D,'M> = delegate of sender:obj * args:TransitionedEventArgs<'D,'M> -> unit


/// Provides data for the MessageProcessed event of StateMachineAgent.
type MessageProcessedEventArgs<'D,'M>( msgProcessed: MessageProcessed<'D,'M>) = 
   inherit EventArgs()
   member this.Result = msgProcessed


/// Handler type for the MessageProcessed event of StateMachineAgent.
type MessageProcessedEventHandler<'D,'M> = delegate of sender:obj * args: MessageProcessedEventArgs<'D,'M> -> unit


/// <summary>
/// An asynchronous message processing agent that dispatches messages to a state tree, and manages the resulting 
/// state transitions as messages arrive and are processed.
/// </summary>
[<Sealed>] 
type StateMachineAgent<'D,'M>
   ( stateTree: StateTree<'D,'M>, 
     initialData: 'D, 
     ?initialStateName: StateName, 
     ?handlerMapping: HandlerMapping<'D,'M> ) as self =  
     
   // Materialize the state tree 
   let rootState, stateMap = stateTree.Root, stateTree.States  
     
   let _handlerMapping = defaultArg handlerMapping (fun _ handler -> handler) 
   let _initialStateName = defaultArg initialStateName rootState.Name 
   let onMessageProcessed = new Event<MessageProcessedEventHandler<'D,'M>, MessageProcessedEventArgs<'D,'M>>() 
   let onTransition = new Event<TransitionedEventHandler<'D,'M>, TransitionedEventArgs<'D,'M>>()  
   let onStopped = new Event<EventHandler, EventArgs>() 
   let currentLifecycle = ref AgentLifecycle.New 
 

   // Validate initial state 
   do if not (stateMap.ContainsKey _initialStateName) then  
         invalidArg "initialStateName" (sprintf "Unable to find initial state %A in the state tree" _initialStateName) 


   /// Converts timespan to a millisecond representation. 
   let timespanInMillis (ts: Option<TimeSpan>) =  
      ts |> Option.map (fun ts -> int ts.TotalMilliseconds ) 


   // Record creation functions
   let mkSmContext data state : StateMachineContext<_,_>=  
      { State = state; Data = data; StateTree = stateTree } 

   let mkStopReason optReason optCode =  
      if Option.isSome optReason || Option.isSome optCode then 
         Some({ Reason = defaultArg optReason ""; Code = defaultArg optCode -1 }) 
      else  
         None 


   // Helper functions to validate current lifecycle 
   let ensureStarted() = 
      match !currentLifecycle with 
      | Started(_) -> () 
      | _ -> invalidOp "Lifecycle is not Started." 


   // Helper functions to raise events.
   let raiseOnStopped() =  
      onStopped.Trigger(self, EventArgs.Empty) 
   let raiseMessageProcessed msgProcessed =  
      onMessageProcessed.Trigger (self, MessageProcessedEventArgs(msgProcessed)) 
   let raiseTransition msgHandled =  
      onTransition.Trigger (self, TransitionedEventArgs(msgHandled)) 


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
      | Started(smContext) ->  
         smContext
      | Stopped(fromState, _, data, reason) -> 
         let terminalState = Terminal(rootState, fromState, reason) 
         mkSmContext data terminalState
      | _ as lifecyle ->  
         invalidOp <| sprintf "Unexpected lifecycle %A" lifecyle  
   

   /// Converts a SMReply to a SMContext 
   let replytoSMContext (smReply: SMReply<_,_>) =  
      lifecycleToSMContext smReply.Lifecyle 


   // Returns a function that raises interesting events describing how a message was processed.
   let mkPostReplyAction msgProcessed nextLifecycle = 
      fun() -> 
         raiseMessageProcessed msgProcessed
         match msgProcessed with 
         | MessageProcessed.HandledMessage(msgHandled) when not msgHandled.ExitedStates.IsEmpty -> 
            raiseTransition msgHandled | _ -> ()
         match nextLifecycle with | Stopped(_) -> raiseOnStopped() | _ -> ()
          

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
               | New, Start -> 
                  // Enter the initial of the state machine.  This is either the initial state provided in the ctor, 
                  // or the initial state for the root state 
                  let! smCtx = StateMachine.initializeContext stateTree initialData initialStateName
                  let nextLifecycle = Started(smCtx) 
                  return! replyAndLoop (StartReply(true, nextLifecycle)) nextLifecycle
                
               | Started(_), GetLifecycle -> 
                  // Just reply with the current lifecycle. 
                  return! replyAndLoop (GetLifecycleReply(lifecycle)) lifecycle
   
               | Started(smContext), Stop(reason) -> 
                  // Transition to terminal state, so that current state is exited before state machine is stopped. 
                  let! nextSmContext = StateMachine.stop smContext reason

                  // Enter stopped lifecycle state. 
                  let nextLifecycle = (AgentLifecycle.Stopped(smContext.State.Name, StopType.External, nextSmContext.Data, reason)) 
                  return! replyAndLoop (StopReply(true, nextLifecycle)) nextLifecycle
                
               | Started(smContext), DispatchMessage(message) -> 

                  let! msgProcessed = StateMachine.processMessage message smContext
   
                  // Bookeeping 
                  let nextLifecycle = msgProcessed |> msgProcessedToLifecycle smContext
                  let postReplyAction = mkPostReplyAction msgProcessed nextLifecycle
                  return! 
                     replyAndLoopWithAction 
                        (DispatchMessageReply(Some(msgProcessed), nextLifecycle)) 
                        nextLifecycle 
                        (Some(postReplyAction))
   
               | Stopped(_), DispatchMessage(_) -> 
                  // Nothing to do if stopped, but make to send a reply message 
                  return! replyAndLoop (SMReply.DispatchMessageReply(None, lifecycle)) lifecycle
                
               | Stopped(_), Stop(_) -> 
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
               return! replyAndLoop (SMReply.ErrorReply(ex, lifecycle)) lifecycle
         }   
      loop AgentLifecycle.New ) 


   /// Unwraps and throws if the choice carries an exception. 
   let valueOrThrow (smReply: SMReply<_,_>) =  
      match smReply with  
      | ErrorReply(ex,_) ->  
         let msg = "An error occurred while processing a message. See the inner excption for details." 
         raise <| new InvalidOperationException(msg, ex) 
      | _ -> smReply 
 

   /// Helper method for starting the state machine. 
   let doStart fSendStartMsg =  
      lock currentLifecycle (fun() -> 
         match !currentLifecycle with 
         | New -> 
            smAgent.Start() 
            let dispatcher reply = SMMessage.Start, reply 
            fSendStartMsg dispatcher 
         | Started(_) -> invalidOp "State machine is already started." 
         | Stopped(_) -> invalidOp "State machine has been stopped." ) 


   /// <summary> 
   ///   Starts the state machine so that it can process messages. The calling thread is blocked until the state 
   ///   machine has fully started. 
   /// </summary> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not New.</exception> 
   member this.Start() = 
      this.Start(?timeout = None)
      
 
   /// <summary>
   ///   Starts the state machine so that it can process messages. The calling thread is blocked until the state 
   ///   machine has fully started, or until the timeout expires. 
   /// </summary>
   /// <param name="timeout">How long to wait for the state machine to start.</param>
   /// <exception cref="System.TimeoutException">If the state machine has not started before the timeout expires.</exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not New.</exception> 
   member this.Start(timeout: TimeSpan) =
      this.Start (?timeout = Some(timeout))


   /// Core method to start state machine
   member private this.Start(?timeout: TimeSpan) = 
      doStart (fun dispatcher -> 
         smAgent.PostAndReply (dispatcher, ?timeout = timespanInMillis timeout) |> valueOrThrow |> ignore  ) 
 

   /// <summary>
   /// Asynchronously starts the state machine so that it can process messages.
   /// </summary>
   /// <exception cref="System.TimeoutException">If the state machine has not started before the timeout expires.</exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine has already been started or stopped.</exception>
   member this.StartAsync() = 
      this.StartAsync(?timeout = None)


   /// <summary>
   /// Asynchronously starts the state machine so that it can process messages.
   /// </summary>
   /// <param name="timeout">How long to wait for the state machine to start.</param>
   /// <exception cref="System.TimeoutException">If the state machine has not started before the timeout expires.</exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine has already been started or stopped.</exception>
   member this.StartAsync(timeout: TimeSpan) =
      this.StartAsync (?timeout = Some timeout)


   /// Core method to start state machine asynchronously.
   member private this.StartAsync(?timeout: TimeSpan) = 
      doStart (fun dispatcher -> async { 
         let! reply = smAgent.PostAndAsyncReply (dispatcher, ?timeout = timespanInMillis timeout) 
         return reply |> valueOrThrow |> ignore 
      }) 
      |> Async.StartAsTask :> Task 


   /// <summary> 
   ///   Stops the state machine. The calling thread is blocked until the state machine has fully stopped. 
   /// </summary> 
   /// <remarks> 
   ///  The state machine may not be restarted after it has been stopped. 
   ///  It is safe to call this method more than once. 
   /// </remarks> 
   /// <exception cref="System.InvalidOperationException">If the state machine has not been started.</exception> 
   member this.Stop() = 
      this.Stop( ?message = None, ?code = None, ?timeout = None)


   /// <summary> 
   ///   Stops the state machine. The calling thread is blocked until the state machine has fully stopped. 
   /// </summary> 
   /// <param name="timeout">How long to wait for the state machine to stop.</param>
   /// <exception cref="System.TimeoutException">If the state machine has not stopped before the timeout expires.</exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine has not been started.</exception> 
   member this.Stop(timeout: TimeSpan) = 
      this.Stop( ?message = None, ?code = None, ?timeout = Some timeout)


    /// <summary> 
   ///   Stops the state machine. The calling thread is blocked until the state machine has fully stopped. 
   /// </summary>
   /// <param name="message"></param>
   /// <param name="code"></param>
   member this.Stop(message:string, code: int)  = 
      let _msg = if not(String.IsNullOrEmpty(message)) then Some message else None
      this.Stop( ?message = _msg, ?code = Some code, ?timeout = None )


   /// <summary>
   ///   Stops the state machine. The calling thread is blocked until the state machine has fully stopped. 
   /// </summary>
   /// <param name="message">
   ///   Describes the reason the state machine is being stopped. May be null if no reason is available.
   /// </param>
   /// <param name="code">
   ///   An application specific code indicating the reason the state machine is being stopped.
   /// </param>
   /// <param name="timeout">How long to wait for the state machine to stop.</param>
   member this.Stop(message:string, code: int, timeout: TimeSpan)  = 
      let _msg = if not(String.IsNullOrEmpty(message)) then Some message else None
      this.Stop( ?message = _msg, ?code = Some code, ?timeout = Some timeout)


   /// <summary> 
   ///   Stops the state machine. The calling thread is blocked until the state machine has fully stopped. 
   /// </summary> 
   /// <exception cref="System.InvalidOperationException">If the state machine has not been started.</exception> 
   member this.Stop(?message:string, ?code: int, ?timeout: TimeSpan) = 
      lock currentLifecycle (fun () -> 
         match !currentLifecycle with 
         | Started(_)  -> 
            let dispatcher reply = SMMessage.Stop(mkStopReason message code), reply 
            match (smAgent.PostAndReply (dispatcher, ?timeout = timespanInMillis timeout) |> valueOrThrow) with 
            | StopReply(true, _)-> 
               (smAgent :> IDisposable).Dispose() 
               raiseOnStopped() 
            | _ -> () // Already stopped, nothing to do. 
         | _ -> ())  // Already stopped, nothing to do. 


   /// <summary> 
   ///  Asynchronously stops the state machine. 
   /// </summary> 
   /// <exception cref="System.InvalidOperationException">If the state machine has not been started.</exception> 
   member this.StopAsync() : Task = 
      this.StopAsync( ?message = None, ?code = None, ?timeout = None)


   /// <summary>
   ///   Asynchronously stops the state machine.
   /// </summary>
   /// <param name="timeout">How long to wait for the state machine to stop.</param>
   /// <exception cref="System.TimeoutException">If the state machine has not stopped before the timeout expires.</exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine has not been started.</exception> 
   member this.StopAsync(timeout: TimeSpan) : Task = 
      this.StopAsync( ?message = None, ?code = None, ?timeout = Some timeout)


   /// <summary>
   ///   Asynchronously stops the state machine. 
   /// </summary>
   /// <param name="message">
   ///   Describes the reason the state machine is being stopped. May be null if no reason is available.
   /// </param>
   /// <param name="code">
   ///   An application specific code indicating the reason the state machine is being stopped.
   /// </param>
   /// <exception cref="System.TimeoutException">If the state machine has not stopped before the timeout expires.</exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine has not been started.</exception> 
   member this.StopAsync(message:string, code: int) : Task = 
      let _msg = if not(String.IsNullOrEmpty(message)) then Some message else None
      this.StopAsync( ?message = _msg, ?code = Some code)


   /// <summary>
   ///   Asynchronously stops the state machine.
   /// </summary>
   /// <param name="message">
   ///   Describes the reason the state machine is being stopped. May be null if no reason is available.
   /// </param>
   /// <param name="code">
   ///   An application specific code indicating the reason the state machine is being stopped.
   /// </param>
   /// <param name="timeout">How long to wait for the state machine to stop.</param>
   /// <exception cref="System.TimeoutException">If the state machine has not stopped before the timeout expires.</exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine has not been started.</exception> 
   member this.StopAsync(message:string, code: int, timeout: TimeSpan) : Task = 
      let _msg = if not(String.IsNullOrEmpty(message)) then Some message else None
      this.StopAsync( ?message = _msg, ?code = Some code, ?timeout = Some timeout)


   /// <summary>
   ///   Asynchronously stops the state machine.
   /// </summary>
   /// <param name="message">
   ///   Describes the reason the state machine is being stopped. May be null if no reason is available.
   /// </param>
   /// <param name="code">
   ///   An application specific code indicating the reason the state machine is being stopped.
   /// </param>
   /// <param name="timeout">How long to wait for the state machine to stop.</param>
   /// <exception cref="System.TimeoutException">If the state machine has not stopped before the timeout expires.</exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine has not been started.</exception> 
   member this.StopAsync(?message:string, ?code: int, ?timeout: TimeSpan) : Task = 
      lock currentLifecycle (fun () -> 
         match !currentLifecycle with 
         | Started(_)  -> 
            let dispatcher reply = SMMessage.Stop(mkStopReason message code), reply 
            async { 
               let! reply = smAgent.PostAndAsyncReply (dispatcher, ?timeout = timespanInMillis timeout) 
               return  
                  match reply |> valueOrThrow with 
                  | StopReply(true, _)-> 
                     (smAgent :> IDisposable).Dispose() 
                     raiseOnStopped() 
                  | _ -> () // Already stopped, nothing to do. 
            } 
         | _ -> async.Return ()) // Already stopped, nothing to do. 
      |> Async.StartAsTask :> Task 
 

   /// <summary>
   /// Raised when a state transition occurred as a message was processed.
   /// </summary>
   /// <remarks>
   ///  Note that this event is raised on a thread pool thread, and will be raised after a message that causes the 
   ///  transition has been processed (that is, after SendMessage returns).
   /// </remarks> 
   [<CLIEvent>] 
   member this.Transitioned = 
      onTransition.Publish


   /// <summary>
   ///  Raised when the state machine has been stopped.
   /// </summary>
   /// <remarks>
   ///  Note that this event is raised on a thread pool thread, and will be raised after a message that causes an 
   ///  internal stop has been processed (that is, after SendMessage returns).
   /// </remarks> 
   [<CLIEvent>] 
   member this.Stopped =
      onStopped.Publish 


   /// <summary>
   ///  Raised when a message has been processed (possibly unsuccessfully) by the state machine.
   /// </summary>
   /// <remarks>
   ///  Note that this event is raised on a thread pool thread, and will be raised after the message has been processed
   ///  (that is, after SendMessage returns).
   /// </remarks> 
   [<CLIEvent>] 
   member this.MessageProcessed =
      onMessageProcessed.Publish
 

   /// Gets the current lifecycle state of the state machine. 
   member this.Lifecycle =  
      !currentLifecycle 


   /// <summary>Sends a message asynchronously to the state machine.</summary> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not Started.</exception> 
   member this.SendMessageAsync( message: 'M ) : Task<StateMachineContext<'D,'M>> = 
      this.SendMessageAsync( message, ?timeout = None)


   /// <summary>Sends a message asynchronously to the state machine.</summary> 
   /// <param name="message">The message to send to the state machine for processing.</param>
   /// <param name="timeout">How long to wait for the state machine to process the message.</param>
   /// <exception cref="System.TimeoutException">
   ///   If the state machine has not processed the message before the timeout expires.
   /// </exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not Started.</exception> 
   member this.SendMessageAsync( message: 'M, timeout: TimeSpan ) : Task<StateMachineContext<'D,'M>> = 
      this.SendMessageAsync( message, ?timeout = Some timeout)


   /// Core method to send a message asynchronously to the state machine
   member private this.SendMessageAsync( message, ?timeout: TimeSpan ) : Task<StateMachineContext<'D,'M>> =  
      lock currentLifecycle (fun () -> 
         ensureStarted() 
         let dispatcher reply = SMMessage.DispatchMessage(message), reply 
         async { 
            let! reply = smAgent.PostAndAsyncReply (dispatcher, ?timeout = timespanInMillis timeout) 
            return reply 
                   |> valueOrThrow 
                   |> replytoSMContext 
         } |> Async.StartAsTask ) 
 

   /// <summary>Sends a message to the state machine. The calling thread is blocked until the message is processed.</summary> 
   /// <param name="message">The message to send to the state machine for processing.</param>
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not Started.</exception> 
   member this.SendMessage(  message: 'M ) : StateMachineContext<'D,'M> =
      this.SendMessage (message, ?timeout = None)


   /// <summary>
   /// Sends a message to the state machine. The calling thread is blocked until the message is processed, or the 
   /// timeout expires.
   /// </summary>
   /// <param name="message">The message to send to the state machine for processing.</param>
   /// <param name="timeout">How long to wait for the state machine to process the message.</param>
   /// <exception cref="System.TimeoutException">
   ///   If the state machine has not processed the message before the timeout expires.
   /// </exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not Started.</exception> 
   member this.SendMessage( message: 'M, timeout: TimeSpan ) : StateMachineContext<'D,'M> = 
      this.SendMessage (message, ?timeout = Some timeout)


   /// Core method to send a message to the state machine
   member private this.SendMessage( message, ?timeout: TimeSpan ) : StateMachineContext<'D,'M> =  
      lock currentLifecycle (fun () -> 
         ensureStarted() 
         let dispatcher reply = SMMessage.DispatchMessage(message), reply 
         let reply = smAgent.PostAndReply (dispatcher, ?timeout = timespanInMillis timeout) 
         reply
         |> valueOrThrow 
         |> replytoSMContext ) 
       
        
   /// <summary>Gets the current context for the state machine.</summary> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not Started.</exception> 
   member this.Context : StateMachineContext<'D,'M> =
      match !currentLifecycle with 
      | Started(smContext) -> smContext
      | _ -> invalidOp "Lifecycle must be running" 
 

   interface IDisposable with 
      member this.Dispose() = this.Stop()
 


/// Provides functions for working with <c>StateMachineAgent<_, _></c> instances.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StateMachineAgent =

   /// Creates a new <c>StateMachineAgent<_, _></c> agent instance.
   [<CompiledName "NewAgent">]
   let newAgent (stateTree: StateTree<'D,'M>) (initialData: 'D) =
      new StateMachineAgent<'D,'M>( stateTree, initialData)


   /// Creates a new StateMachineAgent instance in the specified initial state.
   [<CompiledName "NewAgentIn">]
   let newAgentIn (initialStateName: StateName) (stateTree: StateTree<'D,'M>) (initialData: 'D) =
      new StateMachineAgent<'D,'M>( stateTree, initialData, initialStateName )


   /// Creates and starts a new StateMachineAgent agent instance.
   [<CompiledName "StartNewAgent">]
   let startNewAgent (stateTree: StateTree<'D,'M>) (initialData: 'D) =
      let agent = newAgent stateTree initialData
      agent.Start()
      agent


   /// Creates and starts a new StateMachineAgent instance in the specified initial state.
   [<CompiledName "StartNewAgentIn">]
   let startNewAgentIn (initialStateName: StateName) (stateTree: StateTree<'D,'M>) (initialData: 'D) =
      let agent = newAgentIn initialStateName stateTree initialData
      agent.Start()
      agent


   /// Returns a value indicating if the StateMachineAgent is in the AgentLifecycle.New lifecycle state.
   let inline isNew (agent: StateMachineAgent<_,_>) : bool = 
      match agent.Lifecycle with | New -> true | _ -> false


   /// Returns a value indicating if the StateMachineAgent is in the AgentLifecycle.Started lifecycle state.
   let inline isStarted (agent: StateMachineAgent<_,_>) : bool = 
      match agent.Lifecycle with | Started(_) -> true | _ -> false


   /// Returns a value indicating if the StateMachineAgent is in the AgentLifecycle.Stopped lifecycle state.
   let inline isStopped (agent: StateMachineAgent<_,_>) : bool = 
      match agent.Lifecycle with | Stopped(_) -> true | _ -> false