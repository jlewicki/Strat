namespace Strat.StateMachine

open System
open System.Threading.Tasks


[<Class; Sealed>]
type StateMachineAgent<'D,'M> =
   interface IDisposable

   /// <summary>Gets the current context for the state machine.</summary> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not Started.</exception> 
   member CurrentContext: StateMachineContext<'D,'M>

   /// <summary>
   /// Starts the state machine so that it can process messages. The calling thread is blocked until the state 
   /// machine has fully started, or until the timeout expires. 
   /// </summary>
   /// <param name="timeout">Optional timeout indicating how long to wait for the state machine to start.</param>
   /// <exception cref="System.TimeoutException">If the state machine has not started before the timeout expires.</exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not New.</exception> 
   member Start: ?timeout: TimeSpan -> StateMachineContext<'D,'M>

   /// <summary>
   /// Asynchronously starts the state machine so that it can process messages. Yields the initial context for the 
   /// state machine upon starting.
   /// </summary>
   /// <param name="timeout">How long to wait for the state machine to start.</param>
   /// <exception cref="System.TimeoutException">If the state machine has not started before the timeout expires.</exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine has already been started or stopped.</exception>
   member StartAsync: ?timeout: TimeSpan -> Task<StateMachineContext<'D,'M>>
   
   /// <summary>
   /// Asynchronously sends a message to the state machine
   /// </summary>
   /// <param name="message">The message to send to the state machine for processing.</param>
   /// <param name="timeout">
   /// Optional timeout indicating how long to wait for the state machine to process the message.
   /// </param> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not Started.</exception>
   member PostMessage: message: 'M  -> unit

   /// <summary>
   /// Asynchronously sends a message to the state machine, yielding the result processing the message.
   /// </summary>
   /// <param name="message">The message to send to the state machine for processing.</param>
   /// <param name="timeout">
   /// Optional timeout indicating how long to wait for the state machine to process the message.
   /// </param>
   /// <exception cref="System.TimeoutException">
   /// If the state machine has not processed the message before the timeout expires.
   /// </exception> 
   /// <exception cref="System.InvalidOperationException">If the state machine lifecycle state is not Started.</exception>
   member PostMessageWithAsyncReply: message: 'M * ?timeout: TimeSpan -> Task<MessageProcessed<'D,'M>>

/// Provides functions for working with <c>StateMachineAgent<_, _></c> instances.
module StateMachineAgent =

   /// Creates a new <c>StateMachineAgent<_, _></c> agent instance.
   [<CompiledName "NewAgent">]
   val newAgent: stateTree: StateTree<'D,'M> -> initialData: 'D -> StateMachineAgent<'D,'M>


   /// Creates a new StateMachineAgent instance in the specified initial state.
   [<CompiledName "NewAgentIn">]
   val newAgentIn: initialState: StateId -> stateTree: StateTree<'D,'M> -> initialData: 'D -> StateMachineAgent<'D,'M>

   /// Creates and starts a new StateMachineAgent agent instance.
   [<CompiledName "StartNewAgent">]
   val startNewAgent: stateTree: StateTree<'D,'M> -> initialData: 'D -> StateMachineAgent<'D,'M>