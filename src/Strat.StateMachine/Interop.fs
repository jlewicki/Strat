namespace Strat.StateMachine.Interop

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Strat.StateMachine
open Strat.StateMachine.Definition

/// Similar to MessageContext, defined with interop-friendly types.
type IMessageContext<'D,'M> = 
   /// The message that is being processed.
   abstract Message: 'M
   
   /// The current state machine context.
   abstract Data: 'D

   /// Returns a message result indicating that a transition to the specified state should occur, optionally updating
   /// the state data.
   abstract GoTo:
      nextState: StateName * 
      [<Optional>] nextData:'D * 
      [<Optional>] action: AsyncTransitionHandler<'D,'M> -> MessageResult<'D,'M>
   
   /// Returns a message result indicating that an internal transition should occur, optionally updating the state data.
   /// An internal transition means that the current state will not change, and no entry and exit handlers will be 
   /// called. 
   abstract Stay: [<Optional>] nextData: 'D  -> MessageResult<'D,'M>
   
   /// Returns a message result indicating that an self-transition should occur, optionally updating the state data. A 
   /// self-transition means that the current state is exited and re-entered, calling the handler functions for the 
   /// state.
   abstract GoToSelf: 
      [<Optional>] nextData: 'D *
      [<Optional>] action: AsyncTransitionHandler<'D,'M> -> MessageResult<'D,'M>
   
   /// Returns a message result indicating the message could not be handled by a state, and that any ancestor states 
   /// should be given an opportunity to handle the message.
   abstract Unhandled: unit -> MessageResult<'D,'M>

   /// Returns a message result indicating that the state machine should tranmsition to the terminated state, and 
   /// stop all further message processing.
   abstract Stop: 
      [<Optional>] reason: string *
      [<Optional>] code: int -> MessageResult<'D,'M>

/// Similar to StateHandler, defined with interop-friendly types.
and IStateHandler<'D,'M> = 
   abstract OnMessage: AsyncMessageHandler<'D,'M>
   abstract OnEnter: AsyncTransitionHandler<'D,'M>
   abstract OnExit: AsyncTransitionHandler<'D,'M>

/// Similar to MessageHandler, defined with interop-friendly types
and AsyncMessageHandler<'D,'M> = Func<IMessageContext<'D,'M>, Task<MessageResult<'D,'M>>>
   
/// Similar to TransitionHandler, defined with interop-friendly types
and AsyncTransitionHandler<'D,'M> = Func<TransitionContext<'D,'M>, Task<'D>>

/// Similar to IntialTransition, defined with interop-friendly types
type AsyncIntialTransition<'D> = Func<'D, Task<'D * StateName>>

/// Synchronous message handler, defined with interop-friendly types
type SyncMessageHandler<'D,'M> = Func<IMessageContext<'D,'M>, MessageResult<'D,'M>>

/// Synchronous transition handler, defined with interop-friendly types
type SyncTransitionHandler<'D,'M> = Func<TransitionContext<'D,'M>, 'D>

/// Synchronous initial transition function, defined with interop-friendly types
type SyncIntialTransition<'D> = Func<'D, 'D * StateName>

module internal Interop = 

   // No-op handlers
   let unhandledMessageAsync : MessageHandler<'D,'M> = fun _ -> async.Return MessageResult.Unhandled
   let emptyTransHandlerAsync : TransitionHandler<'D,'M> = fun transCtx -> async.Return transCtx.TargetData
   let emptyTransActionAsync : TransitionHandler<'D,'M> = fun transCtx -> async.Return transCtx.TargetData
   let emptyHandlerAsync : StateHandler<_,_> = 
      { OnMessage = unhandledMessageAsync; 
        OnEnter = emptyTransHandlerAsync; 
        OnExit = emptyTransHandlerAsync }


   /// Conversions from interop-friendly types
   module TransitionHandler =   
      let inline fromAsyncTransitionHandler (transitionHandler: AsyncTransitionHandler<_,_>) : TransitionHandler<_,_> = 
         transitionHandler.Invoke >> Async.AwaitTask
      let inline fromSyncTransitionHandler (transitionHandler: SyncTransitionHandler<_,_>) : TransitionHandler<_,_> = 
         transitionHandler.Invoke >> async.Return

   /// Conversions from interop-friendly types
   module MessageHandler = 
      let valueOrDefault (value:'T) (defaultValue:'T) = 
         if EqualityComparer.Default.Equals(value, Unchecked.defaultof<'T>) then defaultValue else value

      let emptyTransitionAction: AsyncTransitionHandler<_,_> = 
         Func<_,_>(fun transCtx -> Task.FromResult transCtx.TargetData)

      // Wrap message context in interop-friendly interface
      let wrap (ctx: MessageContext<_,_>) = 
         { new IMessageContext<_,_> with
            member __.Message = ctx.Message
            member __.Data = ctx.Data
            member __.GoTo (stateName, nextData, action) = 
               let nextData = valueOrDefault nextData ctx.Data
               let action = valueOrDefault action emptyTransitionAction |> TransitionHandler.fromAsyncTransitionHandler
               ctx.GoTo (stateName, nextData, action) 
            member __.Stay nextData = 
               let nextData = valueOrDefault nextData ctx.Data
               ctx.Stay nextData
            member __.GoToSelf (nextData, action) = 
               let nextData = valueOrDefault nextData ctx.Data
               let action = valueOrDefault action emptyTransitionAction |> TransitionHandler.fromAsyncTransitionHandler
               ctx.GoToSelf (nextData, action) 
            member __.Unhandled () = ctx.Unhandled()
            member __.Stop (reason, code) =
               ctx.Stop( (if isNull reason then "" else reason), code ) }

      let inline fromAsyncMessageHandler (messageHandler: AsyncMessageHandler<_,_>) : MessageHandler<_,_> =
         wrap >> messageHandler.Invoke >> Async.AwaitTask
      let inline fromSyncMessageHandler (messageHandler: SyncMessageHandler<_,_>) : MessageHandler<_,_> = 
         wrap >> messageHandler.Invoke >> async.Return
      
   /// Creates a StateHandler from async handler functions
   let stateHandlerFromAsync onMessage onEnter onExit : StateHandler<_,_> = 
       Async.toHandler 
            (onMessage |> Option.ofObj |> Option.map MessageHandler.fromAsyncMessageHandler) 
            (onEnter |> Option.ofObj |> Option.map TransitionHandler.fromAsyncTransitionHandler) 
            (onExit |> Option.ofObj |> Option.map TransitionHandler.fromAsyncTransitionHandler)

   /// Creates a StateHandler from synchronous handler functions
   let stateHandlerFromSync onMessage onEnter onExit : StateHandler<_,_> = 
        Async.toHandler 
            (onMessage |> Option.ofObj |> Option.map MessageHandler.fromSyncMessageHandler) 
            (onEnter |> Option.ofObj |> Option.map TransitionHandler.fromSyncTransitionHandler) 
            (onExit |> Option.ofObj |> Option.map TransitionHandler.fromSyncTransitionHandler)   

   /// Creates a StateHandler from synchronous handler functions
   let stateHandlerFromInteropHandler (stateHandler: IStateHandler<_,_>) : StateHandler<_,_> =
      { OnMessage = stateHandler.OnMessage |> MessageHandler.fromAsyncMessageHandler;
        OnEnter = stateHandler.OnEnter |> TransitionHandler.fromAsyncTransitionHandler;
        OnExit = stateHandler.OnExit |> TransitionHandler.fromAsyncTransitionHandler; } 


open Interop

/// <summary>
/// A builder that is used to define a 'flat' set of states for a state machine, without introducing hierarchical 
/// relationships between the states.
/// </summary>
type StateBuilder<'D,'M>() = 
   inherit StateBuilderBase<'D,'M>()

   /// Defines a state with the specified async handler functions.
   member this.DefineState
      ( name: StateName,
        [<Optional>] onMessage: AsyncMessageHandler<'D,'M>,
        [<Optional>] onEnter: AsyncTransitionHandler<'D,'M>,
        [<Optional>] onExit: AsyncTransitionHandler<'D,'M> ) =

      let build (lazyRoot: Lazy<State<_,_>>) = 
         lazy (Leaf (name, lazyRoot.Value, stateHandlerFromAsync onMessage onEnter onExit))
      this.AddState name build
      this

   /// Defines a state with the specified synchronous handler functions.
   member this.DefineState
      ( name: StateName,
        [<Optional>] onMessage: SyncMessageHandler<'D,'M>,
        [<Optional>] onEnter: SyncTransitionHandler<'D,'M>,
        [<Optional>] onExit: SyncTransitionHandler<'D,'M> ) =

      let build (lazyRoot: Lazy<State<_,_>>) =
         lazy (Leaf (name, lazyRoot.Value, stateHandlerFromSync onMessage onEnter onExit))
      this.AddState name build
      this

   /// Defines a state with a handler that will be created by the specified function, the first time the state is 
   /// entered.
   member this.DefineState
      ( name: StateName,
        createHandler: unit -> IStateHandler<'D,'M> ) = 
      
      let build (lazyRoot: Lazy<State<_,_>>) = 
         lazy (
            let interopHandler = createHandler()
            Leaf (name, lazyRoot.Value, stateHandlerFromInteropHandler interopHandler))
      this.AddState name build
      this

/// <summary>
/// A builder that can be used to define a hierarchical set of states for a state machine.
/// </summary>
type StateTreeBuilder<'D, 'M>() =
   inherit StateTreeBuilderBase<'D,'M>()

   /// Defines a root state with the specified asynchronous handler functions.
   member this.DefineRootState
      ( name: StateName,
        initialTransition: AsyncIntialTransition<'D>,
        [<Optional>] onMessage: AsyncMessageHandler<'D,'M>,
        [<Optional>] onEnter: AsyncTransitionHandler<'D,'M>,
        [<Optional>] onExit: AsyncTransitionHandler<'D,'M> ) = 
      
      let handler = stateHandlerFromAsync onMessage onEnter onExit
      this.SetRootState (Root(name, handler, initialTransition.Invoke >> Async.AwaitTask))
      this

   /// Defines a root state with the specified synchronous handler functions.
   member this.DefineRootState
      ( name: StateName,
        initialTransition: SyncIntialTransition<'D>,
        [<Optional>] onMessage: SyncMessageHandler<'D,'M>,
        [<Optional>] onEnter: SyncTransitionHandler<'D,'M>,
        [<Optional>] onExit: SyncTransitionHandler<'D,'M> ) = 
      
      let handler = stateHandlerFromSync onMessage onEnter onExit
      this.SetRootState (Root(name, handler, initialTransition.Invoke >> async.Return))
      this

   /// Defines an interior (non-root, non-leaf) state with the specified asynchronous handler functions.
   member this.DefineInteriorState
      ( name: StateName,
        parent: StateName,
        initialTransition: AsyncIntialTransition<'D>,
        [<Optional>] onMessage: AsyncMessageHandler<'D,'M>,
        [<Optional>] onEnter: AsyncTransitionHandler<'D,'M>,
        [<Optional>] onExit: AsyncTransitionHandler<'D,'M> ) = 

      let build (lazyParent: Lazy<State<_,_>>) = 
         let handler = stateHandlerFromAsync onMessage onEnter onExit
         lazy (Intermediate (name, lazyParent.Value, handler, initialTransition.Invoke >> Async.AwaitTask))
      this.AddChildState parent (name, build)
      this

   /// Defines an interior (non-root, non-leaf) state with the specified synchronous handler functions.
   member this.DefineInteriorState
      ( name: StateName,
        parent: StateName,
        initialTransition: SyncIntialTransition<'D>,
        [<Optional>] onMessage: SyncMessageHandler<'D,'M>,
        [<Optional>] onEnter: SyncTransitionHandler<'D,'M>,
        [<Optional>] onExit: SyncTransitionHandler<'D,'M> ) = 

      let build (lazyParent: Lazy<State<_,_>>) = 
         let handler = stateHandlerFromSync onMessage onEnter onExit
         lazy (Intermediate (name, lazyParent.Value, handler, initialTransition.Invoke >> async.Return))
      this.AddChildState parent (name, build)
      this

   /// Defines an interior (non-root, non-leaf) state with a handler that will be created by the specified function, 
   /// the first time the state is entered.
   member this.DefineInteriorState
      ( name: StateName,
        parent: StateName,
        initialTransition: AsyncIntialTransition<'D>,
        createHandler: unit -> IStateHandler<'D,'M> ) = 
      
      let build (lazyParent: Lazy<State<_,_>>) = 
         lazy (
            let interopHandler = createHandler()
            let handler = stateHandlerFromInteropHandler interopHandler
            Intermediate (name, lazyParent.Value, handler, initialTransition.Invoke >> Async.AwaitTask))
      this.AddChildState parent (name, build)
      this

   /// Defines an leaf state with the specified asynchronous handler functions.
   member this.DefineLeafState
      ( name: StateName,
        parent: StateName,
        [<Optional>] onMessage: AsyncMessageHandler<'D,'M>,
        [<Optional>] onEnter: AsyncTransitionHandler<'D,'M>,
        [<Optional>] onExit: AsyncTransitionHandler<'D,'M> ) = 

      let build (lazyParent: Lazy<State<_,_>>) = 
         let handler = stateHandlerFromAsync onMessage onEnter onExit
         lazy (Leaf (name, lazyParent.Value, handler))
      this.AddChildState parent (name, build)
      this

   /// Defines an leaf state with the specified synchronous handler functions.
   member this.DefineLeafState
      ( name: StateName,
        parent: StateName,
        [<Optional>] onMessage: SyncMessageHandler<'D,'M>,
        [<Optional>] onEnter: SyncTransitionHandler<'D,'M>,
        [<Optional>] onExit: SyncTransitionHandler<'D,'M> ) = 

      let build (lazyParent: Lazy<State<_,_>>) = 
         let handler = stateHandlerFromSync onMessage onEnter onExit
         lazy (Leaf (name, lazyParent.Value, handler))
      this.AddChildState parent (name, build)
      this

   /// Defines a leaf state with a handler that will be created by the specified function, the first time the state is 
   /// entered.
   member this.DefineLeafState
      ( name: StateName,
        parent: StateName,
        createHandler: unit -> IStateHandler<'D,'M> ) = 
      
      let build (lazyParent: Lazy<State<_,_>>) = 
         lazy (
            let interopHandler = createHandler()
            Leaf (name, lazyParent.Value, stateHandlerFromInteropHandler interopHandler))
      this.AddChildState parent (name, build)
      this
