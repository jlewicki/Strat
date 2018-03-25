namespace Strat.StateMachine.Test

open System
open System.Threading;
open Strat.StateMachine
open Strat.StateMachine.State
open Strat.StateMachine.Definition


// State tree for unit testing.
module TransitionsSm = 

   type Message = | M1 | M2 | SetContext of Data | Throw of Exception | Stop of Reason:string * Code:  int | Unknown

   and Data = {
      Tick: int
      Entered: List<StateId*int*TransitionContext<Data,Message>>
      Exited: List<StateId*int*TransitionContext<Data,Message>>
      B1ToB2TransitionTick: option<int>
   }

   // Shortcut aliases
   and TransitionContext = TransitionContext<Data, Message>
   type StateHandler = StateHandler<Data, Message>
   type State = State<Data, Message>
   type MessageContext = MessageContext<Data, Message>    

   // Names of the states in the state tree
   let stateRoot = "root"
   let stateA = "A"
   let stateA1 = "A1"
   let stateA2 = "A2"
   let stateA2A = "A2A"
   let stateB = "B"
   let stateB1 = "B1"
   let stateB2 = "B2"
   let stateC = "C"
   let stateC1 = "C1"
   let stateC2 = "C2"
   let stateErrorOnEnter = "ErrorOnEnter"
   let stateSleepOnEnter = "SleepOnEnter"
   let stateSleepOnExit = "SleepOnExit"

   let stateErrorOnEnterMessage = "Thrown when entering B3"
   let stateSleepOnEnterExitSleepInSeconds = 0.1
   
   // Handler functions
   let onEnter (transCtx: TransitionContext) =
      // Deliberate conditions for testing purposes      
      if transCtx.HandlingState.Id = stateErrorOnEnter then
         raise <| Exception(stateErrorOnEnterMessage)
      elif transCtx.HandlingState.Id = stateSleepOnEnter then
         Thread.Sleep(TimeSpan.FromSeconds stateSleepOnEnterExitSleepInSeconds)

      let ctx = transCtx.TargetData
      { ctx with Entered = (transCtx.HandlingState.Id, ctx.Tick, transCtx)::ctx.Entered; Tick = ctx.Tick + 1;} 
  
   let onExit (transCtx: TransitionContext) = 
      // Deliberate conditions for testing purposes      
      if transCtx.HandlingState.Id = stateSleepOnExit then
         Thread.Sleep(TimeSpan.FromSeconds stateSleepOnEnterExitSleepInSeconds)

      let ctx = transCtx.TargetData
      { ctx with Exited = (transCtx.HandlingState.Id, ctx.Tick, transCtx)::ctx.Exited; Tick = ctx.Tick + 1} 
   

   let rootHandler (msgCtx:MessageContext) = 
      match msgCtx.Message with
      | Throw(ex) -> raise ex
      | SetContext(ctx) -> msgCtx.Stay(ctx)
      | Stop(reason,code) -> msgCtx.Stop(reason, code)
      | _ -> MessageResult.Unhandled

   let a2aHandler (msgCtx:MessageContext) = 
      match msgCtx.Message with
      | M1 -> msgCtx.GoTo(stateA1)
      | _ -> MessageResult.Unhandled

   let b1Handler (msgCtx:MessageContext) = 
      let b1Tob2TransitionAction (transCtx:TransitionContext) = 
         let ctx = transCtx.TargetData
         { ctx with B1ToB2TransitionTick = Some(ctx.Tick); Tick = ctx.Tick + 1 }
      match msgCtx.Message with
      | M1 -> msgCtx.GoTo(stateB2, action = TransitionHandler.Sync b1Tob2TransitionAction)
      | _ -> MessageResult.Unhandled

   let sleepOnExitHandler  (msgCtx:MessageContext) = 
      match msgCtx.Message with
      | M1 -> msgCtx.GoToSelf()  // This will exit/reenter
      | _ -> MessageResult.Unhandled

   let cHandler (msgCtx:MessageContext) = 
      match msgCtx.Message with
      | M1 -> msgCtx.GoToSelf() 
      | _ -> MessageResult.Unhandled

   let c2Handler (msgCtx:MessageContext) = 
      match msgCtx.Message with
      | M1 -> msgCtx.GoToSelf()
      | _ -> MessageResult.Unhandled

   // Helper module for wiring in common OnEnter/OnExit handlers
   module Handle  = 
      let withDefaults msgHandler : StateHandler<_,_> = 
         { OnMessage = MessageHandler.Sync msgHandler; OnEnter = TransitionHandler.Sync onEnter; OnExit = TransitionHandler.Sync onExit }

      let emptyHandler = 
         (withDefaults (fun _ -> MessageResult.Unhandled))

   // Definition of the state tree.
   open StateTree
   let stateTree = 
      StateTree.fromRoot stateRoot (Handle.withDefaults rootHandler) (Start.With stateA)
         [ interior stateA  Handle.emptyHandler (Start.With stateA1)
            [ leaf stateA1 Handle.emptyHandler
              interior stateA2  Handle.emptyHandler (Start.With stateA2A)
               [ leaf stateA2A (Handle.withDefaults a2aHandler)] ] 
           interior stateB Handle.emptyHandler (Start.With stateB1)
            [ leaf stateB1 (Handle.withDefaults b1Handler)
              leaf stateB2 Handlers.emptyHandler
              leaf stateErrorOnEnter Handle.emptyHandler
              leaf stateSleepOnEnter Handle.emptyHandler
              leaf stateSleepOnExit (Handle.withDefaults sleepOnExitHandler) ]
           interior stateC (Handle.withDefaults cHandler) (Start.With stateC2)
            [ leaf stateC1 Handlers.emptyHandler
              leaf stateC2 (Handle.withDefaults c2Handler) ] ]

   let initData : Data = {
      Tick = 1
      Entered = List.empty
      Exited = List.empty
      B1ToB2TransitionTick = None
   }

   // Helper method to initialize state machine context
   let initializeContext initState initData =
      let smContext = Machine.initializeContext (stateTree) initData (Some(initState)) |> Async.RunSynchronously 
      // Reset data to make unit testing easier.
      { smContext with Data = initData }

   //let newStateMachine initData initState = 
   //   new StateMachineAgent<Data,Message>(stateTree, initData, initState)

   //let private start resetData initData initState = 
   //   let sm  = newStateMachine initData initState
   //   sm.Start()
   //   if resetData then sm.SendMessage(SetContext(initData)) |> ignore
   //   sm

   //// Helper method to create and start a new state machine agent
   //let startStateMachine = start false



   
// http://accu.org/index.php/journals/252
module ExampleHsm = 

   // State machine data type
   type Data = {
      Foo: bool
   }

   // State machine message type
   type Message = 
      | SetData of Data | A | B | C | D | E | F | G | H

   // Shortcut aliases
   type State = State<Data, Message>
   type TransitionContext = TransitionContext<Data, Message>
   type MessageContext = MessageContext<Data, Message>

   // Names of the states in the state tree
   let s0 = "S0"
   let s1 = "S1"
   let s11 = "S11"
   let s2 = "S2"
   let s21 = "S21"
   let s211 = "S211"

   // Handler functions
   let s0Handler = MessageHandler.Sync (fun msgCtx -> 
      match msgCtx.Message with
      | SetData(data) -> msgCtx.Stay data
      | E -> msgCtx.GoTo s211
      | _ -> MessageResult.Unhandled)

   let s1Handler = MessageHandler.Sync (fun msgCtx -> 
      match msgCtx.Message with
      | A -> msgCtx.Stay()
      | B -> msgCtx.GoTo s11
      | C -> msgCtx.GoTo s2
      | D -> msgCtx.GoTo s1
      | F -> msgCtx.GoTo s211
      | _ -> MessageResult.Unhandled)

   let s11Handler  = MessageHandler.Sync (fun msgCtx -> 
      match msgCtx.Message with
      | G -> msgCtx.GoTo s211
      | H when msgCtx.Data.Foo -> msgCtx.Stay { msgCtx.Data with Foo = false }
      | _ -> MessageResult.Unhandled)

   let s2Handler  = MessageHandler.Sync (fun msgCtx -> 
      match msgCtx.Message with
      | C -> msgCtx.GoTo s1
      | F -> msgCtx.GoTo s11
      | _ -> MessageResult.Unhandled)

   let s21Handler  = MessageHandler.Sync (fun msgCtx -> 
      match msgCtx.Message with
      | B -> msgCtx.GoTo s211
      | H when not msgCtx.Data.Foo -> msgCtx.GoToSelf { msgCtx.Data with Foo = true }
      | _ -> MessageResult.Unhandled)

   let s211Handler  = MessageHandler.Sync (fun msgCtx -> 
      match msgCtx.Message with
      | D -> msgCtx.GoTo s21
      | G -> msgCtx.GoTo s0
      | _ -> MessageResult.Unhandled)


   open StateTree
   // Definition of the state tree.
   let tree = 
      StateTree.fromRoot s0 (Handle.With s0Handler) (Start.With s1)
         [ interior s1 (Handle.With s1Handler) (Start.With s11)
            [ leaf s11 (Handle.With s11Handler) ]
           interior s2 (Handle.With s2Handler) (Start.With s21) 
            [ interior s21 (Handle.With s21Handler) (Start.With s211) 
               [ leaf s211 (Handle.With s211Handler) ] ] ]








