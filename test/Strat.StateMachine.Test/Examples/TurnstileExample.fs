namespace Strat.StateMachine.Test.Examples

open Xunit
open Strat.StateMachine
open Strat.StateMachine.Definition


module TurnstileExample =
   
   type Data = unit

   type Message = 
      | DepositCoin
      | Push
   
   // Shortcut alias
   type MessageContext = MessageContext<Data, Message>
   type StateMachineContext = StateMachineContext<Data, Message>

   // Names of the states in the state tree
   let lockedState = "locked"
   let unlockedState = "unlocked"

   // Handler functions
   let lockedHandler = MessageHandler.Sync (fun msgCtx ->  
      match msgCtx.Message with
      | DepositCoin -> msgCtx.GoTo unlockedState
      | _ -> Unhandled)

   let unlockedHandler = MessageHandler.Sync (fun msgCtx ->  
      match msgCtx.Message with
      | Push -> msgCtx.GoTo lockedState
      | _ -> Unhandled)

   // Definition of the state tree.
   open StateTree
   let tree : StateTree<Data,Message> = 
      StateTree.fromLeaves lockedState
         [ leaf lockedState (Handle.With lockedHandler)
           leaf unlockedState (Handle.With unlockedHandler) ]
   
   let initContext state : StateMachineContext = 
      Machine.initializeContext tree () (Some state) |> Async.RunSynchronously


   [<Fact>]
   let DepositCoin_when_locked_should_transition_to_Unlocked() =
      let smCtx = initContext lockedState
      match Machine.processMessage DepositCoin smCtx |> Async.RunSynchronously with
      | HandledMessage handled ->
         Assert.Equal (unlockedState, handled.NextState.Id)
      | _ -> invalidOp "Message should have been handled"

   [<Fact>]
   let Push_when_unlocked_should_transition_to_Locked()  = 
      let smCtx = initContext unlockedState
      match Machine.processMessage Push smCtx |> Async.RunSynchronously with
      | HandledMessage handled ->
         Assert.Equal (lockedState, handled.NextState.Id)
      | _ -> invalidOp "Message should have been handled"