namespace Strat.StateMachine.Test

open System
open Strat.StateMachine
open Xunit


module Machine =
   
   open TransitionsSm

   module InitializeContext =      
      [<Fact>]
      let should_enter_initial_state_of_root() = 
         let smCtx = Machine.initializeContext stateTree initData None |> Async.RunSynchronously 

         Assert.Equal(stateA1, smCtx.State.Id)
         Assert.Equal(3, smCtx.Data.Entered.Length)
         [stateRoot; stateA; stateA1] 
         |> Seq.zip (smCtx.Data.Entered  |> List.rev |> Seq.map (fun (entered, _, _) -> entered ))
         |> Seq.iter Assert.Same

      [<Fact>]
      let should_enter_initial_state_of_starting_state() = 
         let smCtx = Machine.initializeContext stateTree initData (Some(stateA2)) |> Async.RunSynchronously  
      
         Assert.Equal(stateA2A, smCtx.State.Id)
         Assert.Equal(4, smCtx.Data.Entered.Length)
         [stateRoot; stateA; stateA2; stateA2A] 
         |> Seq.zip (smCtx.Data.Entered  |> List.rev |> Seq.map (fun (entered, _, _) -> entered ))
         |> Seq.iter Assert.Same


   module ProcessMessage = 
      [<Fact>]
      let should_exit_from_current_state_up_to_LCA_when_transitioning() = 
         let smCtx = TransitionsSm.initializeContext stateA2A initData 

         // M1 will cause transition to A1
         let processed = Machine.processMessage M1 smCtx |> Async.RunSynchronously

         match processed with
         | HandledMessage(handled) ->
            let smCtx = handled.NextContext

            // Verify state transition
            Assert.Equal(stateA1, handled.NextContext.State.Id)
            Assert.Equal(2, handled.ExitedStates.Length)

            // Verify that OnExit functions were called.
            Assert.Equal(2, smCtx.Data.Exited.Length)
            [stateA2A; stateA2] 
            |> Seq.zip (smCtx.Data.Exited |> List.rev |> Seq.map (fun (exited, _, transCtx) -> 
               // Make sure transCtx.PrevState always records original state, and NextState records the final destination state.
               Assert.Equal(stateA2A, transCtx.SourceState.Id)
               Assert.Equal(stateA1, transCtx.TargetState.Id)
               // Make sure original data at start of transition is recorded.
               Assert.Same( initData, transCtx.SourceData)
               exited ))
            |> Seq.iter Assert.Same
         | _ -> Assert.False(true)

      [<Fact>]
      let should_enter_from_LCA_down_To_next_state_when_transitioning() = 
         let smCtx = TransitionsSm.initializeContext stateA2A initData 

         // M1 will cause transition to A1
         let processed = Machine.processMessage M1 smCtx |> Async.RunSynchronously

         match processed with
         | HandledMessage(handled) ->
            let smCtx = handled.NextContext

            // Verify state transition
            Assert.Equal(stateA1, handled.NextContext.State.Id)
            Assert.Equal(1, handled.EnteredStates.Length)

            // Verify that OnEnter functions were called.
            Assert.Equal(1, handled.NextContext.Data.Entered.Length)
            [stateA1 ] 
            |> Seq.zip (smCtx.Data.Entered |> List.rev |> Seq.map (fun (entered, _, transCtx) -> 
               // Make sure transCtx.PrevState always records original state, and NextState records the final destination state.
               Assert.Equal(stateA2A, transCtx.SourceState.Id)
               Assert.Equal(stateA1, transCtx.TargetState.Id)
               // Make sure original data at start of transition is recorded.
               Assert.Same( initData, transCtx.SourceData)
               entered ))
            |> Seq.iter Assert.Same
         | _ -> Assert.False(true)

      [<Fact>]
      let should_run_transition_action_after_OnExit_and_before_OnEnter() = 
         let smCtx = TransitionsSm.initializeContext stateB1 initData 

         // This transitions to State B_2
         let processed = Machine.processMessage M1 smCtx |> Async.RunSynchronously

         match processed with
         | HandledMessage(handled) ->
            let smCtx = handled.NextContext
            Assert.Equal(stateB2, smCtx.State.Id)
            Assert.True( smCtx.Data.B1ToB2TransitionTick.IsSome )

            let actionTick =  smCtx.Data.B1ToB2TransitionTick.Value
            // Exit state B1 is tick1, action is tick2, enter B2 is tick 3
            Assert.Equal(2, actionTick)
            Assert.True( smCtx.Data.Exited |> Seq.forall (fun (_, tick, _) -> tick < actionTick))
            Assert.True( smCtx.Data.Entered |> Seq.forall (fun (_, tick, _) -> tick > actionTick))
         | _ -> Assert.False(true)

      [<Fact>]
      let should_reenter_current_state_when_self_transitioning_from_parent_state() = 
         let smCtx = TransitionsSm.initializeContext stateC1 initData 

         // M1 is handled by C, which does a GoToSelf() transition, which should re-enter current state C1 .
         let processed = Machine.processMessage M1 smCtx |> Async.RunSynchronously
      
         match processed with
         | HandledMessage(handled) ->
            let smCtx = handled.NextContext
            Assert.Equal(stateC1, smCtx.State.Id)

            Assert.Equal(1, handled.ExitedStates.Length)
            Assert.Equal(stateC1, handled.ExitedStates.Head |> State.id)
            Assert.Equal(1, handled.EnteredStates.Length)
            Assert.Equal(stateC1, handled.EnteredStates.Head |> State.id)
         | _ -> Assert.False(true)

      [<Fact>]
      let should_reenter_the_current_state_when_self_transitioning() = 
         let smCtx = TransitionsSm.initializeContext stateC2 initData 

         // M1 is handled by C2, which does a GoToSelf() transition, which should re-enter current state C2.
         let processed = Machine.processMessage M1 smCtx |> Async.RunSynchronously
      
         match processed with
         | HandledMessage(handled) ->
            let smCtx = handled.NextContext
            Assert.Equal(stateC2, smCtx.State.Id)
            Assert.Equal(1, handled.ExitedStates.Length)
            Assert.Equal(stateC2, handled.ExitedStates.Head |> State.id)
            Assert.Equal(1, handled.EnteredStates.Length)
            Assert.Equal(stateC2, handled.EnteredStates.Head |> State.id)
         | _ -> Assert.False(true)

      [<Fact>]
      let should_transition_to_terminal_state_when_stopping() = 
         let smCtx = TransitionsSm.initializeContext stateA2A initData 

         let reason, code = "because", 1
         let processed = Machine.processMessage (Stop(reason, code)) smCtx |> Async.RunSynchronously
      
         match processed with
         | HandledMessage(handled) ->
            match handled.NextContext.State with
            | Terminal(parent,fromState,Some(stopReason)) ->
               // Stop message is handled by the root state for this particular state machine.
               Assert.Equal(stateRoot, fromState)
               Assert.Equal(reason, stopReason.Reason)
               Assert.Equal(code, stopReason.Code)
            | _ -> Assert.False(true)
         | _ -> Assert.False(true)

      [<Fact>]
      let should_throw_if_message_handler_throws() = 
         let smCtx = TransitionsSm.initializeContext stateA2A initData
         let ex = new ArgumentOutOfRangeException()
         let thrown = Assert.Throws( typeof<ArgumentOutOfRangeException>,  fun() -> 
            Machine.processMessage (Throw(ex)) smCtx |> Async.RunSynchronously |> ignore )
         Assert.Same( ex, thrown.GetBaseException() )


   module Stop =    
      [<Fact>]
      let should_transition_to_terminal_state() = 
         let smCtx = TransitionsSm.initializeContext stateA2A initData 
         let tree = smCtx.StateTree
         let root = smCtx.StateTree |> StateTree.rootState

         let reason = Some (StopReason ("because", 1))
         let smCtx = Machine.stop smCtx reason |> Async.RunSynchronously 

         match smCtx.State with
         | Terminal(parent, fromState, optReason) ->
            Assert.Same( root, tree |> StateTree.findState parent)
            Assert.Equal( stateA2A, fromState)
            Assert.Equal( reason, optReason )
         | _ -> Assert.False( true )

      [<Fact>]
      let should_throw_if_already_stopped() = 
         let smCtx = TransitionsSm.initializeContext stateA2A initData 
         let root = smCtx.StateTree |> StateTree.rootState

         let reason = Some( StopReason("because", 1))
         let smCtx = Machine.stop smCtx reason |> Async.RunSynchronously 

         Assert.Throws( typeof<ArgumentException>,  fun() -> 
            async {
               let! smCtx = Machine.stop smCtx reason
               let! smCtx = Machine.stop smCtx reason
               return smCtx
            } 
            |> Async.RunSynchronously
            |> ignore )