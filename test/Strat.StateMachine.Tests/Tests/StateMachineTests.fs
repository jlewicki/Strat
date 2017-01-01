namespace Strat.StateMachine.Test

open System
open Strat.StateMachine
open Xunit
open TransitionsSm


module InitializeContextTests = 
   
   [<Fact>]
   let InitializeContext_Should_Enter_Initial_State_Of_Root() = 

      let smCtx = StateMachine.initializeContext stateTree initData None |> Async.RunSynchronously 

      Assert.Equal(stateA1, smCtx.State.Name)
      Assert.Equal(3, smCtx.Data.Entered.Length)
      [stateRoot; stateA; stateA1] 
      |> Seq.zip (smCtx.Data.Entered  |> List.rev |> Seq.map (fun (entered, _, _) -> entered ))
      |> Seq.iter Assert.Same


   [<Fact>]
   let InitializeContext_Should_Enter_Initial_State_Of_Starting_State() = 
      let smCtx = StateMachine.initializeContext stateTree initData (Some(stateA2)) |> Async.RunSynchronously  
      
      Assert.Equal(stateA2A, smCtx.State.Name)
      Assert.Equal(4, smCtx.Data.Entered.Length)
      [stateRoot; stateA; stateA2; stateA2A] 
      |> Seq.zip (smCtx.Data.Entered  |> List.rev |> Seq.map (fun (entered, _, _) -> entered ))
      |> Seq.iter Assert.Same


module TransitionTests = 

   [<Fact>]
   let Transition_Should_Exit_From_Current_State_Up_To_LCA() = 
      let smCtx = TransitionsSm.initializeContext stateA2A initData 

      // M1 will cause transition to A1
      let processed = StateMachine.processMessage M1 smCtx |> Async.RunSynchronously

      match processed with
      | HandledMessage(handled) ->
         let smCtx = handled.NextContext

         // Verify state transition
         Assert.Equal(stateA1, handled.NextContext.State.Name)
         Assert.Equal(2, handled.ExitedStates.Length)

         // Verify that OnExit functions were called.
         Assert.Equal(2, smCtx.Data.Exited.Length)
         [stateA2A; stateA2] 
         |> Seq.zip (smCtx.Data.Exited |> List.rev |> Seq.map (fun (exited, _, transCtx) -> 
            // Make sure transCtx.PrevState always records original state, and NextState records the final destination state.
            Assert.Equal(stateA2A, transCtx.SourceState.Name)
            Assert.Equal(stateA1, transCtx.TargetState.Name)
            // Make sure original data at start of transition is recorded.
            Assert.Same( initData, transCtx.SourceData)
            exited ))
         |> Seq.iter Assert.Same
      | _ -> Assert.False(true)


   [<Fact>]
   let Transition_Should_Enter_From_LCA_Down_To_Next_State() = 
      let smCtx = TransitionsSm.initializeContext stateA2A initData 

      // M1 will cause transition to A1
      let processed = StateMachine.processMessage M1 smCtx |> Async.RunSynchronously

      match processed with
      | HandledMessage(handled) ->
         let smCtx = handled.NextContext

         // Verify state transition
         Assert.Equal(stateA1, handled.NextContext.State.Name)
         Assert.Equal(1, handled.EnteredStates.Length)

         // Verify that OnEnter functions were called.
         Assert.Equal(1, handled.NextContext.Data.Entered.Length)
         [stateA1 ] 
         |> Seq.zip (smCtx.Data.Entered |> List.rev |> Seq.map (fun (entered, _, transCtx) -> 
            // Make sure transCtx.PrevState always records original state, and NextState records the final destination state.
            Assert.Equal(stateA2A, transCtx.SourceState.Name)
            Assert.Equal(stateA1, transCtx.TargetState.Name)
            // Make sure original data at start of transition is recorded.
            Assert.Same( initData, transCtx.SourceData)
            entered ))
         |> Seq.iter Assert.Same
      | _ -> Assert.False(true)


   [<Fact>]
   let Transition_Action_Is_Called_After_OnExit_And_Before_On_Enter() = 
      let smCtx = TransitionsSm.initializeContext stateB1 initData 

      // This transitions to State B_2
      let processed = StateMachine.processMessage M1 smCtx |> Async.RunSynchronously

      match processed with
      | HandledMessage(handled) ->
         let smCtx = handled.NextContext
         Assert.Equal(stateB2, smCtx.State.Name)
         Assert.True( smCtx.Data.B1ToB2TransitionTick.IsSome )

         let actionTick =  smCtx.Data.B1ToB2TransitionTick.Value
         // Exit state B1 is tick1, action is tick2, enter B2 is tick 3
         Assert.Equal(2, actionTick)
         Assert.True( smCtx.Data.Exited |> Seq.forall (fun (_, tick, _) -> tick < actionTick))
         Assert.True( smCtx.Data.Entered |> Seq.forall (fun (_, tick, _) -> tick > actionTick))
      | _ -> Assert.False(true)


   [<Fact>]
   let Stopping_Should_Transition_To_Terminal_State() = 
      let smCtx = TransitionsSm.initializeContext stateA2A initData 

      let reason,code = "because", 1
      let processed = StateMachine.processMessage (Stop(reason, code)) smCtx |> Async.RunSynchronously
      
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


module ErrorHandlingTests =

   [<Fact>]
   let ProcessMessage_Should_Throw_If_Message_Handler_Throws() = 
      let smCtx = TransitionsSm.initializeContext stateA2A initData 

      let ex = new ArgumentOutOfRangeException()
      let thrown = Assert.Throws( typeof<ArgumentOutOfRangeException>,  fun() -> 
         StateMachine.processMessage (Throw(ex)) smCtx |> Async.RunSynchronously |> ignore )
      Assert.Same( ex, thrown.GetBaseException() )


module StopTests = 
   
   [<Fact>]
   let Stop_Should_Transition_To_TerminalState() = 
      let smCtx = TransitionsSm.initializeContext stateA2A initData 
      let root = smCtx.StateTree.Root

      let reason = Some({ Reason="because"; Code = 1 })
      let smCtx = StateMachine.stop smCtx reason |> Async.RunSynchronously 

      match smCtx.State with
      | Terminal(parent, fromState, optReason) ->
         Assert.Same( root, parent )
         Assert.Equal( stateA2A, fromState)
         Assert.Equal( reason, optReason )
      | _ -> Assert.False( true )


   [<Fact>]
   let Stop_Should_Throw_If_Already_Stopped() = 
      let smCtx = TransitionsSm.initializeContext stateA2A initData 
      let root = smCtx.StateTree.Root

      let reason = Some({ Reason="because"; Code = 1 })
      let smCtx = StateMachine.stop smCtx reason |> Async.RunSynchronously 

      Assert.Throws( typeof<ArgumentException>,  fun() -> 
         async {
            let! smCtx = StateMachine.stop smCtx reason
            let! smCtx = StateMachine.stop smCtx reason
            return smCtx
         } 
         |> Async.RunSynchronously
         |> ignore )