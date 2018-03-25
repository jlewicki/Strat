namespace Strat.StateMachine.Test

open System
open System.Threading
open Strat.StateMachine
open Xunit
open Strat.StateMachine.Test.TransitionsSm

//module AgentStopTests = 
   
//   let assertStopped lifecycle (expFromState, expStopType,  expReason) = 
//      match lifecycle with
//      | Stopped(fromState, stopType, _, reason ) ->
//         Assert.Equal(expFromState, fromState)
//         Assert.Equal(expStopType, stopType)
//         Assert.Equal(expReason, reason)
//      | _ -> 
//         Assert.True(false,"Lifecycle should be stopped")


//   [<Fact>]
//   let Internal_Stop_Should_Set_Lifecycle_To_Stopped() = 
//      let sm = startStateMachine initData stateA2A
//      let reason,code = "because", 1
//      sm.SendMessage (Stop(reason, code)) |> ignore
//      assertStopped sm.Lifecycle (stateRoot, StopType.Internal, (Some({ Reason=reason; Code=code })))


//   [<Fact>]
//   let External_Stop_Should_Set_Lifecycle_To_Stopped() = 
//      let sm = startStateMachine initData stateA2A
//      let reason,code = "because", 1
//      sm.Stop(reason, code)
//      assertStopped sm.Lifecycle (stateA2A, StopType.External, (Some({ Reason=reason; Code=code })))


//   [<Fact>]
//   let External_Stop_Should_Raise_Stopped_Event() = 
//      let threadID = Threading.Thread.CurrentThread.ManagedThreadId
//      let mutable stoppedRaised = false
//      let mutable raisedOnThreadID = -1
//      use sm = startStateMachine initData stateA2A
//      sm.Stopped.Add(fun _ -> 
//         stoppedRaised <- true
//         raisedOnThreadID <- Threading.Thread.CurrentThread.ManagedThreadId )
//      sm.Stop()
//      Assert.True(stoppedRaised)
//      Assert.Equal(threadID, raisedOnThreadID)


//   [<Fact>]
//   let Internal_Stop_Should_Raise_Stopped_Event() = 
//      let threadID = Threading.Thread.CurrentThread.ManagedThreadId
//      let mutable raisedOnThreadID = None
//      let lockObj = new obj()
//      use sm = startStateMachine initData stateA2A
//      sm.Stopped.Add( fun _ -> 
//         raisedOnThreadID <- Some(Threading.Thread.CurrentThread.ManagedThreadId)
//         lock lockObj (fun() -> Monitor.Pulse lockObj))
//      let reason,code = "because", 1
   
//      sm.SendMessage (Stop(reason, code)) |> ignore
      
//      // Make sure event is raised on a different thread.
//      Assert.True( lock lockObj (fun() -> Monitor.Wait(lockObj, TimeSpan.FromSeconds 1.0))) 
//      Assert.True(raisedOnThreadID.IsSome)
//      Assert.NotEqual(threadID, raisedOnThreadID.Value)


//   [<Fact>]
//   let Stop_Should_Throw_If_Timeout_Expires() = 
//      let sm = startStateMachine initData stateSleepOnExit
//      let timeout = stateSleepOnEnterExitSleepInSeconds / 2.0
//      Assert.Throws<TimeoutException>( fun() -> sm.Stop(timeout=TimeSpan.FromSeconds timeout) ) |> ignore


//   [<Fact>]
//   let StopAsync_Should_Throw_If_Timeout_Expires() = 
//      let sm = startStateMachine initData stateSleepOnExit
//      let timeout = stateSleepOnEnterExitSleepInSeconds / 2.0
//      let ex = Assert.Throws<AggregateException>( fun() -> sm.StopAsync(timeout=TimeSpan.FromSeconds timeout).Wait() )
//      Assert.IsAssignableFrom<TimeoutException>( ex.GetBaseException() )


//   [<Fact>]
//   let Stop_Should_Only_Raise_Stopped_Event_Once() = 
//      let mutable stoppedRaised = false
//      let sm = startStateMachine initData stateA2A
//      sm.Stopped.Add(fun _ -> stoppedRaised <- true)
//      sm.Stop()
//      Assert.True(stoppedRaised)
//      stoppedRaised <- false
//      sm.Stop()
//      // Stopping second time should not raise event
//      Assert.False(stoppedRaised)


//module AgentDisposeTests = 

//   [<Fact>]
//   let Dispose_Should_Set_StateMachineState_To_Stopped() = 
//      let sm = startStateMachine initData stateA2A
//      (sm :> IDisposable).Dispose()
//      Assert.True(sm |> StateMachineAgent.isStopped)



//module AgentErrorHandlingTests = 

//   [<Fact>]
//   let SendMessage_Should_Throw_When_MessageHandler_Throws() = 
//      use sm = startStateMachine initData stateA2A
//      let ex = new ArgumentException()
//      let thrown = Assert.Throws( typeof<InvalidOperationException>,  fun() -> sm.SendMessage (Throw(ex)) |> ignore )
//      Assert.Same( ex, thrown.GetBaseException() )


//   [<Fact>]
//   let SendMessageAsync_Should_Throw_When_MessageHandler_Throws() = 
//      use sm = startStateMachine initData stateA2A
//      let ex = new ArgumentException()
//      let thrown = 
//         Assert.Throws( typeof<AggregateException>,  fun() -> 
//            (sm.SendMessageAsync (Throw ex)).Result |> ignore )
//      Assert.Same( ex, thrown.GetBaseException().InnerException  )


//   [<Fact>]
//   let Start_Should_Throw_When_Initial_Transition_Throws() = 
//      // Entering B3 will throw
//      use sm = newStateMachine initData stateErrorOnEnter
//      let thrown = Assert.Throws( typeof<InvalidOperationException>, fun() -> sm.Start() )
//      Assert.Same( stateErrorOnEnterMessage, thrown.GetBaseException().Message )


//module AgentStartTests = 

//   [<Fact>]
//   let Start_Should_Set_State_To_Started() =
//      use sm = newStateMachine initData stateA2A
//      Assert.True( sm |> StateMachineAgent.isNew )
//      sm.Start()
//      Assert.True( sm |> StateMachineAgent.isStarted )


//   [<Fact>]
//   let Start_After_Already_Started_Should_Throw() = 
//      use sm = startStateMachine initData stateA2A
//      Assert.Throws<InvalidOperationException>( fun() -> sm.Start() ) |> ignore


//   [<Fact>]
//   let SendMessage_Before_Starting_Should_Throw() = 
//      use sm = newStateMachine initData stateA2A
//      Assert.Throws<InvalidOperationException>( fun() -> sm.SendMessage M1 |> ignore ) |> ignore


//   [<Fact>]
//   let Start_Should_Throw_If_Timeout_Expires() = 
//      let sm = newStateMachine initData stateSleepOnEnter
//      let timeout = stateSleepOnEnterExitSleepInSeconds / 2.0
//      Assert.Throws<TimeoutException>( fun() -> sm.Start(TimeSpan.FromSeconds timeout) ) |> ignore


//   [<Fact>]
//   let StartAsync_Should_Throw_If_Timeout_Expires() = 
//      let sm = newStateMachine initData stateSleepOnEnter
//      let timeout = stateSleepOnEnterExitSleepInSeconds / 2.0
//      let ex = Assert.Throws<AggregateException>( fun() -> 
//         sm.StartAsync(TimeSpan.FromSeconds timeout).Wait() |> ignore)
//      Assert.IsAssignableFrom<TimeoutException>( ex.GetBaseException() )


//module AgentSendMessageTests = 

//   [<Fact>]
//   let SendMessage_Should_Raise_Transitioned_When_Transition_Occurs() = 
//      let threadID = Threading.Thread.CurrentThread.ManagedThreadId
//      let mutable raisedOnThreadID = None
//      let lockObj = new obj()
//      use sm = startStateMachine initData stateA2A
//      sm.Transitioned.Add (fun _ -> 
//         raisedOnThreadID <- Some(Threading.Thread.CurrentThread.ManagedThreadId)
//         lock lockObj (fun() -> Monitor.Pulse lockObj))
      
//      // M1 will transition to stateA1
//      sm.SendMessage M1 |> ignore
      
//      // Make sure event is raised on a different thread.
//      Assert.True( lock lockObj (fun() -> Monitor.Wait(lockObj, TimeSpan.FromSeconds 1.0))) 
//      Assert.True(raisedOnThreadID.IsSome)
//      Assert.NotEqual(threadID, raisedOnThreadID.Value)


//   [<Fact>]
//   let SendMessageAsync_Should_Raise_Transitioned_When_Transition_Occurs() = 
//      let threadID = Threading.Thread.CurrentThread.ManagedThreadId
//      let mutable raisedOnThreadID = -1
//      let lockObj = new obj()
//      use sm = startStateMachine initData stateA2A
//      sm.Transitioned.Add (fun _ -> 
//         raisedOnThreadID <- Threading.Thread.CurrentThread.ManagedThreadId
//         lock lockObj (fun() -> 
//            Monitor.Pulse lockObj) ) 
      
//      // M1 will transition to stateA1
//      (sm.SendMessageAsync M1) |> ignore
      
//      Assert.True( lock lockObj (fun() -> Monitor.Wait(lockObj, TimeSpan.FromSeconds 1.0))) 
//       // Make sure event is raised on a different thread.
//      Assert.NotEqual(threadID, raisedOnThreadID)

//   [<Fact>]
//   let SendMessage_Should_Not_Raise_Transitioned_When_No_Transition_Occurs() = 
//      let mutable transitionedRaised = false
//      let sm = startStateMachine initData stateA2A
//      sm.Transitioned.Add (fun args -> transitionedRaised <- true ) 
      
//      // Setting context will stay in current state, s no transition occurs
//      sm.SendMessage (SetContext initData) |> ignore
        
//      // Stop state machine so that we know any Transitioned events would have been raised.
//      sm.Stop()
      
//      Assert.False( transitionedRaised )


//   [<Fact>]
//   let SendMessageAsync_Should_Not_Raise_Transitioned_When_No_Transition_Occurs() = 
//      let mutable transitionedRaised = false
//      let sm = startStateMachine initData stateA2A
//      sm.Transitioned.Add (fun args -> transitionedRaised <- true ) 
      
//      // Setting context will stay in current state, s no transition occurs
//      (sm.SendMessageAsync (SetContext initData)).Wait()
        
//      // Stop state machine so that we know any Transitioned events would have been raised.
//      sm.Stop()
      
//      Assert.False( transitionedRaised )

   
//   [<Fact>]
//   let SendMessage_Should_Throw_If_Timeout_Expires() = 
//      let sm = startStateMachine initData stateSleepOnExit
//      let timeout = stateSleepOnEnterExitSleepInSeconds / 2.0
//      // M1 will exit/reenter stateSleepOnExit 
//      Assert.Throws<TimeoutException>( fun() -> sm.SendMessage (M1, TimeSpan.FromSeconds timeout) |> ignore ) |> ignore


//   [<Fact>]
//   let SendMessageAsync_Should_Throw_If_Timeout_Expires() = 
//      let sm = startStateMachine initData stateSleepOnExit
//      let timeout = stateSleepOnEnterExitSleepInSeconds / 2.0
//      // M1 will exit/reenter stateSleepOnExit
//      let ex = Assert.Throws<AggregateException>( fun() -> 
//         sm.SendMessageAsync(M1, TimeSpan.FromSeconds timeout).Result |> ignore)
//      Assert.IsAssignableFrom<TimeoutException>( ex.GetBaseException() )

