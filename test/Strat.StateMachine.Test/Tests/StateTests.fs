namespace Strat.StateMachine.Test

open System
open Strat.StateMachine
open Strat.StateMachine.Definition
open Xunit


module State = 

   module Id =
      [<Fact>]
      let should_return_id() =
         Assert.Equal ("a", State.Root ("a", Handlers.emptyHandler, Start.With "b") |> State.id)
         Assert.Equal ("a", State.Interior ("a", "b", Handlers.emptyHandler, Start.With "c") |> State.id)
         Assert.Equal ("a", State.Leaf ("a", "b", Handlers.emptyHandler) |> State.id)
         Assert.Equal (State<string, string>.TerminalStateId, State.Terminal ("a", "b", None) |> State.id)
   

   module Handler =
      [<Fact>]
      let should_return_handler() =
         let handler : StateHandler<string, string> =
            { OnMessage = Handlers.emptyMessageHandler
              OnEnter = Handlers.emptyTransitionHandler
              OnExit = Handlers.emptyTransitionHandler } 
         Assert.Same (handler, State<string,string>.Root ("a", handler, Start.With "b") |> State.handler)
         Assert.Same (handler, State<string,string>.Interior ("a", "b", handler, Start.With "c") |> State.handler)
         Assert.Same (handler, State<string,string>.Leaf ("a", "b", handler) |> State.handler)
         Assert.NotNull (State<string,string>.Terminal ("a", "b", None) |> State.handler)
