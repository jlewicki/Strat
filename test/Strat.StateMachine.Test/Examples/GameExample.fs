namespace Strat.StateMachine

open Strat.StateMachine.Definition
open Xunit

module Game = 
   
   type Data = 
      | HomeSreen 
      | ChooseMission of AvailableMissions: list<string>
      | GameInProgress of Mission: string * TurnNumber: int

   type Message = 
      | NewGame 
      | StartGame

    // Shortcut alias
   type MessageContext = MessageContext<Data, Message>   

   // Names of the states in the state tree
   let homeState = "home"
   let chooseScenarioState = "chooseScenario"
   let gameInProgressState = "gameInProgress" 


   module Transitions =
      let onNewGameGoToNewScenario (ctx: MessageContext<_,_>) = 
         let scenarios = List.empty
         MessageResult.Transition 
            ( chooseScenarioState, 
              ctx.Data, 
              (Some (TransitionHandler.Sync(fun  _ -> ChooseMission scenarios))))


   let homeMessageHandler = MessageHandler.Sync( fun ctx ->
      match ctx.Message with
      | NewGame -> ctx |> Transitions.onNewGameGoToNewScenario 
      | _ -> ctx.Unhandled())


   open StateTree
   let tree = 
      StateTree.fromLeaves homeState 
         [ leaf homeState (Handle.With(homeMessageHandler))
           leaf chooseScenarioState (Handle.With (Handlers.emptyMessageHandler))
           leaf gameInProgressState (Handle.With (Handlers.emptyMessageHandler)) ]