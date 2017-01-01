namespace Strat.StateMachine

open Strat.StateMachine.Definition
open Strat.StateMachine.Definition
open Strat.StateMachine.Definition.Sync
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
    let homeState = StateName "home"
    let chooseScenarioState = StateName "chooseScenario"
    let gameInProgressState = StateName "gameInProgress" 


    module Transitions =
        let onNewGameGoToNewScenario (ctx: MessageContext<_,_>) = 
            let scenarios = List.empty
            MessageResult.Transition 
                ( chooseScenarioState, 
                  ctx.Data, 
                  (Some(fun  _ -> async.Return (ChooseMission scenarios))) )


    let homeMessageHandler (ctx: MessageContext<_,_>) =
        match ctx.Message with
        | NewGame -> ctx |> Transitions.onNewGameGoToNewScenario 
        | _ -> ctx.Unhandled()
        |> async.Return

    let builder = StateBuilder<Data, Message>()
    let tree = 
        builder
            .DefineState(
                name = homeState,
                onMessage = homeMessageHandler)
            .DefineState(
                name = chooseScenarioState,
                onMessage = Async.unhandledMessage,
                onEnter = (fun ctx -> async.Return ctx.TargetData))
            .DefineState(
                name = gameInProgressState,
                onMessage = Async.unhandledMessage)
            .ToStateTree()
