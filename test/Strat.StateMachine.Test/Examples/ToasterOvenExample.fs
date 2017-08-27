namespace Strat.StateMachine

open Xunit
open Strat.StateMachine.Definition.Define
open Strat.StateMachine.Definition.Sync

module ToasterOvenExample =
  
   type Data = 
      { IsLampOn : bool
        BakingTemp: int
        ToastColor: string }

   type Message = 
      | Toast
      | Bake
      | OpenDoor
      | CloseDoor
      | SetBakeTemp of Temp: int
      | SetToastColor of Color: string

   // Shortcut alias
   type MessageContext = MessageContext<Data, Message>
   type TransitionContext = TransitionContext<Data, Message>

   // Names of the states in the state tree
   let heatingState = StateName "heating"
   let toastingState = StateName "toasting"
   let bakingState = StateName "baking"
   let doorOpenState = StateName "doorOpen"

   // Interfaces representing elements controlled by the state machine
   type IHeatingElement = 
      abstract On: unit -> unit
      abstract Off: unit -> unit
      abstract SetTemp: temp:int -> unit
      abstract SetTimer: color:string -> unit
      abstract ClearTimer: unit -> unit

   type IOvenLight = 
      abstract On: unit -> unit
      abstract Off: unit -> unit

   // Handler functions
   let rootHandler (msgCtx:MessageContext) = 
      match msgCtx.Message with
      | SetBakeTemp(temp) -> msgCtx.Stay { msgCtx.Data with BakingTemp = temp } 
      | SetToastColor(color) -> msgCtx.Stay { msgCtx.Data with ToastColor = color }
      | _ -> Unhandled

   let heatingHandler (msgCtx:MessageContext) = 
      match msgCtx.Message with
      | OpenDoor -> msgCtx.GoTo doorOpenState
      | Toast -> msgCtx.GoTo toastingState
      | Bake -> msgCtx.GoTo bakingState
      | _ -> Unhandled 

   let heatingEnter (heater: IHeatingElement) (transCtx:TransitionContext) = 
      heater.On()
      transCtx.TargetData
   
   let heatingExit (heater: IHeatingElement) (transCtx:TransitionContext) = 
      heater.Off()
      transCtx.TargetData

   let heatingHandlers (heater: IHeatingElement) = 
      Handle.With (heatingHandler, heatingEnter heater, heatingExit heater)

   let doorOpenHandler (msgCtx:MessageContext) = 
      match msgCtx.Message with
      | CloseDoor -> msgCtx.GoTo heatingState
      | _ -> Unhandled 

   let doorOpenEnter (light: IOvenLight) (transCtx:TransitionContext) = 
      light.On()
      transCtx.TargetData
   
   let doorOpenExit (light: IOvenLight) (transCtx:TransitionContext) = 
      light.Off()
      transCtx.TargetData

   let doorOpenHandlers light = 
      Handle.With (doorOpenHandler, doorOpenEnter light, doorOpenExit light)

   let bakingEnter (heater: IHeatingElement) (transCtx:TransitionContext) = 
      heater.SetTemp transCtx.TargetData.BakingTemp
      transCtx.TargetData
   
   let bakingExit (heater: IHeatingElement) (transCtx:TransitionContext) = 
      heater.SetTemp 0
      transCtx.TargetData

   let bakingHandlers (heater: IHeatingElement) = 
         Handle.With (unhandledMessage, bakingEnter heater, bakingExit heater)

   let toastingEnter (heater: IHeatingElement) (transCtx:TransitionContext) = 
      heater.SetTimer transCtx.TargetData.ToastColor
      transCtx.TargetData
   
   let toastingExit (heater: IHeatingElement) (transCtx:TransitionContext) = 
      heater.ClearTimer()
      transCtx.TargetData

   let toastingHandlers (heater: IHeatingElement) = 
         Handle.With (unhandledMessage, toastingEnter heater, toastingExit heater)

   // Definition of the state tree.
   let newStateTree heater light = 
      let heatingState = StateName "heating"
      let toastingState = StateName "toasting"
      let bakingState = StateName "baking"
      let doorOpenState = StateName "doorOpen"


      syncRoot (StateName "root") (Start.With heatingState) (Handle.With rootHandler)
         [ syncInterior heatingState (Start.With toastingState) (heatingHandlers heater)
            [ syncLeaf toastingState (toastingHandlers heater)
              syncLeaf bakingState (bakingHandlers heater) ] 
           syncLeaf doorOpenState (doorOpenHandlers light) ]


   // Stub interface implementations
   type OvenLight = {
      mutable IsOn: bool
   } with
      interface IOvenLight with
         member this.On() = this.IsOn <- true
         member this.Off() = this.IsOn <- false
   
   type HeatingElement = {
       mutable IsOn: bool
       mutable Temp: int
       mutable ToastColor: string
   } with
      interface IHeatingElement with
         member this.On() = this.IsOn <- true
         member this.Off() = this.IsOn <- false
         member this.SetTemp(temp) = this.Temp <- temp
         member this.SetTimer(color) = this.ToastColor <- color
         member this.ClearTimer() = this.ToastColor <- ""
   
   // Create a new state tree, wired up with some stub services
   let newStateTreeWithStubs() =
      let light : OvenLight = { IsOn = false }
      let oven: HeatingElement = { IsOn = false; Temp = 0; ToastColor = "" }
      (newStateTree oven light), oven, light

   let initData = { IsLampOn = false; ToastColor = "Light"; BakingTemp = 300 }


   [<Fact>]
   let CloseDoor_When_DoorOpen_Should_Transition_To_Heating() = 
      let stateTree, heater, light = newStateTreeWithStubs()
      use oven = StateMachineAgent.startNewAgentIn doorOpenState stateTree initData 

      let ctx = oven.SendMessage CloseDoor

      Assert.True( ctx.State |> State.isInState heatingState )
      Assert.True( heater.IsOn )


   [<Fact>]
   let Bake_When_Heating_Should_Transition_To_Baking() = 
      let stateTree, heater, light = newStateTreeWithStubs()
      use oven = StateMachineAgent.startNewAgentIn heatingState stateTree initData 
      
      let ctx = oven.SendMessage Bake
      
      Assert.True( ctx.State |> State.isInState bakingState )
      Assert.Equal(heater.Temp, ctx.Data.BakingTemp)


   [<Fact>]
   let OpenDoor_When_Heating_Should_Transition_To_DoorOpen() = 
      let stateTree, heater, light = newStateTreeWithStubs()
      use oven = StateMachineAgent.startNewAgentIn heatingState stateTree initData 
      
      let ctx = oven.SendMessage OpenDoor
      
      Assert.True( ctx.State |> State.isInState doorOpenState )
      Assert.True( light.IsOn )
      