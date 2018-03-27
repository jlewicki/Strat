namespace Strat.StateMachine.Test.Examples

open Xunit
open Strat.StateMachine
open Strat.StateMachine.Definition


module ToasterOvenExample =
  
   open StateTree

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
   type StateMachineContext = StateMachineContext<Data, Message>

   // Names of the states in the state tree
   let heatingState = "heating"
   let toastingState = "toasting"
   let bakingState = "baking"
   let doorOpenState = "doorOpen"

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
   let rootHandler = MessageHandler.Sync (fun msgCtx -> 
      match msgCtx.Message with
      | SetBakeTemp(temp) -> msgCtx.Stay { msgCtx.Data with BakingTemp = temp } 
      | SetToastColor(color) -> msgCtx.Stay { msgCtx.Data with ToastColor = color }
      | _ -> Unhandled)

   let heatingHandler = MessageHandler.Sync (fun msgCtx -> 
      match msgCtx.Message with
      | OpenDoor -> msgCtx.GoTo doorOpenState
      | Toast -> msgCtx.GoTo toastingState
      | Bake -> msgCtx.GoTo bakingState
      | _ -> Unhandled)

   let heatingEnter (heater: IHeatingElement) =
      TransitionHandler.Sync (fun transCtx ->
         heater.On()
         transCtx.TargetData)
   
   let heatingExit (heater: IHeatingElement) =
      TransitionHandler.Sync (fun transCtx ->
         heater.Off()
         transCtx.TargetData)

   let heatingHandlers (heater: IHeatingElement) = 
      Handle.With (heatingHandler, heatingEnter heater, heatingExit heater)

   let doorOpenHandler = MessageHandler.Sync (fun msgCtx -> 
      match msgCtx.Message with
      | CloseDoor -> msgCtx.GoTo heatingState
      | _ -> Unhandled)

   let doorOpenEnter (light: IOvenLight) =
      TransitionHandler.Sync (fun transCtx ->
         light.On()
         transCtx.TargetData)
   
   let doorOpenExit (light: IOvenLight) =
      TransitionHandler.Sync (fun transCtx ->
         light.Off()
         transCtx.TargetData)

   let doorOpenHandlers light = 
      Handle.With (doorOpenHandler, doorOpenEnter light, doorOpenExit light)

   let bakingEnter (heater: IHeatingElement) =
      TransitionHandler.Sync (fun transCtx ->
         heater.SetTemp transCtx.TargetData.BakingTemp
         transCtx.TargetData)
   
   let bakingExit (heater: IHeatingElement) =
      TransitionHandler.Sync (fun transCtx ->
         heater.SetTemp 0
         transCtx.TargetData)

   let bakingHandlers (heater: IHeatingElement) = 
         Handle.With (onEnter=bakingEnter heater, onExit=bakingExit heater)

   let toastingEnter (heater: IHeatingElement) =
      TransitionHandler.Sync (fun transCtx ->
         heater.SetTimer transCtx.TargetData.ToastColor
         transCtx.TargetData)
   
   let toastingExit (heater: IHeatingElement) =
      TransitionHandler.Sync (fun transCtx ->
         heater.ClearTimer()
         transCtx.TargetData)

   let toastingHandlers (heater: IHeatingElement) = 
         Handle.With (onEnter=toastingEnter heater, onExit=toastingExit heater)

   // Definition of the state tree.
   
   let newStateTree heater light = 
      StateTree.fromRoot "root" (Handle.With rootHandler) (Start.With heatingState)
         [ interior heatingState (heatingHandlers heater) (Start.With toastingState)
            [ leaf toastingState (toastingHandlers heater)
              leaf bakingState (bakingHandlers heater) ] 
           leaf doorOpenState (doorOpenHandlers light) ]


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

   let initContext state =
      let tree, heater, light = newStateTreeWithStubs()
      (Machine.initializeContext tree initData (Some state) |> Async.RunSynchronously), heater, light

   [<Fact>]
   let CloseDoor_when_DoorOpen_should_transition_to_Heating() = 
      let smCtx, heater, _ = initContext doorOpenState
      match Machine.processMessage CloseDoor smCtx |> Async.RunSynchronously with
      | HandledMessage handled ->
         Assert.True (smCtx.StateTree |> StateTree.isSelfOrAncestor handled.NextState.Id heatingState )
         Assert.True( heater.IsOn )
      | _ -> invalidOp "Message should have been handled"


   [<Fact>]
   let Bake_When_Heating_Should_Transition_To_Baking() = 
      let smCtx, heater, _ = initContext heatingState
      match Machine.processMessage Bake smCtx |> Async.RunSynchronously with
      | HandledMessage handled ->
         Assert.True (smCtx.StateTree |> StateTree.isSelfOrAncestor handled.NextState.Id bakingState )
         Assert.Equal(heater.Temp, handled.NextData.BakingTemp)
      | _ -> invalidOp "Message should have been handled"


   [<Fact>]
   let OpenDoor_when_Heating_should_transition_to_DoorOpen() = 
      let smCtx, _, light = initContext heatingState
      match Machine.processMessage OpenDoor smCtx |> Async.RunSynchronously with
      | HandledMessage handled ->
         Assert.True (smCtx.StateTree |> StateTree.isSelfOrAncestor handled.NextState.Id doorOpenState )
         Assert.True light.IsOn
      | _ -> invalidOp "Message should have been handled"