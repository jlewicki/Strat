namespace Strat.StateMachine

open System


/// Defines functions for working with State<_,_> instances.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
  
   [<GeneralizableValue>]
   let emptyMessageHandler = MessageHandler.Sync (fun _ -> MessageResult.Unhandled)
   [<GeneralizableValue>]
   let emptyTransitionHandler = TransitionHandler.Sync (fun transCtx -> transCtx.TargetData)
   [<GeneralizableValue>]
   let emptyHandler = 
      { OnMessage = emptyMessageHandler
        OnEnter = emptyTransitionHandler
        OnExit = emptyTransitionHandler } 


   // Returns the name of the state
   let inline name (state:State<_,_>) = 
      state.Name


   let inline ref (state:State<_,_>) = 
      state.Id


   /// Returns a value indicating if the specified state is a root state.
   let isRoot state =  
      match state with 
      | Root _ -> true 
      | _ -> false


   /// Returns a value indicating if the specified state is a terminal state.
   let isTerminal state = 
      match state with
      | Terminal _ -> true 
      | _ -> false


   /// Returns the parent state of the state, if available.
   let parent state = 
      match state with
      | Root _ -> None
      | Interior (_, parent, _, _) -> Some(parent)
      | Leaf (_, parent, _) -> Some(parent)
      | Terminal (parent, _, _) -> Some(parent)


   /// Returns the handler functions for the state.
   let handlers state = 
      match state with
      | Root (_, handlers, _) -> handlers
      | Interior (_, _, handlers, _) -> handlers
      | Leaf (_, _, handlers) -> handlers
      | Terminal _ -> emptyHandler


   let mapHandlers f state = 
      match state with
      | Root (name, handlers, initTransition) -> Root (name, f handlers, initTransition)
      | Interior (name, parent, handlers, initTransition) -> Interior (name, parent, f handlers, initTransition)
      | Leaf (name, parent, handlers) -> Leaf (name, parent, f handlers)
      | Terminal _ as t -> t


   /// Returns the initial transition for the state, if available.
   let initialTransition state =
      match state with
      | Root (_,_,initTransition) -> Some(initTransition)
      | Interior (_, _, _, initTransition) -> Some(initTransition)
      | _ -> None


   /// Returns the root state (that is, the farthest ancestor) for the specified state
   let rec root state = 
      match state with 
      | Interior (_, parent, _, _) ->  root parent
      | Leaf (_, parent, _ ) ->  root parent
      | Terminal (parent, _, _) -> root parent
      | Root _ -> state


   /// Returns a list containing all the ancestor states (in upwards order) of the state.
   let ancestors state = 
      List.unfold (function
         | Interior (_, parent, _, _) -> Some (parent, parent)
         | Leaf (_, parent, _ ) -> Some (parent, parent)
         | Terminal (parent, _, _) -> Some (parent, parent)
         | Root _ -> None ) state


   /// Returns a list containing the state and all of its ancestor ancestor states, in upwards order.
   let selfAndAncestors state = 
      state::(ancestors state)


   /// Returns a value indicating if the state is in the state with the specified name.  That is, if state, or any of its 
   /// ancestor states, has the specified name. 
   let isInState stateRef state = 
      state  
      |> selfAndAncestors  
      |> List.exists (fun s -> s.Id = stateRef)    


   /// Returns the state that is the least common ancestor between the two states (assumes states are different)
   let internal leastCommonAncestor (state1:State<'D,'M>) (state2:State<'D,'M>) = 
      let anc1, anc2 = (ancestors state1 |> List.rev), (ancestors state2 |> List.rev)
      List.zip anc1 anc2
      |> List.takeWhile (Object.ReferenceEquals)
      |> List.last
      |> fst


   /// Throws an exception if parent is not the parent state of child. 
   let internal ensureChild parentState childState = 
      match childState |> parent with 
      | Some parentOfChild ->
         if parentOfChild <> parentState then
            invalidOp <| sprintf "State %s is not a child state of %s" (name childState) (name parentState)
      | None -> 
         invalidOp <| sprintf "State %s is missing a parent state" (name childState)

