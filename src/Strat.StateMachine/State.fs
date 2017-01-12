namespace Strat.StateMachine

/// Defines functions for working with State<_,_> instances.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
   open System


   /// A StateHandler with no-op handler functions
   let private emptyHandler : StateHandler<_,_> = 
      { OnMessage =  fun _ -> async.Return MessageResult.Unhandled; 
        OnEnter = fun transCtx -> async.Return transCtx.TargetData 
        OnExit = fun transCtx -> async.Return transCtx.TargetData }


   /// Returns the name of the state
   let inline name (state:State<_,_>) = 
      state.Name


   /// Returns the textual name of the state
   let nameText (state:State<_,_>) = 
      match state.Name with | StateName name -> name


   /// Returns a value indicating if the specified state is a root state.
   let isRoot state =  
      match state with 
      | Root(_) -> true 
      | _ -> false


   /// Returns a value indicating if the specified state is a terminal state.
   let isTerminal state = 
      match state with
      | Terminal(_) -> true 
      | _ -> false


   /// Returns the parent state of the state, if available.
   let parent state = 
      match state with
      | Root(_) -> None
      | Intermediate(_,parent,_,_) -> Some(parent)
      | Leaf(_,parent,_) -> Some(parent)
      | Terminal(parent, _, _) -> Some(parent)


   /// Returns the handler functions for the state.
   let handlers state = 
      match state with
      | Root(_,handlers,_) -> handlers
      | Intermediate(_,_,handlers,_) -> handlers
      | Leaf(_,_,handlers) -> handlers
      | Terminal(_) -> emptyHandler


   /// Returns the initial transition for the state, if available.
   let initialTransition state =
      match state with
      | Root(_,_,initTransition) -> Some(initTransition)
      | Intermediate(_,_,_,initTransition) -> Some(initTransition)
      | _ -> None


   /// Returns a list containing all the ancestor states (in upwards order) of the state.
   let ancestors state = 
      List.unfold (function
         | Intermediate(_,parent,_,_) -> Some(parent, parent) 
         | Leaf(_, parent, _ ) -> Some(parent, parent)
         | Terminal(parent, _, _) -> Some(parent, parent)
         | Root(_,_,_) -> None ) state


   /// Returns a list containing the state and all of its ancestor ancestor states, in upwards order.
   let selfAndAncestors state = 
      state::(ancestors state)


    /// Determines if the states are equal (defaults to reference equality)
   let equals (state1: State<'D,'M>) (state2: State<'D,'M>) =
      // State<_,_> can't support structural equality, because it stores functions, so use object equality.
      Object.Equals(state1, state2)


   /// Returns a value indicating if the state is in the state with the specified name.  That is, if state, or any of its 
   /// ancestor states, has the specified name. 
   let isInState stateName state = 
      state  
      |> selfAndAncestors  
      |> Seq.exists (fun s -> s.Name = stateName)    



   /// Returns the state that is the least common ancestor between the two states (assumes states are different)
   let internal leastCommonAncestor (state1:State<'D,'M>) (state2:State<'D,'M>) = 
      let anc1, anc2 = (ancestors state1 |> List.rev), (ancestors state2 |> List.rev)
      Seq.zip anc1 anc2
      |> Seq.takeWhile (fun (state1, state2) -> equals state1 state2 )
      |> Seq.last
      |> fst

