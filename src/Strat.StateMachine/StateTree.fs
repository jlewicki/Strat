namespace Strat.StateMachine


type StateTree<'D,'M> = {
   Root: State<'D, 'M>
   StatesByStateId: Map<StateId, State<'D,'M>>
   // TODO: cache ancestor paths for all states?
}


module StateTree = 

   let rootState stateTree = 
      stateTree.Root
   
   let parentState (state: State<'D, 'M>) (stateTree: StateTree<'D,'M>) =
      match state with
      | Root _ -> None
      | Terminal (parentId, _, _) -> stateTree.StatesByStateId.TryFind parentId
      | Interior (_, parentId, _, _) -> stateTree.StatesByStateId.TryFind parentId
      | Leaf (_, parentId, _) ->  stateTree.StatesByStateId.TryFind parentId

   let parentStateById stateId stateTree = 
      match stateTree.StatesByStateId.TryFind stateId with
      | Some state -> parentState state stateTree
      | _ -> None

   let ancestorStatesById stateId stateTree = 
      let rec ancestorStatesAcc stateId stateTree states = 
         match parentStateById stateId stateTree with
         | Some parent -> ancestorStatesAcc parent.Id stateTree (parent::states)
         | _ -> states |> List.rev
      ancestorStatesAcc stateId stateTree List.empty

   let selfAndAncestorStatesById stateId stateTree  = 
      match stateTree.StatesByStateId.TryFind stateId with
      | Some state -> state::(ancestorStatesById stateId stateTree)
      | _ -> List.empty

   let isSelfOrAncestor stateId targetStateId stateTree =
      selfAndAncestorStatesById stateId stateTree
      |> List.exists (fun s -> s.Id = targetStateId)

   let findState stateId stateTree =
      stateTree.StatesByStateId.[stateId]

   let tryFindState stateId stateTree =
      stateTree.StatesByStateId.TryFind stateId


   module Build =

      let newTree (rootId, rootHandlers, initialTransition) =
         let root = Root (rootId, rootHandlers, initialTransition)
         { Root = root
           StatesByStateId = Map.empty |> Map.add rootId root }

      let ensureUniqueId id tree = 
         if tree |> tryFindState id |> Option.isSome then 
            invalidArg "id" (sprintf "A state with id %A has already been defined" id)

      let ensureValidParent parentId tree =
         match tree.StatesByStateId.TryFind parentId with
         | Some (Root _) -> () // Root is a legit parent state
         | Some (Interior _) -> () // Interior state is a legit parent state
         | Some (Leaf _) -> invalidArg "state" "Parent of the state cannnot be a leaf state"
         | Some (Terminal _) -> invalidArg "state" "Parent of the state cannnot be a terminal state"
         | None -> invalidArg "state" (sprintf "Parent state with id %A is not in the state tree" parentId)

      let addState state (tree: StateTree<'D,'M>) : StateTree<'D,'M> =
         ensureUniqueId (state |> State.id) tree
         match state with
         | Root _ -> invalidArg "state" "Tree already has a root state"
         | Terminal _ -> invalidArg "state" "Cannot add a terminal state"
         | Interior (_, parentId, _, _)-> ensureValidParent parentId tree
         | Leaf (_, parentId, _) -> ensureValidParent parentId tree
         { tree with 
            StatesByStateId = tree.StatesByStateId |> Map.add state.Id state }


   module Wrap =

      let handler stateId (wrap: StateHandler<_,_> -> StateHandler<_,_>) stateTree : StateTree<_,_> = 
         let mapHandlers f state = 
            match state with
            | Root (name, handlers, initTransition) -> Root (name, f handlers, initTransition)
            | Interior (name, parent, handlers, initTransition) -> Interior (name, parent, f handlers, initTransition)
            | Leaf (name, parent, handlers) -> Leaf (name, parent, f handlers)
            | Terminal _ as t -> t
         let state = stateTree |> findState stateId
         let newState = state |> mapHandlers wrap
         { stateTree with 
            StatesByStateId = stateTree.StatesByStateId |> Map.add state.Id newState }

      let onEnter stateId (wrap: TransitionHandler<'D,'M> -> TransitionHandler<'D,'M>) stateTree : StateTree<'D,'M> =
         stateTree |> handler stateId (fun h -> { h with OnEnter = wrap h.OnEnter })

      let onExit stateId (wrap: TransitionHandler<'D,'M> -> TransitionHandler<'D,'M>) stateTree : StateTree<'D,'M> =
         stateTree |> handler stateId (fun h -> { h with OnExit = wrap h.OnExit })