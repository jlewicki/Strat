namespace Strat.StateMachine

module StateTree =

   let internal findLazyState stateRef (stateTree: StateTree<'D,'M>) =
      match stateTree.States |> Map.tryFind stateRef with
      | Some lazyState -> lazyState
      | None -> 
         let name  = match stateRef with StateId n -> n 
         invalidOp <| sprintf "Unable to find a state with name %s in the state tree." name


   let internal findState stateRef (stateTree: StateTree<'D,'M>) =
      let lazyState = stateTree |> findLazyState stateRef
      lazyState.Value


   
  