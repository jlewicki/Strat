namespace Strat.StateMachine.Definition

open System
open Strat.StateMachine


[<Sealed>]
type Start private () = 
   static member With( stateName: StateId ) : InitialTransition<'D> = 
      InitialTransition.Sync(fun ctx -> struct (ctx, stateName))


[<Sealed>]
type Handle private() = 
   static member With
      ( ?onMessage: MessageHandler<'D,'M>,
        ?onEnter: TransitionHandler<'D,'M>,
        ?onExit: TransitionHandler<'D,'M> ) : StateHandler<'D,'M> =
      { OnEnter = defaultArg onEnter Handlers.emptyTransitionHandler
        OnMessage = defaultArg onMessage Handlers.emptyMessageHandler
        OnExit = defaultArg onExit Handlers.emptyTransitionHandler }


module StateTree =
   // Describes the type of a child state
   type ChildStateType = InteriorChild = 1 | LeafChild = 2

   // Describes a function that, given a parent state and state tree, will create a new child state, and an updated 
   /// state tree.
   type CreateChildState<'D,'M> = 
      | CreateChild of 
         ChildType:ChildStateType * 
         Creator: (State<'D,'M> * StateTree<'D,'M> -> State<'D,'M> * StateTree<'D,'M>)

   let fromRoot 
      (stateId: StateId) 
      (handler: StateHandler<'D,'M>)
      (initialTransition: InitialTransition<'D>) 
      (childStates: seq<CreateChildState<'D,'M>>) : StateTree<'D,'M> =

      if String.IsNullOrWhiteSpace stateId then nullArg "id"
      let tree = StateTree.Build.newTree (stateId, handler, initialTransition)
      let root = tree |> StateTree.rootState
      let _, tree = 
         childStates 
         |> Seq.fold (fun (children,tree) (CreateChild(_, createChild)) -> 
            let lazyChild, tree = createChild (root, tree)
            lazyChild::children, tree
          ) (List.empty, tree)
      tree

   let leaf (id: StateId) (handler:StateHandler<'D,'M>) : CreateChildState<'D,'M> = 
      if String.IsNullOrWhiteSpace id then 
         nullArg "id"
      CreateChild (ChildStateType.LeafChild, fun (parent,tree) -> 
         let state = Leaf (id, parent.Id, handler)
         let tree = tree |> StateTree.Build.addState state
         state, tree)
      
   let interior 
      (id: StateId) 
      (handler: StateHandler<'D,'M>) 
      (initialTransition: InitialTransition<'D>) 
      (childStates: seq<CreateChildState<'D,'M>>) : CreateChildState<'D,'M> = 
      if String.IsNullOrWhiteSpace id then 
         nullArg "id"
      CreateChild (ChildStateType.LeafChild, fun (parent,tree) -> 
         let state = Interior (id, parent.Id, handler, initialTransition)
         let tree = tree |> StateTree.Build.addState state
         let _, tree = 
            childStates 
            |> Seq.fold (fun (children,tree) (CreateChild(parentState, createChild)) -> 
               let child, tree = createChild (state, tree)
               child::children, tree
            ) (List.empty, tree)
         state, tree)


   let fromLeaves (initialStateName: StateId) (states: seq<CreateChildState<'D,'M>>) : StateTree<'D,'M> =
      let allLeaves = states |> Seq.forall (fun (CreateChild(t, _)) -> t = ChildStateType.LeafChild)
      if not allLeaves then
         raise <| new ArgumentException("Only Leaf child states may be included", "states")
      let rootName = "StateList-RootState"
      fromRoot rootName Handlers.emptyHandler (Start.With initialStateName) states


   
