namespace Strat.StateMachine.Definition

open System.Collections.Generic
open Strat.StateMachine


/// Defines functions and values for defining states with synchronous handler functions.
module Sync = 

   // Describes the function that is called when a state handles a message.
   type MessageHandler<'D,'M> = 
      MessageContext<'D,'M> -> MessageResult<'D,'M>


   /// Describes functions that are called when a transition occures between two states. 
   type TransitionHandler<'D,'M> = 
      TransitionContext<'D,'M> -> 'D 


   /// Describes a set of synchronous functions that define the behavior of a state.
   type StateHandler<'D,'M> = {
      OnMessage: MessageHandler<'D,'M>
      OnEnter: TransitionHandler<'D,'M>
      OnExit: TransitionHandler<'D,'M>
   }

   let unhandledMessage : MessageHandler<'D,'M> = fun _ -> MessageResult.Unhandled
   let emptyTransitionHandler : TransitionHandler<'D,'M> = fun transCtx -> transCtx.TargetData

   /// A SyncStateHandler with no-op handler functions
   let emptyHandler : StateHandler<_,_> = 
      { OnMessage = unhandledMessage; OnEnter = emptyTransitionHandler; OnExit = emptyTransitionHandler }

   /// Constructs a StateHandler from the specified individual handler functions.
   let internal toAsyncHandler
      ( onMessage: option<MessageHandler<'D,'M>> ) 
      ( onEnter: option<TransitionHandler<'D,'M>> ) 
      ( onExit: option<TransitionHandler<'D,'M>> ) :  Strat.StateMachine.StateHandler<'D,'M> = 

      { OnMessage = (defaultArg onMessage unhandledMessage) >> async.Return; 
        OnEnter = (defaultArg onEnter emptyTransitionHandler) >> async.Return; 
        OnExit = (defaultArg onExit emptyTransitionHandler) >> async.Return } 

   /// Adapts a sync state handler to an async one.
   let internal convertToAsync (handler:StateHandler<'D,'M>) : Strat.StateMachine.StateHandler<'D,'M> = 
      { OnMessage = handler.OnMessage >> async.Return
        OnEnter = handler.OnEnter >> async.Return
        OnExit = handler.OnExit >> async.Return } 

   /// Defines functions for creating state handlers.
   type Handle private () = 
         
      /// <summary>
      /// Creates a synchronous state handler that uses the specified functions to define the behavior of the state.
      /// </summary>
      /// <param name="onMessage">Message handling function for the state.</param>
      /// <param name="onEnter">Optional entry function for the state.</param>
      /// <param name="onExit">Optional exit function for the state.</param>
      static member With
         ( onMessage: MessageHandler<'D,'M>, 
           ?onEnter:TransitionHandler<'D,'M>, 
           ?onExit: TransitionHandler<'D,'M> ) : StateHandler<'D,'M> =

         let _onEnter = defaultArg onEnter emptyTransitionHandler
         let _onExit = defaultArg onExit emptyTransitionHandler
         { OnMessage = onMessage; OnEnter = _onEnter; OnExit = _onExit}


      /// <summary>
      /// Creates a synchronous state handler that delegates message handling to its parent state.
      /// </summary>
      /// <param name="onEnter">Optional entry function for the state.</param>
      /// <param name="onExit">Optional exit function for the state.</param>
      static member With(?onEnter: TransitionHandler<'D,'M>, ?onExit: TransitionHandler<'D,'M> ) =
         Handle.With (unhandledMessage, ?onEnter = onEnter, ?onExit = onExit )


/// Defines functions and values for defining states with asynchronous handler functions.
module Async = 
   let unhandledMessage : MessageHandler<'D,'M> = fun _ -> async.Return MessageResult.Unhandled
   let emptyTransitionHandler : TransitionHandler<'D,'M> = fun transCtx -> async.Return transCtx.TargetData
   let emptyTransitionAction : TransitionHandler<'D,'M> = fun transCtx -> async.Return transCtx.TargetData
      
   /// A StateHandler with no-op handler functions
   let emptyHandler : StateHandler<_,_> = 
      { OnMessage = unhandledMessage; 
        OnEnter = emptyTransitionHandler; 
        OnExit = emptyTransitionHandler }

   /// Constructs a StateHandler from the specified individual handler functions.
   let internal toHandler
      ( onMessage: option<MessageHandler<'D,'M>> ) 
      ( onEnter: option<TransitionHandler<'D,'M>> ) 
      ( onExit: option<TransitionHandler<'D,'M>> ) : StateHandler<'D,'M> = 

      { OnMessage = defaultArg onMessage unhandledMessage; 
        OnEnter = defaultArg onEnter emptyTransitionHandler; 
        OnExit = defaultArg onExit emptyTransitionHandler }

   /// Defines functions for creating state handlers.
   type Handle private () = 
  
      /// <summary>
      /// Creates an asynchronous state handler that uses the specified functions to define the behavior of the
      /// state.
      /// </summary>
      /// <param name="onMessage">Message handling function for the state.</param>
      /// <param name="onEnter">Optional entry function for the state.</param>
      /// <param name="onExit">Optional exit function for the state.</param>
      static member With
         ( onMessage: MessageHandler<'D,'M>, 
           ?onEnter: TransitionHandler<'D,'M>, 
           ?onExit: TransitionHandler<'D,'M> ) : StateHandler<'D,'M> =
            
         let _onEnter = defaultArg onEnter emptyTransitionHandler
         let _onExit = defaultArg onExit emptyTransitionHandler
         { StateHandler.OnMessage = onMessage; OnEnter = _onEnter; OnExit = _onExit}


      /// <summary>
      /// Creates an asynchronous state handler that delegates message handling to its parent state.
      /// </summary>
      /// <param name="onEnter">Optional entry function for the state.</param>
      /// <param name="onExit">Optional exit function for the state.</param>
      static member With( ?onEnter: TransitionHandler<'D,'M>, ?onExit: TransitionHandler<'D,'M> ) =
         Handle.With (unhandledMessage, ?onEnter = onEnter, ?onExit = onExit )


/// Defines functions for defining states and state trees in a functional expression-based style.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Define =

   /// Describes a function that, given a parent state and state tree, will create a new child state, and an updated 
   /// state tree.
   type StateDef<'D,'M> = 
      Lazy<State<'D,'M>> * StateTree<'D,'M> -> Lazy<State<'D,'M>> * StateTree<'D,'M>

   /// Throws if the specified name is already in the map.
   let private ensureUniqueName name map = 
      if map |> Map.containsKey name then 
         invalidArg "name" (sprintf "A state with name %A has already been defined" name)


    /// Defines functions for defining the initial transition funcion for a state.
   type Start private () = 
         
      /// Returns an InitialTransition that enters the specified child state when an intermediate state is entered.
      static member With( stateName: StateName ) : InitialTransition<'D> = 
         fun ctx -> async.Return (ctx, stateName)

   /// Defines a leaf state in a state tree that processes messages using the specified asynchronous handler 
   /// functions. 
   let asyncLeaf (name: StateName) (handler:StateHandler<'D,'M>) : StateDef<'D,'M> = 
      if isNull (name :> obj) then nullArg "name"
      fun (parent,tree) -> 
         let lazyState = lazy (Leaf(name, parent.Value, handler))
         ensureUniqueName name tree.States
         lazyState, { tree with States = tree.States |> Map.add name lazyState }

   /// Defines a leaf state in a state tree that processes messages using the specified synchronous handler 
   /// functions. 
   let syncLeaf (name: StateName) (handler:Sync.StateHandler<'D,'M>) : StateDef<'D,'M> = 
      if isNull (name :> obj) then nullArg "name"
      asyncLeaf name (Sync.convertToAsync handler)

   /// Defines an intermediate state in a state tree that processes messages using the specified asynchronous handler 
   /// handler functions. 
   let asyncInterior  
      (name: StateName) 
      (initialTransition: InitialTransition<'D>) 
      (handler: StateHandler<'D,'M>)
      (childStates: seq<StateDef<'D,'M>>) : StateDef<'D,'M> = 

      if isNull (name :> obj) then nullArg "name"
      fun (parent,tree) -> 
         let lazyState = lazy (State.Intermediate (name, parent.Value, handler, initialTransition))
         let _, tree = 
            childStates 
            |> Seq.fold (fun (children,tree) stateDef -> 
               let lazyChild, tree = stateDef (lazyState, tree)
               lazyChild::children, tree) (List.empty, tree)
         ensureUniqueName name tree.States
         lazyState, { tree with States = tree.States |> Map.add name lazyState }


   /// Defines an intermediate state in a state tree that processes messages using the specified synchronous handler 
   /// handler functions. 
   let syncInterior 
      (name: StateName) 
      (initialTransition: InitialTransition<'D>) 
      (handler: Sync.StateHandler<'D,'M>) 
      (childStates: seq<StateDef<'D,'M>>) : StateDef<'D,'M> =

      if isNull (name :> obj) then nullArg "name"
      asyncInterior name initialTransition (Sync.convertToAsync handler) childStates


   /// Constructs a new state tree with rooted at a state with the specified name, initial transition, asynchronous 
   /// handlers, and child states. 
   let asyncRoot 
      (rootStateName: StateName) 
      (initialTransition: InitialTransition<'D>) 
      (handler: StateHandler<'D,'M>)
      (childStates: seq<StateDef<'D,'M>>) : StateTree<'D,'M> =

      if isNull (rootStateName :> obj) then nullArg "rootStateName"
      let root = Root (rootStateName, handler, initialTransition)
      let lazyRoot = lazy root
      let _, tree = 
         childStates 
         |> Seq.fold (fun (children,map) stateDef -> 
            let lazyChild, tree = stateDef (lazyRoot, map)
            lazyChild::children, tree
          ) (List.empty, { Root = root; States = Map.empty })
      ensureUniqueName rootStateName tree.States
      { tree with States = tree.States |> Map.add rootStateName lazyRoot }


   /// Constructs a new state tree with rooted at a state with the specified name, initial transition, handlers, and
   /// child states.
   let syncRoot 
      (rootStateName: StateName) 
      (initialTransition: InitialTransition<'D>) 
      (handler: Sync.StateHandler<'D,'M>) 
      (childStates: seq<StateDef<'D,'M>>) : StateTree<'D,'M> = 
    
      if isNull (rootStateName :> obj) then nullArg "rootStateName"
      asyncRoot rootStateName initialTransition (Sync.convertToAsync handler) childStates 


   /// Constructs a new state tree with a default 'no-op' root state, serving as a parent for the specified set of
   /// child states.
   let flatStates (initialStateName: StateName) (states: seq<StateDef<'D,'M>>) : StateTree<'D,'M> =
      let rootName = StateName "StateList-RootState"
      asyncRoot rootName (Start.With initialStateName) Async.emptyHandler states


module internal Build =
   // Function that creates a child state, given it's parent state
   type internal StateCreator<'D,'M> = Lazy<State<'D,'M>> -> Lazy<State<'D,'M>>
   type internal ChildBuilder<'D,'M> = StateName * StateCreator<'D,'M>


open Build


/// Base class for builders that is used to define a 'flat' set of states for a state machine, without introducing 
/// hierarchical relationships between the states.
[<AbstractClass>]
type StateBuilderBase<'D,'M>() = 
   let stateNames = HashSet<StateName>()
   let stateBuilders = ResizeArray<StateName*StateCreator<'D,'M>>()

   member internal this.AddState name creator = 
      if not (stateNames.Add name) then 
         raise <| invalidArg "name" (sprintf "A state with name %A has already been defined" name)
      stateBuilders.Add (name, creator) 

   /// Returns a new state tree containing all the states that have been defined with this builder. The tree is flat,
   /// consisting of a default root state, with all the states defined by this builder as children.
   member this.ToStateTree() : StateTree<'D,'M> =
      // Just chose the first state as the initial state
      let initTransition = fun ctx -> async.Return (ctx, (stateBuilders |> Seq.head |> fst))
      let rootState = State.Root (StateName "RootState", Async.emptyHandler, initTransition)
      let initStateTree = { Root = rootState; States = Map.empty }
      let lazyRoot = lazy rootState

      stateBuilders
      |> Seq.fold (fun stateTree (name, builder) -> 
         let newState = builder lazyRoot
         let newStates = stateTree.States |> Map.add name newState 
         { stateTree with States = newStates }
      ) initStateTree



/// Base class for builders that define a hierarchical set of states for a state machine.
[<AbstractClass>]
type StateTreeBuilderBase<'D, 'M>() = 
   let mutable rootState = Option.None
   // Map of parent states names, keyed by childstate name 
   // Map of child states names, keyed by parent state name 
   let childMap = Dictionary<StateName, ResizeArray<ChildBuilder<'D,'M>>>()

   member internal this.AddChildState parentName childBuilder = 
      match childMap.TryGetValue parentName with
      | true, children -> 
         children.Add childBuilder
      | false, _ -> 
         childMap.Add (parentName, ResizeArray<ChildBuilder<'D,'M>>(Seq.singleton childBuilder)) 

   member internal this.SetRootState state = 
      if rootState.IsSome then raise <| invalidOp "Root state has already been defined"
      if not (state |> State.isRoot) then raise <| invalidArg "state" "State must be a root state"
      rootState <- Some (state)

   /// Returns a new state tree containing all the states that have been defined with this builder.
   member this.ToStateTree() : StateTree<'D,'M> =
      if rootState.IsNone then raise <| invalidOp "Missing root state"

      let rec buildChildStates parentStateName stateTree = 
         match childMap.TryGetValue parentStateName with
         | true, childBuilders -> 
            childBuilders 
            |> Seq.fold (fun stateTree (childName, builder) -> 
               let lazyParentState = stateTree.States |> Map.find parentStateName 
               let lazyChildState = builder lazyParentState
               let newStates = stateTree.States |> Map.add childName lazyChildState 
               let stateTree = { stateTree with States = newStates }
               buildChildStates childName stateTree ) stateTree 
         | false, _ -> 
            stateTree
      
      let initStateTree = { Root = rootState.Value; States = Map.empty }         
      buildChildStates rootState.Value.Name initStateTree



/// <summary>
/// A builder that is used to define a 'flat' set of states for a state machine, without introducing hierarchical 
/// relationships between the states.
/// </summary>
type StateBuilder<'D,'M>() = 
   inherit StateBuilderBase<'D,'M>()

   /// Defines a state with the specified async handler functions.
   member this.DefineState
      ( name: StateName,
        ?onMessage: MessageHandler<'D,'M>, 
        ?onEnter: TransitionHandler<'D,'M>, 
        ?onExit: TransitionHandler<'D,'M> ) = 

      let build (rootState: Lazy<State<_,_>>) = 
         lazy (Leaf (name, rootState.Value, (Async.toHandler onMessage onEnter onExit)))
      this.AddState name build
      this

   /// Defines a state with the specified synchronous handler functions.
   member this.DefineState
      ( name: StateName,
        ?onMessage: Sync.MessageHandler<'D,'M>, 
        ?onEnter: Sync.TransitionHandler<'D,'M>, 
        ?onExit: Sync.TransitionHandler<'D,'M> ) = 

      let build (rootState: Lazy<State<_,_>>) = 
         let handler = Sync.toAsyncHandler onMessage onEnter onExit
         lazy (Leaf (name, rootState.Value, handler))

      this.AddState name build
      this


/// <summary>
/// A builder that can be used to define a hierarchical set of states for a state machine.
/// </summary>
/// <remarks>
///  This type is intended primarily for use with F#.
/// <remarks>
type StateTreeBuilder<'D, 'M>() =
   inherit StateTreeBuilderBase<'D,'M>()
 
   /// Defines a root state with the specified asynchronous handler functions.
   member this.DefineRootState
      ( name: StateName,
        initialTransition: InitialTransition<'D>,
        ?onMessage: MessageHandler<'D,'M>, 
        ?onEnter: TransitionHandler<'D,'M>, 
        ?onExit: TransitionHandler<'D,'M> ) = 
      
      this.SetRootState (Root(name, (Async.toHandler onMessage onEnter onExit), initialTransition))
      this

   /// Defines a root state with the specified synchronous handler functions.
   member this.DefineRootState
      ( name: StateName,
        initialTransition: InitialTransition<'D>,
        ?onMessage: Sync.MessageHandler<'D,'M>, 
        ?onEnter: Sync.TransitionHandler<'D,'M>, 
        ?onExit: Sync.TransitionHandler<'D,'M> ) = 
      
      this.SetRootState (Root(name, (Sync.toAsyncHandler onMessage onEnter onExit), initialTransition))
      this

   /// Defines an interior (non-root, non-leaf) state with the specified asynchronous handler functions.
   member this.DefineInteriorState
      ( name: StateName,
        parent: StateName,
        initialTransition: InitialTransition<'D>,
        ?onMessage: MessageHandler<'D,'M>, 
        ?onEnter: TransitionHandler<'D,'M>, 
        ?onExit: TransitionHandler<'D,'M> ) = 

      let build (lazyParent: Lazy<State<_,_>>) = 
         lazy (Intermediate (name, lazyParent.Value, (Async.toHandler onMessage onEnter onExit), initialTransition))
      this.AddChildState parent (name, build)
      this

   /// Defines an interior (non-root, non-leaf) state with the specified synchronous handler functions.
   member this.DefineInteriorState
      ( name: StateName,
        parent: StateName,
        initialTransition: InitialTransition<'D>,
        ?onMessage: Sync.MessageHandler<'D,'M>, 
        ?onEnter: Sync.TransitionHandler<'D,'M>, 
        ?onExit: Sync.TransitionHandler<'D,'M> ) = 

      let build (lazyParent: Lazy<State<_,_>>) = 
         lazy (Intermediate (name, lazyParent.Value, (Sync.toAsyncHandler onMessage onEnter onExit), initialTransition))
      this.AddChildState parent (name, build)
      this

   /// Defines an leaf state with the specified asynchronous handler functions.
   member this.DefineLeafState
      ( name: StateName,
        parent: StateName,
        ?onMessage: MessageHandler<'D,'M>, 
        ?onEnter: TransitionHandler<'D,'M>, 
        ?onExit: TransitionHandler<'D,'M> ) = 

      let build (lazyParent: Lazy<State<_,_>>) = 
         lazy (Leaf (name, lazyParent.Value, (Async.toHandler onMessage onEnter onExit)))
      this.AddChildState parent (name, build)
      this

   /// Defines an leaf state with the specified synchronous handler functions.
   member this.DefineLeafState
      ( name: StateName,
        parent: StateName,
        ?onMessage: Sync.MessageHandler<'D,'M>, 
        ?onEnter: Sync.TransitionHandler<'D,'M>, 
        ?onExit: Sync.TransitionHandler<'D,'M> ) = 

      let build (lazyParent: Lazy<State<_,_>>) = 
        lazy (Leaf (name, lazyParent.Value, (Sync.toAsyncHandler onMessage onEnter onExit)))
      this.AddChildState parent (name, build)
      this
