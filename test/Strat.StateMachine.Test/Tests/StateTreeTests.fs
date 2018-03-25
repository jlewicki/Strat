namespace Strat.StateMachine.Test

open System.Collections.Generic
open Strat.StateMachine
open Strat.StateMachine.Definition
open Xunit

module StateTree =

   module NewTree = 
      [<Fact>]
      let should_create_a_new_tree() = 
         let handler : StateHandler<string, string> =
            { OnMessage = Handlers.emptyMessageHandler
              OnEnter = Handlers.emptyTransitionHandler
              OnExit = Handlers.emptyTransitionHandler } 
         let initTransition = Start.With "b"

         let tree = StateTree.Build.newTree ("a", handler, initTransition)
         
         let root = tree |> StateTree.rootState
         Assert.Equal("a", root.Id)
         Assert.Same(handler, root.Handler)
         Assert.Same(initTransition, root |> State.initialTransition |> Option.get)


   module ParentStateById =
      [<Fact>]
      let should_return_parent_state() =
         let tree = ExampleHsm.tree
         let parentStateId stateId = tree |> StateTree.parentStateById stateId |> Option.get |> State.id
         Assert.Equal(ExampleHsm.s21, parentStateId ExampleHsm.s211)
         Assert.Equal(ExampleHsm.s2, parentStateId ExampleHsm.s21)
         
      [<Fact>]
      let shoud_return_none_for_root() = 
         let tree = ExampleHsm.tree
         Assert.True(tree |> StateTree.parentStateById ExampleHsm.s0 |> Option.isNone)

      [<Fact>]
      let shoud_return_none_for_unknown_state() = 
         let tree = ExampleHsm.tree
         Assert.True(tree |> StateTree.parentStateById "foo" |> Option.isNone)


   module AncestorStatesById =
      [<Fact>]
      let should_return_ancestor_states() =
         let tree = ExampleHsm.tree
         
         let ancestors = tree |> StateTree.ancestorStatesById ExampleHsm.s211

         let ancestorIds = ancestors |> List.map State.id
         Assert.Equal<string>([ExampleHsm.s21; ExampleHsm.s2; ExampleHsm.s0], ancestorIds)

      [<Fact>]
      let should_return_empty_list_for_root() =
          let tree = ExampleHsm.tree
          let ancestors = tree |> StateTree.ancestorStatesById ExampleHsm.s0
          Assert.True (ancestors.IsEmpty)

      [<Fact>]
      let should_return_empty_list_for_unknown_state() =
          let tree = ExampleHsm.tree
          let ancestors = tree |> StateTree.ancestorStatesById "foo"
          Assert.True (ancestors.IsEmpty)


   module SelfAncestorStatesById =
      [<Fact>]
      let should_return_ancestor_states() =
         let tree = ExampleHsm.tree
         
         let ancestors = tree |> StateTree.selfAndAncestorStatesById ExampleHsm.s211

         let ancestorIds = ancestors |> List.map State.id
         Assert.Equal<string>([ExampleHsm.s211; ExampleHsm.s21; ExampleHsm.s2; ExampleHsm.s0], ancestorIds)

      [<Fact>]
      let should_return_list_containing_root_for_root() =
          let tree = ExampleHsm.tree
          let ancestors = tree |> StateTree.selfAndAncestorStatesById ExampleHsm.s0
          Assert.Equal (1, ancestors.Length)
          Assert.Equal (ExampleHsm.s0, ancestors.Head.Id)

      [<Fact>]
      let should_return_empty_list_for_unknown_state() =
          let tree = ExampleHsm.tree
          let ancestors = tree |> StateTree.selfAndAncestorStatesById "foo"
          Assert.True (ancestors.IsEmpty)


   module IsSelfOrAncestor =
      [<Fact>]
      let should_return_true_for_ancestor() =
         let tree = ExampleHsm.tree
         let isSelfOrAncestor = tree |> StateTree.isSelfOrAncestor ExampleHsm.s211 ExampleHsm.s2
         Assert.True isSelfOrAncestor

      [<Fact>]
      let should_return_true_for_self() =
         let tree = ExampleHsm.tree
         let isSelfOrAncestor = tree |> StateTree.isSelfOrAncestor ExampleHsm.s211 ExampleHsm.s211
         Assert.True isSelfOrAncestor

      [<Fact>]
      let should_return_false_if_not_ancestor() =
         let tree = ExampleHsm.tree
         let isSelfOrAncestor = tree |> StateTree.isSelfOrAncestor ExampleHsm.s211 ExampleHsm.s1
         Assert.False isSelfOrAncestor


   module FindState =
      
      [<Fact>]
      let should_return_state() =
         let tree = ExampleHsm.tree
         let state = tree |> StateTree.findState ExampleHsm.s211
         Assert.Equal (ExampleHsm.s211, state.Id)

      [<Fact>]
      let should_throw_if_state_does_not_exist() =
         let tree = ExampleHsm.tree
         Assert.Throws<KeyNotFoundException>( fun() -> 
            tree |> StateTree.findState "foo" |> ignore 
         ) |> ignore


   module TryFindState =
      
      [<Fact>]
      let should_return_state() =
         let tree = ExampleHsm.tree
         let optState = tree |> StateTree.tryFindState ExampleHsm.s211
         Assert.True optState.IsSome
         Assert.Equal (ExampleHsm.s211, optState.Value.Id)

      [<Fact>]
      let should_return_None_if_state_does_not_exist() =
         let tree = ExampleHsm.tree
         let optState = tree |> StateTree.tryFindState "foo"
         Assert.True optState.IsNone