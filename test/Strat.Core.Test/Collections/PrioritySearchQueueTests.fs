namespace Strat.Collections.Test

open System
open System.Collections.Generic
open Strat.Collections
open Xunit


module PrioritySearchQueue =
   let numEntries = 1000
   let pairs = Array.init numEntries id |> Array.mapi (fun idx item -> (numEntries - 1 - idx).ToString(), item)
   let pairsInKeyOrder = pairs |> Array.sortBy fst
   let pairsInValueOrder = pairs |> Array.sortBy snd
   let pairMap = pairs |> Map.ofArray


   module Empty = 
      [<Fact>]
      let should_have_size_0() = 
         Assert.Equal(0, PrioritySearchQueue.empty.Count)

      [<Fact>]
      let should_be_empty() = 
         Assert.True PrioritySearchQueue.empty.IsEmpty

      [<Fact>]
      let should_throw_from_indexer() = 
         Assert.Throws<KeyNotFoundException>(fun () -> PrioritySearchQueue.empty.[0]) |> ignore

      [<Fact>]
      let should_yield_no_values_from_enumerator() =
         let list = List.ofSeq PrioritySearchQueue.empty
         Assert.True list.IsEmpty


   module Equals = 
      [<Fact>]
      let should_return_true_for_same_instances() = 
         let q = PrioritySearchQueue.singleton "Key" 1
         Assert.True (q.Equals(q))

      [<Fact>]
      let should_return_true_for_equivalent_queues() = 
          let q1 = PrioritySearchQueue.ofArray pairs
          let q2 = PrioritySearchQueue.ofArray pairs
          Assert.True (q1.Equals(q2))

      [<Fact>]
      let should_return_false_for_queues_with_different_elements() = 
          let q1 = PrioritySearchQueue.ofArray pairs
          let q2 = PrioritySearchQueue.ofArray (pairs |> Array.take (pairs.Length / 2))
          Assert.False (q1.Equals(q2))

      [<Fact>]
      let should_return_false_for_null() = 
         let q = PrioritySearchQueue.singleton "Key" 1
         Assert.False (q.Equals(null))

      [<Fact>]
      let should_return_false_for_other_types() = 
          let q = PrioritySearchQueue.ofArray pairs
          Assert.False (q.Equals(pairs))


   module GetHashCode = 
      [<Fact>]
      let should_compute_same_hashcode_for_same_instance() =
         let q = PrioritySearchQueue.ofArray pairs
         Assert.Equal ( q.GetHashCode(),  q.GetHashCode())

      [<Fact>]
      let should_compute_same_hashcode_for_equivalent_queues() =
         let q1 = PrioritySearchQueue.ofArray pairs
         let q2 = PrioritySearchQueue.ofArray pairs
         Assert.Equal ( q1.GetHashCode(),  q2.GetHashCode())

      [<Fact>]
      let should_return_different_hashcode_for_queues_with_different_elements() = 
          let q1 = PrioritySearchQueue.ofArray pairs
          let q2 = PrioritySearchQueue.ofArray (pairs |> Array.take (pairs.Length / 2))
          Assert.NotEqual ( q1.GetHashCode(),  q2.GetHashCode())


   module Singleton = 
      [<Fact>]
      let should_return_queue_with_single_binding() = 
         let q = PrioritySearchQueue.singleton "Key" 1
         Assert.Equal (1, q.Count)
         Assert.Equal (1, q.["Key"])


   module Count = 
      [<Fact>]
      let should_return_count() = 
         let q = PrioritySearchQueue.ofArray pairs
         Assert.Equal(pairs.Length, q.Count)


   module IsEmpty = 
      [<Fact>]
      let should_return_false_if_not_empty() = 
         let q = PrioritySearchQueue.ofArray pairs
         Assert.False q.IsEmpty


   module Indexer = 
      [<Fact>]
      let should_return_value_for_key() = 
         let q = PrioritySearchQueue.ofArray pairs
         pairs |> Array.iteri (fun idx (k,v) -> Assert.Equal (v, q.[k]))


      [<Fact>]
      let should_throw_if_key_not_found() =
         let q = PrioritySearchQueue.ofSeq [(1, '1'); (2,'2')]
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            q.[0] |> ignore)) |> ignore
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            q.[3] |> ignore)) |> ignore


   module Head = 
      [<Fact>]
      let should_return_binding_at_with_min_value() =
          let q = PrioritySearchQueue.ofArray pairs
          let struct (minK, minV) = q |> PrioritySearchQueue.head
          Assert.Equal (fst pairsInValueOrder.[0], minK)
          Assert.Equal (snd pairsInValueOrder.[0], minV)

      [<Fact>]
      let should_throw_for_empty_queue() = 
         Assert.Throws<InvalidOperationException>( fun() ->
            PrioritySearchQueue.empty |> PrioritySearchQueue.head |> ignore )


   module TryHead = 
      [<Fact>]
      let should_return_binding_at_with_min_value() =
          let q = PrioritySearchQueue.ofArray pairs
          let struct (minK, minV) = q |> PrioritySearchQueue.tryHead |> Option.get
          Assert.Equal (fst pairsInValueOrder.[0], minK)
          Assert.Equal (snd pairsInValueOrder.[0], minV)

      [<Fact>]
      let should_return_none_if_queue_is_empty() = 
         Assert.True (PrioritySearchQueue.empty |> PrioritySearchQueue.tryHead |> Option.isNone)


   module Find = 
      [<Fact>]
      let should_return_value_for_key() = 
         let q = PrioritySearchQueue.ofArray pairs
         pairs |> Array.iteri (fun idx (k,v) -> Assert.Equal (v, q |> PrioritySearchQueue.find k))

      [<Fact>]
      let should_throw_if_key_not_found() =
         let q = PrioritySearchQueue.ofSeq [(1, '1'); (2,'2')]
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            q |> PrioritySearchQueue.find 0 |> ignore)) |> ignore
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            q |> PrioritySearchQueue.find 3 |> ignore)) |> ignore


    module TryFind = 
      [<Fact>]
      let should_return_value_for_key() = 
         let q = PrioritySearchQueue.ofArray pairs
         pairs |> Array.iteri (fun idx (k,v) -> Assert.Equal (v, q |> PrioritySearchQueue.tryFind k |> Option.get))

      [<Fact>]
      let should_return_none_if_key_not_found() =
         let q = PrioritySearchQueue.ofSeq [(1, '1'); (2,'2')]
         Assert.True (q |> PrioritySearchQueue.tryFind 0 |> Option.isNone)
         Assert.True (q |> PrioritySearchQueue.tryFind 3 |> Option.isNone)


   module ContainsKey = 
      [<Fact>]
      let should_return_true_if_key_in_queue() = 
         let q = PrioritySearchQueue.ofArray pairs
         pairs |> Array.iteri (fun idx (k,v) -> Assert.True (q |> PrioritySearchQueue.containsKey k))

      [<Fact>]
      let should_return_fakse_if_key_not_in_queue() =
         let q = PrioritySearchQueue.ofSeq [(1, '1'); (2,'2')]
         Assert.False (q |> PrioritySearchQueue.containsKey 0)
         Assert.False (q |> PrioritySearchQueue.containsKey 3)


   module Add = 
      [<Fact>]
      let should_add_binding_to_queue() = 
         let mutable q = PrioritySearchQueue.empty
         pairs |> Array.iter (fun (k,v) -> q <- q.Add (k, v))
         Assert.Equal (pairs.Length, q.Count)
         pairsInValueOrder 
         |> Array.iter (fun (k,v) -> Assert.Equal( v, q.[k]))


   module RemoveHead = 
      [<Fact>]
      let should_return_binding_with_min_value_and_rest_of_queue() =
         let q = 
            [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
            |> PrioritySearchQueue.ofOrderedSeq
         let struct (k, v, rest) = PrioritySearchQueue.removeHead q
         Assert.Equal<string>( "C", k )
         Assert.Equal( 1, v )
         Assert.Equal( 4, rest.Count )
         rest 
         |> PrioritySearchQueue.toSeq
         |> Seq.zip [("A", 3); ("B", 5); ("D", 2); ("E", 2)] 
         |> Seq.iter Assert.Equal

      [<Fact>]
      let should_throw_for_empty_queue() = 
         Assert.Throws<InvalidOperationException>( fun() ->
            PrioritySearchQueue.empty |> PrioritySearchQueue.removeHead |> ignore )


   module TryRemoveHead = 
      [<Fact>]
      let should_return_binding_with_min_value_and_rest_of_queue() =
         let q = 
            [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
            |> PrioritySearchQueue.ofOrderedSeq
         
         let optResult = q |> PrioritySearchQueue.tryRemoveHead
         Assert.True optResult.IsSome
         let struct (k, v, rest) = optResult.Value
         Assert.Equal<string>( "C", k )
         Assert.Equal( 1, v )
         Assert.Equal( 4, rest.Count )
         rest 
         |> PrioritySearchQueue.toSeq
         |> Seq.zip [("A", 3); ("B", 5); ("D", 2); ("E", 2)] 
         |> Seq.iter Assert.Equal

      [<Fact>]
      let should_return_none_for_empty_queue() = 
         Assert.True (PrioritySearchQueue.empty |> PrioritySearchQueue.tryRemoveHead |> Option.isNone )


   module Remove = 
      [<Fact>]
      let should_remove_binding_from_queue() = 
         let mutable q = PrioritySearchQueue.ofArray pairs
         pairsInKeyOrder |> Array.iter (fun (k,v) ->
            q <- q |> PrioritySearchQueue.remove k
            Assert.False (q |> PrioritySearchQueue.containsKey k))
         Assert.True q.IsEmpty

      [<Fact>]
      let should_return_same_queue_if_key_not_found() = 
         let q = PrioritySearchQueue.ofArray pairs
         let nextQ = q |> PrioritySearchQueue.remove "XYZ"
         Assert.Same (q, nextQ)


   module AtMost =
      [<Fact>]
      let should_return_list_With_values_LTEQ_value() =
         let items = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
         let q = PrioritySearchQueue.ofOrderedSeq items
         let entries = q |> PrioritySearchQueue.atMost 3
         Assert.Equal( 4, entries.Length )
         entries
         |> List.zip [("A", 3); ("C", 1); ("D", 2); ("E", 2)] 
         |> List.iter (fun ((expKey, expValue), (key, value)) ->
            Assert.Equal<string>(expKey, key)
            Assert.Equal(expValue, value) )

      [<Fact>]
      let should_return_empty_list_if_value_Is_LT_head() =
         let items = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
         let q = PrioritySearchQueue.ofOrderedSeq items
         let entries = q |> PrioritySearchQueue.atMost 0
         Assert.Equal( 0, entries.Length )


   module Iter = 
      [<Fact>]
      let should_apply_f_to_each_item() = 
         let q = PrioritySearchQueue.ofArray pairs
         let mutable i = 0
         q |> PrioritySearchQueue.iter (fun k v -> 
            Assert.True (pairMap.[k] = v)
            i <- i + 1)
         Assert.Equal (i, q.Count)
        

   module Map = 
      [<Fact>]
      let should_apply_f_to_each_item_in_ascending_key_order() = 
         let q = PrioritySearchQueue.ofArray pairs
         let mutable i = 0
         let mappedQ = q |> PrioritySearchQueue.map (fun k v -> 
            let expk, expv = pairsInKeyOrder.[i]
            Assert.Equal (expk, k)
            Assert.Equal (expv, v)
            i <- i + 1
            v.ToString())
         Assert.Equal (q.Count, mappedQ.Count)
         pairsInKeyOrder |> Array.iter (fun (expK, expV) -> 
            Assert.Equal (expV.ToString(), mappedQ.[expK]))


   module Filter = 
      [<Fact>]
      let should_apply_predicate_and_only_include_items_when_true() = 
         let q = PrioritySearchQueue.ofArray pairs
         let pred k v = 
            Assert.Equal (pairMap.[k], v)
            v % 2 = 0
         let filteredQ = q |> PrioritySearchQueue.filter pred
         Assert.Equal( pairs.Length / 2, filteredQ.Count)
         for i = 0 to pairs.Length - 1 do
            let k, v = pairs.[i]
            if v % 2 = 0 then Assert.Equal ( v, filteredQ.[k]) 
            else Assert.False (filteredQ.ContainsKey k)


   module Fold = 
      [<Fact>]
      let should_apply_fold_to_each_item_and_return_final_state() = 
         let q = PrioritySearchQueue.ofArray pairs
         let mutable nextI = 0
         let foldSum total k v = 
            Assert.True (pairMap.[k] = v)
            nextI <- nextI + 1 
            total + v
         let total = q |> PrioritySearchQueue.fold foldSum 0
         let expTotal = pairs |> Seq.map snd |> Seq.sum
         Assert.Equal (expTotal, total)

      [<Fact>]
      let should_return_initial_state_if_queue_is_empty() =
         let foldSum total k v = total + v
         let total = PrioritySearchQueue.empty |> PrioritySearchQueue.fold foldSum -1
         Assert.Equal (-1, total)


   module Exists = 
      [<Fact>]
      let should_return_true_if_predicate_matches_binding() =
         let items = [|("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)|] 
         let itemMap = Map.ofArray items
         let q = PrioritySearchQueue.ofOrderedSeq items
         let mutable predCount = 0
         let pred k v = 
            Assert.True (itemMap.[k] = v)
            predCount <- predCount + 1
            v = 1
         let doesExist = q |> PrioritySearchQueue.exists pred
         Assert.True doesExist

      [<Fact>]
      let should_return_false_if_predicate_matches_none() =
         let items = [|("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)|] 
         let q = PrioritySearchQueue.ofOrderedSeq items
         let mutable predCount = 0
         let pred k v = 
            predCount <- predCount + 1
            v = 10
         let doesExist = q |> PrioritySearchQueue.exists pred
         Assert.False doesExist
         Assert.Equal (5, predCount)


   module ForAll = 
      [<Fact>]
      let should_return_true_when_all_items_match() =
         let items = [|("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)|] 
         let itemMap = Map.ofArray items
         let q = PrioritySearchQueue.ofOrderedSeq items
         let mutable predCount = 0
         let pred k v = 
            Assert.True (itemMap.[k] = v)
            predCount <- predCount + 1
            v > 0
         let allTrue = q |> PrioritySearchQueue.forAll pred
         Assert.True allTrue
         Assert.Equal (q.Count, predCount)

      [<Fact>]
      let should_return_false_when_all_items_do_not_match() =
         let items = [|("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)|] 
         let itemMap = Map.ofArray items
         let q = PrioritySearchQueue.ofOrderedSeq items
         let pred k v = 
            Assert.True (itemMap.[k] = v)
            if k = "C" then v = -100 else true
         let allTrue = q |> PrioritySearchQueue.forAll pred
         Assert.False allTrue


   module TryPick = 
      [<Fact>]
      let should_return_picked_item() =
         let items = [|("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)|] 
         let itemMap = Map.ofArray items
         let q = PrioritySearchQueue.ofOrderedSeq items
         let mutable pickCount = 0
         let mutable nextI = 0
         let picker k v = 
            Assert.True (itemMap.[k] = v)
            nextI <- nextI + 1  
            pickCount <- pickCount + 1
            if k = "D" then Some 'd' else None
         let picked = q |> PrioritySearchQueue.tryPick picker
         Assert.True picked.IsSome
         Assert.Equal ('d', picked.Value)

      [<Fact>]
      let should_return_none_if_no_item_is_picked() =
         let items = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
         let q = PrioritySearchQueue.ofOrderedSeq items
         let mutable pickCount = 0
         let picker k v = 
            pickCount <- pickCount + 1
            None
         let picked : option<string> = q |> PrioritySearchQueue.tryPick picker
         Assert.True picked.IsNone
         Assert.Equal (q.Count, pickCount)


   module Pick = 
      [<Fact>]
      let should_return_picked_item() =
         let items = [|("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)|] 
         let itemMap = Map.ofArray items
         let q = PrioritySearchQueue.ofOrderedSeq items
         let mutable pickCount = 0
         let mutable nextI = 0
         let picker k v = 
            Assert.True (itemMap.[k] = v)
            nextI <- nextI + 1  
            pickCount <- pickCount + 1
            if k = "D" then Some 'd' else None
         let picked = q |> PrioritySearchQueue.pick picker
         Assert.Equal ('d', picked)

      [<Fact>]
      let should_throw_if_no_item_is_picked() =
         let items = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
         let q = PrioritySearchQueue.ofOrderedSeq items
         let picker k v = None
         Assert.Throws<KeyNotFoundException>(Action(fun () ->
            q |> PrioritySearchQueue.pick picker |> ignore)) |> ignore


   module ToSeq = 
      [<Fact>]
      let should_return_enumerable_that_iterates_in_ascending_key_order() = 
         let mutable q = PrioritySearchQueue.ofArray pairs
         let mutable i = 0
         let e = (q |> PrioritySearchQueue.toSeq).GetEnumerator()
         while e.MoveNext() do
            let k, v = pairsInKeyOrder.[i]
            Assert.Equal (k, fst e.Current)
            Assert.Equal (v, snd e.Current)
            i <- i + 1
         Assert.Equal (q.Count, i)


   module ToList = 
      [<Fact>]
      let should_return_list_containing_bindings_in_ascending_key_order() = 
         let q = PrioritySearchQueue.ofArray pairs
         let l = q |> PrioritySearchQueue.toList
         l |> List.iteri (fun i (k,v) ->
            let expK, expV = pairsInKeyOrder.[i]
            Assert.Equal (expK, k)
            Assert.Equal (expV, v))
         Assert.Equal (q.Count, l.Length)


   module ToArray = 
      [<Fact>]
      let should_return_array_containing_bindings_in_ascending_key_order() = 
         let q = PrioritySearchQueue.ofArray pairs
         let arr = q |> PrioritySearchQueue.toArray
         arr |> Array.iteri (fun i struct(k,v) ->
            let expK, expV = pairsInKeyOrder.[i]
            Assert.Equal (expK, k)
            Assert.Equal (expV, v))
         Assert.Equal (q.Count, arr.Length)





