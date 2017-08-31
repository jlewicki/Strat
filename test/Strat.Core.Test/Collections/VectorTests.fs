namespace Strat.Collections.Test

open System
open System.Collections.Generic
open Strat.Collections
open Xunit


module Vector =

   let largeArray = Array.unfold (fun i -> if i < 10000 then Some(i + 1, i + 1) else None) 0

   module Empty = 
      [<Fact>]
      let should_have_size_0() = 
         Assert.Equal(0, Vector.empty.Count)

      [<Fact>]
      let should_be_empty() = 
         Assert.True Vector.empty.IsEmpty

      [<Fact>]
      let should_throw_from_indexer() = 
         Assert.Throws<IndexOutOfRangeException>(Action(fun () -> Vector.empty.[0])) |> ignore

      [<Fact>]
      let should_yield_no_values_from_enumerator() =
         let list = List.ofSeq Vector.empty
         Assert.True list.IsEmpty

 
   module Count = 
      [<Fact>]
      let should_return_count() = 
         let pv = Vector.ofArray largeArray
         Assert.Equal(largeArray.Length, pv.Count)


   module IsEmpty = 
      [<Fact>]
      let should_return_false_if_not_empty() = 
         let pv = Vector.ofSeq ["one"; "two"]
         Assert.False pv.IsEmpty


    module Indexer = 
      [<Fact>]
      let should_return_value_at_index() = 
         let pv = Vector.ofArray largeArray
         largeArray |> Array.iteri (fun idx item -> Assert.Equal (item, pv.[idx]))

      [<Fact>]
      let should_throw_index_out_of_range() =
         let pv = Vector.ofSeq ['a'; 'b']
         Assert.Throws<IndexOutOfRangeException>(Action(fun () -> 
            pv.[-1] |> ignore)) |> ignore
         Assert.Throws<IndexOutOfRangeException>(Action(fun () -> 
            pv.[2] |> ignore)) |> ignore


   module Set = 
      [<Fact>]
      let should_set_item_at_index() = 
         let mutable pv = Vector.ofArray largeArray
         // Reverse items in pv
         for i in [largeArray.Length - 1 .. -1 .. 0] do
            let item = largeArray.[i]
            let pvi = largeArray.Length - i - 1
            pv <- pv.Set(pvi, item)

         largeArray
         |> Array.iteri (fun idx item -> 
            let newIdx = largeArray.Length - idx - 1
            Assert.Equal(item, pv.[newIdx])) 

      [<Fact>]
      let should_leave_original_vector_unchanged() = 
         let v = Vector.ofArray largeArray
         let idx = v.Count / 2
         let orig = v.[idx]
         let newV = v.Set (idx, orig *2)
         Assert.Equal(orig, v.[idx])


   module Add = 
      [<Fact>]
      let should_add_item_at_end_of_vector() = 
         let largeArray = Array.unfold (fun i -> if i < 35 then Some(i + 1, i + 1) else None) 0
         let mutable pv = Vector.empty
         largeArray
         |> Array.iteri (fun idx item -> pv <- pv.Add item)

         Assert.Equal (largeArray.Length, pv.Count)
         largeArray
         |> Array.iteri (fun idx item -> 
            Assert.Equal(item, pv.[idx])) 


   module RemoveLast = 
      [<Fact>]
      let should_remove_last_element() = 
         let mutable pv = Vector.ofArray largeArray
         for i in [largeArray.Length - 1 .. -1 .. 0] do
            let item, newPv = pv.RemoveLast()
            Assert.Equal(largeArray.[i], item)
            pv <- newPv
         Assert.True pv.IsEmpty

      [<Fact>]
      let should_throw_if_vector_is_empty() = 
         Assert.Throws<InvalidOperationException>(Action(fun () -> 
            Vector.empty.RemoveLast() |> ignore)) |> ignore


   module GetEnumerator = 
      [<Fact>]
      let should_return_enumerator_that_yields_items_in_vector() =
         let pv = Vector.ofArray largeArray
         use e = (pv :> IEnumerable<int>).GetEnumerator()
         let mutable currentIndex = 0
         while e.MoveNext() do
            Assert.Equal (largeArray.[currentIndex], e.Current)
            currentIndex <- currentIndex + 1 
         Assert.Equal (pv.Count, currentIndex)

      [<Fact>]
      let should_return_enumerator_that_can_be_reset() = 
         let v = Vector.ofArray largeArray
         use e = (v :> IEnumerable<int>).GetEnumerator()     
         for i in [1..(v.Count / 2)] do
            e.MoveNext() |> ignore
        
         e.Reset()
         
         let mutable currentIndex = 0
         while e.MoveNext() do
            Assert.Equal (largeArray.[currentIndex], e.Current)
            currentIndex <- currentIndex + 1 


   module Map = 
      [<Fact>]
      let should_apply_mapping_to_each_item_in_vector() = 
         let v = Vector.ofArray largeArray
         let f (i: int) = i * 2
         let mappedV = Vector.map f v
         Assert.Equal (v.Count, mappedV.Count)
         for i in [0 .. v.Count - 1] do
            Assert.Equal (largeArray.[i] * 2, mappedV.[i])


   module Filter = 
      [<Fact>]
      let should_apply_predicate_and_only_include_items_when_true() = 
         let v = Vector.ofArray largeArray
         let pred item = item % 2 = 0
         let filteredV = v |> Vector.filter pred
         Assert.Equal( largeArray.Length / 2, filteredV.Count)


   module Fold = 
      [<Fact>]
      let should_apply_fold_to_each_item_and_return_final_state() = 
         let v = Vector.ofArray largeArray
         let mutable nextI = 0
         let foldSum total item = 
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1 
            total + item
         let total = v |> Vector.fold foldSum 0
         Assert.Equal (Array.sum largeArray, total)


   module FoldBack = 
      [<Fact>]
      let should_apply_fold_to_each_item_from_back_and_return_final_state() =
         let v = Vector.ofArray [|1; 2; 3|]
         let result = Vector.foldBack (fun acc elem -> acc - elem) v 0
         Assert.Equal (2, result)


   module Collect = 
      [<Fact>]
      let should_collect_each_mapped_vector() = 
         let v = Vector.ofArray [|1; 2; 3|]
         let map item = Vector.ofArray (Array.replicate item item)
         let collectedV = Vector.collect map v
         Assert.Equal ([1; 2; 2; 3; 3; 3], collectedV)


   module Choose = 
      [<Fact>]
      let should_apply_f_to_each_item_and_include_some_values() = 
         let v = Vector.ofArray <| Array.init 64 id
         let chooser item = if item = 0 ||  item = 63 then Some (item * 2) else None
         let chosenV = v |> Vector.choose chooser
         Assert.Equal (2, chosenV.Count)
         Assert.Equal ([0; 126], chosenV)


   module Reverse = 
      [<Fact>]
      let should_reverse_items_in_vector() =
         let v = Vector.ofArray largeArray
         let reversedV = Vector.rev v
         Assert.Equal(v.Count, reversedV.Count)


   module TryFind = 
      [<Fact>]
      let should_return_item_that_matches_predicate() =
         let pv = Vector.ofArray largeArray
         let target = largeArray.[largeArray.Length / 2]
         let matched = pv |> Vector.tryFind (fun item -> item = target)
         Assert.True matched.IsSome
         Assert.Equal(target, matched.Value)

      [<Fact>]
      let should_return_none_if_no_items_match_predicate() = 
         let pv = Vector.ofArray largeArray
         let matched = pv |> Vector.tryFind (fun _ -> false)
         Assert.True matched.IsNone


   module Find = 
      [<Fact>]
      let should_return_item_that_matches_predicate() =
         let pv = Vector.ofArray largeArray
         let target = largeArray.[largeArray.Length / 2]
         let matched = pv |> Vector.find (fun item -> item = target)
         Assert.Equal(target, matched)

      [<Fact>]
      let should_throw_if_no_items_match_predicate() = 
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            Vector.singleton 1 |> Vector.find (fun _ -> false) |> ignore)) |> ignore


   module TryFindIndex = 
      [<Fact>]
      let should_return_item_that_matches_predicate() =
         let pv = Vector.ofArray largeArray
         let targetIndex = largeArray.Length / 2
         let target = largeArray.[largeArray.Length / 2]
         let matchedIndex = pv |> Vector.tryFindIndex (fun item -> item = target)
         Assert.True matchedIndex.IsSome
         Assert.Equal(targetIndex, matchedIndex.Value)

      [<Fact>]
      let should_throw_if_no_items_match_predicate() = 
         let pv = Vector.ofArray largeArray
         let matched = pv |> Vector.tryFindIndex (fun _ -> false)
         Assert.True matched.IsNone


   module FindIndex = 
      [<Fact>]
      let should_return_item_that_matches_predicate() =
         let pv = Vector.ofArray largeArray
         let targetIndex = largeArray.Length / 2
         let target = largeArray.[largeArray.Length / 2]
         let matchedIndex = pv |> Vector.findIndex (fun item -> item = target)
         Assert.Equal(targetIndex, matchedIndex)

      [<Fact>]
      let should_throw_if_no_items_match_predicate() = 
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            Vector.singleton 1 |> Vector.findIndex (fun _ -> false) |> ignore)) |> ignore


   module ForAll = 
      [<Fact>]
      let should_return_true_when_all_items_match() =
         let pv = Vector.ofArray [|3;4;7;6;5;1|]
         let allGreaterThan0 = pv |> Vector.forall (fun i -> i > 0)
         Assert.True allGreaterThan0

      [<Fact>]
      let should_return_false_when_all_items_do_not_match() =
         let pv = Vector.ofArray [|3;4;7;6;5;-1|]
         let allGreaterThan0 = pv |> Vector.forall (fun i -> i > 0)
         Assert.False allGreaterThan0


   module Iter = 
      [<Fact>]
      let should_apply_function_to_each_item_in_vector() = 
         let v = Vector.ofArray largeArray
         let mutable nextI = 0
         let iterAction item =
            Assert.Equal (v.[nextI], item) 
            nextI <- nextI + 1
         v |> Vector.iter iterAction
         Assert.Equal (v.Count, nextI)


   module Iteri = 
      [<Fact>]
      let should_apply_function_to_each_item_in_vector() = 
         let v = Vector.ofArray largeArray
         let mutable nextI = 0
         let iteriAction i item =
            Assert.Equal (nextI, i)
            Assert.Equal (v.[nextI], item) 
            nextI <- nextI + 1
         v |> Vector.iteri iteriAction
         Assert.Equal (v.Count, nextI)


   module ToArray = 
      [<Fact>]
      let should_copy_elements_to_array() = 
         let v = Vector.ofArray largeArray
         let arr = Vector.toArray v
         Assert.Equal (v.Count, arr.Length)
         for i in [0 .. v.Count - 1] do
            Assert.Equal (v.[i], arr.[i]) 

      [<Fact>]
      let should_return_empty_array_if_vector_is_empty() = 
          let arr = Vector.toArray Vector.empty
          Assert.NotNull arr
          Assert.Equal (0, arr.Length)