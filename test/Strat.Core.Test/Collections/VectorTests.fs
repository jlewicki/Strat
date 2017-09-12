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


   module Last = 
      [<Fact>]
      let should_return_last_element_in_vector() = 
         let v = Vector.ofArray largeArray
         let last = v.Last
         Assert.Equal (largeArray.[largeArray.Length - 1], last)

      [<Fact>]
      let should_throw_if_vector_is_empty() = 
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            Vector.empty.Last |> ignore )) |> ignore


   module TryLast = 
      [<Fact>]
      let should_return_last_element_in_vector() = 
         let v = Vector.ofArray largeArray
         let last = v.TryLast
         Assert.True last.IsSome
         Assert.Equal (largeArray.[largeArray.Length - 1], last.Value)

      [<Fact>]
      let should_throw_if_vector_is_empty() = 
         Assert.True (Vector.empty |> Vector.tryLast |> Option.isNone)


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
         largeArray |> Array.iteri (fun idx item -> pv <- pv.Add item)
         Assert.Equal(largeArray, pv)


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


   module Init = 
      [<Fact>]
      let should_call_function_to_create_each_element() = 
         let v = Vector.init 5 (fun i -> i * 2)
         Assert.Equal ([|0; 2; 4; 6; 8;|], v)


   module Append = 
      [<Fact>]
      let should_add_elements_from_vector2_to_end_of_vector1() =
         let v1 = Vector.ofArray [|1; 2; 3|]
         let v2 = Vector.ofArray [|4; 5; 6|]
         let v = Vector.append v1 v2
         Assert.Equal ([|1; 2; 3; 4; 5; 6|], v)
      
      [<Fact>]
      let should_return_vector2_if_vector1_is_empty() =
         let v1 = Vector.empty
         let v2 = Vector.ofArray [|4; 5; 6|]
         let v = Vector.append v1 v2
         Assert.Same (v2, v)

      [<Fact>]
      let should_return_vector1_if_vector2_is_empty() =
         let v1 = Vector.ofArray [|1; 2; 3|]
         let v2 = Vector.empty
         let v = Vector.append v1 v2
         Assert.Same (v1, v)


   module MapIndexed = 
      [<Fact>]
      let should_apply_mapping_to_each_item_in_vector() = 
         let v = Vector.ofArray largeArray
         let mutable nextI = 0
         let f (idx: int) (item: int) =
            Assert.Equal (nextI, idx) 
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1 
            item - idx
         let mappedV = Vector.mapi f v
         Assert.Equal (v.Count, mappedV.Count)
         for i = 0 to v.Count - 1 do
            Assert.Equal (v.[i] - i, mappedV.[i])


   module Map = 
      [<Fact>]
      let should_apply_mapping_to_each_item_in_vector() = 
         let v = Vector.ofArray largeArray
         let mutable nextI = 0
         let f (item: int) =
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1 
            item * 2
         let mappedV = Vector.map f v
         Assert.Equal (v.Count, mappedV.Count)
         for i = 0 to v.Count - 1 do
            Assert.Equal (v.[i] * 2, mappedV.[i])


   module Filter = 
      [<Fact>]
      let should_apply_predicate_and_only_include_items_when_true() = 
         let v = Vector.ofArray largeArray
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1 
            item % 2 = 0
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
         let mutable nextI = v.Count - 1
         let foldDiff item total =
            Assert.Equal (v.[nextI], item)
            nextI <- nextI - 1  
            item - total
         let result = Vector.foldBack foldDiff v 0
         Assert.Equal (2, result)


   module Collect = 
      [<Fact>]
      let should_collect_each_mapped_vector() = 
         let v = Vector.ofArray [|1; 2; 3|]
         let mutable nextI = 0
         let map item = 
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1  
            Vector.ofArray (Array.replicate item item)
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
         Assert.Equal (largeArray |> Array.rev, reversedV)


   module TryFind = 
      [<Fact>]
      let should_return_item_that_matches_predicate() =
         let v = Vector.ofArray largeArray
         let target = largeArray.[largeArray.Length / 2]
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1  
            item = target
         let matched = v |> Vector.tryFind pred
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
         let v = Vector.ofArray largeArray
         let target = largeArray.[largeArray.Length / 2]
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1  
            item = target
         let matched = v |> Vector.find pred
         Assert.Equal(target, matched)

      [<Fact>]
      let should_throw_if_no_items_match_predicate() = 
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            Vector.singleton 1 |> Vector.find (fun _ -> false) |> ignore)) |> ignore


   module TryFindIndex = 
      [<Fact>]
      let should_return_item_that_matches_predicate() =
         let v = Vector.ofArray largeArray
         let targetIndex = largeArray.Length / 2
         let target = largeArray.[largeArray.Length / 2]
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1  
            item = target
         let matchedIndex = v |> Vector.tryFindIndex pred
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
         let v = Vector.ofArray largeArray
         let targetIndex = largeArray.Length / 2
         let target = largeArray.[largeArray.Length / 2]
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1  
            item = target
         let matchedIndex = v |> Vector.findIndex pred
         Assert.Equal(targetIndex, matchedIndex)

      [<Fact>]
      let should_throw_if_no_items_match_predicate() = 
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            Vector.singleton 1 |> Vector.findIndex (fun _ -> false) |> ignore)) |> ignore


   module TryPick = 
      [<Fact>]
      let should_return_first_some_returned_by_function() =
         let v = Vector.ofArray [|3;4;7;6;5;7|]
         let mutable pickCount = 0
         let mutable nextI = 0
         let picker item = 
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1  
            pickCount <- pickCount + 1
            if item = 7 then Some("7") else None
         let picked = v |> Vector.tryPick picker
         Assert.True picked.IsSome
         Assert.Equal ("7", picked.Value)
         Assert.Equal (3, pickCount)


      [<Fact>]
      let should_return_none_if_function_never_returns_some() =
         let v = Vector.ofArray [|3;4;7;6;5;7|]
         let mutable pickCount = 0
         let picker item = 
            pickCount <- pickCount + 1
            None
         let picked = v |> Vector.tryPick picker
         Assert.True picked.IsNone
         Assert.Equal (v.Count, pickCount)


   module Pick = 
      [<Fact>]
      let should_return_first_some_returned_by_function() =
         let v = Vector.ofArray [|3;4;7;6;5;7|]
         let mutable pickCount = 0
         let mutable nextI = 0
         let picker item = 
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1  
            pickCount <- pickCount + 1
            if item = 7 then Some("7") else None
         let picked = v |> Vector.pick picker
         Assert.Equal ("7", picked)
         Assert.Equal (3, pickCount)


      [<Fact>]
      let should_throw_if_function_never_returns_some() =
         let v = Vector.ofArray [|3;4;7;6;5;7|]
         let mutable pickCount = 0
         let picker item = 
            pickCount <- pickCount + 1
            None
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            v |> Vector.pick picker |> ignore)) |> ignore    
         Assert.Equal (v.Count, pickCount)


   module Exists = 
      [<Fact>]
      let should_return_true_if_predicate_matches_element() =
         let v = Vector.ofArray [|3;4;7;6;5;7|]
         let mutable predCount = 0
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1  
            predCount <- predCount + 1
            item = 7
         let doesExist = v |> Vector.exists pred
         Assert.True doesExist
         Assert.Equal (3, predCount)

      [<Fact>]
      let should_return_false_if_predicate_matches_nonw() =
         let pv = Vector.ofArray [|3;4;7;6;5;7|]
         let mutable predCount = 0
         let pred item = 
            predCount <- predCount + 1
            item = 10
         let doesExist = pv |> Vector.exists pred
         Assert.False doesExist
         Assert.Equal (6, predCount)


   module Zip = 
      [<Fact>]
      let should_return_pairs_of_items_in_both_vectors() =
         let v1 = Vector.ofArray [|1;2;3;4;5|] 
         let v2 = Vector.ofArray [|'1';'2';'3';'4';'5'|]
         let zipped = Vector.zip v1 v2
         Assert.Equal<int*char> ([|(1, '1');(2, '2');(3, '3');(4, '4');(5, '5');|], zipped)

      [<Fact>]
      let should_throw_if_vectors_have_different_lengths() = 
         let v1 = Vector.ofArray [|1;2|] 
         let v2 = Vector.ofArray [|'1';|]
         Assert.Throws<ArgumentException>(Action(fun () -> 
            Vector.zip v1 v2 |> ignore)) |> ignore    


   module ForAll = 
      [<Fact>]
      let should_return_true_when_all_items_match() =
         let v = Vector.ofArray [|3;4;7;6;5;1|]
         let mutable nextI = 0
         let mutable predCount = 0
         let pred item = 
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1  
            predCount <- predCount + 1
            item > 0
         let allGreaterThan0 = v |> Vector.forall pred
         Assert.True allGreaterThan0
         Assert.Equal (v.Count, predCount)

      [<Fact>]
      let should_return_false_when_all_items_do_not_match() =
         let v = Vector.ofArray [|3;4;7;6;5;-1|]
         let mutable nextI = 0
         let mutable predCount = 0
         let pred item = 
            Assert.Equal (v.[nextI], item)
            nextI <- nextI + 1  
            predCount <- predCount + 1
            item > 0
         let allGreaterThan0 = v |> Vector.forall pred
         Assert.False allGreaterThan0
         Assert.Equal (v.Count, predCount)


   module Min = 
      [<Fact>]
      let should_return_min_item_in_vector() = 
         let v = Vector.ofArray [|3;4;7;6;5;-1|]
         let sum= v |> Vector.min
         Assert.Equal (-1, sum)
      
      [<Fact>]
      let should_throw_for_empty_vector() =
         Assert.Throws<ArgumentException>(Action(fun () -> 
            Vector.min Vector.empty |> ignore)) |> ignore


   module MinBy = 
      [<Fact>]
      let should_return_min_item_in_vector() = 
         let v = Vector.ofArray [|(3, 1);(4, 2); (7, 3); (6, 4); (5, 6); (-1, 7)|]
         let val1, _ = v |> Vector.minBy fst
         Assert.Equal (-1, val1)
      
      [<Fact>]
      let should_throw_for_empty_vector() =
         Assert.Throws<ArgumentException>(Action(fun () ->
            Vector.minBy fst Vector.empty |> ignore)) |> ignore


   module Max = 
      [<Fact>]
      let should_return_max_item_in_vector() = 
         let v = Vector.ofArray [|3;4;7;6;5;-1|]
         let sum = v |> Vector.max
         Assert.Equal (7, sum)
      
      [<Fact>]
      let should_throw_for_empty_vector() =
         Assert.Throws<ArgumentException>(Action(fun () -> 
            Vector.max Vector.empty |> ignore)) |> ignore


   module MaxBy = 
      [<Fact>]
      let should_return_max_item_in_vector() = 
         let v = Vector.ofArray [|(3, 1);(4, 2); (7, 3); (6, 4); (5, 6); (-1, 7)|]
         let val1, _ = v |> Vector.maxBy fst
         Assert.Equal (7, val1)
      
      [<Fact>]
      let should_throw_for_empty_vector() =
         Assert.Throws<ArgumentException>(Action(fun () ->
            Vector.maxBy fst Vector.empty |> ignore)) |> ignore


   module Sum = 
      [<Fact>]
      let should_sum_items_in_vector() = 
         let v = Vector.ofArray [|3;4;7;6;5;-1|]
         let sum= v |> Vector.sum
         Assert.Equal (24, sum)
      
      [<Fact>]
      let should_return_0_for_empty_vector() =
         Assert.Equal (0, Vector.sum Vector.empty)


    module SumBy = 
      [<Fact>]
      let should_sum_items_in_vector() = 
         let v = Vector.ofArray [|(3, 1);(4, 2); (7, 3); (6, 4); (5, 6); (-1, 7)|]
         let sum= v |> Vector.sumBy fst
         Assert.Equal (24, sum)
      
      [<Fact>]
      let should_return_0_for_empty_vector() =
         let e : Vector<int*int> = Vector.empty
         Assert.Equal (0, e |> Vector.sumBy fst)


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