namespace Strat.Collections.Test

open System
open System.Collections.Generic
open Strat.Collections
open Xunit


module IndexList =

   let largeArray = Array.unfold (fun i -> if i < 10000 then Some(i - 1, i + 1) else None) 0


   module Empty = 
      [<Fact>]
      let should_have_size_0() = 
         Assert.Equal(0, IndexList.empty.Count)

      [<Fact>]
      let should_be_empty() = 
         Assert.True IndexList.empty.IsEmpty

      [<Fact>]
      let should_throw_from_indexer() = 
         Assert.Throws<IndexOutOfRangeException>(Action(fun () -> IndexList.empty.[0])) |> ignore

      [<Fact>]
      let should_yield_no_values_from_enumerator() =
         let list = List.ofSeq IndexList.empty
         Assert.True list.IsEmpty


   module OfArray = 
      [<Fact>]
      let should_create_list_with_same_elements_as_array() = 
         let arr =  [|3;4;7;6;5;7|]
         Assert.Equal (3, arr.[0])
         let il = IndexList.ofArray arr
         for i = 0 to il.Count - 1 do
            Assert.Equal (arr.[i], il.[i])


   module Count = 
      [<Fact>]
      let should_return_count() = 
         let pv = IndexList.ofArray largeArray
         Assert.Equal(largeArray.Length, pv.Count)


   module IsEmpty = 
      [<Fact>]
      let should_return_false_if_not_empty() = 
         let pv = IndexList.ofSeq ["one"; "two"]
         Assert.False pv.IsEmpty


   module Indexer = 
      [<Fact>]
      let should_return_value_at_index() = 
         let il = IndexList.ofArray largeArray
         largeArray |> Array.iteri (fun idx item -> Assert.Equal (item, il.[idx]))

      [<Fact>]
      let should_throw_index_out_of_range() =
         let pv = IndexList.ofSeq ['a'; 'b']
         Assert.Throws<IndexOutOfRangeException>(Action(fun () -> 
            pv.[-1] |> ignore)) |> ignore
         Assert.Throws<IndexOutOfRangeException>(Action(fun () -> 
            pv.[2] |> ignore)) |> ignore



   module Head = 
      [<Fact>]
      let should_return_first_element_in_list() = 
         let v = IndexList.ofArray largeArray
         let head = v.Head
         Assert.Equal (largeArray.[0], head)

      [<Fact>]
      let should_throw_if_list_is_empty() = 
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            IndexList.empty.Head |> ignore )) |> ignore


   module TryHead = 
      [<Fact>]
      let should_return_first_element_in_list() = 
         let v = IndexList.ofArray largeArray
         let head = v.TryHead
         Assert.True head.IsSome
         Assert.Equal (largeArray.[0], head.Value)

      [<Fact>]
      let should_return_none_if_list_is_empty() = 
         Assert.True (IndexList.empty |> IndexList.tryHead |> Option.isNone)


    module Set = 
      [<Fact>]
      let should_set_item_at_index() = 
         let mutable il = IndexList.ofArray largeArray
         // Reverse items in il
         for i in [largeArray.Length - 1 .. -1 .. 0] do
            let item = largeArray.[i]
            let pvi = largeArray.Length - i - 1
            il <- il.Set (pvi, item)

         largeArray
         |> Array.iteri (fun idx item -> 
            let newIdx = largeArray.Length - idx - 1
            Assert.Equal(item, il.[newIdx])) 

      [<Fact>]
      let should_leave_original_vector_unchanged() = 
         let il = IndexList.ofArray largeArray
         let idx = il.Count / 2
         let orig = il.[idx]
         let newV = il.Set (idx, orig *2)
         Assert.Equal(orig, il.[idx])


   module Cons = 
      [<Fact>]
      let should_add_item_at_front_of_list() = 
         let arr = Array.unfold (fun i -> if i < 1000 then Some(i + 1, i + 1) else None) 0
         let mutable il = IndexList.empty
         arr |> Array.iteri (fun idx item -> il <- il.Cons item)
         Assert.Equal(arr |> Array.rev, il)


   module RemoveHead = 
      [<Fact>]
      let should_remove_first_element() = 
         let mutable il = IndexList.ofArray largeArray
         for i = 0 to largeArray.Length - 1 do
            let item, newIl = il.RemoveHead()
            Assert.Equal(largeArray.[i], item)
            il <- newIl
         Assert.True il.IsEmpty

      [<Fact>]
      let should_throw_if_list_is_empty() = 
         Assert.Throws<InvalidOperationException>(Action(fun () -> 
            IndexList.empty.RemoveHead() |> ignore)) |> ignore


   module GetEnumerator = 
      [<Fact>]
      let should_return_enumerator_that_yields_items_in_list() =
         let il = IndexList.ofArray largeArray
         use e = (il :> IEnumerable<int>).GetEnumerator()
         let mutable currentIndex = 0
         while e.MoveNext() do
            Assert.Equal (largeArray.[currentIndex], e.Current)
            currentIndex <- currentIndex + 1 
         Assert.Equal (il.Count, currentIndex)

      [<Fact>]
      let should_return_enumerator_that_can_be_reset() = 
         let il = IndexList.ofArray largeArray
         use e = (il :> IEnumerable<int>).GetEnumerator()     
         for i in [1..(il.Count / 2)] do
            e.MoveNext() |> ignore
        
         e.Reset()
         
         let mutable currentIndex = 0
         while e.MoveNext() do
            Assert.Equal (largeArray.[currentIndex], e.Current)
            currentIndex <- currentIndex + 1 


   module Init = 
      [<Fact>]
      let should_call_function_to_create_each_element() = 
         let il = IndexList.init 5 (fun i -> i * 2)
         Assert.Equal ([|0; 2; 4; 6; 8;|], il)


   module Append = 
      [<Fact>]
      let should_add_elements_from_list2_to_end_of_list1() =
         let l1 = IndexList.ofArray [|1; 2; 3|]
         let l2 = IndexList.ofArray [|4; 5; 6|]
         let l = IndexList.append l1 l2
         Assert.Equal ([|1; 2; 3; 4; 5; 6|], l)
      
      [<Fact>]
      let should_return_list2_if_list1_is_empty() =
         let l1 = IndexList.empty
         let l2 = IndexList.ofArray [|4; 5; 6|]
         let l = IndexList.append l1 l2
         Assert.Same (l2, l)

      [<Fact>]
      let should_return_list1_if_list2_is_empty() =
         let l1 = IndexList.ofArray [|1; 2; 3|]
         let l2 = IndexList.empty
         let l = IndexList.append l1 l2
         Assert.Same (l1, l)


   module MapIndexed = 
      [<Fact>]
      let should_apply_mapping_to_each_item_in_list() = 
         let l = IndexList.ofArray largeArray
         let mutable nextI = 0
         let f (idx: int) (item: int) =
            Assert.Equal (nextI, idx) 
            Assert.Equal (l.[nextI], item)
            nextI <- nextI + 1 
            item - 1
         let mappedL = IndexList.mapi f l
         Assert.Equal (l.Count, mappedL.Count)
         for i = 0 to l.Count - 1 do
            Assert.Equal (l.[i] - 1, mappedL.[i])


   module Map = 
      [<Fact>]
      let should_apply_mapping_to_each_item_in_list() =
         let l = IndexList.ofArray  <| Array.init 33 id
         let mutable nextI = 0
         let f (item: int) =
            Assert.Equal (l.[nextI], item)
            nextI <- nextI + 1 
            item * 2
         let mappedL = IndexList.map f l
         Assert.Equal (l.Count, mappedL.Count)
         for i = 0 to l.Count - 1 do
            Assert.Equal (l.[i] * 2, mappedL.[i])


   module Filter = 
      [<Fact>]
      let should_apply_predicate_and_only_include_items_when_true() = 
         let l = IndexList.ofArray largeArray
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (l.[nextI], item)
            nextI <- nextI + 1 
            item % 2 = 0
         let filteredL = l |> IndexList.filter pred
         Assert.Equal( largeArray.Length / 2, filteredL.Count)


   module Fold = 
      [<Fact>]
      let should_apply_fold_to_each_item_and_return_final_state() = 
         let l = IndexList.ofArray largeArray
         let mutable nextI = 0
         let foldSum total item = 
            Assert.Equal (l.[nextI], item)
            nextI <- nextI + 1 
            total + item
         let total = l |> IndexList.fold foldSum 0
         Assert.Equal (Array.sum largeArray, total)

      [<Fact>]
      let should_return_initial_state_if_list_is_empty() =
         let foldSum total v = total + v
         let total = IndexList.empty |> IndexList.fold foldSum -1
         Assert.Equal (-1, total)


   module FoldBack = 
      [<Fact>]
      let should_apply_fold_to_each_item_from_back_and_return_final_state() =
         let l = IndexList.ofArray [|1; 2; 3|]
         let mutable nextI = l.Count - 1
         let foldDiff item total = 
            Assert.Equal (l.[nextI], item)
            nextI <- nextI - 1  
            item - total
         let result = IndexList.foldBack (fun acc elem -> acc - elem) l 0
         Assert.Equal (2, result)

      [<Fact>]
      let should_return_initial_state_if_list_is_empty() =
         let foldSum v total = total + v
         let total = IndexList.foldBack foldSum IndexList.empty -1
         Assert.Equal (-1, total)


   module Collect = 
      [<Fact>]
      let should_collect_each_mapped_list() = 
         let l = IndexList.ofArray [|1; 2; 3|]
         let mutable nextI = 0
         let map item = 
            Assert.Equal (l.[nextI], item)
            nextI <- nextI + 1  
            IndexList.ofArray (Array.replicate item item)
         let collectedV = IndexList.collect map l
         Assert.Equal ([1; 2; 2; 3; 3; 3], collectedV)


   module Choose = 
      [<Fact>]
      let should_apply_f_to_each_item_and_include_some_values() = 
         let l = IndexList.ofArray <| Array.init 64 id
         let chooser item = if item = 0 || item = 63 then Some (item * 2) else None
         let chosenL = l |> IndexList.choose chooser
         Assert.Equal (2, chosenL.Count)
         Assert.Equal ([0; 126], chosenL)


    module Reverse = 
      [<Fact>]
      let should_reverse_items_in_list() =
         let l = IndexList.ofArray largeArray
         let reversedL = IndexList.rev l
         for i = 0 to l.Count - 1 do
            Assert.Equal (largeArray.[l.Count - 1 - i], reversedL.[i])


   module TryFind = 
      [<Fact>]
      let should_return_item_that_matches_predicate() =
         let il = IndexList.ofArray largeArray
         let target = largeArray.[largeArray.Length / 2]
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (il.[nextI], item)
            nextI <- nextI + 1  
            item = target
         let matched : option<int> = il |> IndexList.tryFind pred
         Assert.True matched.IsSome
         Assert.Equal(target, matched.Value)

      [<Fact>]
      let should_return_none_if_no_items_match_predicate() = 
         let il = IndexList.ofArray largeArray
         let matched : option<int> = il |> IndexList.tryFind (fun _ -> false)
         Assert.True matched.IsNone


   module Find = 
      [<Fact>]
      let should_return_item_that_matches_predicate() =
         let il = IndexList.ofArray largeArray
         let target = largeArray.[largeArray.Length / 2]
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (il.[nextI], item)
            nextI <- nextI + 1  
            item = target
         let matched = il |> IndexList.find pred
         Assert.Equal (target, matched)

      [<Fact>]
      let should_throw_if_no_items_match_predicate() = 
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            IndexList.singleton 1 |> IndexList.find (fun _ -> false) |> ignore)) |> ignore



   module TryFindIndex = 
      [<Fact>]
      let should_return_item_that_matches_predicate() =
         let il = IndexList.ofArray largeArray
         let targetIndex = largeArray.Length / 2
         let target = largeArray.[largeArray.Length / 2]
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (il.[nextI], item)
            nextI <- nextI + 1  
            item = target
         let matchedIndex : option<int> = il |> IndexList.tryFindIndex pred
         Assert.True matchedIndex.IsSome
         Assert.Equal(targetIndex, matchedIndex.Value)

      [<Fact>]
      let should_throw_if_no_items_match_predicate() = 
         let pv = IndexList.ofArray largeArray
         let matched : option<int> = pv |> IndexList.tryFindIndex (fun _ -> false)
         Assert.True matched.IsNone


   module FindIndex = 
      [<Fact>]
      let should_return_item_that_matches_predicate() =
         let il = IndexList.ofArray largeArray
         let targetIndex = largeArray.Length / 2
         let target = largeArray.[largeArray.Length / 2]
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (il.[nextI], item)
            nextI <- nextI + 1  
            item = target
         let matchedIndex = il |> IndexList.findIndex pred
         Assert.Equal(targetIndex, matchedIndex)

      [<Fact>]
      let should_throw_if_no_items_match_predicate() = 
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            IndexList.singleton 1 |> IndexList.findIndex (fun _ -> false) |> ignore)) |> ignore


   module TryPick = 
      [<Fact>]
      let should_return_picked_item() =
         let arr =  [|3;4;7;6;5;7|]
         let il = IndexList.ofArray arr
         let mutable pickCount = 0
         let mutable nextI = 0
         let picker item = 
            Assert.Equal (il.[nextI], item)
            nextI <- nextI + 1  
            pickCount <- pickCount + 1
            if item = 7 then Some("7") else None
         let picked : option<string> = il |> IndexList.tryPick picker
         Assert.True picked.IsSome
         Assert.Equal ("7", picked.Value)
         Assert.Equal (3, pickCount)


      [<Fact>]
      let should_return_none_if_no_item_is_picked() =
         let il = IndexList.ofArray [|3;4;7;6;5;7|]
         let mutable pickCount = 0
         let picker item = 
            pickCount <- pickCount + 1
            None
         let picked : option<string> = il |> IndexList.tryPick picker
         Assert.True picked.IsNone
         Assert.Equal (il.Count, pickCount)


   module Pick = 
      [<Fact>]
      let should_return_first_some_returned_by_function() =
         let il = IndexList.ofArray [|3;4;7;6;5;7|]
         let mutable pickCount = 0
         let mutable nextI = 0
         let picker item = 
            Assert.Equal (il.[nextI], item)
            nextI <- nextI + 1  
            pickCount <- pickCount + 1
            if item = 7 then Some("7") else None
         let picked = il |> IndexList.pick picker
         Assert.Equal ("7", picked)
         Assert.Equal (3, pickCount)


      [<Fact>]
      let should_throw_if_function_never_returns_some() =
         let il = IndexList.ofArray [|3;4;7;6;5;7|]
         let mutable pickCount = 0
         let picker item = 
            pickCount <- pickCount + 1
            None
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            il |> IndexList.pick picker |> ignore)) |> ignore    
         Assert.Equal (il.Count, pickCount)


   module Exists = 
      [<Fact>]
      let should_return_true_if_predicate_matches_element() =
         let il = IndexList.ofArray [|3;4;7;6;5;7|]
         let mutable predCount = 0
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (il.[nextI], item)
            nextI <- nextI + 1  
            predCount <- predCount + 1
            item = 7
         let doesExist = il |> IndexList.exists pred
         Assert.True doesExist
         Assert.Equal (3, predCount)

      [<Fact>]
      let should_return_false_if_predicate_matches_none() =
         let il = IndexList.ofArray [|3;4;7;6;5;7|]
         let mutable predCount = 0
         let pred item = 
            predCount <- predCount + 1
            item = 10
         let doesExist = il |> IndexList.exists pred
         Assert.False doesExist
         Assert.Equal (6, predCount)


   module Zip = 
      [<Fact>]
      let should_return_pairs_of_items_in_both_lists() =
         let il1 = IndexList.ofArray [|1;2;3;4;5|] 
         let il2 = IndexList.ofArray [|'1';'2';'3';'4';'5'|]
         let zipped = IndexList.zip il1 il2
         Assert.Equal<int*char> ([|(1, '1');(2, '2');(3, '3');(4, '4');(5, '5');|], zipped)

      [<Fact>]
      let should_throw_if_lists_have_different_lengths() = 
         let il1 = IndexList.ofArray [|1;2|] 
         let il2 = IndexList.ofArray [|'1';|]
         Assert.Throws<ArgumentException>(Action(fun () -> 
            IndexList.zip il1 il2 |> ignore)) |> ignore


   module ForAll = 
      [<Fact>]
      let should_return_true_when_all_items_match() =
         let il = IndexList.ofArray [|3;4;7;6;5;1|]
         let mutable nextI = 0
         let mutable predCount = 0
         let pred item = 
            Assert.Equal (il.[nextI], item)
            nextI <- nextI + 1  
            predCount <- predCount + 1  
            item > 0
         let allGreaterThan0 = il |> IndexList.forall pred
         Assert.True allGreaterThan0
         Assert.Equal (il.Count, predCount)

      [<Fact>]
      let should_return_false_when_all_items_do_not_match() =
         let il = IndexList.ofArray [|3;4;7;6;5;-1|]
         let mutable nextI = 0
         let mutable predCount = 0
         let pred item = 
            Assert.Equal (il.[nextI], item)
            nextI <- nextI + 1  
            predCount <- predCount + 1  
            item > 0
         let allGreaterThan0 = il |> IndexList.forall pred
         Assert.False allGreaterThan0
         Assert.Equal (il.Count, predCount)


   module Min = 
      [<Fact>]
      let should_return_min_item_in_list() = 
         let il = IndexList.ofArray [|3;-1;7;6;5;-1|]
         let min = il |> IndexList.min
         Assert.Equal (-1, min)
      
      [<Fact>]
      let should_throw_for_empty_list() =
         Assert.Throws<ArgumentException>(Action(fun () -> 
            IndexList.min IndexList.empty |> ignore)) |> ignore


   module Max = 
      [<Fact>]
      let should_return_min_item_in_list() = 
         let il = IndexList.ofArray [|3;-1;7;6;5;-1|]
         let max = il |> IndexList.max
         Assert.Equal (7, max)
      
      [<Fact>]
      let should_throw_for_empty_list() =
         Assert.Throws<ArgumentException>(Action(fun () -> 
            IndexList.max IndexList.empty |> ignore)) |> ignore



   module Sum = 
      [<Fact>]
      let should_sum_items_in_vector() = 
         let il = IndexList.ofArray [|3;4;7;6;5;-1|]
         let sum = il |> IndexList.sum
         Assert.Equal (24, sum)
      
      [<Fact>]
      let should_return_0_for_empty_list() =
         Assert.Equal (0, IndexList.sum IndexList.empty)


   module ToArray = 
      [<Fact>]
      let should_copy_elements_to_array() = 
         let v = IndexList.ofArray largeArray
         let arr = IndexList.toArray v
         Assert.Equal (v, arr)

      [<Fact>]
      let should_return_empty_array_if_list_is_empty() = 
          let arr = IndexList.toArray IndexList.empty
          Assert.NotNull arr
          Assert.Equal (0, arr.Length)


   module ToList = 
      [<Fact>]
      let should_copy_elements_to_list() = 
         let v = IndexList.ofArray largeArray
         let list = IndexList.toList v
         Assert.Equal (v, list)

      [<Fact>]
      let should_return_empty_list_if_list_is_empty() = 
          let list = IndexList.toList IndexList.empty
          Assert.NotNull list
          Assert.True list.IsEmpty