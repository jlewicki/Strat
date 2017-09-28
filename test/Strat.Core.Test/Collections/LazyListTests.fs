namespace Strat.Collections.Test

open System
open System.Collections.Generic
open Strat.Collections
open Xunit

module LazyList = 

   let largeArray = Array.unfold (fun i -> if i < 10000 then Some(i + 1, i + 1) else None) 0

   let wrapWithCounter f = 
      let counter = ref 0
      counter, fun a -> counter := !counter + 1; f a

   let wrapWithCounter2 f = 
      let counter = ref 0
      counter, fun a b -> counter := !counter + 1; f a b

  
   module Empty = 
      [<Fact>]
      let should_have_size_0() = 
         Assert.Equal(0, LazyList.empty.Length())

      [<Fact>]
      let should_be_empty() = 
         Assert.True LazyList.empty.IsEmpty

      [<Fact>]
      let should_yield_no_values_from_enumerator() =
         let list = List.ofSeq LazyList.empty
         Assert.True list.IsEmpty


   module Init = 
      [<Fact>]
      let should_create_list_by_applying_function() = 
         let length = 10
         let mutable generatorCount = 0
         let generator idx =
            Assert.Equal (idx, generatorCount)
            generatorCount <- generatorCount + 1
            idx
         let mutable ll = LazyList.init length generator
         // List is lazy, so no invocations of generator until we enumerate list
         Assert.Equal (0, generatorCount)
         for i = 0 to length - 1 do
            let h = ll.Head
            Assert.Equal (i, h)
            ll <- ll.Tail
         Assert.Equal (length, generatorCount)
         let l = List.ofSeq ll
         // Iterating again should use cached values
         Assert.Equal (length, generatorCount)


   module Unfold = 
      [<Fact>]
      let should_create_list_by_applying_function_until_it_returns_none() =  
         let length = 10
         let mutable generatorCount = 0
         let generator state =
            Assert.Equal (-generatorCount, state)
            generatorCount <- generatorCount + 1
            if state > -length  then Some (state.ToString(), state - 1)
            else None
         let llUnfold = LazyList.unfold generator 0
         let mutable ll = llUnfold
         // List is lazy, so no invocations of generator until we enumerate list
         Assert.Equal (0, generatorCount)
         let mutable idx = 0
         while not ll.IsEmpty do 
            let h = ll.Head
            Assert.Equal (idx.ToString(), h)
            ll <- ll.Tail
            idx <- idx - 1
         // There are lennth + 1 calls to generator. The +1 is from the last call that returns None
         Assert.Equal (length + 1, generatorCount)
         Assert.True ll.IsEmpty
         let l = List.ofSeq llUnfold
         // Iterating again should use cached values
         Assert.Equal (length + 1, generatorCount)


   module Length = 
      [<Fact>]
      let should_return_count() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         Assert.Equal(100, ll.Length())
         Assert.Equal(!counter, 100)
         ll.Length() |> ignore
         Assert.Equal(!counter, 100)


   module IsEmpty = 
      [<Fact>]
      let should_return_false_if_not_empty() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 10 _id
         Assert.False ll.IsEmpty
         Assert.Equal (1, !counter)
         ll.IsEmpty |> ignore
         Assert.Equal (1, !counter)

      [<Fact>]
      let should_return_true_if_empty() = 
         Assert.True LazyList.empty.IsEmpty


   module Head = 
      [<Fact>]
      let should_return_first_element_in_list() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let head = ll.Head
         Assert.Equal (0, head)
         Assert.Equal (1, !counter)
         ll.Head |> ignore
         Assert.Equal (1, !counter)

      [<Fact>]
      let should_throw_if_list_is_empty() = 
         Assert.Throws<InvalidOperationException>(Action(fun () -> 
            IndexList.empty.Head |> ignore )) |> ignore


   module TryHead = 
      [<Fact>]
      let should_return_first_element_in_list() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let head = ll.TryHead
         Assert.True head.IsSome
         Assert.Equal (0, head.Value)
         Assert.Equal (1, !counter)
         ll.TryHead |> ignore
         Assert.Equal (1, !counter)

      [<Fact>]
      let should_return_none_if_list_is_empty() = 
         Assert.True (LazyList.empty |> LazyList.tryHead |> Option.isNone)


   module Tail = 
      [<Fact>]
      let should_return_tail_of_list() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let tail = ll.Tail
         Assert.Equal (1, !counter)
         let head = tail.Head
         Assert.Equal (1, head)
         Assert.Equal (2, !counter)

      [<Fact>]
      let should_throw_if_list_is_empty() = 
         Assert.Throws<InvalidOperationException>(Action(fun () -> 
            LazyList.empty.Tail |> ignore )) |> ignore


   module TryTail = 
      [<Fact>]
      let should_return_tail_of_list() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let tail = ll.TryTail
         Assert.True tail.IsSome
         Assert.Equal (1, !counter)
         let head = tail.Value.Head
         Assert.Equal (1, head)
         Assert.Equal (2, !counter)

      [<Fact>]
      let should_return_none_if_list_is_empty() = 
         Assert.True (LazyList.empty |> LazyList.tryTail |> Option.isNone)


   module Cons = 
      [<Fact>]
      let should_add_item_add_front_of_list() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let llCons = ll |> LazyList.cons -1
         Assert.Equal (0, !counter)
         Assert.Equal (-1, llCons.Head)
         Assert.Same (ll, llCons.Tail)


   module Uncons = 
      [<Fact>]
      let should_return_head_and_tail() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let struct(head, tail) = ll |> LazyList.uncons
         Assert.Equal (1, !counter)
         Assert.Equal (head, ll.Head)
         Assert.Same (tail, ll.Tail)

      [<Fact>]
      let should_throw_if_list_is_empty() = 
         Assert.Throws<InvalidOperationException>(Action(fun () -> 
            LazyList.empty.Uncons() |> ignore )) |> ignore


   module TryUncons = 
      [<Fact>]
      let should_return_head_and_tail() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let struct(head, tail) = ll |> LazyList.tryUncons |> Option.get
         Assert.Equal (1, !counter)
         Assert.Equal (head, ll.Head)
         Assert.Same (tail, ll.Tail)

      [<Fact>]
      let should_return_none_if_list_is_empty() = 
         Assert.True (LazyList.empty |> LazyList.tryUncons |> Option.isNone)


   module Map = 
      [<Fact>]
      let should_apply_mapping_to_each_item_in_list() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id

         let llMapped = LazyList.map (fun item -> item.ToString()) ll
         Assert.Equal (0, !counter)

         Assert.Equal ("0", llMapped.Head)
         Assert.Equal (1, !counter)

         let mutable expectedVal = 0
         llMapped |> LazyList.iter (fun i  -> 
            Assert.Equal (expectedVal.ToString(), i)
            expectedVal <- expectedVal + 1)
         Assert.Equal (100, llMapped.Length())      
         Assert.Equal (100, expectedVal)
         Assert.Equal (100, !counter)


   module Filter = 
      [<Fact>]
      let should_filter_items_in_list() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id

         let llFiltered = LazyList.filter (fun item -> item < 10) ll
         Assert.Equal (0, !counter)

         Assert.Equal (0, llFiltered.Head)
         Assert.Equal (1, !counter)

         let mutable expectedVal = 0
         LazyList.iter (fun i  -> 
            Assert.Equal (expectedVal, i)
            expectedVal <- expectedVal + 1
         ) llFiltered
         Assert.Equal (10, llFiltered.Length())      
         Assert.Equal (10, expectedVal)
         Assert.Equal (100, !counter)


   module Fold = 
      [<Fact>]
      let should_apply_fold_to_each_item_and_return_final_state() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let arr = Array.init 100 id
         let mutable nextI = 0
         let foldSum total item = 
            Assert.Equal (arr.[nextI], item)
            nextI <- nextI + 1 
            total + item
         let total = ll |> LazyList.fold foldSum 0
         Assert.Equal (Array.sum arr, total)

      [<Fact>]
      let should_return_initial_state_if_vector_is_empty() =
         let foldSum total v = total + v
         let total = LazyList.empty |> LazyList.fold foldSum -1
         Assert.Equal (-1, total)


   module Replicate = 
      [<Fact>]
      let should_create_list_containing_the_same_item() = 
         let item = new obj()
         let ll = LazyList.replicate 5 item
         Assert.Equal (5, ll.Length())
         ll |> LazyList.iter (fun o -> Assert.Same(item, o))

      [<Fact>]
      let should_return_empty_list_when_length_is_0() = 
         let ll = LazyList.replicate 0 "A"
         Assert.True ll.IsEmpty


   module Collect = 
      [<Fact>]
      let should_collect_each_mapped_vector() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 4 _id
         let mutable nextI = 0
         let map item = 
            nextI <- nextI + 1  
            LazyList.replicate item item
         let llCollected = LazyList.collect map ll
         Assert.Equal ([1; 2; 2; 3; 3; 3], llCollected)


   module Choose = 
      [<Fact>]
      let should_apply_f_to_each_item_and_include_some_values() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id

         let llChoose= LazyList.choose (fun item -> 
            if item < 10 then Some (item.ToString()) else None ) ll
         Assert.Equal (0, !counter)

         Assert.Equal ("0", llChoose.Head)
         Assert.Equal (1, !counter)

         let mutable expectedVal = 0
         LazyList.iter (fun item  -> 
            Assert.Equal (expectedVal.ToString(), item)
            expectedVal <- expectedVal + 1
         ) llChoose
         Assert.Equal (10, llChoose.Length())      
         Assert.Equal (10, expectedVal)
         Assert.Equal (100, !counter)


   module Exists = 
      [<Fact>]
      let should_return_true_if_predicate_matches_element() =
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let arr = Array.init 100 id
         let mutable predCount = 0
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (arr.[nextI], item)
            nextI <- nextI + 1  
            predCount <- predCount + 1
            item = 3
         let doesExist = ll |> LazyList.exists pred
         Assert.True doesExist
         Assert.Equal (4, predCount)
         Assert.Equal (4, !counter)

      [<Fact>]
      let should_return_false_if_predicate_matches_none() =
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let mutable predCount = 0
         let pred item = 
            predCount <- predCount + 1
            item = -1
         let doesExist = ll |> LazyList.exists pred
         Assert.False doesExist
         Assert.Equal (100, predCount)


   module Find = 
      [<Fact>]
      let should_return_item_that_matches_predicate() =
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let arr = Array.init 100 id
         let mutable predCount = 0
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (arr.[nextI], item)
            nextI <- nextI + 1  
            predCount <- predCount + 1
            item = 3
         let matched = ll |> LazyList.find pred
         Assert.Equal (3, matched)
         Assert.Equal (4, !counter)

      [<Fact>]
      let should_throw_if_no_items_match_predicate() = 
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            LazyList.empty |> LazyList.find (fun _ -> false) |> ignore)) |> ignore


   module TryFind = 
      [<Fact>]
      let should_return_item_that_matches_predicate() =
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let arr = Array.init 100 id
         let mutable predCount = 0
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (arr.[nextI], item)
            nextI <- nextI + 1  
            predCount <- predCount + 1
            item = 3
         let matched = ll |> LazyList.tryFind pred
         Assert.Equal (3, matched.Value)
         Assert.Equal (4, !counter)

      [<Fact>]
      let should_return_none_if_no_items_match_predicate() = 
            Assert.True (LazyList.empty |> LazyList.tryFind (fun _ -> false) |> Option.isNone)


   module Pick = 
      [<Fact>]
      let should_return_picked_item() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let picked = ll |> LazyList.pick (fun i -> if i = 10 then Some "10" else None)
         Assert.Equal ("10", picked)
         Assert.Equal( 11, !counter)

      [<Fact>]
      let should_throw_if_no_item_is_picked() =
         let ll = LazyList.init 10 id
         Assert.Throws<KeyNotFoundException>(Action(fun () -> 
            ll |> LazyList.pick (fun i -> if i = 20 then Some "20" else None) |> ignore)) |> ignore


    module TryPick = 
      [<Fact>]
      let should_return_picked_item() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let picked = ll |> LazyList.tryPick (fun i -> if i = 10 then Some "10" else None)
         Assert.Equal ("10", picked.Value)
         Assert.Equal( 11, !counter)

      [<Fact>]
      let should_return_none_if_no_item_is_picked() =
         let ll = LazyList.init 10 id
         let picked = ll |> LazyList.tryPick (fun i -> if i = 20 then Some "20" else None)
         Assert.True picked.IsNone


   module Get = 
      [<Fact>]
      let should_get_item_at_index() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let item = ll |> LazyList.get 10
         Assert.Equal (10, item)
         Assert.Equal (11, !counter)

      [<Fact>]
      let should_throw_index_LT_0() = 
         let ll = LazyList.init 100 id
         Assert.Throws<IndexOutOfRangeException>(Action(fun () -> 
            ll |> LazyList.get -1 |> ignore)) |> ignore 

      [<Fact>]
      let should_throw_index_GTE_length() = 
         let ll = LazyList.init 100 id
         Assert.Throws<IndexOutOfRangeException>(Action(fun () -> 
            ll |> LazyList.get 100 |> ignore)) |> ignore 


   module ForAll = 
      [<Fact>]
      let should_return_true_when_all_items_match() =
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let arr = Array.init 100 id
         let mutable predCount = 0
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (arr.[nextI], item)
            nextI <- nextI + 1  
            predCount <- predCount + 1
            item >= 0
         let allGTE0 = ll |> LazyList.forall pred
         Assert.True allGTE0
         Assert.Equal (ll.Length(), predCount)
         Assert.Equal (ll.Length(), !counter)

      [<Fact>]
      let should_return_false_when_all_items_do_not_match() =
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let arr = Array.init 100 id
         let mutable predCount = 0
         let mutable nextI = 0
         let pred item = 
            Assert.Equal (arr.[nextI], item)
            nextI <- nextI + 1  
            predCount <- predCount + 1
            item < 10
         let allLessThan10 = ll |> LazyList.forall pred
         Assert.False allLessThan10
         Assert.Equal (11, predCount)
         Assert.Equal (11, !counter)


   module Iter = 
      [<Fact>]
      let should_apply_function_to_each_item_in_list() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let arr = Array.init 100 id
         let mutable nextI = 0
         let iterAction item =
            Assert.Equal (arr.[nextI], item) 
            nextI <- nextI + 1
         ll |> LazyList.iter iterAction
         Assert.Equal (ll.Length(), nextI)


   module Iteri = 
      [<Fact>]
      let should_apply_function_to_each_item_in_list() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let arr = Array.init 100 id
         let mutable nextI = 0
         let iterAction idx item =
            Assert.Equal (nextI, idx)
            Assert.Equal (arr.[idx], item) 
            nextI <- nextI + 1
         ll |> LazyList.iteri iterAction
         Assert.Equal (ll.Length(), nextI)


   module Append = 
      [<Fact>]
      let should_add_elements_from_list2_to_end_of_list1() =
         let counter1, l1Vals = wrapWithCounter id
         let ll1 = LazyList.init 10 l1Vals
         let counter2, l2Vals = wrapWithCounter (fun idx -> idx + 10)
         let ll2 = LazyList.init 10 l2Vals
         
         let ll = LazyList.append ll1 ll2

         Assert.Equal (0, !counter1)
         Assert.Equal (0, !counter2)
         let mutable expectedVal = 0
         LazyList.iter (fun i  -> 
            Assert.Equal (expectedVal, i)
            expectedVal <- expectedVal + 1
         ) ll
         Assert.Equal (expectedVal, 20)
         Assert.Equal (10, !counter1)
         Assert.Equal (10, !counter2)

   
   module Reverse = 
      [<Fact>]
      let should_reverse_items_in_list() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id

         let llRev = LazyList.rev ll
         Assert.Equal (0, !counter)

         Assert.Equal (99, llRev.Head)
         Assert.Equal (100, !counter)

         let mutable expectedVal = 99
         LazyList.iter (fun item  -> 
            Assert.Equal (expectedVal, item)
            expectedVal <- expectedVal - 1
         ) llRev

         Assert.Equal (100, !counter)


   module ToList =
      [<Fact>]
      let should_copy_elements_to_list() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let l = ll |> LazyList.toList
         Assert.Equal (ll.Length(), l.Length)
         Seq.zip l ll
         |> Seq.iter (fun (i1, i2) -> 
            Assert.Equal (i1, i2))

      [<Fact>]
      let should_return_empty_list_if_list_is_empty() = 
          let l = LazyList.toList LazyList.empty
          Assert.True l.IsEmpty 


   module ToArray =
      [<Fact>]
      let should_copy_elements_to_array() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 100 _id
         let arr = ll |> LazyList.toArray
         Assert.Equal (ll.Length(), arr.Length)
         let mutable idx = 0
         ll |> LazyList.iter (fun item -> 
            Assert.Equal (item, arr.[idx])
            idx <- idx + 1 )

      [<Fact>]
      let should_return_empty_array_if_list_is_empty() = 
          let arr = LazyList.toArray LazyList.empty
          Assert.NotNull arr
          Assert.Equal (0, arr.Length)


   module ActivePattern = 
      [<Fact>]
      let should_match_empty_node() = 
         let matched = 
            match LazyList.empty with
            | LazyList.Empty -> true
            | _ -> false
         Assert.True matched 

      [<Fact>]
      let should_match_cons_node() = 
         let counter, _id = wrapWithCounter id
         let ll = LazyList.init 10 _id
         let matched = 
            match ll with
            | LazyList.Cons (x, rest) -> Some (x, rest) 
            | _ -> None
         Assert.True matched.IsSome
         Assert.Equal (0, matched.Value |> fst) 
         Assert.Equal (1, !counter)
