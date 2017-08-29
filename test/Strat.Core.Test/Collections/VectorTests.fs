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


   module Iter = 
      [<Fact>]
      let should_apply_function_to_each_item_in_vector() = 
         let v = Vector.ofArray largeArray
         let mutable nextI = 0
         v |> Vector.iter (fun item ->
            Assert.Equal (v.[nextI], item) 
            nextI <- nextI + 1 )
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