namespace Strat.Collections.Test

open System
open System.Collections.Generic
open Strat.Collections
open Xunit

module Queue = 

   module Empty = 
      [<Fact>]
      let should_have_size_0() = 
         Assert.Equal(0, Queue.empty.Count)

      [<Fact>]
      let should_be_empty() = 
         Assert.True Queue.empty.IsEmpty

      [<Fact>]
      let should_yield_no_values_from_enumerator() =
         let list = List.ofSeq Queue.empty
         Assert.True list.IsEmpty


   module OfSeq = 
      [<Fact>]
      let should_create_queue_containing_items_in_seq() = 
         let items = seq { for i = 1 to 10 do yield i * 2 }
         let mutable q = Queue.ofSeq items
         Assert.Equal (items |> Seq.length, q |> Queue.length)
         items |> Seq.iter (fun item ->
            let struct (head, rest) = q |> Queue.dequeue
            Assert.Equal (item, head)
            q <- rest)
      
      [<Fact>]
      let should_create_empty_queue_for_empty_seq() = 
         let q = Queue.ofSeq Seq.empty
         Assert.True q.IsEmpty


   module OfList = 
      [<Fact>]
      let should_create_queue_containing_items_in_list() = 
         let items = List.init 100 id
         let mutable q = Queue.ofList items
         Assert.Equal (items |> Seq.length, q |> Queue.length)
         items |> Seq.iter (fun item ->
            let struct (head, rest) = q |> Queue.dequeue
            Assert.Equal (item, head)
            q <- rest)
      
      [<Fact>]
      let should_create_empty_queue_for_empty_list() = 
         let q = Queue.ofList List.empty
         Assert.True q.IsEmpty

   
   module OfArray = 
      [<Fact>]
      let should_create_queue_containing_items_in_array() = 
         let items = Array.init 100 id
         let mutable q = Queue.ofArray items
         Assert.Equal (items |> Seq.length, q |> Queue.length)
         items |> Seq.iter (fun item ->
            let struct (head, rest) = q |> Queue.dequeue
            Assert.Equal (item, head)
            q <- rest)
      
      [<Fact>]
      let should_create_empty_queue_for_empty_array() = 
         let q = Queue.ofArray Array.empty
         Assert.True q.IsEmpty


   module Length = 
      [<Fact>]
      let should_return_number_of_items_in_queue() =
         let items = Array.init 1000 id
         let q = Queue.ofArray items
         Assert.Equal (items.Length, q |> Queue.length)

      [<Fact>]
      let should_return_0_for_empty_queue() = 
         Assert.Equal(0, Queue.empty.Count)


   module Enqueue = 
      [<Fact>]
      let should_enqueue_item_at_back_of_queue() = 
         let items = Array.init 10 id
         let mutable q = items |> Array.fold (fun q item -> q |> Queue.enqueue item ) Queue.empty
         Assert.Equal (items.Length, q.Count)
         for i = 0 to items.Length - 1 do
            let struct (head, rest) = q |> Queue.dequeue
            Assert.Equal (items.[i], head)
            q <- rest


    module Dequeue = 
      [<Fact>]
      let should_dequeue_item_at_front_of_queue() = 
         let items = Array.init 10 id
         let mutable q = Queue.ofArray items
         Assert.Equal (items.Length, q.Count)
         for i = 0 to items.Length - 1 do
            let struct (head, rest) = q |> Queue.dequeue
            Assert.Equal (items.[i], head)
            q <- rest

      [<Fact>]
      let should_throw_if_queue_is_empty() =
          Assert.Throws<InvalidOperationException>(Action(fun () -> 
            Queue.Empty |> Queue.dequeue |> ignore)) |> ignore


   module Head = 
      [<Fact>]
      let should_return_item_at_front_of_queue() = 
         let items =[|3;5;2;1;5|]
         let q = items |> Array.fold (fun q item ->q |> Queue.enqueue item ) Queue.empty
         Assert.Equal (3, q.Head)

      [<Fact>]
      let should_throw_if_queue_is_empty() =
          Assert.Throws<InvalidOperationException>(Action(fun () -> 
            Queue.Empty |> Queue.head |> ignore)) |> ignore
