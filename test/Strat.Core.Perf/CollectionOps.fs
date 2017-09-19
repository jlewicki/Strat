namespace Strat.Collections.Perf

open System
open System.Collections.Generic
open System.Diagnostics
open FSharpx.Collections
open Strat.Collections


module Ops =

   let numElements = 10000
   let sourceArray = Array.init numElements id 
  

   module Vector =
      let v = Vector.ofArray sourceArray

      let create() = 
            Vector.ofArray sourceArray |> ignore

      let item() = 
         let mutable i = 0
         while i < v.Count - 1 do
            let item = v.[i]
            i <- i + 1
       
      let iter() = 
         use e = (v :> IEnumerable<_>).GetEnumerator()
         while e.MoveNext() do ()

      let add() = 
         let mutable i = 0
         let mutable v = Vector.empty
         while i < sourceArray.Length - 1 do
            v <- v.Add sourceArray.[i]
            i <- i + 1

      let remove() =
         let mutable i = sourceArray.Length - 1
         let mutable v = v
         while i >= 0 do
            let _, newV = v.RemoveLast()
            v <- newV
            i <- i - 1

      let map() = 
         let f item = true
         v |> Vector.map f |> ignore

      let mapi() = 
         let f idx item = true
         v |> Vector.mapi f |> ignore

      let filter() = 
         let f item = true
         v |> Vector.filter f |> ignore

      let fold() = 
         let f state item = item
         v |> Vector.fold f 0 |> ignore


      let reverse() = 
         let f item = true
         v |> Vector.rev |> ignore


   module IndexList =
      let il = IndexList.ofArray sourceArray

      let create() = 
            IndexList.ofArray sourceArray |> ignore


      let item() = 
         let mutable i = 0
         while i < il.Count - 1 do
            let item = il.[i]
            i <- i + 1
       
      let iter() = 
         use e = (il :> IEnumerable<_>).GetEnumerator()
         while e.MoveNext() do ()

      let add() = 
         let mutable i = 0
         let mutable v = IndexList.empty
         while i < sourceArray.Length - 1 do
            v <- v.Cons sourceArray.[i]
            i <- i + 1

      let remove() =
         let mutable i = sourceArray.Length - 1
         let mutable il = il
         while i >= 0 do
            let _, newIL = il.RemoveHead()
            il <- newIL
            i <- i - 1

      let map() = 
         let f item = true
         il |> IndexList.map f |> ignore

      let mapi() = 
         let f idx item = true
         il |> IndexList.mapi f |> ignore

      let filter() = 
         let f item = true
         il |> IndexList.filter f |> ignore

      let fold() = 
         let f state item = item
         il |> IndexList.fold f 0 |> ignore


      let reverse() = 
         il |> IndexList.rev |> ignore


   module List = 
      let list = List.ofArray sourceArray
      let item() = 
         let mutable i = 0
         while i < sourceArray.Length - 1 do
            let item = list |> List.item i
            i <- i + 1

      let map() = 
         let f item = true
         list |> List.map f |> ignore

      let mapi() = 
         let f i item = true
         list |> List.mapi f |> ignore

      let filter() = 
         let f item = true
         list |> List.filter f |> ignore

      let fold() = 
         let f state item = item
         list |> List.fold f 0 |> ignore


      let reverse() = 
         list |> List.rev |> ignore


   module PersistentVector = 
      let pv = PersistentVector.ofSeq sourceArray
      let add() = 
         let mutable i = 0
         let mutable v = PersistentVector.empty
         while i < sourceArray.Length - 1 do
            v <- v.Conj sourceArray.[i]
            i <- i + 1

      let item() = 
         let mutable i = 0
         while i < sourceArray.Length - 1 do
            let item = pv |> PersistentVector.nth i
            i <- i + 1

      let map() = 
         let f item = true
         pv |> PersistentVector.map f |> ignore


      let reverse() = 
         let f item = true
         pv |> PersistentVector.rev |> ignore

      let fold() = 
         let f state item = item
         pv |> PersistentVector.fold f 0 |> ignore


   module Map = 
      let pairs = sourceArray |> Array.mapi (fun idx item -> sourceArray.Length - 1 - idx, item)

      let create() = 
         pairs |> Map.ofSeq |> ignore 


      let add() = 
         let mutable i = 0
         let mutable m = Map.empty
         while i < sourceArray.Length - 1 do
            m <- m.Add (sourceArray.Length - 1 - i, sourceArray.[i])
            i <- i + 1


   module PrioritySearchQueue = 
      let pairs = sourceArray |> Array.mapi (fun idx item -> sourceArray.Length - 1 - idx, item)
      let sortedPairs = pairs |> Array.sortBy fst

      //let psq = PrioritySearchQueue.ofSeq false pairs

      let createFromOrderedSeq() = 
         sortedPairs |> PrioritySearchQueue.ofOrderedSeq |> ignore 


      let add() = 
         let mutable i = 0
         let mutable v = PrioritySearchQueue.empty
         while i < sourceArray.Length - 1 do
            v <- v.Add (sourceArray.Length - 1 - i, sourceArray.[i])
            i <- i + 1