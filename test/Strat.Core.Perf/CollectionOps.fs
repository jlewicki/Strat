namespace Strat.Collections.Perf

open System
open System.Collections.Generic
open System.Diagnostics
open FSharpx.Collections
open Strat.Collections


module Ops =

   let numElements = 100000
   let sourceArray = Array.init numElements id 
   let v = Vector.ofArray sourceArray
   let list = List.ofArray sourceArray
   //let ral = RandomAccessList.ofArray sourceArray
   let pv = PersistentVector.ofSeq sourceArray

   module Vector =
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


   module List = 
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


   // module IndexedList = 
    
   //    let item() = 
   //       let mutable i = 0
   //       while i < sourceArray.Length - 1 do
   //          let item = ral |> RandomAccessList.get i
   //          i <- i + 1

   //    let map() = 
   //       let f item = true
   //       ral |> RandomAccessList.map f |> ignore

   //    let mapi() = 
   //       let f i item = true
   //       ral |> RandomAccessList.mapi f |> ignore

   //    let filter() = 
   //       let f item = true
   //       ral |> RandomAccessList.filter f |> ignore

   //    let fold() = 
   //       let f state item = item
   //       ral |> RandomAccessList.fold f 0 |> ignore



   module PersistentVector = 
      let item() = 
         let mutable i = 0
         while i < sourceArray.Length - 1 do
            let item = pv |> PersistentVector.nth i
            i <- i + 1

      let map() = 
         let f item = true
         pv |> PersistentVector.map f |> ignore

      let fold() = 
         let f state item = item
         pv |> PersistentVector.fold f 0 |> ignore