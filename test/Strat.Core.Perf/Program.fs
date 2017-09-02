// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open System.Diagnostics
open FSharpx.Collections
open Strat.Collections


let numIterations = 5
let numElements = 100000
let sourceArray = Array.init numElements id 
let v = Vector.ofArray sourceArray
let list = List.ofArray sourceArray
let pv = PersistentVector.ofSeq sourceArray


module Ops =
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



[<EntryPoint>]
let main argv =

   let repeat action = 
      for i in [1..numIterations] do
         action()
   
   Ops.Vector.create()
   let sw = Stopwatch.StartNew()
   Ops.Vector.create |> repeat
   sw.Stop()     
   printfn "Create vector: %A ms" sw.Elapsed.TotalMilliseconds

   Ops.Vector.item()
   let sw = Stopwatch.StartNew()  
   Ops.Vector.item |> repeat
   sw.Stop()     
   printfn "Item: %A ms" sw.Elapsed.TotalMilliseconds

   Ops.Vector.add()
   let sw = Stopwatch.StartNew()
   Ops.Vector.add |> repeat
   sw.Stop  |> repeat   
   printfn "Add: %A ms" sw.Elapsed.TotalMilliseconds

   Ops.Vector.iter()
   let sw = Stopwatch.StartNew()
   Ops.Vector.iter |> repeat
   sw.Stop()     
   printfn "Enumerate: %A ms" sw.Elapsed.TotalMilliseconds
   
   Ops.Vector.remove()
   let sw = Stopwatch.StartNew()
   Ops.Vector.remove |> repeat
   sw.Stop()     
   printfn "Remove: %A ms" sw.Elapsed.TotalMilliseconds

   Ops.Vector.map()
   let sw = Stopwatch.StartNew()
   Ops.Vector.map |> repeat
   sw.Stop()     
   printfn "Map: %A ms" sw.Elapsed.TotalMilliseconds

   Ops.Vector.mapi()
   let sw = Stopwatch.StartNew()
   Ops.Vector.mapi |> repeat
   sw.Stop()     
   printfn "MapIndexed: %A ms" sw.Elapsed.TotalMilliseconds

   Ops.Vector.fold()
   let sw = Stopwatch.StartNew()
   Ops.Vector.fold |> repeat
   sw.Stop()     
   printfn "Fold: %A ms" sw.Elapsed.TotalMilliseconds

   Ops.Vector.filter()
   let sw = Stopwatch.StartNew()
   Ops.Vector.filter |> repeat
   sw.Stop()     
   printfn "Filter: %A ms" sw.Elapsed.TotalMilliseconds


   // Ops.List.item()
   // let sw = Stopwatch.StartNew()
   // Ops.List.item |> repeat
   // sw.Stop()     
   // printfn "List Item: %A ms" sw.Elapsed.TotalMilliseconds

   Ops.List.map()
   let sw = Stopwatch.StartNew()
   Ops.List.map |> repeat
   sw.Stop()     
   printfn "List Map: %A ms" sw.Elapsed.TotalMilliseconds

   Ops.List.mapi()
   let sw = Stopwatch.StartNew()
   Ops.List.mapi |> repeat
   sw.Stop()     
   printfn "List MapIndexed: %A ms" sw.Elapsed.TotalMilliseconds


   Ops.List.fold()
   let sw = Stopwatch.StartNew()
   Ops.List.fold |> repeat
   sw.Stop()     
   printfn "List Fold: %A ms" sw.Elapsed.TotalMilliseconds

   Ops.List.filter()
   let sw = Stopwatch.StartNew()
   Ops.List.filter |> repeat
   sw.Stop()     
   printfn "List Filter: %A ms" sw.Elapsed.TotalMilliseconds   


   Ops.PersistentVector.map()
   let sw = Stopwatch.StartNew()
   Ops.PersistentVector.map |> repeat
   sw.Stop()     
   printfn "PersistentVector Map: %A ms" sw.Elapsed.TotalMilliseconds


   Ops.PersistentVector.fold()
   let sw = Stopwatch.StartNew()
   Ops.PersistentVector.fold |> repeat
   sw.Stop()     
   printfn "PersistentVector Fold: %A ms" sw.Elapsed.TotalMilliseconds

   Ops.PersistentVector.item()
   let sw = Stopwatch.StartNew()  
   Ops.PersistentVector.item |> repeat
   sw.Stop()     
   printfn "PersistentVector Item: %A ms" sw.Elapsed.TotalMilliseconds

  


   
   0 // return an integer exit code
