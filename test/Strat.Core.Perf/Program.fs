// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open System.Diagnostics
open FSharpx.Collections
open Strat.Collections
open Strat.Collections.Perf


let numIterations = 5


[<EntryPoint>]
let main argv =

   // let asyncList = List.init 1000 async.Return
   // let syncList = List.init 1000 id
   // let lastAsync = async {
   //    let mutable last = -1
   //    for a in asyncList do
   //       let! i = a
   //       last <- i
   //    return last
   // }

   // let lastSync = async {
   //    let mutable last = -1
   //    for i in syncList do
   //       last <- i
   //    return last
   // }
   
   // Async.RunSynchronously lastAsync |> ignore  
   // let sw = Stopwatch.StartNew()
   // let res = Async.RunSynchronously lastAsync   
   // sw.Stop()     
   // printfn "Asyncs: %A ms" sw.Elapsed.TotalMilliseconds

   // Async.RunSynchronously lastSync |> ignore
   // let sw = Stopwatch.StartNew()
   // let res = Async.RunSynchronously lastSync   
   // sw.Stop()     
   // printfn "Syncs: %A ms" sw.Elapsed.TotalMilliseconds


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

   Ops.PersistentVector.item()
   let sw = Stopwatch.StartNew()  
   Ops.PersistentVector.item |> repeat
   sw.Stop()     
   printfn "PersistentVector Item: %A ms" sw.Elapsed.TotalMilliseconds

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

   
   // Ops.IndexedList.item()
   // let sw = Stopwatch.StartNew()  
   // Ops.IndexedList.item |> repeat
   // sw.Stop()     
   // printfn "IndexedList Item: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.IndexedList.map()
   // let sw = Stopwatch.StartNew()
   // Ops.IndexedList.map |> repeat
   // sw.Stop()     
   // printfn "IndexedList Map: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.IndexedList.filter()
   // let sw = Stopwatch.StartNew()  
   // Ops.IndexedList.filter |> repeat
   // sw.Stop()     
   // printfn "IndexedList Filter: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.IndexedList.fold()
   // let sw = Stopwatch.StartNew()
   // Ops.IndexedList.fold |> repeat
   // sw.Stop()     
   // printfn "IndexedList Fold: %A ms" sw.Elapsed.TotalMilliseconds

   
  


   
   0 // return an integer exit code
