// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open System.Diagnostics
open BenchmarkDotNet.Running
open FSharpx.Collections
open Strat.Collections
open Strat.Collections.Perf


let numIterations = 20


[<EntryPoint>]
let main argv =

   // let repeat action = 
   //    for i in [1..numIterations] do
   //       action()
   
   // printfn "\nVector ******************************************"
   // Ops.Vector.create()
   // let sw = Stopwatch.StartNew()
   // Ops.Vector.create |> repeat
   // sw.Stop()     
   // printfn "Create vector: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.Vector.item()
   // let sw = Stopwatch.StartNew()  
   // Ops.Vector.item |> repeat
   // sw.Stop()     
   // printfn "Vector Item: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.Vector.add()
   // let sw = Stopwatch.StartNew()
   // Ops.Vector.add |> repeat
   // sw.Stop  |> repeat   
   // printfn "Vector Add: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.Vector.iter()
   // let sw = Stopwatch.StartNew()
   // Ops.Vector.iter |> repeat
   // sw.Stop()     
   // printfn "Vector Enumerate: %A ms" sw.Elapsed.TotalMilliseconds
   
   // Ops.Vector.remove()
   // let sw = Stopwatch.StartNew()
   // Ops.Vector.remove |> repeat
   // sw.Stop()     
   // printfn "Vector Remove: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.Vector.map()
   // let sw = Stopwatch.StartNew()
   // Ops.Vector.map |> repeat
   // sw.Stop()     
   // printfn "Vector Map: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.Vector.mapi()
   // let sw = Stopwatch.StartNew()
   // Ops.Vector.mapi |> repeat
   // sw.Stop()     
   // printfn "Vector MapIndexed: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.Vector.fold()
   // let sw = Stopwatch.StartNew()
   // Ops.Vector.fold |> repeat
   // sw.Stop()     
   // printfn "Vector Fold: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.Vector.filter()
   // let sw = Stopwatch.StartNew()
   // Ops.Vector.filter |> repeat
   // sw.Stop()     
   // printfn "Vector Filter: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.Vector.reverse()
   // let sw = Stopwatch.StartNew()
   // Ops.Vector.reverse |> repeat
   // sw.Stop()     
   // printfn "Vector Reverse: %A ms" sw.Elapsed.TotalMilliseconds


   // printfn "\nList ******************************************"
   // Ops.List.map()
   // let sw = Stopwatch.StartNew()
   // Ops.List.map |> repeat
   // sw.Stop()     
   // printfn "List Map: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.List.mapi()
   // let sw = Stopwatch.StartNew()
   // Ops.List.mapi |> repeat
   // sw.Stop()     
   // printfn "List MapIndexed: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.List.fold()
   // let sw = Stopwatch.StartNew()
   // Ops.List.fold |> repeat
   // sw.Stop()     
   // printfn "List Fold: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.List.filter()
   // let sw = Stopwatch.StartNew()
   // Ops.List.filter |> repeat
   // sw.Stop()     
   // printfn "List Filter: %A ms" sw.Elapsed.TotalMilliseconds  

   // Ops.List.reverse()
   // let sw = Stopwatch.StartNew()
   // Ops.List.reverse |> repeat
   // sw.Stop()     
   // printfn "List Reverse: %A ms" sw.Elapsed.TotalMilliseconds   


   // printfn "\nPersistentVector ******************************************"
   // Ops.PersistentVector.item()
   // let sw = Stopwatch.StartNew()  
   // Ops.PersistentVector.item |> repeat
   // sw.Stop()     
   // printfn "PersistentVector Item: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.PersistentVector.add()
   // let sw = Stopwatch.StartNew()
   // Ops.PersistentVector.add |> repeat
   // sw.Stop  |> repeat   
   // printfn "PersistentVector Add: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.PersistentVector.map()
   // let sw = Stopwatch.StartNew()
   // Ops.PersistentVector.map |> repeat
   // sw.Stop()     
   // printfn "PersistentVector Map: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.PersistentVector.fold()
   // let sw = Stopwatch.StartNew()
   // Ops.PersistentVector.fold |> repeat
   // sw.Stop()     
   // printfn "PersistentVector Fold: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.PersistentVector.reverse()
   // let sw = Stopwatch.StartNew()
   // Ops.PersistentVector.reverse |> repeat
   // sw.Stop()     
   // printfn "PersistentVector Reverse: %A ms" sw.Elapsed.TotalMilliseconds
   

   // printfn "\nIndexList ******************************************"
   // Ops.IndexList.item()
   // let sw = Stopwatch.StartNew()  
   // Ops.IndexList.item |> repeat
   // sw.Stop()     
   // printfn "IndexedList Item: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.IndexList.add()
   // let sw = Stopwatch.StartNew()
   // Ops.IndexList.add |> repeat
   // sw.Stop  |> repeat   
   // printfn "IndexList Add: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.IndexList.map()
   // let sw = Stopwatch.StartNew()
   // Ops.IndexList.map |> repeat
   // sw.Stop()     
   // printfn "IndexedList Map: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.IndexList.filter()
   // let sw = Stopwatch.StartNew()  
   // Ops.IndexList.filter |> repeat
   // sw.Stop()     
   // printfn "IndexedList Filter: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.IndexList.fold()
   // let sw = Stopwatch.StartNew()
   // Ops.IndexList.fold |> repeat
   // sw.Stop()     
   // printfn "IndexedList Fold: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.IndexList.reverse()
   // let sw = Stopwatch.StartNew()
   // Ops.IndexList.reverse |> repeat
   // sw.Stop()     
   // printfn "IndexList Reverse: %A ms" sw.Elapsed.TotalMilliseconds
   

   // printfn "\nMap ******************************************"
   // Ops.Map.create()
   // let sw = Stopwatch.StartNew()
   // Ops.Map.create |> repeat
   // sw.Stop()     
   // printfn "Map Create: %A ms" sw.Elapsed.TotalMilliseconds
   
   // Ops.Map.add()
   // let sw = Stopwatch.StartNew()
   // Ops.Map.add |> repeat
   // sw.Stop()     
   // printfn "Map Add: %A ms" sw.Elapsed.TotalMilliseconds


   // printfn "\nPrioritySearchQueue ******************************************"
   // Ops.PrioritySearchQueue.createFromOrderedSeq()
   // let sw = Stopwatch.StartNew()
   // Ops.PrioritySearchQueue.createFromOrderedSeq |> repeat
   // sw.Stop()     
   // printfn "PrioritySearchQueue Create from ordered seq: %A ms" sw.Elapsed.TotalMilliseconds

   // Ops.PrioritySearchQueue.add()
   // let sw = Stopwatch.StartNew()
   // Ops.PrioritySearchQueue.add |> repeat
   // sw.Stop()     
   // printfn "PrioritySearchQueue Add: %A ms" sw.Elapsed.TotalMilliseconds
   
   // Console.WriteLine("\nPress Enter to exit")
   // Console.ReadLine() |> ignore
 
   let switcher = 
      new BenchmarkSwitcher(
         [|
            typeof<PrioritySearchQueueBenchmarks>
         |])
   switcher.Run(argv) |> ignore
   0

  