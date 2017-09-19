namespace Strat.Collections.Perf

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Attributes.Jobs
open Strat.Collections
open Common


[<MemoryDiagnoser>]
[<SimpleJob(launchCount=1, warmupCount=2, targetCount=2)>]
type PrioritySearchQueueBenchmarks() =
   let q = PrioritySearchQueue.ofArray sourceIntPairs
   let map = Map.ofArray sourceIntPairs
   let targetKeys = 
      [|0; sourceIntPairsByKey.Length / 2; sourceIntPairsByKey.Length - 1|]
      |> Array.map (fun idx -> sourceIntPairsByKey.[idx] |> fst)
      

//    [<Benchmark>]
//    member this.Add() = 
//       let mutable i = 0
//       let mutable v = PrioritySearchQueue.empty
//       while i < sourceIntArray.Length - 1 do
//          v <- v.Add (sourceIntArray.Length - 1 - i, sourceIntArray.[i])
//          i <- i + 1


//    [<Benchmark>]
//    member this.Remove() =
//       let mutable q = q
//       sourceIntPairs |> Array.iter (fun (k,v) ->
//          q <- q |> PrioritySearchQueue.remove k)


//    [<Benchmark>]
//    member this.RemoveHead() =
//       let mutable q = q
//       while not (q.IsEmpty) do
//          let struct(_, _, rest) =  q |> PrioritySearchQueue.removeHead
//          q <- rest
//       q


 


//    [<Benchmark>]
//    member this.Indexer() =  
//       let mutable i = 0
//       while i < sourceIntPairs.Length - 1 do
//          let k, v = sourceIntPairs.[i]
//          let item = q.[k]
//          i <- i + 1
//       i


//    [<Benchmark>]
//    member this.Iter() =  
//       let mutable s = 0
//       q |> PrioritySearchQueue.iter (fun k v -> s <- s + 1 )
//       s


//    [<Benchmark>]
//    member this.Exists() =
//       let mutable s = 0
//       for i = 0 to targetKeys.Length - 1 do
//          let targetk = targetKeys.[i]
//          q |> PrioritySearchQueue.exists (fun k v -> k = targetk ) |> ignore
//       s
      

//    [<Benchmark>]
//    member this.Pick() = 
//       let mutable s = 0
//       for i = 0 to targetKeys.Length - 1 do
//          let targetk = targetKeys.[i]
//          q |> PrioritySearchQueue.tryPick (fun k v -> if k = targetk then Some(targetk) else None ) |> ignore
//       s


   [<Benchmark>]
   member this.Map() =
      q |> PrioritySearchQueue.map (fun k v -> -v )


   [<Benchmark>]
   member this.Filter() = 
      q |> PrioritySearchQueue.filter (fun k v -> v % 2 = 0 )

    
   [<Benchmark>]
   member this.MapFilter() = 
      map |> Map.filter  (fun k v -> v % 2 = 0 )
      


   // [<Benchmark>]
   // member this.MapAdd() =    
   //    let mutable i = 0
   //    let mutable m = Map.empty
   //    while i < sourceIntPairs.Length - 1 do
   //       m <- m.Add (sourceIntArray.Length - 1 - i, sourceIntArray.[i])
   //       i <- i + 1


   // [<Benchmark>]
   // member this.MapIndexer() =  
   //    let mutable i = 0
   //    while i < sourceIntPairs.Length - 1 do
   //       let k, v = sourceIntPairs.[i]
   //       let item = map.[k]
   //       i <- i + 1


   // [<Benchmark>]
   // member this.MapRemove() =
   //    let mutable map = map
   //    sourceIntPairs |> Array.iter (fun (k,v) ->
   //       map <- map |> Map.remove k)


   [<Benchmark>]
   member this.MapMap() =
      map |> Map.map (fun k v -> -v )