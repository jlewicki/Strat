namespace Strat.Collections.Perf

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Attributes.Jobs
open Strat.Collections
open Common

module FSharpxLazyList = global.FSharpx.Collections.LazyList


[<MemoryDiagnoser>]
[<SimpleJob(launchCount=1, warmupCount=2, targetCount=2)>]
type LazyListBenchmarks() =
   
   let forcedLazyList = LazyList.init 1000 id
   do forcedLazyList |> Seq.iter ignore

   let forcedFSharpxLazyList = FSharpxLazyList.unfold (fun idx -> if idx < 1000 then Some(idx, idx + 1) else None) 0
   do forcedFSharpxLazyList |> Seq.iter ignore

   [<Benchmark>]
   member this.Unfold_And_Iter_With_LazyList() = 
      let mutable ll = LazyList.unfold (fun idx -> if idx < 1000 then Some(idx, idx + 1) else None) 0
      let iterator = (ll |> LazyList.toSeq).GetEnumerator()
      let mutable item = ll.Head
      while iterator.MoveNext() do
         item <- iterator.Current
      item


   [<Benchmark>]
   member this.Unfold_And_Iter_With_FSharpxLazyList() = 
      let mutable ll = FSharpxLazyList.unfold (fun idx -> if idx < 1000 then Some(idx, idx + 1) else None) 0
      let iterator = (ll |> FSharpxLazyList.toSeq).GetEnumerator()
      let mutable item = ll.Head
      while iterator.MoveNext() do
         item <- iterator.Current
      item


   [<Benchmark>]
   member this.Unfold_And_Iter_With_List() = 
      let mutable l = List.unfold (fun idx -> if idx < 1000 then Some(idx, idx + 1) else None) 0
      let iterator = (l |> List.toSeq).GetEnumerator()
      let mutable item = l.Head
      while iterator.MoveNext() do
         item <- iterator.Current
      item


   [<Benchmark>]
   member this.Iter_With_LazyList() = 
      let iterator = (forcedLazyList |> LazyList.toSeq).GetEnumerator()
      let mutable item = forcedLazyList.Head
      while iterator.MoveNext() do
         item <- iterator.Current
      item

   [<Benchmark>]
   member this.Iter_With_FSharpxLazyList() = 
      let iterator = (forcedFSharpxLazyList |> FSharpxLazyList.toSeq).GetEnumerator()
      let mutable item = forcedLazyList.Head
      while iterator.MoveNext() do
         item <- iterator.Current
      item


   