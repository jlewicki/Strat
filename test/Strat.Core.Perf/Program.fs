// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open System.Diagnostics
open Strat.Collections


let numIterations = 5
let numElements = 100000
let sourceArray = Array.init numElements id 
let v = Vector.ofArray sourceArray


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
   
   



   
   0 // return an integer exit code
