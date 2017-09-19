namespace Strat.Collections.Perf

module Common = 
   let numElements = 10000
   let sourceIntArray = Array.init numElements id
   let sourceIntPairs = sourceIntArray |> Array.mapi (fun idx item -> sourceIntArray.Length - 1 - idx, item)
   let sourceIntPairsByKey = sourceIntPairs |> Array.sortBy fst
   let strings = Array.init numElements (fun i -> i.ToString()) 