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
