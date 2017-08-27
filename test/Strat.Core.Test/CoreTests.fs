namespace Strat.Core.Test.Tuple2

open Strat
open Xunit

module Swap = 
   [<Fact>]
   let Should_Swap_Items() = 
      let item1, item2 = Tuple2.swap (1, 2)
      Assert.Equal( 2, item1)
      Assert.Equal( 1, item2)


module Map = 
   [<Fact>]
   let Should_Apply_Function_To_Items() =
      let fMap v = v * 3 
      let item1, item2 = Tuple2.map fMap (1, 2)
      Assert.Equal( 3, item1)
      Assert.Equal( 6, item2)


module Nth = 
   [<Fact>]
   let Should_Extract_Nth_Item() =
      let tuple = 1, 2
      Assert.Equal( 1, Tuple2.nth 0 tuple)
      Assert.Equal( 2, Tuple2.nth 1 tuple)


module MapNth = 
   [<Fact>]
   let Should_Apply_Function_To_Nth_Item() =
      let tuple = 1, 2
      let fMap v = v * 3 
      Assert.Equal<obj>( (3, 2), Tuple2.mapNth 0 fMap tuple)
      Assert.Equal<obj>( (1, 6), Tuple2.mapNth 1 fMap tuple)