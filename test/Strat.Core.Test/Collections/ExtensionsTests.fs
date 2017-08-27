namespace Strat.Collections.Test.Seq
open System
open Strat
open Strat.Collections
open Xunit

module FoldI =
   [<Fact>]
   let Should_Include_Index_In_Tuple() =
      let items = [(); (); ()]
      let indices = 
         items 
         |> Seq.foldi( fun indices (idx, _) ->
            idx :: indices) List.empty
      Assert.Equal<int>( [0; 1; 2], indices |> List.rev ) 


module Count =
   [<Fact>]
   let Should_Count_Items_Matching_Predicate() = 
      let items = {1 .. 10}
      let count = items |> Seq.countWhere (fun i -> i <= 4 )
      Assert.Equal( 4, count) 


module TryHead = 
   [<Fact>]
   let Should_Return_First_Item_In_Sequence() = 
      let result = {1..5} |> Seq.tryHead 
      Assert.True( result.IsSome)
      Assert.Equal( 1, result.Value )

   [<Fact>]
   let Should_Return_None_If_Sequence_Is_Empty() = 
      let result = Seq.empty |> Seq.tryHead 
      Assert.True( result.IsNone ) 


module ExceptWhen = 
   [<Fact>]
   let Should_Exclude_Items_That_Match_Predicate() =
      let evenNums =
         seq {1 .. 10}
         |> Seq.exceptWhere odd 

      Assert.Equal( 5, evenNums |> Seq.length )
      evenNums 
      |> Seq.zip [2;4;6;8;10]
      |> Seq.iter (fun (expected, actual) -> 
         Assert.Equal( expected, actual ) )


module FoldWhile = 
   [<Fact>]
   let Should_Return_State_When_Seq_Is_Empty() = 
      let sum = [] |> Seq.foldWhile (fun _ _ -> true) (fun sum i -> sum + i) 5
      Assert.Equal( 5, sum ) 

   [<Fact>] 
   let Should_Fold_While_Folder_Returns_Some() = 
      let sum =
         seq {1 .. 10}
         |> Seq.foldWhile (fun _ i -> i <= 5) (fun sum i ->  sum + i)  0
      Assert.Equal( 15, sum ) 


module Reverse = 
   [<Fact>]
   let Should_Return_Items_In_Reverse_Order() = 
      let nums = [| 1 .. 10 |]
      let reversed = Seq.reverse nums
      
      reversed 
      |> Seq.zip [| 10 .. 1 |]
      |> Seq.iter (fun (expected, actual) -> 
         Assert.Equal( expected, actual ) )


module ReplaceHead = 

   [<Fact>]
   let Should_Replace_Head() = 
      let items = [1; 2; 3;] |> Seq.replaceHead 4
      Assert.Equal<int>( [4; 2; 3;], items |> List.ofSeq)

   [<Fact>]
   let Should_Throw_WhenList_Is_Empty() = 
       Assert.Throws<ArgumentException>( fun() -> Seq.empty |> Seq.replaceHead 4 |> ignore )
     

namespace Strat.Core.Collections.Test.Map
open System
open Strat.Collections
open Xunit

module Update = 
   [<Fact>]
   let Should_Update_Map() = 
      let map = 
         Map.empty 
         |> Map.add "foo" "bar"
         |> Map.update "foo" (fun value -> value.ToUpper())
      Assert.Equal<string>( "BAR", map |> Map.find "foo" )


module AddOrUpdate = 
   [<Fact>]
   let Should_Add_Item_If_Key_Is_Not_Present() = 
      let map = 
         Map.empty
         |> Map.addOrUpdate "foo" (function
            | None -> "bar"
            | _ -> invalidOp "Should not get here")
      Assert.Equal( 1, map.Count )
      Assert.True( Map.containsKey "foo" map )
      Assert.Equal<string>( "bar", (Map.find "foo" map) )

   [<Fact>]
   let Should_Update_Item_If_Key_Is_Present() = 
      let map = 
         Map.empty
         |> Map.add "foo" "ba"
         |> Map.addOrUpdate "foo" (function
            | Some(value) -> value + "r"
            | _ -> invalidOp "Should not get here")
      Assert.Equal( 1, map.Count )
      Assert.True( Map.containsKey "foo" map )
      Assert.Equal<string>( "bar", (Map.find "foo" map) )


module TryUpdate = 
   [<Fact>]
   let Should_Update_Map_If_Key_Exists() = 
      let map = Map.empty |> Map.add 1 "foo"

      let updated, map = map |> Map.tryUpdate 1 (fun v -> v.ToUpper())

      Assert.True updated
      Assert.Equal<string>("FOO", map |> Map.find 1)

   [<Fact>]
   let Should_Update_Map_If_Key_Does_Not_Exist() = 
      let origMap = Map.empty |> Map.add 1 "foo"

      let updated, map = origMap |> Map.tryUpdate 4 (fun v -> v.ToUpper())

      Assert.False updated
      Assert.Same(origMap, map)


namespace Strat.Core.Collections.Test.List
open System
open Strat.Collections
open Xunit

module WithoutFirst = 
   
   [<Fact>]
   let Should_Remove_First_Occurence_Of_Item() = 
      let items = [1; 2; 3; 3; 4; 5]
      let items = items |> List.withoutFirst 3
      Assert.Equal<int>( [1; 2; 3; 4; 5], items )

   [<Fact>]
   let Should_Return_Original_List_If_Value_Not_Found() = 
      let items = [1; 2; 3; 4; 5]
      let items = items |> List.withoutFirst 6
      Assert.Equal<int>( [1; 2; 3; 4; 5], items )


module ReplaceHead = 

   [<Fact>]
   let Should_Replace_Head() = 
      let items = [1; 2; 3;] |> List.replaceHead 4
      Assert.Equal<int>( [4; 2; 3;], items )

   [<Fact>]
   let Should_Throw_WhenList_Is_Empty() = 
       Assert.Throws<ArgumentException>( fun() -> List.Empty |> List.replaceHead 4 |> ignore )


module ReplaceLast = 
   
   [<Fact>]
   let Should_Replace_Last() = 
      let items = [1; 2; 3;] |> List.replaceLast 4
      Assert.Equal<int>( [1; 2; 4;], items )


   [<Fact>]
   let Should_Throw_WhenList_Is_Empty() = 
       Assert.Throws<ArgumentException>( fun() -> List.Empty |> List.replaceLast 4 |> ignore )
      