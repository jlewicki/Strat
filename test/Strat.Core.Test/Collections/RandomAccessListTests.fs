namespace Strat.Collections.Test.RandomAccessList
open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Runtime.Serialization
//open System.Runtime.Serialization.Formatters.Binary
open Strat.Collections
open Xunit

module RAL = Strat.Collections.RandomAccessList

type Head() = 
   [<Fact>]
   member x.Should_Throw_For_An_Empty_List() = 
      Assert.Throws<InvalidOperationException>( fun() -> 
         RAL.empty |> RAL.head |> ignore )

   [<Fact>]
   member x.Should_Return_Head_Of_List() = 
      let items = { 10..-1..1 }
      let head = items |> RAL.ofSeq |> RAL.head
      Assert.Equal( 10, head )

type Tail() = 
   [<Fact>]
   member x.Should_Throw_For_An_Empty_List() = 
      Assert.Throws<InvalidOperationException>( fun() -> 
         RAL.empty |> RAL.tail |> ignore )

   [<Fact>]
   member x.Should_Return_Tail_Of_List() = 
      let items = { 10..-1..1 }
      let tail = items |> RAL.ofSeq |> RAL.tail
      Assert.Equal( 9, tail |> RAL.head )
      Assert.Equal( 9, tail |> RAL.length )

type Cons() = 
   [<Fact>]
   member x.Should_Prepend_Element_To_Empty_List() = 
      let newList = RAL.empty |> RAL.cons 1
      Assert.Equal( 1, newList.Count )
      Assert.Equal( 1, newList.Head )
     
   [<Fact>]
   member x.Should_Prepend_Element_To_Non_Empty_List() = 
      seq{ 1 .. 10 }
      |> Seq.fold (fun (list : RandomAccessList<int>) i ->
         let newList = RAL.cons i list 
         // Check that the head of the new list has the correct value
         Assert.Equal( i, newList.Head)
         // Check the tail of the new list matches the previous list
         Seq.zip newList.Tail list
         |> Seq.iter( fun (newItem, item) -> Assert.Equal(newItem, item))
         newList ) RAL.empty

type Get() = 
   [<Fact>]
   member x.Should_Return_Item_At_Index_i() =
      let items = { 0..9 }
      let list = items |> RAL.ofSeq
      items
      |> Seq.iter( fun i -> Assert.Equal( i, list |> RAL.get (i) ) )

   [<Fact>]
   member x.Should_Throw_If_Index_Out_Of_Range() =
      let list = { 1..10 } |> RAL.ofSeq
      Assert.Throws<ArgumentOutOfRangeException>( fun() -> 
         list |> RAL.get -1 |> ignore ) |> ignore
      Assert.Throws<ArgumentOutOfRangeException>( fun() -> 
         list |> RAL.get 11 |> ignore )

type Set() = 
   [<Fact>]
   member x.Should_Set_Item_At_Index_i() = 
      let items = { 0..9 }
      let list = items |> RAL.ofSeq |> RAL.set 5 100
      items
      |> Seq.iter( fun i -> 
         if i = 5 then Assert.Equal( 100, list |> RAL.get (i) )
         else Assert.Equal( i, list |> RAL.get (i) ) )

   [<Fact>]
   member x.Should_Throw_If_Index_Out_Of_Range() =
      let list = { 1..10 } |> RAL.ofSeq
      Assert.Throws<ArgumentOutOfRangeException>( fun() -> 
         list |> RAL.set -1 20 |> ignore ) |> ignore
      Assert.Throws<ArgumentOutOfRangeException>( fun() -> 
         list |> RAL.set 11 20 |> ignore )

type Empty() = 
   [<Fact>]
   member x.Should_Be_Empty() = 
      Assert.Equal( 0, RAL.empty |> RAL.length )
      Assert.True( RAL.empty |> RAL.isEmpty )

type Contains() = 
   [<Fact>]
   member x.Should_Return_True_If_Item_Is_In_List() =
      let items = seq { 1..10 }
      let list = items |> RAL.ofSeq
      items
      |> Seq.iter( fun i -> 
         Assert.True( list |> RAL.contains i  ) )

   [<Fact>]
   member x.Should_Return_False_If_Item_Is_Not_In_List() =
      let list = seq { 1..5 } |> RAL.ofSeq 
      Assert.False( list |> RAL.contains 0  )
      Assert.False( list |> RAL.contains 6 )

type Singleton() = 
   [<Fact>]
   member x.Should_Return_List_With_One_Item() = 
     let list = RAL.singleton 3
     Assert.Equal( 1, list |> RAL.length )
     Assert.Equal( 3, list |> RAL.get 0 )
     Assert.Equal( 3, list |> RAL.head )
     Assert.True( list |> RAL.contains 3  )

type Iter() = 
   [<Fact>]
   member x.Function_Should_Be_Called_Once_For_Each_Item_In_List() = 
      let items = seq { 1..5 }
      let pq = RAL.ofSeq items
      let calledItems = ref Set.empty
      pq |> RAL.iter( fun i -> calledItems := Set.add i !calledItems )
      Assert.Equal( 5, !calledItems |> Set.count )
      items
      |> Seq.iter( fun i -> Assert.True( !calledItems |> Set.contains i ) )

type ForAll() =
   [<Fact>]
   member x.Should_Return_True_If_Predicate_Is_True_For_All_Items() = 
      let items = seq {1..2..9 } |> Seq.toList
      let list = RAL.ofSeq items
      Assert.True( list |> RAL.forAll( fun i -> i % 2 = 1 ) )

   [<Fact>]
   member x.Should_Return_False_If_Predicate_Is_False_For_Any_Items() = 
      let items = [1; 3; 5; 6; 7; 9]
      let list = RAL.ofSeq items
      Assert.False( list |> RAL.forAll( fun i -> i % 2 = 1 ) )

   [<Fact>]
   member x.Predicate_Should_Be_Called_Until_Predicate_Is_False() = 
      let calledFor = ref Set.empty
      // Predicate that remembers what it was called with
      let pred i = 
         calledFor := !calledFor |> Set.add i
         i % 2 = 1  

      // Verify iteration terminates when predicate returns false 
      let items = [1; 3; 5; 6; 7; 9; 8]
      RAL.ofSeq items |> RAL.forAll pred |> ignore
      Assert.True( (!calledFor |> Set.count) < (items |> List.length ) ) 

      // Verify iteration is over all items if predicate is true for all
      calledFor := Set.empty
      let items = [1; 3; 5; 7; 9;]
      RAL.ofSeq items |> RAL.forAll pred |> ignore
      Assert.True( (!calledFor |> Set.count) = (items |> List.length ) ) 
      Assert.True( items |> List.forall( fun i -> !calledFor |> Set.contains i ) )

type Exists() = 
   [<Fact>]
   member x.Should_Return_True_If_Predicate_Is_True_For_At_Least_One_Item() = 
      let items = [1; 3; 5; 6; 7; 9]
      let list = RAL.ofSeq items
      Assert.True( list |> RAL.exists( fun i -> i % 2 = 0 ) )

   [<Fact>]
   member x.Should_Return_False_If_Predicate_Is_Not_True_For_Any_Item() = 
      let items = [1; 3; 5; 7; 9]
      let list = RAL.ofSeq items
      Assert.False( list |> RAL.exists( fun i -> i % 2 = 0 ) )

   [<Fact>]
   member x.Predicate_Should_Be_Called_Until_Predicate_Is_True() = 
      let calledFor = ref Set.empty
      // Predicate that remembers what it was called with, and tests if number is odd
      let pred i = 
         calledFor := !calledFor |> Set.add i
         i % 2 = 1  

      // Verify iteration terminates when predicate returns true
      let items = [2; 4; 6; 7; 8; 9]
      RAL.ofSeq items |> RAL.exists pred |> ignore
      Assert.True( (!calledFor |> Set.count) < (items |> List.length ) ) 

      // Verify iteration is over all items if predicate is false for all
      calledFor := Set.empty
      let items = [2; 4; 6; 8; 10]
      RAL.ofSeq items |> RAL.exists pred |> ignore
      Assert.True( (!calledFor |> Set.count) = (items |> List.length ) ) 
      Assert.True( items |> List.forall( fun i -> !calledFor |> Set.contains i ) )

type Filter() = 
   [<Fact>]
   member x.Should_Return_List_Containing_Items_For_Which_Predicate_Is_True() =
      let items = {1 .. 10}
      let pred i = i % 2 = 0
      let list = RAL.ofSeq items |> RAL.filter pred
      Assert.Equal( 5, list |> RAL.length )
      Assert.True( list |> RAL.forAll pred )
      Assert.Equal( 2, list |> RAL.head ) 
   
   [<Fact>]
   member x.Should_Return_Empty_List_If_No_Items_Match_Predicate() =
      let items = {2..2..10}
      let pred i = i % 2 = 1
      let list = RAL.ofSeq items |> RAL.filter pred
      Assert.Equal( 0, list |> RAL.length )

   [<Fact>]
   member x.Predicate_Should_Be_Called_Once_For_Each_Item_In_List() =
      let calledFor = ref Set.empty
      // Predicate that remembers what it was called with, and tests if number is odd
      let pred i = 
         calledFor := !calledFor |> Set.add i
         i % 2 = 1  

      let items = [ 1..10 ]
      let pq = RAL.ofSeq items |> RAL.filter pred
      Assert.True( (!calledFor |> Set.count) = (items |> List.length ) ) 
      Assert.True( items |> List.forall( fun i -> !calledFor |> Set.contains i ) )

type Map() = 
   [<Fact>]
   member x.Should_Produce_Mapped_List() =  
      let items = { 1..10 }
      let list = RAL.ofSeq items |> RAL.map (fun i -> i * 2 ) 
      Assert.Equal( 10, list |> RAL.length )
      Assert.Equal( 2, list |> RAL.head )
      items 
      |> Seq.iteri( fun idx item -> Assert.Equal( item * 2,  list |> RAL.get idx ) )
          
   [<Fact>]
   member x.Should_Return_Empty_Queue_If_Queue_Is_Empty() =
      let pred i = i % 2 = 1
      let list = RAL.empty |> RAL.ofSeq |> RAL.map (fun i -> i % 2 = 1 )
      Assert.Equal( 0, list |> RAL.length )

type MapI() = 
   [<Fact>]
   member x.Should_Produce_Mapped_List() =  
      let items = { 1..10 }
      let list = RAL.ofSeq items |> RAL.mapi (fun idx i -> (idx, i * 2) ) 
      Assert.Equal( 10, list |> RAL.length )
      Assert.Equal( 2, list |> RAL.head |> snd )
      items 
      |> Seq.iteri( fun expectedIdx item -> 
         let idx, mappedItem = list |> RAL.get expectedIdx
         Assert.Equal( expectedIdx, idx) 
         Assert.Equal( item * 2,  mappedItem ) )
          
   [<Fact>]
   member x.Should_Return_Empty_Queue_If_Queue_Is_Empty() =
      let pred i = i % 2 = 1
      let list = RAL.empty |> RAL.ofSeq |> RAL.mapi (fun idx i -> i % 2 = 1 )
      Assert.Equal( 0, list |> RAL.length )

type Fold() = 
   [<Fact>]
   member x.Should_Produce_Final_State() = 
      let items = {1..5}
      let sum = items |> RAL.ofSeq |> RAL.fold (fun sum i -> sum + i ) 0
      Assert.Equal( 15, sum )

   [<Fact>]
   member x.Should_Return_Initial_State_For_Empty_List() = 
      let sum = RAL.empty |> RAL.fold (fun sum i -> sum + i ) 10
      Assert.Equal( 10, sum )

   [<Fact>]
   member x.Should_Fold_Front_To_Back() = 
      let items = {1..5}
      let list = items |> RAL.ofSeq
      let intermediates = ref []
      let sum = 
         list
         |> RAL.fold (fun sum item -> 
            intermediates := !intermediates @ [sum]
            sum + item ) 0
      let expectedIntermediates = [0; 1; 3; 6; 10;]
      expectedIntermediates
      |> List.zip !intermediates
      |> List.iter( fun(expected, actual) -> 
         Assert.Equal( expected, actual ) )

type Rev() = 
   [<Fact>]
   member x.Should_Reverse_Items_In_List() = 
      let items = { 1..10 }
      let list = items |> RAL.ofSeq 
      let reversed = list |> RAL.rev
      items
      |> Seq.iteri( fun idx item -> 
         Assert.Equal( list |> RAL.get idx, reversed |> RAL.get (10-idx-1) ) )

   [<Fact>]
   member x.Should_Return_Empty_List_If_List_Is_Empty() = 
      Assert.True( RAL.empty |> RAL.rev |> RAL.isEmpty )

type Count() = 
   [<Fact>]
   member x.Should_Be_Zero_For_Empty_List() = 
      Assert.Equal( 0, RAL.length RAL.empty )
      Assert.True( RAL.isEmpty RAL.empty )

   [<Fact>]
   member x.Should_Return_Number_Of_Elements_In_Queue() = 
      let list = 
         seq { 1 .. 10 }
         |> Seq.fold (fun (list : RandomAccessList<int>) i ->
            Assert.Equal( i - 1, list.Count )
            list |> RAL.cons i ) RAL.empty
      Assert.Equal( 10, list.Count )   

type OfSeq() =
   [<Fact>]
   member x.Should_Return_Empty_Queue_If_Sequence_Is_Empty() = 
      Assert.True( Seq.empty |> RAL.ofSeq |> RAL.isEmpty ) 
   
   [<Fact>]
   member x.Should_Create_List_Containing_Items_In_Sequence() = 
      let items = {1..10}
      let list = items |> RAL.ofSeq 
      Assert.Equal( 10, list |> RAL.length )
      items 
      |> Seq.iteri ( fun idx item -> Assert.Equal( item, list |> RAL.get idx ) )

type OfList() = 
   [<Fact>]
   member x.Should_Return_Empty_List_If_List_Is_Empty() = 
      Assert.True( List.empty |> RAL.ofList |> RAL.isEmpty ) 

   [<Fact>]
   member x.Should_Create_List_Containing_Items_In_Sequence() = 
      let items = [1..10]
      let list = items |> RAL.ofList
      Assert.Equal( 10, list |> RAL.length )
      items 
      |> List.iteri ( fun idx item -> Assert.Equal( item, list |> RAL.get idx ) )

type OfArray() = 
   [<Fact>]
   member x.Should_Return_Empty_List_If_List_Is_Empty() = 
      Assert.True( Array.empty |> RAL.ofArray |> RAL.isEmpty )  

   [<Fact>]
   member x.Should_Create_Queue_Containing_Items_In_Array() = 
      let items = [|1..10|]
      let list = items |> RAL.ofArray 
      Assert.Equal( 10 , list |> RAL.length )
      Assert.Equal( 1, list |> RAL.head )
      items 
      |> Array.iteri ( fun idx item -> Assert.Equal( item, list |> RAL.get idx ) )

type ToSeq() = 
  [<Fact>]
   member x.Should_Return_Empty_Sequence_If_List_Is_Empty() = 
      Assert.True( RAL.empty |> RAL.toSeq |> Seq.isEmpty )

   [<Fact>]
   member x.Should_Return_Sequence_Containing_Same_Elements_As_List() =
      let items = { 1..10 }
      let raList = RAL.ofSeq items
      let seq = raList |> RAL.toSeq
      Assert.Equal( raList |> RAL.length, seq |> Seq.length )
      seq
      |> Seq.zip raList
      |> Seq.iter Assert.Equal

type ToList() = 
   [<Fact>]
   member x.Should_Return_Empty_List_If_List_Is_Empty() = 
      Assert.True( RAL.empty |> RAL.toList |> List.isEmpty )

   [<Fact>]
   member x.Should_Return_List_Containing_Same_Elements_As_List() =
      let items = { 1..10 }
      let raList = RAL.ofSeq items
      let list = raList |> RAL.toList
      Assert.Equal( raList |> RAL.length, list |> List.length )
      list
      |> Seq.zip raList
      |> Seq.iter Assert.Equal

type ToArray() = 
   [<Fact>]
   member x.Should_Return_Empty_Array_If_List_Is_Empty() = 
      Assert.True( RAL.empty |> RAL.toArray |> Array.isEmpty )

   [<Fact>]
   member x.Should_Return_Array_Containing_Same_Elements_As_List() =
      let items = { 1..10 }
      let list = RAL.ofSeq items
      let arr = list |> RAL.toArray
      Assert.Equal( list |> RAL.length, arr |> Array.length )
      arr
      |> Seq.zip list
      |> Seq.iter Assert.Equal


type GetEnumerator() =
   [<Fact>]
   member x.Should_Return_An_Unstarted_Enumerator() = 
      let enumerator = (RAL.empty :> IEnumerable).GetEnumerator()
      Assert.Throws<InvalidOperationException>( 
         fun() -> enumerator.Current )

   [<Fact>]
   member x.Can_Enumerate_Items() = 
      let queue = RAL.ofSeq { 1 .. 10 } :> seq<int>
      
      // Queue should contain these values
      let values = ref (Set.ofList [1..10])
      let enumerator = queue.GetEnumerator()
      [1 .. 10]
      |> List.iter (fun i -> 
         Assert.True( enumerator.MoveNext() )
         // Check current value is one of expected values
         Assert.True( !values |> Set.contains enumerator.Current )
         // Remove current value from expected set, since we should only see it once.
         values := !values |> Set.remove enumerator.Current )
      Assert.False( enumerator.MoveNext() )


// type Serialization() = 
//    [<Fact(Skip="Serialization not supported in portable libraries")>]
//    member x.List_Can_Be_Serialized() = 
//       let items = { 1..10 }
//       let list1 = RAL.ofSeq items
//       use s = new MemoryStream()
//       let f = new BinaryFormatter()
//       f.Serialize( s, list1 )
//       s.Position <- int64 0
//       let list2 = f.Deserialize( s ) :?> RandomAccessList<int>
      
//       Assert.Equal( list1.Count, list2.Count )     
//       list1
//       |> Seq.zip list2
//       |> Seq.iter Assert.Equal
      

