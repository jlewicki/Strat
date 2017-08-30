namespace Strat.Collections 

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices


/// Extensions to the Seq module.
module Seq = 
   
   let private rnd = new Random()

   /// Returns a sequence that invokes the specified function when creating enumerators.
   let mkSeq (f: unit -> IEnumerator<'T>) : seq<'T> = 
      { new IEnumerable<'T> with 
            member x.GetEnumerator() = f()
         interface IEnumerable with 
            member x.GetEnumerator() = (f() :> IEnumerator) }


   /// Returns a value indicating if the specified sequence contains at least one element.
   let inline any items = 
      not( items |> Seq.isEmpty)


   /// Returns the number of elements in the specified sequence for which the given predicate returns true.
   let countWhere predicate items = 
      items |> Seq.where predicate |> Seq.length


   /// Optionally returns the first item in the sequence, if it is not empty.
   let inline tryHead items = 
      items |> Seq.tryPick Some


   /// Returns a new sequence containing only the elements of the sequence for which the given predicate returns false.
   let exceptWhere f items = 
      items |> Seq.filter (f >> not)


   /// Returns a new sequence with the specified item excluded.  If the item exists more than once in the sequence, all
   /// instances are excluded.
   let without item items =
      items |> exceptWhere (fun i -> i = item )

   /// Applies the specified function to each element to obtain a key, and returns a map containing the keys and items.
   let toMapBy fkey items =
      items
      |> Seq.map (fun i -> fkey i, i)
      |> Map.ofSeq


   /// Applies the function fFold to each element of the sequence, threading an accumulator argument through the 
   /// computation. The fold continues as long as the predicate returns true.
   let foldWhile predicate fFold state (items:seq<_>) =
      use e = items.GetEnumerator() 
      let mutable currentState = state
      while e.MoveNext() && predicate currentState e.Current do
         currentState <- fFold currentState e.Current 
      currentState


   /// Splits the sequence into two sequences, containing the elements for which the given predicate returns true and
   /// false respectively.  Note that elements in the returned sequences are computed on demand.
   let partition predicate items = 
      let pairs = seq {
         for i in items do
             if predicate i then
                 yield Some(i), None
             else
                 yield None, Some(i) }
      pairs |> Seq.choose fst, pairs |> Seq.choose snd


   /// Similar to Seq.fold, except that the second argument of the accumulator function is a tuple indicating the 
   /// index of the item being accumulated, and the item
   let foldi f state items = 
      items 
      |> Seq.mapi (fun i x -> i, x) 
      |> Seq.fold f state


   /// Returns a sequence containing the items in the specified sequence in a randomized order.  Note that the 
   /// sequence is buffered in memory, and therefore this function cannot be used with infinite sequences.
   let randomize items = 
      //http://brunoreis.com/tech/uniformly-distributed-random-list-permutation/
      let permute a = 
         let n = Array.length a         
         let rec permuteAux = function
            | 0 -> a
            | k ->
               let i = rnd.Next(k+1)
               let tmp = a.[i]
               a.[i] <- a.[k]
               a.[k] <- tmp
               permuteAux (k-1)
         permuteAux (n-1)
      items
      |> Array.ofSeq
      |> permute


   /// Returns a sequence containing the items in the specified sequence in reverse order.  Note that the sequence is 
   /// buffered in memory, and therefore this function cannot be used with infinite sequences.
   let reverse items = 
      items
      |> Seq.fold (fun list item -> item::list) List.empty 
      |> Seq.ofList


   /// Returns a sequence containing the specified item, followed by all the items in the specified sequence after the
   /// first. Throws an exception if the sequence is empty.
   let replaceHead newHead items = 
      if Seq.isEmpty items then invalidArg "items" "items cannot be an empty sequence"
      else Seq.append [newHead] (items |> Seq.skip 1)



/// Extensions to the List module.
module List =

   /// Returns a new list containing only the elements of the list for which the given predicate returns false.
   let exceptWhere f items = 
      items |> List.filter (f >> not)


   /// Returns a new list with the specified item excluded.  If the item exists more than once in the sequence, only
   /// the first occurrence is excluded.
   let withoutFirst item items =
      let rec withoutFirstAcc acc items = 
         match items with
         | [] -> acc |> List.rev
         | x::xs when x = item -> (acc |> List.rev) @ xs
         | x::xs -> withoutFirstAcc (x::acc) xs
      withoutFirstAcc [] items


   /// Returns a tuple where first element is longest prefix (possibly empty) of the list with elements that do not
   /// satisfy the given predicate, and second element is the remainder of the list.  This is equivalent to the Haskell
   /// break function.
   let split f items = 
      let front, back = 
         items
         |> Seq.ofList
         |> Seq.foldWhile 
            (fun _ item -> not (f item))
            (fun (front, back) item -> item::front, List.tail back) 
            ([], items)
      List.rev front, back

   /// Returns a list containing the specified item, followed by all the items in the specified list after the
   /// first. Throws an exception if the list is empty.
   let replaceHead newHead items = 
      match items with
      | [] -> invalidArg "items" "items cannot be empty"
      | x::rest -> newHead::rest


   /// Returns a list with the same elements as the specified list, except for the last item, which is replaced by the
   /// specified item. 
   let replaceLast newLast items =
      match items with
      | [] -> invalidArg "items" "items cannot be empty"
      | _ -> 
         let _, newList =
            List.foldBack (fun item (replaced, newList) -> 
               if replaced then (replaced, item::newList)
               else (true, newLast::newList)
            ) items (false, List.empty)
         newList

/// Extensions to the Map module.
module Map = 
   
   /// Updates the value in the map for the specified key by applying the specified function to the value. Throws an
   /// exception if the key cannot be found in the map.
   let update key f map = 
      let value = f (map |> Map.find key)
      map |> Map.add key value


   /// Updates the value in the map for the specified key, by applying the specified function to the value. If the
   /// key does not exist in the map, the map is returned unchanged.
   let tryUpdate key f map = 
      match map |> Map.tryFind key with
      | Some(value) ->  true, (map |> Map.add key (f value))
      | None -> false, map


   /// Updates the value in the map for the specified key with the result of applying the specified function.
   let addOrUpdate key f map = 
      let newVal = map |> Map.tryFind key |> f 
      Map.add key newVal map


   /// Returns a sequence containing the values in the specified map
   let inline values (map: Map<_,_>) =
      map |> Seq.map (fun pair -> pair.Value)


   /// Returns a sequence containing the keys in the specified map
   let inline keys (map: Map<_,_>) =
     map |> Seq.map (fun pair -> pair.Key)


   /// Returns a map containing the values in the specified sequence, using the specified function to generate each key.
   let ofSeqBy fKey seq = 
      seq |> Seq.map (fun item -> fKey item, item ) |> Map.ofSeq 



/// C# style Enumerable extension methods 
[<Extension>]
type LinqExtensions() =

   /// Returns a new containing the unique items in the specfied collection
   [<Extension>]
   static member ToSet(items: seq<'T>) =
      if items = null then nullArg "items"
      new HashSet<'T>(items, EqualityComparer.Default)


   /// Returns a new containing the unique items in the specfied collection, using the speciied comparer to determine 
   /// uniqueness.
   [<Extension>]
   static member ToSet(items: seq<'T>, comparer: IEqualityComparer<'T> ) =
      if items = null then nullArg "items"
      if comparer = null then nullArg "comparer"
      new HashSet<'T>(items, comparer)


   /// Returns a sequence containing the indices of items in the specfied collection for the specified predicate 
   /// returns true.
   [<Extension>]
   static member IndicesWhere( items: seq<'T>, predicate: Func<'T, bool> ) =
      items 
      |> Seq.mapi (fun idx item -> idx, item )
      |> Seq.filter (fun (idx, item) -> predicate.Invoke(item))
      |> Seq.map fst

