namespace Strat.Collections

open System.Collections
open System.Collections.Generic


/// <summary>
/// A Vector is a persistent collection that combines the behavior of a vector and an indexed collection. Traditional
/// list operations such as adding, accessing or removing an item at the end of the list in constant time are 
/// supported, as well as (near) constant time lookup of items in the list by index.
/// <para>
/// IndexLists are implemented as bit-partitioned tries, and are closely modeled on the PersistentVector implementation
/// that can be found in Clojure </para>
/// </summary>
[<Class>]
[<Sealed>]
type Vector<'T> =

   interface IEnumerable
   interface IEnumerable<'T>
   interface IReadOnlyList<'T>
   interface Strat.Collections.Primitives.BitTrie.ITrieSource<'T>
   
   /// Returns an empty vector.
   static member Empty: Vector<'T>

   /// Creates a new vector containing the items in the specified sequence.
   new: items: seq<'T> -> Vector<'T>

   /// O(1). Returns the number of items in the vector.
   member Count: int

   /// O(1). Returns a value indicating if this vector is empty.
   member IsEmpty: bool

   /// O(lg32N). Returns the item at the specified 0-based index. Throws an exception if the index is out of range.
   member Item: i:int -> 'T with get

   /// O(1). Returns the last item in the vector. Throws an exception if the index is out of range.
   member Last: 'T

   /// O(1). Returns the last item in the vector, or None if the vector is empty.
   member TryLast: option<'T>

   /// O(lg32N). Returns a new vector by replacing the item at the specified index with the specfied item.
   member Set: index:int * item:'T ->  Vector<'T>

   /// O(lg32N). Returns a new vector by adding the specified item at the end of the vector.
   member Add: item:'T -> Vector<'T>

   /// O(lg32N). Returns the last item in the vector, and a new vector with the last item removed.
   member RemoveLast: unit -> 'T * Vector<'T>


/// Functional operators for <c>Vector<_></c>.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector = 

   /// O(1). Returns a empty vector.
   [<GeneralizableValue>]
   [<CompiledName("Empty")>]
   val empty<'T> : Vector<'T>

   /// O(1). Returns a new vector containing the specified item.
   [<CompiledName("Singleton")>]
   val inline singleton: item:'T -> Vector<'T>

   /// O(N): Returns a new vector containing the items in the specified sequence.
   [<CompiledName("OfSeq")>]
   val inline ofSeq: items:seq<'T> -> Vector<'T>

   /// O(N): Returns a new vector containing the items in the specified list.
   [<CompiledName("OfList")>]
   val inline ofList: items:list<'T> -> Vector<'T>

   /// O(N): Returns a new vector containing the items in the specified array.
   [<CompiledName("OfArray")>]
   val inline ofArray: items:array<'T> -> Vector<'T>

   /// O(N). Returns a new a vector with the specified nunber of items, using the specified function to create an item
   /// for each index on the vector.
   [<CompiledName("Init")>]
   val init : count:int -> f:(int -> 'T) -> Vector<'T>
   
   /// O(lg32N). Returns a new vector by adding the specified item at the end of the vector.
   val inline add: item:'T -> vector: Vector<'T> -> Vector<'T>

   /// O(N):Returns a new vector that contains the elements of the vector, followed by the elements in the sequence.
   [<CompiledName("AddRange")>]
   val addAll: items: seq<'T> -> vector: Vector<'T> -> Vector<'T>

   /// O(N+M). Returns a new vector that contains the elements of the first vector, followed by the elements of the second vector.
   [<CompiledName("Append")>]
   val append: vector1:Vector<'T> -> vector2:Vector<'T> -> Vector<'T>

   /// O(1): Returns the number of items in the vector.
   [<CompiledName("Length")>]
   val inline length: vector:Vector<'T> -> int

   /// O(1). Returns a value indicating if the vector is empty.
   [<CompiledName("IsEmpty")>]
   val inline isEmpty: vector:Vector<'T> -> bool

   /// O(lg32N). Returns the item at the specified 0-based index. Throws an exception if the index is out of range.
   [<CompiledName("Get")>]
   val inline get: index:int -> vector:Vector<'T> -> 'T

   /// O(1). Returns the last item in the vector, or None if the vector is empty.
   [<CompiledName("TryLast")>]
   val inline tryLast: vector:Vector<'T> -> option<'T>

   /// O(1). Returns the last item in the vector. Throws an exception if the index is out of range.
   [<CompiledName("Last")>]
   val inline last: vector:Vector<'T> -> 'T

   /// O(lg32N). Returns a new vector by replacing the item at the specified index with the specified item.
   [<CompiledName("Set")>]
   val inline set: index:int -> item:'T -> Vector<'T> -> Vector<'T>

   /// O(lg32N). Returns the last item in the vector, and new item with the last item removed.
   [<CompiledName("RemoveLast")>]
   val inline removeLast: vector:Vector<'T> -> 'T * Vector<'T>

   /// O(N). Returns a new vector whose elements are the results of applying the specified function to each of the
   /// elements of the vector. The integer index passed to the function indicates the index of element being
   /// transformed.
   [<CompiledName("MapIndexed")>]
   val mapi: f:(int -> 'T -> 'U) -> vector:Vector<'T> -> Vector<'U>

   /// O(N): Returns a new vector whose elements are the results of applying the specified function to each of the
   /// elements of the vector.
   [<CompiledName("Map")>]
   val map: f:('T -> 'U) -> vector:Vector<'T> -> Vector<'U>

   /// O(N). Returns a new vector containing only the elements of the vector for which the specified predicate returns
   /// true.
   [<CompiledName("Filter")>]
   val filter: predicate:('T -> bool) -> vector:Vector<'T> -> Vector<'T>

   /// O(N). Applies the specified function to each element of the vector, threading an accumulator argument through the
   /// computation. The fold function takes the second argument, and applies the function f to it and the first element
   /// of the list. Then, it feeds this result into the function f along with the second element, and so on. It returns
   /// the final result. 
   [<CompiledName("Fold")>]
   val fold: f:('State -> 'T -> 'State) -> initial:'State -> v:Vector<'T> -> 'State

   /// O(N). Applies the specified function to each element of the vector, threading an accumulator argument through
   /// computation. If the input function is f and the elements are i0...iN then computes f i0 (...(f iN s)).
   [<CompiledName("FoldBack")>]
   val foldBack: f:('T -> 'State -> 'State) -> Vector<'T> -> 'State -> 'State

   /// O(N): For each element of the vector, applies the specified function. Concatenates all the results and return the
   /// combined vector.
   [<CompiledName("Collect")>]
   val collect: f:('T -> Vector<'U>) -> Vector<'T> -> Vector<'U>

   /// O(N): Applies the given function to each element of the vector and returns the vector comprised of the results
   /// for each element where the function returns Some with some value.
   [<CompiledName("Choose")>]
   val choose: f:('T -> 'U option) -> Vector<'T> -> Vector<'U>

   /// O(N). Applies the specified function to each element in the vector.
   [<CompiledName("Iterate")>]
   val iter: f:('T -> unit) -> vector:Vector<'T> -> unit

   /// O(N). Applies the specified function to each element of the vector. The integer passed to the function
   /// indicates the index of element.
   [<CompiledName("IterateIndexed")>]
   val iteri: f:(int -> 'T -> unit) -> vector:Vector<'T> -> unit

   /// O(N). Returns a new vector with the elements in reverse order.
   [<CompiledName("Reverse")>]
   val rev: v: Vector<'T> -> Vector<'T> 

   /// O(N). Returns the first element for which the specified function returns true. Return None if no such element 
   /// exists.
   [<CompiledName("TryFind")>]
   val tryFind: predicate:('T -> bool) -> vector:Vector<'T> -> option<'T>

   /// O(N). Returns the first element for which the specified function returns true. Raises KeyNotFoundException if no 
   /// such element exists.
   [<CompiledName("Find")>]
   val find: predicate:('T -> bool) -> vector:Vector<'T> -> 'T

   /// O(N). Returns the index of the first element in the array that satisfies the specified predicate. Return None if
   /// no such element exists.
   [<CompiledName("TryFindIndex")>]
   val tryFindIndex: predicate:('T -> bool) -> vector:Vector<'T> -> option<int>

   /// O(N). Returns the index of the first element in the vector that satisfies the specified predicate. Raises
   /// KeyNotFoundException if none of the elements satisfy the predicate.
   [<CompiledName("FindIndex")>]
   val findIndex: predicate:('T -> bool) -> vector:Vector<'T> -> int

   /// O(N). Applies the specified function to successive elements, returning the first result where the function
   /// returns Some. If the function does not return Some for any element, then None is returned.
   [<CompiledName("TryPick")>]
   val tryPick: f:('T -> 'U option) -> vector:Vector<'T> -> option<'U>

   /// O(N). Applies the given function to successive elements, returning the first result where the function returns
   /// Some. If the function never returns Some then KeyNotFoundException is raised.
   [<CompiledName("Pick")>]
   val pick: f:('T -> 'U option) -> vector:Vector<'T> -> 'U

   /// O(N). Tests if any element in the vector satisfies the given predicate.
   [<CompiledName("Exists")>]
   val exists: predicate:('T -> bool) -> vector:Vector<'T> -> bool

   /// O(N). Combines the two vectors into a vector of pairs. The two vectors must have equal lengths.
   [<CompiledName("Zip")>]
   val zip: vector1:Vector<'T> -> vector2:Vector<'U> -> Vector<'T * 'U>

   /// O(N). Returns a value indicating if all elements in the vector satisfy the given predicate.
   [<CompiledName("ForAll")>]
   val forall: predicate:('T -> bool) -> vector:Vector<'T> -> bool

   /// O(N). Returns the lowest of all elements of the vector, compared via Operators.min.
   [<CompiledName("Min")>]
   val min: vector:Vector<'T> -> 'T  when 'T : comparison 

   /// O(N). Returns the lowest of all elements of the array, compared by using Operators.min on the function result.
   [<CompiledName("MinBy")>]
   val minBy: f:('T -> 'U) -> vector:Vector<'T> -> 'T  when 'U : comparison 

   /// O(N). Returns the highest of all elements of the vector, compared via Operators.max.
   [<CompiledName("Max")>]
   val max: vector:Vector<'T> -> 'T  when 'T : comparison 

   /// O(N). Returns the highest of all elements of the array, compared by using Operators.max on the function result.
   [<CompiledName("MaxBy")>]
   val maxBy: f:('T -> 'U) -> vector:Vector<'T> -> 'T  when 'U : comparison 

   /// O(N). Returns the sum of the elements in the vector.
   [<CompiledName("Sum")>]
   val inline sum : v:(^T Vector) -> ^T 
      when ^T : (static member ( + ) : ^T * ^T -> ^T) 
      and  ^T : (static member Zero : ^T)

   /// O(N). Returns the sum of the results generated by applying the function to each element in the vector.
   [<CompiledName("SumBy")>]
   val inline sumBy: f:('T -> ^U) -> v:Vector<'T> -> ^U
      when ^U : (static member ( + ) : ^U * ^U -> ^U) 
      and  ^U : (static member Zero : ^U)

   /// O(1). Views the specified vector as a sequence.
   [<CompiledName("ToSeq")>]
   val inline toSeq: vector:Vector<'T> -> seq<'T>

   /// O(N). Creates a new array containing the elements in the specfied vector.
   [<CompiledName("ToArray")>]
   val toArray: vector:Vector<'T> -> 'T[]