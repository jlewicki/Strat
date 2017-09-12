namespace Strat.Collections

open System.Collections
open System.Collections.Generic


/// <summary>
/// A IndexList is a persistent collection that provides fast indexed access to elements in the collection, as well as
/// efficient operations to add and remove elements in the list. In general, these operations are O(lg32N), which in
/// practice is effectively constant time.
/// <para>
/// IndexLists are implemented as bit-partitioned tries, and are closely modeled on the PersistentVector implementation that
/// can be found in Clojure.</para>
/// </summary>
[<Class>]
[<Sealed>]
type IndexList<'T> =

   interface IEnumerable
   interface IEnumerable<'T>
   interface IReadOnlyList<'T>
   interface Strat.Collections.Primitives.BitTrie.ITrieSource<'T>
   
   /// Returns an empty list.
   static member Empty: IndexList<'T>

   /// Creates a new list containing the items in the specified sequence.
   new: items: seq<'T> -> IndexList<'T>

   /// O(1). Returns the number of items in the list.
   member Count: int

   /// O(1). Returns a value indicating if this list is empty.
   member IsEmpty: bool

   /// O(lg32N). Returns the item at the specified 0-based index. Throws an exception if the index is out of range.
   member Item: i:int -> 'T with get

   /// O(1). Returns the first item in the list. Throws an exception if the index is out of range.
   member Head: 'T

   /// O(1). Returns the first item in the list, or None if the list is empty.
   member TryHead: option<'T>

   /// O(lg32N). Returns a new list by replacing the item at the specified index with the specfied item.
   member Set: index:int * item:'T ->  IndexList<'T>

   /// O(lg32N). Returns a new list by adding the specified item at the end of the list.
   member Cons: item:'T -> IndexList<'T>

   /// O(lg32N). Returns the first item in the list, and a new list with the first item removed.
   member RemoveHead: unit -> 'T * IndexList<'T>


/// Functional operators for <c>IndexList<_></c>.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IndexList = 

   /// O(1). Returns a empty list.
   [<GeneralizableValue>]
   [<CompiledName("Empty")>]
   val empty<'T> : IndexList<'T>

   /// O(1). Returns a new list containing the specified item.
   [<CompiledName("Singleton")>]
   val inline singleton: item:'T -> IndexList<'T>

   /// O(N): Returns a new list containing the items in the specified sequence.
   [<CompiledName("OfSeq")>]
   val inline ofSeq: items:seq<'T> -> IndexList<'T>

   /// O(N): Returns a new list containing the items in the specified array.
   [<CompiledName("OfArray")>]
   val inline ofArray: items:array<'T> -> IndexList<'T>

   /// O(N). Returns a new a list with the specified nunber of items, using the specified function to create an item
   /// for each index on the list.
   [<CompiledName("Init")>]
   val init : count:int -> f:(int -> 'T) -> IndexList<'T>

   /// O(N+M). Returns a new list that contains the elements of the first list,  followed by the elements the second list.
   [<CompiledName("Append")>]
   val append: list1:IndexList<'T> -> list2:IndexList<'T> -> IndexList<'T>

   /// O(1): Returns the number of items in the list.
   [<CompiledName("Length")>]
   val inline length: list:IndexList<'T> -> int

   /// O(1). Returns a value indicating if the list is empty.
   [<CompiledName("IsEmpty")>]
   val inline isEmpty: list:IndexList<'T> -> bool

   /// O(lg32N). Returns the item at the specified 0-based index. Throws an exception if the index is out of range.
   [<CompiledName("Get")>]
   val inline get: index:int -> list:IndexList<'T> -> 'T

   /// O(1). Returns the first item in the list, or None if the list is empty.
   [<CompiledName("TryHead")>]
   val inline tryHead: list:IndexList<'T> -> option<'T>

   /// O(1). Returns the first item in the list. Throws an exception if the index is out of range.
   [<CompiledName("Head")>]
   val inline head: list:IndexList<'T> -> 'T

   /// O(lg32N). Returns a new list by replacing the item at the specified index with the specified item.
   [<CompiledName("Set")>]
   val inline set: index:int -> item:'T -> IndexList<'T> -> IndexList<'T>

   /// O(lg32N). Returns the last item in the list, and new item with the last item removed.
   [<CompiledName("RemoveHead")>]
   val inline removeHead: list:IndexList<'T> -> 'T * IndexList<'T>

   /// O(N). Returns a new list whose elements are the results of applying the specified function to each of the
   /// elements of the list. The integer index passed to the function indicates the index of element being
   /// transformed.
   [<CompiledName("MapIndexed")>]
   val mapi: f:(int -> 'T -> 'U) -> list:IndexList<'T> -> IndexList<'U>

   /// O(N): Returns a new list whose elements are the results of applying the specified function to each of the
   /// elements of the list.
   [<CompiledName("Map")>]
   val map: f:('T -> 'U) -> list:IndexList<'T> -> IndexList<'U>

   /// O(N). Returns a new list containing only the elements of the list for which the specified predicate returns
   /// true.
   [<CompiledName("Filter")>]
   val filter: predicate:('T -> bool) -> list:IndexList<'T> -> IndexList<'T>

   /// O(N). Applies the specified function to each element of the list, threading an accumulator argument through the
   /// computation. The fold function takes the second argument, and applies the function f to it and the first element
   /// of the list. Then, it feeds this result into the function f along with the second element, and so on. It returns
   /// the final result. 
   [<CompiledName("Fold")>]
   val fold: f:('State -> 'T -> 'State) -> initial:'State -> v:IndexList<'T> -> 'State

   /// O(N). Applies the specified function to each element of the list, threading an accumulator argument through
   /// computation. If the input function is f and the elements are i0...iN then computes f i0 (...(f iN s)).
   [<CompiledName("FoldBack")>]
   val foldBack: f:('T -> 'State -> 'State) -> IndexList<'T> -> 'State -> 'State

   /// O(N): For each element of the list, applies the specified function. Concatenates all the results and return the
   /// combined list.
   [<CompiledName("Collect")>]
   val collect: f:('T -> IndexList<'U>) -> IndexList<'T> -> IndexList<'U>

   /// O(N): Applies the given function to each element of the list and returns the list comprised of the results
   /// for each element where the function returns Some with some value.
   [<CompiledName("Choose")>]
   val choose: f:('T -> 'U option) -> IndexList<'T> -> IndexList<'U>

   /// O(N). Applies the specified function to each element in the list.
   [<CompiledName("Iterate")>]
   val iter: f:('T -> unit) -> list:IndexList<'T> -> unit

   /// O(N). Applies the specified function to each element of the list. The integer passed to the function
   /// indicates the index of element.
   [<CompiledName("IterateIndexed")>]
   val iteri: f:(int -> 'T -> unit) -> list:IndexList<'T> -> unit

   /// O(N). Returns a new list with the elements in reverse order.
   [<CompiledName("Reverse")>]
   val rev: v: IndexList<'T> -> IndexList<'T> 

   /// O(N). Returns the first element for which the specified function returns true. Return None if no such element 
   /// exists.
   [<CompiledName("TryFind")>]
   val tryFind: predicate:('T -> bool) -> list:IndexList<'T> -> option<'T>

   /// O(N). Returns the first element for which the specified function returns true. Raises KeyNotFoundException if no 
   /// such element exists.
   [<CompiledName("Find")>]
   val find: predicate:('T -> bool) -> list:IndexList<'T> -> 'T

   /// O(N). Returns the index of the first element in the array that satisfies the specified predicate. Return None if
   /// no such element exists.
   [<CompiledName("TryFindIndex")>]
   val tryFindIndex: predicate:('T -> bool) -> list:IndexList<'T> -> option<int>

   /// O(N). Returns the index of the first element in the list that satisfies the specified predicate. Raises
   /// KeyNotFoundException if none of the elements satisfy the predicate.
   [<CompiledName("FindIndex")>]
   val findIndex: predicate:('T -> bool) -> list:IndexList<'T> -> int

   /// O(N). Applies the specified function to successive elements, returning the first result where the function
   /// returns Some. If the function does not return Some for any element, then None is returned.
   [<CompiledName("TryPick")>]
   val tryPick: f:('T -> 'U option) -> list:IndexList<'T> -> option<'U>

   /// O(N). Applies the given function to successive elements, returning the first result where the function returns
   /// Some. If the function never returns Some then KeyNotFoundException is raised.
   [<CompiledName("Pick")>]
   val pick: f:('T -> 'U option) -> list:IndexList<'T> -> 'U

   /// O(N). Tests if any element in the list satisfies the given predicate.
   [<CompiledName("Exists")>]
   val exists: predicate:('T -> bool) -> list:IndexList<'T> -> bool

   /// O(N). Combines the two lists into a list of pairs. The two lists must have equal lengths.
   [<CompiledName("Zip")>]
   val zip: list1:IndexList<'T> -> list2:IndexList<'U> -> IndexList<'T * 'U>

   /// O(N). Returns a value indicating if all elements in the list satisfy the given predicate.
   [<CompiledName("ForAll")>]
   val forall: predicate:('T -> bool) -> list:IndexList<'T> -> bool

   /// O(N). Returns the lowest of all elements of the list, compared via Operators.min.
   [<CompiledName("Min")>]
   val min: list:IndexList<'T> -> 'T  when 'T : comparison 

   /// O(N). Returns the lowest of all elements of the array, compared by using Operators.min on the function result.
   [<CompiledName("MinBy")>]
   val minBy: f:('T -> 'U) -> list:IndexList<'T> -> 'T  when 'U : comparison 

   /// O(N). Returns the highest of all elements of the list, compared via Operators.max.
   [<CompiledName("Max")>]
   val max: list:IndexList<'T> -> 'T  when 'T : comparison 

   /// O(N). Returns the highest of all elements of the array, compared by using Operators.max on the function result.
   [<CompiledName("MaxBy")>]
   val maxBy: f:('T -> 'U) -> list:IndexList<'T> -> 'T  when 'U : comparison 

   /// O(N). Returns the sum of the elements in the list.
   [<CompiledName("Sum")>]
   val inline sum : v:(^T IndexList) -> ^T 
      when ^T : (static member ( + ) : ^T * ^T -> ^T) 
      and  ^T : (static member Zero : ^T)

   /// O(N). Returns the sum of the results generated by applying the function to each element in the list.
   [<CompiledName("SumBy")>]
   val inline sumBy: f:('T -> ^U) -> v:IndexList<'T> -> ^U
      when ^U : (static member ( + ) : ^U * ^U -> ^U) 
      and  ^U : (static member Zero : ^U)

   /// O(1). Views the specified list as a sequence.
   [<CompiledName("ToSeq")>]
   val inline toSeq: list:IndexList<'T> -> seq<'T>

   /// O(N). Creates a new array containing the elements in the specfied list.
   [<CompiledName("ToArray")>]
   val toArray: list:IndexList<'T> -> 'T[]

   /// O(N). Creates a new list containing the elements in the specfied index list.
   [<CompiledName("ToList")>]
   val toList: list:IndexList<'T> -> list<'T>