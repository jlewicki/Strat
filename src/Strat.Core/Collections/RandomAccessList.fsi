namespace Strat.Collections

open System.Collections
open System.Collections.Generic


/// A random access list is an immutable collection that provides typical list operations such as constant time access
/// to the head and tail, as well as efficient (lgN) indexed access to elements in the list.
[<Class>]
[<Sealed>]
type RandomAccessList<'T> =

    interface IEnumerable
    interface IEnumerable<'T>
    interface ICollection<'T>
    interface IReadOnlyList<'T>

    /// Returns an emptpy list.
    static member Empty: RandomAccessList<'T>

    /// O(1). Returns a list containing the specified item.
    static member Singleton: item:'T -> RandomAccessList<'T>

    /// O(NlgN). Creates a new list containing the items in the specified sequence.
    new: items: seq<'T> -> RandomAccessList<'T>

    /// O(NlgN). Creates a new list containing the items in the specified array.
    new: items: array<'T> -> RandomAccessList<'T>

    /// O(1). Returns the number of items in the list
    member Count: int

    /// O(1). Returns a value indicating if this list is empty.
    member IsEmpty: bool

    /// O(lgN). Returns the item at the specified 0-based index. Throws an exception if the index is out of range.
    member Item: i:int -> 'T with get

    /// O(1). Returns the item at the front of the list. Throws an exception if the list is empty.
    member Head: 'T

    /// O(1). Returns the list without the first element. Throws an exception of the list is empty.
    member Tail: RandomAccessList<'T>

    /// O(1). Returns a new list with the specified item at the head of the list, followed by the original list.
    member Cons: item:'T -> RandomAccessList<'T>

    /// O(lgN). Returns the item at the specified 0-based index. Throws an exception if the index is out of range.
    member Get: i:int -> 'T

    /// O(lgN): Returns a new list by setting the specified item at the specified index
    member Set: i:int * item:'T ->  RandomAccessList<'T>

    /// O(N): Returns a value indicating if the specified item is in the list.
    member Contains : item:'T -> bool

    /// O(N): Applies the specified function to each element in the list.
    member Iterate: f:('T -> unit) -> unit

    /// O(N): Returns a new list containing the results of applying the specified function to each element in this list.
    member Map: f:('T -> 'U) -> RandomAccessList<'U>

    /// O(N): Returns a new list containing the results of applying the specified function to each element in this list.
    /// The integer index passed to the function indicates the index (from 0) of element being transformed.
    member MapIndexed: f:(int -> 'T -> 'U) -> RandomAccessList<'U>

    /// O(N): Returns a new list containing only the elements of the list for which the given predicate returns true. 
    member Filter: pred:('T -> bool) -> RandomAccessList<'T>

    /// O(N): Applies the given accumulating function to all the elements of the list, and returns the accumulated result.
    member Fold: f:('State -> 'T -> 'State) -> 'State -> 'State

    /// O(N): Returns a new list containing the elements in the list, in reverse order.
    member Reverse: unit -> RandomAccessList<'T>

    /// O(N): Returns a value indicating if the specified predicate is true for any element in the queue.
    member Exists: pred:('T -> bool) -> bool

    /// O(N): Returns the first element that matches the specified predicate, or None.
    member TryFind: pred:('T -> bool) -> option<'T>

    /// O(N): Returns a value indicating if the specified predicate is true for all elements in the queue.
    member ForAll: pred:('T -> bool) -> bool

    /// O(N): Returns a new list containing the elements in this collection.
    member ToList: unit -> list<'T>

    /// O(N): Returns a new array containing the elements in this collection.
    member ToArray: unit -> array<'T>


/// Functional operators for <c>RandomAccessList<_></c> type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RandomAccessList =   
   
   /// O(1). Returns a empty list.
   [<GeneralizableValue>]
   [<CompiledName("Empty")>]
   val inline empty<'T> : RandomAccessList<'T>

   /// O(1). Returns a value indicating if this list is empty.
   [<CompiledName("IsEmpty")>]
   val inline isEmpty:  list: RandomAccessList<'T> -> bool

   /// O(1): Returns a new list containing the specified item.
   [<CompiledName("Singleton")>]
   val inline singleton: item:'T -> RandomAccessList<'T>

   /// O(1). Returns the number of items in the list
   [<CompiledName("Count")>]
   val inline length: RandomAccessList<'T> -> int

   /// O(1). Returns the item at the front of the list. Throws an exception if the list is empty.
   [<CompiledName("Head")>]
   val inline head: list: RandomAccessList<'T> -> 'T

   /// O(1). Returns the list without the first element. Throws an exception of the list is empty.
   [<CompiledName("Tail")>]
   val inline tail: list: RandomAccessList<'T> -> RandomAccessList<'T>

   /// O(1). Returns the last element in the list. Throws an exception of the list is empty.
   [<CompiledName("Last")>]
   val inline last: list: RandomAccessList<'T> -> 'T

   /// O(1): Returns a new list with the specified item at the head of the list, followed by the original list.
   [<CompiledName("Cons")>]
   val inline cons: item:'T -> list: RandomAccessList<'T> -> RandomAccessList<'T>

   /// O(lgN). Returns the item at the specified 0-based index. Throws an exception if the index is out of range.
   [<CompiledName("Get")>]
   val inline get: idx:int -> list: RandomAccessList<'T> -> 'T

   /// O(lgN). Returns a new list by setting the specified element at the specified index
   [<CompiledName("Set")>]
   val inline set: idx:int -> item:'T -> list: RandomAccessList<'T> -> RandomAccessList<'T>

   /// O(N): Returns a new list containing the elements in the list, in reverse order.
   [<CompiledName("Reverse")>]
   val inline rev: list: RandomAccessList<'T> -> RandomAccessList<'T>

   /// O(N): Returns a value indicating if the specified element is in the list.
   [<CompiledName("Contains")>]
   val inline contains: item:'T -> list: RandomAccessList<'T> -> bool

   /// O(N): Applies the specified function to each element in the list.
   [<CompiledName("Iterate")>]
   val inline iter: f:('T -> unit) -> list: RandomAccessList<'T> -> unit

   /// O(N): Returns a value indicating if the specified predicate is true for any element in the queue.
   [<CompiledName("Exists")>]
   val inline exists: f:('T -> bool) -> list: RandomAccessList<'T> -> bool

   /// O(N): Returns the first element for which the given predicate returns true. Return None if no such element exists.
   [<CompiledName("TryFind")>]
   val inline tryFind: f:('T -> bool) -> RandomAccessList<'T> -> option<'T>

   /// O(N): Returns a value indicating if the specified predicate is true for all elements in the queue.
   [<CompiledName("ForAll")>]
   val inline forAll: f:('T -> bool) -> list: RandomAccessList<'T> -> bool

   /// O(N): Returns a new list containing the results of applying the specified function to each element in this list.
   [<CompiledName("Map")>]
   val inline map: f:('T -> 'U) -> list: RandomAccessList<'T> -> RandomAccessList<'U>

   /// O(N): Returns a new list containing the results of applying the specified function to each element in this list.
   /// The integer index passed to the function indicates the index (from 0) of element being transformed.
   [<CompiledName("Map")>]
   val inline mapi: f:(int -> 'T -> 'U) -> list: RandomAccessList<'T> -> RandomAccessList<'U>

   /// O(N): Returns a new list containing only the elements of the list for which the given predicate returns true. 
   [<CompiledName("Filter")>]
   val inline filter: f:('T -> bool) -> list: RandomAccessList<'T> -> RandomAccessList<'T>

   /// O(N): Applies the given accumulating function to all the elements of the list, and returns the accumulated result.
   [<CompiledName("Fold")>]
   val inline fold: f:('State -> 'T -> 'State) -> 'State -> list: RandomAccessList<'T> -> 'State

   /// O(N): Returns a new list containing the items in the specified sequence.
   [<CompiledName("OfSeq")>]
   val inline ofSeq: items:seq<'T> -> RandomAccessList<'T>

   /// O(N): Returns a new list containing the items in the specified array.
   [<CompiledName("OfArray")>]
   val inline ofArray: items:array<'T> -> RandomAccessList<'T>

   /// O(N): Returns a new list containing the items in the specified list
   [<CompiledName("OfList")>]
   val inline ofList: items:list<'T> -> RandomAccessList<'T>

    /// O(N): Returns a new sequence containing the items in the specified list.
   [<CompiledName("ToSeq")>]
   val inline toSeq: list:RandomAccessList<'T> -> seq<'T>

   /// O(N): Returns a new array containing the items in the specified list.
   [<CompiledName("ToArray")>]
   val inline toArray: list:RandomAccessList<'T> -> array<'T>

   /// O(N): Returns a new list containing the items in the specified list
   [<CompiledName("ToList")>]
   val inline toList: list:RandomAccessList<'T> -> list<'T>