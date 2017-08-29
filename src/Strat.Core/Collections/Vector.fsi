namespace Strat.Collections

open System.Collections
open System.Collections.Generic


[<Class>]
[<Sealed>]
type Vector<'T> =

   interface IEnumerable
   interface IEnumerable<'T>
   
   /// Returns an empty vector.
   static member Empty: Vector<'T>

   /// Creates a new vector containing the items in the specified sequence.
   new: items: seq<'T> -> Vector<'T>

   /// Creates a new vector containing the items in the specified collection.
   new: items: ICollection<'T> -> Vector<'T>

   /// O(1). Returns the number of items in the vector.
   member Count: int

   /// O(1). Returns a value indicating if this vector is empty.
   member IsEmpty: bool

   /// O(1). Returns the item at the specified 0-based index. Throws an exception if the index is out of range.
   member Item: i:int -> 'T with get

   /// Returns a new vector by replacing the item at the specified index with the specfied item.
   member Set: index:int * item:'T ->  Vector<'T>

   /// Returns a new vector by adding the specified item at the end of the vector.
   member Add: item:'T -> Vector<'T>

   /// Returns the last item in the vector, and new item with the last item removed.
   member RemoveLast: unit -> 'T * Vector<'T>

   /// O(N): Returns a new vector whose elements are the results of applying the given function to each of the elements
   /// of the vector.
   member Map: f:('T -> 'U) -> Vector<'U>


/// Functional operators for <c>Vector<_></c>.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector = 

   /// O(1). Returns a empty vector.
   [<GeneralizableValue>]
   [<CompiledName("Empty")>]
   val empty<'T> : Vector<'T>

   /// Returns a new vector containing the specified item.
   [<CompiledName("Singleton")>]
   val inline singleton: item:'T -> Vector<'T>

   /// O(N): Returns a new vector containing the items in the specified sequence.
   [<CompiledName("OfSeq")>]
   val inline ofSeq: items:seq<'T> -> Vector<'T>

   /// O(N): Returns a new vector containing the items in the specified array.
   [<CompiledName("OfArray")>]
   val inline ofArray: items:array<'T> -> Vector<'T>

   /// O(1): Returns the number of items in the vector.
   [<CompiledName("Length")>]
   val inline length: vector:Vector<'T> -> int

   /// O(1): Returns a value indicating if the vector is empty.
   [<CompiledName("IsEmpty")>]
   val inline isEmpty: vector:Vector<'T> -> bool

   /// O(1). Returns the item at the specified 0-based index. Throws an exception if the index is out of range.
   [<CompiledName("Get")>]
   val inline nth: index:int -> vector:Vector<'T> -> 'T

   /// Returns a new vector by replacing the item at the specified index with the specfied item.
   [<CompiledName("Set")>]
   val inline set: index:int -> item:'T -> Vector<'T> -> Vector<'T>

   /// Returns the last item in the vector, and new item with the last item removed.
   [<CompiledName("RemoveLast")>]
   val inline removeLast: vector:Vector<'T> -> 'T * Vector<'T>

   /// O(N): Returns a new vector whose elements are the results of applying the given function to each of the elements
   /// of the vector.
   [<CompiledName("Map")>]
   val inline map: f:('T -> 'U) -> vector:Vector<'T> -> Vector<'U>

   /// O(N): Applies the specified function to each element in the vector.
   [<CompiledName("Iterate")>]
   val iter: f:('T -> unit) -> vector:Vector<'T> -> unit

    /// O(N): Applies the specified function to each element of the vector. The integer passed to the function
    // indicates the index of element.
   [<CompiledName("IterateIndexed")>]
   val iteri: f:(int -> 'T -> unit) -> vector:Vector<'T> -> unit

   /// O(1): Views the specified vector as a sequence.
   [<CompiledName("ToSeq")>]
   val inline toSeq: vector:Vector<'T> -> seq<'T>

   /// O(N): Creates a new array containing the elements in the specfied vector.
   [<CompiledName("ToArray")>]
   val toArray: vector:Vector<'T> -> 'T[]