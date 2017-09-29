namespace Strat.Collections

open System.Collections
open System.Collections.Generic

[<Class; Sealed>]
type Queue<'T> =

   interface IEnumerable
   interface IEnumerable<'T>
   
   /// Returns an empty queue.
   static member Empty: Queue<'T>

   /// O(1). Returns the number of items in this queue.
   member Count: int

   /// O(1). Returns a value indicating if this queue is empty.
   member IsEmpty: bool

   /// O(1). Returns the item at the front of this queue. Throws an exception if the queue is empty.
   member Head: 'T

   /// O(1). Adds an item to the back of this queue.
   member Enqueue: item:'T -> Queue<'T>

   /// O(1). Returns a pair consisting of the item at the front of the queue, and the remainder of the queue. Throws
   /// an exception if the queue is empty.
   member Dequeue: unit -> struct ('T * Queue<'T>)


/// Functional operators for <c>Queue<_></c>.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Queue = 

   /// O(1). Returns a empty queue.
   [<GeneralizableValue>]
   [<CompiledName("Empty")>]
   val empty<'T> : Queue<'T>

   /// O(1). Returns a value indicating if the queue is empty.
   [<CompiledName("IsEmpty")>]
   val isEmpty: queue:Queue<'T> -> bool

   /// O(1). Returns the number of items in the queue.
   [<CompiledName("IsEmpty")>]
   val length: queue:Queue<'T> -> int

   /// O(1). Returns the item at the front of the queue. Throws an exception if the queue is empty.
   [<CompiledName("Head")>]
   val head: queue:Queue<'T> -> 'T

   /// O(1). Adds the item to the back of the queue.
   [<CompiledName("Enqueue")>]
   val enqueue: item:'T -> queue:Queue<'T> -> Queue<'T>

   /// O(1). Returns a pair consisting of the item at the front of the queue, and the remainder of the queue. Throws
   /// an exception if the queue is empty.
   [<CompiledName("Dequeue")>]
   val dequeue: queue:Queue<'T> -> struct ('T * Queue<'T>)