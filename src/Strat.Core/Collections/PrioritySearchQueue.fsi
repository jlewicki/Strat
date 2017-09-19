namespace Strat.Collections

open System.Collections
open System.Collections.Generic


/// A priority search queue is a persistent collection that combines the behavior of a priority queue and an 
/// associative map. Traditional priority queue operations such as constant time access to a minimum value, and map
/// operations such as logarithmic access to a binding for a given key, are both supported.
/// <para>
/// Priority search queues are implemented in terms of a priorty search pennant, as described in the paper
/// 'A Simple Implementation Technique for Priority Search Queues' by Ralf Hinze.</para>
[<Class>]
[<Sealed>]
type PrioritySearchQueue<[<EqualityConditionalOn>]'K, [<EqualityConditionalOn>]'V when 'K: comparison and 'V: comparison> = 
    
   interface IEnumerable
   interface IEnumerable<KeyValuePair<'K, 'V>>
   interface IReadOnlyCollection<KeyValuePair<'K, 'V>>
   interface ICollection<KeyValuePair<'K, 'V>>
   interface IDictionary<'K, 'V>
   interface Strat.Collections.Primitives.Pennant.IPennantSource<'K, 'V>

   /// Returns an empty queue.
   static member Empty : PrioritySearchQueue<'K,'V>

   /// O(N^2), O(NlgN) on average. Creates a new queue containing the entries in the specified collection.
   new: items:seq<'K*'V> -> PrioritySearchQueue<'K, 'V>

   /// O(1). Returns the number if items in this queue.
   member Count: int

   /// O(1). Returns true if this queue has no elements.
   member IsEmpty: bool

   /// O(1). Returns the entry with the minimum value in this queue. Throws an exception if the queue is empty.
   member Head: struct ('K * 'V)

   /// O(1). Returns the entry with the minimum value in this queue. Returns None if the queue is empty.
   member TryHead: option<struct('K * 'V)>

   /// O(lgN). Returns the entry with the minimum value in this queue, and a queue with that entry removed.  Throws
   /// an exception if the queue is empty.
   member RemoveHead: struct('K * 'V * PrioritySearchQueue<'K, 'V>)

   /// O(lgN). Returns the entry with the minimum value in this queue, and the queue with that entry removed. Returns
   /// None if the queue is empty.
   member TryRemoveHead: option<struct('K * 'V * PrioritySearchQueue<'K, 'V>)>

   /// O(lgN). Returns the value associated with the specified key. Returns None if the queue does not contain an 
   /// entry with the key.
   member TryFind: key:'K -> option<'V>

   /// O(lgN). Returns a value indicating if the queue contains an entry for the specified key.
   member ContainsKey: key:'K -> bool

   /// O(lgN). Indexer that returns the value associated with the specified key. Throws an exception if the queue 
   /// does not contain an entry with the key.
   member Item : key:'K -> 'V with get

   /// O(lgN). Adds the specified key and value to this queue, replacing an existing entry if necessary.
   member Add: key:'K * value:'V -> PrioritySearchQueue<'K, 'V>

   /// O(lgN). Removes the entry with the specified key to this queue, and returns an updated queue.  This queue is 
   /// returned unchanged if there is no matching entry.
   member Remove: key:'K -> PrioritySearchQueue<'K, 'V>

   /// O(N) worst case. Returns a list of entries, in ascending order by key, that contain values from this queue that
   /// are less than or equal to the specified value.
   member AtMost: value:'V -> list<'K*'V>
 

/// Functional operators for <c>PrioritySearchQueue<_, _></c> type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PrioritySearchQueue =   
   
   /// O(1). Returns a empty queue.
   [<GeneralizableValue>]
   [<CompiledName("Empty")>]
   val empty<'K, 'V when 'K: comparison and 'V: comparison> : PrioritySearchQueue<'K, 'V>

   /// O(N^2), O(NlgN) on average.  Returns a new queue containing the items in the specified sequence
   [<CompiledName("OfSeq")>]
   val ofSeq: items:seq<'K*'V> -> PrioritySearchQueue<'K, 'V>

   /// O(N): Returns a new queue containing the items in the specified sequence, which *must* be ordered by key, in 
   /// ascending order.
   [<CompiledName("OfOrderedSeq")>]
   val ofOrderedSeq: items:seq<'K*'V> -> PrioritySearchQueue<'K, 'V>

   /// O(N): Returns a new queue containing the items in the specified sequence, which *must* be ordered by key, in 
   /// ascending order.
   [<CompiledName("OfArray")>]
   val ofArray: items:('K*'V)[] -> PrioritySearchQueue<'K, 'V>

   /// O(1). Returns a new queue contaning an entry with the specified key and value.
   [<CompiledName("Singleton")>]
   val singleton: key:'K -> value:'V-> PrioritySearchQueue<'K, 'V>

   /// O(1). Returns a value indicating if the queue is empty.
   [<CompiledName("IsEmpty")>]
   val isEmpty: queue:PrioritySearchQueue<'K, 'V> -> bool

   /// O(1). Returns the number of entries in the queue.
   [<CompiledName("Length")>]
   val length: queue:PrioritySearchQueue<'K, 'V> -> int

   /// O(1). Returns the entry with the minimum value in the queue. Throws an exception if the queue is empty.
   [<CompiledName("Head")>]
   val head: queue:PrioritySearchQueue<'K, 'V> -> struct ('K * 'V)

   /// O(1). Returns the entry with the minimum value in the queue. Returns None if the queue is empty.
   [<CompiledName("TryHead")>]
   val tryHead: queue:PrioritySearchQueue<'K, 'V> -> option< struct ('K * 'V) >

   /// O(lgN). Returns the value associated with the specified key in the queue. Throws an exception if the queue does 
   /// not contain an entry with the key.
   [<CompiledName("Find")>]
   val find: key:'K -> queue:PrioritySearchQueue<'K, 'V> -> 'V 

   /// O(lgN). Returns the value associated with the specified key in the queue. Returns None if the queue does not 
   /// contain an entry with the key.
   [<CompiledName("TryFind")>]
   val tryFind: key:'K -> queue:PrioritySearchQueue<'K, 'V> -> option<'V>

   /// O(lgN). Returns a value indicating if the queue contains an entry with the specfied key.
   [<CompiledName("ContainsKey")>]
   val containsKey: key:'K -> queue:PrioritySearchQueue<'K, 'V> -> bool

   /// O(lgN). Adds the specified key and value to the queue, replacing an existing entry if necessary.
   [<CompiledName("Add")>]
   val add: key:'K -> value:'V -> queue:PrioritySearchQueue<'K, 'V> -> PrioritySearchQueue<'K, 'V>

   /// O(lgN). Returns the entry with the minimum value in the queue, and the queue with that entry removed. Throws
   /// an exception if the queue is empty.
   [<CompiledName("RemoveHead")>]
   val removeHead: queue:PrioritySearchQueue<'K, 'V> -> struct ('K * 'V * PrioritySearchQueue<'K, 'V>)

   /// O(lgN). Returns the entry with the minimum value in the queue, and the queue with that entry removed. Returns
   /// None if the queue is empty.
   [<CompiledName("TryRemoveHead")>]
   val tryRemoveHead: queue:PrioritySearchQueue<'K, 'V> -> option< struct ('K * 'V * PrioritySearchQueue<'K, 'V>) >

   /// O(lgN). Removes the entry with the specified key to the queue, and returns an updated queue.  The queue is returned
   /// unchanged if there is no matching entry.
   [<CompiledName("Remove")>]
   val remove: key:'K -> queue:PrioritySearchQueue<'K, 'V> -> PrioritySearchQueue<'K, 'V>

   /// O(N) worst case. Returns a list of entries, in ascending order by key, that contain values from the specified 
   /// queue that are less than or equal to the specified value.
   [<CompiledName("AtMost")>]
   val atMost: value:'V -> queue:PrioritySearchQueue<'K, 'V> -> list<'K*'V>

   /// O(N). Returns a new queue whose values are the results of applying the given function to each of the 
   /// elements of the queue. The key passed to the function indicates the key of element being transformed.
   [<CompiledName("Map")>]
   val map: f:('K -> 'V -> 'U) -> queue:PrioritySearchQueue<'K, 'V> -> PrioritySearchQueue<'K, 'U>

   /// O(N). Returns a queue containing entries from the specified queue for which the specified predicate function
   /// returns true.
   [<CompiledName("Filter")>]
   val filter: predicate:('K -> 'V -> bool) -> PrioritySearchQueue<'K, 'V> -> PrioritySearchQueue<'K, 'V>

   /// O(N). Applies the specified function to each entry in the queue, threading an accumulator argument through the
   /// computation. The fold function takes the second argument, and applies the function f to it and the first element
   /// of the list. Then, it feeds this result into the function f along with the second element, and so on. It returns
   /// the final result.
   [<CompiledName("Fold")>]
   val fold: f:('State -> 'K -> 'V -> 'State) -> initial:'State -> queue:PrioritySearchQueue<'K, 'V> -> 'State

   /// O(N). Tests if any binding in the queue satisfies the given predicate.
   [<CompiledName("Exists")>]
   val exists: predicate:('K -> 'V -> bool) -> queue:PrioritySearchQueue<'K, 'V> -> bool

   /// O(N). Tests if all bindings in the queue satisfy the given predicate.
   [<CompiledName("ForAll")>]
   val forAll: predicate:('K -> 'V -> bool) -> queue:PrioritySearchQueue<'K, 'V> -> bool

   /// O(N). Searches the queue looking for the first entry where the given function returns a Some value. Throws
   /// an exception if function returns None for all entries.
   [<CompiledName("Pick")>]
   val pick: chooser:('K -> 'V -> 'U option) -> queue:PrioritySearchQueue<'K, 'V> -> 'U

   /// O(N). Searches the queue looking for the first element where the given function returns a Some value. 
   [<CompiledName("TryPick")>]
   val tryPick: chooser:('K -> 'V -> 'U option) -> queue:PrioritySearchQueue<'K, 'V> -> 'U option

   /// O(N). Applies the function to each entry in the queue.
   [<CompiledName("Iter")>]
   val iter: f:('K -> 'V -> unit) -> queue:PrioritySearchQueue<'K, 'V> -> unit

   /// O(1), iteration is O(N). Returns a sequence that iterates the entries in the queue, in ascending order by key.
   [<CompiledName("ToSeq")>]
   val toSeq: queue:PrioritySearchQueue<'K, 'V> -> seq<'K * 'V>

   /// O(1), iteration is O(N). Returns a new list containing the entries in the queue, in ascending order by key.
   [<CompiledName("ToList")>]
   val toList: queue:PrioritySearchQueue<'K, 'V> -> list<'K * 'V>

   /// O(1), iteration is O(N). Returns a new array containing the entries in the queue, in ascending order by key.
   [<CompiledName("ToArray")>]
   val toArray: queue:PrioritySearchQueue<'K, 'V> -> struct('K * 'V)[]


   

  

