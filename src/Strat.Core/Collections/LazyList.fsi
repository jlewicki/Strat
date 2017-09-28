namespace Strat.Collections

open System.Collections
open System.Collections.Generic


/// <summary>
/// LazyList is a list-like data structure that provides constant-time access, addition, and removal of the element at
/// the front of the list. Unlike a traditional list, elements in the lazy list are computed lazily, on first demand. In
/// this way, a lazy list is similar to a sequence. However, a lazy list will cache its elements once computed, such that 
/// are only computed once.
/// </summary>
[<Class; Sealed>]
type LazyList<'T> =

   interface IEnumerable
   interface IEnumerable<'T>
   
   /// Returns an empty list.
   static member Empty: LazyList<'T>

   /// O(N). Returns the number of items in the list. Note that this causes the evaluation of all the elements of the 
   /// list.
   member Length: unit -> int

   /// O(1). Returns a value indicating if this list is empty.
   member IsEmpty: bool

   /// O(1). Returns the first item in the list. Throws an exception if the index is out of range.
   member Head: 'T

   /// O(1). Returns the first item in the list, or None if the list is empty.
   member TryHead: option<'T>

   /// O(1). Returns a list containing the items in this ;ist, except the first. Throws an exception if the list is 
   /// empty.
   member Tail: LazyList<'T>

   /// O(1). Returns a list containing the items in this list, except the first. Returns None if the list is empty.
   member TryTail: option<LazyList<'T>>

   /// O(1). Returns new list containing the item, followed by items in this list.
   member Cons: item:'T -> LazyList<'T>

   /// O(1). Returns a pair consisting of the item at the head of this list, and the tail of this list. Throws an 
   /// exception if the list is empty.
   member Uncons: unit -> struct ('T * LazyList<'T>)

   /// O(1). Returns a pair consisting of the item at the head of this list, and the tail of this list. Returns None 
   /// if the list is empty.
   member TryUncons: unit -> option<struct ('T * LazyList<'T>)>

 
 /// Functional operators for <c>LazyList<_></c>.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LazyList = 

   /// O(1). Active pattern for decomposing a lazy list. A lazy list can be viewed as either an empty list, or a Cons 
   /// cell containing the element at the head of the list, and the rest of the list.
   val (|Empty|Cons|) : LazyList<'T> -> Choice<unit,('T * LazyList<'T>)>

   /// O(1). Returns a empty list.
   [<GeneralizableValue>]
   [<CompiledName("Empty")>]
   val empty<'T> : LazyList<'T>

   /// O(N): Returns a new list containing the items in the specified sequence.
   [<CompiledName("OfSeq")>]
   val ofSeq: items:seq<'T> -> LazyList<'T>

   /// O(N): Returns a new list containing the items in the specified array.
   [<CompiledName("OfList")>]
   val ofList: items:'T list -> LazyList<'T>

   /// O(N): Returns a new list containing the items in the specified array.
   [<CompiledName("OfArray")>]
   val ofArray: items:'T[] -> LazyList<'T>

   /// O(1), O(N) to iterate. Generates a new list which, when iterated, will return successive elements by calling the
   /// given function, up to the given count.
   [<CompiledName("Init")>]
   val init : count:int -> f:(int -> 'T) -> LazyList<'T>

   /// O(1), O(N) to iterate. Generates new list of the specified length with every element set to the given value.
   [<CompiledName("Replicate")>]
   val replicate: length:int -> value:'T -> LazyList<'T>

   /// O(1). Generates a new, possibly infinite, list that contains the elements generated by the given computation.
   [<CompiledName("Unfold")>]
   val unfold: ('State -> ('T * 'State) option) -> 'State -> LazyList<'T>

   /// O(1), O(N+M) to iterate. Returns a new list that contains, when iterated, the elements of the first list, 
   /// followed by the elements the second list.
   [<CompiledName("Append")>]
   val append: list1:LazyList<'T> -> list2:LazyList<'T> -> LazyList<'T>

   /// O(1). Returns a value indicating if the list is empty.
   [<CompiledName("IsEmpty")>]
   val isEmpty: list:LazyList<'T> -> bool

   /// O(N): Returns the number of items in the list. Note the entrire list must be traversed in order to determine the 
   /// length.
   [<CompiledName("Length")>]
   val length: list:LazyList<'T> -> int

   /// O(1). Returns the first item in the list. Throws an exception if the index is out of range.
   [<CompiledName("Head")>]
   val head: list:LazyList<'T> -> 'T

   /// O(1). Returns the first item in the list, or None if the list is empty.
   [<CompiledName("TryHead")>]
   val tryHead: list:LazyList<'T> -> option<'T>

   /// O(1). Returns a list containing the elements in the list, except the first. Throws an exception if the list is 
   /// empty.
   [<CompiledName("Tail")>]
   val tail: list:LazyList<'T> -> LazyList<'T>

   /// O(1). Returns a list containing the elements in the list, except the first. Returns None if the list is 
   /// empty.
   [<CompiledName("TryTail")>]
   val tryTail: list:LazyList<'T> -> option<LazyList<'T>>

   /// O(1). Returns new list containing the item, followed by elements in the list.
   [<CompiledName("Cons")>]
   val cons: item:'T -> list:LazyList<'T> -> LazyList<'T>

   /// O(1). Returns a pair consisting of the item at the head of the list, and the tail of the list. Throws an 
   /// exception if the list is empty.
   [<CompiledName("Uncons")>]
   val uncons: list:LazyList<'T> -> struct ('T * LazyList<'T>)

   /// O(1). Returns a pair consisting of the item at the head of the list, and the tail of the list. Throws an 
   /// exception if the list is empty.
   [<CompiledName("TryUncons")>]
   val tryUncons: list:LazyList<'T> -> option<struct ('T * LazyList<'T>)>

   /// O(1), O(N) to iterate. Returns a new list whose elements, when iterated, are the results of applying the 
   /// specified function to each of the elements of the list.
   [<CompiledName("Map")>]
   val map: f:('T -> 'U) -> list:LazyList<'T> -> LazyList<'U>

   /// O(1), O(N) to iterate. Returns a new list whose elements are the results of applying the specified function to 
   /// each of the elements of the list. The integer index passed to the function indicates the index of element being
   /// transformed.
   [<CompiledName("MapIndexed")>]
   val mapi: f:(int -> 'T -> 'U) -> list:LazyList<'T> -> LazyList<'U>

   /// O(1), O(N) to iterate. Returns a new list containing, when iterated, only the elements of the list for which the 
   /// specified predicate returns true.
   [<CompiledName("Filter")>]
   val filter: predicate:('T -> bool) -> list:LazyList<'T> -> LazyList<'T>

   /// O(N). Applies the specified function to each element of the list, threading an accumulator argument through the
   /// computation. The fold function takes the second argument, and applies the function f to it and the first element
   /// of the list. Then, it feeds this result into the function f along with the second element, and so on. It returns
   /// the final result. 
   [<CompiledName("Fold")>]
   val fold: f:('State -> 'T -> 'State) -> initial:'State -> list:LazyList<'T> -> 'State

   /// O(1), O(N) to iterate. Applies the given function to each element of the list and returns the list comprised of
   /// the results for each element where the function returns Some with some value.
   [<CompiledName("Choose")>]
   val choose: f:('T -> 'U option) -> LazyList<'T> -> LazyList<'U>

   /// O(1), O(N*M) to iterate. Returns a list that, when iterated, applies the given function to each element of the 
   /// specified list. Each result is concatenated into a combined list.
   [<CompiledName("Collect")>]
   val collect: f:('T -> LazyList<'U>) -> list:LazyList<'T> -> LazyList<'U>

   /// O(N). Applies the specified function to each element in the list.
   [<CompiledName("Iterate")>]
   val iter: f:('T -> unit) -> list:LazyList<'T> -> unit

   /// O(N). Applies the specified function to each element of the list. The integer passed to the function indicates
   /// the index of element.
   [<CompiledName("IterateIndexed")>]
   val iteri: f:(int -> 'T -> unit) -> list:LazyList<'T> -> unit

   /// O(1), O(N) to iterate. Returns a new list containing, when iterated, the elements of the list in reverse order.
   [<CompiledName("Reverse")>]
   val rev: v: LazyList<'T> -> LazyList<'T> 

   /// O(N). Tests if any element in the list satisfies the given predicate.
   [<CompiledName("Exists")>]
   val exists: predicate:('T -> bool) -> list:LazyList<'T> -> bool

   /// O(N). Returns the first element for which the specified function returns true. Return None if no such element 
   /// exists.
   [<CompiledName("TryFind")>]
   val tryFind: predicate:('T -> bool) -> list:LazyList<'T> -> option<'T>

   /// O(N). Returns the first element for which the specified function returns true. Raises KeyNotFoundException if no 
   /// such element exists.
   [<CompiledName("Find")>]
   val find: predicate:('T -> bool) -> list:LazyList<'T> -> 'T

   /// O(N). Applies the given function to successive elements, returning the first result where function returns Some 
   /// for an element. Raises KeyNotFoundException if no such element exists.
   [<CompiledName("Pick")>]
   val pick: chooser:('T -> 'U option) -> list:LazyList<'T> -> 'U

   /// O(N). Applies the given function to successive elements, returning the first result where function returns Some 
   /// for an element. Returns None if no such element exists.
   [<CompiledName("TryPick")>]
   val tryPick: chooser:('T -> 'U option) -> list:LazyList<'T> -> 'U option

   /// O(N). Returns the item in the list at the specified index. The first element has index 0. Throws an
   [<CompiledName("Get")>]
   val get: idx:int -> list:LazyList<'T> -> 'T

   /// O(N). Returns a value indicating if all elements in the list satisfy the given predicate.
   [<CompiledName("ForAll")>]
   val forall: predicate:('T -> bool) -> list:LazyList<'T> -> bool

   /// O(1). Views the specified list as a sequence.
   [<CompiledName("ToSeq")>]
   val toSeq: list:LazyList<'T> -> seq<'T>

   /// O(N). Creates a new array containing the elements in the specfied list.
   [<CompiledName("ToArray")>]
   val toArray: list:LazyList<'T> -> 'T[]

   /// O(N). Creates a new list containing the elements in the specfied index list.
   [<CompiledName("ToList")>]
   val toList: list:LazyList<'T> -> list<'T>