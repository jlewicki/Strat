namespace Strat.Collections.Primitives

open System
open System.Collections
open System.Collections.Generic
open Strat.Collections



// An implementation of persistent queues, as described in the paper 'Simple and Efficient Purely Functional Queues 
// and Deques' by C. Okasaki.
module ListQueue = 

   [<Struct; NoComparison; NoEquality>]
   type ListQueue<'T> = {
       // Number of items in the queue 
       Count: int
       // Items in this list are at the front of the queue. Dequeues are taken from here.
       Left: LazyList<'T>
       // This is a 'pointer' to a position in the left list marking the boundary between the evaluated and unevaluated 
       // portions of the left list. 
       PrevaledLeft: LazyList<'T>
       // Items in this list are at the back of the queue (in reverse order). Enqueues are added to here.
       Right :LazyList<'T>
   } 


   let newQueue count left right prevaledLeft =
      { Count=count; Left=left; Right=right; PrevaledLeft=prevaledLeft }


   let rec rotate (left: LazyList<'T>) (right: LazyList<'T>) (prevaledLeft: LazyList<'T>) = 
      match left with
      | LazyList.Empty -> LazyList.cons right.Head prevaledLeft
      | LazyList.Cons (headLeft, restLeft) -> 
         match right with
         | LazyList.Empty -> prevaledLeft
         | LazyList.Cons (headRight, restRight) -> 
            let prevaledLeft = rotate restLeft restRight (LazyList.cons headRight prevaledLeft)
            LazyList.cons headLeft prevaledLeft


   let balance (left: LazyList<'T>) (right: LazyList<'T>) (prevaledLeft: LazyList<'T>) = 
      match prevaledLeft with
      | LazyList.Empty -> 
         let newLeft = rotate left right prevaledLeft
         struct (newLeft, LazyList.empty, newLeft)
      | LazyList.Cons (_, prevaledRest) -> 
         struct (left, right, prevaledRest)  


   let length (q: ListQueue<'T>) = 
      q.Count


   let head (q: ListQueue<'T>) = 
      if q.Count = 0 then invalidOp "Queue is empty"
      else q.Left.Head


   let enqueue item (queue: ListQueue<'T>) = 
      let right = LazyList.cons item queue.Right
      let struct (left, right, prevaledLeft) = balance queue.Left right queue.PrevaledLeft
      newQueue (queue.Count + 1) left right prevaledLeft

   let dequeue (queue: ListQueue<'T>) = 
      if queue.Count = 0 then 
         invalidOp "Queue is empty"
      else
         let struct (head, rest) = LazyList.uncons queue.Left
         let struct (left, right, prevaledLeft) = balance rest queue.Right queue.PrevaledLeft
         struct (head, newQueue (queue.Count - 1) left right prevaledLeft)


   type ListQueueEnumerator<'T> (queue: ListQueue<'T>) = 
      let notStarted() = 
         raise <| new InvalidOperationException("The enumerator has not been started by a call to MoveNext")
      let alreadyCompleted() = 
         raise <| new InvalidOperationException("The enumerator has already completed.")
      let mutable isStarted = false
      let mutable currentQueue = queue

      let current() =
         if isStarted then queue |> head
         else notStarted()
      
      let moveNext() =
         if isStarted then 
            if currentQueue.Count = 0 then alreadyCompleted()
            else 
               let struct (_, rest) = currentQueue |> dequeue
               currentQueue <- rest
               currentQueue.Count > 0
         else
             isStarted <- true
             currentQueue.Count > 0

      interface IEnumerator<'T> with
         member x.Current = current()
      interface IEnumerator with 
         member x.Current = box (current())
         member x.MoveNext() = moveNext()
         member x.Reset() = currentQueue <- queue
      interface IDisposable with 
         member x.Dispose() = () 


namespace Strat.Collections

open System
open System.Collections
open System.Collections.Generic
open Strat.Collections.Primitives
open Strat.Collections.Primitives.ListQueue


[<Sealed; NoComparison; NoEquality>]
type Queue<'T> private (q: ListQueue<'T>) = 

   static let empty = new Queue<'T> (newQueue 0 LazyList.empty LazyList.Empty LazyList.empty)
   static member Empty = empty


   member this.IsEmpty = 
      q.Count = 0


   member this.Count = 
      q.Count


   member this.Head = 
      q |> head


   member this.Enqueue item =
      new Queue<'T> (q |> ListQueue.enqueue item)

   
   member this.Dequeue() =
      let struct (item, rest) = q |> ListQueue.dequeue
      struct (item, new Queue<'T> (rest))


   interface IEnumerable with
      member this.GetEnumerator() = 
        new ListQueueEnumerator<'T> (q) :> IEnumerator


   interface IEnumerable<'T> with 
      member this.GetEnumerator() = 
         new ListQueueEnumerator<'T> (q)  :> IEnumerator<'T>



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Queue = 
   
   [<GeneralizableValue>]
   [<CompiledName("Empty")>]
   let empty<'T> = Queue<'T>.Empty

   [<CompiledName("IsEmpty")>]
   let isEmpty (queue:Queue<'T>) = 
      queue.IsEmpty

   [<CompiledName("IsEmpty")>]
   let length (queue:Queue<'T>) = 
      queue.Count

   [<CompiledName("Head")>]
   let head (queue:Queue<'T>) = 
      queue.Head

   [<CompiledName("Enqueue")>]
   let enqueue (item:'T) (queue:Queue<'T>) =
      queue.Enqueue item

   [<CompiledName("Dequeue")>]
   let dequeue (queue:Queue<'T>) =
      queue.Dequeue()




   
