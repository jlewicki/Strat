namespace Strat.Collections

open System
open System.Collections
open System.Collections.Generic
open System.Threading


// Persistent vector, implemented as bit-partitioned vector trie.
//
// This implementation is very similar to Clojure's PersistentVector, with an additional tweak strongly-typed arrays
// in leaf nodes, to avoid boxing overhead.
//
// Some helpful background links:
// http://hypirion.com/musings/understanding-persistent-vector-pt-2
// https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/PersistentVector.java

// Internal implementation details, shared between TransientVector and Vector
module Trie =

   // Number of bits to represent an index into the array stored in each node of the tree
   let Bits = 5
   // Number of elements in the array stored in each node of the tree (2^5 = 32)
   let NodeArraySize = 1 <<< Bits 
   // Bitmask to appy to obtain the index of an item in the node array (31, or 0x1f)
   let NodeArrayMask = NodeArraySize - 1 // 31, or 0x1f


   [<NoEquality; NoComparison>]
   type Node<'T> =
      | Interior of OwnerThread:Ref<Thread> * Items:Node<'T>[]
      | Leaf of OwnerThread:Ref<Thread> * Items:'T[]  
   with
      member this.OwnerThread = 
         match this with 
         | Interior(ot, _) -> ot
         | Leaf(ot, _) -> ot
      member this.Copy() = 
         match this with 
         | Interior(ot, arr) -> Interior (ot, Array.copy arr)
         | Leaf(ot, arr) -> Leaf (ot, Array.copy arr)
      member this.SetOwnerThread ownerThread = 
         match this with 
         | Interior(_, arr) -> Interior (ownerThread, arr)
         | Leaf(_, arr) -> Leaf (ownerThread, arr)
     

   let noEditThread : Ref<Thread> = ref null
   let inline nullNode() = Unchecked.defaultof<Node<'T>>
   let isNullNode (node: Node<'T>) = 
      Object.ReferenceEquals(node, nullNode())

   // Returns the total number of elements in the 'tree part' of the vector (as opposed to the 'tail part'). In other 
   // words: vector.size - tail.size
   let tailOffset (size:int) : int = 
      if size < NodeArraySize then 0
      else ((size - 1) >>> Bits) <<< Bits


   // Returns the index for accessing a level array corresponding to the specified index (into the persistent vector)
   let inline arrayIndex (vectorIndex: int) = 
      vectorIndex &&& NodeArrayMask


   let newNodeArray() = 
      Array.zeroCreate NodeArraySize   


   // Returns the array that contains the element at the specified index
   let leafArrayFor (index: int, count: int, shift: int, root: Node<'T>, tail: 'T[]) : 'T[] = 
      if index >= tailOffset count then 
         // Element at the index is within the 'tail part', so just return the tail array
         tail
      else
         // Element at the index is within the 'tree part', so we need to traverse the tree. Visit each level of the
         // trie by calculating the index in each level of the child to descend into.
         let mutable node = root
         let mutable level = shift
         let mutable leafArray = Unchecked.defaultof<'T[]>
         while level >= 0 do
            let arrIdx = arrayIndex (index >>> level)
            match node with 
            | Interior(_, arr) ->
               node <- arr.[arrIdx]
               level <- level - Bits
            | Leaf(_, arr) ->
               leafArray <- arr
               level <- -1
         leafArray


   // Returns a new node representing the root of a tree with the specified level, and the specified node as the first
   // leaf node in the tree.
   let rec newPath (ownerThread, level: int, node: Node<'T>) : Node<'T> =
      if level = 0 then
         node
      else
         let newArr : Node<'T>[] = newNodeArray()
         newArr.[0] <- newPath (ownerThread, level - Bits, node)   
         Interior (ownerThread, newArr)


   // Returns a new node (at the specified level) with the item at the specified index updated with the 
   // specified value.
   let rec setTree (level: int) (node: Node<'T>) (index: int) (item: 'T) : Node<'T> = 
      match node with
      | Interior(ot, arr) ->
         let newArray = Array.copy arr
         let childIndex = (index >>> level) &&& NodeArrayMask
         let childNode = arr.[childIndex]
         newArray.[childIndex] <- setTree (level - Bits) childNode index item
         Interior(ot, newArray)
      | Leaf(ot, arr) ->
         let newArray = Array.copy arr
         newArray.[arrayIndex index] <- item 
         Leaf(ot, newArray)


   let mkPersistentParent (parent: Node<'T>) = parent.Copy()


   // Returns a new tree with the specified tail node as the rightmost leaf node in the tree
   // Because we will need to create a new node(s) in the tree, and the way it is created varies
   // between persistent and transient cases, we pass a function to create the ndoe
   let rec pushTail (mkParent: Node<'T> -> Node<'T>, size: int, level: int, parent: Node<'T>, tail: Node<'T>) : Node<'T> =
      let newParent = mkParent parent
      let childIndex = ((size - 1) >>> level) &&& NodeArrayMask
      match parent, newParent with
      | Interior (_, arr), Interior (_, newArr) ->
         let childNode = 
            if level = Bits then
               // We're at the leaf of the tree, so we can directly insert the tail node
               tail
            else
               let child = arr.[childIndex]
               if isNullNode child then
                  // No subtree yet at this index, so create a minimal path to the tail node
                  newPath (newParent.OwnerThread, level - Bits, tail)
               else
                  // Subtree exists, so push the tail into the subtree
                  pushTail (mkParent, size, level - Bits, child, tail)
         newArr.[childIndex] <- childNode
         newParent
      | _ ->
         invalidOp "Unexpected leaf nodes"
      

open Trie

// Persistent vector implementation.
//
// count: number of elements stored in the vector.
// shift: (Depth of tree - 1) * Bits per level. In other words, this is the total number of bits a
//        vector index will have to be shifted to get to the leaf value
// root: The root node of the 'tree part' of the vector.
// tail: 'Tail part' of the vector. Logically, the tail part is the rightmost leaf in the tree. Keeping a 
//       direct reference to this leaf array in the vector, instead of in the 'tree part', allows some important
//       optimizations to be performed.
[<Sealed>]
type Vector<'T> internal (count:int, shift: int, root: Node<'T>, tail: 'T[]) =  

   static let emptyInteriorNode : Node<'T> = Interior(noEditThread, newNodeArray())
   static let empty = new Vector<'T> (0, Trie.Bits, emptyInteriorNode, Array.empty) 
   static member Empty = empty
  

   new (items: seq<'T>) = 
      if isNull items then 
         raise <| ArgumentNullException("items")
      let tv : TransientVector<'T> = empty.ToTransient()
      items |> Seq.iter (tv.Add >> ignore)
      let v = tv.ToPersistent()
      // It would be nice to return v directly, but F# requires a explicit construtor invocation as the return value.
      new Vector<'T> (v.Count, v.Shift, v.Root, v.Tail)


   new (items: ICollection<'T>) =
      if isNull items then 
         raise <| ArgumentNullException("items")
      let size = items.Count
      if size <= Trie.NodeArraySize then
         // Small collection that can fit in the tail part
         let arr = Array.zeroCreate size
         items.CopyTo (arr, 0)
         new Vector<'T> (size, Trie.Bits, emptyInteriorNode, arr)
      else
         new Vector<'T> (items :> seq<'T>)


   member this.Count 
      with get() = count


   member this.IsEmpty
      with get() = count = 0


   member this.Item 
      with get (index: int) : 'T =
         if index < 0 || index >= count then 
            raise <| IndexOutOfRangeException (index.ToString())
         let arr = Trie.leafArrayFor (index, count, shift, root, tail)
         arr.[arrayIndex index]


   member this.Set(index: int, item: 'T)  = 
      if index < 0 || index >= count then
         raise <| IndexOutOfRangeException (index.ToString())
      if index >= tailOffset count then
         // Index points to an item in the tail part, so just create a new tail
         let newTail = Array.copy tail
         newTail.[arrayIndex index] <- item
         Vector<'T> (count, shift, root, newTail)
      else
         // Index points to an item in the tree part, so update the tree
         new Vector<'T> (count, shift, (setTree shift root index item), tail)


   member this.Add (item: 'T) = 
      if count - (tailOffset count) < Trie.NodeArraySize then 
         // There is room in the 'tail part' array, so store the item there
         let newTail = Array.zeroCreate (tail.Length + 1)
         System.Array.Copy(tail, newTail, tail.Length)
         newTail.[tail.Length] <- item
         Vector<'T>(count + 1, shift, root, newTail)
      else
         // No room in tail part, so move tail into the tree
         let tailNode = Leaf (root.OwnerThread, tail)
         let mutable newRoot = nullNode()
         let mutable newShift = shift
         if (count >>> Trie.Bits) > (1 <<< shift) then
            // No room in tree part for the tail, so create a new level in the tree
            let newArr = newNodeArray()
            newArr.[0] <- root
            newArr.[1] <- newPath (root.OwnerThread, shift, tailNode)
            newRoot <- Interior (root.OwnerThread, newArr)
            newShift <- shift + Trie.Bits
         else 
            // There is room in the tree for the tail, so push the tail into the tree
            newRoot <- pushTail (mkPersistentParent, count, shift, root, tailNode)
        
         // New vector, with the added item stored in the tail
         new Vector<'T> (count + 1, newShift, newRoot, Array.singleton item)


   member this.RemoveLast() : 'T * Vector<'T> =
      if count = 0 then 
         raise <| InvalidOperationException("Cannot call Remove() on an empty vector.")
      elif count = 1 then
         tail.[tail.Length - 1], empty
      else
         if (count - tailOffset count) > 1 then
            // Last item is in the tail, so build a new tail with the item removed
            let last = tail.[tail.Length - 1]
            let newTail = Array.zeroCreate (tail.Length - 1)
            Array.Copy (tail, newTail, newTail.Length)
            last, Vector<'T> (count - 1, shift, root, newTail)
         else
            let last = this.[count - 1]
            // Move contents of last node into new tail array
            let newTail = leafArrayFor (count - 2, count, shift, root, tail)
            let mutable newRoot = this.RemoveLastLeaf (shift, root)
            let mutable newShift = shift
            if isNullNode newRoot then 
               newRoot <- emptyInteriorNode
            if shift > Trie.Bits then  
               match newRoot with
               | Interior(_, arr) ->
                  if isNullNode arr.[1] then
                     newRoot <- arr.[0]
                     newShift <- newShift - Trie.Bits
               | Leaf(_) -> 
                  invalidOp "Unexpected leaf node"
            // Move contents of last node into tail array
            last, new Vector<'T> (count - 1, newShift, newRoot, newTail)


   member this.Map (f: 'T -> 'U) : Vector<'U> =
      let mutable t = TransientVector<'U>()
      for item in this do 
         t <- t.Add (f item)
      t.ToPersistent()


   member this.Iterate (f: 'T -> unit)  = 
      for item in this do 
         f item


   interface IEnumerable with
      member this.GetEnumerator() = 
         this.CreateEnumerator(0, count) :> IEnumerator


   interface IEnumerable<'T> with
      member this.GetEnumerator() = 
         this.CreateEnumerator(0, count)


   member internal this.Root = root
   member internal this.Shift = shift
   member internal this.Tail = tail
   member internal this.ToTransient() = new TransientVector<'T> (this)


   // Returns a new node that represents the specfied node, with its last node removed
   member private this.RemoveLastLeaf (level: int, node: Node<'T>) : Node<'T> = 
      let childIndex = arrayIndex ((count - 2) >>> level)
      if level > Trie.Bits then
         match node with
         | Interior(_, arr) ->
            // Descend deeper into tree
            let newChild = this.RemoveLastLeaf (level - Trie.Bits, arr.[childIndex])
            if isNullNode newChild && childIndex = 0 then
               nullNode()
            else
               let newArr = Array.copy arr
               newArr.[childIndex] <- newChild
               Interior (root.OwnerThread, newArr)
         | Leaf(_) -> 
            invalidOp "Unexpected leaf node"
      elif childIndex = 0 then
         nullNode()
      else
         match node with
         | Interior(_, arr) ->
            let newArr = Array.copy arr
            newArr.[childIndex] <- nullNode()
            Interior(root.OwnerThread, newArr)
         | Leaf(_) -> 
            invalidOp "Unexpected leaf node"


   // Creates a new enumerator that yields values between startIndex (inclusive) and endIndex (exclusive).
   member private this.CreateEnumerator (startIdx: int, endIdx: int) = 
      let mutable i = -1
      let mutable baseI = -1
      let mutable leafArray = null
      let mutable currentItem = Unchecked.defaultof<'T>
      let reset() = 
         i <- startIdx - 1
         leafArray <- 
            if startIdx < count then leafArrayFor (startIdx, count, shift, root, tail) 
            else null
         baseI <- startIdx - (startIdx % Trie.Bits)
      let current() = 
         if i = startIdx - 1 then 
            raise <| new InvalidOperationException("The enumerator has not been started by a call to MoveNext")
         else
            currentItem

      reset() 
      { new IEnumerator<'T> with
            member x.Current = current()
        interface IEnumerator with 
            member x.Current = box (current())
            member x.MoveNext() = 
               // Onetime bookeeping for starting iteration
               if i = startIdx - 1 then i <- startIdx
               if i < endIdx then
                  if (i - baseI) = Trie.NodeArraySize then
                     // We've iterated thrugh the current leaf array, so get the next one
                     leafArray <- leafArrayFor (i, count, shift, root, tail)
                     baseI <- baseI + Trie.NodeArraySize
                  currentItem <- leafArray.[arrayIndex i]
                  i <- i + 1
                  true
               else
                  false
            member x.Reset() = reset()
        interface IDisposable with 
            member x.Dispose() = () } 


// Mutable vector, to allow for faster updates. Not threadsafe.
and internal TransientVector<'T> (count:int, shift: int, root: Node<'T>, tail: 'T[]) =
   
   static let emptyInteriorNode : Node<'T> = Interior(noEditThread, newNodeArray())

   static let editableNode (node: Node<'T>) : Node<'T> =
      match node with
      | Interior(ot, arr) -> Interior(ref Thread.CurrentThread, Array.copy arr)   
      | Leaf(ot, arr) -> Leaf(ref Thread.CurrentThread, Array.copy arr)   


   static let editableTail (tail: 'T[]) : 'T[] = 
      let editable = newNodeArray()
      Array.Copy(tail, editable, tail.Length)
      editable

   let mutable count = count
   let mutable shift = shift
   let mutable root = root
   let mutable tail = tail

   member this.Root = root
   member this.Shift = shift
   member this.Tail= tail

   new() = 
      TransientVector<'T>(0, Bits, editableNode emptyInteriorNode, editableTail Array.empty)


   new (v: Vector<'T>) = 
         TransientVector<'T>(v.Count, v.Shift, editableNode v.Root, editableTail v.Tail)


   member this.Count 
      with get() : int = 
         this.EnsureEditable()
         count


   member this.Add (item: 'T) : TransientVector<'T> = 
      this.EnsureEditable()
      let i = count
      if (i - tailOffset count) < Trie.NodeArraySize then
         // There is room in the 'tail part' array, so store the item there
         tail.[arrayIndex i] <- item
         count <- count + 1
         this
      else
         let tailNode = Leaf (root.OwnerThread, tail)
         tail <- newNodeArray()
         tail.[0] <- item
         let mutable newRoot = nullNode()
         let mutable newShift = shift
         if (count >>> Trie.Bits) > (1 <<< shift) then
            // No room in tree part for the tail, so create a new level in the tree
            let newArr = newNodeArray()
            newArr.[0] <- root
            newArr.[1] <- newPath (root.OwnerThread, shift, tailNode)
            newRoot <- Interior (root.OwnerThread, newArr)
            newShift <- shift + Trie.Bits
         else 
            // There is room in the tree for the tail, so push the tail into the tree
            newRoot <- pushTail(this.EnsureEditable, count, shift, root, tailNode)
         root <- newRoot
         shift <- newShift
         count <- count + 1
         this


   member this.ToPersistent() : Vector<'T> = 
      this.EnsureEditable()
      root <- root.SetOwnerThread (ref null)
      let trimmedTail = Array.zeroCreate (count - tailOffset count)
      System.Array.Copy (tail, trimmedTail, trimmedTail.Length)
      Vector<'T>(count, shift, root, trimmedTail)
      

   member private this.EnsureEditable() =
      if isNull (!root.OwnerThread :> obj) then
         invalidOp "Transient persitent vector used after call to Persistent() call" 


   member private this.EnsureEditable (node: Node<'T>) =
      if Object.Equals(node.OwnerThread, root.OwnerThread) then node
      else 
         match node with 
         | Interior (_, arr) -> Interior (root.OwnerThread, Array.copy arr)
         | Leaf (_, arr) -> Leaf (root.OwnerThread, Array.copy arr)



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =

   [<GeneralizableValue>]
   [<CompiledName("Empty")>]
   let empty<'T> = Vector<'T>.Empty

   [<CompiledName("Singleton")>]
   let inline singleton (item:'T) =
      Vector<'T> (Array.singleton item :> ICollection<'T>)

   [<CompiledName("OfSeq")>]
   let inline ofSeq (items: seq<'T>) = 
      Vector<'T> (items)

   [<CompiledName("OfArray")>]
   let inline ofArray (items: 'T[] ) = 
      Vector<'T> (items :> ICollection<'T>)

   [<CompiledName("Length")>]
   let inline length (vector: Vector<_>) = 
      vector.Count

   [<CompiledName("IsEmpty")>]
   let inline isEmpty (vector: Vector<_>) = 
      vector.IsEmpty

   [<CompiledName("Get")>]
   let inline nth (index: int) (vector: Vector<'T>) = 
      vector.[index]

   [<CompiledName("Set")>]
   let inline set (index: int) (item:'T) (vector: Vector<'T>) = 
      vector.Set(index, item)

   [<CompiledName("RemoveLast")>]
   let inline removeLast (vector:Vector<'T>) =
      vector.RemoveLast()

   [<CompiledName("Map")>]
   let inline map (f: 'T -> 'U) (vector: Vector<'T>) = 
      vector.Map f

   [<CompiledName("Iterate")>]
   let iter (f: 'T -> unit) (vector: Vector<'T>) = 
      vector.Iterate f

   [<CompiledName("ToSeq")>]
   let inline toSeq (vector: Vector<'T>)  =
      vector :> seq<'T>

   [<CompiledName("ToArray")>]
   let toArray (vector: Vector<'T>) =   
      let arr = Array.zeroCreate vector.Count
      let mutable i = 0
      vector |> iter (fun item ->
         arr.[i] <- item
         i <- i + 1)
      arr