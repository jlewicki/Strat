namespace Strat.Collections

open System
open System.Collections
open System.Collections.Generic

//
// Persisent vector, implemented as bit-partitioned vector trie.
//
// This implementation follows the Clojure implementation closely. All credit belongs there.
//
// Some helpful background links:
// http://hypirion.com/musings/understanding-persistent-vector-pt-2
// https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/PersistentVector.java
// https://github.com/fsprojects/FSharpx.Collections/blob/master/src/FSharpx.Collections/PersistentVector.fs


// Internal implementation details, shared between TransientVector and PersistentVector
module Impl =

   // A node in a persistent vector. If the node is a leaf, items in the array are elements in the vector. If interior, 
   // items are references to other nodes
   // TODO: should this be a struct?
   [<AllowNullLiteral>]
   type Node (ownerThread, array: obj[]) =
      let ownerThread = ownerThread
      member this.Array = array
      member this.OwnerThread = ownerThread
      member this.SetOwnerThread newOwnerThread = ownerThread := newOwnerThread

     
   // Number of bits to represent an index into the array stored in each node of the tree
   let Bits = 5
   // Number of elements in the array stored in each node of the tree (2^5 = 32)
   let NodeArraySize = 1 <<< Bits 
   // Bitmask to appy to obtain the index of an item in the node array (31, or 0x1f)
   let NodeArrayMask = NodeArraySize - 1 // 31, or 0x1f
   
   let newNodeArray() = 
      Array.zeroCreate NodeArraySize

   // Reusable empty node
   let noEditThread = ref null
   let emptyNode = Node(noEditThread, newNodeArray())


   // Creates a copy of the node with the current thread as the owner
   let editableNode (node: Node) : Node = 
      // TODO: use thread once core 2.0
      Node(ref "", node.Array.Clone() :?> obj[])


   // Creates a full-width array, copying in the tail values
   let editableTail (tail: obj[]) : obj[] = 
      let editable = newNodeArray()
      Array.Copy(tail, editable, tail.Length)
      editable


   // Returns a new node representing the root of a tree with the specified level, and the specified node as the first
   // leaf node in the tree.
   let rec newPath (ownerThread, level: int, node: Node) : Node =
      if level = 0 then
         node
      else
         let newNode = Node (ownerThread, newNodeArray())
         newNode.Array.[0] <- newPath(ownerThread, level - Bits, node) :> obj   
         newNode


   // Returns a new tree with the specified tail node as the rightmost leaf node in the tree
   // Because we will need to create a new node(s) in the tree, and the way it is created varies
   // between persistent and transient cases, we pass a function to create the ndoe
   let rec pushTail (fNewParent: Node -> Node, size: int, level: int, parent: Node, tail: Node) : Node =
      let newParent = fNewParent parent
      let childIndex = ((size - 1) >>> level) &&& NodeArrayMask
      let childNode = 
         if level = Bits then
            // We're at the leaf of the tree, so we can directly insert the tail node
            tail
         else
            let child = parent.Array.[childIndex]
            if isNull child then
               // No subtree yet at this index, so create a minimal path to the tail node
               newPath (newParent.OwnerThread, level - Bits, tail)
            else
               // Subtree exists, so push the tail into the subtree
               pushTail (fNewParent, size, level - Bits, child :?> Node, tail)
      newParent.Array.[childIndex] <- upcast childNode
      newParent


   // Returns the total number of elements in the 'tree part' of the vector (as opposed to the 'tail part'). In other 
   // words: vector.size - tail.size
   let tailOffset (size:int) : int = 
      if size < NodeArraySize then 0
      else ((size - 1) >>> Bits) <<< Bits


   // Returns the index for accessing a level array corresponding to the specified index (into the persistent vector)
   let inline arrayIndex (vectorIndex: int) = 
      vectorIndex &&& NodeArrayMask


   // Returns the array that contains the element at the specified index
   let leafArrayFor (index: int, count: int, shift: int, root: Node, tail: obj[]) : obj[] = 
      if index >= tailOffset count then 
         // Element at the index is within the 'tail part', so just return the tail array
         tail
      else
         // Element at the index is within the 'tree part', so we need to traverse the tree. Visit each level of the
         // trie by calculating the index in each level of the child to descend into.
         let mutable node = root
         let mutable level = shift
         while level > 0 do
            let arrIdx = arrayIndex (index >>> level)
            node <- node.Array.[arrIdx] :?> Node
            level <- level - Bits
         node.Array


   // Returns a new node (at the specified level) with the item at the specified index updated with the 
   // specified value.
   let rec setTree (level: int) (node: Node) (index: int) (item: 'T) : Node = 
      let newNode = Node(node.OwnerThread, node.Array.Clone() :?> obj[])
      if level = 0 then
         newNode.Array.[arrayIndex index] <- upcast item 
      else
         let childIndex = (index >>> level) &&& NodeArrayMask
         let childNode = node.Array.[childIndex] :?> Node
         newNode.Array.[childIndex] <- upcast (setTree (level - Bits) childNode index item)
      newNode

 
module PV = Impl
type Node = PV.Node


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
type Vector<'T> internal (count:int, shift: int, root: Node, tail: obj[]) =  
   static let empty = new Vector<'T> (0, PV.Bits, Impl.emptyNode, Array.empty) 
   static member Empty = empty
  
   new (items: seq<'T>) = 
      if isNull items then 
         raise <| ArgumentNullException("items")
      let tv : TransientVector<'T> = empty.ToTransient()
      items |> Seq.iter (tv.Add >> ignore)
      let v = tv.ToPersistent()
      // It would be nice to return v directly, but F# requires a explicit construtor invocation as the return value.
      Vector<'T> (v.Count, v.Shift, v.Root, v.Tail)


   new (items: ICollection<'T>) =
      if isNull items then 
         raise <| ArgumentNullException("items")
      let size = items.Count
      if size <= PV.NodeArraySize then
         // Small collection that can fit in the tail part
         Vector<'T> (size, PV.Bits, PV.emptyNode, [|for i in items -> i :> obj|])
      else
         Vector<'T> (items :> seq<'T>)


   member this.Count 
      with get() = count


   member this.IsEmpty
      with get() = count = 0


   member this.Item 
      with get (index: int) : 'T =
         if index < 0 || index >= count then 
            raise <| IndexOutOfRangeException (index.ToString())
         let arr : obj[] = PV.leafArrayFor (index, count, shift, root, tail)
         arr.[PV.arrayIndex index] :?> 'T


   member this.Add (item: 'T) = 
      // TODO: Move local function to Impl module, or a class member to avoid allocation
      let newParent (parent: PV.Node) = Node(parent.OwnerThread, parent.Array.Clone() :?> obj[])

      if count - (PV.tailOffset count) < PV.NodeArraySize then 
         // There is room in the 'tail part' array, so store the item there
         let newTail = Array.zeroCreate (count + 1)
         System.Array.Copy(tail, newTail, tail.Length)
         newTail.[tail.Length] <- item :> obj
         Vector<'T>(count + 1, shift, root, newTail)
      else
         // No room in tail part, so move tail into the tree
         let tailNode = Node (root.OwnerThread, tail)
         let mutable newRoot = Unchecked.defaultof<Node>
         let mutable newShift = shift
         if (count >>> PV.Bits) > (1 <<< shift) then
            // No room in tree part for the tail, so create a new level in the tree
            newRoot <- Node (root.OwnerThread, PV.newNodeArray())
            newRoot.Array.[0] <- upcast root
            newRoot.Array.[1] <- upcast PV.newPath (root.OwnerThread, shift, tailNode)
            newShift <- shift + PV.Bits
         else 
            // There is room in the tree for the tail, so push the tail into the tree
            newRoot <- PV.pushTail(newParent, count, shift, root, tailNode)
        
         // New vector, with the added item stored in the tail
         new Vector<'T>(count + 1, newShift, newRoot, [| item :> obj |])


   member this.Set(index: int, item: 'T)  = 
      if index < 0 || index >= count then
         raise <| IndexOutOfRangeException (index.ToString())

      if index >= PV.tailOffset count then
         // Index points to an item in the tail part, so just create a new tail
         let newTail = tail.Clone() :?> obj[]
         newTail.[PV.arrayIndex index] <- item :> obj
         Vector<'T> (count, shift, root, newTail)
      else
         // Index points to an item in the tree part, so update the tree
         Vector<'T> (count, shift, (PV.setTree shift root index item), tail)


   member this.RemoveLast() : 'T * Vector<'T> =
      if count = 0 then 
         raise <| InvalidOperationException("Cannot call Remove() on an empty vector.")
      elif count = 1 then
         tail.[tail.Length - 1] :?> 'T, empty
      else
         if (count - PV.tailOffset count ) > 1 then
            // Last item is in the tail, so build a new tail with the item removed
            let last = tail.[tail.Length - 1] :?> 'T
            let newTail = Array.zeroCreate (tail.Length - 1)
            Array.Copy (tail, newTail, newTail.Length)
            last, Vector<'T> (count - 1, shift, root, newTail)
         else
            let last = this.[count - 1]
            let newTail = PV.leafArrayFor (count - 2, count, shift, root, tail)
            let mutable newRoot = this.RemoveLastFromTree (shift, root)
            let mutable newShift = shift
            if isNull newRoot then 
               newRoot <- PV.emptyNode
            if shift > PV.Bits && isNull newRoot.Array.[1] then
               newRoot <- newRoot.Array.[0] :?> Node
               newShift <- newShift - PV.Bits 
            last, Vector<'T> (count - 1, newShift, newRoot, newTail)
 

   member this.Map( f: 'T -> 'U) : Vector<'U> =
      let mutable t = TransientVector<'U>()
      for item in this do 
         t <- t.Add (f item)
      t.ToPersistent()


   interface IEnumerable with
      member this.GetEnumerator() = 
         this.CreateEnumerator(0, count) :> IEnumerator


   interface IEnumerable<'T> with
      member this.GetEnumerator() = 
         this.CreateEnumerator(0, count)


   member internal this.Root : Node = root
   member internal this.Shift : int = shift
   member internal this.Tail : obj[] = tail
   member internal this.ToTransient() = TransientVector<'T>(this)


   // Creates a new enumerator that yields values between startIndex (inclusive) and endIndex (exclusive).
   member private this.CreateEnumerator (startIdx: int, endIdx: int) = 
      let mutable i = -1
      let mutable baseI = -1
      let mutable leafArray = null
      let mutable currentItem = Unchecked.defaultof<'T>
      let reset() = 
         i <- startIdx - 1
         leafArray <- 
            if startIdx < count then PV.leafArrayFor (startIdx, count, shift, root, tail) 
            else null
         baseI <- startIdx - (startIdx % PV.Bits)
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
                  if (i - baseI) = PV.NodeArraySize then
                     // We've iterated thrugh the current leaf array, so get the next one
                     leafArray <- PV.leafArrayFor (i, count, shift, root, tail)
                     baseI <- baseI + PV.NodeArraySize
                  currentItem <- leafArray.[PV.arrayIndex i] :?> 'T
                  i <- i + 1
                  true
               else
                  false
            member x.Reset() = reset()
        interface IDisposable with 
            member x.Dispose() = () } 


   // Returns a new node that represents the specfied node, with its last element removed
   member private this.RemoveLastFromTree (level: int, node: Node) = 
      let childIndex = PV.arrayIndex ((count - 2) >>> level)
      if level > PV.Bits then
         // Descend deeper into tree
         let newChild = this.RemoveLastFromTree (level - PV.Bits, node.Array.[childIndex] :?> Node)
         if isNull newChild && childIndex = 0 then
            null
         else
            let newNode = Node (root.OwnerThread, node.Array.Clone() :?> obj[])
            newNode.Array.[childIndex] <- upcast newChild
            newNode
      elif childIndex = 0 then
         null
      else
         let newNode = Node (root.OwnerThread, node.Array.Clone() :?> obj[])
         newNode.Array.[childIndex] <- null
         newNode
     

// Mutable vector, to allow for faster updates. Not threadsafe.
and internal TransientVector<'T> (count:int, shift: int, root: Node, tail: obj[]) =
   
   let mutable count = count
   let mutable shift = shift
   let mutable root = root
   let mutable tail = tail

   member this.Root : Node = root
   member this.Shift : int = shift
   member this.Tail : obj[] = tail

   new() = 
      TransientVector<'T>(0, PV.Bits, PV.editableNode PV.emptyNode, PV.editableTail Array.empty)


   new (pv: Vector<'T>) = 
         TransientVector<'T>(pv.Count, pv.Shift, PV.editableNode pv.Root, PV.editableTail pv.Tail)


   member this.Count 
      with get() : int = 
         this.EnsureEditable()
         count


   member this.Add (item: 'T) : TransientVector<'T> = 
      this.EnsureEditable()
      let i = count
      if (i - PV.tailOffset count) < PV.NodeArraySize then
         // There is room in the 'tail part' array, so store the item there
         tail.[PV.arrayIndex i] <- item :> obj
         count <- count + 1
         this
      else
         let tailNode = Node (root.OwnerThread, tail)
         tail <- PV.newNodeArray()
         tail.[0] <- item :> obj
         let mutable newRoot = Unchecked.defaultof<Node>
         let mutable newShift = shift
         if (count >>> PV.Bits) > (1 <<< shift) then
            // No room in tree part for the tail, so create a new level in the tree
            newRoot <- Node (root.OwnerThread, PV.newNodeArray())
            newRoot.Array.[0] <- upcast root
            newRoot.Array.[1] <- upcast PV.newPath (root.OwnerThread, shift, tailNode)
            newShift <- shift + PV.Bits
         else 
            // There is room in the tree for the tail, so push the tail into the tree
            newRoot <- PV.pushTail(this.EnsureEditable, count, shift, root, tailNode)
         root <- newRoot
         shift <- newShift
         count <- count + 1
         this


   member this.ToPersistent() : Vector<'T> = 
      this.EnsureEditable()
      root.SetOwnerThread null
      let trimmedTail = Array.zeroCreate (count - PV.tailOffset count)
      System.Array.Copy (tail, trimmedTail, trimmedTail.Length)
      Vector<'T>(count, shift, root, trimmedTail)
      

   member private this.EnsureEditable() =
      if isNull (!root.OwnerThread :> obj) then
         invalidOp "Transient persitent vector used after call to Persistent() call" 


   member private this.EnsureEditable (node: Node) =
      if Object.Equals(node.OwnerThread, root.OwnerThread) then node
      else Node(root.OwnerThread, node.Array.Clone() :?> obj[])
         


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =

   [<GeneralizableValue>]
   [<CompiledName("Empty")>]
   let empty<'T> = Vector<'T>.Empty

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