namespace Strat.Collections.Primitives

open System
open System.Collections
open System.Collections.Generic
open System.Threading


module BitTrie = 

   // Number of bits to represent an index into the array stored in each node of the tree
   let Bits = 5
   // Number of elements in the array stored in each node of the tree (2^5 = 32)
   let NodeArraySize = 1 <<< Bits 
   // Bitmask to appy to obtain the index of an item in the node array (31, or 0x1f)
   let NodeArrayMask = NodeArraySize - 1 // 31, or 0x1f


   // A node in the trie
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


   // A bit partitioned trie 
   [<NoComparison>]
   type Trie<'T> = {
      // Number of elements stored in the trie.
      Count:int; 
      // (Depth of tree - 1) * Bits per level. In other words, this is the total number of bits a vector index will 
      // have to be shifted to get to the leaf value. 
      Shift: int; 
      // The root node of the 'tree part' of the trie.
      Root: Node<'T>; 
      // 'Tail part' of the trie. Logically, the tail part is the rightmost leaf in the tree. Keeping a direct reference
      // to this leaf array, instead of in the 'tree part', allows some optimizations to be performed.
      Tail: 'T[] 
   }

   
   // Provides access to a bit trie. Useful for keeping Trie<'T> out of main collection APIs.
   type ITrieSource<'T> = 
      abstract member Trie: Trie<'T> 


   let private noEditThread : Ref<Thread> = ref null
   let inline private nullNode() = 
      Unchecked.defaultof<Node<'T>>
   let inline private isNullNode (node: Node<'T>) = 
      Object.ReferenceEquals(node, nullNode())
   let inline private newNodeArray() = 
      Array.zeroCreate NodeArraySize   
   let internal emptyInteriorNode<'T> : Node<'T> = 
      Interior(noEditThread, newNodeArray())
   let empty<'T> : Trie<'T> = 
      { Count = 0; Shift = Bits; Root = emptyInteriorNode; Tail = Array.empty }
   let inline newTrie (count, shift, root, tail) : Trie<'T> =
      { Count = count; Shift = shift; Root = root; Tail = tail }


   // Makes a copy of the node
   let inline private copyNodePersistent (node: Node<'T>) =
      node.Copy()


   // Returns the index for accessing a level array corresponding to the specified vector index.
   let inline private arrayIndex (vectorIndex: int) = 
      vectorIndex &&& NodeArrayMask


   // Inverts the index in a collection of the specified size (ex:  0 -> count -1 )
   let inline reverseIndex (idx: int) (count: int) = 
      count - idx - 1


   // Returns the vector index that is the first index whose element is stored in the tail array. Or in other words, the
   // total number of elements in the 'tree part' of the vector (vector.size - tail.size)
   let private tailOffset (size:int) : int = 
      if size < NodeArraySize then 0
      else ((size - 1) >>> Bits) <<< Bits


   // Returns the array that contains the element at the specified index
   let leafArray (index: int) count shift root tail = 
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


   // Returns the array that contains the element at the specified index
   let inline leafArrayFor (index: int) (trie: Trie<'T>) : 'T[] = 
      leafArray index trie.Count trie.Shift trie.Root trie.Tail


   // Returns a new node representing the root of a tree with the specified level, and the specified node as the first
   // leaf node in the tree.
   let rec private newPath (ownerThread: Ref<Thread>) (level: int) (node: Node<'T>) : Node<'T> =
      if level = 0 then
         node
      else
         let newArr : Node<'T>[] = newNodeArray()
         newArr.[0] <- newPath ownerThread (level - Bits) node   
         Interior (ownerThread, newArr)


   // Returns a new node (at the specified level) with the item at the specified index updated with the specified 
   // value.
   let rec private setTree (level: int, node: Node<'T>, index: int, item: 'T) : Node<'T> = 
      match node with
      | Interior(ot, arr) ->
         let newArray = Array.copy arr
         let childIndex = (index >>> level) &&& NodeArrayMask
         let childNode = arr.[childIndex]
         newArray.[childIndex] <- setTree ((level - Bits), childNode, index, item)
         Interior(ot, newArray)
      | Leaf(ot, arr) ->
         let newArray = Array.copy arr
         newArray.[arrayIndex index] <- item 
         Leaf(ot, newArray)


   // Returns a new tree with the specified tail node as the rightmost leaf node in the tree. Because we will need to
   // create a new node(s) in the tree, and the way it is created varies between persistent and transient cases, we 
   // pass a function to create the node
   let rec private pushTail (copyParent: Node<'T> -> Node<'T>, size: int, level: int, parent: Node<'T>, tail: Node<'T>) : Node<'T> =
      let newParent = copyParent parent
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
                  newPath newParent.OwnerThread (level - Bits) tail
               else
                  // Subtree exists, so push the tail into the subtree
                  pushTail (copyParent, size, level - Bits, child, tail)
         newArr.[childIndex] <- childNode
         newParent
      | _ ->
         invalidOp "Unexpected leaf nodes"


   // Adds the specified item to the end of the trie
   let add (item: 'T, trie: Trie<'T>) = 
      let count = trie.Count
      let root = trie.Root
      let shift = trie.Shift
      let tail = trie.Tail
      if count - (tailOffset count) < NodeArraySize then 
         // There is room in the 'tail part' array, so store the item there
         let newTail = Array.zeroCreate (tail.Length + 1)
         System.Array.Copy(tail, newTail, tail.Length)
         newTail.[tail.Length] <- item
         { trie with Count = trie.Count + 1; Tail = newTail }
      else
         // No room in tail part, so move tail into the tree
         let tailNode = Leaf (root.OwnerThread, tail)
         let mutable newRoot = nullNode()
         let mutable newShift = shift
         if (count >>> Bits) > (1 <<< shift) then
            // No room in tree part for the tail, so create a new level in the tree
            let newArr = newNodeArray()
            newArr.[0] <- root
            newArr.[1] <- newPath root.OwnerThread shift tailNode
            newRoot <- Interior (root.OwnerThread, newArr)
            newShift <- shift + Bits
         else 
            // There is room in the tree for the tail, so push the tail into the tree
            newRoot <- pushTail (copyNodePersistent, count, shift, root, tailNode)    
         // New trie, with the added item stored in the tail
         { trie with Count = count + 1; Shift = newShift; Root = newRoot; Tail = Array.singleton item }


   // Gets the item in the trie at the specfied index
   let get (index: int) (trie: Trie<'T>) =
      let arr = leafArrayFor index trie
      arr.[arrayIndex index]


   // Sets the element in the trie at the specified index, and returns an updated trie.
   let set (index: int) (item: 'T) (trie: Trie<'T>) =
      if index < 0 || index >= trie.Count then
         raise <| IndexOutOfRangeException (index.ToString())
      if index >= tailOffset trie.Count then
         // Index points to an item in the tail part, so just create a new tail
         let newTail = Array.copy trie.Tail
         newTail.[arrayIndex index] <- item
         { trie with Tail = newTail }
      else
         // Index points to an item in the tree part, so update the tree
         { trie with Root = setTree (trie.Shift, trie.Root, index, item) }


   // Returns the node, with it's last (rightmost) node removed, or null if the node becomes empty
   let rec private removeLastLeaf (level: int, node: Node<'T>, trie: Trie<'T>) = 
      let childIndex = arrayIndex ((trie.Count - 2) >>> level)
      if level > Bits then
         match node with
         | Interior(_, arr) ->
            // Descend deeper into tree
            let newChild = removeLastLeaf (level - Bits, arr.[childIndex], trie)
            if isNullNode newChild && childIndex = 0 then
               nullNode()
            else
               let newArr = Array.copy arr
               newArr.[childIndex] <- newChild
               Interior (trie.Root.OwnerThread, newArr)
         | Leaf(_) -> 
            invalidOp "Unexpected leaf node"
      elif childIndex = 0 then
         nullNode()
      else
         match node with
         | Interior(_, arr) ->
            let newArr = Array.copy arr
            newArr.[childIndex] <- nullNode()
            Interior(trie.Root.OwnerThread, newArr)
         | Leaf(_) -> 
            invalidOp "Unexpected leaf node"


   // Returns the last element in the trie, and the trie with that element removed
   let removeLast ({Count=count; Shift=shift; Root=root; Tail=tail} as trie) =
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
            last, { trie with Count = count - 1; Tail = newTail }
         else
            let last = get (count - 1) trie
            // Move contents of last node into new tail array
            let newTail = leafArrayFor (count - 2) trie
            let mutable newRoot = removeLastLeaf (shift, root, trie)
            let mutable newShift = shift
            if isNullNode newRoot then 
               newRoot <- emptyInteriorNode
            if shift > Bits then  
               match newRoot with
               | Interior(_, arr) ->
                  if isNullNode arr.[1] then
                     newRoot <- arr.[0]
                     newShift <- newShift - Bits
               | Leaf(_) -> 
                  invalidOp "Unexpected leaf node"
            // Move contents of last node into tail array
            last, newTrie (count - 1, newShift, newRoot, newTail)


   // Applies the function to each element in the list, with index. Iteration continues while the function returns 
   // true. Note: Inlining this into call sites makes a substantial perf improvement
   let inline iteri (f: int->'T->bool, startIdx: int, endIdxExclusive: int, trie) = 
      let mutable i = startIdx
      // Items are stored in arrays of length NodeArraySize.  baseI is the vector index for the
      // first element in the leaf array that stores element i.
      let mutable baseI = startIdx - (startIdx % NodeArraySize)
      let mutable leafArray = leafArrayFor startIdx trie 
      let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (f)
      while i < endIdxExclusive do
         if (i - baseI) = NodeArraySize then
            // We've iterated thrugh the current leaf array, so get the next one
            leafArray <- leafArrayFor i trie
            baseI <- baseI + NodeArraySize
         let cont = f.Invoke (i, leafArray.[arrayIndex i])
         i <- if cont then i + 1 else endIdxExclusive


   // Applies the function to each element in the list, with index, in reverse order. Iteration continues while the
   // function returns true.
   let inline iteriRev (f: int->'T->bool, startIdx: int, endIdxExclusive: int, trie) = 
      let mutable i = endIdxExclusive - 1
      if i >= startIdx then 
         let mutable leafArray = leafArrayFor i trie
         let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (f)
         // Items are stored in arrays of length NodeArraySize.  baseI is the vector index for the first element in the 
         // leaf array that stores element i. The starting point for the iteration may be in the tail array, so we need to
         // take that into account.
         let mutable baseI = 
            if Object.ReferenceEquals(leafArray, trie.Tail) then i - trie.Tail.Length
            else startIdx - (startIdx % NodeArraySize)
         while i >= startIdx do
            if i <= baseI then
               // We've iterated through the current leaf array, so get the previous one
               leafArray <- leafArrayFor i trie
               baseI <- baseI - NodeArraySize
            let cont = f.Invoke (i, leafArray.[arrayIndex i])
            i <- if cont then i - 1 else startIdx - 1


   // Create a new enumerator that enumerates items in forward order.
   let internal createEnumerator (startIdx: int, endIdxExclusive: int, trie: Trie<'T>) = 
      let mutable i = -1
      let mutable baseI = -1
      let mutable leafArray = null
      let mutable currentItem = Unchecked.defaultof<'T>
      let reset() = 
         i <- startIdx - 1
         leafArray <- 
            if startIdx < trie.Count then leafArrayFor startIdx trie 
            else null
         // Items are stored in arrays of length NodeArraySize.  baseI is the vector index for the first element in the
         // leaf array that stores element i.
         baseI <- startIdx - (startIdx % NodeArraySize)
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
               if i < endIdxExclusive then
                  if (i - baseI) = NodeArraySize then
                     // We've iterated thrugh the current leaf array, so get the next one
                     leafArray <- leafArrayFor i trie
                     baseI <- baseI + NodeArraySize
                  currentItem <- leafArray.[arrayIndex i]
                  i <- i + 1
                  true
               else
                  false
            member x.Reset() = reset()
        interface IDisposable with 
            member x.Dispose() = () } 


   // Create a new enumerator that enumerates items in reverse order .
   let internal createRevEnumerator (startIdx: int, endIdxExclusive: int, trie: Trie<'T>) = 
      let mutable i = -1
      let mutable baseI = -1
      let mutable leafArray = null
      let mutable currentItem = Unchecked.defaultof<'T>
      let reset() = 
         i <- endIdxExclusive
         leafArray <- 
            if endIdxExclusive > 0 then leafArrayFor (endIdxExclusive - 1) trie 
            else null
         baseI <-
            // Items are stored in arrays of length NodeArraySize.  baseI is the vector index for the first element in 
            // the leaf array that stores element i. The starting point for the iteration may be in the tail array, so 
            // we need to take that into account.
            if Object.ReferenceEquals(leafArray, trie.Tail) then endIdxExclusive - 1 - trie.Tail.Length
            else startIdx - (startIdx % NodeArraySize)
      let current() = 
         if i = endIdxExclusive then 
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
               if i = endIdxExclusive then i <- endIdxExclusive - 1
               if i >= startIdx then
                  if i <= baseI then
                     // We've iterated through the current leaf array, so get the previous one
                     leafArray <- leafArrayFor i trie
                     baseI <- baseI - NodeArraySize
                  let idx = arrayIndex i
                  if idx < 0 || idx >= leafArray.Length then
                     invalidOp (sprintf "Bad leaf array index %d for i %d and leaf array length %d" idx i leafArray.Length)
                  currentItem <- leafArray.[arrayIndex i]
                  i <- i - 1
                  true
               else
                  false
            member x.Reset() = reset()
        interface IDisposable with 
            member x.Dispose() = () } 



   // Functions for a 'transient' trie, which is a mutable version of the tree that allows faster addition of new 
   // nodes 
   module Transient = 

      let private editableNode (node: Node<'T>) : Node<'T> =
          match node with
          | Interior(ot, arr) -> Interior(ref Thread.CurrentThread, Array.copy arr)   
          | Leaf(ot, arr) -> Leaf(ref Thread.CurrentThread, Array.copy arr)   
      

      let private editableTail (tail: 'T[]) : 'T[] = 
          let editable = newNodeArray()
          Array.Copy(tail, editable, tail.Length)
          editable


       // Makes a copy of the node, if the nodes do not share the same owner thread.
      let private copyNodeEditable (root: Node<'T>) (node: Node<'T>) =
         if Object.Equals(node.OwnerThread, root.OwnerThread) then node
         else 
            match node with 
            | Interior (_, arr) -> Interior (root.OwnerThread, Array.copy arr)
            | Leaf (_, arr) -> Leaf (root.OwnerThread, Array.copy arr)

      


      [<NoComparison; NoEquality>]
      type TransientTrie<'T> = {
         mutable Count: int
         mutable Shift: int
         mutable Root: Node<'T>
         mutable Tail: 'T[]
      } with
         member this.Add( item : 'T) =
            if (this.Count - tailOffset this.Count) < NodeArraySize then
                // There is room in the 'tail part' array, so store the item there
                this.Tail.[arrayIndex this.Count] <- item
                this.Count <- this.Count + 1
            else
               let tailNode = Leaf (this.Root.OwnerThread, this.Tail)
               this.Tail <- newNodeArray()
               this.Tail.[0] <- item
               let mutable newRoot = nullNode()
               let mutable newShift = this.Shift
               if (this.Count >>> Bits) > (1 <<< this.Shift) then
                  // No room in tree part for the tail, so create a new level in the tree
                  let newArr = newNodeArray()
                  newArr.[0] <- this.Root
                  newArr.[1] <- newPath this.Root.OwnerThread this.Shift tailNode
                  newRoot <- Interior (this.Root.OwnerThread, newArr)
                  newShift <- this.Shift + Bits
               else 
                  // There is room in the tree for the tail, so push the tail into the tree
                  newRoot <- pushTail(copyNodeEditable this.Root, this.Count, this.Shift, this.Root, tailNode)
               this.Root <- newRoot
               this.Shift <- newShift
               this.Count <- this.Count + 1


         member this.ToPersistentTrie() : Trie<'T> = 
            this.Root <- this.Root.SetOwnerThread (ref null)
            let trimmedTail = Array.zeroCreate (this.Count - tailOffset this.Count)
            System.Array.Copy (this.Tail, trimmedTail, trimmedTail.Length)
            { Count = this.Count; Shift = this.Shift; Root = this.Root; Tail = trimmedTail }


         member this.Reverse() = 
            let mutable forwardI = 0 
            let mutable forwardLeafArray = leafArray 0 this.Count this.Shift this.Root this.Tail
            let mutable forwardBaseI = 0
            let mutable backwardI = this.Count - 1
            let mutable backwardLeafArray = leafArray (this.Count - 1) this.Count this.Shift this.Root this.Tail
            let mutable backwardBaseI =
               if Object.ReferenceEquals(backwardLeafArray, this.Tail) then tailOffset this.Count
               else 0
            while forwardI < backwardI do
               if (forwardI - forwardBaseI) = NodeArraySize then
                  // We've iterated thrugh the current leaf array, so get the next one
                  forwardLeafArray <- leafArray forwardI this.Count this.Shift this.Root this.Tail
                  forwardBaseI <- forwardBaseI + NodeArraySize
               if backwardI < backwardBaseI then
                  // We've iterated through the current leaf array, so get the previous one
                  backwardLeafArray <- leafArray backwardI this.Count this.Shift this.Root this.Tail
                  backwardBaseI <- backwardBaseI - NodeArraySize
               let forwardArrIdx = arrayIndex forwardI
               let forwardItem = forwardLeafArray.[forwardArrIdx]
               let backwardArrIdx = arrayIndex backwardI
               let backwardItem = backwardLeafArray.[backwardArrIdx]
               forwardLeafArray.[forwardArrIdx] <- backwardItem
               backwardLeafArray.[backwardArrIdx] <- forwardItem
               forwardI <- forwardI + 1
               backwardI <- backwardI - 1
         

      let inline emptyTrie() : TransientTrie<'T> = 
         { Count = 0; Shift = Bits; Root = Interior(ref Thread.CurrentThread, newNodeArray()); Tail = newNodeArray() }


      let toTransientTrie (trie: Trie<'T>) : TransientTrie<'T> =
        { Count = trie.Count; Shift = trie.Shift; Root = editableNode trie.Root; Tail = editableTail trie.Tail}


   let rev revIter (trie: Trie<'T>) = 
      let reversedT = Transient.emptyTrie()
      let inline iteriReverse (_:int) item = 
         reversedT.Add item
         true
      iteriRev (iteriReverse, 0, trie.Count, trie)
      reversedT.ToPersistentTrie()

   let ofArray (rev: bool) (arr: 'T[]) = 
      let t = Transient.emptyTrie()
      if rev then for i = arr.Length - 1 downto 0 do t.Add arr.[i]
      else for i =  0 to arr.Length - 1 do t.Add arr.[i]
      t.ToPersistentTrie()


   let ofList (rev: bool) (list: list<'T>) = 
      let t = Transient.emptyTrie()
      let list = if rev then list |> List.rev else list
      list |> List.iter (fun item -> t.Add item)
      t.ToPersistentTrie()


   let ofIList (rev: bool) (list : IList<'T>) = 
      let t = Transient.emptyTrie()
      if rev then for i = list.Count - 1 downto 0 do t.Add list.[i]
      else for i =  0 to list.Count - 1 do t.Add list.[i]
      t.ToPersistentTrie()

   
   let ofSeq (revIter: bool) (s: seq<'T>) =
      match s with
      | :? ICollection<'T> as c when c.Count <= NodeArraySize ->
          // Small collection that can fit in the tail part
         let mutable arr = Array.zeroCreate c.Count
         c.CopyTo (arr, 0)
         if revIter then arr <- arr |> Array.rev
         newTrie (c.Count, Bits, emptyInteriorNode, arr)
      | :? list<'T> as l -> ofList revIter l
      | :? ('T[]) as arr -> ofArray revIter arr
      | :? IList<'T> as l -> ofIList revIter l
      | _ ->
         let t = Transient.emptyTrie()
         s |> Seq.iter (fun item -> t.Add item)
         if revIter then t.Reverse()
         t.ToPersistentTrie()
         

   let last (trie: Trie<'T>) =   
      if trie.Count = 0 then 
         raise <| ArgumentException ("Collection is empty")
      get (trie.Count - 1) trie
            

   let append revIter (trie1: Trie<'T>) (trie2: Trie<'T>) = 
      let fIteri = iteri
      let t = Transient.emptyTrie()
      let iteriAppend (_:int) (item: 'T) =
         t.Add item
         true
      if revIter then
         fIteri (iteriAppend, 0, trie2.Count, trie2)
         fIteri (iteriAppend, 0, trie1.Count, trie1)
      else 
         fIteri (iteriAppend, 0, trie2.Count, trie1)
         fIteri (iteriAppend, 0, trie1.Count, trie2)
      t.ToPersistentTrie()


   let addAll (items: seq<'T>) (trie: Trie<'T>) = 
      let t = Transient.toTransientTrie trie
      items |> Seq.iter t.Add
      t.ToPersistentTrie()


   let mapi revIter (f: int -> 'T -> 'U) (trie:Trie<'T>) =
      let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (f)
      let count = trie.Count
      let mappedT = Transient.emptyTrie()
      let iteriMap (idx:int) (item: 'T) =
         mappedT.Add (f.Invoke(idx, item))
         true
      let fIteri = if revIter then iteriRev else iteri
      fIteri (iteriMap, 0, count, trie)
      if revIter then mappedT.Reverse()
      mappedT.ToPersistentTrie()


   let filter revIter predicate trie = 
      let filteredT = Transient.emptyTrie()
      let iteriFiltered (_:int) item = 
         let included = predicate item
         if included then 
            filteredT.Add item
         true
      let fIteri = if revIter then iteriRev else iteri
      fIteri (iteriFiltered, 0, trie.Count, trie)
      filteredT.ToPersistentTrie()


   let fold revIter (f: 'State -> 'T -> 'State) (initial: 'State) trie =
      let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (f)
      let mutable state = initial
      let iterFold (_:int) item = 
         state <- f.Invoke(state, item)
         true
      let fIteri = if revIter then iteriRev else iteri
      fIteri (iterFold, 0, trie.Count, trie)
      state


   let foldBack revIter (f:'T -> 'State -> 'State) (trie: Trie<'T>) (initial: 'State) : 'State =
      let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (f)
      let mutable state = initial
      let iterFold (_:int) item = 
         state <- f.Invoke (item, state)
         true
      let fIteri = if revIter then iteri else iteriRev
      fIteri (iterFold, 0, trie.Count, trie)
      state


   let collect revIter (f: 'T -> Trie<'U>) (trie:Trie<'T>) : Trie<'U> = 
      let collectedT = Transient.emptyTrie()
      let fIteri = if revIter then iteriRev else iteri
      let iterCollect (_:int) item =
         let nextTrie = f item
         let iterAdd (_:int) nextItem = 
            collectedT.Add nextItem
            true
         let fIteri = if revIter then iteriRev else iteri
         fIteri (iterAdd, 0, nextTrie.Count, nextTrie)   
         true
      fIteri (iterCollect, 0, trie.Count, trie)
      if revIter then collectedT.Reverse()
      collectedT.ToPersistentTrie()


   let choose revIter (f: 'T -> 'U option) (trie: Trie<'T>) : Trie<'U> =
      let chooseT = Transient.emptyTrie()
      let fIteri = if revIter then iteriRev else iteri
      let iterChoose (_:int) item =
         match f item with
         | Some v -> chooseT.Add v
         | _ -> ()
         true
      fIteri (iterChoose, 0, trie.Count, trie)
      if revIter then chooseT.Reverse()
      chooseT.ToPersistentTrie()


   let tryFind revIter (predicate: 'T -> bool) (trie: Trie<'T>) : option<'T> =
      let fIteri = if revIter then iteriRev else iteri
      let mutable matched = None
      let iteriFind (_:int) item = 
         if predicate item then
            matched <- Some item
            false
         else
            true
      fIteri (iteriFind, 0, trie.Count, trie)
      matched


   let find revIter (predicate: 'T -> bool) (trie: Trie<'T>) : 'T =
      match tryFind revIter predicate trie with
      | Some s -> s
      | None -> raise <| new KeyNotFoundException()


   let tryFindIndex revIter (predicate: 'T -> bool) (trie: Trie<'T>) =
      let fIteri = if revIter then iteriRev else iteri
      let mutable matched = None
      let iteriFind (index:int) item = 
         if predicate item then
            matched <- Some index
            false
         else
            true
      fIteri (iteriFind, 0, trie.Count, trie)
      matched


   let findIndex revIter (predicate: 'T -> bool) (trie: Trie<'T>) =
      match tryFindIndex revIter predicate trie with
      | Some s -> s
      | None -> raise <| new KeyNotFoundException()

   
   let tryPick revIter (f: 'T -> 'U option) (trie: Trie<'T>) = 
      let fIteri = if revIter then iteriRev else iteri
      let mutable matched = None
      let iteriPick (_:int) item = 
         match f item with
         | Some(_) as s -> 
            matched <- s
            false
         | None ->
            true
      fIteri (iteriPick, 0, trie.Count, trie)
      matched


   let pick revIter (f: 'T -> 'U option) (trie: Trie<'T>) = 
      match tryPick revIter f trie with
      | Some v -> v
      | None -> raise <| new KeyNotFoundException()


   let exists revIter (predicate:'T -> bool) (trie: Trie<'T>) =
      match tryFind revIter predicate trie with
      | Some _ -> true
      | None -> false
  

   let zip revIter (t1: Trie<'T>) (t2: Trie<'U>) =
      if t1.Count <> t2.Count then
         let msg = sprintf "Count of items %d in vector1 does not match number of items %d in vector2" t1.Count t2.Count
         raise <| new ArgumentException (msg)
      let zippedT = Transient.emptyTrie()
      let createEnum t = if revIter then createRevEnumerator(0, t.Count, t) else createEnumerator(0, t.Count, t)
      let it1 = createEnum t1
      let it2 = createEnum t2
      while it1.MoveNext() && it2.MoveNext() do 
         zippedT.Add (it1.Current, it2.Current)
      if revIter then zippedT.Reverse()
      zippedT.ToPersistentTrie()


   let forall revIter (predicate: 'T -> bool) (trie: Trie<'T>) =
      let fIteri = if revIter then iteriRev else iteri
      let mutable matched = true
      let iteriForAll (_:int) item = 
         if predicate item then 
            true
         else
            matched <- false
            false
      fIteri (iteriForAll, 0, trie.Count, trie)
      matched


   let min revIter (trie: Trie<'T>) =
      if trie.Count = 0 then 
         raise <| new ArgumentException("Can't call min on empty collection")
      let fIteri = if revIter then iteriRev else iteri
      let mutable currentMin = last trie
      let iteriMin (_:int) item = 
         if item < currentMin then 
            currentMin <- item
         true
      fIteri (iteriMin, 0, trie.Count, trie)
      currentMin


   let inline minBy revIter f (trie: Trie<'T>) = 
      if trie.Count = 0 then 
         raise <| new ArgumentException("Can't call maxBy on empty collection")
      let fIteri = if revIter then iteriRev else iteri
      let mutable currentMinItem = last trie
      let mutable currentMin = f currentMinItem
      let iteriMax (_:int) item =
         let fItem = f item
         if fItem < currentMin then
            currentMinItem <- item
            currentMin <- fItem
         true
      fIteri (iteriMax, 0, trie.Count, trie)
      currentMinItem 


   let max revIter (trie: Trie<'T>) =
      if trie.Count = 0 then
         raise <| new ArgumentException("Can't call min on empty collection")
      let fIteri = if revIter then iteriRev else iteri
      let mutable currentMax = last trie
      let iteriMin (_:int) item = 
         if item > currentMax then 
            currentMax <- item
         true
      fIteri (iteriMin, 0, trie.Count, trie)
      currentMax


   let inline maxBy revIter f (trie: Trie<'T>) = 
      if trie.Count = 0 then 
         raise <| new ArgumentException("Can't call maxBy on empty collection")
      let fIteri = if revIter then iteriRev else iteri
      let mutable currentMaxItem = last trie
      let mutable currentMax = f currentMaxItem
      let iteriMax (_:int) item =
         let fItem = f item
         if fItem > currentMax then
            currentMaxItem <- item
            currentMax <- fItem
         true
      fIteri (iteriMax, 0, trie.Count, trie)
      currentMaxItem 


   let inline sum revIter (trie: Trie<'T>) : 'T  =
      let fIteri = if revIter then iteriRev else iteri
      let mutable acc = LanguagePrimitives.GenericZero< ^T>
      let iteriSum (_:int) item =
         acc <- Checked.(+) acc item
         true
      fIteri (iteriSum, 0, trie.Count, trie)
      acc


   let inline sumBy revIter (f: 'T -> ^U) (trie: Trie<'T>) : ^U = 
      let fIteri = if revIter then iteriRev else iteri
      let mutable acc = LanguagePrimitives.GenericZero< ^U>
      let iteriSum (_:int) item =
         acc <- Checked.(+) acc (f item)
         true
      fIteri (iteriSum, 0, trie.Count, trie)
      acc


   let toArray revIter (trie: Trie<'T>) =   
      let fIteri = if revIter then iteriRev else iteri
      let arr = Array.zeroCreate trie.Count
      let mutable i = 0
      let iteriSet (_:int) item =
         arr.[i] <- item
         i <- i + 1
         true
      fIteri (iteriSet, 0, trie.Count, trie)
      arr


   let toList revIter (trie: Trie<'T>) = 
      let mutable l = List.empty
      let fIteri = if revIter then iteri else iteriRev
      let iteriSet (_:int) item =
         l <- item::l
         true
      fIteri (iteriSet, 0, trie.Count, trie)
      l
    
