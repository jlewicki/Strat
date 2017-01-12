namespace Strat.Collections

open System
open System.Collections
open System.Collections.Generic
#if DOTNETCORE
#else
open System.Runtime.Serialization
#endif
open System.Runtime.InteropServices

// Defines core algorithms for a random access list.
//
// This code is a direct implementation of the skew random access list presented in 'Purely 
// Functional Data Structures' by Osaki.  This data structure performs the core list operations
// cons, head, and tail in O(1) time, and the core indexed operations get and set in O(lgN) time.
module internal SkewRandomAccessList = 

   // Simple binary tree
   type Tree<'T> =
      | Leaf of 'T
      | Node of 'T * Tree<'T> * Tree<'T>
      
   // The integers in each entry represent the weight of the corresponding tree.
   type ListEntry<'T> = int * Tree<'T>
   type RAList<'T> = ListEntry<'T> list 

   // The empty list.
   let empty : RAList<'T> = []

   // Returns a value indicating if the list is empty
   let isEmpty raList = List.isEmpty raList 

   let raiseListIsEmpty() = raise <| new InvalidOperationException("The list is empty")
   let raiseInvalidIndex i = raise <| new ArgumentOutOfRangeException()

   // Returns a new list with the element at the head of the list, followed by the original list.
   // This is O(1).
   let cons x raList = 
      match raList with
      | (w1,t1)::(w2,t2)::rest ->
         if w1 = w2 then (1 + w1 + w2, Node(x, t1, t2 ))::rest
         else (1, Leaf(x))::raList
      | _ -> (1, Leaf(x))::raList
           
   // Returns the item at the head of this list.  This is O(1).
   let head raList = 
      match raList with
      | [] ->  raiseListIsEmpty()
      | (1, Leaf(x)) :: rest -> x
      | (w, Node(x, _, _)) :: rest -> x 
      // We will never hit this
      | _ ->  raise <| new InvalidOperationException()

   // Returns the list without the first element.  This is O(1).
   let tail raList = 
      match raList with
      | [] ->  raiseListIsEmpty()
      | (1, Leaf(x)) :: rest -> rest
      | (w, Node(x, t1, t2)) :: rest -> 
         let halfWeight = w / 2
         (halfWeight, t1) :: (halfWeight, t2) :: rest
      // We will never hit this
      | _ ->  raise <| new InvalidOperationException()

   // Returns the item at the specified index.  This is O(lgN).
   let rec get i raList = 
      let rec lookupTree = function
         | 1, Leaf(x), 0  -> x
         | 1, Leaf(x), idx -> raiseInvalidIndex i 
         | w, Node(x, t1, t2), 0 -> x
         | w, Node(x, t1, t2), idx -> 
            let halfWeight = w / 2
            if idx <= halfWeight then lookupTree( halfWeight, t1, idx - 1 )
            else lookupTree( halfWeight, t2, idx - 1 - halfWeight )
         // We will never hit this
         | _ ->  raise <| new InvalidOperationException()
      match raList with
      | [] -> raiseInvalidIndex( i )
      | (w, t)::rest -> 
         if i < w then lookupTree( w, t, i )
         else get (i - w) rest 

   // Returns a list by setting the value at the specified to the specified item. This is O(lgN).
   let rec set i item raList = 
      let rec updateTree = function
         | 1, Leaf(x), 0, y -> Leaf(y)
         | 1, Leaf(x), idx, y ->  raiseInvalidIndex i 
         | w, Node(x, t1, t2), 0, y -> Node(y, t1, t2)
         | w, Node(x, t1, t2), idx, y ->
            let halfWeight = w / 2
            if idx <= halfWeight then Node(x, updateTree(halfWeight, t1, idx-1, y), t2 )
            else Node(x, t1, updateTree(halfWeight, t2, idx-1-halfWeight, y))
         // We will never hit this
         | _ ->  raise <| new InvalidOperationException()
      match raList with
      | [] -> raiseInvalidIndex( i )
      | (w, t)::rest ->
         if i < w then (w, updateTree( w, t, i, item )) :: rest
         else (w, t) :: (set (i-w) item rest)

   // Returns the number of elements in the list.  This is O(lgN).
   let count raList = 
      let rec countWithAcc = function
      | count, [] -> count
      | count, (w, t)::rest -> countWithAcc ( w+count, rest )
      countWithAcc(0, raList)

   // Reverses the list.  This is O(N)
   let rev raList = 
      let rec revWithAcc(reversed, list) =
         match list with
         | [] -> reversed
         | list -> 
            let h = list |> head
            let tail = list |> tail
            revWithAcc( reversed |> cons h, tail)
      revWithAcc( [], raList)

   
   let rec foldTree f state tree  = 
      match tree with 
      | Leaf(x) -> f state x
      | Node(x, l, r) ->  
         let state = f state x
         let state = foldTree f state l
         foldTree f state r

   // Applies the given accumulating function to all the elements of the list.
   let rec fold f state raList = 
      match raList with
      | [] -> state
      | (w, Leaf(x)) :: rest -> fold f (f state x) rest
      | (w, Node(x, l, r)) :: rest -> 
         let state = (f state x)
         let state = foldTree f state l
         let state = foldTree f state r
         fold f state rest

   // Returns a value indicating if the specified item is in the list.  This is O(N).
   let rec contains (comparer: IComparer<'T>) item raList = 
      let treeContains tree =
         foldTree (fun flag elem -> flag || comparer.Compare(item, elem) = 0) false tree
      match raList with 
      | [] -> false
      | (w, Leaf(x)) :: rest -> 
         if comparer.Compare(item, x) = 0 then true
         else contains comparer item rest
      | (w, Node(x, l, r)) :: rest -> 
         if comparer.Compare(item,x) = 0 then true
         elif treeContains l then true
         elif treeContains r then true
         else contains comparer item rest

   // Maps the specified function over the elements in the list, passing index of element to the mapping function.
   // This is O(N).
   let mapi f raList = 
      let rec mapTree i tree = 
         match tree with 
         | Leaf(x) -> i + 1, Leaf(f i x)
         | Node(x, l, r) -> 
            let i, v = (i + 1), f i x
            let i, mappedLeft = mapTree i l
            let i, mappedRight = mapTree i r 
            i, Node(v, mappedLeft, mappedRight)
      let rec mapiacc i raList = 
         match raList with 
         | [] -> []
         | (w, t) :: rest -> 
            let i, mappedTree = mapTree i t 
            (w, mappedTree) :: (mapiacc i rest)
      mapiacc 0 raList

   // Maps the specified function over the elements in the list.  This is O(N).
   let rec map f raList = 
      mapi (fun _ a -> f a) raList

   // Returns a new list containing only the elements of the heap for which the given predicate 
   // returns true.  This is O(N).
   let rec filter pred raList =
      let foldFunc filtered x =
         if pred x then filtered |> cons x 
         else filtered
      // We need to reverse list at end, because the fold func uses cons which will reverse order
      // of original list.
      raList |> fold foldFunc empty |> rev

   // Returns a value indicating if the specified predicate holds true for all items in the list.
   // This is O(N).
   let rec forall pred raList = 
      let treeForAll tree = 
         foldTree (fun flag elem -> flag && pred elem ) true tree
      match raList with 
      | [] -> true
      | (w, Leaf(x))::rest -> 
         pred x && forall pred rest
      | (w, Node(x, l, r))::rest -> 
         pred x && treeForAll l && treeForAll r && forall pred rest

   // Tests if any element in the heap satisfies the predicate.
   let rec exists pred raList = 
      let treeExists tree = 
         foldTree (fun flag elem -> flag || pred elem ) false tree
      match raList with
      | [] -> false
      | (w, Leaf(x))::rest -> 
         pred x || exists pred rest
      | (w, Node(x, l, r))::rest -> 
         pred x || treeExists l || treeExists r || exists pred rest

   // Returns the element that satisfies the predicate, or None. This is O(N).
   let rec tryFind pred raList =
      let asOpt x = if pred x then Some(x) else None
      let rec treeTryFind = function
         | Leaf(x) -> asOpt x
         | Node(x, l, r) -> 
            if pred x then Some(x) 
            else
               match treeTryFind l with
               | Some(_) as found -> found
               | _ -> treeTryFind r
      match raList with
      | [] -> None
      | (w, Leaf(x))::rest -> 
         if pred x then Some(x) else tryFind pred rest
      | (w, Node(x, l, r))::rest ->
         match asOpt x with
         | Some(_) as found -> found
         | _ -> 
            match treeTryFind l with
            | Some(_) as found -> found
            | _ ->
               match treeTryFind r with
               | Some(_) as found -> found
               | _ -> tryFind pred rest
         
   // Returns a heap containing the items in the specified sequence. This is O(N).
   let ofSeq (items: seq<'T>) =      
      use e = items.GetEnumerator()
      let mutable res = [] 
      while e.MoveNext() do
         res <- res |> cons e.Current 
      // Reverse list, because just using cons will invert the order of the original sequence
      rev res

   // Calls the specified function for each element in the list. This is O(N).
   let iter f (raList: RAList<'T>) =
      let rec iterTree = function
         | Leaf(x) -> f x
         | Node(x, l, r) -> f x; iterTree l; iterTree r;
      raList |> List.iter ( snd >> iterTree )

   // Calls the specified function for each element in the list. This is O(N).
   let iteri f (raList: RAList<'T>) =
      let idx = ref 0
      let rec iterTree = function
         | Leaf(x) -> f !idx x; idx := !idx + 1
         | Node(x, l, r) -> 
            f !idx x; idx := !idx + 1
            iterTree l 
            iterTree r
      raList |> List.iter ( snd >> iterTree )

   // Copies the elements in the list to the array, beginning at index i.
   let copyToArray (raList: RAList<'T>) (arr: _[]) i =
      let j = ref i 
      iter (fun x -> arr.[!j] <- x; j := !j + 1) raList

   // Creates a new array containing the items in the list. This is O(N).
   let toArray raList = 
      let arr = Array.zeroCreate (count raList) 
      copyToArray raList arr 0
      arr

   // Returns a list containing the elements in the specified random access list.  This is O(N).
   let toList raList = 
      raList
      |> fold (fun list item -> item::list) List.empty
      |> List.rev

   // Returns a list containing the items in the specified array.  This is O(N).
   let ofArray (arr: _[]) = 
      let mutable list = empty
      for i = arr.Length-1 downto 0 do
         list <- list |> cons arr.[i]
      list

   // Iterator class for a random access list
   type internal ListEnumerator<'T>( raList : RAList<'T> ) =
      let notStarted() = 
         raise <| new InvalidOperationException("The enumerator has not been started by a call to MoveNext")
      let alreadyCompleted() = 
         raise <| new InvalidOperationException("The enumerator has already completed.")

      // Returns either [] or a list starting with a leaf.  This essentially 'flattens' any tree
      // at the left hand side of the list
      let rec flattenLHS list =
         match list with
         | [] -> []
         | (_, Leaf(x)) :: rest -> list
         | (_, Node(x, l, r)) :: rest -> 
            ( (1, Leaf(x)) :: (1, l ) :: (1, r) :: rest )

      let initTreeList =  flattenLHS raList
      let mutable currentList = initTreeList
      let mutable isStarted = false

      // Get the current item in the enumerator
      let current() =
         if isStarted then
             match currentList with
               | (_, Leaf(x))  :: rest -> x
               | [] -> alreadyCompleted()
               // Will never reach here
               | _ -> invalidOp ("")
         else
             notStarted()
      
      // Positions the enumerator at the next item in the collection
      let moveNext() =
         if isStarted then
             match currentList with
               | (_, Leaf(x))  :: rest -> 
                  currentList <- flattenLHS rest
                  not rest.IsEmpty
               | [] -> false
               // Will never reach here
               | _ -> invalidOp ("")
         else
             isStarted <- true;  // The first call to MoveNext "starts" the enumeration.
             not currentList.IsEmpty
 
      interface IEnumerator<'T> with
         member x.Current = current()
      interface IEnumerator with 
         member x.Current = box (current())
         member x.MoveNext() = moveNext()
         member x.Reset() = currentList <- initTreeList
      interface IDisposable with 
         member x.Dispose() = () 
   

// Alias module for brevity
module RAL = SkewRandomAccessList


// Documentation in signature file.
[<Sealed>]
type RandomAccessList<'T> internal ( raList : RAL.RAList<'T> ) =
   
   static let collectionIsReadOnly() = 
      new NotSupportedException("The operation is not valid because the collection is read-only")   

   static let empty = new RandomAccessList<'T>( RAL.empty )

   //[<NonSerialized>]
   // NOTE: This type is logically immutable. This field is only mutated during deserialization. 
   let mutable raList = raList

   // WARNING: The compiled name of this field may never be changed because it is part of the logical 
   // permanent serialization format for this type.
   let mutable serializedData = null  

   new ( items: seq<'T> ) = 
      new RandomAccessList<'T>( items |> RAL.ofSeq )

   new ( items: array<'T> ) = 
      new RandomAccessList<'T>( items |> RAL.ofArray)

   member x.Count with get() = RAL.count raList

   member x.IsEmpty with get() = RAL.isEmpty raList

   member x.Item with get(i) =  x.Get i

   member x.Head = RAL.head raList

   member x.Tail = new RandomAccessList<'T>( RAL.tail raList )

   member x.Cons item = new RandomAccessList<'T>( raList |> RAL.cons item )

   member x.Get i = raList |> RAL.get i 

   member x.Set(i, item) = new RandomAccessList<'T>( raList |> RAL.set i item )

   member x.Contains( item ) = RAL.contains Comparer<'T>.Default item raList

   member x.Reverse() = new RandomAccessList<'T>( raList |> RAL.rev )

   member s.Iterate(f) = RAL.iter f raList

   member x.Fold f state = RAL.fold f state raList

   member x.Filter predicate = new RandomAccessList<'T>( RAL.filter predicate raList )

   member x.Map (f: 'T -> 'U) = new RandomAccessList<'U>( RAL.map f raList )

   member x.MapIndexed (f: int -> 'T -> 'U) = new RandomAccessList<'U>( RAL.mapi f raList )

   member x.Exists predicate = RAL.exists predicate raList

   member x.TryFind predicate = RAL.tryFind predicate raList

   member x.ForAll predicate = RAL.forall predicate raList

   member x.ToList() = RAL.toList raList

   member x.ToArray() = RAL.toArray raList

   interface IEnumerable with
      member x.GetEnumerator() = 
         new RAL.ListEnumerator<'T>( raList ) :> IEnumerator

   interface IEnumerable<'T> with
      member x.GetEnumerator() = 
         new RAL.ListEnumerator<'T>( raList ) :> IEnumerator<'T>

   interface ICollection<'T> with
      member x.Count = x.Count
      member x.IsReadOnly = true
      member x.Add item = raise <| collectionIsReadOnly()
      member x.Remove item = raise <| collectionIsReadOnly()
      member x.Clear() = raise <| collectionIsReadOnly()
      member x.Contains item =  RAL.contains Comparer<'T>.Default item raList
      member x.CopyTo( array, arrayIndex ) = RAL.copyToArray raList array arrayIndex

   interface IReadOnlyList<'T> with
      member x.Count = x.Count
      member x.Item with get(idx) = x.Get idx

   static member Empty : RandomAccessList<'T> = empty

   // Returns a list containing the specified element.
   static member Singleton( item: 'T ) = 
      new RandomAccessList<'T>( RAL.cons item RAL.empty )

   // Returns a list containing all the items in the specified collection.
   static member Create( items : seq<'T> ) = 
      new RandomAccessList<'T>( items )

#if DOTNETCORE 
#else
   [<OnSerializing>]
   member private x.OnSerializing(context: StreamingContext) =
      serializedData <- box ( RAL.toArray raList)

   [<OnSerialized>]
   member private x.OnSerialized(context: StreamingContext) =
      serializedData <- null

   [<OnDeserialized>]
   member private x.OnDeserialized(context: StreamingContext) =
      let  oArray = unbox serializedData
      raList <- RAL.ofArray oArray
#endif

// Documentation in signature file.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RandomAccessList = 
   
   [<CompiledName("Empty")>]
   let inline empty<'T> = RandomAccessList<'T>.Empty

   [<CompiledName("IsEmpty")>]
   let inline isEmpty (list : RandomAccessList<'T>) = list.IsEmpty

   [<CompiledName("Singleton")>]
   let inline singleton item = RandomAccessList<'T>.Singleton(item)

   [<CompiledName("Count")>]
   let inline length (list : RandomAccessList<'T>) = list.Count
   
   [<CompiledName("Head")>]
   let inline head (list: RandomAccessList<'T>) = list.Head

   [<CompiledName("Tail")>]
   let inline tail (list: RandomAccessList<'T>) = list.Tail

   [<CompiledName("Last")>]
   let inline last (list: RandomAccessList<'T>) = list.[list.Count - 1]

   [<CompiledName("Cons")>]
   let inline cons item (list: RandomAccessList<'T>) = list.Cons item

   [<CompiledName("Get")>]
   let inline get index (list: RandomAccessList<'T>) = list.Get index

   [<CompiledName("Set")>]
   let inline set index item (list: RandomAccessList<'T>) = list.Set(index, item)

   [<CompiledName("Reverse")>]
   let inline rev (list : RandomAccessList<'T>) = list.Reverse()
      
   [<CompiledName("Contains")>]
   let inline contains item (list : RandomAccessList<'T>) = list.Contains item

   [<CompiledName("Iterate")>]
   let inline iter f (list : RandomAccessList<'T>) = list.Iterate(f)

   [<CompiledName("Exists")>]
   let inline exists pred (list : RandomAccessList<'T>) = list.Exists pred

   [<CompiledName("TryFind")>]
   let inline tryFind pred (list : RandomAccessList<'T>) = list.TryFind pred

   [<CompiledName("ForAll")>]
   let inline forAll pred (list: RandomAccessList<'T>) = list.ForAll pred

   [<CompiledName("Map")>]
   let inline map f (list : RandomAccessList<'T>) = list.Map f

   [<CompiledName("Map")>]
   let inline mapi f (list : RandomAccessList<'T>) = list.MapIndexed f

   [<CompiledName("Filter")>]
   let inline filter pred (list : RandomAccessList<'T>) = list.Filter pred

   [<CompiledName("Fold")>]
   let inline fold f state (list : RandomAccessList<'T>) = list.Fold f state

   [<CompiledName("OfSeq")>]
   let inline ofSeq (items: seq<'T>) = new RandomAccessList<'T>( items )

   [<CompiledName("OfList")>]
   let inline ofList items = new RandomAccessList<'T>(List.toSeq items)

   [<CompiledName("OfArray")>]
   let inline ofArray (items : 'T array) = new RandomAccessList<'T>( items )

   [<CompiledName("ToSeq")>]
   let inline toSeq (list : RandomAccessList<'T>) = (list :> seq<'T>)

   [<CompiledName("ToList")>]
   let inline toList (list : RandomAccessList<'T>) = list.ToList()

   [<CompiledName("ToArray")>]
   let inline toArray (list : RandomAccessList<'T>) = list.ToArray()
