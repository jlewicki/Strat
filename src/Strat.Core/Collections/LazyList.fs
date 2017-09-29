namespace Strat.Collections

open System
open System.Collections
open System.Collections.Generic


[<NoEquality; NoComparison>]
type internal LNode<'T> = 
   | Empty
   | Cons of 'T * LazyList<'T>


and [<Sealed; NoEquality; NoComparison>] LazyList<'T> internal (lazyNode: Lazy<LNode<'T>>) = 
   static let empty = new LazyList<'T>(lazy Empty)

   static member Empty = empty


   member internal this.Node = lazyNode

   member this.IsEmpty = 
      match lazyNode with
      | Lazy Empty -> true
      | _ -> false


   member this.Length() = 
      let rec lengthAux length (list: LazyList<'T>) = 
         match list.Node with
         | Lazy Empty -> length
         | Lazy (Cons (_, rest)) -> lengthAux (length + 1) rest
      lengthAux 0 this


   member this.Head = 
      match lazyNode with
      | Lazy Empty -> invalidOp "Empty list"
      | Lazy (Cons (x, _)) -> x


   member this.TryHead = 
      match lazyNode with
      | Lazy Empty -> None
      | Lazy (Cons (x, _)) -> Some x


   member this.Tail = 
      match lazyNode with
      | Lazy Empty -> invalidOp "Empty list"
      | Lazy (Cons (_, rest)) -> rest


   member this.TryTail = 
      match lazyNode with
      | Lazy Empty -> None
      | Lazy (Cons (_, rest)) -> Some rest

   
   member this.Cons item =
      new LazyList<'T> (lazy (Cons(item, this)))


   member this.Uncons() = 
      match lazyNode with
      | Lazy Empty -> invalidOp "Empty list"
      | Lazy (Cons (item, rest)) -> struct (item, rest)


   member this.TryUncons() = 
      match lazyNode with
      | Lazy Empty -> None
      | Lazy (Cons (item, rest)) -> Some struct (item, rest)


   interface IEnumerable with
      member this.GetEnumerator() = 
        new LazyListEnumerator<'T> (this) :> IEnumerator


   interface IEnumerable<'T> with 
      member this.GetEnumerator() = 
         new LazyListEnumerator<'T> (this)  :> IEnumerator<'T>


and internal LazyListEnumerator<'T> (list: LazyList<'T>) = 
   let notStarted() = 
      raise <| new InvalidOperationException("The enumerator has not been started by a call to MoveNext")
   let alreadyCompleted() = 
      raise <| new InvalidOperationException("The enumerator has already completed.")
   let mutable isStarted = false
   let mutable currentList = list

   let current() =
      if isStarted then currentList.Head
      else notStarted()
   
   let moveNext() =
      if isStarted then 
         if currentList.IsEmpty then alreadyCompleted()
         else 
            currentList <- currentList.Tail
            not currentList.IsEmpty
      else
          isStarted <- true
          not currentList.IsEmpty

   interface IEnumerator<'T> with
      member x.Current = current()
   interface IEnumerator with 
      member x.Current = box (current())
      member x.MoveNext() = moveNext()
      member x.Reset() = currentList <- list
   interface IDisposable with 
      member x.Dispose() = () 


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LazyList =

   [<GeneralizableValue>]
   [<CompiledName("Empty")>]
   let empty<'T> = LazyList<'T>.Empty

   let internal toLazyList (node: Lazy<LNode<'T>>) = 
      new LazyList<'T> (node) 


   [<CompiledName("OfSeq")>]
   let ofSeq (items: seq<'T>) = 
      // This eagerly consumes the seq, which seems to defeat the purpose of a lazy list, but would it be safe to only 
      // consume at some point in the future, since we don't know if seq is threadsafe/persistent?
      items 
      |> Seq.rev
      |> Seq.fold (fun ll item -> 
          // Since we are not lazily building the list, we'll go ahead a force/cache the lazy value
          let forcedCell = lazy (Cons (item, ll))
          forcedCell.Value |> ignore
          forcedCell |> toLazyList
      ) empty


   let rec ofSeqLazy (items: seq<'T>) = 
      toLazyList <| lazy  
         if items |> Seq.isEmpty then Empty
         else  Cons(Seq.head items, ofSeqLazy (Seq.skip 1 items))


   [<CompiledName("OfArray")>]
   let ofArray (items: 'T[]) = 
      empty 
      |> Array.foldBack (fun item ll -> 
          lazy (Cons (item, ll)) |> toLazyList
      ) items


   [<CompiledName("OfList")>]
   let ofList (items: list<'T>) = 
      empty 
      |> List.foldBack (fun item ll -> 
          lazy (Cons (item, ll)) |> toLazyList
      ) items


   [<CompiledName("Init")>]
   let init length initializer : LazyList<'T> = 
      let rec initAux idx length (initializer: int -> 'T) = 
         let rest = 
            if idx = length - 1 then lazy Empty 
            else lazy initAux (idx + 1) length initializer 
         (Cons ((initializer idx), toLazyList rest))
      toLazyList <| lazy initAux 0 length initializer


   [<CompiledName("Replicate")>]
   let replicate length value : LazyList<'T> = 
      let rec replicateAux remainining value = 
         if remainining = 0 then Empty
         else
            let rest = lazy replicateAux (remainining - 1) value |> toLazyList 
            Cons (value, rest)
      toLazyList <| lazy replicateAux length value


   [<CompiledName("Unfold")>]
   let unfold (f:'State -> ('T * 'State) option) (initial:'State) : LazyList<'T> = 
      let rec unfoldAux state f  = 
         match f state with
         | Some (nextItem, nextState) -> Cons (nextItem, toLazyList <| lazy unfoldAux nextState f)
         | None -> Empty
      toLazyList <| lazy unfoldAux initial f


   [<CompiledName("IsEmpty")>]
   let isEmpty (list: LazyList<'T>) =
      list.IsEmpty

   
   [<CompiledName("Length")>]
   let length (list: LazyList<'T>) =
      list.Length()


   [<CompiledName("Head")>]
   let head (list: LazyList<'T>) = 
      list.Head


   [<CompiledName("TryHead")>]
   let tryHead (list: LazyList<'T>) = 
      list.TryHead 


   [<CompiledName("Tail")>]
   let tail (list: LazyList<'T>) = 
      list.Tail 


   [<CompiledName("TryTail")>]
   let tryTail (list: LazyList<'T>) = 
      list.TryTail 


   [<CompiledName("Cons")>]
   let cons (item:'T) (list:LazyList<'T>) : LazyList<'T> =
      list.Cons item 

   [<CompiledName("Uncons")>]
   let uncons (list:LazyList<'T>) = 
      list.Uncons()


   [<CompiledName("TryUncons")>]
   let tryUncons (list:LazyList<'T>) = 
      list.TryUncons()


   [<CompiledName("Map")>]  
   let rec map f (list: LazyList<'T>) : LazyList<'U> = 
      toLazyList <| lazy
         (match list.Node with
         | Lazy Empty -> Empty
         | Lazy (Cons (x, rest)) -> (Cons (f x, (map f rest))))


   [<CompiledName("MapIndexed")>]  
   let rec mapi f (list: LazyList<'T>) : LazyList<'U> = 
      let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (f)
      lazy (mapiAux f 0 list) |> toLazyList
   and private mapiAux (f: OptimizedClosures.FSharpFunc<_,_,_>) index (list: LazyList<'T>) : LNode<'U> = 
      match list.Node with
      | Lazy Empty -> Empty
      | Lazy (Cons (x, rest)) -> 
         Cons (f.Invoke (index, x), lazy (mapiAux f (index + 1) rest) |> toLazyList) 

    
   [<CompiledName("Filter")>]
   let rec filter predicate (list: LazyList<'T>) : LazyList<'T> =
      let rec filterAux predicate (list: LazyList<'T>) : LNode<'T> = 
         match list.Node with
         | Lazy Empty -> Empty
         | Lazy (Cons (x, rest)) -> 
            if predicate x then Cons (x, lazy (filterAux predicate rest) |> toLazyList) 
            else filterAux predicate rest
      lazy (filterAux predicate list) |> toLazyList


   [<CompiledName("Fold")>]
   let rec fold (f: 'State -> 'T -> 'State) (state: 'State) (list: LazyList<'T>) = 
      foldOpt (OptimizedClosures.FSharpFunc<_,_,_>.Adapt (f)) state list
   and foldOpt (f: OptimizedClosures.FSharpFunc<_,_,_>) (state: 'State) (list: LazyList<'T>) = 
      match list.Node with
      | Lazy Empty -> state
      | Lazy (Cons (x, rest)) -> foldOpt f (f.Invoke (state, x)) rest


   [<CompiledName("Iterate")>]
   let rec iter f (list: LazyList<'T>) = 
      match list.Node with
      | Lazy Empty -> ()
      | Lazy (Cons (x, rest)) -> f x; iter f rest


   [<CompiledName("IterateIndexed")>]
   let rec iteri f (list: LazyList<'T>) = 
      iteriOpt (OptimizedClosures.FSharpFunc<_,_,_>.Adapt (f)) 0 list
   and iteriOpt (f:OptimizedClosures.FSharpFunc<_,_,_>) idx (list: LazyList<'T>) = 
      match list.Node with
      | Lazy Empty -> ()
      | Lazy (Cons (x, rest)) -> f.Invoke (idx, x); iteriOpt f (idx + 1) rest


   [<CompiledName("Choose")>]
   let choose f (list: LazyList<'T>) : LazyList<'U> = 
      let rec chooseAux (f: 'T -> 'U option) (list: LazyList<'T>) : LNode<'U> = 
         match list.Node with
         | Lazy Empty -> Empty
         | Lazy (Cons (x, rest)) -> 
            match f x with 
            | Some u -> Cons (u, lazy (chooseAux f rest) |> toLazyList)
            | None -> chooseAux f rest
      lazy (chooseAux f list) |> toLazyList


   [<CompiledName("Exists")>]
   let rec exists (predicate: 'T -> bool) (list: LazyList<'T>) : bool = 
      match list.Node with
      | Lazy Empty -> false
      | Lazy (Cons (x, rest)) -> 
         if predicate x then true
         else exists predicate rest


   [<CompiledName("Find")>]
   let rec find (predicate: 'T -> bool) (list: LazyList<'T>) : 'T = 
      match list.Node with
      | Lazy Empty -> raise <| new KeyNotFoundException()
      | Lazy (Cons (x, rest)) -> 
         if predicate x then x
         else find predicate rest


   [<CompiledName("TryFind")>]
   let rec tryFind (predicate: 'T -> bool) (list: LazyList<'T>) : option<'T> = 
      match list.Node with
      | Lazy Empty -> None
      | Lazy (Cons (x, rest)) -> 
         if predicate x then Some x
         else tryFind predicate rest


   [<CompiledName("Pick")>]
   let rec pick (chooser:'T -> 'U option) (list:LazyList<'T>) : 'U =
      match list.Node with
      | Lazy Empty -> raise <| new KeyNotFoundException()
      | Lazy (Cons (t, rest)) -> 
         match chooser t with
         | Some u -> u
         | None -> pick chooser rest


   [<CompiledName("TryPick")>]
   let rec tryPick (chooser:'T -> 'U option) (list:LazyList<'T>) : 'U option =
      match list.Node with
      | Lazy Empty -> None
      | Lazy (Cons (t, rest)) -> 
         match chooser t with
         | None -> tryPick chooser rest
         | _ as s -> s


   [<CompiledName("Get")>]
   let get idx (list: LazyList<'T>) = 
      if idx < 0 then raise <| new IndexOutOfRangeException(idx.ToString())
      let rec getAux currentIdx idx (list: LazyList<'T>) =
         match list.Node with
         | Lazy Empty -> raise <| new IndexOutOfRangeException(idx.ToString())
         | Lazy (Cons (x, rest)) ->  if currentIdx = idx then x else getAux (currentIdx + 1) idx rest
      getAux 0 idx list


   [<CompiledName("ForAll")>]
   let forall (predicate: 'T -> bool) (list: LazyList<'T>) : bool = 
      let rec forallAux result (predicate: 'T -> bool) (list: LazyList<'T>) =
         match list.Node with
         | Lazy Empty -> result
         | Lazy (Cons (x, rest)) -> 
            if predicate x then forallAux true predicate rest
            else false
      forallAux false predicate list


   [<CompiledName("Append")>]
   let rec append (list1: LazyList<'T>) (list2: LazyList<'T>) : LazyList<'T> =  
      toLazyList <| lazy
         match list1.Node with
         | Lazy Empty -> list2.Node.Value
         | Lazy (Cons (x, rest)) -> (Cons (x, append rest list2))


   [<CompiledName("Collect")>]
   let collect (f: 'T -> LazyList<'U>) (list: LazyList<'T>) : LazyList<'U> = 
      let rec collectAux (f: 'T -> LazyList<'U>) (list: LazyList<'T>) : LNode<'U> = 
         match list.Node with
         | Lazy Empty -> Empty
         | Lazy (Cons (x, rest)) -> 
            match (f x).Node with 
            | Lazy Empty -> 
               collectAux f rest
            | Lazy (Cons (u, restU)) ->
               let restU = append restU (lazy (collectAux f rest) |> toLazyList)
               Cons (u, restU)
      lazy (collectAux f list) |> toLazyList


   [<CompiledName("Reverse")>]
   let rev (list: LazyList<'T>) : LazyList<'T> =
      let rec revAux (list: LazyList<'T>) revNode : LNode<'T> = 
         match list.Node with
         | Lazy Empty -> revNode
         | Lazy (Cons (x, rest)) -> 
            revAux rest (Cons (x, lazy revNode |> toLazyList))
      lazy (revAux list Empty) |> toLazyList


   let rec ofListLazy (items: list<'T>) = 
      toLazyList <| lazy  
         match items with
         | [] -> Empty
         | x::rest -> Cons(x, ofListLazy rest)


   [<CompiledName("ToSeq")>]
   let toSeq (list: LazyList<'T>) = 
      list :> seq<'T>


   [<CompiledName("ToList")>]
   let toList (list: LazyList<'T>) =
      list
      |> fold (fun l item -> item::l) List.empty
      |> List.rev


   [<CompiledName("ToArray")>]
   let toArray (list: LazyList<'T>) = 
      let arr = Array.zeroCreate (list.Length())
      let mutable idx = 0
      list |> iter (fun item -> arr.[idx] <- item; idx <- idx + 1)
      arr


   let (|Empty|Cons|) (list: LazyList<'T>) = 
      match list.Node with
         | Lazy LNode.Empty -> Empty
         | Lazy (LNode.Cons (x, rest)) -> Cons(x, rest) 
    