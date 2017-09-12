namespace Strat.Collections

open System
open System.Collections
open System.Collections.Generic
open System.Threading
open Strat.Collections.Primitives
open Strat.Collections.Primitives.BitTrie


// IndexList is almost identical to Vector, except that input/output indicies are reversed. Additionally the direction
// of iteration for various IndexList combinators is reversed.
[<Sealed>]
type IndexList<'T> (trie: Trie<'T>) =
   static let emptyInteriorNode : Node<'T> = BitTrie.emptyInteriorNode
   static let empty = new IndexList<'T> ( newTrie (0, Bits, emptyInteriorNode, Array.empty))
   static member Empty = empty
   
   
   new (items: seq<'T>) = 
      if isNull items then 
         raise <| ArgumentNullException("items")
      new IndexList<'T> (ofSeq false items |> rev)


   member this.Count 
      with get() = trie.Count


   member this.IsEmpty
      with get() = trie.Count = 0


   member this.Item 
      with get (index: int) : 'T =
         if index < 0 || index >= trie.Count then 
            raise <| IndexOutOfRangeException (index.ToString())
         let index = reverseIndex index trie.Count
         get index trie


   member this.Set(index: int, item: 'T) =
      let index = reverseIndex index trie.Count
      new IndexList<'T> (set index item trie)


   member this.TryHead = 
      if this.IsEmpty then None
      else Some this.[0]


   member this.Head
      with get() = 
         match this.TryHead with
         | Some item -> item
         | None -> raise <| new KeyNotFoundException()


   member this.Cons (item: 'T) = 
      new IndexList<'T> (add (item, trie))


   member this.RemoveHead() =
      let removed, trie = removeLast trie
      removed, new IndexList<'T> (trie)



   interface IEnumerable with
      member this.GetEnumerator() = 
        createRevEnumerator (0, trie.Count, trie) :> IEnumerator


   interface IEnumerable<'T> with
      member this.GetEnumerator() = 
         createRevEnumerator (0, trie.Count, trie)


   interface IReadOnlyCollection<'T> with
      member this.Count = 
         this.Count


   interface IReadOnlyList<'T> with
      member this.Item
         with get (index: int) = this.[reverseIndex index trie.Count]


   interface ITrieSource<'T> with
      member this.Trie = trie


   member this.Trie = trie
   

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IndexList =
   
   [<GeneralizableValue>]
   [<CompiledName("Empty")>]
   let empty<'T> = IndexList<'T>.Empty


   [<CompiledName("Singleton")>]
   let inline singleton (item:'T) =
      IndexList<'T> (Array.singleton item :> ICollection<'T>)


   [<CompiledName("OfSeq")>]
   let inline ofSeq (items: seq<'T>) = 
      match items with
      | :? IndexList<'T> as l -> l
      | _ -> new IndexList<'T> (items)


   [<CompiledName("OfArray")>]
   let inline ofArray (items: 'T[] ) = 
      IndexList<'T> (items)


   [<CompiledName("OfList")>]
   let inline ofList (items: list<'T> ) = 
      IndexList<'T> (items)


   [<CompiledName("Init")>]
   let init (count:int) (f:int -> 'T) =
      let tt = Transient.emptyTrie()
      let mutable i = 0
      while i < count do
         tt.Add (f i)
         i <- i + 1
      let reversed = rev (tt.ToPersistentTrie())
      new IndexList<'T> (reversed)


   [<CompiledName("Append")>]
   let append (l1: IndexList<'T>) (l2: IndexList<'T>) =
      if l1.IsEmpty then l2
      elif l2.IsEmpty then l1
      else
         let t = Transient.emptyTrie()
         let iteriAppend (_:int) (item: 'T) =
            t.Add item
            true
         // TODO: can we do this without iterating through one tree?
         iteri (iteriAppend, 0, l2.Count, l2.Trie)
         iteri (iteriAppend, 0, l1.Count, l1.Trie)
         new IndexList<'T> (t.ToPersistentTrie())


   [<CompiledName("Length")>]
   let inline length (l: IndexList<_>) = 
      l.Count


   [<CompiledName("IsEmpty")>]
   let inline isEmpty (l: IndexList<_>) = 
      l.IsEmpty


   [<CompiledName("Get")>]
   let inline get (index: int) (l: IndexList<'T>) = 
      l.[index]


   [<CompiledName("Head")>]
   let inline head (l: IndexList<'T>) =
      l.Head

   [<CompiledName("TryHead")>]
   let inline tryHead (l: IndexList<'T>) =
      l.TryHead


   [<CompiledName("Set")>]
   let inline set (index: int) (item:'T) (l: IndexList<'T>) = 
      l.Set (index, item)


   [<CompiledName("RemoveHead")>]
   let inline removeHead (l: IndexList<'T>) =
      l.RemoveHead()


   [<CompiledName("MapIndexed")>]
   let mapi (f: int -> 'T -> 'U) (l: IndexList<'T>) = 
      let f idx item = f (reverseIndex idx l.Count) item
      // Sad that we have to reverse at the end :(
      new IndexList<'U> (mapi true f l.Trie |> rev)


   [<CompiledName("Map")>]
   let map (f: 'T -> 'U) (l: IndexList<'T>) = 
      // Sad that we have to reverse at the end :(
      new IndexList<'U> (BitTrie.mapi true (fun _ item -> f item) l.Trie |> rev)


   [<CompiledName("Filter")>]
   let filter (f: 'T -> bool) (l: IndexList<'T>) = 
      new IndexList<'T> (filter true f l.Trie)

    
   [<CompiledName("Fold")>]
   let fold (f: 'State -> 'T -> 'State) (initial: 'State) (v: IndexList<'T>) =
      fold true f initial v.Trie


   [<CompiledName("FoldBack")>]
   let foldBack (f:'T -> 'State -> 'State) (l: IndexList<'T>) (initial: 'State) =
      foldBack true f l.Trie initial


   [<CompiledName("Collect")>]
   let collect (f: 'T -> IndexList<'U>) (v:IndexList<'T>) =
      new IndexList<'U> (collect true (fun item -> (f item).Trie ) v.Trie)


   [<CompiledName("Choose")>]
   let choose (f: 'T -> 'U option) (v: IndexList<'T>) =
      new IndexList<'U> (choose true f v.Trie |> rev)


   [<CompiledName("Reverse")>]
   let rev (v: IndexList<'T>) = 
      new IndexList<'U> (rev v.Trie)


   [<CompiledName("TryFind")>]
   let tryFind (predicate: 'T -> bool) (v: IndexList<'T>) =
      tryFind true predicate v.Trie


   [<CompiledName("Find")>]
   let find (predicate: 'T -> bool) (v: IndexList<'T>) =
      find true predicate v.Trie


   [<CompiledName("TryFindIndex")>]
   let tryFindIndex (predicate: 'T -> bool) (l: IndexList<'T>) =
      tryFindIndex true predicate l.Trie |> Option.map (fun idx -> reverseIndex idx l.Count)


   [<CompiledName("FindIndex")>]
   let findIndex (predicate: 'T -> bool) (l: IndexList<'T>) =
      let idx = findIndex true predicate l.Trie
      reverseIndex idx l.Count


   [<CompiledName("TryPick")>]
   let tryPick (f: 'T -> 'U option) (l: IndexList<'T>) = 
      tryPick true f l.Trie


   [<CompiledName("Pick")>]
   let pick (f: 'T -> 'U option) (l: IndexList<'T>) = 
      pick true f l.Trie


   [<CompiledName("Exists")>]

   let exists (predicate:'T -> bool) (l: IndexList<'T>) : bool =
      exists true predicate l.Trie


   [<CompiledName("Zip")>]
   let zip (l1: IndexList<'T>) (l2: IndexList<'U>) =
      new IndexList<'T*'U> (zip true l1.Trie l2.Trie |> BitTrie.rev)


   [<CompiledName("ForAll")>]
   let forall (predicate: 'T -> bool) (l: IndexList<'T>) : bool =
      forall true predicate l.Trie


   [<CompiledName("Iterate")>]
   let iter (f: 'T -> unit) (l: IndexList<'T>) =
      let iterAll (_:int) item = f item; true
      iteriRev (iterAll, 0, l.Count, l.Trie)


   [<CompiledName("IterateIndexed")>]
   let iteri (f: int -> 'T -> unit) (l: IndexList<'T>) =
      let iterAll idx item = f idx item; true
      iteriRev (iterAll, 0, l.Count, l.Trie)


   [<CompiledName("Min")>]
   let min (l: IndexList<'T>) =
      min true l.Trie


   [<CompiledName("MinBy")>]
   let minBy (f: 'T -> 'U) (l: IndexList<'T>) =
      minBy true f l.Trie


   [<CompiledName("Max")>]
   let max (l: IndexList<'T>) =
      max true l.Trie


   [<CompiledName("MaxBy")>]
   let maxBy (f: 'T -> 'U) (l: IndexList<'T>) =
      maxBy true f l.Trie


   [<CompiledName("Sum")>]
   let inline sum (l: IndexList<'T>) : 'T =
      sum true (l :> ITrieSource<'T>).Trie


   [<CompiledName("SumBy")>]
   let inline sumBy (f:'T -> ^U) (l: IndexList<'T>) : ^U =
      sumBy true f (l :> ITrieSource<'T>).Trie


   [<CompiledName("ToSeq")>]
   let inline toSeq (l: IndexList<'T>)  =
      l :> seq<'T>


   [<CompiledName("ToArray")>]
   let toArray (l: IndexList<'T>) =   
      toArray true l.Trie


   [<CompiledName("ToList")>]
   let toList (l: IndexList<'T>) =   
      toList true l.Trie