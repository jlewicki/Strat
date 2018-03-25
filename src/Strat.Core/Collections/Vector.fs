namespace Strat.Collections

open System
open System.Collections
open System.Collections.Generic
open System.Threading
open Strat.Collections.Primitives
open Strat.Collections.Primitives.BitTrie


[<Sealed>]
type Vector<'T> (trie: Trie<'T>) =
   static let emptyInteriorNode : Node<'T> = BitTrie.emptyInteriorNode
   static let empty = new Vector<'T> (newTrie (0, Bits, emptyInteriorNode, Array.empty))
   static member Empty = empty
   
   
   new (items: seq<'T>) = 
      if isNull items then 
         raise <| ArgumentNullException("items")
      new Vector<'T> (ofSeq false items)


   member this.Count 
      with get() = trie.Count


   member this.IsEmpty
      with get() = trie.Count = 0


   member this.Item 
      with get (index: int) : 'T =
         if index < 0 || index >= trie.Count then 
            raise <| IndexOutOfRangeException (index.ToString())
         get index trie


   member this.Set(index: int, item: 'T) =
      new Vector<'T> (set index item trie)


   member this.TryLast = 
      if this.IsEmpty then None
      else Some this.[this.Count - 1]


   member this.Last
      with get() = 
         match this.TryLast with
         | Some item -> item
         | None -> raise <| new InvalidOperationException("Collection is emtpy")


   member this.Add (item: 'T) = 
      new Vector<'T> (add (item, trie))


   member this.RemoveLast() =
      let removed, trie = removeLast trie
      removed, new Vector<'T> (trie)


   interface IEnumerable with
      member this.GetEnumerator() = 
        createEnumerator (0, trie.Count, trie) :> IEnumerator


   interface IEnumerable<'T> with
      member this.GetEnumerator() = 
         createEnumerator (0, trie.Count, trie)


   interface IReadOnlyCollection<'T> with
      member this.Count = 
         this.Count


   interface IReadOnlyList<'T> with
      member this.Item
         with get (index: int) = this.[index]


   interface ITrieSource<'T> with
      member this.Trie = trie


   member this.Trie = trie


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
      match items with
      | :? Vector<'T> as l -> l
      | _ -> new Vector<'T> (items)


   [<CompiledName("OfArray")>]
   let inline ofArray (items: 'T[] ) = 
      Vector<'T> (items)


   [<CompiledName("OfList")>]
   let inline ofList (items: list<'T> ) = 
      Vector<'T> (items)


   [<CompiledName("Init")>]
   let init (count:int) (f:int -> 'T) =
      let tt = Transient.emptyTrie()
      let mutable i = 0
      while i < count do
         tt.Add (f i)
         i <- i + 1
      new Vector<'T> (tt.ToPersistentTrie())

   let inline add item (v: Vector<'T>) =
      v.Add item

   [<CompiledName("Append")>]
   let append (l1: Vector<'T>) (l2: Vector<'T>) =
      if l1.IsEmpty then l2
      elif l2.IsEmpty then l1
      else new Vector<'T> (append false l1.Trie l2.Trie)


   [<CompiledName("Length")>]
   let inline length (l: Vector<_>) = 
      l.Count


   [<CompiledName("IsEmpty")>]
   let inline isEmpty (l: Vector<_>) = 
      l.IsEmpty


   [<CompiledName("Get")>]
   let inline get (index: int) (l: Vector<'T>) = 
      l.[index]


   [<CompiledName("Last")>]
   let inline last (l: Vector<'T>) =
      l.Last

   [<CompiledName("TryLast")>]
   let inline tryLast (l: Vector<'T>) =
      l.TryLast


   [<CompiledName("Set")>]
   let inline set (index: int) (item:'T) (l: Vector<'T>) = 
      l.Set (index, item)


   [<CompiledName("RemoveLast")>]
   let inline removeLast (l: Vector<'T>) =
      l.RemoveLast()


   [<CompiledName("MapIndexed")>]
   let mapi (f: int -> 'T -> 'U) (l: Vector<'T>) = 
      new Vector<'U> (mapi false f l.Trie)


   [<CompiledName("Map")>]
   let map (f: 'T -> 'U) (l: Vector<'T>) = 
      new Vector<'U> (BitTrie.mapi false (fun _ item -> f item) l.Trie)


   [<CompiledName("Filter")>]
   let filter (f: 'T -> bool) (l: Vector<'T>) = 
      new Vector<'T> (filter false f l.Trie)

    
   [<CompiledName("Fold")>]
   let fold (f: 'State -> 'T -> 'State) (initial: 'State) (v: Vector<'T>) =
      fold false f initial v.Trie


   [<CompiledName("FoldBack")>]
   let foldBack (f:'T -> 'State -> 'State) (l: Vector<'T>) (initial: 'State) =
      foldBack false f l.Trie initial


   [<CompiledName("Collect")>]
   let collect (f: 'T -> Vector<'U>) (v:Vector<'T>) =
      new Vector<'U> (collect false (fun item -> (f item).Trie ) v.Trie)


   [<CompiledName("Choose")>]
   let choose (f: 'T -> 'U option) (v: Vector<'T>) =
      new Vector<'U> (choose false f v.Trie)


   [<CompiledName("Reverse")>]
   let rev (v: Vector<'T>) = 
      new Vector<'U> (rev false v.Trie)


   [<CompiledName("TryFind")>]
   let tryFind (predicate: 'T -> bool) (v: Vector<'T>) =
      tryFind false predicate v.Trie


   [<CompiledName("Find")>]
   let find (predicate: 'T -> bool) (v: Vector<'T>) =
      find false predicate v.Trie


   [<CompiledName("TryFindIndex")>]
   let tryFindIndex (predicate: 'T -> bool) (l: Vector<'T>) =
      tryFindIndex false predicate l.Trie

   [<CompiledName("FindIndex")>]
   let findIndex (predicate: 'T -> bool) (l: Vector<'T>) =
      findIndex false predicate l.Trie


   [<CompiledName("TryPick")>]
   let tryPick (f: 'T -> 'U option) (l: Vector<'T>) = 
      tryPick false f l.Trie


   [<CompiledName("Pick")>]
   let pick (f: 'T -> 'U option) (l: Vector<'T>) = 
      pick false f l.Trie


   [<CompiledName("Exists")>]

   let exists (predicate:'T -> bool) (l: Vector<'T>) : bool =
      exists false predicate l.Trie


   [<CompiledName("Zip")>]
   let zip (l1: Vector<'T>) (l2: Vector<'U>) =
      new Vector<'T*'U> (zip false l1.Trie l2.Trie)


   [<CompiledName("ForAll")>]
   let forall (predicate: 'T -> bool) (l: Vector<'T>) : bool =
      forall false predicate l.Trie


   [<CompiledName("Iterate")>]
   let iter (f: 'T -> unit) (l: Vector<'T>) =
      let iterAll (_:int) item = f item; true
      iteri (iterAll, 0, l.Count, l.Trie)


   [<CompiledName("IterateIndexed")>]
   let iteri (f: int -> 'T -> unit) (l: Vector<'T>) =
      let iterAll idx item = f idx item; true
      iteri (iterAll, 0, l.Count, l.Trie)


   [<CompiledName("Min")>]
   let min (l: Vector<'T>) =
      min false l.Trie


   [<CompiledName("MinBy")>]
   let minBy (f: 'T -> 'U) (l: Vector<'T>) =
      minBy false f l.Trie


   [<CompiledName("Max")>]
   let max (l: Vector<'T>) =
      max false l.Trie


   [<CompiledName("MaxBy")>]
   let maxBy (f: 'T -> 'U) (l: Vector<'T>) =
      maxBy false f l.Trie


   [<CompiledName("Sum")>]
   let inline sum (l: Vector<'T>) : 'T =
      sum false (l :> ITrieSource<'T>).Trie


   [<CompiledName("SumBy")>]
   let inline sumBy (f:'T -> ^U) (l: Vector<'T>) : ^U =
      sumBy false f (l :> ITrieSource<'T>).Trie


   [<CompiledName("ToSeq")>]
   let inline toSeq (l: Vector<'T>)  =
      l :> seq<'T>


   [<CompiledName("ToArray")>]
   let toArray (l: Vector<'T>) =   
      toArray false l.Trie


   [<CompiledName("ToList")>]
   let toList (l: Vector<'T>) =   
      toList false l.Trie