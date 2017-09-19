namespace Strat.Collections.Primitives

open System
open System.Collections
open System.Collections.Generic


// An implementation of a priority search pennants as described in the paper "A Simple Implementation Technique for 
// Priority Search Queues" by R. Hinze.
module Pennant = 
   // Set to true to assert that tree is always balanced.  
   let private validateBalance = false


   // The priority search queue is defined in terms of a semi-heap strucure called a pennant.  This pennant is described
   // in relation to a tournament tree, hence the winner and loser nomenclature. Note that winnerKey and winnerValue
   // logically form a tuple/pair, but they are split out into discrete properties for efficiency. Also note that
   // the loser tree has its length as a discrete property to ensure constant-time access.
   [<NoEquality; NoComparison>]
   type Pennant<'K, 'V when 'V: comparison> = 
      | Void
      | Winner of WinnerKey:'K * WinnerValue: 'V * LTree:LoserTree<'K, 'V> * MaxKey:'K
   and [<NoEquality; NoComparison>] LoserTree<'K, 'V> =
      | Lf
      | Nd of LoserKey:'K * LoserValue:'V * Left:LoserTree<'K, 'V> * SplitKey:'K * Right:LoserTree<'K, 'V> * Length:int 


   // Provides access to a bit trie. Useful for keeping Trie<'T> out of main collection APIs.
   type IPennantSource<'K, 'V when 'V: comparison> = 
      abstract member Pennant: Pennant<'K, 'V> 


   // Returns the number of items in the tree.  This is O(1).
   let private lengthTree tree = 
      match tree with
      | Lf -> 0
      | Nd(_, _, _, _, _,length) -> length


   // Tree constructors that hide length details.
   let private node key value leftTree splitKey rightTree = 
      Nd(key, value, leftTree, splitKey, rightTree, 1 + lengthTree leftTree + lengthTree rightTree)      
   let private leaf = Lf


   // Tree deconstructor.
   let private (|Leaf|Node|) tree = 
      match tree with
      | Lf -> Leaf
      | Nd(key, value,left, splitKey, right, length) ->
         Node(key, value,left, splitKey, right)


   // An empty pennant.
   let empty : Pennant<'K, 'V> = Void


   // Returns a value indicating if the pennant is empty.  This is O(1).
   let isEmpty pennant  =
      match pennant with
      | Void -> true
      | _ -> false


   // Returns the number of items in the pennant. This is O(1).
   let length pennant =
      match pennant with
      | Void -> 0
      | Winner( _, _, Lf, _) -> 1
      | Winner( _, _, Nd(_, _, _, _, _,length), _) -> length + 1


   // Returns a pennant containing the specified key and value.
   let singleton key value = 
      Winner (key, value, Lf, key)


   // Returns a pennant containing the key and value of the specified pair.
   let ofTuple (key, value) = 
      singleton key value


   // Returns the minimum key and value in the pennant, or throws if pennant is empty.  This is O(1).
   let minBinding pennant =
      match pennant with
      | Void -> invalidOp "empty pennant"
      | Winner( k, v, _, _) -> struct (k, v)


   // Returns the minimum key and value in the pennan, or None if the pennant is empty.  This is O(1).
   let peekMinBinding pennant =
      match pennant with
      | Void -> None
      | Winner( k, v, _, _) -> Some struct (k, v)
      

   // Returns the key of the binding with the maximum value in the pennant.  This is O(1).
   let maxKey pennant =
      match pennant with
      | Void -> invalidOp "empty pennant"
      | Winner( _, _, _, max) -> max


   // Returns the maximum depth of the tree.  This is O(lgN)
   let rec private depth tree =
      match tree with
      | Leaf -> 1
      | Node( _, _, left, _, right) -> 1 + max (depth left) (depth right) 


   // Returns a tree node that balances the nodes in the left at right trees. See the paper for more details, and more 
   // particularly: Stephen Adams. Functional pearls: Efficient sets -- a balancing act
   module internal Balance =
      let assertBalanced = function 
         | Leaf -> leaf
         | Node( _, _, left, _, right) as node ->
            let depthl = depth left
            let depthr = depth right
            assert ((abs (depthl - depthr)) <= 1)
            node

      // Rotation functions
      let singleLeft (kcomp: IComparer<'K>) (vcomp: IComparer<'V>) key value left splitKey right = 
         match right with
         | Leaf -> leaf
         | Node (key2, value2, left2, splitKey2, right2) -> 
            if kcomp.Compare(key2,splitKey2) <= 0 && vcomp.Compare(value, value2) <= 0 then 
               node key value (node key2 value2 left splitKey left2) splitKey2 right2
            else 
               node key2 value2 (node key value left splitKey left2) splitKey2 right2
      
      let singleRight (kcomp: IComparer<'K>) (vcomp: IComparer<'V>) key value left splitKey right = 
         match left with
         | Leaf -> leaf
         | Node (key2, value2, left2, splitKey2, right2) -> 
            if kcomp.Compare(key2,splitKey2) > 0 && vcomp.Compare(value, value2) <= 0 then 
               node key value left2 splitKey2 (node key2 value2 right2 splitKey right)
            else 
               node key2 value2 left2 splitKey2 (node key value right2 splitKey right)
          
      let doubleLeft (kcomp: IComparer<'K>) (vcomp: IComparer<'V>) key value left splitKey right = 
         match right with
         | Leaf -> leaf
         | Node (key2, value2, left2, splitKey2, right2) -> 
            singleLeft kcomp vcomp key value left splitKey (singleRight kcomp vcomp key2 value2 left2 splitKey2 right2)

      let doubleRight (kcomp: IComparer<'K>) (vcomp: IComparer<'V>) key value left splitKey right = 
         match left with
         | Leaf -> leaf
         | Node (key2, value2, left2, splitKey2, right2) -> 
            singleRight kcomp vcomp key value (singleLeft kcomp vcomp key2 value2 left2 splitKey2 right2) splitKey right

      // Balance functions
      let balanceLeft (kcomp: IComparer<'K>) (vcomp: IComparer<'V>) key value left splitKey right = 
         match right with
         | Leaf -> leaf
         | Node (key2, value2, left2, splitKey2, right2) ->
            if lengthTree left2 < lengthTree right2 then 
               singleLeft kcomp vcomp key value left splitKey right
            else 
               doubleLeft kcomp vcomp key value left splitKey right 

      let balanceRight (kcomp: IComparer<'K>) (vcomp: IComparer<'V>) key value left splitKey right = 
         match left with
         | Leaf -> leaf
         | Node (key2, value2, left2, splitKey2, right2) ->
            if lengthTree right2 < lengthTree left2 then 
               singleRight kcomp vcomp key value left splitKey right
            else 
               doubleRight kcomp vcomp key value left splitKey right 

      let balance (kcomp: IComparer<'K>) (vcomp: IComparer<'V>) key value left splitKey right =
         let weightFactor = 4
         let lenl = lengthTree left
         let lenr = lengthTree right   
         if lenl + lenr < 2 then 
            node key value left splitKey right
         elif lenr > weightFactor * lenl then 
            balanceLeft kcomp vcomp key value left splitKey right
         elif lenl > weightFactor * lenr then 
            balanceRight kcomp vcomp key value left splitKey right
         else 
            node key value left splitKey right 


   // Merges two pennants and returns a new pennant, such that keys in the first tree are strictly smaller than keys 
   // in the second tree. This is O(1).
   let private merge kcomp (vcomp: IComparer<'V>) pennant1 pennant2 = 
      match struct (pennant1, pennant2) with
      | struct (Void, _) -> pennant2
      | struct (_, Void) -> pennant1
      | struct (Winner (key1, value1, ltree1, max1), Winner( key2, value2, ltree2, max2)) ->
         if vcomp.Compare(value1, value2) < 0 then
            Winner (key1, value1, (Balance.balance kcomp vcomp key2 value2 ltree1 max1 ltree2), max2)
         else
            Winner (key2, value2, (Balance.balance kcomp vcomp key1 value1 ltree1 max1 ltree2), max2)
       

   // Returns a pennant containing values from the specified list, which *must* be sorted by key, in ascending order.
   // This is O(N).
   let fromOrderedList kcomp vcomp (bindings: list<'K *'V>) : Pennant<'K, 'V> = 
      // A variation of left and right fold that folds a list in a binary sub-division fashion, producing an almost 
      // balanced tree. The  expression tree  generated by foldm takes the form of a leaf-oriented Braun tree: for any 
      // given subexpression f l r , the left part l has either the same number of leaves as the right part, or one leaf 
      // more. 
      let foldm f state items =
         match items with
         | [] -> state
         | _ -> 
            let rec recurse length items = 
               match length, items with
               | 1, (x::xs) -> x, xs
               | n, xs ->
                  let m = n / 2
                  let x1, xs1 = recurse (n - m) xs
                  let x2, xs2 = recurse m xs1
                  f x1 x2, xs2
            fst (recurse (List.length items) items)

      bindings 
      |> List.map ofTuple
      |> foldm (merge kcomp vcomp) empty
         

   // Returns a pennant containing the values from the specfied sequence. If ordered is false, the values will be 
   // sorted before adding to the pennant.
   let ofSeq kcomp vcomp ordered (items: seq<'K*'V>) = 
      let items = if ordered then items else items |> Seq.sortBy fst
      items  
      |> Seq.toList
      |> fromOrderedList kcomp vcomp


   // View of a pennant as a priority queue. That is, a pennant is either
   //  - Void (empty)
   //  - Min, which gives the minimum entry in the queue, and another pennant representing the 'next best'
   //    entries. 
   [<Struct>]
   type private PriorityQueueView<'K,'V when 'V: comparison> = 
      | Empty 
      | Min of Key:'K * Val:'V * Rest: Pennant<'K,'V>

   let private toPriorityQueueView (kcomp: IComparer<'K>) (vcomp: IComparer<'V>) pennant =
      let rec secondBest (kcomp: IComparer<'K>) (vcomp: IComparer<'V>) loserTree key  = 
         match loserTree, key with
         | Leaf, _ -> Void
         | Node(loserKey, loserValue, ltree, splitKey, rtree), m ->
            if kcomp.Compare(loserKey, splitKey) <= 0 then
               merge kcomp vcomp (Winner(loserKey, loserValue, ltree, splitKey)) (secondBest kcomp vcomp rtree m)
            else 
               merge kcomp vcomp (secondBest kcomp vcomp ltree splitKey) (Winner(loserKey, loserValue, rtree, m))

      match pennant with
      | Void -> Empty
      | Winner (key, value, ltree, maxKey) -> Min (key, value, (secondBest kcomp vcomp ltree maxKey))


   // View of a pennant as a tournament tree.  That is, a pennant is either
   // - Empty
   // - Singleton: A tree containing single entry
   // - Merged: The result of merging two pennants to determine a winner. All keys in the first pennant are strictly 
   //   less than keys in the second pennant. This is effectively the inverse of the merge function. 
   // It would seem natural to make this an active pattern, but given the extensive usage it causes a large number
   // of extraneous allocations, so we use a struct instead.
   [<Struct>]
   type private TournamentView<'K,'V when 'V: comparison> =
      | Empty
      | Singleton of EntryKey:'K * EntryVal:'V
      | Merged of Winners: Pennant<'K,'V> * Losers: Pennant<'K,'V> 

   let private toTournamentView (kcomp: IComparer<'K>) pennant =
      match pennant with
      | Void -> Empty
      | Winner (key, value, Leaf, _) -> Singleton (key, value)
      | Winner (key, value, (Node(lkey, lvalue, leftTree, splitKey, rightTree)), maxKey) ->  
         let struct (pennant1, pennant2) = 
            // TODO: Paper suggests splitting Winner case into two cases to avoid this comparison 
            if kcomp.Compare(lkey, splitKey) <= 0 then
               struct (Winner (lkey, lvalue, leftTree, splitKey), Winner(key, value, rightTree, maxKey))
            else
               struct (Winner (key, value, leftTree, splitKey), Winner(lkey, lvalue, rightTree, maxKey))
         Merged (pennant1, pennant2)


   // Returns the binding with the minimum value in the queue, and the pennant with that binding removed. This is
   // O(lgN).  
   let tryRemoveMin kcomp vcomp pennant = 
      match toPriorityQueueView kcomp vcomp pennant with
      | PriorityQueueView.Empty -> None
      | Min (k, v, rest) -> Some (k, v, rest)


   // O(lgN). Returns the value associated with the specified key in the pennant, or throws.
   let rec find (comparer: IComparer<'K>) key pennant =
      match pennant with
      | Void -> raise (KeyNotFoundException(sprintf "%A" key))
      | Winner (k, v, ltree, _) -> 
         if comparer.Compare (key, k) = 0 then  v
         else findTree comparer key ltree
   and private findTree (comparer: IComparer<'K>) key tree =
      match tree with 
      | Lf -> raise (KeyNotFoundException(sprintf "%A" key))
      | Nd (k, v, left, splitKey, right, _) ->
         if comparer.Compare (key, k) = 0 then v 
         elif comparer.Compare (key, splitKey) <= 0 then findTree comparer key left
         else findTree comparer key right


   // O(lgN). Returns the value associated with the specified key in the pennant, or None
   let rec tryFind (comparer: IComparer<'K>) key pennant =
      match pennant with
      | Void -> None
      | Winner (k, v, ltree, _) -> 
         if comparer.Compare (key, k) = 0 then Some v
         else tryFindTree comparer key ltree
    and private tryFindTree (comparer: IComparer<'K>) key tree =
      match tree with 
      | Lf -> None
      | Nd (k, v, left, splitKey, right, _) ->
         if comparer.Compare (key, k) = 0 then Some v 
         elif comparer.Compare (key, splitKey) <= 0 then tryFindTree comparer key left
         else tryFindTree comparer key right


   // Returns pennant, with the value of the specified key adjusted by applying the specied function to the current 
   // value. This is O(lgN) on average.
   let rec adjust (kcomp: IComparer<'K>) vcomp f key pennant = 
      match toTournamentView kcomp pennant with 
      | Empty -> 
         pennant
      | Singleton (k, v) -> 
         if kcomp.Compare (key, k) = 0 then singleton k (f v) else pennant
      | Merged (pennant1, pennant2) -> 
         if kcomp.Compare (key,maxKey pennant1) <= 0 then 
            merge kcomp vcomp (adjust kcomp vcomp f key pennant1) pennant2
         else 
            merge kcomp vcomp pennant1 (adjust kcomp vcomp f key pennant2) 


   // Inserts the specified key and value into pennant, and returns an updated pennant.  If the pennant already 
   // contains the key, the corresponding value is replaced.  This is O(lgN) on average. 
   let rec insert (kcomp: IComparer<'K>) vcomp key value pennant = 
      match toTournamentView kcomp pennant with 
      | Empty -> singleton key value
      | Singleton (k, _) -> 
         let c = kcomp.Compare (key, k)
         if c < 0 then merge kcomp vcomp (singleton key value) pennant
         elif c = 0 then singleton key value  // Update existing value
         else merge kcomp vcomp pennant (singleton key value) 
      | Merged (pennant1, pennant2) -> 
         if kcomp.Compare (key, maxKey pennant1) <= 0 then 
            merge kcomp vcomp (insert kcomp vcomp key value pennant1) pennant2
         else 
            merge kcomp vcomp pennant1 (insert kcomp vcomp key value pennant2) 


   // Removes the entry with the specified key from the pennant, and returns an updated pennant, and a flag indicating 
   // if an item was removed.
   let rec delete (kcomp: IComparer<'K>) vcomp key pennant = 
      match toTournamentView kcomp pennant with 
      | Empty -> struct (false, pennant)
      | Singleton (k, _) -> 
         if kcomp.Compare (key,k) = 0 then struct (true, empty) else struct (false, pennant)
      | Merged (pennant1, pennant2) -> 
         if kcomp.Compare (key,maxKey pennant1) <= 0 then 
            let struct (deleted, p) = delete kcomp vcomp key pennant1
            if deleted then struct (true, merge kcomp vcomp p pennant2) else struct (false, pennant)
         else 
            let struct (deleted, p) = delete kcomp vcomp key pennant2
            if deleted then struct (true, merge kcomp vcomp pennant1 p) else struct (false, pennant)

   
   // Returns an ordered list of the keys in the pennant.
   let rec toOrderedList (kcomp: IComparer<'K>) pennant =
      match toTournamentView kcomp pennant with 
      | Empty -> []
      | Singleton (k, v) -> [k]
      | Merged(pennant1, pennant2) ->
        List.append (toOrderedList kcomp pennant1) (toOrderedList kcomp pennant2)


   // Returns a list of entries, ordered by key, that contain values from the specified pennant that are less than or 
   // equal to the specified value.
   let rec atMost (kcomp: IComparer<'K>) value pennant =
      let struct (mink, minv) = minBinding pennant 
      if minv > value then []
      else
         match toTournamentView kcomp pennant with
         | Empty -> []
         | Singleton (k, v) -> [(k, v)] // Since we know v <= value
         | Merged(pennant1, pennant2) ->
            List.append (atMost kcomp value pennant1) (atMost kcomp value pennant2) 


   // O(N). Applies the f to each entry in the pennant.
   let rec iter f pennant =
      match pennant with
      | Void -> ()
      | Winner (k, v, ltree, _) -> f k v; iterTree f ltree
   and private iterTree f tree =
      match tree with 
      | Lf -> ()
      | Nd (k, v, left, splitKey, right, _) -> f k v; iterTree f left; iterTree f right


   // O(N). Applies the f to each entry in the pennant, in order of ascending key value. This is O(N).
   let rec iterOrdered (kcomp: IComparer<'K>) f pennant = 
      match toTournamentView kcomp pennant with 
      | Empty -> ()
      | Singleton (k, v) -> f k v
      | Merged (pennant1, pennant2) ->
         iterOrdered kcomp f pennant1
         iterOrdered kcomp f pennant2 


   // Applies the f to each entry in the pennant, while f returns true. This is O(N).
   let rec iterWhile f pennant = 
       iterWhileOpt (OptimizedClosures.FSharpFunc<_,_,_>.Adapt (f)) pennant   
   and private iterWhileOpt (f:OptimizedClosures.FSharpFunc<_,_,_>) pennant =
      match pennant with
      | Void -> false
      | Winner (k, v, ltree, _) -> if f.Invoke(k, v) then iterWhileOptTree f ltree else false
   and private iterWhileOptTree f tree =
      match tree with 
      | Lf -> true
      | Nd (k, v, left, splitKey, right, _) -> 
         if f.Invoke(k, v) then
            if iterWhileOptTree f left then
               iterWhileOptTree f right
            else false
         else false


   // Applies the f to each entry in the pennant, while f returns true, in order of ascending key value. This is O(N).
   let rec iterOrderedWhile (kcomp: IComparer<'K>) f pennant = 
       iterOrderedWhileOpt kcomp (OptimizedClosures.FSharpFunc<_,_,_>.Adapt (f)) pennant
   and private iterOrderedWhileOpt (kcomp: IComparer<'K>) (f:OptimizedClosures.FSharpFunc<_,_,_>) pennant =
      match toTournamentView kcomp pennant with
      | Empty -> false
      | Singleton (k, v) -> f.Invoke (k, v)
      | Merged(pennant1, pennant2) ->
         let cont = iterOrderedWhileOpt kcomp f pennant1
         if cont then iterOrderedWhileOpt kcomp  f pennant2  else false


   // O(N). Returns a value indicating if the pennant contains a binding that matches the specified predicate.
   let exists f pennant = 
      let mutable found = false
      iterWhile (fun k v  -> found <- (f k v); not found) pennant |> ignore
      found

   // O(N). Returns a value indicating if all bindings in the pennant match the specified predicate.
   let forAll f pennant = 
      let mutable allTrue = false
      iterWhile (fun k v  -> allTrue <- (f k v); allTrue) pennant |> ignore
      allTrue


   // O(N). Searches the pennant looking for the first element where the given function returns a Some value. 
   let tryPick f pennant = 
      let mutable picked = None
      iterWhile (fun k v  -> picked <- (f k v); not picked.IsSome) pennant |> ignore
      picked


   // O(NlgN). Returns a new pennant whose values are the results of applying the given function to each of the 
   // elements of the pennant . The key passed to the function indicates the key of element being transformed.
   let rec map (kcomp: IComparer<'K>) (vcomp: IComparer<'U>) f (pennant: Pennant<'K, 'V>) : Pennant<'K, 'U>=
      mapOpt kcomp vcomp (OptimizedClosures.FSharpFunc<_,_,_>.Adapt (f)) pennant
   and private mapOpt (kcomp: IComparer<'K>) vcomp  (f:OptimizedClosures.FSharpFunc<_,_,_>) pennant =
      match toTournamentView kcomp pennant with
      | Empty -> empty
      | Singleton (k, v) -> singleton k (f.Invoke (k, v))
      | Merged(pennant1, pennant2) -> 
         merge kcomp vcomp (mapOpt kcomp vcomp f pennant1) (mapOpt kcomp vcomp f pennant2)


   // O(NlgN). Returns a pennant containing only those entries for which the predicate returns true.
   let rec filter (kcomp: IComparer<'K>) vcomp f pennant =
      filterOpt kcomp vcomp (OptimizedClosures.FSharpFunc<_,_,_>.Adapt (f)) pennant
   and private filterOpt (kcomp: IComparer<'K>) vcomp (f:OptimizedClosures.FSharpFunc<_,_,_>) pennant =
      match toTournamentView kcomp pennant with
      | Empty -> pennant
      | Singleton (k, v) -> if f.Invoke (k, v) then pennant else empty
      | Merged(pennant1, pennant2) -> 
         merge kcomp vcomp (filterOpt kcomp vcomp f pennant1) (filterOpt kcomp vcomp f pennant2)


   // O(N). Applies the specified function to each entry in the queue, threading an accumulator argument through the
   // computation.
   let rec fold (kcomp: IComparer<'K>) (f: 'State -> 'K -> 'V -> 'State) state pennant  = 
      foldOpt kcomp (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt (f)) state pennant
   and foldOpt (kcomp: IComparer<'K>) (f:OptimizedClosures.FSharpFunc<_,_,_,_>) state pennant  = 
      match pennant with
      | Void -> state
      | Winner (k, v, ltree, _) -> 
         let state = f.Invoke (state, k, v)
         foldOptTree kcomp f state ltree 
   and private foldOptTree (kcomp: IComparer<'K>) (f:OptimizedClosures.FSharpFunc<_,_,_,_>) state tree =
      match tree with 
      | Lf -> state
      | Nd (k, v, left, splitKey, right, _) -> 
         let state = f.Invoke (state, k, v)
         foldOptTree kcomp f (foldOptTree kcomp f state left) right


   // O(N). Applies the specified function to each entry in the queue, threading an accumulator argument through the
   // computation. Entries are iterated in ascending order by key.
   let rec foldOrdered (kcomp: IComparer<'K>) (f: 'State -> 'K -> 'V -> 'State) state pennant  = 
      foldOrderedOpt kcomp (OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt (f)) state pennant
   and private foldOrderedOpt (kcomp: IComparer<'K>) (f:OptimizedClosures.FSharpFunc<_,_,_,_>) state pennant  = 
      match toTournamentView kcomp pennant with
      | Empty -> state
      | Singleton (k, v) -> f.Invoke(state, k, v)
      | Merged (pennant1, pennant2) -> 
         let state = foldOrderedOpt kcomp f state pennant1
         foldOrderedOpt kcomp f state pennant2

      

   // Iterator class for a pennant that iterates bindings in order of ascending keys. Complete iteration is O(N).
   type PennantEnumerator<'K, 'V when 'K: comparison and 'V: comparison> ( kcomp: IComparer<'K>, pennant : Pennant<'K, 'V> ) =
      let notStarted() = 
         raise <| new InvalidOperationException("The enumerator has not been started by a call to MoveNext")
      let alreadyCompleted() = 
         raise <| new InvalidOperationException("The enumerator has already completed.")

      // Always returns either [] or a list starting with Singleton. 
      // The intrinsic F# Map iterates using this technique.
      let rec collapseLHS pennantList =
         match pennantList with 
         | [] -> []
         | x::rest -> 
            match toTournamentView kcomp x with
            | Empty -> rest
            | Singleton (k, v) -> pennantList
            | Merged (pennant1, pennant2) -> 
               collapseLHS (pennant1 :: pennant2 :: rest)

      // The first element in this list is always the current value of the enumerator.
      let mutable pennantList  = collapseLHS [pennant]
      let mutable isStarted = false

      // Get the current item in the enumerator
      let current() =
         if isStarted then 
            match pennantList with
            | []  -> alreadyCompleted()
            | x::_ ->
               match toTournamentView kcomp x with
               | Singleton (k, v) -> new KeyValuePair<_,_>(k,v)
               | _ -> failwith "Unexpected error error iterating PrioritySearchQueue."
         else notStarted()
      
      // Positions the enumerator at the next item in the collection
      let moveNext() =
         if isStarted then 
            match pennantList with
            | [] -> false
            | x::rest ->
               match toTournamentView kcomp x with
               | Singleton (_, _) -> 
                  pennantList <- collapseLHS rest
                  not pennantList.IsEmpty
               | _ -> failwith "Unexpected error error iterating PrioritySearchQueue."
         else
             isStarted <- true;
             not pennantList.IsEmpty
 
      interface IEnumerator<KeyValuePair<'K, 'V>> with
         member x.Current = current()
      interface IEnumerator with 
         member x.Current = box (current())
         member x.MoveNext() = moveNext()
         member x.Reset() = pennantList <- collapseLHS [pennant]
      interface IDisposable with 
         member x.Dispose() = () 


   // // TODO: eval perf of this.
   // let rec anonEnumerator pennant = 
   //    match pennant with
   //    | Winner (k, v, ltree, _) -> 
   //       seq {
   //          yield struct(k,v)
   //          yield! anonTreeEnumerator ltree
   //       }
   //    | _ -> Seq.empty
   // and anonTreeEnumerator tree = 
   //    match tree with 
   //    | Lf -> Seq.empty
   //    | Nd (k, v, left, splitKey, right, _) -> 
   //        seq {
   //          yield struct(k,v)
   //          yield! anonTreeEnumerator left
   //          yield! anonTreeEnumerator right
   //       }