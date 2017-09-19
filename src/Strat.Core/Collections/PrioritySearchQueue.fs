namespace Strat.Collections

open System
open System.Collections
open System.Collections.Generic
open Strat.Collections.Primitives
open Strat.Collections.Primitives.Pennant


[<Sealed>]
type PrioritySearchQueue<[<EqualityConditionalOn>]'K, [<EqualityConditionalOn>]'V when 'K: comparison and 'V: comparison>
   ( kcomp: IComparer<'K>,
     vcomp: IComparer<'V>, 
     pennant: Pennant<'K, 'V>) =

   static let empty = 
      let kcomp = LanguagePrimitives.FastGenericComparer<'K> 
      let vcomp = LanguagePrimitives.FastGenericComparer<'V> 
      new PrioritySearchQueue<'K, 'V> (kcomp, vcomp, Pennant.empty)


   member this.KeyComparer = 
      kcomp

   member this.ValueComparer = 
      vcomp

   static member Empty : PrioritySearchQueue<'K, 'V> = 
      empty


   new (unorderedItems: seq<'K*'V>) = 
      let kcomp = LanguagePrimitives.FastGenericComparer<'K> 
      let vcomp = LanguagePrimitives.FastGenericComparer<'V> 
      new PrioritySearchQueue<'K,'V> (kcomp, vcomp, ofSeq kcomp vcomp false unorderedItems)


   member this.Count : int = 
      length pennant


   member this.IsEmpty : bool = 
      this.Count = 0


   member this.Head = 
      minBinding pennant


   member this.TryHead = 
      peekMinBinding pennant


   member this.Item 
      with get (key: 'K) = 
         find kcomp key pennant


   member this.TryFind key = 
      tryFind kcomp key pennant 


   member this.ContainsKey key = 
      tryFind kcomp key pennant |> Option.isSome


   member this.Add (key, value) =
      new PrioritySearchQueue<'K, 'V> (kcomp, vcomp, insert kcomp vcomp key value pennant)


   member this.Remove key =
      match delete kcomp vcomp key pennant with
      | struct(true, rest) ->  new PrioritySearchQueue<'K, 'V> (kcomp, vcomp, rest)
      | _ -> this


   member this.RemoveHead =
      match tryRemoveMin kcomp vcomp pennant with
      | Some (k,v,rest) -> struct (k, v, new PrioritySearchQueue<'K, 'V> (kcomp, vcomp, rest))
      | None -> raise <| invalidOp "Collection is empty" 


   member this.TryRemoveHead =
      match tryRemoveMin kcomp vcomp pennant with
      | Some (k,v,rest) -> Some (struct (k, v, new PrioritySearchQueue<'K, 'V> (kcomp, vcomp, rest)))
      | None -> None


   member this.AtMost value = 
      atMost kcomp value pennant


   override this.Equals(other: obj) =
      if Object.ReferenceEquals(this, obj) then true
      else
        match other with 
        | :? PrioritySearchQueue<'K,'V> as otherQ ->
            use e1 = (this :> seq<_>).GetEnumerator() 
            use e2 = (otherQ :> seq<_>).GetEnumerator() 
            let mutable eq = false
            let mutable cont = true
            while cont do
               let hasNext1 = e1.MoveNext() 
               let hasNext2 = e2.MoveNext()
               if hasNext1 && hasNext2 then
                  let kvp1 = e1.Current
                  let kvp2 = e2.Current
                  eq <- kvp1.Key = kvp2.Key && Unchecked.equals kvp1.Value kvp2.Value
                  //eq <- kvp1.Key = kvp2.Key && Unchecked.equals kvp1.Value kvp2.Value
                  cont <- eq
               elif (not hasNext1) && (not hasNext2) then 
                  cont <- false
               else 
                  eq <- false
                  cont <- false
            eq
        | _ -> false


   override this.GetHashCode() =
      // This basically works this same way as FSharpMap<'K,'V>.ComputeHashCode
      let combineHash x y = (x <<< 1) + y + 631 
      let mutable res = 0
      iter (fun k v ->
         res <- (combineHash res (hash k))
         res <- combineHash res (Unchecked.hash v)) pennant
      abs res


   interface IEnumerable with
      member x.GetEnumerator() = 
         new PennantEnumerator<'K, 'V> (kcomp, pennant) :> IEnumerator


   interface IEnumerable<KeyValuePair<'K, 'V>> with
      member x.GetEnumerator() = 
         new PennantEnumerator<'K, 'V> (kcomp, pennant) :> IEnumerator<KeyValuePair<'K, 'V>>


   interface IReadOnlyCollection<KeyValuePair<'K, 'V>> with
      member this.Count = 
         this.Count

   interface ICollection<KeyValuePair<'K, 'V>> with 
      member this.Add kvp = raise <| NotSupportedException()
      member this.Clear() =  raise <| NotSupportedException()
      member this.Remove kvp =  raise <| NotSupportedException()
      member this.Contains kvp = this.ContainsKey kvp.Key && Unchecked.equals this.[kvp.Key] kvp.Value
      member this.CopyTo (arr,i) =
         let mutable nextIdx = i
         pennant |> iter (fun k v -> arr.[nextIdx] <- KeyValuePair<'K,'V>(k,v); nextIdx <- nextIdx + 1)  
      member this.IsReadOnly = true
      member this.Count = this.Count

   interface IDictionary<'K, 'V> with 
      member this.Item 
          with get k = this.[k]            
          and  set x v = raise <| NotSupportedException()
      member this.Keys = ([| for kvp in this -> kvp.Key |] :> ICollection<'K>)
      member this.Values = ([| for kvp in this -> kvp.Value |] :> ICollection<'V>)
      member this.Add(k,v) = raise <| NotSupportedException()
      member this.ContainsKey k = this.ContainsKey k
      member this.TryGetValue (k, r) =
         if this.ContainsKey k then (r <- this.[k]; true) 
         else false
      member this.Remove (ky) = raise <| NotSupportedException()


   interface IPennantSource<'K, 'V> with
      member this.Pennant = 
         pennant


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PrioritySearchQueue = 

   [<GeneralizableValue>]
   [<CompiledName("Empty")>]
   let empty<'K, 'V when 'K: comparison and 'V: comparison> : PrioritySearchQueue<'K, 'V>= 
      PrioritySearchQueue<'K, 'V>.Empty


   [<CompiledName("OfSeq")>]
   let ofSeq (items: seq<'K*'V>) : PrioritySearchQueue<'K, 'V> = 
      new PrioritySearchQueue<_, _> (items)


   [<CompiledName("OfOrderedSeq")>]
   let ofOrderedSeq (items: seq<'K*'V>) : PrioritySearchQueue<'K, 'V> = 
      let kcomp = LanguagePrimitives.FastGenericComparer<'K> 
      let vcomp = LanguagePrimitives.FastGenericComparer<'V> 
      new PrioritySearchQueue<_, _> (kcomp, vcomp, Pennant.ofSeq kcomp vcomp true items)


   [<CompiledName("OfArray")>]
   let ofArray (items: ('K*'V)[]) : PrioritySearchQueue<'K, 'V> = 
      new PrioritySearchQueue<_, _> (items)


   [<CompiledName("Singleton")>]
   let singleton key value : PrioritySearchQueue<'K, 'V> = 
      let kcomp = LanguagePrimitives.FastGenericComparer<'K> 
      let vcomp = LanguagePrimitives.FastGenericComparer<'V> 
      new PrioritySearchQueue<_, _> (kcomp, vcomp, singleton key value)


   [<CompiledName("IsEmpty")>]
   let isEmpty (queue: PrioritySearchQueue<'K, 'V>) : bool = 
      queue.IsEmpty


   [<CompiledName("Length")>]
   let length (queue:PrioritySearchQueue<'K, 'V>) : int = 
      queue.Count


   [<CompiledName("Head")>]
   let head (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.Head


   [<CompiledName("TryHead")>]
   let tryHead (queue:PrioritySearchQueue<'K, 'V>) : option< struct ('K * 'V) >= 
      queue.TryHead


   [<CompiledName("Find")>]
   let find key (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.[key]


   [<CompiledName("TryFind")>]
   let tryFind key (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.TryFind key


   [<CompiledName("ContainsKey")>]
   let containsKey key (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.ContainsKey key


   [<CompiledName("Add")>]
   let add key value (queue:PrioritySearchQueue<'K, 'V>) =
      queue.Add (key, value)


   [<CompiledName("RemoveHead")>]
   let removeHead (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.RemoveHead


   [<CompiledName("TryRemoveHead")>]
   let tryRemoveHead (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.TryRemoveHead


   [<CompiledName("Remove")>]
   let remove (key: 'K) (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.Remove key


   [<CompiledName("AtMost")>]
   let atMost (value:'V) (queue: PrioritySearchQueue<'K, 'V>) : list<'K*'V> =
      queue.AtMost value


   [<CompiledName("Iter")>]
   let iter (f: 'K -> 'V -> unit) (queue: PrioritySearchQueue<'K, 'V>) =
      iter f (queue :> IPennantSource<_,_>).Pennant


   [<CompiledName("Map")>]
   let map (f: 'K -> 'V -> 'U) (queue: PrioritySearchQueue<'K, 'V>) : PrioritySearchQueue<'K, 'U> =
      let vcomp = LanguagePrimitives.FastGenericComparer<'U> 
      let pennant = map queue.KeyComparer vcomp f (queue :> IPennantSource<'K,'V>).Pennant
      new PrioritySearchQueue<_, _> (queue.KeyComparer, vcomp, pennant)


   [<CompiledName("Filter")>]
   let filter (predicate:'K -> 'V -> bool) (queue:PrioritySearchQueue<'K, 'V>) : PrioritySearchQueue<'K, 'V> =
      let pennant = filter queue.KeyComparer queue.ValueComparer predicate (queue :> IPennantSource<'K,'V>).Pennant
      new PrioritySearchQueue<_, _> (queue.KeyComparer, queue.ValueComparer, pennant)
      

   [<CompiledName("Fold")>]
   let fold (f:'State -> 'K -> 'V -> 'State) (initial: 'State) (queue: PrioritySearchQueue<'K, 'V>) : 'State =
      fold (queue.KeyComparer) f initial (queue :> IPennantSource<_,_>).Pennant


   [<CompiledName("Exists")>]
   let exists (predicate:'K -> 'V -> bool) (queue:PrioritySearchQueue<'K, 'V>) : bool =
      exists predicate (queue :> IPennantSource<'K,'V>).Pennant 


   [<CompiledName("ForAll")>]
   let forAll (predicate:'K -> 'V -> bool) (queue:PrioritySearchQueue<'K, 'V>) : bool =
     forAll predicate (queue :> IPennantSource<'K,'V>).Pennant 


   [<CompiledName("TryPick")>]
   let tryPick (chooser:'K -> 'V -> 'U option) (queue:PrioritySearchQueue<'K, 'V>) : 'U option =
      tryPick chooser (queue :> IPennantSource<'K,'V>).Pennant 


   [<CompiledName("Pick")>]
   let pick (chooser:'K -> 'V -> 'U option) (queue:PrioritySearchQueue<'K, 'V>) : 'U = 
      match tryPick chooser queue with
      | Some u -> u
      | None -> raise (KeyNotFoundException())


   [<CompiledName("ToSeq")>]
   let toSeq (queue:PrioritySearchQueue<'K, 'V>) = 
      queue :> IEnumerable<KeyValuePair<'K,'V>>
      |> Seq.map (fun kvp -> kvp.Key, kvp.Value)


   [<CompiledName("ToList")>]
   let toList (queue: PrioritySearchQueue<'K, 'V>) : list<'K * 'V> =
      queue |> toSeq |> List.ofSeq


   [<CompiledName("ToArray")>]
   let toArray (queue: PrioritySearchQueue<'K, 'V>) : struct('K * 'V)[] =
      let arr = Array.zeroCreate queue.Count
      (queue :> IEnumerable<KeyValuePair<'K,'V>>)
      |> Seq.iteri (fun idx kvp -> 
         arr.[idx] <- struct (kvp.Key, kvp.Value)) 
      arr
      



