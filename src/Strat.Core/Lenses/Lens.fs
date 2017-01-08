namespace Strat.Core.Lenses


/// Functions for creating and using lens
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lens =
   
   type Store<'T, 'TField> = ('TField -> 'T) * 'TField
   type Lens<'T,'TField> = Lens of ('T -> Store<'T,'TField>)
   type PartialStore<'T, 'TField> = ('TField -> 'T) * option<'TField>
   type PartialLens<'T,'TField> = PLens of ('T -> PartialStore<'T,'TField>)


   let fromGetSet (get: 'T -> 'TField) (set: 'TField -> 'T -> 'T) : Lens<'T,'TField> = 
      Lens(fun t -> (fun value -> set value t), (get t))


   let fromGetSetPartial (get: 'T -> option<'TField>) (set: 'TField -> 'T -> 'T) : PartialLens<'T,'TField> = 
      PLens(fun t -> (fun value -> set value t), (get t))


   let get (Lens(lens)) from : 'TField = 
      snd (lens from)


   let (|->) from lens = 
      get lens from

   
   let set (Lens(lens)) value into = 
      fst (lens into) value


   let update (Lens(lens)) fUpdate into = 
      let set, cur = lens into
      set (fUpdate cur)


   let getPartial (PLens(plens)) from : option<'TField> =
      snd (plens from)


   let setPartial (PLens(plens)) value into = 
      fst (plens into) value


   let (<-|) lens value  =
      set lens value 
 

   let updatePartial (PLens(plens)) fUpdate into = 
      match plens into with
      | set, Some(current) -> set (fUpdate current)
      | _, None -> into


   let  (|?>) from lens = 
      getPartial lens from


   let (<?|) lens value  =
      setPartial lens value 


   let (-->) (Lens(lens1)) (Lens(lens2)) = 
      Lens(fun t ->
         let set1, c1 = lens1 t
         let set2, c2  = lens2 c1
         let set = fun v -> set1 (set2 v)
         set, c2 )
 

   let (??>) (PLens(plens1)) (PLens(plens2)) =
      PLens(fun t ->
         match plens1 t with
         | set1, Some(c1) ->
            let set2, c2  = plens2 c1
            let set = fun v -> set1 (set2 v)
            set, c2 
         | set1, None ->
           (fun v -> t), None)


   let (?->) (PLens(plens1)) (Lens(lens2)) =
      PLens(fun t ->
         match plens1 t with
         | set1, Some(c1) ->
            let set2, c2  = lens2 c1
            let set = fun v -> set1 (set2 v)
            set, (Some(c2)) 
         | set1, None ->
           (fun v -> t), None)


   let (-?>) (Lens(lens1)) (PLens(plens2)) =
      PLens(fun t ->
         let set1, c1 = lens1 t
         let set2, c2 = plens2 c1
         let set = fun v -> set1 (set2 v)
         set, c2 )