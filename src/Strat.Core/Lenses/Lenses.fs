namespace Strat.Core.Lenses

open Strat.Core.Lenses.Lens

/// Lenses for Map<_,_>
module Map = 

   /// Returns a partial lens for a map entry with the specified key.
   let tryKeyL key = 
      fromGetSetPartial 
         (fun (m:Map<_,_>) -> m |> Map.tryFind key ) 
         (fun v (m:Map<_,_>) -> m |> Map.add key v )


   /// Returns a lens for a map entry with the specified key. Accessing the lens will throw an exception if the key
   /// does not exist.
   let keyL key = 
      fromGetSet 
         (fun (m:Map<_,_>) -> m |> Map.find key ) 
         (fun v (m:Map<_,_>) -> m |> Map.add key v )




