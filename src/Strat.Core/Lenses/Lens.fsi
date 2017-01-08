namespace Strat.Core.Lenses

open System
open System.Threading


/// Functions for creating and using lenses, which may be thought of as 'functional properties'. A lens represents a
/// reference to a storage location in an immutable type, and a programmer can use the lens both read and write values
/// to that storage location in a functional manner. Lenses can easily be composed, making it easy to update deeply 
/// nested immutable data structures.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lens = 

   /// A lens, which can be used to get or set values of type 'TField on values of type 'T
   type Lens<'T,'TField>


   /// A partial lens, which can be used to get or set values of type 'TField on values of type 'T. The lens is partial,
   /// because there may not be a current field value, and therefore reading from the lens returns an option.
   type PartialLens<'T,'TField> 


   /// Creates a lens from the specified get and set functions
   val fromGetSet: get:('T -> 'TField) -> set:('TField -> 'T -> 'T) -> Lens<'T,'TField>


   /// Creates a partial lens from the specified get and set functions
   val fromGetSetPartial: get:('T -> option<'TField>) -> set:('TField -> 'T -> 'T) -> PartialLens<'T,'TField>


   /// Gets the value for the specified lens from the specified object.
   val get: lens:Lens<'T,'TField> -> from:'T -> 'TField 

   
   /// Gets the value for the specified partial lens from the specified object.
   val getPartial: lens:PartialLens<'T,'TField> -> from:'T -> option<'TField>


   /// Returns a new object by setting the specified value using the specified lens.
   val set: lens:Lens<'T,'TField> -> value:'TField -> into:'T -> 'T


   /// Returns a new object by setting the specified value using the specified partial lens.
   val setPartial: lens:PartialLens<'T,'TField> -> value:'TField -> into:'T -> 'T


   /// Returns a new object by applying the specified function to the value of the specified lens.
   val update: lens:Lens<'T,'TField> -> fUpdate:('TField -> 'TField) -> into:'T -> 'T


   /// Returns a new object by applying the specified function to the value of the specified partial lens.
   val updatePartial: lens:PartialLens<'T,'TField> -> fUpdate:('TField -> 'TField) -> into:'T -> 'T


   /// Gets the value for the specified lens from the specified object. A symbolic shortcut for the get function.
   val (|->) : from:'T -> lens:Lens<'T,'TField> -> 'TField


   /// Gets the value for the specified partial lens from the specified object. A symbolic shortcut for the getPartial
   /// function.
   val (|?>) : from:'T -> lens:PartialLens<'T,'TField> -> option<'TField>


   /// Returns a new object by setting the specified value using the specified lens. A symbolic shortcut for the set 
   /// function.
   val (<-|): lens:Lens<'T,'TField> -> value:'TField -> ('T -> 'T)


   /// Returns a new object by setting the specified value using the specified partial lens. A symbolic shortcut for
   /// the setPartial function.
   val (<?|): lens:PartialLens<'T,'TField> -> value:'TField -> ('T -> 'T)


   /// Returns a new lens that composes two lenses.
   val (-->) : lens1:Lens<'A,'AField> -> lens2:Lens<'AField,'BField> -> Lens<'A,'BField>


   /// Returns a new partial lens that composes a lens and a partial lens.
   val (-?>) : lens1:Lens<'A,'AField> -> lens2:PartialLens<'AField,'BField> -> PartialLens<'A,'BField>


   /// Returns a new partial lens that composes a partial lens and a lens.
   val (?->) : lens1:PartialLens<'A,'AField> -> lens2:Lens<'AField,'BField> -> PartialLens<'A,'BField>


   /// Returns a new partial lens that composes two partial lenses.
   val (??>) : lens1:PartialLens<'A,'AField> -> lens2:PartialLens<'AField,'BField> -> PartialLens<'A,'BField>