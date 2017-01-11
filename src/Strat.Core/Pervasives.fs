namespace Strat.Core

open System
open System.Collections.Generic
open System.Diagnostics


/// Describes a (X, Y) position on a 2-dimensional plane.
[<Struct>]
[<DebuggerDisplay("X = {X}, Y = {Y}")>]
type Point2D( x: float, y: float) = 
   member this.X = x
   member this.Y = y
  

/// Describes a (X, Y, Z) position in a 3-dimensional space.
[<Struct>]
[<DebuggerDisplay("X = {X}, Y = {Y}, Z = {Z}")>]
type Point3D( x: float, y: float, z: float) = 
   member this.X = x
   member this.Y = y
   member this.Z = y


/// Identfies an (x, y) location on a hexagonal grid.
[<Struct>]
[<DebuggerDisplay("X = {X}, Y = {Y}")>]
type Hex( x: int, y: int) = 
   member this.X = x
   member this.Y = y
   member this.Offset(x: int, y:int) = Hex(this.X + x, this.Y + y)
   member internal this.Q with get() = this.X
   member internal this.R with get() = this.Y
   override this.ToString() = 
      sprintf "X = %d, Y = %d" this.X this.Y
   static member internal OfRQ( r:int, q: int) = 
      Hex(q, r)
   


/// A type class for numeric operations 
type INumericOps<'T> =  
   abstract Zero: 'T
   abstract Compare : 'T -> 'T -> int
   abstract Add : 'T -> 'T -> 'T
   abstract Subtract : 'T -> 'T -> 'T
 

/// Provides implementations of INumericOps<T>.
module NumericOps = 
   /// Numeric operations for the Int32 type.
   let Int32 = { 
      new INumericOps<int> with
         member x.Zero = 0
         member x.Compare v1 v2 = if v1 < v2 then -1 else if v1 = v2 then 0 else 1
         member x.Add v1 v2 = v1 + v2
         member x.Subtract v1 v2 = v1 - v2
   }

   /// Numeric operations for the Int32 type.
   let Int64 = { 
      new INumericOps<int64> with
         member x.Zero = 0L
         member x.Compare v1 v2 = if v1 < v2 then -1 else if v1 = v2 then 0 else 1
         member x.Add v1 v2 = v1 + v2
         member x.Subtract v1 v2 = v1 - v2
   }

   /// Numeric operations for the Single floating point type
   let Single = { 
      new INumericOps<single> with
         member x.Zero = 0.0F
         member x.Compare v1 v2 = if v1 < v2 then -1 else if v1 = v2 then 0 else 1
         member x.Add v1 v2 = v1 + v2
         member x.Subtract v1 v2 = v1 - v2
   }

   /// Numeric operations for the Double floating point type
   let Double = { 
      new INumericOps<double> with
         member x.Zero = 0.0
         member x.Compare v1 v2 = if v1 < v2 then -1 else if v1 = v2 then 0 else 1
         member x.Add v1 v2 = v1 + v2
         member x.Subtract v1 v2 = v1 - v2
   }

   let private map = lazy(
      let opMap = new Dictionary<Type, obj>()
      [ typeof<int>, (Int32 :> obj)
        typeof<int64>, (Int64 :> obj)
        typeof<single>, (Single :> obj)
        typeof<double>, (Double :> obj); ]
      |> Seq.iter( fun (t, ops) -> opMap.Add( t, ops ) )
      opMap)

   /// Gets the numeric operations associated with the specified type.
   let Default<'T> = 
      let found, ops = map.Value.TryGetValue(typeof<'T>)
      if not(found) then invalidOp <| sprintf "Unable to locate numeric ops corresponding to type %A." typeof<'T> 
      ops :?> INumericOps<'T>


/// General-purpose utility functions
[<AutoOpen>]
module Pervasives = 
   
   /// Converts a tuple to a Hex instance.
   let inline toHex (x, y) = Hex(x, y)

   /// Comverts a sequence of tuples to a sequence of Hex instances.
   let inline toHexes tuples = tuples |> Seq.map toHex

   /// Returns a value indicating if the specified value is an even number.
   let inline even x = x % 2 = 0
   
   /// Returns a value indicating if the specified value is an odd number.
   let inline odd x = x % 2 = 1

   /// Returns a value indicating if the specified value is null.
   // This will be in the next release of F# (part of VS 2017)
   let inline isNotNull o = not (isNull o)

   /// Returns Even if the specified value is an even number, otherwise Odd.
   let (|Even|Odd|) value = 
      if even value then Even
      else Odd

   /// Returns the euclidian distance between the specified points
   let distance2D (pt1: Point2D) (pt2: Point2D) =
      sqrt( double(pt2.X - pt1.X) ** 2.0 + double(pt2.Y - pt1.Y) ** 2.0 )

   /// Converts a function accepting a tupled argument into a curried function.
   let inline curry2 f = fun x y -> f(x, y)

   /// Returns the result of applying the specified function n times to state.
   let applyN n f state = 
      [ 1..n ]
      |> List.fold( fun s i -> f s ) state

   /// Returns a new IComparer implementation that wraps the specified comparison function.
   let toComparer (comparison: 'T -> 'T -> int) = { 
      new IComparer<'T> with
         member x.Compare( v1, v2 ) =
            comparison v1 v2
      }


/// Functional programming operators for tuples with 2 items.
module Tuple2 = 

   /// Swaps the elements of the tuple.
   let inline swap (x, y) = (y, x)

   /// Applies the specified function to each element in the tuple
   let inline map f (x,y) = (f x, f y)

   /// Returns the n'th item from the specified tuple. Note that i is 0-based.
   let nth i (item0, item1) = 
      if i = 0 then item0
      elif i = 1 then item1
      else raise <| invalidArg "i" "i must be 0 or 1"

   /// Returns a new tuple by applying specified function to the first element in the tuple
   let inline mapFst f (item0, item1) = 
      (f item0), item1   

   /// Returns a new tuple by applying specified function to the second element in the tuple
   let inline mapSnd f (item0, item1) = 
      item0, (f item1)

   /// Returns a new tuple by applying the function to the n'th item in the specified tuple.  Note that i is 0-based.
   let mapNth i f (item0, item1) = 
      if i = 0 then (f item0, item1)
      elif i = 1 then (item0, f item1)
      else raise <| invalidArg "i" "i must be 0 or 1"


/// Functional programming operators for tuples with 3 items.
module Tuple3 = 

   /// Returns the first element of the tuple.
   let inline fst (x,y,z) = x

   /// Returns the second element of the tuple.
   let inline snd (x,y,z) = y

   /// Returns the third element of the tuple.
   let inline third (x,y,z) = z

   /// Applies the specified function to each element in the tuple
   let inline map f (x,y,z) = (f x, f y, f z)

   /// Returns the n'th item from the specified tuple. Note that i is 0-based.
   let nth i (item0, item1, item2) = 
      if i = 0 then item0
      elif i = 1 then item1
      elif i = 2 then item2
      else raise <| invalidArg "i" "i must be 0, 1 or 2"

   /// Returns a new tuple by applying the function to the n'th item in the specified tuple.  Note that i is 0-based.
   let mapNth i f (item0, item1, item2) = 
      if i = 0 then (f item0, item1, item2)
      elif i = 1 then (item0, f item1, item2)
      elif i = 2 then (item0, item1, f item2)
      else raise <| invalidArg "i" "i must be 0 or 1"


/// Functional programming operators for tuples with 4 items.
module Tuple4 = 

   /// Returns the first element of the tuple.
   let inline fst (w,x,y,z) = w

   /// Updates the first element of the tuple.
   let inline mapFst f (w,x,y,z) = (f w,x,y,z)

   /// Returns the second element of the tuple.
   let inline snd (w,x,y,z) = x

   /// Updates the second element of the tuple.
   let inline mapSnd f (w,x,y,z) = (w,f x,y,z)

   /// Returns the third element of the tuple.
   let inline third (w,x,y,z) = y

   /// Updates the third element of the tuple.
   let inline mapThird f (w,x,y,z) = (w,x,f y,z)

   /// Returns the fourth element of the tuple.
   let inline fourth (w,x,y,z) = z

   /// Updates the fourth element of the tuple.
   let inline mapFourth f (w,x,y,z) = (w,x,y,f z)

   /// Applies the specified function to each element in the tuple
   let inline map f (w,x,y,z) = (f w, f x, f y, f z)

   /// Returns the n'th item from the specified tuple. Note that i is 0-based.
   let nth i (item0, item1, item2, item3) = 
      if i = 0 then item0
      elif i = 1 then item1
      elif i = 2 then item2
      elif i = 3 then item3
      else raise <| invalidArg "i" "i must be 0, 1 or 2"

   /// Returns a new tuple by applying the function to the n'th item in the specified tuple.  Note that i is 0-based.
   let mapNth i f (item0, item1, item2, item3) = 
      if i = 0 then (f item0, item1, item2, item3)
      elif i = 1 then (item0, f item1, item2, item3)
      elif i = 2 then (item0, item1, f item2, item3)
      elif i = 3 then (item0, item1, item2, f item3)
      else raise <| invalidArg "i" "i must be 0 or 1"


