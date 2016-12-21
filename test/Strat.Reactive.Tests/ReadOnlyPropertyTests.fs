namespace Strat.Reactive.Tests

open System
open System.ComponentModel
open System.Collections.Generic
open System.Reactive
open System.Reactive.Linq
open System.Reactive.Subjects
open System.Reactive.Concurrency
open Strat.Reactive
open Xunit


module ReadOnlyProperty =

    module Ctor = 
        [<Fact>]
        let ``Should Throw For Null futureValues``() = 
            Assert.Throws<ArgumentNullException>( fun () -> 
                new ReadOnlyProperty<int> (34, null) |> ignore )

        [<Fact>]
        let ``Should Throw For Null comparer``() = 
            Assert.Throws<ArgumentNullException>( fun () -> 
                new ReadOnlyProperty<int> (34, Observable.Never(), null) |> ignore )

    module Value = 
        [<Fact>]
        let ``Should Return Initial Value Before futureValues Yields``() = 
            let prop = new ReadOnlyProperty<int> (3, Observable.Never())
            Assert.Equal(3, prop.Value)

        [<Fact>]
        let ``Should Return Last Value Yielded``() = 
            let prop = new ReadOnlyProperty<int> (3, Observable.Range(1, 6) )
            Assert.Equal(6, prop.Value)

        [<Fact>]
        let ``Should Rethrow Exceptions Observed In futureValues``() = 
            let prop = new ReadOnlyProperty<int> (3, Observable.Throw (InvalidOperationException()))
            Assert.Throws<InvalidOperationException>( fun () -> 
                prop.Value |> ignore)

    module PropertyChanged = 
        [<Fact>]
        let ``Should Be Raised When futureValues Yields New Value``() =
            use values = new Subject<int>()
            let nextValue = 5
            let prop = new ReadOnlyProperty<int>(4, values)
            let mutable eventRaised = false
            prop.PropertyChanged.AddHandler (fun sender args -> 
                Assert.Same (prop, sender)
                Assert.Equal ("Value", args.PropertyName)
                Assert.Equal (nextValue, prop.Value)
                eventRaised <- true)
            values.OnNext nextValue
            Assert.True eventRaised

        [<Fact>]
        let ``Should Not Be Raised When futureValues Yields Same Value``() =
            use values = new Subject<int>()
            let nextValue = 4
            let prop = new ReadOnlyProperty<int>(4, values)
            let mutable eventRaised = false
            prop.PropertyChanged.AddHandler (fun sender args -> 
                eventRaised <- true)
            values.OnNext nextValue
            Assert.False eventRaised

        [<Fact>]
        let ``Should Use comparer When Checking If Values Are Same``() = 
            let comparer = 
                // Silly comparer that consider a number and its negativr to be the same
                { new IEqualityComparer<int> with
                    member __.Equals(v1, v2) = v1 = -v2
                    member __.GetHashCode v = v.GetHashCode() }
            use values = new Subject<int>()
            let prop = new ReadOnlyProperty<int> (4, values, comparer)
            let mutable eventRaised = false
            prop.PropertyChanged.AddHandler (fun sender args -> 
                eventRaised <- true)
            values.OnNext -4
            Assert.Equal (4, prop.Value)
            Assert.False eventRaised


    module Subscribe =
        [<Fact>]
        let ``Should Synchronously Yield Current Value To Subscriber``() =
            let prop = new ReadOnlyProperty<int> (4, Observable.Never()) :> IObservable<int>
            let mutable yieldedValue = None
            prop.Subscribe (fun v -> yieldedValue <- Some(v)) |> ignore
            Assert.True yieldedValue.IsSome
            Assert.Equal (4, yieldedValue.Value)


        [<Fact>]
        let ``Should Yield Future Values To Subscriber``() = 
            use values = new Subject<int>()
            let prop = new ReadOnlyProperty<int> (4, values)
            let nextValue = 5
            let mutable onNextCalled = false   
            // Skip 1 because current value is always provided to observers when subscribing   
            prop.Skip(1).Subscribe (fun next ->
                Assert.Equal (nextValue, next)
                onNextCalled <- true) 
            |> ignore
            values.OnNext nextValue
            Assert.True onNextCalled 

        [<Fact>]
        let ``Should Not Cause Multiple Subscriptions To futureValues``() = 
            let mutable equalsCount = 0
            let comparer = 
                { new IEqualityComparer<int> with
                    member __.Equals(v1, v2) =
                         equalsCount <- equalsCount + 1
                         v1 = v2
                    member __.GetHashCode v = v.GetHashCode() }
            let prop = new ReadOnlyProperty<int> (4, Observable.Return 5, comparer)
            // Subscribe twice, but our comparer should only evebtr be called once per yielded value
            prop.Subscribe ignore |> ignore
            prop.Subscribe ignore |> ignore
            Assert.Equal (1, equalsCount)
        
        [<Fact>]
        let ``Should Propagate Errors Observed In futureValues``() = 
            let errorToThrow = InvalidOperationException()
            let prop = new ReadOnlyProperty<int> (3, Observable.Throw errorToThrow)
            let mutable error: exn = null
            prop.Subscribe( 
                (fun v -> Assert.True(false, "Should not be called")),
                (fun (ex: exn) -> error <- ex))  |> ignore
            Assert.Same (errorToThrow, error)
