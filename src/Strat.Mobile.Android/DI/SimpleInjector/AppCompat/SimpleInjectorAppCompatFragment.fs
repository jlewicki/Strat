namespace Strat.Mobile.Android.DI.SimpleInjector.AppCompat

open Android.Support.V4.App
open SimpleInjector
open Strat.Mobile.Android.DI.SimpleInjector

type SimpleInjectorFragment () =
   inherit Fragment ()

   let mutable _fragmentContainer = Unchecked.defaultof<Container>

   override this.OnActivityCreated (savedInstanceState) = 
      base.OnActivityCreated savedInstanceState

      // Create a container for activity scoped service
      _fragmentContainer <- (this.Activity :> obj :?> IFragmentContainerFactory).CreateFragmentContainer (this)
      this.RegisterServices _fragmentContainer

      // Inject services into this instance.  
      let reg = _fragmentContainer.GetRegistration(this.GetType(), true).Registration
      reg.InitializeInstance(this)
   
   abstract RegisterServices: container: Container -> unit
   default this.RegisterServices container = 
      // Register the fragment type with the container. It will never be resolved, but it allows a 
      // registration to be obtained for the type from the container, which allows injecting into a 
      // preexisting instance that has not been created by the container. This is important, since Android
      // creates activity instances, not the container.
      container.RegisterInstance(this.GetType(), this)

      // Register the fragment
      container.RegisterInstance<Fragment> (this)

