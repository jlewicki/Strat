namespace Strat.Mobile.Android.DI.SimpleInjector.AppCompat

open Android.App
open SimpleInjector
open Strat.Mobile.Android.DI.SimpleInjector
open Android.Support.V4.App
open Android.Support.V7.App


/// Base class for activities that support injection of dependencies.
/// 
/// If a subclass has writable properties decorated with [<Inject>], then those fields will be injected with
/// instances from the DI container during the OnCreate method.
type SimpleInjectorAppCompatActivity() = 
   inherit AppCompatActivity() 

   let mutable _activityContainer = Unchecked.defaultof<Container>
  
   [<Inject>]
   member val FragmentContainerFactory = Unchecked.defaultof<IChildContainerFactory<Fragment>> with get, set
   
   interface IFragmentContainerFactory with
      member this.CreateFragmentContainer owner =
         this.FragmentContainerFactory.CreateChildContainer (owner, _activityContainer)
   
   override this.OnCreate (savedInstanceState) =
      base.OnCreate (savedInstanceState)

      // Create a container for activity scoped service
      _activityContainer <- (this.Application :> obj :?> IActivityContainerFactory).CreateActivityContainer (this)
      this.RegisterServices _activityContainer

      // Inject services into this instance.  
      let reg = _activityContainer.GetRegistration(this.GetType(), true).Registration
      reg.InitializeInstance(this)

   abstract RegisterServices: container: Container -> unit
   default this.RegisterServices container = 
      // Register the activity type with the container. It will never be resolved, but it allows a 
      // registration to be obtained for the type from the container, which allows injecting into a 
      // preexisting instance that has not been created by the container. This is important, since Android
      // creates activity instances, not the container.
      container.RegisterInstance(this.GetType(), this)

      // Register the activity
      let t = this.GetType().FullName
      container.RegisterInstance<Activity> (this)