namespace Strat.Mobile.Android.DI.SimpleInjector

open System
open System.Collections.Generic
open Android.App
open Android.Runtime
open SimpleInjector


[<AbstractClass>]
type SimpleInjectorApplication(javaRef: IntPtr, transfer: JniHandleOwnership) =
   inherit Application(javaRef, transfer)

   let mutable _container = Unchecked.defaultof<Container>
   let mutable _activityContainerFactory = Unchecked.defaultof<IChildContainerFactory<Activity>>
   let mutable _fragmentContainerFactory = Unchecked.defaultof<IChildContainerFactory<Fragment>>

   interface IActivityContainerFactory with
      member this.CreateActivityContainer owner =
         _activityContainerFactory.CreateChildContainer (owner, _container)

   override this.OnCreate() =
      base.OnCreate()

      // Initialize the DI container for the app.
      let container, activityPackagesByTypeName, fragmentPackagesByTypeName = this.CreateContainer();
      _container <- container
      _activityContainerFactory <- new ChildContainerFactory<Activity>(activityPackagesByTypeName)
      _fragmentContainerFactory <- new ChildContainerFactory<Fragment>(fragmentPackagesByTypeName)
      this.RegisterServices _container

      // Inject services into this instance, in case subclass has [<Inject>]ions.
      _container.RegisterSingleton(this.GetType(), fun() -> this :> obj)
      let reg = container.GetRegistration(this.GetType(), true).Registration
      reg.InitializeInstance(this)


   member this.Container = 
      _container

   /// Subclasses can override to add services to the container.  The subclass *must* invoke the base implementation.
   abstract RegisterServices: container: Container -> unit
   default this.RegisterServices container =
      container.RegisterInstance<Application>(this)
      container.RegisterInstance<IChildContainerFactory<Fragment>>(_fragmentContainerFactory)
 

   member private this.CreateContainer() = 
      let container = new Container()

      // Add a behavior to the container that enables property injection
      container.Options.PropertySelectionBehavior <- new InjectAttributePropertySelectionBehavior()
      
      // Let any packages decorating the application register services with the container
      this.GetType().GetCustomAttributes(typeof<ContainerPackageAttribute>, true)
      |> Array.iter (fun oAttr -> 
         let packageAttr = oAttr :?> ContainerPackageAttribute
         let package = Activator.CreateInstance packageAttr.PackageType :?> IPackage
         package.RegisterServices container)

      // Build a map containing packages decorating each activity/frag type, keyed by type name
      // TODO: conderider how to broaden list of assemblies to search
      let activityPackagesByTypeName = Dictionary<string, IChildContainerPackage[]>()
      let fragmentPackagesByTypeName = Dictionary<string, IChildContainerPackage[]>()
      this.GetType().Assembly.ExportedTypes 
      |> Seq.filter (fun t -> typeof<Activity>.IsAssignableFrom t || typeof<Fragment>.IsAssignableFrom t)
      |> Seq.iter (fun  t -> 
         if typeof<Activity>.IsAssignableFrom t then
            let packages = ActivityContainerPackageAttribute.CreatePackages t
            activityPackagesByTypeName.Add (t.Name, packages)
         else
            let packages = FragmentContainerPackageAttribute.CreatePackages t
            fragmentPackagesByTypeName.Add (t.FullName, packages))
                                                                                                               
      container, activityPackagesByTypeName, fragmentPackagesByTypeName



