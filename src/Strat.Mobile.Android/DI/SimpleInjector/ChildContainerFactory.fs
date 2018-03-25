namespace DroidApp.Common.DI.SimpleInjector

open System.Collections.Generic
open SimpleInjector


type ChildContainerFactory<'TOwner when 'TOwner : not struct> (packagesByTypeName: Dictionary<string, IChildContainerPackage[]>) =
   interface IChildContainerFactory<'TOwner> with
      member this.CreateChildContainer (owner, parentContainer) =
         // Create a container to hold activity scoped services
         let childContainer = new Container()
         childContainer.Options.PropertySelectionBehavior <- new InjectAttributePropertySelectionBehavior()

         // Add a handler for unregistered types, so that service resolutions can be delegated to the root 
         // container if they are not registered with the activity container.
         childContainer.ResolveUnregisteredType.Add (fun args -> 
            let instanceProducer = parentContainer.GetRegistration(args.UnregisteredServiceType, false)
            if not (isNull instanceProducer) then
               args.Register instanceProducer.Registration)

         // Look up any packages that were registered for the activity with attributes.
         let success, packages = packagesByTypeName.TryGetValue(owner.GetType().FullName)
         let packages = if success then packages else Array.empty
        
         // Let each package register itself with the container
         packages 
         |> Array.iter (fun package -> package.RegisterServices(childContainer, parentContainer))
            
         childContainer