namespace Strat.Mobile.Android.DI.SimpleInjector

open System
open System.Linq.Expressions
open System.Reflection
open SimpleInjector
open Android.App
open System.Runtime.CompilerServices


// Attribute indicating that a property on an an activity/fragment is intended to recieve its value from the DI
// container. This is necessary because the Android OS often intantiates activities and fragments directly, and 
// so ctor injection can not be used.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple=false)>]
type InjectAttribute() = inherit Attribute()

/// Registers services with a container
type IPackage = 
   abstract RegisterServices: container:Container  -> unit


/// Attribute specifiying a package type that is available for registering services.  Typically decorates the 
/// Application class.
[<AttributeUsage(AttributeTargets.Class, AllowMultiple=false)>]
type ContainerPackageAttribute(packageType: Type) = 
   inherit Attribute()
   do if not(typeof<IPackage>.IsAssignableFrom(packageType)) then 
         invalidArg "packageType" (sprintf "%s must implement %s" packageType.FullName typeof<IPackage>.FullName)
   member this.PackageType = packageType


/// Registers services with a container. The container is associated with the specified parent container, so that any
/// services that cannot be resolved from the container are resolved from the parent container.
type IChildContainerPackage = 
   abstract RegisterServices: container:Container * parentContainer: Container -> unit


/// Attribute specifiying a package type that is available for registering services for an activity. These lifetime of
/// these services are intended to match the lifetime of the activity.
[<AttributeUsage(AttributeTargets.Class, AllowMultiple=true, Inherited=true)>]
type ActivityContainerPackageAttribute(activityPackageType: Type) = 
   inherit Attribute()
   do if not(typeof<IChildContainerPackage>.IsAssignableFrom(activityPackageType)) then 
         invalidArg "packageType" (sprintf "%s must implement %s" activityPackageType.FullName typeof<IChildContainerPackage>.FullName)
   member this.ActivityPackageType = activityPackageType

   static member CreatePackages (activityType: Type) = 
      activityType.GetCustomAttributes(typeof<ActivityContainerPackageAttribute>, true)
      |> Seq.map (fun oAttr -> 
         let packageAttr = oAttr :?> ActivityContainerPackageAttribute
         Activator.CreateInstance packageAttr.ActivityPackageType :?> IChildContainerPackage)
      |> Seq.toArray


/// Attribute specifiying a package type that is available for registering services for a fragment. These lifetime of
/// these services are intended to match the lifetime of the fragment.
[<AttributeUsage(AttributeTargets.Class, AllowMultiple=true, Inherited=true)>]
type FragmentContainerPackageAttribute(fragmentPackageType: Type) = 
   inherit Attribute()
   do if not(typeof<IChildContainerPackage>.IsAssignableFrom(fragmentPackageType)) then 
         invalidArg "packageType" (sprintf "%s must implement %s" fragmentPackageType.FullName typeof<IChildContainerPackage>.FullName)
   member this.FragmentPackageType = fragmentPackageType

   static member CreatePackages (fragmentType: Type) = 
      fragmentType.GetCustomAttributes(typeof<ActivityContainerPackageAttribute>, true)
      |> Seq.map (fun oAttr -> 
         let packageAttr = oAttr :?> FragmentContainerPackageAttribute
         Activator.CreateInstance packageAttr.FragmentPackageType :?> IChildContainerPackage)
      |> Seq.toArray


type IActivityContainerFactory = 
   abstract CreateActivityContainer: forOwner:Activity -> Container


type IFragmentContainerFactory = 
   abstract CreateFragmentContainer: forOwner:Fragment -> Container


type IChildContainerFactory<'TOwner when 'TOwner : not struct> = 
   abstract CreateChildContainer: forOwner:'TOwner * parentContainer: Container -> Container
  

/// Plugin for SimpleInject to allow the container to inject properties decorated with InjectAttribute.
type InjectAttributePropertySelectionBehavior() = 
   interface Advanced.IPropertySelectionBehavior with
      member this.SelectProperty(implementationType: Type, prop: PropertyInfo) =
         prop.GetCustomAttributes(typeof<InjectAttribute>) |> (Seq.isEmpty >> not)



[<Extension>]
type ContainerOptionsExtensions =
   /// Adds a handler for the ResolveUnregisteredType event that will automaticlly register Func<T> types with the 
   /// container.  When the Func is invoked, the underlying regstration with the container for the type T is used to
   /// produce the instance that is returned.
   [<Extension>]
   static member AllowResolvingFuncFactories (options: ContainerOptions) = 
      options.Container.ResolveUnregisteredType.Add (fun args -> 
         let t = args.UnregisteredServiceType
         if (not args.Handled) && t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Func<_>> then            
            let serviceType = t.GetGenericArguments() |> Seq.head
            let registration = options.Container.GetRegistration(serviceType, true)
            let funcType = typedefof<Func<_>>.MakeGenericType(serviceType)
            let factoryDelegate = Expression.Lambda(funcType, registration.BuildExpression()).Compile()
            args.Register (Expression.Constant factoryDelegate))

