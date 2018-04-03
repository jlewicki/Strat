namespace Strat.Mobile.Android.UI

open System
open System.Reactive.Disposables
open System.Reactive.Linq
open System.Reactive.Subjects
open Android.Views
open Android.Support.V7.Widget
open Strat.Mobile.Android.DI.SimpleInjector.AppCompat
open Strat.UI


type MenuText = | Text of string | ResourceId of int
type MenuItem = Item of MenuItemId:int * Text:MenuText * IconResId: option<int> 
type MenuItems = | Items of list<MenuItem> | ResourceId of int

type ResourceOr<'T> = 
   | Value of 'T
   | ResourceId of int


[<AbstractClass>]
//[<ActivityContainerPackage(typeof<HuntersActivityPackage>)>]
type ViewModelAppCompatActivity<'ViewModel> () =
   inherit SimpleInjectorAppCompatActivity()

   let mutable _viewModel = Unchecked.defaultof<'ViewModel>
   let mutable _actionBarMenuItems: option<ResourceOr<list<MenuItem>>> = None
   let mutable _bindingDisposable = Disposable.Empty
   let _optionsItemSelectedSubject = new Subject<int>()

   interface IView<'ViewModel> with 
      // TODO: Should we be providing an Observable<'ViewModel> instead?
      member this.SetViewModel vm = 
         // Dispose of any subscriptions the subclass has made to avoid memory leaks
         _bindingDisposable.Dispose()
         _viewModel <- vm 
         // Bind to the new view model, capturing any subscriptions made by the subclass
         _bindingDisposable <- new CompositeDisposable(this.BindViewModel(vm))


   /// Called when this activity should data bind to the specified view model. Subclasses should return a sequence of
   /// disposables representing subscriptions to events from the view model, or views loaded by the activity. These
   /// subscriptions will be destroyed before this method is called again with a new view model. 
   abstract BindViewModel: viewModel: 'ViewModel -> seq<IDisposable>


   /// Called when the options menu is being populated. By default no menu items will be added, but subclases can 
   /// override to provide the menu items specific to the activity.
   abstract GetOptionsMenuItems: menu:IMenu -> option<MenuItems>
   default this.GetOptionsMenuItems menu = None


   /// Returns an observable that yields each time the user selects the menu intem with the specfied ID.
   member this.OptionItemSelected (menuItemId: int) : IObservable<unit> = 
      _optionsItemSelectedSubject.Where( ((=) menuItemId) ).Select( ignore )


   /// Initializes the action bar for this actiivity. Activities that call this method must have a Toolbar defined in
   /// their layouts 
   member this.InitAppActionBar 
      ( ?title: ResourceOr<string>, 
        ?showBackButton: bool, 
        ?toolbar: ResourceOr<Toolbar>, 
        ?optionsMenu: ResourceOr<list<MenuItem>> ) =
      
      let tb = 
         match toolbar with
         | Some (ResourceId resId) ->
            let tb = this.FindViewById<Toolbar> resId
            if isNull tb then invalidOp "Cannot find toolbar"
            tb
         | Some (Value tb) -> tb
         | None -> new Toolbar(this)
      this.SetSupportActionBar tb

      let abTitle = 
         match title with
         | Some (ResourceId resId) ->  this.GetString resId 
         | Some (Value title) ->  title
         | None -> ""
      this.SupportActionBar.Title <- abTitle
      
      _actionBarMenuItems <- optionsMenu
      if defaultArg showBackButton false then
         this.SupportActionBar.SetDisplayHomeAsUpEnabled true


   /// Adds the menu items returned by the subclass to the action bar
   override this.OnCreateOptionsMenu menu =
      match _actionBarMenuItems with
      | Some (ResourceId resId) ->
         this.MenuInflater.Inflate (resId, menu)
         true
      | Some (Value menuItems) ->
         menuItems 
         |> Seq.iter (fun (MenuItem.Item(id, text, optIconResId)) -> 
            let group = 1
            let menuItem = 
               match text with
               | MenuText.Text text -> menu.Add (group, id, Menu.None, text)
               | MenuText.ResourceId resId -> menu.Add (group, id, Menu.None, resId)
            menuItem.SetShowAsAction ShowAsAction.Always)
         true
      | None -> false
      

   /// Yields values from the OptionsItemSelected observable as items are selected by the user.
   override this.OnOptionsItemSelected menuItem = 
      if menuItem.ItemId = Android.Resource.Id.Home then 
         base.OnOptionsItemSelected menuItem
      else
         _optionsItemSelectedSubject.OnNext menuItem.ItemId
         true