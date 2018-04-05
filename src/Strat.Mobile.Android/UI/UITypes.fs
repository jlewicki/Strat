namespace Strat.Mobile.Android.UI

/// Represents a value of type 'T that can sourced from an Android application resource, or an instance.
type ResourceOrValue<'T> = 
   | Value of 'T
   | ResourceId of int


/// Represents data for a menu item.
type MenuItem = Item of MenuItemId: int * Text: ResourceOrValue<string> * IconResId: option<int> 