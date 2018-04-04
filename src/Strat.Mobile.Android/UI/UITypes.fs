namespace Strat.Mobile.Android.UI


type ResourceOrValue<'T> = 
   | Value of 'T
   | ResourceId of int

type MenuItem = Item of MenuItemId: int * Text: ResourceOrValue<string> * IconResId: option<int> 