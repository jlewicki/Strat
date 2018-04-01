namespace Strat.UI

type IView<'ViewModel> =
   abstract SetViewModel: 'ViewModel -> unit

