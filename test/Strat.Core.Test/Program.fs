open Strat.Collections.Test

module Program = 
   let [<EntryPoint>] main _ =
      LazyList.Unfold.should_create_list_by_applying_function_until_it_returns_none()
      0