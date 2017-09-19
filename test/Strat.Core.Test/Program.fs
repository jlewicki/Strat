open Strat.Collections.Test

module Program = 
   let [<EntryPoint>] main _ =
      PrioritySearchQueue.Pick.should_return_picked_item()
      0