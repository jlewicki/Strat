open Strat.Collections.Test

module Program = 
   let [<EntryPoint>] main _ =
      IndexList.Map.should_apply_mapping_to_each_item_in_list()
      0