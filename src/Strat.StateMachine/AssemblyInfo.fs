namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Strat.StateMachine")>]
[<assembly: AssemblyProductAttribute("Strat.StateMachine")>]
[<assembly: AssemblyDescriptionAttribute("Hierarchical state machine for F#")>]
[<assembly: AssemblyVersionAttribute("0.5.0")>]
[<assembly: AssemblyFileVersionAttribute("0.5.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.5.0"
