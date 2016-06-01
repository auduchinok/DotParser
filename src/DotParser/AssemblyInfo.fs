namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("DotParser")>]
[<assembly: AssemblyProductAttribute("DotParser")>]
[<assembly: AssemblyDescriptionAttribute("Graphviz dot parser")>]
[<assembly: AssemblyVersionAttribute("0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1"
    let [<Literal>] InformationalVersion = "0.1"
