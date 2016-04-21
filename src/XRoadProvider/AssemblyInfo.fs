namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("XRoadProvider")>]
[<assembly: AssemblyProductAttribute("XRoadProvider")>]
[<assembly: AssemblyDescriptionAttribute("Type providers for generating types and service interfaces for XRoad producers.")>]
[<assembly: AssemblyVersionAttribute("1.0.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0.0")>]
[<assembly: AssemblyInformationalVersionAttribute("1.0.0-alpha005")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.0"
    let [<Literal>] InformationalVersion = "1.0.0-alpha005"
