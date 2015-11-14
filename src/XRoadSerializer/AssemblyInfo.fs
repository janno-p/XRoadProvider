namespace System
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("XRoadSerializer")>]
[<assembly: AssemblyProductAttribute("XRoadProvider")>]
[<assembly: AssemblyDescriptionAttribute("Type providers for generating types and service interfaces for XRoad producers.")>]
[<assembly: AssemblyVersionAttribute("1.0.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0.0")>]
[<assembly: InternalsVisibleToAttribute("XRoadProvider.Tests")>]
[<assembly: TypeProviderAssemblyAttribute()>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.0"
