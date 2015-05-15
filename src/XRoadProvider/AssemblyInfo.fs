namespace System
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("XRoadProvider")>]
[<assembly: AssemblyProductAttribute("XRoadProvider")>]
[<assembly: AssemblyDescriptionAttribute("Type providers for generating types and service interfaces for XRoad producers.")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
[<assembly: InternalsVisibleToAttribute("XRoadProvider.Tests")>]
[<assembly: TypeProviderAssemblyAttribute()>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
