namespace System

open System.Reflection
#if DEBUG
open System.Runtime.CompilerServices
#else
open Microsoft.FSharp.Core.CompilerServices
#endif

[<assembly: AssemblyTitleAttribute("XRoadProvider")>]
[<assembly: AssemblyProductAttribute("XRoadProvider")>]
[<assembly: AssemblyDescriptionAttribute("Type providers for generating types and service interfaces for XRoad producers.")>]

#if DEBUG
[<assembly: InternalsVisibleToAttribute("XRoadProvider.Tests")>]
#else
[<assembly: TypeProviderAssemblyAttribute>]
#endif

do ()
