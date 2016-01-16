namespace System

open System.Reflection
#if DEBUG
open System.Runtime.CompilerServices
#else
open Microsoft.FSharp.Core.CompilerServices
#endif

[<assembly: AssemblyTitle("XRoadProvider")>]
[<assembly: AssemblyProduct("XRoadProvider")>]
[<assembly: AssemblyDescription("Type providers for generating types and service interfaces for XRoad producers.")>]

#if DEBUG
[<assembly: InternalsVisibleTo("XRoadProvider.Tests")>]
#else
[<assembly: TypeProviderAssemblyAttribute>]
#endif

do ()
