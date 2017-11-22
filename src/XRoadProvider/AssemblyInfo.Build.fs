namespace System

open Microsoft.FSharp.Core.CompilerServices

#if DEBUG
open System.Runtime.CompilerServices
#endif

[<assembly: TypeProviderAssembly>]

#if DEBUG
[<assembly: InternalsVisibleTo("XRoadProvider.Tests")>]
#endif

do ()
