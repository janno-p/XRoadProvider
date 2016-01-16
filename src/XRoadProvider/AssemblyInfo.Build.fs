namespace System

#if DEBUG
open System.Runtime.CompilerServices
#else
open Microsoft.FSharp.Core.CompilerServices
#endif

#if DEBUG
[<assembly: InternalsVisibleTo("XRoadProvider.Tests")>]
#else
[<assembly: TypeProviderAssembly>]
#endif

do ()
