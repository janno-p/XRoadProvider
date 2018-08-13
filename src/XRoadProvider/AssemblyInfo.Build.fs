namespace System

#if DEBUG
open System.Runtime.CompilerServices
#else
open Microsoft.FSharp.Core.CompilerServices
#endif

#if DEBUG
[<assembly: InternalsVisibleTo("XRoadProvider.Tests")>]
[<assembly: InternalsVisibleTo("XRoadProvider.Tests.net40")>]
#else
[<assembly: TypeProviderAssembly>]
#endif

do ()
