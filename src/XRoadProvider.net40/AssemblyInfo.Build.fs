namespace System

open Microsoft.FSharp.Core.CompilerServices

#if !IS_DESIGNTIME
[<assembly: TypeProviderAssembly>]
do ()
#endif
