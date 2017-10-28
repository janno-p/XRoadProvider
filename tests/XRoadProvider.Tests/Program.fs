module XRoadProvider.Tests.Program

open Expecto
open System.Threading

[<EntryPoint>]
let main args =
    runTestsInAssembly Expecto.Tests.defaultConfig args
