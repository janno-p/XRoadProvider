module XRoadProvider.Tests.Program

open Expecto

[<EntryPoint>]
let main args =
    System.Threading.Thread.Sleep(10000)
    runTestsInAssembly Expecto.Tests.defaultConfig args
