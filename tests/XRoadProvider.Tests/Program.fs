module XRoadProvider.Tests.Program

open Expecto

[<EntryPoint>]
let main args =
    //runTestsInAssembly Expecto.Tests.defaultConfig args
    runTests Expecto.Tests.defaultConfig AdsTest.tests |> ignore
    XRoad.EmitterDsl.stream.Dispose()
    0
