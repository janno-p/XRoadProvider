module XRoadProvider.Tests.Program

open Expecto

[<EntryPoint>]
let main args =
    runTestsInAssembly Expecto.Tests.defaultConfig args
    (*
    SerializationTest.tests
    |> Test.filter (fun n -> n.EndsWith("deserialize merge content array"))
    |> runTests defaultConfig
    //*)
