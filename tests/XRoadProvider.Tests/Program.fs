module XRoadProvider.Tests.Program

open Expecto

[<EntryPoint>]
let main args =
    //SerializationTest.tests
    //|> Test.filter (fun n -> n.EndsWith("can deserialize simple value"))
    //|> runTests defaultConfig
    runTestsInAssembly Expecto.Tests.defaultConfig args
