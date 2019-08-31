module XRoadProvider.Tests.DebugProvider

open Expecto
open XRoad.ProducerDefinition

let [<Tests>] tests =
    ptestList "Debug provider errors" [
        test "provider should not produce any errors" {
            buildProducerTargetClass "TargetClass" @"C:\Users\Janno\Projects\itdak.xml" "et" []
            |> ignore
        }
    ]
