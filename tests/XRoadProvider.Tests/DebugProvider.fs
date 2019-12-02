module XRoadProvider.Tests.DebugProvider

open Expecto
open XRoad
open XRoad.ProducerDefinition
open XRoad.Wsdl

let [<Tests>] tests =
    ptestList "Debug provider errors" [
        test "provider should not produce any errors" {
            let schema = ProducerDescription.Load(resolveUri @"C:\Users\Janno\Projects\itdak.xml", "et", [])
            buildProducerTargetClass "TargetClass" schema
            |> ignore
        }
    ]
