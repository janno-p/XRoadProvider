module XRoadProvider.Tests.XRoadRpcEncodedV4Test

open Expecto
open System
open XRoad

let [<Tests>] tests =
    testList "rpc/encoded style tests" [
        test "parse Maakataster xml schema definition" {
            let schema = ProducerDescription.Load(Uri(__SOURCE_DIRECTORY__ + "/Wsdl/Maakataster.wsdl.xml"), "et", [])
            let typeSchemas = schema.TypeSchemas
            Expect.equal typeSchemas.Count 3 "should parse 3 type schemas"
            Expect.isTrue (typeSchemas.ContainsKey "http://producers.maakataster.xtee.riik.ee/producer/maakataster") "should contain default schema"
            Expect.isTrue (typeSchemas.ContainsKey "http://www.w3.org/1999/xlink") "should contain xlink schema"
            Expect.isTrue (typeSchemas.ContainsKey "http://x-tee.riik.ee/xsd/xtee.xsd") "should contain xtee schema"
            let mainSchema = typeSchemas.["http://producers.maakataster.xtee.riik.ee/producer/maakataster"]
            Expect.equal mainSchema.TargetNamespace.NamespaceName "http://producers.maakataster.xtee.riik.ee/producer/maakataster" "has wrong target namespace"
        }
    ]
