namespace XRoadProvider.Test

open FsUnit
open NUnit.Framework
open System.IO
open XRoad

[<TestFixture>]
module XRoadRpcEncodedV4Test =
    [<Test>]
    let ``Parse Maakataster xml schema definition`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/Maakataster.wsdl.xml", "et")
        let typeSchemas = schema.TypeSchemas
        typeSchemas.Count |> should equal 3
        typeSchemas.ContainsKey "http://producers.maakataster.xtee.riik.ee/producer/maakataster" |> should be True
        typeSchemas.ContainsKey "http://www.w3.org/1999/xlink" |> should be True
        typeSchemas.ContainsKey "http://x-tee.riik.ee/xsd/xtee.xsd" |> should be True
        let mainSchema = typeSchemas.["http://producers.maakataster.xtee.riik.ee/producer/maakataster"]
        mainSchema.TargetNamespace.NamespaceName |> should equal "http://producers.maakataster.xtee.riik.ee/producer/maakataster"
