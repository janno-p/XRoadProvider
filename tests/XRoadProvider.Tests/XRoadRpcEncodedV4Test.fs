namespace XRoadProvider.Test

open NUnit.Framework
open Swensen.Unquote
open System.IO
open XRoad

[<TestFixture>]
module XRoadRpcEncodedV4Test =
    [<Test>]
    let ``Parse Maakataster xml schema definition`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/Maakataster.wsdl.xml", "et")
        let typeSchemas = schema.TypeSchemas
        test <@ typeSchemas.Count = 3 @>
        test <@ typeSchemas.ContainsKey("http://producers.maakataster.xtee.riik.ee/producer/maakataster") @>
        test <@ typeSchemas.ContainsKey("http://www.w3.org/1999/xlink") @>
        test <@ typeSchemas.ContainsKey("http://x-tee.riik.ee/xsd/xtee.xsd") @>
        let mainSchema = typeSchemas.["http://producers.maakataster.xtee.riik.ee/producer/maakataster"]
        test <@ mainSchema.TargetNamespace.NamespaceName = "http://producers.maakataster.xtee.riik.ee/producer/maakataster" @>
        ()
