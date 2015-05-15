namespace XRoadProvider.Test

open NUnit.Framework
open Swensen.Unquote
open System.IO
open XRoad

[<TestFixture>]
module XRoadRpcEncodedV4Test =
    [<Test>]
    let ``Parse Maakataster xml schema definition`` () =
        let schema = ProducerDescription.Load(__SOURCE_DIRECTORY__ + "/Wsdl/Maakataster.wsdl.xml")
        let typeSchemas = schema.TypeSchemas
        test <@ typeSchemas.Count = 3 @>
        test <@ typeSchemas.ContainsKey("http://producers.maakataster.xtee.riik.ee/producer/maakataster") @>
        test <@ typeSchemas.ContainsKey("http://www.w3.org/1999/xlink") @>
        test <@ typeSchemas.ContainsKey("http://x-tee.riik.ee/xsd/xtee.xsd") @>
        let mainSchema = typeSchemas.["http://producers.maakataster.xtee.riik.ee/producer/maakataster"]
        test <@ mainSchema.TargetNamespace.NamespaceName = "http://producers.maakataster.xtee.riik.ee/producer/maakataster" @>
        (*
        let document = System.Xml.Linq.XDocument.Load(__SOURCE_DIRECTORY__ + "/Wsdl/Maakataster.wsdl.xml")
        let definitionsNode = document.Element(System.Xml.Linq.XName.Get("definitions", XmlNamespace.Wsdl))
        let typesNode = definitionsNode.Element(System.Xml.Linq.XName.Get("types", XmlNamespace.Wsdl))
        let schemaNode = typesNode.Element(System.Xml.Linq.XName.Get("schema", XmlNamespace.Xsd))
        let result = XRoadProvider.Wsdl.XsdSchema.parseSchemaNode(schemaNode)
        printfn "%A" result
        *)
        ()
