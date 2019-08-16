module XRoadProvider.Tests.XmlSchemaTest

open Expecto
open System.Xml.Linq
open XRoad.TypeSchema
open XRoad.TypeSchema.Parser

let [<Tests>] tests =
    testList "XML schema parsing" [
        test "header element inside schema after type definitions" {
            let fragment = """<schema xmlns="http://www.w3.org/2001/XMLSchema">
                               <element name="testElement" type="string" />
                               <annotation />
                               <include namespace="urn:some-namespace-to-include" />
                            </schema>"""
            let node = XElement.Parse(fragment)
            let schemaNode = SchemaNode.FromNode(node)
            Expect.throwsC
                (fun () -> node |> parseSchemaNode schemaNode |> ignore)
                (fun e -> Expect.equal e.Message "Element {http://www.w3.org/2001/XMLSchema}include inside schema element was not expected at the current position!")
                "did not detect invalid schema"
        }
    ]
