﻿namespace XRoadProvider.Test

open FsUnit
open NUnit.Framework
open System.Xml.Linq
open XRoad.TypeSchema
open XRoad.TypeSchema.Parser

[<TestFixture>]
module XmlSchemaTest =
    [<Test>]
    let ``Header element inside schema after type definitions`` () =
        let fragment = """<schema xmlns="http://www.w3.org/2001/XMLSchema">
                           <element name="testElement" type="string" />
                           <annotation />
                           <include namespace="urn:some-namespace-to-include" />
                        </schema>"""
        let node = XElement.Parse(fragment)
        let schemaNode = SchemaNode.FromNode(node)
        (fun () -> node |> parseSchemaNode schemaNode |> ignore)
        |> should (throwWithMessage "Element {http://www.w3.org/2001/XMLSchema}include inside schema element was not expected at the current position!") typeof<exn>
