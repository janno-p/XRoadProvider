module XRoadSerializer.Tests.SerializerTest

open FsUnit
open NUnit.Framework
open System
open System.IO
open System.Text
open System.Xml
open XRoad
open XRoad.Attributes

[<RequireQualifiedAccessAttribute>]
module TestXml =
    let [<Literal>] AbstractBaseType = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Reference xsi:type=""Concrete1""><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></Reference></keha></wrapper>"
    let [<Literal>] AbstractBaseTypeExplicitName = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:t=""testns""><keha><Reference xsi:type=""t:ConcreteTypeName""><BaseValue>test</BaseValue><SubValue3>test2</SubValue3></Reference></keha></wrapper>"
    let [<Literal>] ExtendedType = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Member xsi:type=""ExtendedType""><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></Member></keha></wrapper>"
    let [<Literal>] IntegerValue = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha>32</keha></wrapper>"
    let [<Literal>] NullableValues = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Value1>13</Value1><Value2 xsi:nil=""true"" /></keha></wrapper>"
    let [<Literal>] NullValue = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha xsi:nil=""true"" /></wrapper>"
    let [<Literal>] RootName = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><root xmlns=""urn:some-namespace""><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></root></wrapper>"
    let [<Literal>] SimpleValue = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></keha></wrapper>"
    let [<Literal>] StringValue = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha>string value</keha></wrapper>"
    let [<Literal>] SubTypeWithBaseTypeMembers = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></keha></wrapper>"

module TestType =
    type UnserializableType() =
        member val Value = 10 with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type WithContent() =
        [<XRoadContent>]
        member val ContentValue = true with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type ComplexType() =
        [<XRoadElement>]
        member val String = "test" with get, set
        [<XRoadElement>]
        member val BigInteger = 100I with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type SimpleType() =
        [<XRoadElement>]
        member val Value = 13 with get, set
        [<XRoadElement>]
        member val ComplexValue = ComplexType() with get, set
        [<XRoadElement>]
        member val SubContent = WithContent() with get, set
        member val IgnoredValue = true with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type WithNullableMembers() =
        [<XRoadElement(IsNullable=true)>]
        member val Value1 = Nullable(13) with get, set
        [<XRoadElement(IsNullable=true)>]
        member val Value2 = Nullable() with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type ExtendedType() =
        inherit ComplexType()
        [<XRoadElement>]
        member val OwnElement = "test" with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type UseBaseClass() =
        [<XRoadElement>]
        member val Member = ExtendedType() :> ComplexType with get, set

    [<AbstractClass; XRoadType(LayoutKind.Sequence)>]
    type AbstractBase() =
        [<XRoadElement>]
        member val BaseValue = "test" with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type Concrete1() =
        inherit AbstractBase()
        [<XRoadElement>]
        member val SubValue1 = "test2" with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type Concrete2() =
        inherit AbstractBase()
        [<XRoadElement>]
        member val SubValue2 = "test3" with get, set

    [<XRoadType("ConcreteTypeName", LayoutKind.Sequence, Namespace="testns")>]
    type Concrete3() =
        inherit AbstractBase()
        [<XRoadElement>]
        member val SubValue3 = "test2" with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type Referrer() =
        [<XRoadElement>]
        member val Reference = Concrete1() :> AbstractBase with get, set

module Serialization =
    let serialize qn (nslist: (string * string) list) value =
        let serializer = Serializer()
        use stream = new MemoryStream()
        use sw = new StreamWriter(stream, Encoding.UTF8)
        use writer = XmlWriter.Create(sw)
        writer.WriteStartDocument()
        writer.WriteStartElement("wrapper")
        writer.WriteAttributeString("xmlns", "xsi", XmlNamespace.Xmlns, XmlNamespace.Xsi)
        nslist |> List.iter (fun (pr,ns) -> writer.WriteAttributeString("xmlns", pr, XmlNamespace.Xmlns, ns))
        serializer.Serialize(writer, value, qn)
        writer.WriteEndElement()
        writer.WriteEndDocument()
        writer.Flush()
        sw.Flush()
        stream.Position <- 0L
        use sr = new StreamReader(stream, Encoding.UTF8)
        sr.ReadToEnd()

    let serialize' v = serialize (XmlQualifiedName("keha")) [] v

    let [<Test>] ``initializes new serializer`` () =
        Serializer() |> should not' (equal null)

    let [<Test>] ``can serialize simple value`` () =
        TestType.SimpleType() |> serialize' |> should equal TestXml.SimpleValue

    let [<Test>] ``serialize null value`` () =
        (null: string) |> serialize' |> should equal TestXml.NullValue

    let [<Test>] ``write qualified root name`` () =
        TestType.SimpleType() |> serialize (XmlQualifiedName("root", "urn:some-namespace")) [] |> should equal TestXml.RootName

    let [<Test>] ``serializing unserializable type`` () =
        TestDelegate(fun _ -> TestType.UnserializableType() |> serialize' |> ignore)
        |> should (throwWithMessage "Type `XRoadSerializer.Tests.SerializerTest+TestType+UnserializableType` is not serializable.") typeof<Exception>

    let [<Test>] ``serialize string value`` () =
        "string value" |> serialize' |> should equal TestXml.StringValue

    let [<Test>] ``serialize integer value`` () =
        32 |> serialize' |> should equal TestXml.IntegerValue

    let [<Test>] ``serialize nullable values`` () =
        TestType.WithNullableMembers() |> serialize' |> should equal TestXml.NullableValues

    let [<Test>] ``serialize not nullable as null`` () =
        TestDelegate (fun _ -> TestType.ComplexType(String = null) |> serialize' |> ignore)
        |> should (throwWithMessage "Not nullable property `String` of type `XRoadSerializer.Tests.SerializerTest+TestType+ComplexType` has null value.") typeof<Exception>

    let [<Test>] ``serialize extended type with base type contents`` () =
        TestType.ExtendedType() |> serialize' |> should equal TestXml.SubTypeWithBaseTypeMembers

    let [<Test>] ``serialize base type when subtype is used`` () =
        TestType.UseBaseClass() |> serialize' |> should equal TestXml.ExtendedType

    let [<Test>] ``serialize abstract base type when subtype is used`` () =
        TestType.Referrer() |> serialize' |> should equal TestXml.AbstractBaseType

    let [<Test>] ``serialize abstract base type when subtype is used (with explicit name and namespace)`` () =
        TestType.Referrer(Reference=TestType.Concrete3()) |> serialize (XmlQualifiedName("keha")) ["t", "testns"] |> should equal TestXml.AbstractBaseTypeExplicitName

module Deserialization =
    let deserialize (rootName: XmlQualifiedName) xml : 'T =
        let serializer = Serializer()
        use sr = new StringReader(xml)
        use reader = XmlReader.Create(sr)
        let rec moveToRoot () =
            if reader.NodeType = XmlNodeType.Element && reader.LocalName = rootName.Name && reader.NamespaceURI = rootName.Namespace
            then true
            else match reader.Read() with false -> false | true -> moveToRoot()
        match moveToRoot() with
        | true -> serializer.Deserialize<'T>(reader)
        | false -> failwith "Invalid xml: could not find root element."

    let deserialize'<'T> xml : 'T = deserialize (XmlQualifiedName("keha")) xml

    let [<Test>] ``deserialize null value`` () =
        TestXml.NullValue |> deserialize'<string> |> should equal null
