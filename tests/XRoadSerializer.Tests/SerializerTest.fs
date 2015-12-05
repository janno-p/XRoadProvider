module XRoadSerializer.Tests.SerializerTest

#nowarn "1104"

open FsUnit
open NUnit.Framework
open System
open System.IO
open System.Runtime.InteropServices
open System.Text
open System.Xml
open XRoad
open XRoad.Attributes

[<RequireQualifiedAccessAttribute>]
module TestXml =
    let [<Literal>] AbstractBaseType = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Reference xsi:type=""Concrete1""><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></Reference></keha></wrapper>"
    let [<Literal>] AbstractBaseTypeExplicitName = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:t=""testns""><keha><Reference xsi:type=""t:ConcreteTypeName""><BaseValue>test</BaseValue><SubValue3>test2</SubValue3></Reference></keha></wrapper>"
    let [<Literal>] AbstractType = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></keha></wrapper>"
    let [<Literal>] Choice1Of2 = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Choice1Element>test</Choice1Element></keha></wrapper>"
    let [<Literal>] Choice2Of2 = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Choice2><Choice2Element>test</Choice2Element></Choice2></keha></wrapper>"
    let [<Literal>] ExtendedType = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Member xsi:type=""ExtendedType""><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></Member></keha></wrapper>"
    let [<Literal>] IntegerValue = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha>32</keha></wrapper>"
    let [<Literal>] NullableValues = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Value1>13</Value1><Value2 xsi:nil=""true"" /></keha></wrapper>"
    let [<Literal>] NullValue = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha xsi:nil=""true"" /></wrapper>"
    let [<Literal>] RootName = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><root xmlns=""urn:some-namespace""><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></root></wrapper>"
    let [<Literal>] SimpleValue = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></keha></wrapper>"
    let [<Literal>] StringValue = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha>string value</keha></wrapper>"
    let [<Literal>] SubTypeWithBaseTypeMembers = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></keha></wrapper>"
    let [<Literal>] WithChoice1Sample = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><NotAChoice>tere</NotAChoice><Choice1Element>test</Choice1Element></keha></wrapper>"
    let [<Literal>] WithChoice2Sample = @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><NotAChoice>tere</NotAChoice><Choice2><Choice2Element>test</Choice2Element></Choice2></keha></wrapper>"

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

    [<AllowNullLiteral; XRoadType(LayoutKind.Sequence)>]
    type Choice1() =
        [<XRoadElement>]
        member val Choice1Element = "test" with get, set

    [<AllowNullLiteral; XRoadType(LayoutKind.Sequence)>]
    type Choice2() =
        [<XRoadElement>]
        member val Choice2Element = "test" with get, set

    [<XRoadType(LayoutKind.Choice)>]
    [<XRoadChoiceOption(1, "Choice1", false)>]
    [<XRoadChoiceOption(2, "Choice2", true)>]
    type TestChoice =
        val private ``@__id``: int
        val private ``@__value``: obj
        private new(id, value: obj) = { ``@__id`` = id; ``@__value`` = value }
        member this.TryGetChoice1([<Out>] value: Choice1 byref) =
            if this.``@__id`` = 1 then value <- unbox this.``@__value``
            else value <- null
            this.``@__id`` = 1
        member this.TryGetChoice2([<Out>] value: Choice2 byref) =
            if this.``@__id`` = 2 then value <- unbox this.``@__value``
            else value <- null
            this.``@__id`` = 2
        static member NewChoice1(value: Choice1) = TestChoice(1, value)
        static member NewChoice2(value: Choice2) = TestChoice(2, value)

    [<XRoadType(LayoutKind.Sequence)>]
    type WithChoice() =
        [<XRoadElement>]
        member val NotAChoice = "tere" with get, set
        [<XRoadElement>]
        member val IsAChoice = TestChoice.NewChoice1(Choice1()) with get, set

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

    let [<Test>] ``serialize choice type 1`` () =
        TestType.TestChoice.NewChoice1(TestType.Choice1())
        |> serialize'
        |> should equal TestXml.Choice1Of2

    let [<Test>] ``serialize choice type 2`` () =
        TestType.TestChoice.NewChoice2(TestType.Choice2())
        |> serialize'
        |> should equal TestXml.Choice2Of2

    let [<Test>] ``serialize inner choice 1 element`` () =
        TestType.WithChoice() |> serialize' |> should equal TestXml.WithChoice1Sample

    let [<Test>] ``serialize inner choice 2 element`` () =
        let value = TestType.WithChoice()
        value.IsAChoice <- TestType.TestChoice.NewChoice2(TestType.Choice2())
        value |> serialize' |> should equal TestXml.WithChoice2Sample

module Deserialization =
    let deserialize<'T> (rootName: XmlQualifiedName) xml : 'T =
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
        TestXml.NullValue |> deserialize'<string> |> should be Null

    let [<Test>] ``deserialize string value`` () =
        TestXml.StringValue |> deserialize'<string> |> should equal "string value"

    let [<Test>] ``deserialize integer value`` () =
        TestXml.IntegerValue |> deserialize'<int> |> should equal 32

    let [<Test>] ``deserialize simple value`` () =
        let result = TestXml.SimpleValue |> deserialize'<TestType.SimpleType>
        result |> should not' (be Null)
        result.Value |> should equal 13
        result.ComplexValue |> should not' (be Null)
        result.ComplexValue.BigInteger |> should equal 100I
        result.ComplexValue.String |> should equal "test"
        result.SubContent |> should not' (be Null)
        result.SubContent.ContentValue |> should equal true

    let [<Test>] ``deserialize abstract type`` () =
        TestDelegate (fun _ -> TestXml.AbstractType |> deserialize'<TestType.AbstractBase> |> ignore)
        |> should (throwWithMessage "Cannot deserialize abstract type `XRoadSerializer.Tests.SerializerTest+TestType+AbstractBase`.") typeof<Exception>

    let [<Test>] ``deserialize choice type 1`` () =
        let result = TestXml.Choice1Of2 |> deserialize'<TestType.TestChoice>
        result |> should not' (be Null)
        let (success, value) = result.TryGetChoice1()
        success |> should equal true
        value |> should not' (be Null)
        value.Choice1Element |> should equal "test"
        let (success, value) = result.TryGetChoice2()
        success |> should equal false
        value |> should be Null

    let [<Test>] ``deserialize choice type 2`` () =
        let result = TestXml.Choice2Of2 |> deserialize'<TestType.TestChoice>
        result |> should not' (be Null)
        let (success, value) = result.TryGetChoice1()
        success |> should equal false
        value |> should be Null
        let (success, value) = result.TryGetChoice2()
        success |> should equal true
        value |> should not' (be Null)
        value.Choice2Element |> should equal "test"

    let [<Test>] ``deserialize inner choice 1 element`` () =
        let result = TestXml.WithChoice1Sample |> deserialize'<TestType.WithChoice>
        result |> should not' (be Null)
        result.NotAChoice |> should equal "tere"
        result.IsAChoice |> should not' (be Null)
        let (success, value) = result.IsAChoice.TryGetChoice1()
        success |> should equal true
        value |> should not' (be Null)
        value.Choice1Element |> should equal "test"
        let (success, value) = result.IsAChoice.TryGetChoice2()
        success |> should equal false
        value |> should be Null

    let [<Test>] ``deserialize inner choice 2 element`` () =
        let result = TestXml.WithChoice2Sample |> deserialize'<TestType.WithChoice>
        result |> should not' (be Null)
        result.NotAChoice |> should equal "tere"
        result.IsAChoice |> should not' (be Null)
        let (success, value) = result.IsAChoice.TryGetChoice1()
        success |> should equal false
        value |> should be Null
        let (success, value) = result.IsAChoice.TryGetChoice2()
        success |> should equal true
        value |> should not' (be Null)
        value.Choice2Element |> should equal "test"
