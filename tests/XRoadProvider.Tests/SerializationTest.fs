module XRoadProvider.Tests.SerializationTest

open Expecto
open System
open System.IO
open System.Runtime.InteropServices
open System.Text
open System.Threading
open System.Xml
open System.Xml.Linq
open XRoad
open XRoad.Emitter
open XRoad.Serialization.Attributes
open XRoadProvider.Tests.SoapUtil

let [<Literal>] producerName = "producer"
let [<Literal>] producerNamespace = "http://producer.x-road.eu/"

module Types =
    type UnserializableType() =
        member val Value = Unchecked.defaultof<int> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type WithContent() =
        [<XRoadElement(MergeContent=true)>]
        member val ContentValue = Unchecked.defaultof<bool> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type ComplexType() =
        [<XRoadElement>]
        member val String = Unchecked.defaultof<string> with get, set
        [<XRoadElement>]
        member val BigInteger = Unchecked.defaultof<bigint> with get, set
    
    [<XRoadType(LayoutKind.Sequence)>]
    type ExtendedType() =
        inherit ComplexType()
        [<XRoadElement>]
        member val OwnElement = Unchecked.defaultof<string> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type UseBaseClass() =
        [<XRoadElement>]
        member val Member = Unchecked.defaultof<ComplexType> with get, set
    
    [<XRoadType(LayoutKind.Sequence)>]
    type SimpleType() =
        [<XRoadElement>]
        member val Value = Unchecked.defaultof<int> with get, set
        [<XRoadElement>]
        member val ComplexValue = Unchecked.defaultof<ComplexType> with get, set
        [<XRoadElement>]
        member val SubContent = Unchecked.defaultof<WithContent> with get, set
        member val IgnoredValue = true with get, set

    [<XRoadType(LayoutKind.Sequence)>]
    type WithNullableMembers() =
        [<XRoadElement(IsNullable=true)>]
        member val Value1 = Unchecked.defaultof<Nullable<int>> with get, set
        [<XRoadElement(IsNullable=true)>]
        member val Value2 = Unchecked.defaultof<Nullable<int>> with get, set
        
    [<AbstractClass; AllowNullLiteral; XRoadType(LayoutKind.Sequence)>]
    type AbstractBase() =
        [<XRoadElement>]
        member val BaseValue = Unchecked.defaultof<string> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type Referrer() =
        [<XRoadElement>]
        member val Reference = Unchecked.defaultof<AbstractBase> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type Concrete1() =
        inherit AbstractBase()
        [<XRoadElement>]
        member val SubValue1 = Unchecked.defaultof<string> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type Concrete2() =
        inherit AbstractBase()
        [<XRoadElement>]
        member val SubValue2 = Unchecked.defaultof<string> with get, set

    [<XRoadType("ConcreteTypeName", LayoutKind.Sequence, Namespace="testns")>]
    type Concrete3() =
        inherit AbstractBase()
        [<XRoadElement>]
        member val SubValue3 = Unchecked.defaultof<string> with get, set
        
    [<XRoadType(LayoutKind.Choice)>]
    [<XRoadChoiceOption(1, "value1", MergeContent=false)>]
    [<XRoadChoiceOption(2, "value2", MergeContent=false)>]
    type AbstractRootChoice =
        val private __id: int
        val private __value: obj
        private new(id, value: obj) = { __id = id; __value = value }
        member this.TryGet_value1([<Out>] value: AbstractBase byref) =
            if this.__id = 1 then value <- unbox this.__value
            else value <- null
            this.__id = 1
        member this.TryGet_value2([<Out>] value: string byref) =
            if this.__id = 2 then value <- unbox this.__value
            else value <- null
            this.__id = 2
        static member New_value1(value: AbstractBase) = AbstractRootChoice(1, value)
        static member New_value2(value: string) = AbstractRootChoice(2, value)
        
    [<XRoadType(LayoutKind.Sequence)>]
    type TypeWithAbstractChoice() =
        [<XRoadElement>]
        member val X = Unchecked.defaultof<AbstractRootChoice> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type WithArray1() =
        [<XRoadElement(IsNullable=true)>]
        [<XRoadCollection(ItemIsNullable=true)>]
        member val Array = Unchecked.defaultof<bool[]> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type InnerReferrer() =
        [<XRoadElement>]
        member val Ref = Unchecked.defaultof<Referrer> with get, set
        
    [<AllowNullLiteral; XRoadType(LayoutKind.Sequence)>]
    type Choice1() =
        [<XRoadElement>]
        member val Choice1Element = Unchecked.defaultof<string> with get, set

    [<AllowNullLiteral; XRoadType(LayoutKind.Sequence)>]
    type Choice2() =
        [<XRoadElement>]
        member val Choice2Element = Unchecked.defaultof<string> with get, set

    [<XRoadType(LayoutKind.Choice)>]
    [<XRoadChoiceOption(1, "Choice1", MergeContent=true)>]
    [<XRoadChoiceOption(2, "Choice2", MergeContent=false)>]
    type TestChoice =
        val private __id: int
        val private __value: obj
        private new(id, value: obj) = { __id = id; __value = value }
        member this.TryGetChoice1([<Out>] value: Choice1 byref) =
            if this.__id = 1 then value <- unbox this.__value
            else value <- null
            this.__id = 1
        member this.TryGetChoice2([<Out>] value: Choice2 byref) =
            if this.__id = 2 then value <- unbox this.__value
            else value <- null
            this.__id = 2
        static member NewChoice1(value: Choice1) = TestChoice(1, value)
        static member NewChoice2(value: Choice2) = TestChoice(2, value)
        
    [<XRoadType(LayoutKind.Sequence)>]
    type WithChoice() =
        [<XRoadElement(IsNullable=true)>]
        member val NotAChoice = Unchecked.defaultof<string> with get, set
        [<XRoadElement>]
        member val IsAChoice = Unchecked.defaultof<TestChoice> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type WithBinaryContent() =
        [<XRoadElement>]
        member val BinaryContent = Unchecked.defaultof<BinaryContent> with get, set
        
    [<XRoadType(LayoutKind.Sequence)>]
    type WithXopBinaryContent() =
        [<XRoadElement(UseXop=true)>]
        member val BinaryContent = Unchecked.defaultof<BinaryContent> with get, set
        
    [<XRoadType>]
    type HasOptionalElements () =
        [<XRoadElement>]
        member val Value1 = Optional.Option.None<string>() with get, set
        [<XRoadElement>]
        member val Value2 = Optional.Option.None<int>() with get, set
        [<XRoadElement; XRoadCollection("item")>]
        member val Array1 = Optional.Option.None<int[]>() with get, set
        
    [<XRoadType>]
    type Level1 () =
        [<XRoadElement>]
        member val Value1 = Nullable<int>() with get, set
    
    [<XRoadType>]
    type Level2 () =
        inherit Level1()
        [<XRoadElement>]
        member val Value2 = Nullable<int>() with get, set
    
    [<XRoadType>]
    type Level3 () =
        inherit Level2()
        [<XRoadElement>]
        member val Value3 = Nullable<int>() with get, set

type Services =
    [<XRoadOperation("Service1", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service1", producerNamespace)>]
    [<XRoadResponse("Service1Response", producerNamespace)>]
    abstract Service1: unit -> unit

    [<XRoadOperation("Service2", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service2", producerNamespace)>]
    [<XRoadResponse("Service2Response", producerNamespace)>]
    abstract Service2: int64 -> string[]
    
    [<XRoadOperation("SimpleValueService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("SimpleValueService", producerNamespace)>]
    [<XRoadResponse("SimpleValueServiceResponse", producerNamespace)>]
    abstract SimpleValueService: [<XRoadElement("request")>] request: Types.SimpleType -> Types.SimpleType
    
    [<XRoadOperation("StringService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("StringService", producerNamespace)>]
    [<XRoadResponse("StringServiceResponse", producerNamespace)>]
    abstract StringService: [<XRoadElement("request", IsNullable = true)>] request: string -> string
    
    [<XRoadOperation("IntService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("IntService", producerNamespace)>]
    [<XRoadResponse("IntServiceResponse", producerNamespace)>]
    abstract IntService: [<XRoadElement("request")>] request: int32 -> int32
    
    [<XRoadOperation("NullableService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("NullableService", producerNamespace)>]
    [<XRoadResponse("NullableServiceResponse", producerNamespace)>]
    abstract NullableService: [<XRoadElement("request")>] request: Types.WithNullableMembers -> Types.WithNullableMembers
    
    [<XRoadOperation("ComplexTypeService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("ComplexTypeService", producerNamespace)>]
    [<XRoadResponse("ComplexTypeServiceResponse", producerNamespace)>]
    abstract ComplexTypeService: [<XRoadElement("request")>] request: Types.ComplexType -> Types.ComplexType
    
    [<XRoadOperation("QualifiedRootService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("QualifiedRootService", producerNamespace)>]
    [<XRoadResponse("QualifiedRootServiceResponse", producerNamespace)>]
    abstract QualifiedRootService: [<XRoadElement("root", Namespace = "urn:some-namespace")>] request: Types.SimpleType -> Types.SimpleType
    
    [<XRoadOperation("UnserializableService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("UnserializableService", producerNamespace)>]
    [<XRoadResponse("UnserializableServiceResponse", producerNamespace)>]
    abstract UnserializableService: [<XRoadElement("request")>] request: Types.UnserializableType -> Types.UnserializableType

    [<XRoadOperation("AbstractChoiceService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("AbstractChoiceService", producerNamespace)>]
    [<XRoadResponse("AbstractChoiceServiceResponse", producerNamespace)>]
    abstract AbstractChoiceService: [<XRoadElement("request")>] request: Types.TypeWithAbstractChoice -> Types.TypeWithAbstractChoice
    
    [<XRoadOperation("WithArray1Service", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("WithArray1Service", producerNamespace)>]
    [<XRoadResponse("WithArray1ServiceResponse", producerNamespace)>]
    abstract WithArray1Service: [<XRoadElement("request")>] request: Types.WithArray1 -> Types.WithArray1
    
    [<XRoadOperation("ExtendedTypeService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("ExtendedTypeService", producerNamespace)>]
    [<XRoadResponse("ExtendedTypeServiceResponse", producerNamespace)>]
    abstract ExtendedTypeService: [<XRoadElement("request")>] request: Types.ComplexType -> Types.ComplexType
    
    [<XRoadOperation("UseBaseClassService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("UseBaseClassService", producerNamespace)>]
    [<XRoadResponse("UseBaseClassServiceResponse", producerNamespace)>]
    abstract UseBaseClassService: [<XRoadElement("request")>] request: Types.UseBaseClass -> Types.UseBaseClass
    
    [<XRoadOperation("ReferrerService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("ReferrerService", producerNamespace)>]
    [<XRoadResponse("ReferrerServiceResponse", producerNamespace)>]
    abstract ReferrerService: [<XRoadElement("request")>] request: Types.Referrer -> Types.Referrer
    
    [<XRoadOperation("InnerReferrerService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("InnerReferrerService", producerNamespace)>]
    [<XRoadResponse("InnerReferrerServiceResponse", producerNamespace)>]
    abstract InnerReferrerService: [<XRoadElement("request")>] request: Types.InnerReferrer -> Types.InnerReferrer
    
    [<XRoadOperation("TestChoiceService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("TestChoiceService", producerNamespace)>]
    [<XRoadResponse("TestChoiceServiceResponse", producerNamespace)>]
    abstract TestChoiceService: [<XRoadElement("request")>] request: Types.TestChoice -> Types.TestChoice
    
    [<XRoadOperation("WithChoiceService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("WithChoiceService", producerNamespace)>]
    [<XRoadResponse("WithChoiceServiceResponse", producerNamespace)>]
    abstract WithChoiceService: [<XRoadElement("request")>] request: Types.WithChoice -> Types.WithChoice
    
    [<XRoadOperation("WithBinaryContentService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("WithBinaryContentService", producerNamespace)>]
    [<XRoadResponse("WithBinaryContentServiceResponse", producerNamespace)>]
    abstract WithBinaryContentService: [<XRoadElement("request")>] request: Types.WithBinaryContent -> Types.WithBinaryContent
    
    [<XRoadOperation("WithXopBinaryContentService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("WithXopBinaryContentService", producerNamespace)>]
    [<XRoadResponse("WithXopBinaryContentServiceResponse", producerNamespace)>]
    abstract WithXopBinaryContentService: [<XRoadElement("request")>] request: Types.WithXopBinaryContent -> Types.WithXopBinaryContent

    [<XRoadOperation("HasOptionalElementsService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("HasOptionalElementsService", producerNamespace)>]
    [<XRoadResponse("HasOptionalElementsServiceResponse", producerNamespace)>]
    abstract HasOptionalElementsService: [<XRoadElement("request")>] request: Types.HasOptionalElements -> Types.HasOptionalElements
    
    [<XRoadOperation("ArrayService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("ArrayService", producerNamespace)>]
    [<XRoadResponse("ArrayServiceResponse", producerNamespace)>]
    abstract ArrayService: [<XRoadElement("request")>] request: string[] -> string[]
    
    [<XRoadOperation("OptionalIntService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("OptionalIntService", producerNamespace)>]
    [<XRoadResponse("OptionalIntServiceResponse", producerNamespace)>]
    abstract OptionalIntService: [<XRoadElement("request")>] request: Optional.Option<int> -> Optional.Option<int>
    
    [<XRoadOperation("Level3Service", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Level3Service", producerNamespace)>]
    [<XRoadResponse("Level3ServiceResponse", producerNamespace)>]
    abstract Level3Service: [<XRoadElement("request")>] request: Types.Level3 -> Types.Level3


let deserialize (nm: string) (xml: string) =
    let map = typeof<Services>.GetMethod(nm) |> getMethodMap
    use reader = XDocument.Parse(xml).CreateReader()
    map.Deserializer.Invoke(reader, SerializerContext())

let serialize nm value context =
    let map = typeof<Services>.GetMethod(nm) |> getMethodMap
    use stream = new MemoryStream()
    use sw = new StreamWriter(stream, Encoding.UTF8)
    use writer = XmlWriter.Create(sw)
    writer.WriteStartDocument()
    writer.WriteStartElement("Body")
    writer.WriteAttributeString("xmlns", "xsi", XmlNamespace.Xmlns, XmlNamespace.Xsi)
    writer.WriteAttributeString("xmlns", "tns", XmlNamespace.Xmlns, producerNamespace)
    writer.WriteAttributeString("xmlns", "test", XmlNamespace.Xmlns, "testns")
    map.Serializer.Invoke(writer, value, context)
    writer.WriteEndElement()
    writer.WriteEndDocument()
    writer.Flush()
    stream.Position <- 0L
    use reader = new StreamReader(stream, Encoding.UTF8)
    reader.ReadToEnd()

let serialize' nm value =
    serialize nm value (SerializerContext())

let simpleTypeEntity =
    let entity = Types.SimpleType(Value = 13)
    entity.ComplexValue <- Types.ComplexType(String = "test", BigInteger = 100I)
    entity.SubContent <- Types.WithContent(ContentValue = true)
    entity

let [<Tests>] tests =
    testList "serialization tests" [
        ptest "can handle array type response" {
            let xml = @"<response><item /><item /></response>"
            let response = xml |> deserialize "Service2" |> unbox<string[]>
            Expect.isNotNull response "response should have value"
            Expect.equal response.Length 2 "response should have exactly 2 items"
        }
        
        test "can serialize unit request" {
            let xml = serialize' "Service1" [||]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:Service1 /></Body>" "invalid xml result"
        }
        
        test "can deserialize unit response" {
            let xml = @"<?xml version=""1.0"" encoding=""utf-8""?><Service1 xmlns=""http://producer.x-road.eu/"" />"
            let response = xml |> deserialize "Service1"
            Expect.isNull response "invalid xml result"
        }
        
        test "can serialize simple value" {
            let xml = serialize' "SimpleValueService" [| simpleTypeEntity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:SimpleValueService><request><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></request></tns:SimpleValueService></Body>" "invalid xml result"
        }
        
        ptest "can deserialize simple value" {
            let xml = @"<?xml version=""1.0"" encoding=""utf-8""?><SimpleValueServiceResponse xmlns=""http://producer.x-road.eu/""><response xmlns=""""><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></response></SimpleValueServiceResponse>"
            let response = xml |> deserialize "SimpleValueService"
            Expect.isNotNull response "invalid deserialization result"
            let result: Types.SimpleType = response |> unbox
            Expect.equal result.Value 13 "wrong result.Value value"
            Expect.equal result.ComplexValue.BigInteger 100I "wrong result.ComplexValue.BigInteger value"
            Expect.equal result.ComplexValue.String "test" "wrong result.ComplexValue.String value"
            Expect.isTrue result.SubContent.ContentValue "wrong result.SubContent.ContentValue value"
        }
        
        test "serialize null value" {
            let xml = serialize' "StringService" [| (null: string) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:StringService><request xsi:nil=""true"" /></tns:StringService></Body>" "invalid serialization result"
        }
        
        test "deserialize null value" {
            let xml = @"<?xml version=""1.0"" encoding=""utf-8""?><StringServiceResponse xmlns=""http://producer.x-road.eu/""><response xsi:nil=""true"" xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns="""" /></StringServiceResponse>"
            let response = xml |> deserialize "StringService"
            Expect.isNull response "response should be null"
        }
        
        test "write qualified root name" {
            let xml = serialize' "QualifiedRootService" [| simpleTypeEntity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:QualifiedRootService><root xmlns=""urn:some-namespace""><Value xmlns="""">13</Value><ComplexValue xmlns=""""><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent xmlns="""">true</SubContent></root></tns:QualifiedRootService></Body>" "invalid xml result"
        }
        
        test "serializing unserializable type" {
            Expect.throwsC
                (fun _ ->
                    let value = Types.UnserializableType(Value = 10)
                    serialize' "UnserializableService" [| value |] |> ignore)
                (fun e -> Expect.equal e.Message "Type `XRoadProvider.Tests.SerializationTest+Types+UnserializableType` is not serializable." "invalid exception")
        }
        
        test "serialize string value" {
            let xml = serialize' "StringService" [| "string value" |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:StringService><request>string value</request></tns:StringService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize string value" {
            let xml = @"<?xml version=""1.0"" encoding=""utf-8""?><StringServiceResponse xmlns=""http://producer.x-road.eu/""><response xmlns="""">string value</response></StringServiceResponse>"
            let response = xml |> deserialize "StringService" |> unbox<string>
            Expect.equal response "string value" "response not equal to 'string value'"
        }
        
        test "serialize int value" {
            let xml = serialize' "IntService" [| 32 |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:IntService><request>32</request></tns:IntService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize int value" {
            failtest "needs review"
            (*
            let resultXml = 32 |> serialize'
            resultXml |> should equal @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha>32</keha></wrapper>"
            resultXml |> deserialize'<int> |> should equal 32
            *)
        }
        
        test "serialize nullable values" {
            let xml = serialize' "NullableService" [| Types.WithNullableMembers(Value1 = Nullable(13)) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:NullableService><request><Value1>13</Value1><Value2 xsi:nil=""true"" /></request></tns:NullableService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize nullable values" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.WithNullableMembers>
            result |> should not' (be Null)
            result.Value1 |> should not' (be Null)
            result.Value1 |> should equal 13
            result.Value2 |> should be Null
            *)
        }
        
        test "serialize not nullable as null" {
            Expect.throwsC
                (fun _ -> serialize' "ComplexTypeService" [| Types.ComplexType(String = null) |] |> ignore)
                (fun e -> Expect.equal e.Message "Not nullable property `String` of type `ComplexType` has null value." "invalid exception message")
        }
        
        test "serialize choice with abstract root element" {
            let optionEntity = Types.Concrete1(SubValue1 = "test2", BaseValue = "test")
            let xml = serialize' "AbstractChoiceService" [| Types.TypeWithAbstractChoice(X = Types.AbstractRootChoice.New_value1(optionEntity)) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:AbstractChoiceService><request><value1 xsi:type=""Concrete1""><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></value1></request></tns:AbstractChoiceService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize choice with abstract root element" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.TypeWithAbstractChoice>
            result |> should not' (be Null)
            result.X |> should not' (be Null)
            let (success, value) = result.X.TryGet_value1()
            success |> should equal true
            value |> should not' (be Null)
            value.BaseValue |> should equal "test"
            *)
        }
        
        test "serialize array with default property names" {
            let entity = Types.WithArray1(Array = [| true; false; true; true |])
            let xml = serialize' "WithArray1Service" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithArray1Service><request><Array><item>true</item><item>false</item><item>true</item><item>true</item></Array></request></tns:WithArray1Service></Body>" "invalid serialization result"
        }
        
        ptest "deserialize array with default property names" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.WithArray1>
            result |> should not' (be Null)
            result.Array |> should not' (be Null)
            result.Array |> should equal entity.Array
            *)
        }
        
        ptest "deserialize abstract type" {
            failtest "needs review"
            (*
            (fun () ->
                @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></keha></wrapper>"
                |> deserialize'<TestType.AbstractBase>
                |> ignore)
            |> should (throwWithMessage "Cannot deserialize abstract type `AbstractBase`.") typeof<Exception>
            *)
        }
        
        ptest "deserialize abstract type with no sub types" {
            failtest "needs review"
            (*
            (fun () ->
                @"<?xml version=""1.0"" encoding=""utf-8""?><wrapper xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""><keha><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></keha></wrapper>"
                |> deserialize'<TestType.AbstractBaseWithNoSubTypes>
                |> ignore)
            |> should (throwWithMessage "Cannot deserialize abstract type `AbstractBaseWithNoSubTypes`.") typeof<Exception>
            *)
        }
        
        test "serialize extended type with base type contents" {
            let entity = Types.ExtendedType(OwnElement = "test", String = "test", BigInteger = 100I)
            let xml = serialize' "ExtendedTypeService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:ExtendedTypeService><request xsi:type=""ExtendedType""><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></request></tns:ExtendedTypeService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize extended type with base type contents" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.ExtendedType>
            result |> should not' (be Null)
            result.BigInteger |> should equal entity.BigInteger
            result.OwnElement |> should equal entity.OwnElement
            result.String |> should equal entity.String
            let result = resultXml |> deserialize'<TestType.ComplexType>
            result |> should not' (be Null)
            result |> should be instanceOfType<TestType.ExtendedType>
            (result :?> TestType.ExtendedType).OwnElement |> should equal entity.OwnElement
            result.BigInteger |> should equal entity.BigInteger
            result.String |> should equal entity.String
            *)
        }
        
        test "serialize base type when subtype is used" {
            let entityMember = Types.ExtendedType(OwnElement = "test", String = "test", BigInteger = 100I)
            let entity = Types.UseBaseClass(Member = entityMember)
            let xml = serialize' "UseBaseClassService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:UseBaseClassService><request><Member xsi:type=""ExtendedType""><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></Member></request></tns:UseBaseClassService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize base type when subtype is used" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.UseBaseClass>
            result |> should not' (be Null)
            result.Member |> should not' (be Null)
            result.Member |> should be instanceOfType<TestType.ExtendedType>
            result.Member.BigInteger |> should equal entityMember.BigInteger
            (result.Member :?> TestType.ExtendedType).OwnElement |> should equal entityMember.OwnElement
            result.Member.String |> should equal entityMember.String
            *)
        }
        
        test "serialize abstract base type when subtype is used" {
            let concreteEntity = Types.Concrete1(SubValue1 = "test2", BaseValue = "test")
            let entity = Types.Referrer(Reference = concreteEntity)
            let xml = serialize' "ReferrerService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:ReferrerService><request><Reference xsi:type=""Concrete1""><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></Reference></request></tns:ReferrerService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize abstract base type when subtype is used" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.Referrer>
            result |> should not' (be Null)
            result.Reference |> should not' (be Null)
            result.Reference |> should be instanceOfType<TestType.Concrete1>
            (result.Reference :?> TestType.Concrete1).SubValue1 |> should equal concreteEntity.SubValue1
            *)
        }
        
        test "serialize abstract base type when subtype is used (with explicit name and namespace)" {
            let concreteEntity = Types.Concrete3(SubValue3 = "test2", BaseValue = "test")
            let entity = Types.Referrer(Reference = concreteEntity)
            let xml = serialize' "ReferrerService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:ReferrerService><request><Reference xsi:type=""test:ConcreteTypeName""><BaseValue>test</BaseValue><SubValue3>test2</SubValue3></Reference></request></tns:ReferrerService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize abstract base type when subtype is used (with explicit name and namespace)" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.Referrer>
            result |> should not' (be Null)
            result.Reference |> should not' (be Null)
            result.Reference |> should be instanceOfType<TestType.Concrete3>
            (result.Reference :?> TestType.Concrete3).SubValue3 |> should equal concreteEntity.SubValue3
            *)
        }
        
        test "serialize inner abstract base type" {
            let referenceEntity = Types.Concrete1(SubValue1 = "kino", BaseValue = "basev")
            let referrerEntity = Types.Referrer(Reference = referenceEntity)
            let entity = Types.InnerReferrer(Ref = referrerEntity)
            let xml = serialize' "InnerReferrerService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:InnerReferrerService><request><Ref><Reference xsi:type=""Concrete1""><BaseValue>basev</BaseValue><SubValue1>kino</SubValue1></Reference></Ref></request></tns:InnerReferrerService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize inner abstract base type" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.InnerReferrer>
            result |> should not' (be Null)
            result.Ref |> should not' (be Null)
            result.Ref.Reference |> should not' (be Null)
            result.Ref.Reference |> should be instanceOfType<TestType.Concrete1>
            (result.Ref.Reference :?> TestType.Concrete1).SubValue1 |> should equal referenceEntity.SubValue1
            result.Ref.Reference.BaseValue |> should equal referenceEntity.BaseValue
            *)
        }
        
        test "serialize choice type 1" {
            let optionEntity = Types.Choice1(Choice1Element = "test")
            let entity = Types.TestChoice.NewChoice1(optionEntity)
            let xml = serialize' "TestChoiceService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:TestChoiceService><Choice1Element>test</Choice1Element></tns:TestChoiceService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize choice type 1" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.TestChoice>
            result |> should not' (be Null)
            let (success, value) = result.TryGetChoice1()
            success |> should equal true
            value |> should not' (be Null)
            value.Choice1Element |> should equal optionEntity.Choice1Element
            let (success, value) = result.TryGetChoice2()
            success |> should equal false
            value |> should be Null
            *)
        }
        
        test "serialize choice type 2" {
            let optionEntity = Types.Choice2(Choice2Element = "test")
            let entity = Types.TestChoice.NewChoice2(optionEntity)
            let xml = serialize' "TestChoiceService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:TestChoiceService><Choice2><Choice2Element>test</Choice2Element></Choice2></tns:TestChoiceService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize choice type 2" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.TestChoice>
            result |> should not' (be Null)
            let (success, value) = result.TryGetChoice1()
            success |> should equal false
            value |> should be Null
            let (success, value) = result.TryGetChoice2()
            success |> should equal true
            value |> should not' (be Null)
            value.Choice2Element |> should equal optionEntity.Choice2Element
            *)
        }
        
        test "serialize inner choice 1 element" {
            let optionEntity = Types.Choice1(Choice1Element = "test")
            let choiceEntity = Types.TestChoice.NewChoice1(optionEntity)
            let entity = Types.WithChoice(NotAChoice = "tere", IsAChoice = choiceEntity)
            let xml = serialize' "WithChoiceService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithChoiceService><request><NotAChoice>tere</NotAChoice><Choice1Element>test</Choice1Element></request></tns:WithChoiceService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize inner choice 1 element" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.WithChoice>
            result |> should not' (be Null)
            result.NotAChoice |> should equal entity.NotAChoice
            result.IsAChoice |> should not' (be Null)
            let (success, value) = result.IsAChoice.TryGetChoice1()
            success |> should equal true
            value |> should not' (be Null)
            value.Choice1Element |> should equal optionEntity.Choice1Element
            let (success, value) = result.IsAChoice.TryGetChoice2()
            success |> should equal false
            value |> should be Null
            *)
        }
        
        test "serialize inner choice 2 element" {
            let optionEntity = Types.Choice2(Choice2Element = "test")
            let choiceEntity = Types.TestChoice.NewChoice2(optionEntity)
            let entity = Types.WithChoice(NotAChoice = "tere", IsAChoice = choiceEntity)
            let xml = serialize' "WithChoiceService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithChoiceService><request><NotAChoice>tere</NotAChoice><Choice2><Choice2Element>test</Choice2Element></Choice2></request></tns:WithChoiceService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize inner choice 2 element" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.WithChoice>
            result |> should not' (be Null)
            result.NotAChoice |> should equal entity.NotAChoice
            result.IsAChoice |> should not' (be Null)
            let (success, value) = result.IsAChoice.TryGetChoice1()
            success |> should equal false
            value |> should be Null
            let (success, value) = result.IsAChoice.TryGetChoice2()
            success |> should equal true
            value |> should not' (be Null)
            value.Choice2Element |> should equal optionEntity.Choice2Element
            *)
        }
        
        test "serialize empty string" {
            let optionEntity = Types.Choice1(Choice1Element = "test")
            let choiceEntity = Types.TestChoice.NewChoice1(optionEntity)
            let entity = Types.WithChoice(NotAChoice = "", IsAChoice = choiceEntity)
            let xml = serialize' "WithChoiceService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithChoiceService><request><NotAChoice /><Choice1Element>test</Choice1Element></request></tns:WithChoiceService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize empty string" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.WithChoice>
            result |> should not' (be Null)
            result.NotAChoice |> should equal ""
            result.IsAChoice |> should not' (be Null)
            let (success, value) = result.IsAChoice.TryGetChoice1()
            success |> should equal true
            value |> should not' (be Null)
            *)
        }
        
        test "serialize null string" {
            let optionEntity = Types.Choice1(Choice1Element = "test")
            let choiceEntity = Types.TestChoice.NewChoice1(optionEntity)
            let entity = Types.WithChoice(NotAChoice = null, IsAChoice = choiceEntity)
            let xml = serialize' "WithChoiceService" [| entity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithChoiceService><request><NotAChoice xsi:nil=""true"" /><Choice1Element>test</Choice1Element></request></tns:WithChoiceService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize null string" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.WithChoice>
            result |> should not' (be Null)
            result.NotAChoice |> should be Null
            result.IsAChoice |> should not' (be Null)
            let (success, value) = result.IsAChoice.TryGetChoice1()
            success |> should equal true
            value |> should not' (be Null)
            *)
        }
        
        test "serialize null array" {
            let xml = serialize' "WithArray1Service" [| Types.WithArray1() |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithArray1Service><request><Array xsi:nil=""true"" /></request></tns:WithArray1Service></Body>" "invalid serialization result"
        }
        
        ptest "deserialize null array" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.WithArray1>
            result |> should not' (be Null)
            result.Array |> should be Null
            *)
        }
        
        test "serialize inline file" {
            let context = SerializerContext()
            let entity = Types.WithBinaryContent(BinaryContent=BinaryContent.Create([| 1uy; 2uy; 3uy; 4uy |]))
            let xml = serialize "WithBinaryContentService" [| entity |] context
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithBinaryContentService><request><BinaryContent>AQIDBA==</BinaryContent></request></tns:WithBinaryContentService></Body>" "invalid serialization result"
            Expect.isEmpty context.Attachments "no serialized attachments was expected"
        }
        
        ptest "deserialize inline file" {
            failtest "needs review"
            (*
            let result = resultXml |> deserializeWithContext'<TestType.WithBinaryContent> context
            result |> should not' (be Null)
            context.Attachments |> should not' (be Null)
            context.Attachments.Count |> should equal 0
            result.BinaryContent |> should not' (be Null)
            result.BinaryContent.ContentID |> should not' (be Null)
            result.BinaryContent.GetBytes() |> should equal [| 1uy; 2uy; 3uy; 4uy |]
            *)
        }
        
        test "serialize multipart file" {
            let context = SerializerContext(IsMultipart=true)
            let entity = Types.WithBinaryContent(BinaryContent=BinaryContent.Create("Content-ID", [| 1uy; 2uy; 3uy; 4uy |]))
            let xml = serialize "WithBinaryContentService" [| entity |] context
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithBinaryContentService><request><BinaryContent href=""cid:Content-ID"" /></request></tns:WithBinaryContentService></Body>" "invalid serialization result"
            Expect.equal 1 context.Attachments.Count "result should have exactly 1 attachment"
            Expect.isTrue (context.Attachments.ContainsKey("Content-ID")) "attachment has wrong key"
        }
        
        ptest "deserialize multipart file" {
            failtest "needs review"
            (*
            let result = resultXml |> deserializeWithContext'<TestType.WithBinaryContent> context
            result |> should not' (be Null)
            context.Attachments |> should not' (be Null)
            context.Attachments.Count |> should equal 1
            context.Attachments.ContainsKey("Content-ID") |> should equal true
            result.BinaryContent |> should not' (be Null)
            result.BinaryContent.ContentID |> should equal "Content-ID"
            result.BinaryContent |> should be (sameAs context.Attachments.["Content-ID"])
            result.BinaryContent.GetBytes() |> should equal [| 1uy; 2uy; 3uy; 4uy |]
            *)
        }
        
        test "serialize xop file" {
            let context = SerializerContext()
            let entity = Types.WithXopBinaryContent(BinaryContent=BinaryContent.Create("Content-ID", [| 1uy; 2uy; 3uy; 4uy |]))
            let xml = serialize "WithXopBinaryContentService" [| entity |] context
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:WithXopBinaryContentService><request><BinaryContent><xop:Include href=""cid:Content-ID"" xmlns:xop=""http://www.w3.org/2004/08/xop/include"" /></BinaryContent></request></tns:WithXopBinaryContentService></Body>" "invalid serialization result"
            Expect.equal 1 context.Attachments.Count "result should have exactly 1 attachment"
            Expect.isTrue (context.Attachments.ContainsKey("Content-ID")) "attachment has wrong key"
        }
        
        ptest "deserialize xop file" {
            failtest "needs review"
            (*
            let result = resultXml |> deserializeWithContext'<TestType.WithXopBinaryContent> context
            result |> should not' (be Null)
            context.Attachments |> should not' (be Null)
            context.Attachments.Count |> should equal 1
            context.Attachments.ContainsKey("Content-ID") |> should equal true
            result.BinaryContent |> should not' (be Null)
            result.BinaryContent.ContentID |> should equal "Content-ID"
            result.BinaryContent |> should be (sameAs context.Attachments.["Content-ID"])
            result.BinaryContent.GetBytes() |> should equal [| 1uy; 2uy; 3uy; 4uy |]
            *)
        }
        
        test "can serialize type with optional reference type members" {
            let xml = serialize' "HasOptionalElementsService" [| Types.HasOptionalElements(Value1 = Optional.Option.Some<string>("value")) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:HasOptionalElementsService><request><Value1>value</Value1></request></tns:HasOptionalElementsService></Body>" "invalid serialization result"
        }
        
        ptest "can deserialize type with optional reference type members" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.HasOptionalElements>
            result |> should not' (be Null)
            result.Value1.HasValue |> should be True
            result.Value1.ValueOr("") |> should equal "value"
            result.Value2.HasValue |> should be False
            result.Array1.HasValue |> should be False
            *)
        }
        
        test "can serialize type with optional value type members" {
            let xml = serialize' "HasOptionalElementsService" [| Types.HasOptionalElements(Value2 = Optional.Option.Some<int32>(15)) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:HasOptionalElementsService><request><Value2>15</Value2></request></tns:HasOptionalElementsService></Body>" "invalid serialization result"
        }
        
        ptest "can deserialize type with optional value type members" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.HasOptionalElements>
            result |> should not' (be Null)
            result.Value1.HasValue |> should be False
            result.Value2.HasValue |> should be True
            result.Value2.ValueOr(0) |> should equal 15
            result.Array1.HasValue |> should be False
            *)
        }
        
        test "can serialize type with optional array type members" {
            let xml = serialize' "HasOptionalElementsService" [| Types.HasOptionalElements(Array1 = Optional.Option.Some<int[]>([| 1; 2; 3 |])) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:HasOptionalElementsService><request><Array1><item>1</item><item>2</item><item>3</item></Array1></request></tns:HasOptionalElementsService></Body>" "invalid serialization result"
        }
        
        ptest "can deserialize type with optional array type members" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.HasOptionalElements>
            result |> should not' (be Null)
            result.Value1.HasValue |> should be False
            result.Value2.HasValue |> should be False
            result.Array1.HasValue |> should be True
            result.Array1.ValueOr(null: int[]) |> should equal [| 1; 2; 3 |]
            *)
        }
        
        test "can serialize type with no optional members set" {
            let xml = serialize' "HasOptionalElementsService" [| Types.HasOptionalElements() |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:HasOptionalElementsService><request /></tns:HasOptionalElementsService></Body>" "invalid serialization result"
        }
        
        ptest "can deserialize type with no optional members set" {
            failtest "needs review"
            (*
            let result = resultXml |> deserialize'<TestType.HasOptionalElements>
            result |> should not' (be Null)
            result.Value1.HasValue |> should be False
            result.Value2.HasValue |> should be False
            *)
        }
        
        test "serialize array of system type values" {
            let xml = serialize' "ArrayService" [| [| "1"; "2"; "3" |] |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:ArrayService><request><item>1</item><item>2</item><item>3</item></request></tns:ArrayService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize array of system type values" {
            failtest "needs review"
            (*
            resultXml |> deserialize'<string> |> should equal [| "1"; "2"; "3" |]
            *)
        }
        
        test "serialize root optional some value" {
            let initial = Optional.Option.Some<int>(202)
            let xml = serialize' "OptionalIntService" [| initial |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:OptionalIntService><request>202</request></tns:OptionalIntService></Body>" "invalid serialization result"
        }
        
        ptest "deserialize root optional some value" {
            failtest "needs review"
            (*
            resultXml |> deserialize'<Optional.Option<int>> |> should equal initial
            *)
        }
        
        test "serialize root optional none value" {
            let initial = Optional.Option.None<int>()
            let xml = serialize' "OptionalIntService" [| initial |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:OptionalIntService /></Body>" "invalid serialization result"
        }
        
        ptest "deserialize root optional none value" {
            failtest "needs review"
            (*
            resultXml |> deserialize'<Optional.Option<int>> |> should equal initial
            *)
        }
        
        test "serialize multiple levels of inheritance" {
            let initial = Types.Level3(Value1 = Nullable<int>(1), Value2 = Nullable<int>(2), Value3 = Nullable<int>(3))
            let xml = serialize' "Level3Service" [| initial |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Body xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:tns=""http://producer.x-road.eu/"" xmlns:test=""testns""><tns:Level3Service><request><Value1>1</Value1><Value2>2</Value2><Value3>3</Value3></request></tns:Level3Service></Body>" "invalid serialization result"
        }
        
        ptest "deserialize multiple levels of inheritance" {
            failtest "needs review"
            (*
            resultXml |> deserialize'<Level3> |> should equal initial
            *)
        }
    ]
