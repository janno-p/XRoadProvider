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
    abstract SimpleValueService: [<XRoadParam("request")>] request: Types.SimpleType -> [<return: XRoadParam("response")>] Types.SimpleType
    
    [<XRoadOperation("StringService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("StringService", producerNamespace)>]
    [<XRoadResponse("StringServiceResponse", producerNamespace)>]
    abstract StringService: [<XRoadParam("request")>] request: string -> [<return: XRoadParam("response")>] string
    
    [<XRoadOperation("IntService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("IntService", producerNamespace)>]
    [<XRoadResponse("IntServiceResponse", producerNamespace)>]
    abstract IntService: [<XRoadParam("request")>] request: int32 -> [<return: XRoadParam("response")>] int32
    
    [<XRoadOperation("NullableService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("NullableService", producerNamespace)>]
    [<XRoadResponse("NullableServiceResponse", producerNamespace)>]
    abstract NullableService: [<XRoadParam("request")>] request: Types.WithNullableMembers -> [<return: XRoadParam("response")>] Types.WithNullableMembers
    
    [<XRoadOperation("ComplexTypeService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("ComplexTypeService", producerNamespace)>]
    [<XRoadResponse("ComplexTypeServiceResponse", producerNamespace)>]
    abstract ComplexTypeService: [<XRoadParam("request")>] request: Types.ComplexType -> [<return: XRoadParam("response")>] Types.ComplexType
    
    [<XRoadOperation("QualifiedRootService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("QualifiedRootService", producerNamespace)>]
    [<XRoadResponse("QualifiedRootServiceResponse", producerNamespace)>]
    abstract QualifiedRootService: [<XRoadParam("root", "urn:some-namespace")>] request: Types.SimpleType -> [<return: XRoadParam("root", "urn:some-namespace")>] Types.SimpleType
    
    [<XRoadOperation("UnserializableService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("UnserializableService", producerNamespace)>]
    [<XRoadResponse("UnserializableServiceResponse", producerNamespace)>]
    abstract UnserializableService: [<XRoadParam("request")>] request: Types.UnserializableType -> [<return: XRoadParam("response")>] Types.UnserializableType

    [<XRoadOperation("AbstractChoiceService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("AbstractChoiceService", producerNamespace)>]
    [<XRoadResponse("AbstractChoiceServiceResponse", producerNamespace)>]
    abstract AbstractChoiceService: [<XRoadParam("request")>] request: Types.TypeWithAbstractChoice -> [<return: XRoadParam("response")>] Types.TypeWithAbstractChoice
    
    [<XRoadOperation("WithArray1Service", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("WithArray1Service", producerNamespace)>]
    [<XRoadResponse("WithArray1ServiceResponse", producerNamespace)>]
    abstract WithArray1Service: [<XRoadParam("request")>] request: Types.WithArray1 -> [<return: XRoadParam("response")>] Types.WithArray1
    
    [<XRoadOperation("ExtendedTypeService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("ExtendedTypeService", producerNamespace)>]
    [<XRoadResponse("ExtendedTypeServiceResponse", producerNamespace)>]
    abstract ExtendedTypeService: [<XRoadParam("request")>] request: Types.ComplexType -> [<return: XRoadParam("response")>] Types.ComplexType
    
    [<XRoadOperation("UseBaseClassService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("UseBaseClassService", producerNamespace)>]
    [<XRoadResponse("UseBaseClassServiceResponse", producerNamespace)>]
    abstract UseBaseClassService: [<XRoadParam("request")>] request: Types.UseBaseClass -> [<return: XRoadParam("response")>] Types.UseBaseClass
    
    [<XRoadOperation("ReferrerService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("ReferrerService", producerNamespace)>]
    [<XRoadResponse("ReferrerServiceResponse", producerNamespace)>]
    abstract ReferrerService: [<XRoadParam("request")>] request: Types.Referrer -> [<return: XRoadParam("response")>] Types.Referrer
    
    [<XRoadOperation("InnerReferrerService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("InnerReferrerService", producerNamespace)>]
    [<XRoadResponse("InnerReferrerServiceResponse", producerNamespace)>]
    abstract InnerReferrerService: [<XRoadParam("request")>] request: Types.InnerReferrer -> [<return: XRoadParam("response")>] Types.InnerReferrer
    
    [<XRoadOperation("TestChoiceService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("TestChoiceService", producerNamespace)>]
    [<XRoadResponse("TestChoiceServiceResponse", producerNamespace)>]
    abstract TestChoiceService: [<XRoadParam("request")>] request: Types.TestChoice -> [<return: XRoadParam("response")>] Types.TestChoice
    
    [<XRoadOperation("WithChoiceService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("WithChoiceService", producerNamespace)>]
    [<XRoadResponse("WithChoiceServiceResponse", producerNamespace)>]
    abstract WithChoiceService: [<XRoadParam("request")>] request: Types.WithChoice -> [<return: XRoadParam("response")>] Types.WithChoice
    
    [<XRoadOperation("WithBinaryContentService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("WithBinaryContentService", producerNamespace)>]
    [<XRoadResponse("WithBinaryContentServiceResponse", producerNamespace)>]
    abstract WithBinaryContentService: [<XRoadParam("request")>] request: Types.WithBinaryContent -> [<return: XRoadParam("response")>] Types.WithBinaryContent
    
    [<XRoadOperation("WithXopBinaryContentService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("WithXopBinaryContentService", producerNamespace)>]
    [<XRoadResponse("WithXopBinaryContentServiceResponse", producerNamespace)>]
    abstract WithXopBinaryContentService: [<XRoadParam("request")>] request: Types.WithXopBinaryContent -> [<return: XRoadParam("response")>] Types.WithXopBinaryContent

    [<XRoadOperation("HasOptionalElementsService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("HasOptionalElementsService", producerNamespace)>]
    [<XRoadResponse("HasOptionalElementsServiceResponse", producerNamespace)>]
    abstract HasOptionalElementsService: [<XRoadParam("request")>] request: Types.HasOptionalElements -> [<return: XRoadParam("response")>] Types.HasOptionalElements
    
    [<XRoadOperation("ArrayService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("ArrayService", producerNamespace)>]
    [<XRoadResponse("ArrayServiceResponse", producerNamespace)>]
    abstract ArrayService: [<XRoadParam("request")>] request: string[] -> [<return: XRoadParam("response")>] string[]
    
    [<XRoadOperation("OptionalIntService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("OptionalIntService", producerNamespace)>]
    [<XRoadResponse("OptionalIntServiceResponse", producerNamespace)>]
    abstract OptionalIntService: [<XRoadParam("request")>] request: Optional.Option<int> -> [<return: XRoadParam("response")>] Optional.Option<int>
    
    [<XRoadOperation("Level3Service", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Level3Service", producerNamespace)>]
    [<XRoadResponse("Level3ServiceResponse", producerNamespace)>]
    abstract Level3Service: [<XRoadParam("request")>] request: Types.Level3 -> [<return: XRoadParam("response")>] Types.Level3

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
    map.Serializer.Invoke(writer, context, value)
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
        test "can handle array type response" {
            let xml = @"<response><item /><item /></response>"
            let response = xml |> deserialize "Service2" |> unbox<string[]>
            Expect.isNotNull response "response should have value"
            Expect.equal response.Length 2 "response should have exactly 2 items"
        }
        
        test "can serialize unit request" {
            let xml = serialize' "Service1" [||]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Service1 xmlns=""http://producer.x-road.eu/"" />" "invalid xml result"
        }
        
        test "can deserialize unit response" {
            let xml = @"<?xml version=""1.0"" encoding=""utf-8""?><Service1 xmlns=""http://producer.x-road.eu/"" />"
            let response = xml |> deserialize "Service1"
            Expect.equal response [||] "invalid xml result"
        }
        
        test "can serialize simple value" {
            let xml = serialize' "SimpleValueService" [| simpleTypeEntity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><SimpleValueService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></request></SimpleValueService>" "invalid xml result"
        }
        
        test "can deserialize simple value" {
            let xml = @"<?xml version=""1.0"" encoding=""utf-8""?><SimpleValueServiceResponse xmlns=""http://producer.x-road.eu/""><response xmlns=""""><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></response></SimpleValueServiceResponse>"
            let response = xml |> deserialize "SimpleValueService"
            Expect.equal 1 response.Length "should return exactly 1 value"
            Expect.isTrue (response.[0] :? Types.SimpleType) "wrong return type"
            let result: Types.SimpleType = response.[0] |> unbox
            Expect.equal result.Value 13 "wrong result.Value value"
            Expect.equal result.ComplexValue.BigInteger 100I "wrong result.ComplexValue.BigInteger value"
            Expect.equal result.ComplexValue.String "test" "wrong result.ComplexValue.String value"
            Expect.isTrue result.SubContent.ContentValue "wrong result.SubContent.ContentValue value"
        }
        
        test "serialize null value" {
            let xml = serialize' "StringService" [| (null: string) |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><StringService xmlns=""http://producer.x-road.eu/""><request p2:nil=""true"" xmlns:p2=""http://www.w3.org/2001/XMLSchema-instance"" xmlns="""" /></StringService>" "invalid serialization result"
        }
        
        test "deserialize null value" {
            let xml = @"<?xml version=""1.0"" encoding=""utf-8""?><StringServiceResponse xmlns=""http://producer.x-road.eu/""><response xsi:nil=""true"" xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns="""" /></StringServiceResponse>"
            let response = xml |> deserialize "StringService"
            Expect.isNull response "response should be null"
        }
        
        test "write qualified root name" {
            let xml = serialize' "QualifiedRootService" [| simpleTypeEntity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><QualifiedRootService xmlns=""http://producer.x-road.eu/""><root xmlns=""urn:some-namespace""><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></root></QualifiedRootService>" "invalid xml result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><StringService xmlns=""http://producer.x-road.eu/""><request xmlns="""">string value</request></StringService>" "invalid serialization result"
        }
        
        test "deserialize string value" {
            let xml = @"<?xml version=""1.0"" encoding=""utf-8""?><StringServiceResponse xmlns=""http://producer.x-road.eu/""><response xmlns="""">string value</response></StringServiceResponse>"
            let response = xml |> deserialize "StringService" |> unbox<string>
            Expect.equal response "string value" "response not equal to 'string value'"
        }
        
        test "serialize int value" {
            let xml = serialize' "IntService" [| 32 |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><IntService xmlns=""http://producer.x-road.eu/""><request xmlns="""">32</request></IntService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><NullableService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Value1>13</Value1><Value2 p3:nil=""true"" xmlns:p3=""http://www.w3.org/2001/XMLSchema-instance"" /></request></NullableService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><AbstractChoiceService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><value1 p3:type=""Concrete1"" xmlns:p3=""http://www.w3.org/2001/XMLSchema-instance""><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></value1></request></AbstractChoiceService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><WithArray1Service xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Array><item>true</item><item>false</item><item>true</item><item>true</item></Array></request></WithArray1Service>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><ExtendedTypeService xmlns=""http://producer.x-road.eu/""><request p2:type=""ExtendedType"" xmlns:p2=""http://www.w3.org/2001/XMLSchema-instance"" xmlns=""""><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></request></ExtendedTypeService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><UseBaseClassService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Member p3:type=""ExtendedType"" xmlns:p3=""http://www.w3.org/2001/XMLSchema-instance""><String>test</String><BigInteger>100</BigInteger><OwnElement>test</OwnElement></Member></request></UseBaseClassService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><ReferrerService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Reference p3:type=""Concrete1"" xmlns:p3=""http://www.w3.org/2001/XMLSchema-instance""><BaseValue>test</BaseValue><SubValue1>test2</SubValue1></Reference></request></ReferrerService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><ReferrerService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Reference p3:type=""p4:ConcreteTypeName"" xmlns:p4=""testns"" xmlns:p3=""http://www.w3.org/2001/XMLSchema-instance""><BaseValue>test</BaseValue><SubValue3>test2</SubValue3></Reference></request></ReferrerService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><InnerReferrerService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Ref><Reference p4:type=""Concrete1"" xmlns:p4=""http://www.w3.org/2001/XMLSchema-instance""><BaseValue>basev</BaseValue><SubValue1>kino</SubValue1></Reference></Ref></request></InnerReferrerService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><TestChoiceService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Choice1Element>test</Choice1Element></request></TestChoiceService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><TestChoiceService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Choice2><Choice2Element>test</Choice2Element></Choice2></request></TestChoiceService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><WithChoiceService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><NotAChoice>tere</NotAChoice><Choice1Element>test</Choice1Element></request></WithChoiceService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><WithChoiceService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><NotAChoice>tere</NotAChoice><Choice2><Choice2Element>test</Choice2Element></Choice2></request></WithChoiceService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><WithChoiceService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><NotAChoice /><Choice1Element>test</Choice1Element></request></WithChoiceService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><WithChoiceService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><NotAChoice p3:nil=""true"" xmlns:p3=""http://www.w3.org/2001/XMLSchema-instance"" /><Choice1Element>test</Choice1Element></request></WithChoiceService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><WithArray1Service xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Array p3:nil=""true"" xmlns:p3=""http://www.w3.org/2001/XMLSchema-instance"" /></request></WithArray1Service>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><WithBinaryContentService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><BinaryContent>AQIDBA==</BinaryContent></request></WithBinaryContentService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><WithBinaryContentService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><BinaryContent href=""cid:Content-ID"" /></request></WithBinaryContentService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><WithXopBinaryContentService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><BinaryContent><xop:Include href=""cid:Content-ID"" xmlns:xop=""http://www.w3.org/2004/08/xop/include"" /></BinaryContent></request></WithXopBinaryContentService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><HasOptionalElementsService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Value1>value</Value1></request></HasOptionalElementsService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><HasOptionalElementsService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Value2>15</Value2></request></HasOptionalElementsService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><HasOptionalElementsService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Array1><item>1</item><item>2</item><item>3</item></Array1></request></HasOptionalElementsService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><HasOptionalElementsService xmlns=""http://producer.x-road.eu/""><request xmlns="""" /></HasOptionalElementsService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><ArrayService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><item>1</item><item>2</item><item>3</item></request></ArrayService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><OptionalIntService xmlns=""http://producer.x-road.eu/""><request xmlns="""">202</request></OptionalIntService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><OptionalIntService xmlns=""http://producer.x-road.eu/""><request xmlns="""" /></OptionalIntService>" "invalid serialization result"
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
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Level3Service xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Value1>1</Value1><Value2>2</Value2><Value3>3</Value3></request></Level3Service>" "invalid serialization result"
        }
        
        ptest "deserialize multiple levels of inheritance" {
            failtest "needs review"
            (*
            resultXml |> deserialize'<Level3> |> should equal initial
            *)
        }
    ]
