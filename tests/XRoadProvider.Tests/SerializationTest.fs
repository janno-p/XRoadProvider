module XRoadProvider.Tests.SerializationTest

open Expecto
open System.IO
open System.Text
open System.Threading
open System.Xml
open System.Xml.Linq
open XRoad
open XRoad.DynamicMethods
open XRoad.Serialization.Attributes
open XRoadProvider.Tests.SoapUtil

let [<Literal>] producerName = "producer"
let [<Literal>] producerNamespace = "http://producer.x-road.eu/"

module Types =
    type Type1 =
        class
        end
        
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
    type SimpleType() =
        [<XRoadElement>]
        member val Value = Unchecked.defaultof<int> with get, set
        [<XRoadElement>]
        member val ComplexValue = Unchecked.defaultof<ComplexType> with get, set
        [<XRoadElement>]
        member val SubContent = Unchecked.defaultof<WithContent> with get, set
        member val IgnoredValue = true with get, set

type Services =
    [<XRoadOperation("Service1", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service1", producerNamespace)>]
    [<XRoadResponse("Service1Response", producerNamespace)>]
    abstract Service1: unit -> unit

    [<XRoadOperation("Service2", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service2", producerNamespace)>]
    [<XRoadResponse("Service2Response", producerNamespace)>]
    abstract Service2: int64 -> Types.Type1[]
    
    [<XRoadOperation("SimpleValueService", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("SimpleValueService", producerNamespace)>]
    [<XRoadResponse("SimpleValueServiceResponse", producerNamespace)>]
    abstract SimpleValueService: [<XRoadParam("request")>] request: Types.SimpleType -> [<return: XRoadParam("response")>] Types.SimpleType

let deserialize (nm: string) (xml: string) =
    let map = typeof<Services>.GetMethod(nm) |> getMethodMap
    use reader = XDocument.Parse(xml).CreateReader()
    map.Deserialize(reader, SerializerContext())

let serialize nm value =
    let map = typeof<Services>.GetMethod(nm) |> getMethodMap
    use stream = new MemoryStream()
    use sw = new StreamWriter(stream, Encoding.UTF8)
    use writer = XmlWriter.Create(sw)
    writer.WriteStartDocument()
    map.Serialize(writer, SerializerContext(), value)
    writer.WriteEndDocument()
    writer.Flush()
    stream.Position <- 0L
    use reader = new StreamReader(stream, Encoding.UTF8)
    reader.ReadToEnd()

let simpleTypeEntity =
    let entity = Types.SimpleType(Value = 13)
    entity.ComplexValue <- Types.ComplexType(String = "test", BigInteger = 100I)
    entity.SubContent <- Types.WithContent(ContentValue = true)
    entity

let [<Tests>] tests =
    testList "serialization tests" [
        ptest "can handle array type response" {
            let xml = @"<response><item /><item /></response>"
            let response = xml |> deserialize "Service2" |> unbox<Types.Type1[]>
            Expect.isNotNull response "response should have value"
            Expect.equal response.Length 2 "response should have exactly 2 items"
        }
        
        test "can serialize unit request" {
            let xml = serialize "Service1" [||]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><Service1 xmlns=""http://producer.x-road.eu/"" />" "invalid xml result"
        }
        
        test "can deserialize unit response" {
            let xml = @"<?xml version=""1.0"" encoding=""utf-8""?><Service1 xmlns=""http://producer.x-road.eu/"" />"
            let response = xml |> deserialize "Service1"
            Expect.equal response [||] "invalid xml result"
        }
        
        test "can serialize simple value" {
            let xml = serialize "SimpleValueService" [| simpleTypeEntity |]
            Expect.equal xml @"<?xml version=""1.0"" encoding=""utf-8""?><SimpleValueService xmlns=""http://producer.x-road.eu/""><request xmlns=""""><Value>13</Value><ComplexValue><String>test</String><BigInteger>100</BigInteger></ComplexValue><SubContent>true</SubContent></request></SimpleValueService>" "invalid xml result"
        }
        
        ptest "can deserialize simple value" {
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
    ]
