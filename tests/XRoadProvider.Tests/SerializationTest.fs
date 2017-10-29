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

type Services =
    [<XRoadOperation("Service1", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service1", producerNamespace)>]
    [<XRoadResponse("Service1Response", producerNamespace)>]
    abstract Service1: unit -> unit

    [<XRoadOperation("Service2", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service2", producerNamespace)>]
    [<XRoadResponse("Service2Response", producerNamespace)>]
    abstract Service2: int64 -> Types.Type1[]

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
    ]
