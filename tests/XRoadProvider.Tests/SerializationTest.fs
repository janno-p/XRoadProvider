
module XRoadProvider.Tests.SerializationTest

open Expecto
open System.Threading
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
    abstract Service1: unit -> unit

    [<XRoadOperation("Service2", "v1", XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service2", producerNamespace)>]
    [<XRoadResponse("Service2Response", producerNamespace)>]
    abstract Service2: int64 -> Types.Type1[]

let deserialize<'T> (nm: string) (xml: string) : 'T =
    let map = typeof<Services>.GetMethod(nm) |> getMethodMap
    use reader = XDocument.Parse(xml).CreateReader()
    map.Deserialize(reader, SerializerContext()) |> unbox

let [<Tests>] tests =
    testList "serialization tests" [
        test "can handle actions" {
            Expect.throwsT<exn>
                (fun _ -> @"" |> deserialize<unit> "Service1")
                "it is not implemented yet!"
        }
        
        test "can handle array type response" {
            let xml = @"<response><item /><item /></response>"
            let response = xml |> deserialize<Types.Type1[]> "Service2"
            Expect.isNotNull response "response should have value"
            Expect.equal response.Length 2 "response should have exactly 2 items"
        }
    ]
