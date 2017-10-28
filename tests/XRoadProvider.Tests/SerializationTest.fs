
module XRoadProvider.Tests.SerializationTest

open Expecto
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open System.Threading
open XRoad
open XRoad.Serialization.Attributes
open XRoadProvider.Tests.SoapUtil

let [<Literal>] producerName = "producer"
let [<Literal>] producerNamespace = "http://producer.x-road.eu/"

let producerUri resourceName = sprintf "http://127.0.0.1:8080/%s" resourceName

let [<Literal>] service2Response = @"<response><item /><item /></response>"

let routes =
    POST >=> choose
        [ path "/service1" >=> OK "Hello, World!"
          path "/service2" >=> OK (makeSoapResponse service2Response) ]

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

let [<Tests>] tests =
    testList "serialization tests" [
        test "can handle actions" {
            Expect.throwsT<exn>
                (fun _ -> XRoadUtil.MakeServiceCall(typeof<Services>, "Service1", (producerUri "service1"), producerName, null, [||]) |> ignore)
                "it is not implemented yet!"
        }
        
        test "can handle array type response" {
            let response = XRoadUtil.MakeServiceCall(typeof<Services>, "Service2", (producerUri "service2"), producerName, XRoadHeader(), [||])
            Expect.isNotNull response "response should have value"
            Expect.isTrue (response :? Types.Type1[]) "response should be array of Type1"
            let arr: Types.Type1[] = unbox response
            Expect.equal arr.Length 2 "response should have exactly 2 items"
        }
    ]
