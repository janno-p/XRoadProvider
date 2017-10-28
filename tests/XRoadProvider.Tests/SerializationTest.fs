module XRoadProvider.Tests.SerializationTest

open Expecto
open Suave
open System.Threading
open XRoad

let [<Literal>] producerName = "producer"
let [<Literal>] producerUri = "http://producer.x-road.eu/"

module Types =
    type Type1 =
        class
        end

type Services =
    abstract Service1: unit -> unit
    abstract Service2: int64 -> Types.Type1[]

let [<Tests>] tests =
    testList "serialization tests" [
        test "can handle actions" {
            Expect.throwsT<exn>
                (fun _ -> XRoadUtil.MakeServiceCall(typeof<Services>, "Service1", producerUri, producerName, null, [||]) |> ignore)
                "it is not implemented yet!"
        }
        
        test "can handle array type response" {
            let response = XRoadUtil.MakeServiceCall(typeof<Services>, "Service2", producerUri, producerName, null, [||])
            Expect.isNotNull response "response should have value"
            Expect.isTrue (response :? Types.Type1[]) "response should be array of Type1"
            let arr: Types.Type1[] = unbox response
            Expect.equal arr.Length 2 "response should have exactly 2 items"
        }
    ]
