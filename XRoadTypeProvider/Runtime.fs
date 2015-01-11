module XRoadTypeProvider.Runtime

open System.Collections.Generic
open System.IO

type XRoadEntity () =
    let data = Dictionary<string, obj>()

    member __.SetProperty (name, value) =
        data.[name] <- value

    member __.GetProperty<'T> (name) =
        if data.ContainsKey name then
            unbox data.[name]
        else Unchecked.defaultof<'T>

type XRoadBindingStyle =
    | RpcEncoded = 0y
    | DocumentLiteral = 1y

type XRoadOperation (bindingStyle: XRoadBindingStyle, version: string) =
    member __.BindingStyle with get() = bindingStyle
    member __.Version with get() = version

[<Interface>]
type IXRoadContext =
    abstract member Address: string with get, set
    abstract member Producer: string with get, set
    abstract member XRoadSettings: XRoad.XRoadHeader with get

type XRoadContext () =
    interface IXRoadContext with
        member val Address = "" with get, set
        member val Producer = "" with get, set
        member val XRoadSettings = XRoad.XRoadHeader() with get

type AttachmentCollection () =
    member __.Add (stream: Stream) =
        true
