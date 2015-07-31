namespace XRoad

open System.Net
open System
open System.Xml.Linq
open System.Collections.Generic
open System.IO

type XRoadStreamWriter() =
    class
    end

type XRoadStreamReader() =
    class
    end

type XRoadProtocol =
    | Version20
    | Version30
    | Version31
    | Version40

type XRoadOptions(uri: string) =
    member val IsEncoded = false with get, set
    member val Protocol = XRoadProtocol.Version31 with get, set
    member val Uri = uri with get, set

type XRoadMessage() =
    member val Headers: (XName * obj) array = [||] with get, set
    member val Content: (XName * obj) array = [||] with get, set
    member val Attachments = Dictionary<string, Stream>() with get, set

type XRoadResponse(response: WebResponse) =
    member __.RetrieveMessage(): XRoadMessage =
        XRoadMessage()
    interface IDisposable with
        member __.Dispose() =
            (response :> IDisposable).Dispose()

type XRoadRequest(opt: XRoadOptions) =
    let request = WebRequest.Create(opt.Uri, Method="POST", ContentType="text/xml; charset=utf-8")
    do request.Headers.Set("SOAPAction", "")
    member __.SendMessage(_: XRoadMessage) =
        ()
    member __.GetResponse() =
        new XRoadResponse(request.GetResponse())

type public XRoadUtil =
    static member MakeServiceCall(message, options) =
        let request = new XRoadRequest(options)
        request.SendMessage(message)
        use response = request.GetResponse()
        response.RetrieveMessage()
