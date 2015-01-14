module XRoadTypeProvider.Runtime

open System
open System.Collections.Generic
open System.IO
open System.Xml
open Wsdl

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

type XRoadOperation = {
    BindingStyle: XRoadBindingStyle
    Version: string
    QualifiedName: XmlQualifiedName
}

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

[<Interface>]
type IXRoadResponseWithAttachments<'T> =
    abstract member Result: 'T with get
    abstract member Attachments: Stream [] with get

type XRoadServiceRequest () =
    member __.Execute(context: IXRoadContext, operation, args: obj []) =
        //let settings = defaultArg settings (XRoad.XRoadHeader())
        let settings = XRoad.XRoadHeader()

        let req = System.Net.WebRequest.Create(context.Address)
        req.Method <- "POST"

        let operationName, operationNamespace, operationVersion = operation

        let producer = defaultArg settings.Producer context.Producer
        let serviceName = sprintf "%s.%s.%s" producer operationName operationVersion

        let writeReq () =
            use stream = req.GetRequestStream()
            use writer = XmlWriter.Create(stream)

            let writerXRoadProperty name value =
                writer.WriteStartElement(name, XmlNamespace.XRoad)
                match value with
                | Some value -> writer.WriteString(value)
                | _ -> ()
                writer.WriteEndElement()

            let writeSoapHeader () =
                writer.WriteStartElement("Header", XmlNamespace.SoapEnvelope)
                writer.WriteAttributeString("xmlns", "xrd", null, XmlNamespace.XRoad)
                writerXRoadProperty "consumer" (Some (defaultArg settings.Consumer "10239452"))
                writerXRoadProperty "producer" (Some producer)
                writerXRoadProperty "userId" (Some (defaultArg settings.UserId "EE30101010007"))
                writerXRoadProperty "id" (Some (defaultArg settings.Id "3aed1ae3813eb7fbed9396fda70ca1215d3f3fe1"))
                writerXRoadProperty "service" (Some serviceName)
                writerXRoadProperty "issue" None
                writer.WriteEndElement()

            writer.WriteStartDocument()
            writer.WriteStartElement("SOAP-ENV", "Envelope", XmlNamespace.SoapEnvelope)
            writeSoapHeader()
            writer.WriteStartElement("Body", XmlNamespace.SoapEnvelope)

            writer.WriteStartElement(operationName, operationNamespace)
            writer.WriteEndElement()

            writer.WriteEndElement()
            writer.WriteEndElement()
            writer.WriteEndDocument()

        writeReq()

        use resp = req.GetResponse()
        use reader = new System.IO.StreamReader(resp.GetResponseStream())
        printfn "%A" (reader.ReadToEnd())
        obj()
    interface IDisposable with
        override __.Dispose() = ()
