namespace XRoadTypeProvider

open System
open System.Xml

module XmlNamespace =
    let [<Literal>] SoapEnvelope = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] XRoad = "http://x-road.ee/xsd/x-road.xsd"

type XRoadServiceRequest () =
    member __.Execute(body: obj, attachments: Runtime.AttachmentCollection option, settings: XRoad.XRoadHeader option) =
        let settings = defaultArg settings (XRoad.XRoadHeader())

        let hostName =
            match settings.Producer with
            | Some producer -> producer
            | _ -> "http://localhost/"

        let consumer =
            match settings.Consumer with
            | Some consumer -> consumer
            | _ -> "10239452"

        let req = System.Net.WebRequest.Create(hostName)
        req.Method <- "POST"

        let writeReq () =
            use stream = req.GetRequestStream()
            use writer = XmlWriter.Create(stream)
            writer.WriteStartDocument()
            writer.WriteStartElement("Envelope", XmlNamespace.SoapEnvelope)
            writer.WriteAttributeString("xmlns", "SOAP-ENV", null, XmlNamespace.SoapEnvelope)
            writer.WriteStartElement("Header", XmlNamespace.SoapEnvelope)
            writer.WriteAttributeString("xmlns", "xrd", null, XmlNamespace.XRoad)
            writer.WriteStartElement("consumer", XmlNamespace.XRoad)
            writer.WriteString(consumer)
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
